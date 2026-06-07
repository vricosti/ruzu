// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `frontend/maxwell/translate_program.cpp`
//!
//! Top-level shader translation: takes a CFG and environment, translates
//! each block using the TranslatorVisitor, builds the structured control
//! flow AST, and returns an IR::Program.

use crate::ir::basic_block::Block;
use crate::ir::opcodes::Opcode;
use crate::ir::program::{Program, SyntaxNode};
use crate::ir::types::ShaderStage;
use crate::ir::value::{Attribute, InstRef, Value};
use crate::ir_opt;
use crate::program_header::{PixelImap, ProgramHeader};
use crate::runtime_info::{AttributeType, RuntimeInfo};
use crate::shader_info::Interpolation;
use crate::varying_state::VaryingState;
use std::collections::{BTreeMap, VecDeque};

/// Translate a Maxwell shader program from instruction words to IR.
///
/// Upstream takes prebuilt CFG and object-pool owners plus `Environment&` /
/// `HostTranslateInfo&`. Ruzu's public compatibility entry point still takes
/// only instruction words and a stage, so it delegates to the currently ported
/// CFG/structured-CF/translation/pass driver in `pipeline_cache.rs`.
pub fn translate_program(instructions: &[u64], stage: crate::ir::types::ShaderStage) -> Program {
    crate::pipeline_cache::translate_program_at_offset(instructions, stage, 0)
}

/// Merge dual vertex programs (VertexA + VertexB) into a single VertexB program.
///
/// Port of upstream `MergeDualVertexPrograms` in
/// `frontend/maxwell/translate_program.cpp`. Upstream syntax nodes store block
/// pointers, so appending VertexB syntax after VertexA is pointer-safe. Rust
/// syntax nodes store block indices; this port explicitly remaps every VertexB
/// block reference and every `Value::Inst` reference by the VertexA block count.
pub fn merge_dual_vertex_programs(vertex_a: &mut Program, vertex_b: &mut Program) -> Program {
    let vertex_b_block_offset = vertex_a.blocks.len() as u32;

    ir_opt::dual_vertex_pass::vertex_a_transform_pass(vertex_a);
    ir_opt::dual_vertex_pass::vertex_b_transform_pass(vertex_b);

    let mut result = Program::new(ShaderStage::VertexB);
    result.syntax_list = vertex_a
        .syntax_list
        .iter()
        .filter(|node| !matches!(node, SyntaxNode::Return))
        .cloned()
        .collect();
    result.syntax_list.extend(
        vertex_b
            .syntax_list
            .iter()
            .map(|node| remap_syntax_node_blocks(node, vertex_b_block_offset)),
    );

    result.blocks = vertex_a.blocks.clone();
    result.blocks.extend(
        vertex_b
            .blocks
            .iter()
            .cloned()
            .map(|block| remap_block(block, vertex_b_block_offset)),
    );
    regenerate_block_order_from_syntax(&mut result);

    result.post_order_blocks = vertex_b
        .post_order_blocks
        .iter()
        .map(|&block| block + vertex_b_block_offset)
        .collect();
    result
        .post_order_blocks
        .extend(vertex_a.post_order_blocks.iter().copied());

    result.info = vertex_a.info.clone();
    for (dst, src) in result
        .info
        .loads
        .mask
        .iter_mut()
        .zip(vertex_b.info.loads.mask)
    {
        *dst |= src;
    }
    for (dst, src) in result
        .info
        .stores
        .mask
        .iter_mut()
        .zip(vertex_b.info.stores.mask)
    {
        *dst |= src;
    }
    result.local_memory_size = vertex_a.local_memory_size.max(vertex_b.local_memory_size);

    ir_opt::texture_pass::join_texture_info(&mut result.info, &mut vertex_b.info);
    ir_opt::global_memory_to_storage_buffer_pass::join_storage_info(
        &mut result.info,
        &mut vertex_b.info,
    );
    ir_opt::dead_code_elimination::dead_code_elimination_pass(&mut result);
    ir_opt::collect_info::collect_shader_info_pass(&mut result);

    result
}

/// Convert legacy (fixed-function) varyings to generic attributes.
pub fn convert_legacy_to_generic(program: &mut Program, runtime_info: &RuntimeInfo) {
    if program.info.stores.legacy() {
        let mut unused_output_generics = VecDeque::new();
        for index in 0..NUM_GENERICS {
            if !program.info.stores.generic_any(index) {
                unused_output_generics.push_back(generic_x(index));
            }
        }

        program.info.legacy_stores_mapping = generate_legacy_to_generic_mappings(
            &program.info.stores,
            unused_output_generics,
            &BTreeMap::new(),
        );

        let block_indices = program.post_order_blocks.clone();
        let mappings = program.info.legacy_stores_mapping.clone();
        let mut mapped_store_attrs = Vec::new();
        for block_index in block_indices {
            for inst in program.block_mut(block_index).iter_mut() {
                if inst.opcode != Opcode::SetAttribute {
                    continue;
                }
                let Some(Value::Attribute(attr)) = inst.args.first().copied() else {
                    continue;
                };
                if !is_legacy_attribute(attr) {
                    continue;
                }
                if let Some(&mapped_attr) = mappings.get(&(attr.0 as u64)) {
                    mapped_store_attrs.push(mapped_attr);
                    inst.args[0] = Value::Attribute(Attribute(mapped_attr as u32));
                }
            }
        }
        for mapped_attr in mapped_store_attrs {
            program.info.stores.set(mapped_attr as usize, true);
        }
    }

    if program.info.loads.legacy() {
        let mut unused_input_generics = VecDeque::new();
        for index in 0..NUM_GENERICS {
            let input_type = runtime_info.generic_input_types[index];
            if !runtime_info.previous_stage_stores.generic_any(index)
                || !program.info.loads.generic_any(index)
                || input_type == AttributeType::Disabled
            {
                unused_input_generics.push_back(generic_x(index));
            }
        }

        let mappings = generate_legacy_to_generic_mappings(
            &program.info.loads,
            unused_input_generics,
            &runtime_info.previous_stage_legacy_stores_mapping,
        );

        let block_indices = program.post_order_blocks.clone();
        let mut mapped_load_attrs = Vec::new();
        for block_index in block_indices {
            for inst in program.block_mut(block_index).iter_mut() {
                if inst.opcode != Opcode::GetAttribute {
                    continue;
                }
                let Some(Value::Attribute(attr)) = inst.args.first().copied() else {
                    continue;
                };
                if !is_legacy_attribute(attr) {
                    continue;
                }
                if let Some(&mapped_attr) = mappings.get(&(attr.0 as u64)) {
                    mapped_load_attrs.push(mapped_attr);
                    inst.args[0] = Value::Attribute(Attribute(mapped_attr as u32));
                }
            }
        }
        for mapped_attr in mapped_load_attrs {
            program.info.loads.set(mapped_attr as usize, true);
        }
    }
}

/// Port of upstream `CollectInterpolationInfo`.
pub fn collect_interpolation_info(sph: &ProgramHeader, program: &mut Program) {
    if program.stage != ShaderStage::Fragment {
        return;
    }
    for index in 0..NUM_GENERICS {
        let mut imap = None;
        for value in sph.ps_generic_input_map(index as u32) {
            if value == PixelImap::Unused {
                continue;
            }
            if imap.is_some_and(|current| current != value) {
                log::warn!(
                    "Per-component interpolation not implemented for generic input {}",
                    index
                );
                continue;
            }
            imap = Some(value);
        }
        let Some(imap) = imap else {
            continue;
        };
        program.info.interpolation[index] = match imap {
            PixelImap::Unused | PixelImap::Perspective => Interpolation::Smooth,
            PixelImap::Constant => Interpolation::Flat,
            PixelImap::ScreenLinear => Interpolation::NoPerspective,
        };
    }
}

/// Generate a passthrough geometry shader for layer emulation.
///
/// Not yet implemented: requires generating a full passthrough GS IR program.
pub fn generate_geometry_passthrough(_source_program: &mut Program) -> Program {
    log::warn!("GenerateGeometryPassthrough not yet implemented — returning empty program");
    Program::new(ShaderStage::VertexB)
}

fn remap_syntax_node_blocks(node: &SyntaxNode, offset: u32) -> SyntaxNode {
    match node {
        SyntaxNode::Block(block) => SyntaxNode::Block(block + offset),
        SyntaxNode::If { cond, body, merge } => SyntaxNode::If {
            cond: remap_value_blocks(*cond, offset),
            body: body + offset,
            merge: merge + offset,
        },
        SyntaxNode::EndIf { merge } => SyntaxNode::EndIf {
            merge: merge + offset,
        },
        SyntaxNode::Loop {
            body,
            continue_block,
            merge,
        } => SyntaxNode::Loop {
            body: body + offset,
            continue_block: continue_block + offset,
            merge: merge + offset,
        },
        SyntaxNode::Repeat {
            cond,
            loop_header,
            merge,
        } => SyntaxNode::Repeat {
            cond: remap_value_blocks(*cond, offset),
            loop_header: loop_header + offset,
            merge: merge + offset,
        },
        SyntaxNode::Break { cond, merge, skip } => SyntaxNode::Break {
            cond: remap_value_blocks(*cond, offset),
            merge: merge + offset,
            skip: skip + offset,
        },
        SyntaxNode::Return => SyntaxNode::Return,
        SyntaxNode::Unreachable => SyntaxNode::Unreachable,
    }
}

fn remap_block(mut block: Block, offset: u32) -> Block {
    for inst in block.iter_mut() {
        for arg in &mut inst.args {
            *arg = remap_value_blocks(*arg, offset);
        }
    }
    for value in &mut block.ssa_reg_values {
        *value = remap_value_blocks(*value, offset);
    }
    for predecessor in &mut block.imm_predecessors {
        *predecessor += offset;
    }
    for successor in &mut block.imm_successors {
        *successor += offset;
    }
    block
}

fn remap_value_blocks(value: Value, offset: u32) -> Value {
    match value {
        Value::Inst(InstRef { block, inst }) => Value::Inst(InstRef {
            block: block + offset,
            inst,
        }),
        other => other,
    }
}

const NUM_GENERICS: usize = 32;
const NUM_FIXEDFNCTEXTURE: usize = 10;
const COLOR_FRONT_DIFFUSE_R: u32 = 160;
const COLOR_BACK_SPECULAR_A: u32 = 175;
const FOG_COORDINATE: u32 = 186;
const FIXED_FNC_TEXTURE0_S: u32 = 192;
const FIXED_FNC_TEXTURE9_Q: u32 = 231;

fn generic_x(index: usize) -> u64 {
    (Attribute::generic(index as u32, 0).0) as u64
}

fn is_legacy_attribute(attribute: Attribute) -> bool {
    let raw = attribute.0;
    (COLOR_FRONT_DIFFUSE_R..=COLOR_BACK_SPECULAR_A).contains(&raw)
        || raw == FOG_COORDINATE
        || (FIXED_FNC_TEXTURE0_S..=FIXED_FNC_TEXTURE9_Q).contains(&raw)
}

fn generate_legacy_to_generic_mappings(
    state: &VaryingState,
    mut unused_generics: VecDeque<u64>,
    previous_stage_mapping: &BTreeMap<u64, u64>,
) -> BTreeMap<u64, u64> {
    let mut mapping = BTreeMap::new();

    for index in 0..4 {
        let attr = COLOR_FRONT_DIFFUSE_R as usize + index * 4;
        if state.any_component(attr) {
            update_legacy_mapping(
                &mut mapping,
                &mut unused_generics,
                previous_stage_mapping,
                attr as u64,
                4,
            );
        }
    }

    if state.get(FOG_COORDINATE as usize) {
        update_legacy_mapping(
            &mut mapping,
            &mut unused_generics,
            previous_stage_mapping,
            FOG_COORDINATE as u64,
            1,
        );
    }

    for index in 0..NUM_FIXEDFNCTEXTURE {
        let attr = FIXED_FNC_TEXTURE0_S as usize + index * 4;
        if state.any_component(attr) {
            update_legacy_mapping(
                &mut mapping,
                &mut unused_generics,
                previous_stage_mapping,
                attr as u64,
                4,
            );
        }
    }

    mapping
}

fn update_legacy_mapping(
    mapping: &mut BTreeMap<u64, u64>,
    unused_generics: &mut VecDeque<u64>,
    previous_stage_mapping: &BTreeMap<u64, u64>,
    attr: u64,
    count: u64,
) {
    if previous_stage_mapping.contains_key(&attr) {
        for i in 0..count {
            let key = attr + i;
            mapping.insert(key, previous_stage_mapping[&key]);
        }
    } else {
        let generic = unused_generics
            .front()
            .copied()
            .expect("no free generic attribute for legacy varying conversion");
        for i in 0..count {
            mapping.insert(attr + i, generic + i);
        }
        unused_generics.pop_front();
    }
}

#[cfg(test)]
mod convert_legacy_tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;

    #[test]
    fn convert_legacy_to_generic_rewrites_store_and_records_mapping() {
        let mut program = Program::new(ShaderStage::VertexB);
        let mut block = Block::new();
        block.append_inst(Inst::new(
            Opcode::SetAttribute,
            vec![
                Value::Attribute(Attribute(COLOR_FRONT_DIFFUSE_R)),
                Value::ImmF32(1.0),
                Value::ImmU32(0),
            ],
        ));
        program.blocks.push(block);
        program.post_order_blocks.push(0);
        program
            .info
            .stores
            .set(COLOR_FRONT_DIFFUSE_R as usize, true);

        convert_legacy_to_generic(&mut program, &RuntimeInfo::default());

        let mapped = program.info.legacy_stores_mapping[&(COLOR_FRONT_DIFFUSE_R as u64)];
        assert_eq!(mapped, Attribute::generic(0, 0).0 as u64);
        assert!(program.info.stores.get(mapped as usize));
        assert_eq!(
            program.blocks[0].inst(0).args[0],
            Value::Attribute(Attribute(mapped as u32))
        );
    }

    #[test]
    fn convert_legacy_to_generic_reuses_previous_stage_mapping_for_load() {
        let mut runtime_info = RuntimeInfo::default();
        runtime_info
            .previous_stage_legacy_stores_mapping
            .insert(FOG_COORDINATE as u64, Attribute::generic(7, 0).0 as u64);

        let mut program = Program::new(ShaderStage::Fragment);
        let mut block = Block::new();
        block.append_inst(Inst::new(
            Opcode::GetAttribute,
            vec![
                Value::Attribute(Attribute(FOG_COORDINATE)),
                Value::ImmU32(0),
            ],
        ));
        program.blocks.push(block);
        program.post_order_blocks.push(0);
        program.info.loads.set(FOG_COORDINATE as usize, true);

        convert_legacy_to_generic(&mut program, &runtime_info);

        assert!(program.info.loads.get(Attribute::generic(7, 0).0 as usize));
        assert_eq!(
            program.blocks[0].inst(0).args[0],
            Value::Attribute(Attribute::generic(7, 0))
        );
    }
}

#[cfg(test)]
mod interpolation_tests {
    use super::*;

    #[test]
    fn collect_interpolation_info_matches_ps_imap() {
        let mut sph = ProgramHeader::default();
        sph.raw[6] = 0b01_01_01_01 | (0b11_11_11_11 << 8) | (0b10_10_10_10 << 16);
        let mut program = Program::new(ShaderStage::Fragment);

        collect_interpolation_info(&sph, &mut program);

        assert_eq!(program.info.interpolation[0], Interpolation::Flat);
        assert_eq!(program.info.interpolation[1], Interpolation::NoPerspective);
        assert_eq!(program.info.interpolation[2], Interpolation::Smooth);
        assert_eq!(program.info.interpolation[3], Interpolation::Smooth);
    }
}

fn regenerate_block_order_from_syntax(program: &mut Program) {
    let mut order = 0;
    for node in &program.syntax_list {
        if let SyntaxNode::Block(block_index) = *node {
            if let Some(block) = program.blocks.get_mut(block_index as usize) {
                block.order = order;
                order += 1;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;
    use crate::ir::opcodes::Opcode;

    #[test]
    fn translate_program_uses_cfg_driver_instead_of_empty_stub() {
        let program = translate_program(&[0, 0], ShaderStage::VertexB);

        assert_eq!(program.stage, ShaderStage::VertexB);
        assert!(
            !program.blocks.is_empty(),
            "translate_program must build IR blocks instead of returning Program::new(stage)"
        );
    }

    #[test]
    fn merge_dual_vertex_programs_remaps_vertex_b_block_references() {
        let mut vertex_a = Program::new(ShaderStage::VertexA);
        let mut va_block = Block::new();
        va_block.append_inst(Inst::new(Opcode::Epilogue, Vec::new()));
        vertex_a.blocks.push(va_block);
        vertex_a.syntax_list = vec![SyntaxNode::Block(0), SyntaxNode::Return];
        vertex_a.post_order_blocks = vec![0];
        vertex_a.local_memory_size = 0x20;
        vertex_a.info.loads.mask[0] = 0x1;

        let mut vertex_b = Program::new(ShaderStage::VertexB);
        let mut vb_block = Block::new();
        vb_block.add_successor(0);
        vb_block.append_inst(Inst::new(Opcode::Prologue, Vec::new()));
        vb_block.append_inst(Inst::new(
            Opcode::Identity,
            vec![Value::Inst(InstRef { block: 0, inst: 0 })],
        ));
        vertex_b.blocks.push(vb_block);
        vertex_b.syntax_list = vec![SyntaxNode::Block(0), SyntaxNode::Return];
        vertex_b.post_order_blocks = vec![0];
        vertex_b.local_memory_size = 0x40;
        vertex_b.info.stores.mask[0] = 0x2;

        let result = merge_dual_vertex_programs(&mut vertex_a, &mut vertex_b);

        assert_eq!(result.stage, ShaderStage::VertexB);
        assert_eq!(result.local_memory_size, 0x40);
        assert_eq!(result.info.loads.mask[0], 0x1);
        assert_eq!(result.info.stores.mask[0], 0x2);
        assert!(matches!(result.syntax_list[0], SyntaxNode::Block(0)));
        assert!(matches!(result.syntax_list[1], SyntaxNode::Block(1)));
        assert!(matches!(result.syntax_list[2], SyntaxNode::Return));
        assert_eq!(result.post_order_blocks, vec![1, 0]);
        assert_eq!(result.blocks[1].imm_successors, vec![1]);
    }
}
