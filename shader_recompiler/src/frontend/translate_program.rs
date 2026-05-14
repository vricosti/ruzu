// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `frontend/maxwell/translate_program.cpp`
//!
//! Top-level shader translation: takes a CFG and environment, translates
//! each block using the TranslatorVisitor, builds the structured control
//! flow AST, and returns an IR::Program.

use crate::ir::basic_block::Block;
use crate::ir::program::{Program, SyntaxNode};
use crate::ir::types::ShaderStage;
use crate::ir::value::{InstRef, Value};
use crate::ir_opt;

/// Translate a Maxwell shader program from CFG to IR.
///
/// Not yet implemented: requires full CFG construction, block translation loop,
/// and structured control-flow AST building.
pub fn translate_program(_instructions: &[u64], stage: crate::ir::types::ShaderStage) -> Program {
    log::warn!("TranslateProgram not yet implemented — returning empty program");
    Program::new(stage)
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
///
/// Not yet implemented: requires attribute mapping tables and instruction mutation.
pub fn convert_legacy_to_generic(
    _program: &mut Program,
    _runtime_info: &crate::runtime_info::RuntimeInfo,
) {
    log::warn!("ConvertLegacyToGeneric not yet implemented — attributes left unmapped");
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
    for inst in &mut block.instructions {
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
    fn merge_dual_vertex_programs_remaps_vertex_b_block_references() {
        let mut vertex_a = Program::new(ShaderStage::VertexA);
        let mut va_block = Block::new();
        va_block
            .instructions
            .push(Inst::new(Opcode::Epilogue, Vec::new()));
        vertex_a.blocks.push(va_block);
        vertex_a.syntax_list = vec![SyntaxNode::Block(0), SyntaxNode::Return];
        vertex_a.post_order_blocks = vec![0];
        vertex_a.local_memory_size = 0x20;
        vertex_a.info.loads.mask[0] = 0x1;

        let mut vertex_b = Program::new(ShaderStage::VertexB);
        let mut vb_block = Block::new();
        vb_block.add_successor(0);
        vb_block
            .instructions
            .push(Inst::new(Opcode::Prologue, Vec::new()));
        vb_block.instructions.push(Inst::new(
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
