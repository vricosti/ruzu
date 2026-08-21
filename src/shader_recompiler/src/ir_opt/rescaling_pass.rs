// SPDX-FileCopyrightText: Copyright 2026 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/rescaling_pass.cpp`.

use crate::ir::basic_block::Block;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::{Program, SyntaxNode};
use crate::ir::types::{ShaderStage, TextureInstInfo};
use crate::ir::value::{Attribute, InstRef, Value};
use crate::shader_info::TextureType;

const POSITION_SHUFFLE_MARKER: u32 = 0xDEAD_BEEF;

#[derive(Clone, Copy)]
struct ScalingConfig {
    up_scale: u32,
    down_shift: u32,
    up_factor: f32,
    down_factor: f32,
    rescale_hack: bool,
}

fn scaling_config() -> ScalingConfig {
    let values = common::settings::values();
    ScalingConfig {
        up_scale: values.resolution_info.up_scale,
        down_shift: values.resolution_info.down_shift,
        up_factor: values.resolution_info.up_factor,
        down_factor: values.resolution_info.down_factor,
        rescale_hack: *values.rescale_hack.get_value(),
    }
}

fn texture_type(info: TextureInstInfo) -> TextureType {
    TextureType::from_u8(info.texture_type)
}

fn is_texture_type_rescalable(texture_type: TextureType) -> bool {
    matches!(
        texture_type,
        TextureType::Color2D | TextureType::ColorArray2D | TextureType::Color2DRect
    )
}

fn insert_before(
    block: &mut Block,
    block_index: u32,
    before: u32,
    opcode: Opcode,
    args: Vec<Value>,
) -> Value {
    let inst = block.insert_inst_before(before, Inst::new(opcode, args));
    Value::Inst(InstRef {
        block: block_index,
        inst,
    })
}

fn clone_before(block: &mut Block, block_index: u32, before: u32, inst: &Inst) -> Value {
    let inst = block.clone_inst_before(before, inst);
    Value::Inst(InstRef {
        block: block_index,
        inst,
    })
}

fn inst_recursive(program: &Program, value: Value) -> Option<InstRef> {
    let mut current = match value {
        Value::Inst(inst) => inst,
        _ => return None,
    };
    loop {
        let inst = program.block(current.block).inst(current.inst);
        if inst.opcode != Opcode::Identity {
            return Some(current);
        }
        current = match inst.args.first().copied() {
            Some(Value::Inst(inst)) => inst,
            _ => return Some(current),
        };
    }
}

fn replace_uses_with(program: &mut Program, old: InstRef, replacement: Value) {
    let old = Value::Inst(old);
    for block in &mut program.blocks {
        for inst in block.iter_mut() {
            for arg in &mut inst.args {
                if *arg == old {
                    *arg = replacement;
                }
            }
            for (_, value) in &mut inst.phi_args {
                if *value == old {
                    *value = replacement;
                }
            }
        }
    }
    for node in &mut program.syntax_list {
        match node {
            SyntaxNode::If { cond, .. }
            | SyntaxNode::Repeat { cond, .. }
            | SyntaxNode::Break { cond, .. }
                if *cond == old =>
            {
                *cond = replacement
            }
            _ => {}
        }
    }
}

fn visit_mark(program: &mut Program, block_index: u32, inst_index: u32, config: ScalingConfig) {
    let inst = program.block(block_index).inst(inst_index);
    if !matches!(
        inst.opcode,
        Opcode::ShuffleIndex | Opcode::ShuffleUp | Opcode::ShuffleDown | Opcode::ShuffleButterfly
    ) {
        return;
    }
    let Some(shuffle_arg) = inst.args.first().copied() else {
        return;
    };
    if shuffle_arg.is_immediate() {
        return;
    }
    let Some(arg_ref) = inst_recursive(program, shuffle_arg) else {
        return;
    };
    let arg_inst = program.block(arg_ref.block).inst(arg_ref.inst);
    if arg_inst.opcode != Opcode::BitCastU32F32 {
        return;
    }
    let Some(bitcast_arg) = arg_inst.args.first().copied() else {
        return;
    };
    if bitcast_arg.is_immediate() {
        return;
    }
    let Some(attribute_ref) = inst_recursive(program, bitcast_arg) else {
        return;
    };
    let attribute_inst = program.block(attribute_ref.block).inst(attribute_ref.inst);
    if attribute_inst.opcode != Opcode::GetAttribute
        || !matches!(
            attribute_inst.args.first(),
            Some(Value::Attribute(
                Attribute::POSITION_X | Attribute::POSITION_Y
            ))
        )
    {
        return;
    }

    program
        .block_mut(attribute_ref.block)
        .inst_mut(attribute_ref.inst)
        .flags = POSITION_SHUFFLE_MARKER;

    let original = program.block(block_index).inst(inst_index).clone();
    let replacement = {
        let block = program.block_mut(block_index);
        let cloned = clone_before(block, block_index, inst_index, &original);
        let value = if config.rescale_hack {
            cloned
        } else {
            insert_before(
                block,
                block_index,
                inst_index,
                Opcode::ConvertF32U32,
                vec![cloned],
            )
        };
        let down_factor = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::ResolutionDownFactor,
            vec![],
        );
        let up_factor = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::FPRecip32,
            vec![down_factor],
        );
        insert_before(
            block,
            block_index,
            inst_index,
            Opcode::FPMul32,
            vec![value, up_factor],
        )
    };
    replace_uses_with(
        program,
        InstRef {
            block: block_index,
            inst: inst_index,
        },
        replacement,
    );
}

fn patch_frag_coord(program: &mut Program, block_index: u32, inst_index: u32) {
    let original = program.block(block_index).inst(inst_index).clone();
    let replacement = {
        let block = program.block_mut(block_index);
        let down_factor = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::ResolutionDownFactor,
            vec![],
        );
        let frag_coord = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::GetAttribute,
            original.args,
        );
        insert_before(
            block,
            block_index,
            inst_index,
            Opcode::FPMul32,
            vec![frag_coord, down_factor],
        )
    };
    replace_uses_with(
        program,
        InstRef {
            block: block_index,
            inst: inst_index,
        },
        replacement,
    );
}

fn patch_point_size(program: &mut Program, block_index: u32, inst_index: u32) {
    let point_value = program.block(block_index).inst(inst_index).args[1];
    let replacement = {
        let block = program.block_mut(block_index);
        let down_factor = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::ResolutionDownFactor,
            vec![],
        );
        let up_factor = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::FPRecip32,
            vec![down_factor],
        );
        insert_before(
            block,
            block_index,
            inst_index,
            Opcode::FPMul32,
            vec![point_value, up_factor],
        )
    };
    program.block_mut(block_index).inst_mut(inst_index).args[1] = replacement;
}

fn scale(
    block: &mut Block,
    block_index: u32,
    before: u32,
    is_scaled: Value,
    value: Value,
    config: ScalingConfig,
) -> Value {
    let mut scaled = value;
    if config.up_scale != 1 {
        scaled = insert_before(
            block,
            block_index,
            before,
            Opcode::IMul32,
            vec![scaled, Value::ImmU32(config.up_scale)],
        );
    }
    if config.down_shift != 0 {
        scaled = insert_before(
            block,
            block_index,
            before,
            Opcode::ShiftRightArithmetic32,
            vec![scaled, Value::ImmU32(config.down_shift)],
        );
    }
    insert_before(
        block,
        block_index,
        before,
        Opcode::SelectU32,
        vec![is_scaled, scaled, value],
    )
}

fn sub_scale(
    block: &mut Block,
    block_index: u32,
    before: u32,
    is_scaled: Value,
    value: Value,
    attribute: Attribute,
    config: ScalingConfig,
) -> Value {
    let as_float = insert_before(
        block,
        block_index,
        before,
        Opcode::ConvertF32U32,
        vec![value],
    );
    let base = insert_before(
        block,
        block_index,
        before,
        Opcode::FPMul32,
        vec![as_float, Value::ImmF32(config.up_factor)],
    );
    let frag_coord = insert_before(
        block,
        block_index,
        before,
        Opcode::GetAttribute,
        vec![Value::Attribute(attribute), Value::ImmU32(0)],
    );
    let downscaled_frag_coord = insert_before(
        block,
        block_index,
        before,
        Opcode::FPMul32,
        vec![frag_coord, Value::ImmF32(config.down_factor)],
    );
    let floor = insert_before(
        block,
        block_index,
        before,
        Opcode::FPFloor32,
        vec![downscaled_frag_coord],
    );
    let floor = insert_before(
        block,
        block_index,
        before,
        Opcode::FPMul32,
        vec![Value::ImmF32(config.up_factor), floor],
    );
    let negative_floor = insert_before(block, block_index, before, Opcode::FPNeg32, vec![floor]);
    let deviation = insert_before(
        block,
        block_index,
        before,
        Opcode::FPAdd32,
        vec![frag_coord, negative_floor],
    );
    let deviation = insert_before(
        block,
        block_index,
        before,
        Opcode::FPAdd32,
        vec![base, deviation],
    );
    let converted = insert_before(
        block,
        block_index,
        before,
        Opcode::ConvertU32F32,
        vec![deviation],
    );
    insert_before(
        block,
        block_index,
        before,
        Opcode::SelectU32,
        vec![is_scaled, converted, value],
    )
}

fn down_scale(
    block: &mut Block,
    block_index: u32,
    before: u32,
    is_scaled: Value,
    value: Value,
    config: ScalingConfig,
) -> Value {
    let mut scaled = value;
    if config.down_shift != 0 {
        scaled = insert_before(
            block,
            block_index,
            before,
            Opcode::ShiftLeftLogical32,
            vec![scaled, Value::ImmU32(config.down_shift)],
        );
    }
    if config.up_scale != 1 {
        scaled = insert_before(
            block,
            block_index,
            before,
            Opcode::UDiv32,
            vec![scaled, Value::ImmU32(config.up_scale)],
        );
    }
    insert_before(
        block,
        block_index,
        before,
        Opcode::SelectU32,
        vec![is_scaled, scaled, value],
    )
}

fn is_scaled(
    block: &mut Block,
    block_index: u32,
    before: u32,
    opcode: Opcode,
    descriptor_index: u16,
) -> Value {
    insert_before(
        block,
        block_index,
        before,
        opcode,
        vec![Value::ImmU32(descriptor_index as u32)],
    )
}

fn patch_image_query_dimensions(
    program: &mut Program,
    block_index: u32,
    inst_index: u32,
    config: ScalingConfig,
) {
    let original = program.block(block_index).inst(inst_index).clone();
    let info = TextureInstInfo::from_u32(original.flags);
    let replacement = {
        let block = program.block_mut(block_index);
        let scaled = is_scaled(
            block,
            block_index,
            inst_index,
            Opcode::IsTextureScaled,
            info.descriptor_index,
        );
        if !is_texture_type_rescalable(texture_type(info)) {
            return;
        }
        let cloned = clone_before(block, block_index, inst_index, &original);
        let width = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::CompositeExtractU32x4,
            vec![cloned, Value::ImmU32(0)],
        );
        let width = down_scale(block, block_index, inst_index, scaled, width, config);
        let height = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::CompositeExtractU32x4,
            vec![cloned, Value::ImmU32(1)],
        );
        let height = down_scale(block, block_index, inst_index, scaled, height, config);
        let depth = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::CompositeExtractU32x4,
            vec![cloned, Value::ImmU32(2)],
        );
        let levels = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::CompositeExtractU32x4,
            vec![cloned, Value::ImmU32(3)],
        );
        insert_before(
            block,
            block_index,
            inst_index,
            Opcode::CompositeConstructU32x4,
            vec![width, height, depth, levels],
        )
    };
    replace_uses_with(
        program,
        InstRef {
            block: block_index,
            inst: inst_index,
        },
        replacement,
    );
}

fn extract_coord(
    block: &mut Block,
    block_index: u32,
    before: u32,
    composite: Value,
    texture_type: TextureType,
    index: u32,
) -> Value {
    let opcode = if texture_type == TextureType::ColorArray2D {
        Opcode::CompositeExtractU32x3
    } else {
        Opcode::CompositeExtractU32x2
    };
    insert_before(
        block,
        block_index,
        before,
        opcode,
        vec![composite, Value::ImmU32(index)],
    )
}

fn scale_integer_composite(
    block: &mut Block,
    block_index: u32,
    inst_index: u32,
    is_scaled: Value,
    index: usize,
    config: ScalingConfig,
) {
    let composite = block.inst(inst_index).args[index];
    if composite == Value::Void {
        return;
    }
    let info = TextureInstInfo::from_u32(block.inst(inst_index).flags);
    let texture_type = texture_type(info);
    let x = extract_coord(block, block_index, inst_index, composite, texture_type, 0);
    let x = scale(block, block_index, inst_index, is_scaled, x, config);
    let y = extract_coord(block, block_index, inst_index, composite, texture_type, 1);
    let y = scale(block, block_index, inst_index, is_scaled, y, config);
    let replacement = match texture_type {
        TextureType::Color2D | TextureType::Color2DRect => insert_before(
            block,
            block_index,
            inst_index,
            Opcode::CompositeConstructU32x2,
            vec![x, y],
        ),
        TextureType::ColorArray2D => {
            let z = extract_coord(block, block_index, inst_index, composite, texture_type, 2);
            insert_before(
                block,
                block_index,
                inst_index,
                Opcode::CompositeConstructU32x3,
                vec![x, y, z],
            )
        }
        _ => return,
    };
    block.inst_mut(inst_index).args[index] = replacement;
}

fn scale_integer_offset_composite(
    block: &mut Block,
    block_index: u32,
    inst_index: u32,
    is_scaled: Value,
    index: usize,
    config: ScalingConfig,
) {
    let composite = block.inst(inst_index).args[index];
    if composite == Value::Void {
        return;
    }
    let info = TextureInstInfo::from_u32(block.inst(inst_index).flags);
    let texture_type = texture_type(info);
    let x = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeExtractU32x2,
        vec![composite, Value::ImmU32(0)],
    );
    let x = scale(block, block_index, inst_index, is_scaled, x, config);
    let y = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeExtractU32x2,
        vec![composite, Value::ImmU32(1)],
    );
    let y = scale(block, block_index, inst_index, is_scaled, y, config);
    if is_texture_type_rescalable(texture_type) {
        block.inst_mut(inst_index).args[index] = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::CompositeConstructU32x2,
            vec![x, y],
        );
    }
}

fn sub_scale_coord(
    block: &mut Block,
    block_index: u32,
    inst_index: u32,
    is_scaled: Value,
    config: ScalingConfig,
) {
    let info = TextureInstInfo::from_u32(block.inst(inst_index).flags);
    let texture_type = texture_type(info);
    let coord = block.inst(inst_index).args[1];
    let x = extract_coord(block, block_index, inst_index, coord, texture_type, 0);
    let y = extract_coord(block, block_index, inst_index, coord, texture_type, 1);
    let x = sub_scale(
        block,
        block_index,
        inst_index,
        is_scaled,
        x,
        Attribute::POSITION_X,
        config,
    );
    let y = sub_scale(
        block,
        block_index,
        inst_index,
        is_scaled,
        y,
        Attribute::POSITION_Y,
        config,
    );
    let replacement = match texture_type {
        TextureType::Color2D | TextureType::Color2DRect => insert_before(
            block,
            block_index,
            inst_index,
            Opcode::CompositeConstructU32x2,
            vec![x, y],
        ),
        TextureType::ColorArray2D => {
            let z = extract_coord(block, block_index, inst_index, coord, texture_type, 2);
            insert_before(
                block,
                block_index,
                inst_index,
                Opcode::CompositeConstructU32x3,
                vec![x, y, z],
            )
        }
        _ => return,
    };
    block.inst_mut(inst_index).args[1] = replacement;
}

fn sub_scale_image_fetch(
    block: &mut Block,
    block_index: u32,
    inst_index: u32,
    config: ScalingConfig,
) {
    let info = TextureInstInfo::from_u32(block.inst(inst_index).flags);
    if !is_texture_type_rescalable(texture_type(info)) {
        return;
    }
    let scaled = is_scaled(
        block,
        block_index,
        inst_index,
        Opcode::IsTextureScaled,
        info.descriptor_index,
    );
    sub_scale_coord(block, block_index, inst_index, scaled, config);
    scale_integer_offset_composite(block, block_index, inst_index, scaled, 2, config);
}

fn sub_scale_image_read(
    block: &mut Block,
    block_index: u32,
    inst_index: u32,
    config: ScalingConfig,
) {
    let info = TextureInstInfo::from_u32(block.inst(inst_index).flags);
    if !is_texture_type_rescalable(texture_type(info)) {
        return;
    }
    let scaled = is_scaled(
        block,
        block_index,
        inst_index,
        Opcode::IsImageScaled,
        info.descriptor_index,
    );
    sub_scale_coord(block, block_index, inst_index, scaled, config);
}

fn patch_image_fetch(block: &mut Block, block_index: u32, inst_index: u32, config: ScalingConfig) {
    let info = TextureInstInfo::from_u32(block.inst(inst_index).flags);
    if !is_texture_type_rescalable(texture_type(info)) {
        return;
    }
    let scaled = is_scaled(
        block,
        block_index,
        inst_index,
        Opcode::IsTextureScaled,
        info.descriptor_index,
    );
    scale_integer_composite(block, block_index, inst_index, scaled, 1, config);
    scale_integer_offset_composite(block, block_index, inst_index, scaled, 2, config);
}

fn patch_image_read(block: &mut Block, block_index: u32, inst_index: u32, config: ScalingConfig) {
    let info = TextureInstInfo::from_u32(block.inst(inst_index).flags);
    if !is_texture_type_rescalable(texture_type(info)) {
        return;
    }
    let scaled = is_scaled(
        block,
        block_index,
        inst_index,
        Opcode::IsImageScaled,
        info.descriptor_index,
    );
    scale_integer_composite(block, block_index, inst_index, scaled, 1, config);
}

fn visit(program: &mut Program, block_index: u32, inst_index: u32, config: ScalingConfig) {
    let inst = program.block(block_index).inst(inst_index);
    let opcode = inst.opcode;
    let flags = inst.flags;
    let attribute = inst.args.first().copied();
    match opcode {
        Opcode::GetAttribute => {
            if program.stage == ShaderStage::Fragment
                && flags != POSITION_SHUFFLE_MARKER
                && matches!(
                    attribute,
                    Some(Value::Attribute(
                        Attribute::POSITION_X | Attribute::POSITION_Y
                    ))
                )
            {
                patch_frag_coord(program, block_index, inst_index);
            }
        }
        Opcode::SetAttribute => {
            if flags != POSITION_SHUFFLE_MARKER
                && attribute == Some(Value::Attribute(Attribute::POINT_SIZE))
            {
                patch_point_size(program, block_index, inst_index);
            }
        }
        Opcode::ImageQueryDimensions => {
            patch_image_query_dimensions(program, block_index, inst_index, config)
        }
        Opcode::ImageFetch if program.stage == ShaderStage::Fragment => sub_scale_image_fetch(
            program.block_mut(block_index),
            block_index,
            inst_index,
            config,
        ),
        Opcode::ImageFetch => patch_image_fetch(
            program.block_mut(block_index),
            block_index,
            inst_index,
            config,
        ),
        Opcode::ImageRead if program.stage == ShaderStage::Fragment => sub_scale_image_read(
            program.block_mut(block_index),
            block_index,
            inst_index,
            config,
        ),
        Opcode::ImageRead => patch_image_read(
            program.block_mut(block_index),
            block_index,
            inst_index,
            config,
        ),
        _ => {}
    }
}

/// Apply resolution rescaling to texture operations.
pub fn rescaling_pass(program: &mut Program) {
    let config = scaling_config();
    if program.stage == ShaderStage::Fragment {
        for block_index in program.post_order_blocks.clone() {
            let instructions: Vec<u32> = program
                .block(block_index)
                .indexed_iter()
                .map(|(index, _)| index)
                .collect();
            for inst_index in instructions {
                visit_mark(program, block_index, inst_index, config);
            }
        }
    }
    for block_index in program.post_order_blocks.clone() {
        let instructions: Vec<u32> = program
            .block(block_index)
            .indexed_iter()
            .map(|(index, _)| index)
            .collect();
        for inst_index in instructions {
            visit(program, block_index, inst_index, config);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn program_with(stage: ShaderStage, inst: Inst) -> (Program, InstRef) {
        let mut program = Program::new(stage);
        let block = program.add_block();
        program.post_order_blocks.push(block);
        let inst = program.block_mut(block).append_inst(inst);
        (program, InstRef { block, inst })
    }

    fn opcodes(program: &Program, block: u32) -> Vec<Opcode> {
        program
            .block(block)
            .iter()
            .map(|inst| inst.opcode)
            .collect()
    }

    #[test]
    fn fragment_position_is_downscaled() {
        let (mut program, position) = program_with(
            ShaderStage::Fragment,
            Inst::new(
                Opcode::GetAttribute,
                vec![Value::Attribute(Attribute::POSITION_X), Value::ImmU32(0)],
            ),
        );
        let user = program.block_mut(0).append_inst(Inst::new(
            Opcode::FPMul32,
            vec![Value::Inst(position), Value::ImmF32(2.0)],
        ));

        rescaling_pass(&mut program);

        assert!(opcodes(&program, 0).contains(&Opcode::ResolutionDownFactor));
        assert_ne!(program.block(0).inst(user).args[0], Value::Inst(position));
    }

    #[test]
    fn image_query_dimensions_checks_texture_scale_mask() {
        let info = TextureInstInfo {
            descriptor_index: 3,
            texture_type: TextureType::Color2D as u8,
            ..TextureInstInfo::default()
        };
        let (mut program, query) = program_with(
            ShaderStage::VertexB,
            Inst::with_flags(
                Opcode::ImageQueryDimensions,
                vec![Value::ImmU32(0), Value::ImmU32(0), Value::ImmU1(false)],
                info.to_u32(),
            ),
        );
        let user = program.block_mut(0).append_inst(Inst::new(
            Opcode::CompositeExtractU32x4,
            vec![Value::Inst(query), Value::ImmU32(0)],
        ));

        rescaling_pass(&mut program);

        assert!(opcodes(&program, 0).contains(&Opcode::IsTextureScaled));
        assert_ne!(program.block(0).inst(user).args[0], Value::Inst(query));
    }

    #[test]
    fn non_rescalable_image_fetch_is_unchanged() {
        let info = TextureInstInfo {
            descriptor_index: 1,
            texture_type: TextureType::Color3D as u8,
            ..TextureInstInfo::default()
        };
        let (mut program, fetch) = program_with(
            ShaderStage::VertexB,
            Inst::with_flags(
                Opcode::ImageFetch,
                vec![
                    Value::ImmU32(0),
                    Value::Void,
                    Value::Void,
                    Value::ImmU32(0),
                    Value::ImmU32(0),
                ],
                info.to_u32(),
            ),
        );

        rescaling_pass(&mut program);

        assert_eq!(program.block(0).inst(fetch.inst).args[1], Value::Void);
        assert!(!opcodes(&program, 0).contains(&Opcode::IsTextureScaled));
    }
}
