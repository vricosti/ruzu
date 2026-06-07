// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/layer_pass.cpp`
//!
//! Emulates gl_Layer output on GPUs that don't support setting Layer
//! from non-geometry shader stages. Rewrites Layer attribute stores
//! to use an unused generic attribute instead.

use crate::host_translate_info::HostTranslateInfo;
use crate::ir::opcodes::Opcode;
use crate::ir::program::Program;
use crate::ir::types::ShaderStage;
use crate::ir::value::{Attribute, Value};
use crate::varying_state::VaryingState;

fn emulated_layer_attribute(stores: &VaryingState) -> Attribute {
    for index in 0..32 {
        if !stores.generic_any(index) {
            return Attribute::generic(index as u32, 0);
        }
    }
    Attribute::LAYER
}

fn permitted_program_stage(stage: ShaderStage) -> bool {
    matches!(
        stage,
        ShaderStage::VertexA
            | ShaderStage::VertexB
            | ShaderStage::TessellationControl
            | ShaderStage::TessellationEval
    )
}

/// Port of upstream `Optimization::LayerPass`.
pub fn layer_pass(program: &mut Program, host_info: &HostTranslateInfo) {
    if host_info.support_viewport_index_layer || !permitted_program_stage(program.stage) {
        return;
    }

    let layer_attribute = emulated_layer_attribute(&program.info.stores);
    let mut requires_layer_emulation = false;

    for block_index in program.post_order_blocks.clone() {
        let Some(block) = program.blocks.get_mut(block_index as usize) else {
            continue;
        };
        for inst in block.iter_mut() {
            if inst.opcode == Opcode::SetAttribute
                && matches!(inst.arg(0), Value::Attribute(Attribute::LAYER))
            {
                requires_layer_emulation = true;
                inst.set_arg(0, Value::Attribute(layer_attribute));
            }
        }
    }

    if requires_layer_emulation {
        program.info.requires_layer_emulation = true;
        program.info.emulated_layer = layer_attribute.0 as u64;
        program.info.stores.set(Attribute::LAYER.0 as usize, false);
        program.info.stores.set(layer_attribute.0 as usize, true);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;

    fn program_with_layer_store(stage: ShaderStage) -> Program {
        let mut program = Program::new(stage);
        let mut block = Block::new();
        block.append_inst(Inst::new(
            Opcode::SetAttribute,
            vec![
                Value::Attribute(Attribute::LAYER),
                Value::ImmF32(1.0),
                Value::ImmU32(0),
            ],
        ));
        program.blocks.push(block);
        program.post_order_blocks.push(0);
        program.info.stores.set(Attribute::LAYER.0 as usize, true);
        program
    }

    fn stored_attribute(program: &Program) -> Attribute {
        match program.blocks[0].inst(0).arg(0) {
            Value::Attribute(attribute) => *attribute,
            other => panic!("expected attribute arg, got {other:?}"),
        }
    }

    #[test]
    fn layer_pass_emulates_layer_store_on_vertex_stage_without_native_support() {
        let mut program = program_with_layer_store(ShaderStage::VertexB);

        layer_pass(&mut program, &HostTranslateInfo::default());

        assert_eq!(stored_attribute(&program), Attribute::generic(0, 0));
        assert!(program.info.requires_layer_emulation);
        assert_eq!(
            program.info.emulated_layer,
            Attribute::generic(0, 0).0 as u64
        );
        assert!(!program.info.stores.get(Attribute::LAYER.0 as usize));
        assert!(program.info.stores.get(Attribute::generic(0, 0).0 as usize));
    }

    #[test]
    fn layer_pass_keeps_layer_store_when_native_support_exists() {
        let mut program = program_with_layer_store(ShaderStage::VertexB);
        let host_info = HostTranslateInfo {
            support_viewport_index_layer: true,
            ..HostTranslateInfo::default()
        };

        layer_pass(&mut program, &host_info);

        assert_eq!(stored_attribute(&program), Attribute::LAYER);
        assert!(!program.info.requires_layer_emulation);
        assert!(program.info.stores.get(Attribute::LAYER.0 as usize));
    }

    #[test]
    fn layer_pass_keeps_layer_store_on_non_permitted_stage() {
        let mut program = program_with_layer_store(ShaderStage::Geometry);

        layer_pass(&mut program, &HostTranslateInfo::default());

        assert_eq!(stored_attribute(&program), Attribute::LAYER);
        assert!(!program.info.requires_layer_emulation);
        assert!(program.info.stores.get(Attribute::LAYER.0 as usize));
    }

    #[test]
    fn layer_pass_uses_first_unused_generic_attribute() {
        let mut program = program_with_layer_store(ShaderStage::TessellationControl);
        program
            .info
            .stores
            .set(Attribute::generic(0, 0).0 as usize, true);

        layer_pass(&mut program, &HostTranslateInfo::default());

        assert_eq!(stored_attribute(&program), Attribute::generic(1, 0));
        assert_eq!(
            program.info.emulated_layer,
            Attribute::generic(1, 0).0 as u64
        );
    }
}
