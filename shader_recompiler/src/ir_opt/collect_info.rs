// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Collect shader info pass — scan IR to determine resource usage.
//!
//! Matches zuyu's `collect_shader_info_pass.cpp`.
//!
//! Scans all instructions to determine which constant buffers, textures,
//! generic attributes, and storage buffers are used. Populates `Info`
//! via `VaryingState` for loads/stores (upstream-faithful pattern).

use crate::ir::opcodes::Opcode;
use crate::ir::program::{CbufDescriptor, Program, StorageDescriptor, TexDescriptor};
use crate::ir::value::Value;

/// Collect shader resource usage information.
pub fn collect_shader_info_pass(program: &mut Program) {
    let mut uses_local_memory = false;

    let mut cbuf_set = std::collections::BTreeSet::<u32>::new();
    let mut tex_set = std::collections::BTreeSet::<u32>::new();

    for block in &program.blocks {
        for inst in &block.instructions {
            match inst.opcode {
                // Constant buffer access
                Opcode::GetCbufU32
                | Opcode::GetCbufF32
                | Opcode::GetCbufU8
                | Opcode::GetCbufS8
                | Opcode::GetCbufU16
                | Opcode::GetCbufS16 => {
                    if let Some(&Value::ImmU32(idx)) = inst.args.first() {
                        cbuf_set.insert(idx);
                    }
                }

                // Attribute loads → VaryingState
                Opcode::GetAttribute | Opcode::GetAttributeU32 => {
                    if let Some(Value::Attribute(attr)) = inst.args.first() {
                        program.info.loads.set(attr.0 as usize, true);
                    }
                }

                // Attribute stores → VaryingState
                Opcode::SetAttribute => {
                    if let Some(Value::Attribute(attr)) = inst.args.first() {
                        program.info.stores.set(attr.0 as usize, true);
                    }
                }

                // Fragment color output
                Opcode::SetFragColor => {
                    // Fragment shaders always store to render target 0+
                }

                // Texture access
                Opcode::ImageSampleImplicitLod
                | Opcode::ImageSampleExplicitLod
                | Opcode::ImageSampleDrefImplicitLod
                | Opcode::ImageSampleDrefExplicitLod
                | Opcode::ImageFetch
                | Opcode::ImageQueryDimensions
                | Opcode::ImageGather
                | Opcode::ImageGatherDref => {
                    if let Some(&Value::ImmU32(idx)) = inst.args.first() {
                        tex_set.insert(idx);
                    }
                }

                // Local memory
                Opcode::LoadLocal | Opcode::WriteLocal => {
                    uses_local_memory = true;
                }

                _ => {}
            }
        }
    }

    program.info.constant_buffer_descriptors = cbuf_set
        .into_iter()
        .map(|index| CbufDescriptor { index, count: 1 })
        .collect();

    program.info.texture_descriptors = tex_set
        .into_iter()
        .map(|index| TexDescriptor {
            cbuf_index: index,
            texture_type: crate::shader_info::TextureType::Color2D,
            is_depth: false,
            is_multisample: false,
            has_secondary: false,
            cbuf_offset: 0,
            shift_left: 0,
            secondary_cbuf_index: 0,
            secondary_cbuf_offset: 0,
            secondary_shift_left: 0,
            count: 1,
            size_shift: 0,
        })
        .collect();

    if uses_local_memory {
        program.info.uses_local_memory = true;
        if program.local_memory_size == 0 {
            program.local_memory_size = 0x1000;
        }
    }
}
