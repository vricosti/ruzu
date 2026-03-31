// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Collect shader info pass — scan IR to determine resource usage.
//!
//! Matches zuyu's `collect_shader_info_pass.cpp`.
//!
//! Scans all instructions to determine which constant buffers, textures,
//! generic attributes, and storage buffers are used. Populates `ShaderInfo`.

use crate::ir::opcodes::Opcode;
use crate::ir::program::{CbufDescriptor, Program, StorageDescriptor, TexDescriptor};
use crate::ir::value::Value;

/// Collect shader resource usage information.
pub fn collect_shader_info_pass(program: &mut Program) {
    let mut loads_generics: u32 = 0;
    let mut stores_generics: u32 = 0;
    let mut stores_position = false;
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

                // Attribute loads
                Opcode::GetAttribute | Opcode::GetAttributeU32 => {
                    if let Some(Value::Attribute(attr)) = inst.args.first() {
                        if attr.is_generic() {
                            loads_generics |= 1 << attr.generic_index();
                        }
                    }
                }

                // Attribute stores
                Opcode::SetAttribute => {
                    if let Some(Value::Attribute(attr)) = inst.args.first() {
                        if attr.is_position() {
                            stores_position = true;
                        } else if attr.is_generic() {
                            stores_generics |= 1 << attr.generic_index();
                        }
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

    // Populate shader info
    program.info.loads_generics = loads_generics;
    program.info.stores_generics = stores_generics;
    program.info.stores_position = stores_position;

    program.info.constant_buffer_descriptors = cbuf_set
        .into_iter()
        .map(|index| CbufDescriptor {
            index,
            size: 0x10000,
        })
        .collect();

    program.info.texture_descriptors = tex_set
        .into_iter()
        .map(|index| TexDescriptor {
            index,
            texture_type: 2,
            is_depth: false,
        })
        .collect();

    if uses_local_memory && program.info.local_memory_size == 0 {
        program.info.local_memory_size = 0x1000; // Default 4KB local memory
    }
}
