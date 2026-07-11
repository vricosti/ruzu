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
use crate::ir::program::{CbufDescriptor, Program, TexDescriptor};
use crate::ir::types::{TextureInstInfo, Type};
use crate::ir::value::{Attribute, Value};
use crate::program_header::{PixelImap, ProgramHeader};
use crate::shader_info::TextureType;

fn cbuf_type_bit(opcode: Opcode) -> u32 {
    match opcode {
        Opcode::GetCbufU8 | Opcode::GetCbufS8 => Type::U8 as u32,
        Opcode::GetCbufU16 | Opcode::GetCbufS16 => Type::U16 as u32,
        Opcode::GetCbufU32 => Type::U32 as u32,
        Opcode::GetCbufF32 => Type::F32 as u32,
        Opcode::GetCbufU32x2 => Type::U32x2 as u32,
        _ => 0,
    }
}

fn cbuf_element_size(opcode: Opcode) -> u32 {
    match opcode {
        Opcode::GetCbufU8 | Opcode::GetCbufS8 => 1,
        Opcode::GetCbufU16 | Opcode::GetCbufS16 => 2,
        Opcode::GetCbufU32 | Opcode::GetCbufF32 => 4,
        Opcode::GetCbufU32x2 => 8,
        _ => 0,
    }
}

/// Collect shader resource usage information.
pub fn collect_shader_info_pass(program: &mut Program) {
    let mut uses_local_memory = false;

    // Rebuild cbuf usage from the optimized IR. Frontend translation can
    // conservatively register cbufs before DCE removes the actual load; keeping
    // those stale mask bits makes OpenGL bind zero-sized UBO ranges.
    program.info.constant_buffer_mask = 0;
    program.info.constant_buffer_used_sizes = [0; crate::shader_info::Info::MAX_CBUFS];
    program.info.constant_buffer_descriptors.clear();
    program.info.used_constant_buffer_types = 0;

    let mut cbuf_set = std::collections::BTreeSet::<u32>::new();
    let mut tex_set = std::collections::BTreeSet::<u32>::new();

    for block in &program.blocks {
        for inst in block.iter() {
            match inst.opcode {
                // Constant buffer access
                Opcode::GetCbufU32
                | Opcode::GetCbufF32
                | Opcode::GetCbufU8
                | Opcode::GetCbufS8
                | Opcode::GetCbufU16
                | Opcode::GetCbufS16
                | Opcode::GetCbufU32x2 => {
                    if let Some(&Value::ImmU32(idx)) = inst.args.first() {
                        cbuf_set.insert(idx);
                        program.info.constant_buffer_mask |= 1u32 << idx;
                        let element_size = cbuf_element_size(inst.opcode);
                        let size = &mut program.info.constant_buffer_used_sizes[idx as usize];
                        if let Some(Value::ImmU32(offset)) = inst.args.get(1) {
                            *size = (*size).max(offset + element_size).div_ceil(16) * 16;
                        } else {
                            *size = 0x10000;
                        }
                        program.info.used_constant_buffer_types |= cbuf_type_bit(inst.opcode);
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
                Opcode::GetAttributeIndexed => {
                    program.info.loads_indexed_attributes = true;
                }
                Opcode::SetAttributeIndexed => {
                    program.info.stores_indexed_attributes = true;
                }
                Opcode::GetPatch => {
                    if let Some(Value::Patch(patch)) = inst.args.first() {
                        if patch.is_generic() {
                            program.info.uses_patches[patch.generic_index() as usize] = true;
                        }
                    }
                }
                Opcode::SetPatch => {
                    if let Some(Value::Patch(patch)) = inst.args.first() {
                        if patch.is_generic() {
                            program.info.uses_patches[patch.generic_index() as usize] = true;
                        } else {
                            match *patch {
                                crate::ir::value::Patch::TESS_LOD_LEFT
                                | crate::ir::value::Patch::TESS_LOD_TOP
                                | crate::ir::value::Patch::TESS_LOD_RIGHT
                                | crate::ir::value::Patch::TESS_LOD_BOTTOM => {
                                    program.info.stores_tess_level_outer = true;
                                }
                                crate::ir::value::Patch::TESS_LOD_INTERIOR_U
                                | crate::ir::value::Patch::TESS_LOD_INTERIOR_V => {
                                    program.info.stores_tess_level_inner = true;
                                }
                                _ => {}
                            }
                        }
                    }
                }

                // Fragment color output
                Opcode::SetFragColor => {
                    if let Some(Value::ImmU32(render_target)) = inst.args.first() {
                        if (*render_target as usize) < program.info.stores_frag_color.len() {
                            program.info.stores_frag_color[*render_target as usize] = true;
                        }
                    }
                }
                Opcode::SetSampleMask => {
                    program.info.stores_sample_mask = true;
                }
                Opcode::SetFragDepth => {
                    program.info.stores_frag_depth = true;
                }
                Opcode::WorkgroupId => {
                    program.info.uses_workgroup_id = true;
                }
                Opcode::LocalInvocationId => {
                    program.info.uses_local_invocation_id = true;
                }
                Opcode::InvocationId => {
                    program.info.uses_invocation_id = true;
                }
                Opcode::InvocationInfo => {
                    program.info.uses_invocation_info = true;
                }
                Opcode::SampleId => {
                    program.info.uses_sample_id = true;
                }
                Opcode::IsHelperInvocation => {
                    program.info.uses_is_helper_invocation = true;
                }
                Opcode::ResolutionDownFactor => {
                    program.info.uses_rescaling_uniform = true;
                }
                Opcode::RenderArea => {
                    program.info.uses_render_area = true;
                }
                Opcode::DemoteToHelperInvocation => {
                    program.info.uses_demote_to_helper_invocation = true;
                }

                // Texture access
                Opcode::BindlessImageSampleImplicitLod
                | Opcode::BindlessImageSampleExplicitLod
                | Opcode::BindlessImageSampleDrefImplicitLod
                | Opcode::BindlessImageSampleDrefExplicitLod
                | Opcode::BindlessImageGather
                | Opcode::BindlessImageGatherDref
                | Opcode::BindlessImageFetch
                | Opcode::BindlessImageQueryDimensions
                | Opcode::BindlessImageQueryLod
                | Opcode::BindlessImageGradient
                | Opcode::BoundImageSampleImplicitLod
                | Opcode::BoundImageSampleExplicitLod
                | Opcode::BoundImageSampleDrefImplicitLod
                | Opcode::BoundImageSampleDrefExplicitLod
                | Opcode::BoundImageGather
                | Opcode::BoundImageGatherDref
                | Opcode::BoundImageFetch
                | Opcode::BoundImageQueryDimensions
                | Opcode::BoundImageQueryLod
                | Opcode::BoundImageGradient
                | Opcode::ImageFetch
                | Opcode::ImageQueryDimensions
                | Opcode::ImageGradient
                | Opcode::ImageGather
                | Opcode::ImageGatherDref => {
                    if let Some(&Value::ImmU32(idx)) = inst.args.first() {
                        tex_set.insert(idx);
                    }
                    let flags = TextureInstInfo::from_u32(inst.flags);
                    let ty = TextureType::from_u8(flags.texture_type);
                    program.info.uses_sampled_1d |=
                        ty == TextureType::Color1D || ty == TextureType::ColorArray1D;
                    program.info.uses_sparse_residency |= inst
                        .get_associated_pseudo(Opcode::GetSparseFromOp)
                        .is_some();
                }
                Opcode::ImageSampleImplicitLod
                | Opcode::ImageSampleExplicitLod
                | Opcode::ImageSampleDrefImplicitLod
                | Opcode::ImageSampleDrefExplicitLod
                | Opcode::ImageQueryLod => {
                    if let Some(&Value::ImmU32(idx)) = inst.args.first() {
                        tex_set.insert(idx);
                    }
                    let flags = TextureInstInfo::from_u32(inst.flags);
                    let ty = TextureType::from_u8(flags.texture_type);
                    program.info.uses_sampled_1d |=
                        ty == TextureType::Color1D || ty == TextureType::ColorArray1D;
                    program.info.uses_shadow_lod |= flags.is_depth;
                    program.info.uses_sparse_residency |= inst
                        .get_associated_pseudo(Opcode::GetSparseFromOp)
                        .is_some();
                }

                // Local memory
                Opcode::LoadLocal | Opcode::WriteLocal => {
                    uses_local_memory = true;
                }

                // Global memory
                Opcode::LoadGlobalU8
                | Opcode::LoadGlobalS8
                | Opcode::LoadGlobalU16
                | Opcode::LoadGlobalS16
                | Opcode::LoadGlobal32
                | Opcode::LoadGlobal64
                | Opcode::LoadGlobal128 => {
                    program.info.uses_int64 = true;
                    program.info.uses_global_memory = true;
                    program.info.used_constant_buffer_types |=
                        (Type::U32 as u32) | (Type::U32x2 as u32);
                    program.info.used_storage_buffer_types |=
                        (Type::U32 as u32) | (Type::U32x2 as u32) | (Type::U32x4 as u32);
                }
                Opcode::WriteGlobalU8
                | Opcode::WriteGlobalS8
                | Opcode::WriteGlobalU16
                | Opcode::WriteGlobalS16
                | Opcode::WriteGlobal32
                | Opcode::WriteGlobal64
                | Opcode::WriteGlobal128 => {
                    program.info.stores_global_memory = true;
                    program.info.uses_int64 = true;
                    program.info.uses_global_memory = true;
                    program.info.used_constant_buffer_types |=
                        (Type::U32 as u32) | (Type::U32x2 as u32);
                    program.info.used_storage_buffer_types |=
                        (Type::U32 as u32) | (Type::U32x2 as u32) | (Type::U32x4 as u32);
                }
                Opcode::SharedAtomicSMin32
                | Opcode::StorageAtomicSMin32
                | Opcode::GlobalAtomicSMin32 => {
                    program.info.uses_atomic_s32_min = true;
                }
                Opcode::SharedAtomicSMax32
                | Opcode::StorageAtomicSMax32
                | Opcode::GlobalAtomicSMax32 => {
                    program.info.uses_atomic_s32_max = true;
                }

                _ => {}
            }
        }
    }

    program.info.constant_buffer_descriptors = cbuf_set
        .into_iter()
        .map(|index| CbufDescriptor { index, count: 1 })
        .collect();

    if program.info.texture_descriptors.is_empty() {
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
    }

    if uses_local_memory {
        program.info.uses_local_memory = true;
        if program.local_memory_size == 0 {
            program.local_memory_size = 0x1000;
        }
    }
}

/// Header-aware variant of upstream `CollectShaderInfoPass`.
///
/// Upstream calls `GatherInfoFromHeader(env, info)` after scanning IR. The
/// Rust port keeps the env-less pass for tests and legacy call sites, while
/// shader-cache paths that own the SPH call this variant.
pub fn collect_shader_info_pass_with_sph(program: &mut Program, sph: &ProgramHeader) {
    collect_shader_info_pass(program);
    gather_info_from_header(program, sph);
}

fn gather_info_from_header(program: &mut Program, sph: &ProgramHeader) {
    use crate::ir::types::ShaderStage;

    if program.stage == ShaderStage::Compute {
        return;
    }

    if program.stage == ShaderStage::Fragment {
        if !program.info.loads_indexed_attributes {
            return;
        }
        for index in 0..32 {
            let mask = sph.ps_generic_input_map(index);
            for (element, imap) in mask.iter().enumerate() {
                program.info.loads.set(
                    Attribute::generic(index, element as u32).0 as usize,
                    *imap != PixelImap::Unused,
                );
            }
        }
        return;
    }

    if program.info.loads_indexed_attributes {
        for index in 0..32 {
            let mask = sph.vtg_input_generic(index as usize);
            for (element, used) in mask.iter().enumerate() {
                program
                    .info
                    .loads
                    .set(Attribute::generic(index, element as u32).0 as usize, *used);
            }
        }
        set_clip_distances(&mut program.info.loads, sph.vtg_imap_systemc());
        set_systemb_attributes(&mut program.info.loads, sph.vtg_imap_systemb());
        set_systemc_attributes(&mut program.info.loads, sph.vtg_imap_systemc());
    }

    if program.info.stores_indexed_attributes {
        for index in 0..32 {
            let mask = sph.vtg_output_generic(index as usize);
            for (element, used) in mask.iter().enumerate() {
                program
                    .info
                    .stores
                    .set(Attribute::generic(index, element as u32).0 as usize, *used);
            }
        }
        let clip_mask = sph.vtg_omap_systemc();
        set_clip_distances(&mut program.info.stores, clip_mask);
        for index in 0..8 {
            if ((clip_mask >> index) & 1) != 0 {
                program.info.used_clip_distances = index + 1;
            }
        }
        set_systemb_attributes(&mut program.info.stores, sph.vtg_omap_systemb());
        set_systemc_attributes(&mut program.info.stores, sph.vtg_omap_systemc());
    }
}

fn set_clip_distances(state: &mut crate::varying_state::VaryingState, mask: u16) {
    for index in 0..8 {
        state.set(
            (Attribute::CLIP_DISTANCE_0.0 + index) as usize,
            ((mask >> index) & 1) != 0,
        );
    }
}

fn set_systemb_attributes(state: &mut crate::varying_state::VaryingState, mask: u8) {
    const ATTRIBUTES: [Attribute; 8] = [
        Attribute::PRIMITIVE_ID,
        Attribute::LAYER,
        Attribute::VIEWPORT_INDEX,
        Attribute::POINT_SIZE,
        Attribute::POSITION_X,
        Attribute::POSITION_Y,
        Attribute::POSITION_Z,
        Attribute::POSITION_W,
    ];
    for (index, attribute) in ATTRIBUTES.iter().enumerate() {
        state.set(attribute.0 as usize, ((mask >> index) & 1) != 0);
    }
}

fn set_systemc_attributes(state: &mut crate::varying_state::VaryingState, mask: u16) {
    const FOG_COORDINATE: Attribute = Attribute(186);
    const ATTRIBUTES: [(Attribute, u16); 7] = [
        (Attribute::POINT_SPRITE_S, 1 << 8),
        (Attribute::POINT_SPRITE_T, 1 << 9),
        (FOG_COORDINATE, 1 << 10),
        (Attribute::TESSELLATION_EVALUATION_POINT_U, 1 << 12),
        (Attribute::TESSELLATION_EVALUATION_POINT_V, 1 << 13),
        (Attribute::INSTANCE_ID, 1 << 14),
        (Attribute::VERTEX_ID, 1 << 15),
    ];
    for (attribute, bit) in ATTRIBUTES {
        state.set(attribute.0 as usize, (mask & bit) != 0);
    }
}

#[cfg(test)]
mod tests {
    use super::{collect_shader_info_pass, collect_shader_info_pass_with_sph};
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::{ShaderStage, TextureInstInfo};
    use crate::ir::value::{Attribute, Value};
    use crate::program_header::ProgramHeader;
    use crate::shader_info::TextureType;

    #[test]
    fn collect_info_records_demote_usage_like_upstream() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        program
            .block_mut(0)
            .append_inst(Inst::new(Opcode::DemoteToHelperInvocation, vec![]));

        collect_shader_info_pass(&mut program);

        assert!(program.info.uses_demote_to_helper_invocation);
    }

    #[test]
    fn collect_info_header_sets_fragment_indexed_generic_loads() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        program.block_mut(0).append_inst(Inst::new(
            Opcode::GetAttributeIndexed,
            vec![Value::ImmU32(0), Value::ImmU32(0)],
        ));

        let mut sph = ProgramHeader::default();
        sph.raw[6] = 0b11_10_01_00;

        collect_shader_info_pass_with_sph(&mut program, &sph);

        assert!(!program.info.loads.get(Attribute::generic(0, 0).0 as usize));
        assert!(program.info.loads.get(Attribute::generic(0, 1).0 as usize));
        assert!(program.info.loads.get(Attribute::generic(0, 2).0 as usize));
        assert!(program.info.loads.get(Attribute::generic(0, 3).0 as usize));
    }

    #[test]
    fn collect_info_sets_signed_atomic_helper_flags() {
        let mut program = Program::new(ShaderStage::Compute);
        program.blocks.push(Block::new());
        program.block_mut(0).append_inst(Inst::new(
            Opcode::StorageAtomicSMin32,
            vec![Value::ImmU32(0), Value::ImmU32(16), Value::ImmU32(7)],
        ));
        program.block_mut(0).append_inst(Inst::new(
            Opcode::SharedAtomicSMax32,
            vec![Value::ImmU32(0), Value::ImmU32(7)],
        ));

        super::collect_shader_info_pass(&mut program);

        assert!(program.info.uses_atomic_s32_min);
        assert!(program.info.uses_atomic_s32_max);
    }

    #[test]
    fn collect_info_marks_depth_texture_samples_as_shadow_lod_users() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let flags = TextureInstInfo {
            descriptor_index: 0,
            texture_type: TextureType::ColorArray2D as u8,
            is_depth: true,
            ..Default::default()
        };
        program.block_mut(0).append_inst(Inst::with_flags(
            Opcode::ImageSampleDrefExplicitLod,
            vec![
                Value::ImmU32(3),
                Value::ImmU32(0),
                Value::ImmU32(0),
                Value::ImmU32(0),
            ],
            flags.to_u32(),
        ));

        super::collect_shader_info_pass(&mut program);

        assert!(program.info.uses_shadow_lod);
        assert_eq!(program.info.texture_descriptors.len(), 1);
        assert_eq!(program.info.texture_descriptors[0].cbuf_index, 3);
    }

    #[test]
    fn collect_info_header_sets_vtg_indexed_generic_loads_and_stores() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.block_mut(0).append_inst(Inst::new(
            Opcode::GetAttributeIndexed,
            vec![Value::ImmU32(0), Value::ImmU32(0)],
        ));
        program.block_mut(0).append_inst(Inst::new(
            Opcode::SetAttributeIndexed,
            vec![Value::ImmU32(0), Value::ImmU32(0)],
        ));

        let mut sph = ProgramHeader::default();
        sph.raw[5] = 0b0000_0010 << 24;
        sph.raw[6] = 0b1010;
        sph.raw[10] = (0b0000_0011 | (1 << 14)) << 16;
        sph.raw[13] = (0b0000_1000 << 8) | (0b0101 << 16);
        sph.raw[18] = 0b1000_0000 | (1 << 9) | (1 << 15);

        collect_shader_info_pass_with_sph(&mut program, &sph);

        assert!(!program.info.loads.get(Attribute::PRIMITIVE_ID.0 as usize));
        assert!(program.info.loads.get(Attribute::LAYER.0 as usize));
        assert!(program
            .info
            .loads
            .get(Attribute::CLIP_DISTANCE_0.0 as usize));
        assert!(program
            .info
            .loads
            .get(Attribute::CLIP_DISTANCE_0.0 as usize + 1));
        assert!(program.info.loads.get(Attribute::INSTANCE_ID.0 as usize));

        assert!(!program.info.loads.get(Attribute::generic(0, 0).0 as usize));
        assert!(program.info.loads.get(Attribute::generic(0, 1).0 as usize));
        assert!(!program.info.loads.get(Attribute::generic(0, 2).0 as usize));
        assert!(program.info.loads.get(Attribute::generic(0, 3).0 as usize));

        assert!(program.info.stores.get(Attribute::POINT_SIZE.0 as usize));
        assert!(program
            .info
            .stores
            .get(Attribute::CLIP_DISTANCE_0.0 as usize + 7));
        assert!(program
            .info
            .stores
            .get(Attribute::POINT_SPRITE_T.0 as usize));
        assert!(program.info.stores.get(Attribute::VERTEX_ID.0 as usize));
        assert_eq!(program.info.used_clip_distances, 8);

        assert!(program.info.stores.get(Attribute::generic(0, 0).0 as usize));
        assert!(!program.info.stores.get(Attribute::generic(0, 1).0 as usize));
        assert!(program.info.stores.get(Attribute::generic(0, 2).0 as usize));
        assert!(!program.info.stores.get(Attribute::generic(0, 3).0 as usize));
    }
}
