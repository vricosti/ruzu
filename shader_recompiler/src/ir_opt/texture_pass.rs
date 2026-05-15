// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/texture_pass.cpp`
//!
//! Resolves bindless and bound texture operations to indexed texture
//! operations. Tracks constant buffer addresses that reference texture
//! descriptors and rewrites texture instructions with concrete descriptor
//! indices.

use crate::environment::Environment;
use crate::host_translate_info::HostTranslateInfo;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::{Program, ShaderInfo};
use crate::ir::types::TextureInstInfo;
use crate::ir::value::{InstRef, Value};
use crate::shader_info::{
    ImageBufferDescriptor, ImageDescriptor, ImageFormat, TextureBufferDescriptor,
    TextureDescriptor, TexturePixelFormat, TextureType,
};

const DESCRIPTOR_SIZE_SHIFT: u32 = 3;
const DESCRIPTOR_SIZE: u32 = 8;

#[derive(Debug, Clone, Copy)]
struct ConstBufferAddr {
    index: u32,
    offset: u32,
    shift_left: u32,
    secondary_index: u32,
    secondary_offset: u32,
    secondary_shift_left: u32,
    dynamic_offset: Value,
    count: u32,
    has_secondary: bool,
}

impl ConstBufferAddr {
    fn bound(index: u32, offset: u32) -> Self {
        Self {
            index,
            offset,
            shift_left: 0,
            secondary_index: 0,
            secondary_offset: 0,
            secondary_shift_left: 0,
            dynamic_offset: Value::Void,
            count: 1,
            has_secondary: false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct TextureInst {
    block: u32,
    inst: u32,
    cbuf: ConstBufferAddr,
}

/// Resolve bindless/bound texture operations to indexed operations.
pub fn texture_pass(
    env: &mut dyn Environment,
    program: &mut Program,
    host_info: &HostTranslateInfo,
) {
    texture_pass_impl(program, host_info, Some(env), None);
}

/// Bound-texture subset of upstream `TexturePass`.
///
/// Upstream bound texture instructions enter `TexturePass` with Arg(0) as a
/// byte offset into `env.TextureBoundBuffer()`. The pass creates/deduplicates
/// a `TextureDescriptor`, then rewrites Arg(0) to the compact descriptor index
/// used by backends. Bindless tracking, texture buffers, images, multisample,
/// dynamic arrays and TIC-driven type refinement are still missing.
pub fn texture_pass_bound_textures(program: &mut Program, texture_bound_buffer: u32) {
    texture_pass_impl(
        program,
        &HostTranslateInfo::default(),
        None,
        Some(texture_bound_buffer),
    );
}

fn texture_pass_impl(
    program: &mut Program,
    host_info: &HostTranslateInfo,
    env: Option<&mut dyn Environment>,
    texture_bound_buffer_override: Option<u32>,
) {
    let mut to_replace = Vec::new();
    let mut env = env;

    for (block_index, block) in program.blocks.iter().enumerate() {
        for (inst_index, inst) in block.instructions.iter().enumerate() {
            let Some(inst) = inst.as_ref() else {
                continue;
            };
            if !is_texture_instruction(inst.opcode) {
                continue;
            }
            let Some(handle) = inst.args.first().copied() else {
                continue;
            };
            let cbuf = match handle {
                Value::ImmU32(cbuf_offset) if !is_bindless(inst.opcode) => {
                    let texture_bound_buffer = texture_bound_buffer_override
                        .or_else(|| env.as_ref().map(|env| env.texture_bound_buffer()))
                        .unwrap_or(0);
                    ConstBufferAddr::bound(texture_bound_buffer, cbuf_offset)
                }
                value => match track(value, program) {
                    Some(cbuf) => cbuf,
                    None => {
                        log::warn!(
                            "TexturePass: failed to track texture handle for {:?} at {}:{}",
                            inst.opcode,
                            block_index,
                            inst_index
                        );
                        continue;
                    }
                },
            };
            to_replace.push(TextureInst {
                block: block_index as u32,
                inst: inst_index as u32,
                cbuf,
            });
        }
    }

    to_replace.sort_by_key(|texture_inst| (texture_inst.cbuf.index, texture_inst.cbuf.offset));
    if !to_replace.is_empty() {
        // Upstream `TexturePass` owns the texture/image descriptor vectors.
        // The Rust translators still opportunistically call `register_texture`
        // while lowering instructions; discard those pre-pass placeholders so
        // descriptor_index points at the rebuilt upstream-style arrays.
        program.info.texture_buffer_descriptors.clear();
        program.info.image_buffer_descriptors.clear();
        program.info.texture_descriptors.clear();
        program.info.image_descriptors.clear();
    }

    for texture_inst in to_replace {
        let inst_snapshot = program.blocks[texture_inst.block as usize].instructions
            [texture_inst.inst as usize]
            .as_ref()
            .expect("texture inst collected from live slot")
            .clone();
        let mut flags = TextureInstInfo::from_u32(inst_snapshot.flags);
        let cbuf = texture_inst.cbuf;
        let indexed_opcode =
            indexed_instruction(inst_snapshot.opcode).unwrap_or(inst_snapshot.opcode);
        let texture_type = env
            .as_deref_mut()
            .map(|env| read_texture_type(env, &cbuf))
            .unwrap_or_else(|| texture_type_from_flags(flags));
        if indexed_opcode == Opcode::ImageQueryDimensions {
            flags.texture_type = texture_type as u8;
        } else if indexed_opcode == Opcode::ImageFetch
            && texture_type_from_flags(flags) == TextureType::Color1D
            && texture_type == TextureType::Buffer
        {
            flags.texture_type = TextureType::Buffer as u8;
        }
        let image_fetch_can_be_multisample = indexed_opcode == Opcode::ImageFetch
            && matches!(
                texture_type_from_flags(flags),
                TextureType::Color2D | TextureType::Color2DRect | TextureType::ColorArray2D
            );
        let is_multisample = image_fetch_can_be_multisample
            && !matches!(inst_snapshot.args.get(4), None | Some(Value::Void));

        let descriptor_index = if is_storage_image_instruction(indexed_opcode) {
            if cbuf.has_secondary {
                log::warn!("TexturePass: unexpected separate sampler for image op");
                continue;
            }
            let is_integer = env
                .as_deref_mut()
                .map(|env| is_texture_pixel_format_integer(env, &cbuf))
                .unwrap_or(false);
            if texture_type_from_flags(flags) == TextureType::Buffer {
                add_image_buffer_descriptor(
                    &mut program.info.image_buffer_descriptors,
                    ImageBufferDescriptor {
                        format: image_format_from_flags(flags),
                        is_written: indexed_opcode != Opcode::ImageRead,
                        is_read: indexed_opcode != Opcode::ImageWrite,
                        is_integer,
                        cbuf_index: cbuf.index,
                        cbuf_offset: cbuf.offset,
                        count: cbuf.count,
                        size_shift: DESCRIPTOR_SIZE_SHIFT,
                    },
                )
            } else {
                add_image_descriptor(
                    &mut program.info.image_descriptors,
                    ImageDescriptor {
                        texture_type: texture_type_from_flags(flags),
                        format: image_format_from_flags(flags),
                        is_written: indexed_opcode != Opcode::ImageRead,
                        is_read: indexed_opcode != Opcode::ImageWrite,
                        is_integer,
                        cbuf_index: cbuf.index,
                        cbuf_offset: cbuf.offset,
                        count: cbuf.count,
                        size_shift: DESCRIPTOR_SIZE_SHIFT,
                    },
                )
            }
        } else if flags.texture_type == TextureType::Buffer as u8 {
            add_texture_buffer_descriptor(
                &mut program.info.texture_buffer_descriptors,
                TextureBufferDescriptor {
                    has_secondary: cbuf.has_secondary,
                    cbuf_index: cbuf.index,
                    cbuf_offset: cbuf.offset,
                    shift_left: cbuf.shift_left,
                    secondary_cbuf_index: cbuf.secondary_index,
                    secondary_cbuf_offset: cbuf.secondary_offset,
                    secondary_shift_left: cbuf.secondary_shift_left,
                    count: cbuf.count,
                    size_shift: DESCRIPTOR_SIZE_SHIFT,
                },
            )
        } else {
            let descriptor = TextureDescriptor {
                texture_type: texture_type_from_flags(flags),
                is_depth: flags.is_depth,
                is_multisample,
                has_secondary: cbuf.has_secondary,
                cbuf_index: cbuf.index,
                cbuf_offset: cbuf.offset,
                shift_left: cbuf.shift_left,
                secondary_cbuf_index: cbuf.secondary_index,
                secondary_cbuf_offset: cbuf.secondary_offset,
                secondary_shift_left: cbuf.secondary_shift_left,
                count: cbuf.count,
                size_shift: DESCRIPTOR_SIZE_SHIFT,
            };
            add_texture_descriptor(&mut program.info.texture_descriptors, descriptor)
        };

        let inst = program.blocks[texture_inst.block as usize].instructions
            [texture_inst.inst as usize]
            .as_mut()
            .expect("texture inst collected from live slot");
        inst.opcode = indexed_opcode;
        flags.descriptor_index = descriptor_index as u16;
        inst.flags = flags.to_u32();
        if indexed_opcode == Opcode::ImageFetch && !image_fetch_can_be_multisample {
            if inst.args.len() > 4 {
                inst.args[4] = Value::Void;
            }
        }
        inst.args[0] = if cbuf.count > 1 {
            // Upstream emits `umin(asr(dynamic_offset, 3), 7)` here. Ruzu's
            // GLSL image emission does not consume dynamic descriptor indices
            // yet, so keep the dynamic SSA value visible instead of erasing it.
            cbuf.dynamic_offset
        } else {
            Value::ImmU32(descriptor_index)
        };

        if !host_info.support_snorm_render_buffer
            && indexed_opcode == Opcode::ImageFetch
            && texture_type_from_flags(flags) == TextureType::Buffer
        {
            if let Some(env) = env.as_deref_mut() {
                let pixel_format = read_texture_pixel_format(env, &cbuf);
                if is_pixel_format_snorm(pixel_format) {
                    log::warn!("TexturePass: SNORM texel-fetch patch not yet ported");
                }
            }
        }
    }
}

fn get_inst<'a>(program: &'a Program, inst_ref: InstRef) -> Option<&'a Inst> {
    program
        .blocks
        .get(inst_ref.block as usize)?
        .instructions
        .get(inst_ref.inst as usize)?
        .as_ref()
}

fn track(value: Value, program: &Program) -> Option<ConstBufferAddr> {
    track_inner(value, program, 0)
}

fn track_inner(value: Value, program: &Program, depth: u32) -> Option<ConstBufferAddr> {
    if depth > 16 {
        return None;
    }
    let Value::Inst(inst_ref) = value else {
        return None;
    };
    let inst = get_inst(program, inst_ref)?;
    try_get_const_buffer(inst, program, depth + 1)
}

fn try_get_const_buffer(inst: &Inst, program: &Program, depth: u32) -> Option<ConstBufferAddr> {
    match inst.opcode {
        Opcode::BitwiseOr32 => {
            let mut lhs = track_inner(*inst.args.first()?, program, depth)?;
            let mut rhs = track_inner(*inst.args.get(1)?, program, depth)?;
            if lhs.has_secondary || rhs.has_secondary || lhs.count > 1 || rhs.count > 1 {
                return None;
            }
            if lhs.shift_left > 0 || lhs.index > rhs.index || lhs.offset > rhs.offset {
                std::mem::swap(&mut lhs, &mut rhs);
            }
            Some(ConstBufferAddr {
                index: lhs.index,
                offset: lhs.offset,
                shift_left: lhs.shift_left,
                secondary_index: rhs.index,
                secondary_offset: rhs.offset,
                secondary_shift_left: rhs.shift_left,
                dynamic_offset: Value::Void,
                count: 1,
                has_secondary: true,
            })
        }
        Opcode::ShiftLeftLogical32 => {
            let shift = inst.args.get(1)?;
            if !shift.is_immediate() {
                return None;
            }
            let mut lhs = track_inner(*inst.args.first()?, program, depth)?;
            lhs.shift_left = shift.imm_u32();
            Some(lhs)
        }
        Opcode::BitwiseAnd32 => {
            let mut op1 = *inst.args.first()?;
            let mut op2 = *inst.args.get(1)?;
            if op1.is_immediate() {
                std::mem::swap(&mut op1, &mut op2);
            }
            if !op2.is_immediate() && !op1.is_immediate() {
                if let Some(value) = try_get_constant(op1, program) {
                    op1 = op2;
                    op2 = Value::ImmU32(value);
                } else if let Some(value) = try_get_constant(op2, program) {
                    op2 = Value::ImmU32(value);
                } else {
                    return None;
                }
            } else if !op2.is_immediate() {
                return None;
            }
            let mask = op2.imm_u32();
            if mask == 0 {
                return None;
            }
            let mut lhs = track_inner(op1, program, depth)?;
            lhs.shift_left = mask.trailing_zeros();
            Some(lhs)
        }
        Opcode::GetCbufU32 | Opcode::GetCbufU32x2 => {
            let index = *inst.args.first()?;
            let offset = *inst.args.get(1)?;
            if !index.is_immediate() {
                return None;
            }
            if offset.is_immediate() {
                return Some(ConstBufferAddr::bound(index.imm_u32(), offset.imm_u32()));
            }
            let Value::Inst(offset_ref) = offset else {
                return None;
            };
            let offset_inst = get_inst(program, offset_ref)?;
            if offset_inst.opcode != Opcode::IAdd32 {
                return None;
            }
            let lhs = *offset_inst.args.first()?;
            let rhs = *offset_inst.args.get(1)?;
            if lhs.is_immediate() {
                Some(ConstBufferAddr {
                    count: DESCRIPTOR_SIZE,
                    dynamic_offset: rhs,
                    ..ConstBufferAddr::bound(index.imm_u32(), lhs.imm_u32())
                })
            } else if rhs.is_immediate() {
                Some(ConstBufferAddr {
                    count: DESCRIPTOR_SIZE,
                    dynamic_offset: lhs,
                    ..ConstBufferAddr::bound(index.imm_u32(), rhs.imm_u32())
                })
            } else {
                None
            }
        }
        _ => None,
    }
}

fn try_get_constant(value: Value, program: &Program) -> Option<u32> {
    let Value::Inst(inst_ref) = value else {
        return None;
    };
    let inst = get_inst(program, inst_ref)?;
    if inst.opcode != Opcode::GetCbufU32 {
        return None;
    }
    let index = *inst.args.first()?;
    let _offset = *inst.args.get(1)?;
    if !index.is_immediate() || !_offset.is_immediate() || index.imm_u32() != 1 {
        return None;
    }
    None
}

fn is_texture_instruction(opcode: Opcode) -> bool {
    indexed_instruction(opcode).is_some()
}

fn indexed_instruction(opcode: Opcode) -> Option<Opcode> {
    Some(match opcode {
        Opcode::BindlessImageSampleImplicitLod | Opcode::BoundImageSampleImplicitLod => {
            Opcode::ImageSampleImplicitLod
        }
        Opcode::BindlessImageSampleExplicitLod | Opcode::BoundImageSampleExplicitLod => {
            Opcode::ImageSampleExplicitLod
        }
        Opcode::BindlessImageSampleDrefImplicitLod | Opcode::BoundImageSampleDrefImplicitLod => {
            Opcode::ImageSampleDrefImplicitLod
        }
        Opcode::BindlessImageSampleDrefExplicitLod | Opcode::BoundImageSampleDrefExplicitLod => {
            Opcode::ImageSampleDrefExplicitLod
        }
        Opcode::BindlessImageGather | Opcode::BoundImageGather => Opcode::ImageGather,
        Opcode::BindlessImageGatherDref | Opcode::BoundImageGatherDref => Opcode::ImageGatherDref,
        Opcode::BindlessImageFetch | Opcode::BoundImageFetch => Opcode::ImageFetch,
        Opcode::BindlessImageQueryDimensions | Opcode::BoundImageQueryDimensions => {
            Opcode::ImageQueryDimensions
        }
        Opcode::BindlessImageQueryLod | Opcode::BoundImageQueryLod => Opcode::ImageQueryLod,
        Opcode::BindlessImageGradient | Opcode::BoundImageGradient => Opcode::ImageGradient,
        Opcode::BindlessImageRead | Opcode::BoundImageRead => Opcode::ImageRead,
        Opcode::BindlessImageWrite | Opcode::BoundImageWrite => Opcode::ImageWrite,
        Opcode::BindlessImageAtomicIAdd32 | Opcode::BoundImageAtomicIAdd32 => {
            Opcode::ImageAtomicIAdd32
        }
        Opcode::BindlessImageAtomicSMin32 | Opcode::BoundImageAtomicSMin32 => {
            Opcode::ImageAtomicSMin32
        }
        Opcode::BindlessImageAtomicUMin32 | Opcode::BoundImageAtomicUMin32 => {
            Opcode::ImageAtomicUMin32
        }
        Opcode::BindlessImageAtomicSMax32 | Opcode::BoundImageAtomicSMax32 => {
            Opcode::ImageAtomicSMax32
        }
        Opcode::BindlessImageAtomicUMax32 | Opcode::BoundImageAtomicUMax32 => {
            Opcode::ImageAtomicUMax32
        }
        Opcode::BindlessImageAtomicInc32 | Opcode::BoundImageAtomicInc32 => {
            Opcode::ImageAtomicInc32
        }
        Opcode::BindlessImageAtomicDec32 | Opcode::BoundImageAtomicDec32 => {
            Opcode::ImageAtomicDec32
        }
        Opcode::BindlessImageAtomicAnd32 | Opcode::BoundImageAtomicAnd32 => {
            Opcode::ImageAtomicAnd32
        }
        Opcode::BindlessImageAtomicOr32 | Opcode::BoundImageAtomicOr32 => Opcode::ImageAtomicOr32,
        Opcode::BindlessImageAtomicXor32 | Opcode::BoundImageAtomicXor32 => {
            Opcode::ImageAtomicXor32
        }
        Opcode::BindlessImageAtomicExchange32 | Opcode::BoundImageAtomicExchange32 => {
            Opcode::ImageAtomicExchange32
        }
        Opcode::ImageSampleImplicitLod
        | Opcode::ImageSampleExplicitLod
        | Opcode::ImageSampleDrefImplicitLod
        | Opcode::ImageSampleDrefExplicitLod
        | Opcode::ImageFetch
        | Opcode::ImageQueryDimensions
        | Opcode::ImageGather
        | Opcode::ImageGatherDref
        | Opcode::ImageQueryLod
        | Opcode::ImageGradient
        | Opcode::ImageRead
        | Opcode::ImageWrite
        | Opcode::ImageAtomicIAdd32
        | Opcode::ImageAtomicSMin32
        | Opcode::ImageAtomicUMin32
        | Opcode::ImageAtomicSMax32
        | Opcode::ImageAtomicUMax32
        | Opcode::ImageAtomicInc32
        | Opcode::ImageAtomicDec32
        | Opcode::ImageAtomicAnd32
        | Opcode::ImageAtomicOr32
        | Opcode::ImageAtomicXor32
        | Opcode::ImageAtomicExchange32 => opcode,
        _ => return None,
    })
}

fn is_bindless(opcode: Opcode) -> bool {
    matches!(
        opcode,
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
            | Opcode::BindlessImageRead
            | Opcode::BindlessImageWrite
            | Opcode::BindlessImageAtomicIAdd32
            | Opcode::BindlessImageAtomicSMin32
            | Opcode::BindlessImageAtomicUMin32
            | Opcode::BindlessImageAtomicSMax32
            | Opcode::BindlessImageAtomicUMax32
            | Opcode::BindlessImageAtomicInc32
            | Opcode::BindlessImageAtomicDec32
            | Opcode::BindlessImageAtomicAnd32
            | Opcode::BindlessImageAtomicOr32
            | Opcode::BindlessImageAtomicXor32
            | Opcode::BindlessImageAtomicExchange32
    )
}

fn is_storage_image_instruction(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::ImageRead
            | Opcode::ImageWrite
            | Opcode::ImageAtomicIAdd32
            | Opcode::ImageAtomicSMin32
            | Opcode::ImageAtomicUMin32
            | Opcode::ImageAtomicSMax32
            | Opcode::ImageAtomicUMax32
            | Opcode::ImageAtomicInc32
            | Opcode::ImageAtomicDec32
            | Opcode::ImageAtomicAnd32
            | Opcode::ImageAtomicOr32
            | Opcode::ImageAtomicXor32
            | Opcode::ImageAtomicExchange32
    )
}

fn texture_type_from_flags(flags: TextureInstInfo) -> TextureType {
    match flags.texture_type {
        0 => TextureType::Color1D,
        1 | 2 => TextureType::Color2D,
        3 => TextureType::Color3D,
        4 => TextureType::ColorCube,
        5 => TextureType::ColorArray1D,
        6 => TextureType::ColorArray2D,
        _ => TextureType::Color2D,
    }
}

fn image_format_from_flags(_flags: TextureInstInfo) -> ImageFormat {
    ImageFormat::Typeless
}

fn get_texture_handle(env: &mut dyn Environment, cbuf: &ConstBufferAddr) -> u32 {
    let secondary_index = if cbuf.has_secondary {
        cbuf.secondary_index
    } else {
        cbuf.index
    };
    let secondary_offset = if cbuf.has_secondary {
        cbuf.secondary_offset
    } else {
        cbuf.offset
    };
    let lhs_raw = env.read_cbuf_value(cbuf.index, cbuf.offset) << cbuf.shift_left;
    let rhs_raw =
        env.read_cbuf_value(secondary_index, secondary_offset) << cbuf.secondary_shift_left;
    lhs_raw | rhs_raw
}

fn read_texture_type(env: &mut dyn Environment, cbuf: &ConstBufferAddr) -> TextureType {
    let handle = get_texture_handle(env, cbuf);
    env.read_texture_type(handle)
}

fn read_texture_pixel_format(
    env: &mut dyn Environment,
    cbuf: &ConstBufferAddr,
) -> TexturePixelFormat {
    let handle = get_texture_handle(env, cbuf);
    env.read_texture_pixel_format(handle)
}

fn is_texture_pixel_format_integer(env: &mut dyn Environment, cbuf: &ConstBufferAddr) -> bool {
    let handle = get_texture_handle(env, cbuf);
    env.is_texture_pixel_format_integer(handle)
}

fn is_pixel_format_snorm(format: TexturePixelFormat) -> bool {
    matches!(
        format,
        TexturePixelFormat::A8B8G8R8Snorm
            | TexturePixelFormat::R8G8Snorm
            | TexturePixelFormat::R8Snorm
            | TexturePixelFormat::R16G16B16A16Snorm
            | TexturePixelFormat::R16G16Snorm
            | TexturePixelFormat::R16Snorm
    )
}

fn add_texture_descriptor(
    descriptors: &mut Vec<TextureDescriptor>,
    descriptor: TextureDescriptor,
) -> u32 {
    if let Some(index) = descriptors.iter().position(|existing| {
        existing.texture_type == descriptor.texture_type
            && existing.is_depth == descriptor.is_depth
            && existing.has_secondary == descriptor.has_secondary
            && existing.cbuf_index == descriptor.cbuf_index
            && existing.cbuf_offset == descriptor.cbuf_offset
            && existing.shift_left == descriptor.shift_left
            && existing.secondary_cbuf_index == descriptor.secondary_cbuf_index
            && existing.secondary_cbuf_offset == descriptor.secondary_cbuf_offset
            && existing.secondary_shift_left == descriptor.secondary_shift_left
            && existing.count == descriptor.count
            && existing.size_shift == descriptor.size_shift
    }) {
        descriptors[index].is_multisample |= descriptor.is_multisample;
        return index as u32;
    }
    descriptors.push(descriptor);
    (descriptors.len() - 1) as u32
}

fn add_texture_buffer_descriptor(
    descriptors: &mut Vec<TextureBufferDescriptor>,
    descriptor: TextureBufferDescriptor,
) -> u32 {
    if let Some(index) = descriptors
        .iter()
        .position(|existing| *existing == descriptor)
    {
        return index as u32;
    }
    descriptors.push(descriptor);
    (descriptors.len() - 1) as u32
}

fn add_image_descriptor(
    descriptors: &mut Vec<ImageDescriptor>,
    descriptor: ImageDescriptor,
) -> u32 {
    if let Some(index) = descriptors.iter().position(|existing| {
        existing.texture_type == descriptor.texture_type
            && existing.format == descriptor.format
            && existing.cbuf_index == descriptor.cbuf_index
            && existing.cbuf_offset == descriptor.cbuf_offset
            && existing.count == descriptor.count
            && existing.size_shift == descriptor.size_shift
    }) {
        let existing = &mut descriptors[index];
        existing.is_written |= descriptor.is_written;
        existing.is_read |= descriptor.is_read;
        existing.is_integer |= descriptor.is_integer;
        return index as u32;
    }
    descriptors.push(descriptor);
    (descriptors.len() - 1) as u32
}

fn add_image_buffer_descriptor(
    descriptors: &mut Vec<ImageBufferDescriptor>,
    descriptor: ImageBufferDescriptor,
) -> u32 {
    if let Some(index) = descriptors.iter().position(|existing| {
        existing.format == descriptor.format
            && existing.cbuf_index == descriptor.cbuf_index
            && existing.cbuf_offset == descriptor.cbuf_offset
            && existing.count == descriptor.count
            && existing.size_shift == descriptor.size_shift
    }) {
        let existing = &mut descriptors[index];
        existing.is_written |= descriptor.is_written;
        existing.is_read |= descriptor.is_read;
        existing.is_integer |= descriptor.is_integer;
        return index as u32;
    }
    descriptors.push(descriptor);
    (descriptors.len() - 1) as u32
}

/// Join texture descriptors from `source` into `base`.
///
/// Upstream: `JoinTextureInfo` in `texture_pass.cpp`.
///
/// Not yet implemented: requires the full texture descriptor tracking infrastructure.
pub fn join_texture_info(_base: &mut ShaderInfo, _source: &mut ShaderInfo) {
    for desc in _source.texture_buffer_descriptors.drain(..) {
        add_texture_buffer_descriptor(&mut _base.texture_buffer_descriptors, desc);
    }
    for desc in _source.image_buffer_descriptors.drain(..) {
        if let Some(index) = _base.image_buffer_descriptors.iter().position(|existing| {
            existing.format == desc.format
                && existing.cbuf_index == desc.cbuf_index
                && existing.cbuf_offset == desc.cbuf_offset
                && existing.count == desc.count
                && existing.size_shift == desc.size_shift
        }) {
            let existing = &mut _base.image_buffer_descriptors[index];
            existing.is_written |= desc.is_written;
            existing.is_read |= desc.is_read;
            existing.is_integer |= desc.is_integer;
        } else {
            _base.image_buffer_descriptors.push(desc);
        }
    }
    for desc in _source.texture_descriptors.drain(..) {
        add_texture_descriptor(&mut _base.texture_descriptors, desc);
    }
    for desc in _source.image_descriptors.drain(..) {
        add_image_descriptor(&mut _base.image_descriptors, desc);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;
    use crate::ir::types::ShaderStage;

    #[test]
    fn bound_texture_pass_rewrites_handle_to_descriptor_index() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let flags = TextureInstInfo {
            texture_type: 2,
            ..Default::default()
        }
        .to_u32();
        program.blocks[0].instructions.push(Some(Inst::with_flags(
            Opcode::ImageSampleImplicitLod,
            vec![Value::ImmU32(0x28), Value::Void],
            flags,
        )));

        texture_pass_bound_textures(&mut program, 3);

        assert_eq!(program.info.texture_descriptors.len(), 1);
        let desc = &program.info.texture_descriptors[0];
        assert_eq!(desc.cbuf_index, 3);
        assert_eq!(desc.cbuf_offset, 0x28);
        assert_eq!(desc.size_shift, DESCRIPTOR_SIZE_SHIFT);
        let inst = program.blocks[0].instructions[0].as_ref().unwrap();
        assert_eq!(inst.args[0], Value::ImmU32(0));
        let flags = TextureInstInfo::from_u32(inst.flags);
        assert_eq!(flags.descriptor_index, 0);
    }

    #[test]
    fn texture_pass_tracks_separate_texture_sampler_or_handle() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = &mut program.blocks[0];
        block.instructions.push(Some(Inst::new(
            Opcode::GetCbufU32,
            vec![Value::ImmU32(2), Value::ImmU32(0x40)],
        )));
        block.instructions.push(Some(Inst::new(
            Opcode::ShiftLeftLogical32,
            vec![
                Value::Inst(crate::ir::value::InstRef { block: 0, inst: 0 }),
                Value::ImmU32(8),
            ],
        )));
        block.instructions.push(Some(Inst::new(
            Opcode::GetCbufU32,
            vec![Value::ImmU32(3), Value::ImmU32(0x20)],
        )));
        block.instructions.push(Some(Inst::new(
            Opcode::BitwiseOr32,
            vec![
                Value::Inst(crate::ir::value::InstRef { block: 0, inst: 1 }),
                Value::Inst(crate::ir::value::InstRef { block: 0, inst: 2 }),
            ],
        )));
        let flags = TextureInstInfo {
            texture_type: 2,
            ..Default::default()
        }
        .to_u32();
        block.instructions.push(Some(Inst::with_flags(
            Opcode::ImageSampleImplicitLod,
            vec![
                Value::Inst(crate::ir::value::InstRef { block: 0, inst: 3 }),
                Value::Void,
            ],
            flags,
        )));

        texture_pass_bound_textures(&mut program, 0);

        assert_eq!(program.info.texture_descriptors.len(), 1);
        let desc = &program.info.texture_descriptors[0];
        assert!(desc.has_secondary);
        assert_eq!(desc.cbuf_index, 3);
        assert_eq!(desc.cbuf_offset, 0x20);
        assert_eq!(desc.shift_left, 0);
        assert_eq!(desc.secondary_cbuf_index, 2);
        assert_eq!(desc.secondary_cbuf_offset, 0x40);
        assert_eq!(desc.secondary_shift_left, 8);
        let inst = program.blocks[0].instructions[4].as_ref().unwrap();
        assert_eq!(inst.args[0], Value::ImmU32(0));
        assert_eq!(TextureInstInfo::from_u32(inst.flags).descriptor_index, 0);
    }

    #[test]
    fn texture_pass_tracks_dynamic_cbuf_offset_as_descriptor_array() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = &mut program.blocks[0];
        block.instructions.push(Some(Inst::new(
            Opcode::IAdd32,
            vec![Value::ImmU32(0x80), Value::Reg(crate::ir::value::Reg(4))],
        )));
        block.instructions.push(Some(Inst::new(
            Opcode::GetCbufU32,
            vec![
                Value::ImmU32(5),
                Value::Inst(crate::ir::value::InstRef { block: 0, inst: 0 }),
            ],
        )));
        let flags = TextureInstInfo {
            texture_type: 2,
            ..Default::default()
        }
        .to_u32();
        block.instructions.push(Some(Inst::with_flags(
            Opcode::ImageSampleImplicitLod,
            vec![
                Value::Inst(crate::ir::value::InstRef { block: 0, inst: 1 }),
                Value::Void,
            ],
            flags,
        )));

        texture_pass_bound_textures(&mut program, 0);

        assert_eq!(program.info.texture_descriptors.len(), 1);
        let desc = &program.info.texture_descriptors[0];
        assert_eq!(desc.cbuf_index, 5);
        assert_eq!(desc.cbuf_offset, 0x80);
        assert_eq!(desc.count, DESCRIPTOR_SIZE);
        assert_eq!(desc.size_shift, DESCRIPTOR_SIZE_SHIFT);
        let inst = program.blocks[0].instructions[2].as_ref().unwrap();
        assert_eq!(inst.args[0], Value::Reg(crate::ir::value::Reg(4)));
    }

    #[test]
    fn texture_pass_normalizes_bound_opcode_to_indexed_opcode() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let flags = TextureInstInfo {
            texture_type: 2,
            ..Default::default()
        }
        .to_u32();
        program.blocks[0].instructions.push(Some(Inst::with_flags(
            Opcode::BoundImageSampleImplicitLod,
            vec![Value::ImmU32(0x30), Value::Void],
            flags,
        )));

        texture_pass_bound_textures(&mut program, 4);

        let inst = program.blocks[0].instructions[0].as_ref().unwrap();
        assert_eq!(inst.opcode, Opcode::ImageSampleImplicitLod);
        assert_eq!(inst.args[0], Value::ImmU32(0));
        assert_eq!(program.info.texture_descriptors[0].cbuf_index, 4);
        assert_eq!(program.info.texture_descriptors[0].cbuf_offset, 0x30);
    }

    #[test]
    fn texture_pass_registers_image_atomic_as_image_descriptor() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let flags = TextureInstInfo {
            texture_type: 2,
            ..Default::default()
        }
        .to_u32();
        program.blocks[0].instructions.push(Some(Inst::with_flags(
            Opcode::BoundImageAtomicIAdd32,
            vec![Value::ImmU32(0x38), Value::Void, Value::ImmU32(1)],
            flags,
        )));

        texture_pass_bound_textures(&mut program, 6);

        let inst = program.blocks[0].instructions[0].as_ref().unwrap();
        assert_eq!(inst.opcode, Opcode::ImageAtomicIAdd32);
        assert_eq!(program.info.image_descriptors.len(), 1);
        let desc = &program.info.image_descriptors[0];
        assert_eq!(desc.cbuf_index, 6);
        assert_eq!(desc.cbuf_offset, 0x38);
        assert!(desc.is_read);
        assert!(desc.is_written);
    }

    #[test]
    fn join_texture_info_dedupes_and_merges_image_flags() {
        let mut base = ShaderInfo::default();
        let mut source = ShaderInfo::default();
        base.image_descriptors.push(ImageDescriptor {
            texture_type: TextureType::Color2D,
            format: ImageFormat::R32Uint,
            is_written: false,
            is_read: true,
            is_integer: false,
            cbuf_index: 1,
            cbuf_offset: 0x20,
            count: 1,
            size_shift: DESCRIPTOR_SIZE_SHIFT,
        });
        source.image_descriptors.push(ImageDescriptor {
            texture_type: TextureType::Color2D,
            format: ImageFormat::R32Uint,
            is_written: true,
            is_read: false,
            is_integer: true,
            cbuf_index: 1,
            cbuf_offset: 0x20,
            count: 1,
            size_shift: DESCRIPTOR_SIZE_SHIFT,
        });

        join_texture_info(&mut base, &mut source);

        assert!(source.image_descriptors.is_empty());
        assert_eq!(base.image_descriptors.len(), 1);
        let desc = &base.image_descriptors[0];
        assert!(desc.is_read);
        assert!(desc.is_written);
        assert!(desc.is_integer);
    }

    #[test]
    fn image_fetch_clears_ms_arg_for_non_2d_fetches() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let flags = TextureInstInfo {
            texture_type: TextureType::Color1D as u8,
            ..Default::default()
        }
        .to_u32();
        program.blocks[0].instructions.push(Some(Inst::with_flags(
            Opcode::BoundImageFetch,
            vec![
                Value::ImmU32(0x48),
                Value::Void,
                Value::Void,
                Value::Void,
                Value::ImmU32(7),
            ],
            flags,
        )));

        texture_pass_bound_textures(&mut program, 2);

        let inst = program.blocks[0].instructions[0].as_ref().unwrap();
        assert_eq!(inst.opcode, Opcode::ImageFetch);
        assert_eq!(inst.args[4], Value::Void);
    }

    #[test]
    fn join_texture_info_dedupes_and_merges_multisample_flag() {
        let mut base = ShaderInfo::default();
        let mut source = ShaderInfo::default();
        base.texture_descriptors.push(TextureDescriptor {
            texture_type: TextureType::Color2D,
            is_depth: false,
            is_multisample: false,
            has_secondary: false,
            cbuf_index: 1,
            cbuf_offset: 0x20,
            shift_left: 0,
            secondary_cbuf_index: 1,
            secondary_cbuf_offset: 0x20,
            secondary_shift_left: 0,
            count: 1,
            size_shift: DESCRIPTOR_SIZE_SHIFT,
        });
        source.texture_descriptors.push(TextureDescriptor {
            texture_type: TextureType::Color2D,
            is_depth: false,
            is_multisample: true,
            has_secondary: false,
            cbuf_index: 1,
            cbuf_offset: 0x20,
            shift_left: 0,
            secondary_cbuf_index: 1,
            secondary_cbuf_offset: 0x20,
            secondary_shift_left: 0,
            count: 1,
            size_shift: DESCRIPTOR_SIZE_SHIFT,
        });

        join_texture_info(&mut base, &mut source);

        assert!(source.texture_descriptors.is_empty());
        assert_eq!(base.texture_descriptors.len(), 1);
        assert!(base.texture_descriptors[0].is_multisample);
    }
}
