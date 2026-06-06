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
use crate::ir::basic_block::Block;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::{Program, ShaderInfo, SyntaxNode};
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
                value => match track(value, program, &mut env) {
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

        if indexed_opcode == Opcode::ImageSampleImplicitLod
            && texture_type_from_flags(flags) == TextureType::Color2D
            && texture_type == TextureType::Color2DRect
        {
            patch_image_sample_implicit_lod(
                &mut program.blocks[texture_inst.block as usize],
                texture_inst.block,
                texture_inst.inst,
                flags,
            );
        }

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
        let dynamic_index = if cbuf.count > 1 {
            let block = &mut program.blocks[texture_inst.block as usize];
            let shifted = insert_before(
                block,
                texture_inst.block,
                texture_inst.inst,
                Opcode::ShiftRightArithmetic32,
                vec![cbuf.dynamic_offset, Value::ImmU32(DESCRIPTOR_SIZE_SHIFT)],
            );
            insert_before(
                block,
                texture_inst.block,
                texture_inst.inst,
                Opcode::UMin32,
                vec![shifted, Value::ImmU32(DESCRIPTOR_SIZE - 1)],
            )
        } else {
            Value::Void
        };
        let inst = program.blocks[texture_inst.block as usize].instructions
            [texture_inst.inst as usize]
            .as_mut()
            .expect("texture inst collected from live slot");
        inst.args[0] = dynamic_index;

        if !host_info.support_snorm_render_buffer
            && indexed_opcode == Opcode::ImageFetch
            && texture_type_from_flags(flags) == TextureType::Buffer
        {
            if let Some(env) = env.as_deref_mut() {
                let pixel_format = read_texture_pixel_format(env, &cbuf);
                if is_pixel_format_snorm(pixel_format) {
                    patch_texel_fetch(program, texture_inst.block, texture_inst.inst, pixel_format);
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

fn track(
    value: Value,
    program: &Program,
    env: &mut Option<&mut dyn Environment>,
) -> Option<ConstBufferAddr> {
    track_inner(value, program, env, 0)
}

fn track_inner(
    value: Value,
    program: &Program,
    env: &mut Option<&mut dyn Environment>,
    depth: u32,
) -> Option<ConstBufferAddr> {
    if depth > 16 {
        return None;
    }
    let Value::Inst(inst_ref) = value else {
        return None;
    };
    let inst = get_inst(program, inst_ref)?;
    try_get_const_buffer(inst, program, env, depth + 1)
}

fn try_get_const_buffer(
    inst: &Inst,
    program: &Program,
    env: &mut Option<&mut dyn Environment>,
    depth: u32,
) -> Option<ConstBufferAddr> {
    match inst.opcode {
        Opcode::BitwiseOr32 => {
            let mut lhs = track_inner(*inst.args.first()?, program, env, depth)?;
            let mut rhs = track_inner(*inst.args.get(1)?, program, env, depth)?;
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
            let mut lhs = track_inner(*inst.args.first()?, program, env, depth)?;
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
                if let Some(value) = try_get_constant(op1, program, env) {
                    op1 = op2;
                    op2 = Value::ImmU32(value);
                } else if let Some(value) = try_get_constant(op2, program, env) {
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
            let mut lhs = track_inner(op1, program, env, depth)?;
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

fn try_get_constant(
    value: Value,
    program: &Program,
    env: &mut Option<&mut dyn Environment>,
) -> Option<u32> {
    let Value::Inst(inst_ref) = value else {
        return None;
    };
    let inst = get_inst(program, inst_ref)?;
    if inst.opcode != Opcode::GetCbufU32 {
        return None;
    }
    let index = *inst.args.first()?;
    let offset = *inst.args.get(1)?;
    if !index.is_immediate() || !offset.is_immediate() || index.imm_u32() != 1 {
        return None;
    }
    let env = env.as_deref_mut()?;
    Some(env.read_cbuf_value(index.imm_u32(), offset.imm_u32()))
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
    TextureType::from_u8(flags.texture_type)
}

fn image_format_from_flags(flags: TextureInstInfo) -> ImageFormat {
    ImageFormat::from_u8(flags.image_format)
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

fn insert_before_with_flags(
    block: &mut Block,
    block_index: u32,
    before: u32,
    opcode: Opcode,
    args: Vec<Value>,
    flags: u32,
) -> Value {
    let inst = block.insert_inst_before(before, Inst::with_flags(opcode, args, flags));
    Value::Inst(InstRef {
        block: block_index,
        inst,
    })
}

fn patch_image_sample_implicit_lod(
    block: &mut Block,
    block_index: u32,
    inst_index: u32,
    info: TextureInstInfo,
) {
    let coord = block.inst(inst_index).args[1];
    let texture_size = insert_before_with_flags(
        block,
        block_index,
        inst_index,
        Opcode::ImageQueryDimensions,
        vec![Value::ImmU32(0), Value::ImmU32(0), Value::ImmU1(true)],
        info.to_u32(),
    );
    let coord_x = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeExtractF32x2,
        vec![coord, Value::ImmU32(0)],
    );
    let size_x = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeExtractU32x4,
        vec![texture_size, Value::ImmU32(0)],
    );
    let size_x_f32 = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::ConvertF32U32,
        vec![size_x],
    );
    let recip_x = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::FPRecip32,
        vec![size_x_f32],
    );
    let scaled_x = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::FPMul32,
        vec![coord_x, recip_x],
    );
    let coord_y = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeExtractF32x2,
        vec![coord, Value::ImmU32(1)],
    );
    let size_y = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeExtractU32x4,
        vec![texture_size, Value::ImmU32(1)],
    );
    let size_y_f32 = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::ConvertF32U32,
        vec![size_y],
    );
    let recip_y = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::FPRecip32,
        vec![size_y_f32],
    );
    let scaled_y = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::FPMul32,
        vec![coord_y, recip_y],
    );
    let scaled_coord = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeConstructF32x2,
        vec![scaled_x, scaled_y],
    );
    block.inst_mut(inst_index).args[1] = scaled_coord;
}

fn snorm_max_value(format: TexturePixelFormat) -> f32 {
    match format {
        TexturePixelFormat::A8B8G8R8Snorm
        | TexturePixelFormat::R8G8Snorm
        | TexturePixelFormat::R8Snorm => 1.0 / i8::MAX as f32,
        TexturePixelFormat::R16G16B16A16Snorm
        | TexturePixelFormat::R16G16Snorm
        | TexturePixelFormat::R16Snorm => 1.0 / i16::MAX as f32,
        _ => panic!("Invalid texture pixel format for SNORM patch: {:?}", format),
    }
}

fn replace_uses_with(program: &mut Program, old: InstRef, replacement: Value) {
    let old_value = Value::Inst(old);
    for block in &mut program.blocks {
        for inst in block.iter_mut() {
            for arg in &mut inst.args {
                if *arg == old_value {
                    *arg = replacement;
                }
            }
            for (_, value) in &mut inst.phi_args {
                if *value == old_value {
                    *value = replacement;
                }
            }
        }
    }
    for node in &mut program.syntax_list {
        match node {
            SyntaxNode::If { cond, .. }
            | SyntaxNode::Repeat { cond, .. }
            | SyntaxNode::Break { cond, .. } => {
                if *cond == old_value {
                    *cond = replacement;
                }
            }
            _ => {}
        }
    }
}

fn patch_texel_fetch(
    program: &mut Program,
    block_index: u32,
    inst_index: u32,
    pixel_format: TexturePixelFormat,
) {
    let base_inst = program.block(block_index).inst(inst_index).clone();
    let block = program.block_mut(block_index);
    let fetch = block.insert_inst_before(inst_index, base_inst);
    let fetch_value = Value::Inst(InstRef {
        block: block_index,
        inst: fetch,
    });

    let x = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeExtractF32x4,
        vec![fetch_value, Value::ImmU32(0)],
    );
    let y = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeExtractF32x4,
        vec![fetch_value, Value::ImmU32(1)],
    );
    let z = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeExtractF32x4,
        vec![fetch_value, Value::ImmU32(2)],
    );
    let w = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeExtractF32x4,
        vec![fetch_value, Value::ImmU32(3)],
    );
    let max_value = Value::ImmF32(snorm_max_value(pixel_format));

    let convert = |block: &mut Block, component: Value| {
        let bits = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::BitCastU32F32,
            vec![component],
        );
        let as_float = insert_before(
            block,
            block_index,
            inst_index,
            Opcode::ConvertF32S32,
            vec![bits],
        );
        insert_before(
            block,
            block_index,
            inst_index,
            Opcode::FPMul32,
            vec![as_float, max_value],
        )
    };

    let converted_x = convert(block, x);
    let converted_y = convert(block, y);
    let converted_z = convert(block, z);
    let converted_w = convert(block, w);
    let converted = insert_before(
        block,
        block_index,
        inst_index,
        Opcode::CompositeConstructF32x4,
        vec![converted_x, converted_y, converted_z, converted_w],
    );

    replace_uses_with(
        program,
        InstRef {
            block: block_index,
            inst: inst_index,
        },
        converted,
    );
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
    use crate::environment::Environment;
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;
    use crate::ir::types::ShaderStage;
    use crate::program_header::ProgramHeader;
    use crate::shader_info::ReplaceConstant;
    use crate::stage::Stage;

    struct MockEnvironment {
        sph: ProgramHeader,
        texture_type: TextureType,
        texture_pixel_format: TexturePixelFormat,
    }

    impl Default for MockEnvironment {
        fn default() -> Self {
            Self {
                sph: ProgramHeader::default(),
                texture_type: TextureType::Color2D,
                texture_pixel_format: TexturePixelFormat::A8B8G8R8Unorm,
            }
        }
    }

    impl Environment for MockEnvironment {
        fn read_instruction(&mut self, _address: u32) -> u64 {
            0
        }

        fn read_cbuf_value(&mut self, cbuf_index: u32, cbuf_offset: u32) -> u32 {
            match (cbuf_index, cbuf_offset) {
                // Upstream TryGetConstant resolves only bank 1 constants.
                (1, 0x10) => 0x38,
                // The tracked handle source. Shifted by countr_zero(0x38) = 3.
                (4, 0x40) => 1,
                _ => 0,
            }
        }

        fn read_texture_type(&mut self, _raw_handle: u32) -> TextureType {
            self.texture_type
        }

        fn read_texture_pixel_format(&mut self, _raw_handle: u32) -> TexturePixelFormat {
            self.texture_pixel_format
        }

        fn is_texture_pixel_format_integer(&mut self, _raw_handle: u32) -> bool {
            false
        }

        fn read_viewport_transform_state(&mut self) -> u32 {
            0
        }

        fn texture_bound_buffer(&self) -> u32 {
            0
        }

        fn local_memory_size(&self) -> u32 {
            0
        }

        fn shared_memory_size(&self) -> u32 {
            0
        }

        fn workgroup_size(&self) -> [u32; 3] {
            [1, 1, 1]
        }

        fn has_hle_macro_state(&self) -> bool {
            false
        }

        fn get_replace_const_buffer(
            &mut self,
            _bank: u32,
            _offset: u32,
        ) -> Option<ReplaceConstant> {
            None
        }

        fn dump(&mut self, _pipeline_hash: u64, _shader_hash: u64) {}

        fn sph(&self) -> &ProgramHeader {
            &self.sph
        }

        fn gp_passthrough_mask(&self) -> &[u32; 8] {
            static MASK: [u32; 8] = [0; 8];
            &MASK
        }

        fn shader_stage(&self) -> Stage {
            Stage::Fragment
        }

        fn start_address(&self) -> u32 {
            0
        }

        fn is_proprietary_driver(&self) -> bool {
            false
        }
    }

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
        assert_eq!(inst.args[0], Value::Void);
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
        assert_eq!(inst.args[0], Value::Void);
        assert_eq!(TextureInstInfo::from_u32(inst.flags).descriptor_index, 0);
    }

    #[test]
    fn texture_pass_tracks_dynamic_cbuf_offset_as_descriptor_array() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = &mut program.blocks[0];
        block.append_inst(Inst::new(
            Opcode::IAdd32,
            vec![Value::ImmU32(0x80), Value::Reg(crate::ir::value::Reg(4))],
        ));
        block.append_inst(Inst::new(
            Opcode::GetCbufU32,
            vec![
                Value::ImmU32(5),
                Value::Inst(crate::ir::value::InstRef { block: 0, inst: 0 }),
            ],
        ));
        let flags = TextureInstInfo {
            texture_type: 2,
            ..Default::default()
        }
        .to_u32();
        block.append_inst(Inst::with_flags(
            Opcode::ImageSampleImplicitLod,
            vec![
                Value::Inst(crate::ir::value::InstRef { block: 0, inst: 1 }),
                Value::Void,
            ],
            flags,
        ));

        texture_pass_bound_textures(&mut program, 0);

        assert_eq!(program.info.texture_descriptors.len(), 1);
        let desc = &program.info.texture_descriptors[0];
        assert_eq!(desc.cbuf_index, 5);
        assert_eq!(desc.cbuf_offset, 0x80);
        assert_eq!(desc.count, DESCRIPTOR_SIZE);
        assert_eq!(desc.size_shift, DESCRIPTOR_SIZE_SHIFT);
        let inst = program.blocks[0].instructions[2].as_ref().unwrap();
        let Value::Inst(dynamic_index) = inst.args[0] else {
            panic!("expected dynamic descriptor index instruction");
        };
        assert_eq!(
            program.blocks[0].inst(dynamic_index.inst).opcode,
            Opcode::UMin32
        );
        let ordered_opcodes: Vec<Opcode> = program.blocks[0]
            .indexed_iter()
            .map(|(_, inst)| inst.opcode)
            .collect();
        let asr_pos = ordered_opcodes
            .iter()
            .position(|&opcode| opcode == Opcode::ShiftRightArithmetic32)
            .unwrap();
        let umin_pos = ordered_opcodes
            .iter()
            .position(|&opcode| opcode == Opcode::UMin32)
            .unwrap();
        let sample_pos = ordered_opcodes
            .iter()
            .position(|&opcode| opcode == Opcode::ImageSampleImplicitLod)
            .unwrap();
        assert!(asr_pos < umin_pos);
        assert!(umin_pos < sample_pos);
    }

    #[test]
    fn texture_pass_tracks_mask_constant_loaded_from_cbuf_bank_one() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = &mut program.blocks[0];
        block.instructions.push(Some(Inst::new(
            Opcode::GetCbufU32,
            vec![Value::ImmU32(4), Value::ImmU32(0x40)],
        )));
        block.instructions.push(Some(Inst::new(
            Opcode::GetCbufU32,
            vec![Value::ImmU32(1), Value::ImmU32(0x10)],
        )));
        block.instructions.push(Some(Inst::new(
            Opcode::BitwiseAnd32,
            vec![
                Value::Inst(crate::ir::value::InstRef { block: 0, inst: 0 }),
                Value::Inst(crate::ir::value::InstRef { block: 0, inst: 1 }),
            ],
        )));
        let flags = TextureInstInfo {
            texture_type: TextureType::Color2D as u8,
            ..Default::default()
        }
        .to_u32();
        block.instructions.push(Some(Inst::with_flags(
            Opcode::BindlessImageSampleImplicitLod,
            vec![
                Value::Inst(crate::ir::value::InstRef { block: 0, inst: 2 }),
                Value::Void,
            ],
            flags,
        )));

        let mut env = MockEnvironment::default();
        texture_pass(&mut env, &mut program, &HostTranslateInfo::default());

        let inst = program.blocks[0].instructions[3].as_ref().unwrap();
        assert_eq!(inst.opcode, Opcode::ImageSampleImplicitLod);
        assert_eq!(inst.args[0], Value::Void);
        assert_eq!(program.info.texture_descriptors.len(), 1);
        let desc = &program.info.texture_descriptors[0];
        assert_eq!(desc.cbuf_index, 4);
        assert_eq!(desc.cbuf_offset, 0x40);
        assert_eq!(desc.shift_left, 3);
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
        assert_eq!(inst.args[0], Value::Void);
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
    fn texture_pass_reads_storage_image_format_from_flags() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let flags = TextureInstInfo {
            texture_type: TextureType::Color2D as u8,
            image_format: ImageFormat::R32G32Uint as u8,
            ..Default::default()
        }
        .to_u32();
        program.blocks[0].instructions.push(Some(Inst::with_flags(
            Opcode::BoundImageRead,
            vec![Value::ImmU32(0x3c), Value::Void],
            flags,
        )));

        texture_pass_bound_textures(&mut program, 6);

        assert_eq!(program.info.image_descriptors.len(), 1);
        assert_eq!(
            program.info.image_descriptors[0].format,
            ImageFormat::R32G32Uint
        );
    }

    #[test]
    fn texture_pass_preserves_upstream_texture_type_enum_values() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let flags = TextureInstInfo {
            texture_type: TextureType::ColorArray2D as u8,
            ..Default::default()
        }
        .to_u32();
        program.blocks[0].instructions.push(Some(Inst::with_flags(
            Opcode::BoundImageSampleImplicitLod,
            vec![Value::ImmU32(0x50), Value::Void],
            flags,
        )));

        texture_pass_bound_textures(&mut program, 6);

        assert_eq!(program.info.texture_descriptors.len(), 1);
        assert_eq!(
            program.info.texture_descriptors[0].texture_type,
            TextureType::ColorArray2D
        );
    }

    #[test]
    fn texture_pass_patches_rect_implicit_lod_coords_before_sample() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let coord = program.blocks[0].append_inst(Inst::new(
            Opcode::CompositeConstructF32x2,
            vec![Value::ImmF32(64.0), Value::ImmF32(32.0)],
        ));
        let flags = TextureInstInfo {
            texture_type: TextureType::Color2D as u8,
            ..Default::default()
        }
        .to_u32();
        program.blocks[0].append_inst(Inst::with_flags(
            Opcode::BoundImageSampleImplicitLod,
            vec![
                Value::ImmU32(0x50),
                Value::Inst(InstRef {
                    block: 0,
                    inst: coord,
                }),
            ],
            flags,
        ));

        let mut env = MockEnvironment {
            texture_type: TextureType::Color2DRect,
            ..Default::default()
        };
        texture_pass(&mut env, &mut program, &HostTranslateInfo::default());

        let ordered_opcodes: Vec<Opcode> = program.blocks[0]
            .indexed_iter()
            .map(|(_, inst)| inst.opcode)
            .collect();
        let sample_pos = ordered_opcodes
            .iter()
            .position(|&opcode| opcode == Opcode::ImageSampleImplicitLod)
            .unwrap();
        let query_pos = ordered_opcodes
            .iter()
            .position(|&opcode| opcode == Opcode::ImageQueryDimensions)
            .unwrap();
        assert!(query_pos < sample_pos);

        let sample = program.blocks[0].indexed_iter().last().unwrap().1;
        assert_eq!(sample.opcode, Opcode::ImageSampleImplicitLod);
        assert!(matches!(sample.args[1], Value::Inst(_)));
        assert!(ordered_opcodes.contains(&Opcode::CompositeConstructF32x2));
        assert!(ordered_opcodes.contains(&Opcode::FPRecip32));
    }

    #[test]
    fn texture_pass_patches_snorm_texel_fetch_uses() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let flags = TextureInstInfo {
            texture_type: TextureType::Color1D as u8,
            ..Default::default()
        }
        .to_u32();
        program.blocks[0].append_inst(Inst::with_flags(
            Opcode::BoundImageFetch,
            vec![Value::ImmU32(0x28), Value::Void, Value::ImmU32(0)],
            flags,
        ));
        program.blocks[0].append_inst(Inst::new(
            Opcode::CompositeExtractF32x4,
            vec![Value::Inst(InstRef { block: 0, inst: 0 }), Value::ImmU32(0)],
        ));

        let mut env = MockEnvironment {
            texture_type: TextureType::Buffer,
            texture_pixel_format: TexturePixelFormat::R8Snorm,
            ..Default::default()
        };
        texture_pass(&mut env, &mut program, &HostTranslateInfo::default());

        let consumer = program.blocks[0].inst(1);
        let Value::Inst(converted_ref) = consumer.args[0] else {
            panic!("expected consumer to use converted SNORM value");
        };
        assert_eq!(
            program.blocks[0].inst(converted_ref.inst).opcode,
            Opcode::CompositeConstructF32x4
        );
        let ordered_opcodes: Vec<Opcode> = program.blocks[0]
            .indexed_iter()
            .map(|(_, inst)| inst.opcode)
            .collect();
        let first_fetch = ordered_opcodes
            .iter()
            .position(|&opcode| opcode == Opcode::ImageFetch)
            .unwrap();
        let construct = ordered_opcodes
            .iter()
            .position(|&opcode| opcode == Opcode::CompositeConstructF32x4)
            .unwrap();
        let original_fetch = ordered_opcodes
            .iter()
            .rposition(|&opcode| opcode == Opcode::ImageFetch)
            .unwrap();
        assert!(first_fetch < construct);
        assert!(construct < original_fetch);
        assert!(ordered_opcodes.contains(&Opcode::BitCastU32F32));
        assert!(ordered_opcodes.contains(&Opcode::ConvertF32S32));
        assert!(ordered_opcodes.contains(&Opcode::FPMul32));
    }

    #[test]
    fn replace_uses_with_rewrites_syntax_conditions() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let old = program.blocks[0].append_inst(Inst::new(
            Opcode::ImageFetch,
            vec![Value::ImmU32(0), Value::Void, Value::ImmU32(0)],
        ));
        let old_ref = InstRef {
            block: 0,
            inst: old,
        };
        let replacement = Value::ImmU1(true);
        program.syntax_list.push(SyntaxNode::If {
            cond: Value::Inst(old_ref),
            body: 0,
            merge: 0,
        });

        replace_uses_with(&mut program, old_ref, replacement);

        let SyntaxNode::If { cond, .. } = program.syntax_list[0] else {
            panic!("expected If node");
        };
        assert_eq!(cond, replacement);
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
