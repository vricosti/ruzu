// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V image/texture emission — maps to zuyu's
//! `backend/spirv/emit_spirv_image.cpp`.
//!
//! Handles texture sampling, image loads, and texture queries.

use super::spirv_emit_context::SpirvEmitContext;
use rspirv::spirv::Word;

/// Emit ImageSampleImplicitLod (TEX/TEXS with implicit LOD).
///
/// Matches upstream `EmitImageSampleImplicitLod`.
pub fn emit_image_sample_implicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_implicit_lod");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageSampleExplicitLod (TXL).
pub fn emit_image_sample_explicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _lod: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_explicit_lod");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageSampleDrefImplicitLod (shadow TEX).
pub fn emit_image_sample_dref_implicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dref: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_dref_implicit_lod");
    ctx.builder.undef(ctx.f32_type, None)
}

/// Emit ImageSampleDrefExplicitLod (shadow TXL).
pub fn emit_image_sample_dref_explicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dref: Word,
    _lod: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_dref_explicit_lod");
    ctx.builder.undef(ctx.f32_type, None)
}

/// Emit ImageFetch (TLD — texel fetch).
pub fn emit_image_fetch(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _lod: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_fetch");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageGather (TLD4).
pub fn emit_image_gather(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _component: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_gather");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageGatherDref (TLD4 with depth comparison).
pub fn emit_image_gather_dref(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dref: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_gather_dref");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageQueryDimensions (TXQ).
pub fn emit_image_query_dimensions(ctx: &mut SpirvEmitContext, _handle: Word, _lod: Word) -> Word {
    log::trace!("SPIR-V: emit_image_query_dimensions");
    ctx.builder.undef(ctx.u32_vec4_type, None)
}

/// Emit ImageQueryLod (TMML).
pub fn emit_image_query_lod(ctx: &mut SpirvEmitContext, _handle: Word, _coords: Word) -> Word {
    log::trace!("SPIR-V: emit_image_query_lod");
    ctx.builder.undef(ctx.f32_vec2_type, None)
}

/// Emit ImageGradient (TXD — explicit gradients).
pub fn emit_image_gradient(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dpdx: Word,
    _dpdy: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_gradient");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

// ── IR-instruction dispatching helpers (called from spirv_emit_context) ───

use crate::ir::program::Program;
use crate::ir::types::TextureInstInfo;
use crate::ir::value::Value;
use crate::ir::{self, Opcode};
use rspirv::dr::Operand;
use rspirv::spirv;

struct ImageOperands {
    mask: spirv::ImageOperands,
    operands: Vec<Operand>,
}

impl Default for ImageOperands {
    fn default() -> Self {
        Self {
            mask: spirv::ImageOperands::NONE,
            operands: Vec::new(),
        }
    }
}

impl ImageOperands {
    fn for_sample(
        ctx: &mut SpirvEmitContext,
        program: &Program,
        has_bias: bool,
        has_lod: bool,
        has_lod_clamp: bool,
        lod: Word,
        offset: Value,
    ) -> Self {
        let mut operands = Self::default();
        if has_bias {
            let bias = if has_lod_clamp {
                ctx.builder
                    .composite_extract(ctx.f32_type, None, lod, vec![0])
                    .unwrap()
            } else {
                lod
            };
            operands.add(spirv::ImageOperands::BIAS, bias);
        }
        if has_lod {
            let lod_value = if has_lod_clamp {
                ctx.builder
                    .composite_extract(ctx.f32_type, None, lod, vec![0])
                    .unwrap()
            } else {
                lod
            };
            operands.add(spirv::ImageOperands::LOD, lod_value);
        }
        operands.add_offset(ctx, program, offset, false);
        if has_lod_clamp {
            let lod_clamp = if has_bias {
                ctx.builder
                    .composite_extract(ctx.f32_type, None, lod, vec![1])
                    .unwrap()
            } else {
                lod
            };
            operands.add(spirv::ImageOperands::MIN_LOD, lod_clamp);
        }
        operands
    }

    fn add_offset(
        &mut self,
        ctx: &mut SpirvEmitContext,
        program: &Program,
        offset: Value,
        runtime_offset_allowed: bool,
    ) {
        let offset = resolve_ir_value(program, offset);
        if matches!(offset, Value::Void) {
            return;
        }
        if let Some(components) = immediate_offset_components(program, offset) {
            let offset_id = if components.len() == 1 {
                ctx.constant_i32(components[0] as i32)
            } else {
                let component_ids = components
                    .iter()
                    .map(|&value| ctx.constant_i32(value as i32))
                    .collect::<Vec<_>>();
                let offset_type = ctx
                    .builder
                    .type_vector(ctx.i32_type, components.len() as u32);
                ctx.builder.constant_composite(offset_type, component_ids)
            };
            self.add(spirv::ImageOperands::CONST_OFFSET, offset_id);
        } else if runtime_offset_allowed {
            let offset_id = ctx.resolve_value(&offset);
            self.add(spirv::ImageOperands::OFFSET, offset_id);
        }
    }

    fn add(&mut self, mask: spirv::ImageOperands, value: Word) {
        self.mask |= mask;
        self.operands.push(Operand::IdRef(value));
    }

    fn mask_optional(&self) -> Option<spirv::ImageOperands> {
        (!self.mask.is_empty()).then_some(self.mask)
    }
}

fn resolve_ir_value(program: &Program, mut value: Value) -> Value {
    while let Value::Inst(inst_ref) = value {
        let inst = program.block(inst_ref.block).inst(inst_ref.inst);
        if inst.opcode != Opcode::Identity || inst.args.is_empty() {
            break;
        }
        value = inst.args[0];
    }
    value
}

fn immediate_offset_components(program: &Program, offset: Value) -> Option<Vec<u32>> {
    match resolve_ir_value(program, offset) {
        Value::ImmU32(value) => Some(vec![value]),
        Value::Inst(inst_ref) => {
            let inst = program.block(inst_ref.block).inst(inst_ref.inst);
            let component_count = match inst.opcode {
                Opcode::CompositeConstructU32x2 => 2,
                Opcode::CompositeConstructU32x3 => 3,
                Opcode::CompositeConstructU32x4 => 4,
                _ => return None,
            };
            inst.args
                .iter()
                .take(component_count)
                .map(|&arg| match resolve_ir_value(program, arg) {
                    Value::ImmU32(value) => Some(value),
                    _ => None,
                })
                .collect()
        }
        _ => None,
    }
}

fn decorate_sample(ctx: &mut SpirvEmitContext, info: TextureInstInfo, sample: Word) -> Word {
    if info.relaxed_precision {
        ctx.builder
            .decorate(sample, spirv::Decoration::RelaxedPrecision, vec![]);
    }
    sample
}

/// Port of upstream `Texture`: load a combined image sampler, including
/// descriptor-array indexing when `count > 1`.
fn texture(ctx: &mut SpirvEmitContext, descriptor_index: u32, index: Value) -> Option<Word> {
    let def = *ctx.textures.get(descriptor_index as usize)?;
    let pointer = if def.count > 1 {
        let index = ctx.resolve_value(&index);
        ctx.builder
            .access_chain(def.pointer_type, None, def.id, vec![index])
            .unwrap()
    } else {
        def.id
    };
    Some(
        ctx.builder
            .load(def.sampled_type, None, pointer, None, vec![])
            .unwrap(),
    )
}

/// Port of upstream `TextureImage`: extract the image from a combined sampler.
fn texture_image(ctx: &mut SpirvEmitContext, descriptor_index: u32, index: Value) -> Option<Word> {
    let def = *ctx.textures.get(descriptor_index as usize)?;
    let sampled_image = if def.count > 1 {
        let index = ctx.resolve_value(&index);
        let pointer = ctx
            .builder
            .access_chain(def.pointer_type, None, def.id, vec![index])
            .unwrap();
        ctx.builder
            .load(def.sampled_type, None, pointer, None, vec![])
            .unwrap()
    // Upstream's default-constructed `IR::Value{}` is the implicit index zero
    // used for non-array descriptors; Rust represents it as `Value::Void`.
    } else if matches!(index, Value::Void) || (index.is_immediate() && index.imm_u32() == 0) {
        ctx.builder
            .load(def.sampled_type, None, def.id, None, vec![])
            .unwrap()
    } else {
        panic!("SPIR-V: indirect texture image indexing is not implemented");
    };
    Some(
        ctx.builder
            .image(def.image_type, None, sampled_image)
            .unwrap(),
    )
}

/// Dispatch ImageSampleImplicitLod / ImageSampleExplicitLod IR instructions.
pub fn emit_image_sample(
    ctx: &mut SpirvEmitContext,
    program: &Program,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let info = TextureInstInfo::from_u32(inst.flags);
    let desc_idx = info.descriptor_index as u32;
    let coord = ctx.resolve_value(inst.arg(1));

    if let Some(sampled_image) = texture(ctx, desc_idx, *inst.arg(0)) {
        let id = if inst.opcode == Opcode::ImageSampleExplicitLod {
            let lod = ctx.resolve_value(inst.arg(2));
            let operands =
                ImageOperands::for_sample(ctx, program, false, true, false, lod, *inst.arg(3));
            ctx.builder
                .image_sample_explicit_lod(
                    ctx.f32_vec4_type,
                    None,
                    sampled_image,
                    coord,
                    operands.mask,
                    operands.operands,
                )
                .unwrap()
        } else if ctx.stage == crate::stage::Stage::Fragment {
            let bias_lc = if info.has_bias || info.has_lod_clamp {
                ctx.resolve_value(inst.arg(2))
            } else {
                0
            };
            let operands = ImageOperands::for_sample(
                ctx,
                program,
                info.has_bias,
                false,
                info.has_lod_clamp,
                bias_lc,
                *inst.arg(3),
            );
            ctx.builder
                .image_sample_implicit_lod(
                    ctx.f32_vec4_type,
                    None,
                    sampled_image,
                    coord,
                    operands.mask_optional(),
                    operands.operands,
                )
                .unwrap()
        } else {
            let lod = ctx.constant_f32(0.0);
            let operands = ImageOperands::for_sample(
                ctx,
                program,
                false,
                true,
                info.has_lod_clamp,
                lod,
                *inst.arg(3),
            );
            ctx.builder
                .image_sample_explicit_lod(
                    ctx.f32_vec4_type,
                    None,
                    sampled_image,
                    coord,
                    operands.mask,
                    operands.operands,
                )
                .unwrap()
        };

        let id = decorate_sample(ctx, info, id);
        ctx.set_value(block_idx, inst_idx, id);
    } else {
        let zero = ctx.const_zero_f32;
        let id = ctx
            .builder
            .composite_construct(ctx.f32_vec4_type, None, vec![zero, zero, zero, zero])
            .unwrap();
        ctx.set_value(block_idx, inst_idx, id);
    }
}

/// Dispatch ImageSampleDrefImplicitLod / ImageSampleDrefExplicitLod IR instructions.
pub fn emit_image_sample_dref(
    ctx: &mut SpirvEmitContext,
    program: &Program,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let info = TextureInstInfo::from_u32(inst.flags);
    let desc_idx = info.descriptor_index as u32;
    let coord = ctx.resolve_value(inst.arg(1));
    let dref = ctx.resolve_value(inst.arg(2));

    if let Some(sampled_image) = texture(ctx, desc_idx, *inst.arg(0)) {
        let id = if inst.opcode == Opcode::ImageSampleDrefExplicitLod {
            let lod = ctx.resolve_value(inst.arg(3));
            let operands =
                ImageOperands::for_sample(ctx, program, false, true, false, lod, *inst.arg(4));
            ctx.builder
                .image_sample_dref_explicit_lod(
                    ctx.f32_type,
                    None,
                    sampled_image,
                    coord,
                    dref,
                    operands.mask,
                    operands.operands,
                )
                .unwrap()
        } else if ctx.stage == crate::stage::Stage::Fragment {
            let bias_lc = if info.has_bias || info.has_lod_clamp {
                ctx.resolve_value(inst.arg(3))
            } else {
                0
            };
            let operands = ImageOperands::for_sample(
                ctx,
                program,
                info.has_bias,
                false,
                info.has_lod_clamp,
                bias_lc,
                *inst.arg(4),
            );
            ctx.builder
                .image_sample_dref_implicit_lod(
                    ctx.f32_type,
                    None,
                    sampled_image,
                    coord,
                    dref,
                    operands.mask_optional(),
                    operands.operands,
                )
                .unwrap()
        } else {
            let lod = ctx.constant_f32(0.0);
            let operands =
                ImageOperands::for_sample(ctx, program, false, true, false, lod, *inst.arg(4));
            ctx.builder
                .image_sample_dref_explicit_lod(
                    ctx.f32_type,
                    None,
                    sampled_image,
                    coord,
                    dref,
                    operands.mask,
                    operands.operands,
                )
                .unwrap()
        };

        let id = decorate_sample(ctx, info, id);
        ctx.set_value(block_idx, inst_idx, id);
    } else {
        let id = ctx.builder.undef(ctx.f32_type, None);
        ctx.set_value(block_idx, inst_idx, id);
    }
}

/// Dispatch ImageFetch IR instructions.
pub fn emit_image_fetch_inst(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let info = TextureInstInfo::from_u32(inst.flags);
    let desc_idx = info.descriptor_index as u32;
    let coord = ctx.resolve_value(inst.arg(1));

    if let Some(image) = texture_image(ctx, desc_idx, *inst.arg(0)) {
        let lod = if inst.args.len() > 3 {
            ctx.resolve_value(inst.arg(3))
        } else {
            ctx.const_zero_u32
        };

        let id = ctx
            .builder
            .image_fetch(
                ctx.f32_vec4_type,
                None,
                image,
                coord,
                Some(spirv::ImageOperands::LOD),
                vec![Operand::IdRef(lod)],
            )
            .unwrap();

        ctx.set_value(block_idx, inst_idx, id);
    } else {
        let zero = ctx.const_zero_f32;
        let id = ctx
            .builder
            .composite_construct(ctx.f32_vec4_type, None, vec![zero, zero, zero, zero])
            .unwrap();
        ctx.set_value(block_idx, inst_idx, id);
    }
}

/// Dispatch ImageQueryDimensions IR instructions.
pub fn emit_image_query(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let info = TextureInstInfo::from_u32(inst.flags);
    let desc_idx = info.descriptor_index as u32;

    if let Some(image) = texture_image(ctx, desc_idx, *inst.arg(0)) {
        let lod = if inst.args.len() > 1 {
            ctx.resolve_value(inst.arg(1))
        } else {
            ctx.const_zero_u32
        };

        let id = ctx
            .builder
            .image_query_size_lod(ctx.u32_vec2_type, None, image, lod)
            .unwrap();

        ctx.set_value(block_idx, inst_idx, id);
    } else {
        let one = ctx.const_one_u32;
        let id = ctx
            .builder
            .composite_construct(ctx.u32_vec2_type, None, vec![one, one])
            .unwrap();
        ctx.set_value(block_idx, inst_idx, id);
    }
}

/// Dispatch ImageGather / ImageGatherDref IR instructions.
pub fn emit_image_gather_inst(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let info = TextureInstInfo::from_u32(inst.flags);
    let desc_idx = info.descriptor_index as u32;
    let coord = ctx.resolve_value(inst.arg(1));

    if let Some(sampled_image) = texture(ctx, desc_idx, *inst.arg(0)) {
        let id = if inst.opcode == Opcode::ImageGatherDref && inst.args.len() > 4 {
            let dref = ctx.resolve_value(inst.arg(4));
            ctx.builder
                .image_dref_gather(
                    ctx.f32_vec4_type,
                    None,
                    sampled_image,
                    coord,
                    dref,
                    None,
                    vec![],
                )
                .unwrap()
        } else {
            let component = ctx
                .builder
                .constant_bit32(ctx.u32_type, info.gather_component as u32);
            ctx.builder
                .image_gather(
                    ctx.f32_vec4_type,
                    None,
                    sampled_image,
                    coord,
                    component,
                    None,
                    vec![],
                )
                .unwrap()
        };

        ctx.set_value(block_idx, inst_idx, id);
    } else {
        let zero = ctx.const_zero_f32;
        let id = ctx
            .builder
            .composite_construct(ctx.f32_vec4_type, None, vec![zero, zero, zero, zero])
            .unwrap();
        ctx.set_value(block_idx, inst_idx, id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;
    use crate::ir::types::ShaderStage;
    use crate::ir::value::InstRef;

    #[test]
    fn constant_sample_offset_follows_identity_values() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(Opcode::Identity, vec![Value::ImmU32(1)]));
        block.append_inst(Inst::new(Opcode::Identity, vec![Value::ImmU32(u32::MAX)]));
        block.append_inst(Inst::new(
            Opcode::CompositeConstructU32x2,
            vec![
                Value::Inst(InstRef { block: 0, inst: 0 }),
                Value::Inst(InstRef { block: 0, inst: 1 }),
            ],
        ));

        assert_eq!(
            immediate_offset_components(&program, Value::Inst(InstRef { block: 0, inst: 2 })),
            Some(vec![1, u32::MAX])
        );
    }
}
