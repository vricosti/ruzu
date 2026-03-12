// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V emission for composite (vector) construct and extract opcodes.

use super::spirv_context::EmitContext;
use crate::ir::{self, Opcode};

pub fn emit_composite(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let id = match inst.opcode {
        Opcode::CompositeConstructF32x2 => {
            let a = ctx.resolve_value(inst.arg(0));
            let b = ctx.resolve_value(inst.arg(1));
            ctx.builder
                .composite_construct(ctx.f32_vec2_type, None, vec![a, b])
                .unwrap()
        }
        Opcode::CompositeConstructF32x4 => {
            let a = ctx.resolve_value(inst.arg(0));
            let b = ctx.resolve_value(inst.arg(1));
            let c = ctx.resolve_value(inst.arg(2));
            let d = ctx.resolve_value(inst.arg(3));
            ctx.builder
                .composite_construct(ctx.f32_vec4_type, None, vec![a, b, c, d])
                .unwrap()
        }
        Opcode::CompositeConstructU32x2 => {
            let a = ctx.resolve_value(inst.arg(0));
            let b = ctx.resolve_value(inst.arg(1));
            ctx.builder
                .composite_construct(ctx.u32_vec2_type, None, vec![a, b])
                .unwrap()
        }
        Opcode::CompositeConstructU32x4 => {
            let a = ctx.resolve_value(inst.arg(0));
            let b = ctx.resolve_value(inst.arg(1));
            let c = ctx.resolve_value(inst.arg(2));
            let d = ctx.resolve_value(inst.arg(3));
            ctx.builder
                .composite_construct(ctx.u32_vec4_type, None, vec![a, b, c, d])
                .unwrap()
        }
        Opcode::CompositeExtractF32x4 => {
            let composite = ctx.resolve_value(inst.arg(0));
            let index = inst.arg(1).imm_u32();
            ctx.builder
                .composite_extract(ctx.f32_type, None, composite, vec![index])
                .unwrap()
        }
        Opcode::CompositeExtractU32x2 => {
            let composite = ctx.resolve_value(inst.arg(0));
            let index = inst.arg(1).imm_u32();
            ctx.builder
                .composite_extract(ctx.u32_type, None, composite, vec![index])
                .unwrap()
        }
        Opcode::CompositeExtractU32x4 => {
            let composite = ctx.resolve_value(inst.arg(0));
            let index = inst.arg(1).imm_u32();
            ctx.builder
                .composite_extract(ctx.u32_type, None, composite, vec![index])
                .unwrap()
        }
        _ => {
            log::trace!("SPIR-V: unhandled composite opcode {:?}", inst.opcode);
            ctx.const_zero_u32
        }
    };

    ctx.set_value(block_idx, inst_idx, id);
}
