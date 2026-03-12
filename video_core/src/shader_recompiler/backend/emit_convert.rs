// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V emission for type conversion and bitcast opcodes.

use super::spirv_context::EmitContext;
use crate::shader_recompiler::ir::{self, Opcode};

pub fn emit_convert(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));

    let id = match inst.opcode {
        Opcode::ConvertS32F32 => {
            // Float → Signed int (truncate toward zero)
            ctx.builder
                .convert_f_to_s(ctx.i32_type, None, a)
                .unwrap()
        }
        Opcode::ConvertU32F32 => {
            // Float → Unsigned int
            ctx.builder
                .convert_f_to_u(ctx.u32_type, None, a)
                .unwrap()
        }
        Opcode::ConvertF32S32 => {
            // Signed int → Float
            ctx.builder
                .convert_s_to_f(ctx.f32_type, None, a)
                .unwrap()
        }
        Opcode::ConvertF32U32 => {
            // Unsigned int → Float
            ctx.builder
                .convert_u_to_f(ctx.f32_type, None, a)
                .unwrap()
        }
        Opcode::BitCastU32F32 => {
            // Float reinterpreted as uint
            ctx.builder.bitcast(ctx.u32_type, None, a).unwrap()
        }
        Opcode::BitCastF32U32 => {
            // Uint reinterpreted as float
            ctx.builder.bitcast(ctx.f32_type, None, a).unwrap()
        }
        _ => {
            log::trace!("SPIR-V: unhandled conversion opcode {:?}", inst.opcode);
            ctx.const_zero_u32
        }
    };

    ctx.set_value(block_idx, inst_idx, id);
}
