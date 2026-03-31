// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/video_minimum_maximum.cpp

use super::video_helper::{extract_video_operand_value, VideoWidth};
use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Value;

/// VMNMX — Video minimum/maximum.
///
/// Upstream: `TranslatorVisitor::VMNMX(u64 insn)`
pub fn vmnmx(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = field(insn, 0, 8);
    let src_b_imm16 = field(insn, 20, 16) as u32;
    let src_b_sel = field(insn, 28, 2);
    let src_b_width = VideoWidth::from_u32(field(insn, 29, 2));
    let src_a_sel = field(insn, 36, 2);
    let src_a_width = VideoWidth::from_u32(field(insn, 37, 2));
    let src_a_sign = bit(insn, 48);
    let src_b_sign = bit(insn, 49);
    let is_src_b_reg = bit(insn, 50);
    let op_bits = field(insn, 51, 3);
    let mx = bit(insn, 56);

    // op_bits: MIN=5, MAX=6 from VideoMinMaxOps; anything else — warn and return zero
    if op_bits != 5 && op_bits != 6 {
        log::warn!("VMNMX: unsupported op {} (insn={:#018x})", op_bits, insn);
        tv.set_x(dst, Value::ImmU32(0));
        return;
    }

    let src_a_reg = field(insn, 8, 8);
    let src_a_raw = tv.x(src_a_reg);
    let src_b_raw = if is_src_b_reg {
        tv.get_reg20(insn)
    } else {
        Value::ImmU32(src_b_imm16)
    };

    let lhs = extract_video_operand_value(tv, src_a_raw, src_a_width, src_a_sel, src_a_sign);
    let rhs = extract_video_operand_value(tv, src_b_raw, src_b_width, src_b_sel, src_b_sign);

    // When mx=true the instruction does max, else min; op_bits provides a further selector.
    let use_max = mx || op_bits == 6;
    let result = if use_max {
        if src_a_sign {
            tv.ir.s_max_32(lhs, rhs)
        } else {
            tv.ir.u_max_32(lhs, rhs)
        }
    } else {
        if src_a_sign {
            tv.ir.s_min_32(lhs, rhs)
        } else {
            tv.ir.u_min_32(lhs, rhs)
        }
    };
    tv.set_x(dst, result);
}
