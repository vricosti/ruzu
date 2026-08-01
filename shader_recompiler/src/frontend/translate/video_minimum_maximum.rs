// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/video_minimum_maximum.cpp

use super::video_helper::{extract_video_operand_value, get_video_source_width, VideoWidth};
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
    let cc = bit(insn, 47);
    let src_a_sign = bit(insn, 48);
    let src_b_sign = bit(insn, 49);
    let is_src_b_reg = bit(insn, 50);
    let op_bits = field(insn, 51, 3);
    let dest_sign = bit(insn, 54);
    let sat = bit(insn, 55);
    let mx = bit(insn, 56);

    if cc {
        panic!("VMNMX CC");
    }
    if sat {
        panic!("VMNMX SAT");
    }
    if op_bits != 5 && op_bits != 6 {
        panic!("VMNMX: unsupported op {}", op_bits);
    }

    let is_b_imm = !is_src_b_reg;
    let src_a_reg = field(insn, 8, 8);
    let src_a_raw = tv.x(src_a_reg);
    let src_b_raw = if is_b_imm {
        Value::ImmU32(src_b_imm16)
    } else {
        tv.get_reg20(insn)
    };
    let src_c = tv.get_reg39(insn);

    let b_width = get_video_source_width(src_b_width, is_b_imm);
    let b_selector = if is_b_imm { 0 } else { src_b_sel };
    let op_a = extract_video_operand_value(tv, src_a_raw, src_a_width, src_a_sel, src_a_sign);
    let op_b = extract_video_operand_value(tv, src_b_raw, b_width, b_selector, src_b_sign);

    let lhs = if mx {
        if src_b_sign {
            tv.ir.s_max_32(op_a, op_b)
        } else {
            tv.ir.u_max_32(op_a, op_b)
        }
    } else {
        if src_b_sign {
            tv.ir.s_min_32(op_a, op_b)
        } else {
            tv.ir.u_min_32(op_a, op_b)
        }
    };

    let result = match (op_bits, dest_sign) {
        (5, true) => tv.ir.s_min_32(lhs, src_c),
        (5, false) => tv.ir.u_min_32(lhs, src_c),
        (6, true) => tv.ir.s_max_32(lhs, src_c),
        (6, false) => tv.ir.u_max_32(lhs, src_c),
        _ => unreachable!("validated VMNMX operation"),
    };
    tv.set_x(dst, result);
}
