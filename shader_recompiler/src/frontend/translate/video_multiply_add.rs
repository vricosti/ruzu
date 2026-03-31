// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/video_multiply_add.cpp

use super::video_helper::{extract_video_operand_value, get_video_source_width, VideoWidth};
use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Value;

/// VMAD — Video multiply-add.
///
/// Computes `dest = ExtractVideoOp(src_a) * ExtractVideoOp(src_b) + src_c`.
pub fn vmad(tv: &mut TranslatorVisitor, insn: u64) {
    let dest_reg = field(insn, 0, 8);
    let src_b_imm = field(insn, 20, 16);
    let src_b_sel = field(insn, 28, 2);
    let src_b_width = VideoWidth::from_u32(field(insn, 29, 2));
    let src_a_sel = field(insn, 36, 2);
    let src_a_width = VideoWidth::from_u32(field(insn, 37, 2));
    let cc = bit(insn, 47);
    let src_a_sign = bit(insn, 48);
    let src_b_sign = bit(insn, 49);
    let is_src_b_reg = bit(insn, 50);
    let scale = field(insn, 51, 2);
    let src_c_neg = bit(insn, 53);
    let src_a_neg = bit(insn, 54);
    let sat = bit(insn, 55);

    if cc {
        log::warn!("VMAD: CC not supported (insn={:#018x})", insn);
    }
    if sat {
        log::warn!("VMAD: SAT not supported (insn={:#018x})", insn);
    }
    if scale != 0 {
        log::warn!("VMAD: non-zero SCALE not supported (insn={:#018x})", insn);
    }
    if src_a_neg && src_c_neg {
        log::warn!("VMAD: PO (both neg) not supported (insn={:#018x})", insn);
    }
    if src_a_neg || src_c_neg {
        log::warn!("VMAD: NEG modifier not supported (insn={:#018x})", insn);
    }

    let is_b_imm = !is_src_b_reg;
    let src_a_reg = field(insn, 8, 8);
    let src_a_val = tv.x(src_a_reg);
    let src_b_val = if is_b_imm {
        Value::ImmU32(src_b_imm)
    } else {
        tv.get_reg20(insn)
    };
    let src_c_val = tv.get_reg39(insn);

    // Immediate values can't have a selector.
    let a_sel = src_a_sel;
    let b_sel = if is_b_imm { 0 } else { src_b_sel };
    let b_width = get_video_source_width(src_b_width, is_b_imm);

    let op_a = extract_video_operand_value(tv, src_a_val, src_a_width, a_sel, src_a_sign);
    let op_b = extract_video_operand_value(tv, src_b_val, b_width, b_sel, src_b_sign);

    // result = op_a * op_b + src_c
    let product = tv.ir.imul_32(op_a, op_b);
    let result = tv.ir.iadd_32(product, src_c_val);
    tv.set_x(dest_reg, result);
}
