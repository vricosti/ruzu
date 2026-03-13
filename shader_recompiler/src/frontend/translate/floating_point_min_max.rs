// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_min_max.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;

pub fn fmnmx(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b_f32(insn, opcode);

    let abs_a = bit(insn, 46);
    let neg_a = bit(insn, 48);
    let abs_b = bit(insn, 44);
    let neg_b = bit(insn, 45);

    let a = tv.ir.fp_abs_neg_32(src_a, abs_a, neg_a);
    let b = tv.ir.fp_abs_neg_32(src_b, abs_b, neg_b);

    // Predicate bit 42 selects min vs max
    let pred = field(insn, 42, 1);
    let result = if pred == 0 {
        tv.ir.fp_min_32(a, b)
    } else {
        tv.ir.fp_max_32(a, b)
    };

    tv.set_f(dst, result);
}
