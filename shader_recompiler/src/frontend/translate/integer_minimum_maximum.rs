// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_minimum_maximum.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Pred;

/// IMNMX / IMNMX_reg / IMNMX_cbuf / IMNMX_imm — Integer minimum/maximum.
pub fn imnmx(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);

    let is_signed = bit(insn, 48);
    let pred_idx = field(insn, 42, 3);

    // Predicate selects min vs max
    let pred = tv.ir.get_pred(Pred(pred_idx as u8), false);

    let min_val = if is_signed {
        tv.ir.s_min_32(src_a, src_b)
    } else {
        tv.ir.u_min_32(src_a, src_b)
    };

    let max_val = if is_signed {
        tv.ir.s_max_32(src_a, src_b)
    } else {
        tv.ir.u_max_32(src_a, src_b)
    };

    let result = tv.ir.select_u32(pred, min_val, max_val);

    tv.set_x(dst, result);
}
