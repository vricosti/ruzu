// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/select_source_with_predicate.cpp

use super::{field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::{Pred, Value};

/// SEL — Select source with predicate.
///
/// If pred is true, selects src_a; if false (or negated), selects src_b.
/// Note: upstream swaps op_a/op_b when neg_pred is set, so we preserve that.
pub fn sel(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let pred_idx = field(insn, 39, 3);
    let pred_neg = (insn >> 42) & 1 != 0;

    // Upstream: when neg_pred != 0, swaps op_a and op_b so that the predicate
    // selects in the negated sense.
    let (op_a, op_b): (Value, Value) = if pred_neg {
        (src_b, src_a)
    } else {
        (src_a, src_b)
    };

    let cond = tv.ir.get_pred(Pred(pred_idx as u8), false);
    let result = tv.ir.select_u32(cond, op_a, op_b);

    tv.set_x(dst, result);
}
