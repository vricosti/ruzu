// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/move_predicate_to_register.cpp
//!
use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Pred, Value};

/// P2R (reg) — Not implemented upstream.
pub fn p2r_reg(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("P2R_reg not implemented (upstream NotImplementedException)");
}

/// P2R (cbuf) — Not implemented upstream.
pub fn p2r_cbuf(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("P2R_cbuf not implemented (upstream NotImplementedException)");
}

/// P2R (imm) — Move selected predicate or condition-code bits into a register.
pub fn p2r_imm(tv: &mut TranslatorVisitor<'_>, insn: u64) {
    let dest_reg = field(insn, 0, 8);
    let src_reg = field(insn, 8, 8);
    let pr_mode = !bit(insn, 40);
    let byte_selector = field(insn, 41, 2);
    let Value::ImmU32(mask) = tv.get_imm20(insn) else {
        unreachable!("GetImm20 always returns an immediate")
    };
    let num_items = if pr_mode { 7 } else { 4 };
    let offset = byte_selector * 8;
    let mut insert = Value::ImmU32(0);

    for index in 0..num_items {
        if ((mask >> index) & 1) == 0 {
            continue;
        }
        let cond = if pr_mode {
            tv.ir.get_pred(Pred(index as u8), false)
        } else {
            match index {
                0 => tv.ir.get_z_flag(),
                1 => tv.ir.get_s_flag(),
                2 => tv.ir.get_c_flag(),
                3 => tv.ir.get_o_flag(),
                _ => unreachable!("P2R condition-code index"),
            }
        };
        let bit_value = tv.ir.select_u32(
            cond,
            Value::ImmU32(1u32 << (index + offset)),
            Value::ImmU32(0),
        );
        insert = tv.ir.bitwise_or_32(insert, bit_value);
    }

    let src = tv.x(src_reg);
    let masked_out = tv.ir.bitwise_and_32(src, Value::ImmU32(!(mask << offset)));
    let result = tv.ir.bitwise_or_32(masked_out, insert);
    tv.set_x(dest_reg, result);
}
