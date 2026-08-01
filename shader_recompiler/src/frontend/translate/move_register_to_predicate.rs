// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/move_register_to_predicate.cpp

use super::{field, TranslatorVisitor};
use crate::ir::value::{Pred, Value};

fn r2p_impl(tv: &mut TranslatorVisitor, insn: u64, mask: Value, src: Value) {
    let mode = field(insn, 40, 1); // 0 = PR, 1 = CC
    let byte_selector = field(insn, 41, 2);
    let pr_mode = mode == 0;
    let num_items = if pr_mode { 7 } else { 4 };
    let count = Value::ImmU32(1);
    let offset_base = byte_selector * 8;
    for index in 0..num_items {
        let offset = Value::ImmU32(offset_base + index);
        let src_bfe = tv
            .ir
            .bit_field_u_extract(src.clone(), offset, count.clone());
        let src_zero = tv.ir.get_zero_from_op(src_bfe);
        let src_bit = tv.ir.logical_not(src_zero);
        let mask_bfe = tv
            .ir
            .bit_field_u_extract(mask.clone(), Value::ImmU32(index), count.clone());
        let inv_mask_bit = tv.ir.get_zero_from_op(mask_bfe);

        if pr_mode {
            let pred = Pred(index as u8);
            let existing = tv.ir.get_pred(pred, false);
            let value = tv.ir.select_u1(inv_mask_bit, existing, src_bit);
            tv.ir.set_pred(pred, value);
        } else {
            let existing = match index {
                0 => tv.ir.get_z_flag(),
                1 => tv.ir.get_s_flag(),
                2 => tv.ir.get_c_flag(),
                3 => tv.ir.get_o_flag(),
                _ => unreachable!("R2P condition-code index"),
            };
            let value = tv.ir.select_u1(inv_mask_bit, existing, src_bit);
            match index {
                0 => tv.ir.set_z_flag(value),
                1 => tv.ir.set_s_flag(value),
                2 => tv.ir.set_c_flag(value),
                3 => tv.ir.set_o_flag(value),
                _ => unreachable!("R2P condition-code index"),
            }
        }
    }
}

/// R2P (reg) — Move bits from a general-purpose register into predicate registers.
pub fn r2p_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_reg = field(insn, 8, 8);
    let src = tv.x(src_reg);
    let mask = tv.get_reg20(insn);
    r2p_impl(tv, insn, mask, src);
}

/// R2P (cbuf) — Move bits from a constant buffer value into predicate registers.
pub fn r2p_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let src_reg = field(insn, 8, 8);
    let src = tv.x(src_reg);
    let mask = tv.get_cbuf(insn);
    r2p_impl(tv, insn, mask, src);
}

/// R2P (imm) — Move bits from an immediate value into predicate registers.
pub fn r2p_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let src_reg = field(insn, 8, 8);
    let src = tv.x(src_reg);
    let mask = tv.get_imm20(insn);
    r2p_impl(tv, insn, mask, src);
}
