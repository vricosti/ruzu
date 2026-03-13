// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/move_register_to_predicate.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Pred, Value};

/// Inner R2P implementation (PR mode only — CC mode requires condition-code IR).
///
/// For each of the 7 predicate registers in the byte-selector window, we
/// extract the corresponding source bit from `src` and, if the mask bit
/// is set (i.e. inv_mask_bit is false), write it to the predicate register.
fn r2p_impl(tv: &mut TranslatorVisitor, insn: u64, mask: Value, src: Value) {
    let mode            = field(insn, 40, 1); // 0 = PR, 1 = CC
    let byte_selector   = field(insn, 41, 2);

    if mode != 0 {
        // CC mode requires condition-code flag IR — not yet ported.
        log::warn!("R2P: CC mode not yet implemented (insn={:#018x})", insn);
        return;
    }

    let count = Value::ImmU32(1);
    let offset_base = byte_selector * 8;
    for i in 0u32..7 {
        let offset = Value::ImmU32(offset_base + i);
        // Extract bit `i` from mask: if the bit is 0 the mask covers this pred.
        let mask_bfe = tv.ir.bit_field_u_extract(mask.clone(), Value::ImmU32(i), count.clone());
        let inv_mask_bit = tv.ir.get_zero_from_op(mask_bfe);
        // Extract source bit from `src` at the offset-relative position.
        let src_bfe  = tv.ir.bit_field_u_extract(src.clone(), offset, count.clone());
        let src_zero = tv.ir.get_zero_from_op(src_bfe);
        let src_bit  = tv.ir.logical_not(src_zero);
        // If inv_mask_bit is true (mask bit = 0), keep existing pred value; else use src_bit.
        let pred = Pred(i as u8);
        let existing = tv.ir.get_pred(pred, false);
        let new_val = tv.ir.select_u1(inv_mask_bit, existing, src_bit);
        tv.ir.set_pred(pred, new_val);
    }
}

/// R2P (reg) — Move bits from a general-purpose register into predicate registers.
pub fn r2p_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_reg = field(insn, 8, 8);
    let src  = tv.x(src_reg);
    let mask = tv.get_reg20(insn);
    r2p_impl(tv, insn, mask, src);
}

/// R2P (cbuf) — Move bits from a constant buffer value into predicate registers.
pub fn r2p_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let src_reg = field(insn, 8, 8);
    let src  = tv.x(src_reg);
    let mask = tv.get_cbuf(insn);
    r2p_impl(tv, insn, mask, src);
}

/// R2P (imm) — Move bits from an immediate value into predicate registers.
pub fn r2p_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let src_reg = field(insn, 8, 8);
    let src  = tv.x(src_reg);
    let mask = tv.get_imm20(insn);
    r2p_impl(tv, insn, mask, src);
}
