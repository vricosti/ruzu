// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/condition_code_set.cpp
//!
//! CSET and CSETP test the Maxwell condition code flags (Z, S, C, O) via a
//! flow-test selector.  Full implementation requires the condition code flag
//! infrastructure (GetZFlag, GetSFlag, …) which is not yet ported to IR.
//! These stubs emit a no-op (zero / false) and log a warning.

use super::TranslatorVisitor;

/// CSET — Condition code set register.
///
/// Tests the condition codes using a flow-test selector, combines the result
/// with a predicate via a boolean op, and writes 0xFFFFFFFF / 0x3F800000 / 0 to the destination.
///
/// Not fully implemented: requires condition-code flag IR (GetZFlag / GetFlowTestResult).
pub fn cset(tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!(
        "CSET: condition code IR not yet ported — emitting zero result (insn={:#018x})",
        insn
    );
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}

/// CSETP — Condition code set predicate.
///
/// Same as CSET but writes results to two predicate registers instead of a register.
///
/// Not fully implemented: requires condition-code flag IR (GetFlowTestResult).
pub fn csetp(tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!(
        "CSETP: condition code IR not yet ported — emitting false predicates (insn={:#018x})",
        insn
    );
    // Set both destination predicates to false.
    let dst_pred_b = super::field(insn, 0, 3);
    let dst_pred_a = super::field(insn, 3, 3);
    let f = tv.ir.imm_u1(false);
    tv.ir
        .set_pred(crate::ir::value::Pred(dst_pred_b as u8), f.clone());
    tv.ir.set_pred(crate::ir::value::Pred(dst_pred_a as u8), f);
}
