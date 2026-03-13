// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/move_predicate_to_register.cpp
//!
//! P2R_reg and P2R_cbuf are not implemented in the upstream yuzu shader compiler
//! (they throw `NotImplementedException`).  P2R_imm moves predicate register bits
//! into a general-purpose register.

use super::TranslatorVisitor;

/// P2R (reg) — Not implemented upstream; emits a warning and no-op.
pub fn p2r_reg(tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!("P2R_reg: not implemented in upstream (insn={:#018x})", insn);
}

/// P2R (cbuf) — Not implemented upstream; emits a warning and no-op.
pub fn p2r_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!("P2R_cbuf: not implemented in upstream (insn={:#018x})", insn);
}

/// P2R (imm) — Move predicate registers into a general-purpose register.
///
/// Upstream implementation packs up to 7 predicate bits into a register,
/// masked by the immediate.  Not yet fully implemented here (requires the
/// same mask/byte-selector logic as R2P).  Emits a zero result for now.
pub fn p2r_imm(tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!("P2R_imm: not yet implemented — emitting zero (insn={:#018x})", insn);
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}
