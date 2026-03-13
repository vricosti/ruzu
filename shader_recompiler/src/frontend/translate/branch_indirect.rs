// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/branch_indirect.cpp

use super::TranslatorVisitor;

/// Check the instruction for unsupported modes (cbuf_mode and LMT).
///
/// Upstream throws `NotImplementedException` for both flags; we emit a warning instead.
fn check(insn: u64) {
    let cbuf_mode = (insn >> 5) & 1 != 0;
    let lmt       = (insn >> 6) & 1 != 0;

    if cbuf_mode {
        log::warn!("BRX/JMX: constant buffer mode not implemented (insn={:#018x})", insn);
    }
    if lmt {
        log::warn!("BRX/JMX: LMT not implemented (insn={:#018x})", insn);
    }
}

/// BRX — Branch indirect (branch to address computed from a register).
pub fn brx(_tv: &mut TranslatorVisitor, insn: u64) {
    check(insn);
}

/// JMX — Jump indirect (same encoding as BRX).
pub fn jmx(_tv: &mut TranslatorVisitor, insn: u64) {
    check(insn);
}
