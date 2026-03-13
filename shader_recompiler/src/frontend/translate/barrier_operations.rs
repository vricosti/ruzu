// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/maxwell/translate/impl/barrier_operations.cpp`

use super::{bit, field, TranslatorVisitor};

/// BAR — Barrier synchronization.
///
/// Matches upstream `TranslatorVisitor::BAR(u64 insn)`.
/// Only BAR.SYNC with imm_a=0, imm_b=0, pred=PT is supported (as in upstream).
pub fn bar(v: &mut TranslatorVisitor<'_>, insn: u64) {
    // Upstream mode detection:
    // BAR.SYNC  = bits masked with 0x0000009B00000000 == 0x0000008000000000
    // BAR.ARRIVE = 0x0000008100000000
    // etc.
    let mode_bits = insn & 0x0000_009B_0000_0000u64;
    let is_sync = mode_bits == 0x0000_0080_0000_0000u64;
    if !is_sync {
        log::warn!("BAR: unsupported mode bits 0x{:016X}, skipping", insn);
        return;
    }

    let is_a_imm = bit(insn, 43);
    let is_b_imm = bit(insn, 44);
    let imm_a = field(insn, 8, 8);
    let imm_b = field(insn, 20, 12);

    if !is_a_imm {
        log::warn!("BAR: non-immediate input A not implemented");
        return;
    }
    if imm_a != 0 {
        log::warn!("BAR: non-zero input A not implemented");
        return;
    }
    if !is_b_imm {
        log::warn!("BAR: non-immediate input B not implemented");
        return;
    }
    if imm_b != 0 {
        log::warn!("BAR: non-zero input B not implemented");
        return;
    }

    v.ir.barrier();
}
