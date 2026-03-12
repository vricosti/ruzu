// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Not-implemented instruction stubs — maps to zuyu's
//! `frontend/maxwell/translate/impl/not_implemented.cpp`.
//!
//! Instructions that are recognized by the decoder but not yet implemented
//! in the translator. Each function logs a warning or panics, matching
//! upstream behavior.

use super::TranslatorVisitor;

impl<'a> TranslatorVisitor<'a> {
    /// NOP — No operation. Upstream is a no-op.
    pub fn translate_nop(&mut self, _insn: u64) {
        // NOP is No-Op.
    }

    /// CAL — Call subroutine. Upstream is a no-op.
    pub fn translate_cal(&mut self, _insn: u64) {
        // CAL is a no-op
    }

    /// KIL — Kill thread. Upstream is a no-op.
    pub fn translate_kil(&mut self, _insn: u64) {
        // KIL is a no-op
    }

    /// PBK — Pre-break. Upstream is a no-op.
    pub fn translate_pbk(&mut self, _insn: u64) {
        // PBK is a no-op
    }

    /// PCNT — Pre-continue. Upstream is a no-op.
    pub fn translate_pcnt(&mut self, _insn: u64) {
        // PCNT is a no-op
    }

    /// SSY — Set synchronization point. Upstream is a no-op.
    pub fn translate_ssy(&mut self, _insn: u64) {
        // SSY is a no-op
    }

    /// RAM — Upstream is stubbed with a warning.
    pub fn translate_ram(&mut self, _insn: u64) {
        log::warn!("(STUBBED) RAM Instruction");
    }

    /// SAM — Upstream is stubbed with a warning.
    pub fn translate_sam(&mut self, _insn: u64) {
        log::warn!("(STUBBED) SAM Instruction");
    }

    /// B2R — Not implemented in upstream.
    pub fn translate_b2r(&mut self, _insn: u64) {
        log::error!("Instruction B2R is not implemented");
    }

    /// BPT — Not implemented in upstream.
    pub fn translate_bpt(&mut self, _insn: u64) {
        log::error!("Instruction BPT is not implemented");
    }

    /// CCTL — Not implemented in upstream.
    pub fn translate_cctl(&mut self, _insn: u64) {
        log::error!("Instruction CCTL is not implemented");
    }

    /// CCTLL — Not implemented in upstream.
    pub fn translate_cctll(&mut self, _insn: u64) {
        log::error!("Instruction CCTLL is not implemented");
    }

    /// CS2R — Not implemented in upstream.
    pub fn translate_cs2r(&mut self, _insn: u64) {
        log::error!("Instruction CS2R is not implemented");
    }

    /// GETCRSPTR — Not implemented in upstream.
    pub fn translate_getcrsptr(&mut self, _insn: u64) {
        log::error!("Instruction GETCRSPTR is not implemented");
    }

    /// GETLMEMBASE — Not implemented in upstream.
    pub fn translate_getlmembase(&mut self, _insn: u64) {
        log::error!("Instruction GETLMEMBASE is not implemented");
    }

    /// IDE — Not implemented in upstream.
    pub fn translate_ide(&mut self, _insn: u64) {
        log::error!("Instruction IDE is not implemented");
    }

    /// JCAL — Not implemented in upstream.
    pub fn translate_jcal(&mut self, _insn: u64) {
        log::error!("Instruction JCAL is not implemented");
    }

    /// JMP — Not implemented in upstream.
    pub fn translate_jmp(&mut self, _insn: u64) {
        log::error!("Instruction JMP is not implemented");
    }

    /// LD — Not implemented in upstream.
    pub fn translate_ld(&mut self, _insn: u64) {
        log::error!("Instruction LD is not implemented");
    }

    /// LEPC — Not implemented in upstream.
    pub fn translate_lepc(&mut self, _insn: u64) {
        log::error!("Instruction LEPC is not implemented");
    }

    /// LONGJMP — Not implemented in upstream.
    pub fn translate_longjmp(&mut self, _insn: u64) {
        log::error!("Instruction LONGJMP is not implemented");
    }

    /// PEXIT — Not implemented in upstream.
    pub fn translate_pexit(&mut self, _insn: u64) {
        log::error!("Instruction PEXIT is not implemented");
    }

    /// PLONGJMP — Not implemented in upstream.
    pub fn translate_plongjmp(&mut self, _insn: u64) {
        log::error!("Instruction PLONGJMP is not implemented");
    }

    /// PRET — Not implemented in upstream.
    pub fn translate_pret(&mut self, _insn: u64) {
        log::error!("Instruction PRET is not implemented");
    }

    /// R2B — Not implemented in upstream.
    pub fn translate_r2b(&mut self, _insn: u64) {
        log::error!("Instruction R2B is not implemented");
    }

    /// RET — Not implemented in upstream.
    pub fn translate_ret(&mut self, _insn: u64) {
        log::error!("Instruction RET is not implemented");
    }

    /// RTT — Not implemented in upstream.
    pub fn translate_rtt(&mut self, _insn: u64) {
        log::error!("Instruction RTT is not implemented");
    }

    /// SETCRSPTR — Not implemented in upstream.
    pub fn translate_setcrsptr(&mut self, _insn: u64) {
        log::error!("Instruction SETCRSPTR is not implemented");
    }

    /// SETLMEMBASE — Not implemented in upstream.
    pub fn translate_setlmembase(&mut self, _insn: u64) {
        log::error!("Instruction SETLMEMBASE is not implemented");
    }

    /// ST — Not implemented in upstream.
    pub fn translate_st(&mut self, _insn: u64) {
        log::error!("Instruction ST is not implemented");
    }

    /// STP — Not implemented in upstream.
    pub fn translate_stp(&mut self, _insn: u64) {
        log::error!("Instruction STP is not implemented");
    }

    /// SYNC — Not implemented in upstream.
    pub fn translate_sync(&mut self, _insn: u64) {
        log::error!("Instruction SYNC is not implemented");
    }

    /// TXA — Not implemented in upstream.
    pub fn translate_txa(&mut self, _insn: u64) {
        log::error!("Instruction TXA is not implemented");
    }

    /// VABSDIFF — Not implemented in upstream.
    pub fn translate_vabsdiff(&mut self, _insn: u64) {
        log::error!("Instruction VABSDIFF is not implemented");
    }

    /// VABSDIFF4 — Not implemented in upstream.
    pub fn translate_vabsdiff4(&mut self, _insn: u64) {
        log::error!("Instruction VABSDIFF4 is not implemented");
    }
}
