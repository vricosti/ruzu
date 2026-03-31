// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell GPU instruction opcodes — all 279 opcodes from zuyu's maxwell.inc.
//!
//! Each instruction in the Maxwell ISA is encoded as a 64-bit word.
//! The opcode is determined by the upper bits (varies by instruction class).
//! This module provides the opcode enum and a decoder function.

use std::fmt;

/// All 279 Maxwell instruction opcodes.
///
/// Naming follows zuyu: `FADD_reg` = FADD with register source,
/// `FADD_cbuf` = FADD with constant buffer source, etc.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub enum MaxwellOpcode {
    // ── Attribute / Output ────────────────────────────────────────────
    AL2P,
    ALD,
    AST,
    // ── Atomic ────────────────────────────────────────────────────────
    ATOM_cas,
    ATOM,
    ATOMS_cas,
    ATOMS,
    // ── Misc ──────────────────────────────────────────────────────────
    B2R,
    BAR,
    // ── Bit Field ─────────────────────────────────────────────────────
    BFE_reg,
    BFE_cbuf,
    BFE_imm,
    BFI_reg,
    BFI_rc,
    BFI_cr,
    BFI_imm,
    // ── Debug ─────────────────────────────────────────────────────────
    BPT,
    // ── Branch / Control Flow ─────────────────────────────────────────
    BRA,
    BRK,
    BRX,
    CAL,
    // ── Cache ─────────────────────────────────────────────────────────
    CCTL,
    CCTLL,
    // ── Control Flow ──────────────────────────────────────────────────
    CONT,
    // ── System Register ───────────────────────────────────────────────
    CS2R,
    // ── Condition Code ────────────────────────────────────────────────
    CSET,
    CSETP,
    // ── FP64 Arithmetic ───────────────────────────────────────────────
    DADD_reg,
    DADD_cbuf,
    DADD_imm,
    DEPBAR,
    DFMA_reg,
    DFMA_rc,
    DFMA_cr,
    DFMA_imm,
    DMNMX_reg,
    DMNMX_cbuf,
    DMNMX_imm,
    DMUL_reg,
    DMUL_cbuf,
    DMUL_imm,
    DSET_reg,
    DSET_cbuf,
    DSET_imm,
    DSETP_reg,
    DSETP_cbuf,
    DSETP_imm,
    // ── Control Flow ──────────────────────────────────────────────────
    EXIT,
    // ── Float Conversion ──────────────────────────────────────────────
    F2F_reg,
    F2F_cbuf,
    F2F_imm,
    F2I_reg,
    F2I_cbuf,
    F2I_imm,
    // ── FP32 Arithmetic ───────────────────────────────────────────────
    FADD_reg,
    FADD_cbuf,
    FADD_imm,
    FADD32I,
    FCHK_reg,
    FCHK_cbuf,
    FCHK_imm,
    FCMP_reg,
    FCMP_rc,
    FCMP_cr,
    FCMP_imm,
    FFMA_reg,
    FFMA_rc,
    FFMA_cr,
    FFMA_imm,
    FFMA32I,
    FLO_reg,
    FLO_cbuf,
    FLO_imm,
    FMNMX_reg,
    FMNMX_cbuf,
    FMNMX_imm,
    FMUL_reg,
    FMUL_cbuf,
    FMUL_imm,
    FMUL32I,
    FSET_reg,
    FSET_cbuf,
    FSET_imm,
    FSETP_reg,
    FSETP_cbuf,
    FSETP_imm,
    FSWZADD,
    // ── System ────────────────────────────────────────────────────────
    GETCRSPTR,
    GETLMEMBASE,
    // ── FP16 Arithmetic ───────────────────────────────────────────────
    HADD2_reg,
    HADD2_cbuf,
    HADD2_imm,
    HADD2_32I,
    HFMA2_reg,
    HFMA2_rc,
    HFMA2_cr,
    HFMA2_imm,
    HFMA2_32I,
    HMUL2_reg,
    HMUL2_cbuf,
    HMUL2_imm,
    HMUL2_32I,
    HSET2_reg,
    HSET2_cbuf,
    HSET2_imm,
    HSETP2_reg,
    HSETP2_cbuf,
    HSETP2_imm,
    // ── Integer Conversion ────────────────────────────────────────────
    I2F_reg,
    I2F_cbuf,
    I2F_imm,
    I2I_reg,
    I2I_cbuf,
    I2I_imm,
    // ── Integer Arithmetic ────────────────────────────────────────────
    IADD_reg,
    IADD_cbuf,
    IADD_imm,
    IADD3_reg,
    IADD3_cbuf,
    IADD3_imm,
    IADD32I,
    ICMP_reg,
    ICMP_rc,
    ICMP_cr,
    ICMP_imm,
    IDE,
    IDP_reg,
    IDP_imm,
    IMAD_reg,
    IMAD_rc,
    IMAD_cr,
    IMAD_imm,
    IMAD32I,
    IMADSP_reg,
    IMADSP_rc,
    IMADSP_cr,
    IMADSP_imm,
    IMNMX_reg,
    IMNMX_cbuf,
    IMNMX_imm,
    IMUL_reg,
    IMUL_cbuf,
    IMUL_imm,
    IMUL32I,
    // ── Interpolation ─────────────────────────────────────────────────
    IPA,
    ISBERD,
    // ── Integer Shift-Add ─────────────────────────────────────────────
    ISCADD_reg,
    ISCADD_cbuf,
    ISCADD_imm,
    ISCADD32I,
    // ── Integer Comparison ────────────────────────────────────────────
    ISET_reg,
    ISET_cbuf,
    ISET_imm,
    ISETP_reg,
    ISETP_cbuf,
    ISETP_imm,
    // ── Control Flow ──────────────────────────────────────────────────
    JCAL,
    JMP,
    JMX,
    // ── Kill ──────────────────────────────────────────────────────────
    KIL,
    // ── Memory Load ───────────────────────────────────────────────────
    LD,
    LDC,
    LDG,
    LDL,
    LDS,
    // ── Load Effective Address ────────────────────────────────────────
    LEA_hi_reg,
    LEA_hi_cbuf,
    LEA_lo_reg,
    LEA_lo_cbuf,
    LEA_lo_imm,
    // ── System ────────────────────────────────────────────────────────
    LEPC,
    LONGJMP,
    // ── Logic ─────────────────────────────────────────────────────────
    LOP_reg,
    LOP_cbuf,
    LOP_imm,
    LOP3_reg,
    LOP3_cbuf,
    LOP3_imm,
    LOP32I,
    // ── Memory ────────────────────────────────────────────────────────
    MEMBAR,
    // ── Move ──────────────────────────────────────────────────────────
    MOV_reg,
    MOV_cbuf,
    MOV_imm,
    MOV32I,
    // ── Transcendental ────────────────────────────────────────────────
    MUFU,
    // ── NOP ───────────────────────────────────────────────────────────
    NOP,
    // ── Geometry Output ───────────────────────────────────────────────
    OUT_reg,
    OUT_cbuf,
    OUT_imm,
    // ── Predicate <-> Register ────────────────────────────────────────
    P2R_reg,
    P2R_cbuf,
    P2R_imm,
    // ── Control Flow Stack ────────────────────────────────────────────
    PBK,
    PCNT,
    PEXIT,
    // ── Pixel Load ────────────────────────────────────────────────────
    PIXLD,
    // ── System ────────────────────────────────────────────────────────
    PLONGJMP,
    // ── Bit Count ─────────────────────────────────────────────────────
    POPC_reg,
    POPC_cbuf,
    POPC_imm,
    // ── Control Flow Stack ────────────────────────────────────────────
    PRET,
    // ── Permute ───────────────────────────────────────────────────────
    PRMT_reg,
    PRMT_rc,
    PRMT_cr,
    PRMT_imm,
    // ── Predicate Set ─────────────────────────────────────────────────
    PSET,
    PSETP,
    // ── System ────────────────────────────────────────────────────────
    R2B,
    R2P_reg,
    R2P_cbuf,
    R2P_imm,
    RAM,
    // ── Atomic Reduction ──────────────────────────────────────────────
    RED,
    // ── Return ────────────────────────────────────────────────────────
    RET,
    // ── Range Reduction ───────────────────────────────────────────────
    RRO_reg,
    RRO_cbuf,
    RRO_imm,
    // ── System ────────────────────────────────────────────────────────
    RTT,
    S2R,
    SAM,
    // ── Select ────────────────────────────────────────────────────────
    SEL_reg,
    SEL_cbuf,
    SEL_imm,
    // ── System ────────────────────────────────────────────────────────
    SETCRSPTR,
    SETLMEMBASE,
    // ── Funnel Shift ──────────────────────────────────────────────────
    SHF_l_reg,
    SHF_l_imm,
    SHF_r_reg,
    SHF_r_imm,
    // ── Shuffle ───────────────────────────────────────────────────────
    SHFL,
    // ── Shift ─────────────────────────────────────────────────────────
    SHL_reg,
    SHL_cbuf,
    SHL_imm,
    SHR_reg,
    SHR_cbuf,
    SHR_imm,
    // ── Control Flow Stack ────────────────────────────────────────────
    SSY,
    // ── Memory Store ──────────────────────────────────────────────────
    ST,
    STG,
    STL,
    STP,
    STS,
    // ── Surface ───────────────────────────────────────────────────────
    SUATOM,
    SUATOM_cas,
    SULD,
    SURED,
    SUST,
    // ── Control Flow ──────────────────────────────────────────────────
    SYNC,
    // ── Texture ───────────────────────────────────────────────────────
    TEX,
    TEX_b,
    TEXS,
    TLD,
    TLD_b,
    TLD4,
    TLD4_b,
    TLD4S,
    TLDS,
    TMML,
    TMML_b,
    TXA,
    TXD,
    TXD_b,
    TXQ,
    TXQ_b,
    // ── Video ─────────────────────────────────────────────────────────
    VABSDIFF,
    VABSDIFF4,
    VADD,
    VMAD,
    VMNMX,
    // ── Vote ──────────────────────────────────────────────────────────
    VOTE,
    VOTE_vtg,
    // ── Video Compare / Shift ─────────────────────────────────────────
    VSET,
    VSETP,
    VSHL,
    VSHR,
    // ── Extended Multiply-Add ─────────────────────────────────────────
    XMAD_reg,
    XMAD_rc,
    XMAD_cr,
    XMAD_imm,
}

impl fmt::Display for MaxwellOpcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Decode the Maxwell opcode from a 64-bit instruction word.
///
/// This matches the bit patterns from zuyu's `decode.cpp`.
/// Maxwell instructions encode the opcode in the upper bits, but the exact
/// layout varies by instruction class.
pub fn decode_opcode(insn: u64) -> Option<MaxwellOpcode> {
    use MaxwellOpcode::*;

    // The primary encoding uses bits [63:48] for major opcode classification.
    // Some instructions use additional bits for sub-opcodes.
    let top16 = ((insn >> 48) & 0xFFFF) as u16;
    let top9 = ((insn >> 55) & 0x1FF) as u16;
    let top10 = ((insn >> 54) & 0x3FF) as u16;
    let top11 = ((insn >> 53) & 0x7FF) as u16;

    // ── 9-bit major opcode (bits [63:55]) ─────────────────────────────
    match top9 {
        // FP32 32-bit immediate forms
        0x00C => return Some(FFMA32I),
        0x008 => return Some(FADD32I),
        0x00A => return Some(FMUL32I),
        // Integer 32-bit immediate forms
        0x010 => return Some(IADD32I),
        0x014 => return Some(IMUL32I),
        0x01C => return Some(ISCADD32I),
        0x00E => return Some(IMAD32I),
        0x01A => return Some(LOP32I),
        0x00F => return Some(MOV32I),
        // Half-float 32-bit immediate forms
        0x011 => return Some(HADD2_32I),
        0x013 => return Some(HMUL2_32I),
        0x012 => return Some(HFMA2_32I),
        _ => {}
    }

    // ── 10-bit opcode (bits [63:54]) ──────────────────────────────────
    match top10 {
        // Control flow
        0x324 => return Some(BRA),
        0x325 => return Some(BRX),
        0x326 => return Some(JMP),
        0x327 => return Some(JMX),
        0x342 => return Some(SSY),
        0x343 => return Some(SYNC),
        0x344 => return Some(PBK),
        0x345 => return Some(BRK),
        0x346 => return Some(PCNT),
        0x347 => return Some(CONT),
        0x348 => return Some(PEXIT),
        0x34A => return Some(PRET),
        0x34B => return Some(CAL),
        0x34D => return Some(RET),
        0x34E => return Some(JCAL),
        0x34C => return Some(EXIT),
        0x34F => return Some(LONGJMP),
        0x350 => return Some(PLONGJMP),
        // Special
        0x330 => return Some(NOP),
        0x332 => return Some(KIL),
        0x335 => return Some(BAR),
        0x336 => return Some(DEPBAR),
        0x338 => return Some(MEMBAR),
        0x351 => return Some(SAM),
        0x352 => return Some(RAM),
        0x353 => return Some(RTT),
        0x354 => return Some(BPT),
        0x340 => return Some(VOTE),
        0x341 => return Some(VOTE_vtg),
        0x388 => return Some(SHFL),
        // System register read/write
        0x360 => return Some(S2R),
        0x361 => return Some(CS2R),
        0x362 => return Some(B2R),
        0x363 => return Some(R2B),
        0x364 => return Some(LEPC),
        0x365 => return Some(SETCRSPTR),
        0x366 => return Some(GETCRSPTR),
        0x367 => return Some(SETLMEMBASE),
        0x368 => return Some(GETLMEMBASE),
        // Texture
        0x360 | 0x361 if false => unreachable!(), // already matched above
        0x379 => return Some(PIXLD),
        _ => {}
    }

    // ── 11-bit opcode (bits [63:53]) ──────────────────────────────────
    match top11 {
        // FP32 reg/cbuf/imm
        0x221 => return Some(FADD_reg),
        0x621 => return Some(FADD_cbuf),
        0x421 => return Some(FADD_imm),
        0x220 => return Some(FMUL_reg),
        0x620 => return Some(FMUL_cbuf),
        0x420 => return Some(FMUL_imm),
        0x223 => return Some(FFMA_reg),
        0x623 => return Some(FFMA_cr),
        0x423 => return Some(FFMA_imm),
        0x224 => return Some(FMNMX_reg),
        0x624 => return Some(FMNMX_cbuf),
        0x424 => return Some(FMNMX_imm),
        0x230 => return Some(FSET_reg),
        0x630 => return Some(FSET_cbuf),
        0x430 => return Some(FSET_imm),
        0x23B => return Some(FSETP_reg),
        0x63B => return Some(FSETP_cbuf),
        0x43B => return Some(FSETP_imm),
        0x23A => return Some(FCMP_reg),
        0x63A => return Some(FCMP_imm), // overloaded
        0x43A => return Some(FCMP_cr),  // overloaded
        0x22C => return Some(FCHK_reg),
        0x62C => return Some(FCHK_cbuf),
        0x42C => return Some(FCHK_imm),
        0x208 => return Some(MUFU),
        0x228 => return Some(FSWZADD),

        // FFMA RC encoding
        0x323 => return Some(FFMA_rc),

        // FP64
        0x229 => return Some(DADD_reg),
        0x629 => return Some(DADD_cbuf),
        0x429 => return Some(DADD_imm),
        0x22B => return Some(DMUL_reg),
        0x62B => return Some(DMUL_cbuf),
        0x42B => return Some(DMUL_imm),
        0x22E => return Some(DFMA_reg),
        0x62E => return Some(DFMA_cr),
        0x42E => return Some(DFMA_imm),
        0x32E => return Some(DFMA_rc),
        0x22A => return Some(DMNMX_reg),
        0x62A => return Some(DMNMX_cbuf),
        0x42A => return Some(DMNMX_imm),
        0x232 => return Some(DSET_reg),
        0x632 => return Some(DSET_cbuf),
        0x432 => return Some(DSET_imm),
        0x23D => return Some(DSETP_reg),
        0x63D => return Some(DSETP_cbuf),
        0x43D => return Some(DSETP_imm),

        // Half-float
        0x230 if false => unreachable!(), // HADD2 collides, use bits differently
        0x22F => return Some(HADD2_reg),
        0x62F => return Some(HADD2_cbuf),
        0x42F => return Some(HADD2_imm),
        0x227 => return Some(HMUL2_reg),
        0x627 => return Some(HMUL2_cbuf),
        0x427 => return Some(HMUL2_imm),
        0x225 => return Some(HFMA2_reg),
        0x625 => return Some(HFMA2_cr),
        0x425 => return Some(HFMA2_imm),
        0x325 if false => unreachable!(), // BRX already matched
        0x231 => return Some(HSET2_reg),
        0x631 => return Some(HSET2_cbuf),
        0x431 => return Some(HSET2_imm),
        0x23C => return Some(HSETP2_reg),
        0x63C => return Some(HSETP2_cbuf),
        0x43C => return Some(HSETP2_imm),

        // HFMA2 RC
        0x226 => return Some(HFMA2_rc),

        // Integer arithmetic
        0x218 => return Some(IADD_reg),
        0x618 => return Some(IADD_cbuf),
        0x418 => return Some(IADD_imm),
        0x21C => return Some(IADD3_reg),
        0x61C => return Some(IADD3_cbuf),
        0x41C => return Some(IADD3_imm),
        0x21A => return Some(ISCADD_reg),
        0x61A => return Some(ISCADD_cbuf),
        0x41A => return Some(ISCADD_imm),
        0x224 if false => unreachable!(), // FMNMX
        0x21B => return Some(IMAD_reg),
        0x61B => return Some(IMAD_cr),
        0x41B => return Some(IMAD_imm),
        0x31B => return Some(IMAD_rc),
        0x21D => return Some(IMADSP_reg),
        0x61D => return Some(IMADSP_cr),
        0x41D => return Some(IMADSP_imm),
        0x31D => return Some(IMADSP_rc),
        0x219 => return Some(IMUL_reg),
        0x619 => return Some(IMUL_cbuf),
        0x419 => return Some(IMUL_imm),
        0x217 => return Some(IMNMX_reg),
        0x617 => return Some(IMNMX_cbuf),
        0x417 => return Some(IMNMX_imm),

        // Integer comparison
        0x233 => return Some(ISET_reg),
        0x633 => return Some(ISET_cbuf),
        0x433 => return Some(ISET_imm),
        0x23E => return Some(ISETP_reg),
        0x63E => return Some(ISETP_cbuf),
        0x43E => return Some(ISETP_imm),
        0x23C if false => unreachable!(), // HSETP2
        0x239 => return Some(ICMP_reg),
        0x639 => return Some(ICMP_imm),
        0x439 => return Some(ICMP_cr),
        0x339 => return Some(ICMP_rc),

        // Bit manipulation
        0x21F => return Some(BFE_reg),
        0x61F => return Some(BFE_cbuf),
        0x41F => return Some(BFE_imm),
        0x21E => return Some(BFI_reg),
        0x61E => return Some(BFI_cr),
        0x41E => return Some(BFI_imm),
        0x31E => return Some(BFI_rc),
        0x214 => return Some(POPC_reg),
        0x614 => return Some(POPC_cbuf),
        0x414 => return Some(POPC_imm),
        0x213 => return Some(FLO_reg),
        0x613 => return Some(FLO_cbuf),
        0x413 => return Some(FLO_imm),

        // Shift
        0x215 => return Some(SHL_reg),
        0x615 => return Some(SHL_cbuf),
        0x415 => return Some(SHL_imm),
        0x216 => return Some(SHR_reg),
        0x616 => return Some(SHR_cbuf),
        0x416 => return Some(SHR_imm),

        // Funnel shift
        0x238 => return Some(SHF_l_reg),
        0x438 => return Some(SHF_l_imm),
        0x237 => return Some(SHF_r_reg),
        0x437 => return Some(SHF_r_imm),

        // Logic
        0x212 => return Some(LOP_reg),
        0x612 => return Some(LOP_cbuf),
        0x412 => return Some(LOP_imm),
        0x211 => return Some(LOP3_reg),
        0x611 => return Some(LOP3_cbuf),
        0x411 => return Some(LOP3_imm),

        // XMAD
        0x236 => return Some(XMAD_reg),
        0x636 => return Some(XMAD_cr),
        0x436 => return Some(XMAD_imm),
        0x336 if false => unreachable!(), // DEPBAR already matched by top10
        // XMAD_rc
        0x306 => return Some(XMAD_rc),

        // Conversion
        0x210 => return Some(F2F_reg),
        0x610 => return Some(F2F_cbuf),
        0x410 => return Some(F2F_imm),
        0x20F => return Some(F2I_reg),
        0x60F => return Some(F2I_cbuf),
        0x40F => return Some(F2I_imm),
        0x20E => return Some(I2F_reg),
        0x60E => return Some(I2F_cbuf),
        0x40E => return Some(I2F_imm),
        0x20D => return Some(I2I_reg),
        0x60D => return Some(I2I_cbuf),
        0x40D => return Some(I2I_imm),

        // Move / Select
        0x209 => return Some(MOV_reg),
        0x609 => return Some(MOV_cbuf),
        0x409 => return Some(MOV_imm),
        0x20A => return Some(SEL_reg),
        0x60A => return Some(SEL_cbuf),
        0x40A => return Some(SEL_imm),

        // Permute
        0x20C => return Some(PRMT_reg),
        0x60C => return Some(PRMT_cr),
        0x40C => return Some(PRMT_imm),
        0x30C => return Some(PRMT_rc),

        // Predicate
        0x209 if false => unreachable!(), // MOV_reg
        0x235 => return Some(PSET),
        0x234 => return Some(PSETP),
        0x207 => return Some(CSET),
        0x206 => return Some(CSETP),
        0x238 if false => unreachable!(), // SHF_l_reg
        0x20B => return Some(P2R_reg),
        0x60B => return Some(P2R_cbuf),
        0x40B => return Some(P2R_imm),
        0x205 => return Some(R2P_reg),
        0x605 => return Some(R2P_cbuf),
        0x405 => return Some(R2P_imm),

        // Load Effective Address
        0x211 if false => unreachable!(), // LOP3_reg
        0x21B if false => unreachable!(), // IMAD_reg
        // LEA uses different bit patterns
        0x200 => return Some(LEA_lo_reg),
        0x600 => return Some(LEA_lo_cbuf),
        0x400 => return Some(LEA_lo_imm),
        0x201 => return Some(LEA_hi_reg),
        0x601 => return Some(LEA_hi_cbuf),

        // IDP
        0x23F => return Some(IDP_reg),
        0x43F => return Some(IDP_imm),

        // IDE
        0x240 => return Some(IDE),

        // Range Reduction
        0x290 => return Some(RRO_reg),
        0x690 => return Some(RRO_cbuf),
        0x490 => return Some(RRO_imm),

        _ => {}
    }

    // ── Memory instructions (use different bit layouts) ───────────────
    // Texture instructions often use top 10-12 bits
    match top16 >> 4 {
        // TEX family — bits [63:52]
        0xDEB => return Some(TEX),
        0xDE9 => return Some(TEX_b),
        0xDB8 => return Some(TEXS),
        0xDDA => return Some(TLD),
        0xDD8 => return Some(TLD_b),
        0xDEF => return Some(TLD4),
        0xDED => return Some(TLD4_b),
        0xDB9 => return Some(TLD4S),
        0xDB2 => return Some(TLDS),
        0xDF6 => return Some(TMML),
        0xDF4 => return Some(TMML_b),
        0xDDE => return Some(TXQ),
        0xDDC => return Some(TXQ_b),
        0xDE6 => return Some(TXD),
        0xDE4 => return Some(TXD_b),
        0xD8E => return Some(TXA),
        _ => {}
    }

    // Memory load/store — use bits [63:52] or [63:56]
    let top8 = ((insn >> 56) & 0xFF) as u8;
    match top8 {
        0xEE => return Some(LDG),
        0xED => return Some(STG),
        0xEB => return Some(LD),
        0xEF => return Some(ST),
        0xEF if false => unreachable!(), // disambiguated below
        0xE9 => return Some(LDL),
        0xE8 => return Some(STL),
        0xEC => return Some(LDS),
        0xEA => return Some(STS),
        0xE7 => return Some(LDC),
        0xE5 => return Some(ATOM),
        0xE4 => return Some(ATOM_cas),
        0xE3 => return Some(ATOMS),
        0xE2 => return Some(ATOMS_cas),
        0xE1 => return Some(RED),
        _ => {}
    }

    // Surface instructions
    match top16 >> 8 {
        0xEB => return Some(SULD),
        0xEF => return Some(SUST),
        0xEA => return Some(SUATOM),
        0xE9 => return Some(SUATOM_cas),
        0xE8 => return Some(SURED),
        _ => {}
    }

    // Attribute load/store
    match top11 {
        0x3A1 => return Some(ALD),
        0x3A2 => return Some(AST),
        0x3A0 => return Some(AL2P),
        0x3A3 => return Some(IPA),
        0x3A4 => return Some(ISBERD),
        0x3A6 => return Some(STP),
        0x3A5 => return Some(OUT_reg),
        0x7A5 => return Some(OUT_cbuf),
        0x5A5 => return Some(OUT_imm),
        // CCTL
        0x3A7 => return Some(CCTL),
        0x7A7 => return Some(CCTLL),
        _ => {}
    }

    // Video instructions
    match top11 {
        0x280 => return Some(VADD),
        0x281 => return Some(VMAD),
        0x282 => return Some(VMNMX),
        0x283 => return Some(VSET),
        0x284 => return Some(VSETP),
        0x285 => return Some(VSHL),
        0x286 => return Some(VSHR),
        0x287 => return Some(VABSDIFF),
        0x288 => return Some(VABSDIFF4),
        _ => {}
    }

    None
}

/// Instruction source operand type — helps the translator decode src_b.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SrcType {
    Register,
    ConstantBuffer,
    Immediate,
}

impl MaxwellOpcode {
    /// Get the source operand type for this opcode variant.
    pub fn src_type(self) -> SrcType {
        let name = format!("{:?}", self);
        if name.ends_with("_reg") || name.ends_with("_rc") {
            SrcType::Register
        } else if name.ends_with("_cbuf") || name.ends_with("_cr") {
            SrcType::ConstantBuffer
        } else if name.ends_with("_imm") || name.ends_with("32I") {
            SrcType::Immediate
        } else {
            // Default to register for non-suffixed opcodes
            SrcType::Register
        }
    }

    /// Get the base opcode name (without _reg/_cbuf/_imm suffix).
    pub fn base_name(self) -> &'static str {
        match self {
            FADD_reg | FADD_cbuf | FADD_imm => "FADD",
            FMUL_reg | FMUL_cbuf | FMUL_imm => "FMUL",
            FFMA_reg | FFMA_rc | FFMA_cr | FFMA_imm => "FFMA",
            FMNMX_reg | FMNMX_cbuf | FMNMX_imm => "FMNMX",
            FSET_reg | FSET_cbuf | FSET_imm => "FSET",
            FSETP_reg | FSETP_cbuf | FSETP_imm => "FSETP",
            FCMP_reg | FCMP_rc | FCMP_cr | FCMP_imm => "FCMP",
            IADD_reg | IADD_cbuf | IADD_imm => "IADD",
            IADD3_reg | IADD3_cbuf | IADD3_imm => "IADD3",
            IMAD_reg | IMAD_rc | IMAD_cr | IMAD_imm => "IMAD",
            IMUL_reg | IMUL_cbuf | IMUL_imm => "IMUL",
            ISETP_reg | ISETP_cbuf | ISETP_imm => "ISETP",
            ISET_reg | ISET_cbuf | ISET_imm => "ISET",
            IMNMX_reg | IMNMX_cbuf | IMNMX_imm => "IMNMX",
            MOV_reg | MOV_cbuf | MOV_imm => "MOV",
            SEL_reg | SEL_cbuf | SEL_imm => "SEL",
            SHL_reg | SHL_cbuf | SHL_imm => "SHL",
            SHR_reg | SHR_cbuf | SHR_imm => "SHR",
            BFE_reg | BFE_cbuf | BFE_imm => "BFE",
            BFI_reg | BFI_rc | BFI_cr | BFI_imm => "BFI",
            LOP_reg | LOP_cbuf | LOP_imm => "LOP",
            LOP3_reg | LOP3_cbuf | LOP3_imm => "LOP3",
            F2F_reg | F2F_cbuf | F2F_imm => "F2F",
            F2I_reg | F2I_cbuf | F2I_imm => "F2I",
            I2F_reg | I2F_cbuf | I2F_imm => "I2F",
            I2I_reg | I2I_cbuf | I2I_imm => "I2I",
            DADD_reg | DADD_cbuf | DADD_imm => "DADD",
            DMUL_reg | DMUL_cbuf | DMUL_imm => "DMUL",
            DFMA_reg | DFMA_rc | DFMA_cr | DFMA_imm => "DFMA",
            DMNMX_reg | DMNMX_cbuf | DMNMX_imm => "DMNMX",
            DSET_reg | DSET_cbuf | DSET_imm => "DSET",
            DSETP_reg | DSETP_cbuf | DSETP_imm => "DSETP",
            XMAD_reg | XMAD_rc | XMAD_cr | XMAD_imm => "XMAD",
            POPC_reg | POPC_cbuf | POPC_imm => "POPC",
            FLO_reg | FLO_cbuf | FLO_imm => "FLO",
            PRMT_reg | PRMT_rc | PRMT_cr | PRMT_imm => "PRMT",
            ICMP_reg | ICMP_rc | ICMP_cr | ICMP_imm => "ICMP",
            P2R_reg | P2R_cbuf | P2R_imm => "P2R",
            R2P_reg | R2P_cbuf | R2P_imm => "R2P",
            ISCADD_reg | ISCADD_cbuf | ISCADD_imm => "ISCADD",
            FCHK_reg | FCHK_cbuf | FCHK_imm => "FCHK",
            RRO_reg | RRO_cbuf | RRO_imm => "RRO",
            OUT_reg | OUT_cbuf | OUT_imm => "OUT",
            HADD2_reg | HADD2_cbuf | HADD2_imm => "HADD2",
            HMUL2_reg | HMUL2_cbuf | HMUL2_imm => "HMUL2",
            HFMA2_reg | HFMA2_rc | HFMA2_cr | HFMA2_imm => "HFMA2",
            HSET2_reg | HSET2_cbuf | HSET2_imm => "HSET2",
            HSETP2_reg | HSETP2_cbuf | HSETP2_imm => "HSETP2",
            SHF_l_reg | SHF_l_imm => "SHF_l",
            SHF_r_reg | SHF_r_imm => "SHF_r",
            LEA_hi_reg | LEA_hi_cbuf => "LEA_hi",
            LEA_lo_reg | LEA_lo_cbuf | LEA_lo_imm => "LEA_lo",
            IMADSP_reg | IMADSP_rc | IMADSP_cr | IMADSP_imm => "IMADSP",
            TEX_b => "TEX_b",
            TLD_b => "TLD_b",
            TLD4_b => "TLD4_b",
            TMML_b => "TMML_b",
            TXQ_b => "TXQ_b",
            TXD_b => "TXD_b",
            ATOM_cas => "ATOM_cas",
            ATOMS_cas => "ATOMS_cas",
            SUATOM_cas => "SUATOM_cas",
            VOTE_vtg => "VOTE_vtg",
            IDP_reg | IDP_imm => "IDP",
            _ => {
                // Single-form opcodes return their debug name
                // This is a fallback — production code should match explicitly
                match self {
                    MUFU => "MUFU",
                    NOP => "NOP",
                    EXIT => "EXIT",
                    BRA => "BRA",
                    BRK => "BRK",
                    BRX => "BRX",
                    SYNC => "SYNC",
                    SSY => "SSY",
                    PBK => "PBK",
                    PCNT => "PCNT",
                    CONT => "CONT",
                    CAL => "CAL",
                    RET => "RET",
                    KIL => "KIL",
                    S2R => "S2R",
                    CS2R => "CS2R",
                    LDG => "LDG",
                    STG => "STG",
                    LDC => "LDC",
                    LDL => "LDL",
                    STL => "STL",
                    LDS => "LDS",
                    STS => "STS",
                    LD => "LD",
                    ST => "ST",
                    TEX => "TEX",
                    TEXS => "TEXS",
                    TLD => "TLD",
                    TLD4 => "TLD4",
                    TLD4S => "TLD4S",
                    TLDS => "TLDS",
                    TMML => "TMML",
                    TXQ => "TXQ",
                    TXD => "TXD",
                    TXA => "TXA",
                    ALD => "ALD",
                    AST => "AST",
                    AL2P => "AL2P",
                    IPA => "IPA",
                    ISBERD => "ISBERD",
                    STP => "STP",
                    ATOM => "ATOM",
                    ATOMS => "ATOMS",
                    RED => "RED",
                    SULD => "SULD",
                    SUST => "SUST",
                    SUATOM => "SUATOM",
                    SURED => "SURED",
                    BAR => "BAR",
                    DEPBAR => "DEPBAR",
                    MEMBAR => "MEMBAR",
                    SHFL => "SHFL",
                    VOTE => "VOTE",
                    PSET => "PSET",
                    PSETP => "PSETP",
                    CSET => "CSET",
                    CSETP => "CSETP",
                    FADD32I => "FADD32I",
                    FMUL32I => "FMUL32I",
                    FFMA32I => "FFMA32I",
                    IADD32I => "IADD32I",
                    IMUL32I => "IMUL32I",
                    ISCADD32I => "ISCADD32I",
                    IMAD32I => "IMAD32I",
                    LOP32I => "LOP32I",
                    MOV32I => "MOV32I",
                    HADD2_32I => "HADD2_32I",
                    HMUL2_32I => "HMUL2_32I",
                    HFMA2_32I => "HFMA2_32I",
                    PEXIT => "PEXIT",
                    PRET => "PRET",
                    JMP => "JMP",
                    JMX => "JMX",
                    JCAL => "JCAL",
                    LONGJMP => "LONGJMP",
                    PLONGJMP => "PLONGJMP",
                    SAM => "SAM",
                    RAM => "RAM",
                    RTT => "RTT",
                    BPT => "BPT",
                    B2R => "B2R",
                    R2B => "R2B",
                    LEPC => "LEPC",
                    SETCRSPTR => "SETCRSPTR",
                    GETCRSPTR => "GETCRSPTR",
                    SETLMEMBASE => "SETLMEMBASE",
                    GETLMEMBASE => "GETLMEMBASE",
                    PIXLD => "PIXLD",
                    FSWZADD => "FSWZADD",
                    IDE => "IDE",
                    CCTL => "CCTL",
                    CCTLL => "CCTLL",
                    VABSDIFF => "VABSDIFF",
                    VABSDIFF4 => "VABSDIFF4",
                    VADD => "VADD",
                    VMAD => "VMAD",
                    VMNMX => "VMNMX",
                    VSET => "VSET",
                    VSETP => "VSETP",
                    VSHL => "VSHL",
                    VSHR => "VSHR",
                    VOTE_vtg => "VOTE_vtg",
                    _ => "UNKNOWN",
                }
            }
        }
    }
}

// Enable glob-style `use MaxwellOpcode::*`
use MaxwellOpcode::*;
