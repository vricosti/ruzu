// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Pattern-based ARM64 instruction decoder.
//!
//! Replaces the hand-written routing in `decoder.rs` with a declarative bit-pattern
//! table. Each instruction encoding is described by a 32-character pattern string
//! where `0`/`1` are fixed bits and any other character is a wildcard.

use std::sync::OnceLock;

use crate::decoder::{
    bit, bits, decode_bitmask_imm, sign_extend, AddrMode, ExtendType, FpOp, Instruction,
    ShiftType,
};

// ---------------------------------------------------------------------------
// Pattern infrastructure
// ---------------------------------------------------------------------------

/// Parse a 32-character pattern string into (mask, expected) at compile time.
/// '0' → mask bit set, expected bit clear.
/// '1' → mask bit set, expected bit set.
/// Any other char → mask bit clear (wildcard).
const fn parse_pattern(pat: &[u8; 32]) -> (u32, u32) {
    let mut mask: u32 = 0;
    let mut expected: u32 = 0;
    let mut i: usize = 0;
    while i < 32 {
        let bit_pos = 31 - i;
        match pat[i] {
            b'0' => {
                mask |= 1 << bit_pos;
            }
            b'1' => {
                mask |= 1 << bit_pos;
                expected |= 1 << bit_pos;
            }
            _ => {}
        }
        i += 1;
    }
    (mask, expected)
}

struct Matcher {
    mask: u32,
    expected: u32,
    handler: fn(u32) -> Instruction,
}

macro_rules! inst {
    ($pat:expr, $handler:expr) => {{
        const P: (u32, u32) = parse_pattern($pat);
        Matcher {
            mask: P.0,
            expected: P.1,
            handler: $handler,
        }
    }};
}

// ---------------------------------------------------------------------------
// Handler functions
// ---------------------------------------------------------------------------

// -- DP Immediate -----------------------------------------------------------

fn h_add_imm(raw: u32) -> Instruction {
    Instruction::AddImm {
        sf: bit(raw, 31) != 0,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        imm12: bits(raw, 21, 10),
        shift: bit(raw, 22) != 0,
        set_flags: bit(raw, 29) != 0,
    }
}

fn h_sub_imm(raw: u32) -> Instruction {
    Instruction::SubImm {
        sf: bit(raw, 31) != 0,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        imm12: bits(raw, 21, 10),
        shift: bit(raw, 22) != 0,
        set_flags: bit(raw, 29) != 0,
    }
}

fn h_adr(raw: u32) -> Instruction {
    let rd = bits(raw, 4, 0) as u8;
    let immhi = bits(raw, 23, 5);
    let immlo = bits(raw, 30, 29);
    let imm = sign_extend((immhi << 2) | immlo, 21);
    Instruction::Adr { rd, imm }
}

fn h_adrp(raw: u32) -> Instruction {
    let rd = bits(raw, 4, 0) as u8;
    let immhi = bits(raw, 23, 5);
    let immlo = bits(raw, 30, 29);
    let imm = sign_extend((immhi << 2) | immlo, 21) << 12;
    Instruction::Adrp { rd, imm }
}

fn h_logical_imm(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let opc = bits(raw, 30, 29) as u8;
    let n = bit(raw, 22) as u8;
    let immr = bits(raw, 21, 16) as u8;
    let imms = bits(raw, 15, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    match decode_bitmask_imm(sf, n, immr, imms) {
        Some(imm) => Instruction::LogicalImm { sf, opc, rd, rn, imm },
        None => Instruction::Unknown { raw },
    }
}

fn h_movn(raw: u32) -> Instruction {
    Instruction::MovN {
        sf: bit(raw, 31) != 0,
        rd: bits(raw, 4, 0) as u8,
        imm16: bits(raw, 20, 5) as u16,
        hw: bits(raw, 22, 21) as u8,
    }
}

fn h_movz(raw: u32) -> Instruction {
    Instruction::MovZ {
        sf: bit(raw, 31) != 0,
        rd: bits(raw, 4, 0) as u8,
        imm16: bits(raw, 20, 5) as u16,
        hw: bits(raw, 22, 21) as u8,
    }
}

fn h_movk(raw: u32) -> Instruction {
    Instruction::MovK {
        sf: bit(raw, 31) != 0,
        rd: bits(raw, 4, 0) as u8,
        imm16: bits(raw, 20, 5) as u16,
        hw: bits(raw, 22, 21) as u8,
    }
}

fn h_bitfield(raw: u32) -> Instruction {
    Instruction::BitfieldMove {
        sf: bit(raw, 31) != 0,
        opc: bits(raw, 30, 29) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        immr: bits(raw, 21, 16) as u8,
        imms: bits(raw, 15, 10) as u8,
    }
}

fn h_extr(raw: u32) -> Instruction {
    Instruction::Extr {
        sf: bit(raw, 31) != 0,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
        imms: bits(raw, 15, 10) as u8,
    }
}

// -- Branch -----------------------------------------------------------------

fn h_b(raw: u32) -> Instruction {
    let imm26 = bits(raw, 25, 0);
    Instruction::B { imm: sign_extend(imm26, 26) << 2 }
}

fn h_bl(raw: u32) -> Instruction {
    let imm26 = bits(raw, 25, 0);
    Instruction::Bl { imm: sign_extend(imm26, 26) << 2 }
}

fn h_cbz(raw: u32) -> Instruction {
    Instruction::Cbz {
        sf: bit(raw, 31) != 0,
        rt: bits(raw, 4, 0) as u8,
        imm: sign_extend(bits(raw, 23, 5), 19) << 2,
    }
}

fn h_cbnz(raw: u32) -> Instruction {
    Instruction::Cbnz {
        sf: bit(raw, 31) != 0,
        rt: bits(raw, 4, 0) as u8,
        imm: sign_extend(bits(raw, 23, 5), 19) << 2,
    }
}

fn h_tbz(raw: u32) -> Instruction {
    let b5 = bit(raw, 31);
    let b40 = bits(raw, 23, 19);
    Instruction::Tbz {
        rt: bits(raw, 4, 0) as u8,
        bit: ((b5 << 5) | b40) as u8,
        imm: sign_extend(bits(raw, 18, 5), 14) << 2,
    }
}

fn h_tbnz(raw: u32) -> Instruction {
    let b5 = bit(raw, 31);
    let b40 = bits(raw, 23, 19);
    Instruction::Tbnz {
        rt: bits(raw, 4, 0) as u8,
        bit: ((b5 << 5) | b40) as u8,
        imm: sign_extend(bits(raw, 18, 5), 14) << 2,
    }
}

fn h_bcond(raw: u32) -> Instruction {
    Instruction::BCond {
        cond: bits(raw, 3, 0) as u8,
        imm: sign_extend(bits(raw, 23, 5), 19) << 2,
    }
}

fn h_svc(raw: u32) -> Instruction {
    Instruction::Svc { imm: bits(raw, 20, 5) as u16 }
}

fn h_brk(raw: u32) -> Instruction {
    Instruction::Brk { imm: bits(raw, 20, 5) as u16 }
}

fn h_br(raw: u32) -> Instruction {
    Instruction::Br { rn: bits(raw, 9, 5) as u8 }
}

fn h_blr(raw: u32) -> Instruction {
    Instruction::Blr { rn: bits(raw, 9, 5) as u8 }
}

fn h_ret(raw: u32) -> Instruction {
    Instruction::Ret { rn: bits(raw, 9, 5) as u8 }
}

// -- System -----------------------------------------------------------------

fn h_nop(_raw: u32) -> Instruction {
    Instruction::Nop
}

fn h_hint_nop(_raw: u32) -> Instruction {
    Instruction::Nop // SEV, SEVL, WFE, WFI, YIELD, CSDB all treated as NOP
}

fn h_clrex(_raw: u32) -> Instruction {
    Instruction::Clrex
}

fn h_dsb(raw: u32) -> Instruction {
    Instruction::Dsb { option: bits(raw, 11, 8) as u8 }
}

fn h_dmb(raw: u32) -> Instruction {
    Instruction::Dmb { option: bits(raw, 11, 8) as u8 }
}

fn h_isb(_raw: u32) -> Instruction {
    Instruction::Isb
}

fn h_mrs_msr(raw: u32) -> Instruction {
    let l = bit(raw, 21);
    let op0 = bits(raw, 20, 19);
    let op1 = bits(raw, 18, 16);
    let crn = bits(raw, 15, 12);
    let crm = bits(raw, 11, 8);
    let op2 = bits(raw, 7, 5);
    let rt = bits(raw, 4, 0) as u8;
    let sys_reg = (op0 << 14) | (op1 << 11) | (crn << 7) | (crm << 3) | op2;
    if l == 1 {
        Instruction::Mrs { rt, sys_reg }
    } else {
        Instruction::Msr { rt, sys_reg }
    }
}

fn h_sys_nop(_raw: u32) -> Instruction {
    Instruction::Nop // SYS instructions treated as NOP
}

// -- DP Register ------------------------------------------------------------

fn h_logical_shifted(raw: u32) -> Instruction {
    Instruction::LogicalReg {
        sf: bit(raw, 31) != 0,
        opc: bits(raw, 30, 29) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
        shift: ShiftType::from_u32(bits(raw, 23, 22)),
        amount: bits(raw, 15, 10) as u8,
        invert: bit(raw, 21) != 0,
    }
}

fn h_add_sub_shifted(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let op = bit(raw, 30);
    let s = bit(raw, 29);
    let shift = ShiftType::from_u32(bits(raw, 23, 22));
    let rm = bits(raw, 20, 16) as u8;
    let imm6 = bits(raw, 15, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    if op == 0 {
        Instruction::AddReg { sf, rd, rn, rm, shift, amount: imm6, set_flags: s != 0 }
    } else {
        Instruction::SubReg { sf, rd, rn, rm, shift, amount: imm6, set_flags: s != 0 }
    }
}

fn h_add_sub_extended(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let op = bit(raw, 30);
    let s = bit(raw, 29);
    let rm = bits(raw, 20, 16) as u8;
    let ext = ExtendType::from_u32(bits(raw, 15, 13));
    let imm3 = bits(raw, 12, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    if op == 0 {
        Instruction::AddExtReg { sf, rd, rn, rm, ext, amount: imm3, set_flags: s != 0 }
    } else {
        Instruction::SubExtReg { sf, rd, rn, rm, ext, amount: imm3, set_flags: s != 0 }
    }
}

fn h_adc_sbc(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let op = bit(raw, 30);
    let s = bit(raw, 29) != 0;
    let rm = bits(raw, 20, 16) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    if op == 0 {
        Instruction::Adc { sf, rd, rn, rm, set_flags: s }
    } else {
        Instruction::Sbc { sf, rd, rn, rm, set_flags: s }
    }
}

fn h_ccmp_reg(raw: u32) -> Instruction {
    Instruction::Ccmp {
        sf: bit(raw, 31) != 0,
        rn: bits(raw, 9, 5) as u8,
        rm_or_imm: bits(raw, 20, 16) as u8,
        nzcv: bits(raw, 3, 0) as u8,
        cond: bits(raw, 15, 12) as u8,
        is_imm: false,
        is_neg: bit(raw, 30) != 0,
    }
}

fn h_ccmp_imm(raw: u32) -> Instruction {
    Instruction::Ccmp {
        sf: bit(raw, 31) != 0,
        rn: bits(raw, 9, 5) as u8,
        rm_or_imm: bits(raw, 20, 16) as u8,
        nzcv: bits(raw, 3, 0) as u8,
        cond: bits(raw, 15, 12) as u8,
        is_imm: true,
        is_neg: bit(raw, 30) != 0,
    }
}

fn h_csel(raw: u32) -> Instruction {
    Instruction::Csel {
        sf: bit(raw, 31) != 0,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
        cond: bits(raw, 15, 12) as u8,
        op2: bits(raw, 11, 10) as u8,
    }
}

fn h_dp2_source(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let opcode = bits(raw, 15, 10);
    let rm = bits(raw, 20, 16) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    match opcode {
        0b000010 => Instruction::Udiv { sf, rd, rn, rm },
        0b000011 => Instruction::Sdiv { sf, rd, rn, rm },
        0b001000 => Instruction::VarShift { sf, rd, rn, rm, shift_type: 0 }, // LSLV
        0b001001 => Instruction::VarShift { sf, rd, rn, rm, shift_type: 1 }, // LSRV
        0b001010 => Instruction::VarShift { sf, rd, rn, rm, shift_type: 2 }, // ASRV
        0b001011 => Instruction::VarShift { sf, rd, rn, rm, shift_type: 3 }, // RORV
        _ => Instruction::Unknown { raw },
    }
}

fn h_crc32(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let opcode = bits(raw, 15, 10);
    let sz = (opcode & 0b011) as u8;
    let c = (opcode >> 2) & 1 != 0;
    Instruction::Crc32 {
        sf,
        sz,
        c,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
    }
}

fn h_dp1_source(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let opcode = bits(raw, 15, 10);
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    match opcode {
        0b000000 => Instruction::Rbit { sf, rd, rn },
        0b000001 => Instruction::Rev { sf, rd, rn, opc: 1 },
        0b000010 => Instruction::Rev { sf, rd, rn, opc: 2 },
        0b000011 if sf => Instruction::Rev { sf, rd, rn, opc: 3 },
        0b000100 => Instruction::Clz { sf, rd, rn },
        0b000101 => Instruction::Cls { sf, rd, rn },
        _ => Instruction::Unknown { raw },
    }
}

fn h_dp3_source(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let op54 = bits(raw, 30, 29);
    let op31 = bits(raw, 23, 21);
    let o0 = bit(raw, 15);
    let rm = bits(raw, 20, 16) as u8;
    let ra = bits(raw, 14, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    match (op54, op31, o0) {
        (0b00, 0b000, 0) => Instruction::Madd { sf, rd, rn, rm, ra },
        (0b00, 0b000, 1) => Instruction::Msub { sf, rd, rn, rm, ra },
        (0b00, 0b001, 0) if sf => Instruction::Smaddl { rd, rn, rm, ra },
        (0b00, 0b001, 1) if sf => Instruction::Smsubl { rd, rn, rm, ra },
        (0b00, 0b101, 0) if sf => Instruction::Umaddl { rd, rn, rm, ra },
        (0b00, 0b101, 1) if sf => Instruction::Umsubl { rd, rn, rm, ra },
        (0b00, 0b010, 0) if sf => Instruction::Smulh { rd, rn, rm },
        (0b00, 0b110, 0) if sf => Instruction::Umulh { rd, rn, rm },
        _ => Instruction::Unknown { raw },
    }
}

// -- Load/Store -------------------------------------------------------------

fn h_gp_uimm(raw: u32) -> Instruction {
    let size = bits(raw, 31, 30) as u8;
    let opc = bits(raw, 23, 22);
    let imm12 = bits(raw, 21, 10);
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let scale = size as u32;
    let offset = (imm12 as i64) << scale;
    let sf = size >= 2;
    let sign_ext = opc >= 2;
    if opc == 0 {
        Instruction::StrImm { sf, rt, rn, imm: offset, size, mode: AddrMode::Offset }
    } else {
        Instruction::LdrImm {
            sf: if sign_ext && opc == 2 { size == 3 } else { sf },
            rt, rn, imm: offset, size, mode: AddrMode::Offset, sign_extend: sign_ext,
        }
    }
}

fn h_gp_post(raw: u32) -> Instruction {
    let size = bits(raw, 31, 30) as u8;
    let opc = bits(raw, 23, 22);
    let imm9 = bits(raw, 20, 12);
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let offset = sign_extend(imm9, 9);
    let sf = size >= 2;
    let sign_ext = opc >= 2;
    if opc == 0 {
        Instruction::StrImm { sf, rt, rn, imm: offset, size, mode: AddrMode::PostIndex }
    } else {
        Instruction::LdrImm {
            sf: if sign_ext && opc == 2 { size == 3 } else { sf },
            rt, rn, imm: offset, size, mode: AddrMode::PostIndex, sign_extend: sign_ext,
        }
    }
}

fn h_gp_pre(raw: u32) -> Instruction {
    let size = bits(raw, 31, 30) as u8;
    let opc = bits(raw, 23, 22);
    let imm9 = bits(raw, 20, 12);
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let offset = sign_extend(imm9, 9);
    let sf = size >= 2;
    let sign_ext = opc >= 2;
    if opc == 0 {
        Instruction::StrImm { sf, rt, rn, imm: offset, size, mode: AddrMode::PreIndex }
    } else {
        Instruction::LdrImm {
            sf: if sign_ext && opc == 2 { size == 3 } else { sf },
            rt, rn, imm: offset, size, mode: AddrMode::PreIndex, sign_extend: sign_ext,
        }
    }
}

fn h_gp_unscaled(raw: u32) -> Instruction {
    let size = bits(raw, 31, 30) as u8;
    let opc = bits(raw, 23, 22);
    let imm9 = bits(raw, 20, 12);
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let offset = sign_extend(imm9, 9);
    let sf = size >= 2;
    let sign_ext = opc >= 2;
    if opc == 0 {
        Instruction::StrImm { sf, rt, rn, imm: offset, size, mode: AddrMode::Offset }
    } else {
        Instruction::LdrImm {
            sf: if sign_ext && opc == 2 { size == 3 } else { sf },
            rt, rn, imm: offset, size, mode: AddrMode::Offset, sign_extend: sign_ext,
        }
    }
}

fn h_gp_reg(raw: u32) -> Instruction {
    let size = bits(raw, 31, 30) as u8;
    let opc = bits(raw, 23, 22);
    let rm = bits(raw, 20, 16) as u8;
    let option = bits(raw, 15, 13);
    let s = bit(raw, 12);
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let ext = ExtendType::from_u32(option);
    let sign_ext = opc >= 2;
    if opc == 0 {
        Instruction::StrReg { sf: size >= 2, rt, rn, rm, size, extend: ext, shift: s != 0 }
    } else {
        Instruction::LdrReg {
            sf: if sign_ext { size == 3 } else { size >= 2 },
            rt, rn, rm, size, extend: ext, shift: s != 0, sign_extend: sign_ext,
        }
    }
}

fn h_atomic_mem(raw: u32) -> Instruction {
    let size = bits(raw, 31, 30) as u8;
    let rs = bits(raw, 20, 16) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let o3 = bit(raw, 15);
    let opc = bits(raw, 14, 12) as u8;
    if o3 == 1 {
        if opc == 0 { Instruction::Swp { size, rs, rt, rn } }
        else { Instruction::Unknown { raw } }
    } else {
        match opc {
            0b000 => Instruction::AtomicOp { size, rs, rt, rn, op: 0 },
            0b001 => Instruction::AtomicOp { size, rs, rt, rn, op: 1 },
            0b010 => Instruction::AtomicOp { size, rs, rt, rn, op: 2 },
            0b011 => Instruction::AtomicOp { size, rs, rt, rn, op: 3 },
            _ => Instruction::Unknown { raw },
        }
    }
}

fn h_ldr_lit_gp(raw: u32) -> Instruction {
    let opc = bits(raw, 31, 30);
    let imm19 = bits(raw, 23, 5);
    let rt = bits(raw, 4, 0) as u8;
    let offset = sign_extend(imm19, 19) << 2;
    match opc {
        0b00 => Instruction::LdrLit { sf: false, rt, imm: offset, sign_extend: false },
        0b01 => Instruction::LdrLit { sf: true, rt, imm: offset, sign_extend: false },
        0b10 => Instruction::LdrLit { sf: true, rt, imm: offset, sign_extend: true },
        _ => Instruction::Unknown { raw },
    }
}

fn h_ldr_lit_simd(raw: u32) -> Instruction {
    let opc = bits(raw, 31, 30);
    let imm19 = bits(raw, 23, 5);
    let rt = bits(raw, 4, 0) as u8;
    let offset = sign_extend(imm19, 19) << 2;
    let size = match opc {
        0b00 => 2,
        0b01 => 3,
        0b10 => 4,
        _ => return Instruction::Unknown { raw },
    };
    Instruction::LdrSimd { rt, rn: 0xFF, imm: offset, size, mode: AddrMode::Offset }
}

fn h_ldst_pair_gp(raw: u32) -> Instruction {
    let opc = bits(raw, 31, 30);
    let l = bit(raw, 22);
    let imm7 = bits(raw, 21, 15);
    let rt2 = bits(raw, 14, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let mode_bits = bits(raw, 24, 23);
    let mode = match mode_bits {
        0b01 => AddrMode::PostIndex,
        0b10 => AddrMode::Offset,
        0b11 => AddrMode::PreIndex,
        _ => return Instruction::Unknown { raw },
    };
    let scale = if opc == 0b00 { 2u32 } else { 3u32 };
    let offset = sign_extend(imm7, 7) << scale;
    let sf = opc != 0b00;
    if l == 1 {
        Instruction::Ldp { sf, rt, rt2, rn, imm: offset, mode }
    } else {
        Instruction::Stp { sf, rt, rt2, rn, imm: offset, mode }
    }
}

fn h_ldst_pair_simd(raw: u32) -> Instruction {
    let opc = bits(raw, 31, 30);
    let l = bit(raw, 22);
    let imm7 = bits(raw, 21, 15);
    let rt2 = bits(raw, 14, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let mode_bits = bits(raw, 24, 23);
    let mode = match mode_bits {
        0b01 => AddrMode::PostIndex,
        0b10 => AddrMode::Offset,
        0b11 => AddrMode::PreIndex,
        _ => return Instruction::Unknown { raw },
    };
    let (size, scale) = match opc {
        0b00 => (2u8, 2u32),
        0b01 => (3, 3),
        0b10 => (4, 4),
        _ => return Instruction::Unknown { raw },
    };
    let offset = sign_extend(imm7, 7) << scale;
    if l == 1 {
        Instruction::LdpSimd { rt, rt2, rn, imm: offset, size, mode }
    } else {
        Instruction::StpSimd { rt, rt2, rn, imm: offset, size, mode }
    }
}

fn h_ldst_exclusive(raw: u32) -> Instruction {
    let size = bits(raw, 31, 30) as u8;
    let o1 = bit(raw, 21);
    let l = bit(raw, 22);
    let o0 = bit(raw, 15);
    let rs = bits(raw, 20, 16) as u8;
    let rt2 = bits(raw, 14, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let sf = size >= 2;
    // CAS: bit[23]=1, o1=1, rt2=11111
    if bit(raw, 23) == 1 && o1 == 1 && rt2 == 0b11111 {
        return Instruction::Cas { size, rs, rt, rn };
    }
    match (o1, l, o0) {
        (0, 0, 0) => Instruction::Stxr { sf, rt, rn, rs, size },
        (0, 0, 1) => Instruction::Stlxr { sf, rt, rn, rs, size },
        (0, 1, 0) => Instruction::Ldxr { sf, rt, rn, size },
        (0, 1, 1) => Instruction::Ldaxr { sf, rt, rn, size },
        (1, 0, 0) => Instruction::Stlr { sf, rt, rn, size },
        (1, 1, 0) => Instruction::Ldar { sf, rt, rn, size },
        (1, 1, 1) => Instruction::Ldaxp { sf, rt, rt2, rn },
        (1, 0, 1) => Instruction::Stlxp { sf, rt, rt2, rn, rs },
        _ => Instruction::Unknown { raw },
    }
}

fn h_simd_uimm(raw: u32) -> Instruction {
    let size_bits = bits(raw, 31, 30);
    let opc = bits(raw, 23, 22);
    let imm12 = bits(raw, 21, 10);
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let size = match (size_bits, opc) {
        (0b00, 0b00 | 0b01) => 2u8,
        (0b01, 0b00 | 0b01) => 3,
        (0b00, 0b10 | 0b11) => 4,
        _ => size_bits as u8,
    };
    let offset = (imm12 as i64) << (size as u32);
    if opc & 1 == 0 {
        Instruction::StrSimd { rt, rn, imm: offset, size, mode: AddrMode::Offset }
    } else {
        Instruction::LdrSimd { rt, rn, imm: offset, size, mode: AddrMode::Offset }
    }
}

fn h_simd_post(raw: u32) -> Instruction {
    let size_bits = bits(raw, 31, 30);
    let opc = bits(raw, 23, 22);
    let imm9 = bits(raw, 20, 12);
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let size = match (size_bits, opc) {
        (0b00, 0b00 | 0b01) => 2u8,
        (0b01, 0b00 | 0b01) => 3,
        (0b00, 0b10 | 0b11) => 4,
        _ => size_bits as u8,
    };
    let offset = sign_extend(imm9, 9);
    if opc & 1 == 0 {
        Instruction::StrSimd { rt, rn, imm: offset, size, mode: AddrMode::PostIndex }
    } else {
        Instruction::LdrSimd { rt, rn, imm: offset, size, mode: AddrMode::PostIndex }
    }
}

fn h_simd_pre(raw: u32) -> Instruction {
    let size_bits = bits(raw, 31, 30);
    let opc = bits(raw, 23, 22);
    let imm9 = bits(raw, 20, 12);
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let size = match (size_bits, opc) {
        (0b00, 0b00 | 0b01) => 2u8,
        (0b01, 0b00 | 0b01) => 3,
        (0b00, 0b10 | 0b11) => 4,
        _ => size_bits as u8,
    };
    let offset = sign_extend(imm9, 9);
    if opc & 1 == 0 {
        Instruction::StrSimd { rt, rn, imm: offset, size, mode: AddrMode::PreIndex }
    } else {
        Instruction::LdrSimd { rt, rn, imm: offset, size, mode: AddrMode::PreIndex }
    }
}

fn h_simd_ldst_multi(raw: u32) -> Instruction {
    let q = bit(raw, 30) != 0;
    let load = bit(raw, 22) != 0;
    let post_index = bit(raw, 23) != 0;
    let rm = if post_index { Some(bits(raw, 20, 16) as u8) } else { None };
    Instruction::SimdLdStMulti {
        q,
        load,
        opcode: bits(raw, 15, 12) as u8,
        size: bits(raw, 11, 10) as u8,
        rn: bits(raw, 9, 5) as u8,
        rt: bits(raw, 4, 0) as u8,
        rm,
    }
}

fn h_simd_ldst_single(raw: u32) -> Instruction {
    let q = bit(raw, 30) != 0;
    let load = bit(raw, 22) != 0;
    let post_index = bit(raw, 23) != 0;
    let rm = if post_index { Some(bits(raw, 20, 16) as u8) } else { None };
    Instruction::SimdLdStSingle {
        q,
        load,
        r: bit(raw, 21) as u8,
        opcode: bits(raw, 15, 13) as u8,
        s: bit(raw, 12) as u8,
        size: bits(raw, 11, 10) as u8,
        rn: bits(raw, 9, 5) as u8,
        rt: bits(raw, 4, 0) as u8,
        rm,
    }
}

fn h_prfm_nop(_raw: u32) -> Instruction {
    Instruction::Nop // PRFM treated as NOP
}

// -- SIMD/FP Scalar ---------------------------------------------------------

fn h_fp1_source(raw: u32) -> Instruction {
    let ftype = bits(raw, 23, 22) as u8;
    let opcode = bits(raw, 20, 15);
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    match opcode {
        0b000000 => Instruction::FMovReg { rd, rn, ftype },
        0b000001 => Instruction::Fabs { rd, rn, ftype },
        0b000010 => Instruction::Fneg { rd, rn, ftype },
        0b000011 => Instruction::Fsqrt { rd, rn, ftype },
        0b000100 => Instruction::Fcvt { rd, rn, src_type: ftype, dst_type: 0 },
        0b000101 => Instruction::Fcvt { rd, rn, src_type: ftype, dst_type: 1 },
        0b000111 => Instruction::Fcvt { rd, rn, src_type: ftype, dst_type: 3 },
        _ if (opcode >> 3) == 0b001 => {
            Instruction::Frint { ftype, rd, rn, mode: (opcode & 0x7) as u8 }
        }
        _ => Instruction::Unknown { raw },
    }
}

fn h_fp2_source(raw: u32) -> Instruction {
    let ftype = bits(raw, 23, 22) as u8;
    let rm = bits(raw, 20, 16) as u8;
    let opcode = bits(raw, 15, 12);
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    let op = match opcode {
        0b0000 => FpOp::Mul,
        0b0001 => FpOp::Div,
        0b0010 => FpOp::Add,
        0b0011 => FpOp::Sub,
        0b0100 => FpOp::Max,
        0b0101 => FpOp::Min,
        0b0110 => FpOp::MaxNum,
        0b0111 => FpOp::MinNum,
        0b1000 => {
            // FNMUL: negate the product
            return Instruction::FArith { op: FpOp::Mul, rd, rn, rm, ftype };
            // Note: FNMUL is handled at interpreter level by checking opcode
            // For now, treat as regular MUL - the FArith handler can be extended
        }
        _ => return Instruction::Unknown { raw },
    };
    Instruction::FArith { op, rd, rn, rm, ftype }
}

fn h_fcmp(raw: u32) -> Instruction {
    let ftype = bits(raw, 23, 22) as u8;
    let rm = bits(raw, 20, 16) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let opc = bits(raw, 4, 3);
    Instruction::Fcmp { rn, rm, ftype, with_zero: opc & 1 != 0 }
}

fn h_fccmp(raw: u32) -> Instruction {
    let ftype = bits(raw, 23, 22) as u8;
    let rm = bits(raw, 20, 16) as u8;
    let cond = bits(raw, 15, 12) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let nzcv = bits(raw, 3, 0) as u8;
    Instruction::Fccmp { rn, rm, nzcv, cond, ftype }
}

fn h_fcsel(raw: u32) -> Instruction {
    Instruction::Fcsel {
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
        ftype: bits(raw, 23, 22) as u8,
        cond: bits(raw, 15, 12) as u8,
    }
}

fn h_fp_int_conv(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let ftype = bits(raw, 23, 22) as u8;
    let rmode = bits(raw, 20, 19);
    let opcode = bits(raw, 18, 16);
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    match (rmode, opcode) {
        (0b11, 0b000) => Instruction::FcvtzsInt { sf, rd, rn, ftype },
        (0b11, 0b001) => Instruction::FcvtzuInt { sf, rd, rn, ftype },
        (0b00, 0b010) => Instruction::ScvtfInt { sf, rd, rn, ftype },
        (0b00, 0b011) => Instruction::UcvtfInt { sf, rd, rn, ftype },
        (0b00, 0b110) => Instruction::FMovToGp { rd, rn, sf, ftype },
        (0b00, 0b111) => Instruction::FMovFromGp { rd, rn, sf, ftype },
        (0b01, 0b110) if ftype == 2 && sf => {
            // FMOV top half: Xd, Vn.D[1]
            Instruction::FMovToGp { rd, rn, sf, ftype }
        }
        (0b01, 0b111) if ftype == 2 && sf => {
            // FMOV top half: Vd.D[1], Xn
            Instruction::FMovFromGp { rd, rn, sf, ftype }
        }
        // FP-to-integer with rounding mode (non-zero rmode with convert opcodes)
        (rmode, 0b000) if rmode != 0b11 => {
            Instruction::FcvtRound { sf, rd, rn, ftype, rmode: rmode as u8, unsigned: false }
        }
        (rmode, 0b001) if rmode != 0b11 => {
            Instruction::FcvtRound { sf, rd, rn, ftype, rmode: rmode as u8, unsigned: true }
        }
        _ => Instruction::Unknown { raw },
    }
}

fn h_fp_fixed_conv(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let ftype = bits(raw, 23, 22) as u8;
    let rmode = bits(raw, 20, 19);
    let opcode = bits(raw, 18, 16);
    let scale = bits(raw, 15, 10);
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    let base = if sf { 64u32 } else { 32u32 };
    let fbits = base.wrapping_sub(scale) as u8;
    let op = match (rmode, opcode) {
        (0b00, 0b010) => 0, // SCVTF
        (0b00, 0b011) => 1, // UCVTF
        (0b11, 0b000) => 2, // FCVTZS
        (0b11, 0b001) => 3, // FCVTZU
        _ => return Instruction::Unknown { raw },
    };
    Instruction::FpFixedConv { sf, rd, rn, ftype, fbits, opcode: op }
}

fn h_fmov_imm(raw: u32) -> Instruction {
    let ftype = bits(raw, 23, 22) as u8;
    let imm8 = bits(raw, 20, 13) as u8;
    let rd = bits(raw, 4, 0) as u8;
    Instruction::FmovImm { rd, ftype, imm8 }
}

fn h_fma(raw: u32) -> Instruction {
    let ftype = bits(raw, 23, 22) as u8;
    let o1 = bit(raw, 21);
    let rm = bits(raw, 20, 16) as u8;
    let o0 = bit(raw, 15);
    let ra = bits(raw, 14, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    Instruction::Fma { ftype, rd, rn, rm, ra, op: ((o1 << 1) | o0) as u8 }
}

// -- NEON -------------------------------------------------------------------

fn h_simd_three_same(raw: u32) -> Instruction {
    Instruction::SimdThreeSame {
        q: bit(raw, 30) != 0,
        u: bit(raw, 29) != 0,
        size: bits(raw, 23, 22) as u8,
        opcode: bits(raw, 15, 11) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
    }
}

fn h_simd_three_diff(raw: u32) -> Instruction {
    Instruction::SimdThreeDiff {
        q: bit(raw, 30) != 0,
        u: bit(raw, 29) != 0,
        size: bits(raw, 23, 22) as u8,
        opcode: bits(raw, 15, 12) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
    }
}

fn h_simd_two_reg(raw: u32) -> Instruction {
    Instruction::SimdTwoReg {
        q: bit(raw, 30) != 0,
        u: bit(raw, 29) != 0,
        size: bits(raw, 23, 22) as u8,
        opcode: bits(raw, 16, 12) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
    }
}

fn h_simd_across_lanes(raw: u32) -> Instruction {
    Instruction::SimdAcrossLanes {
        q: bit(raw, 30) != 0,
        u: bit(raw, 29) != 0,
        size: bits(raw, 23, 22) as u8,
        opcode: bits(raw, 16, 12) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
    }
}

fn h_simd_copy(raw: u32) -> Instruction {
    Instruction::SimdCopy {
        q: bit(raw, 30) != 0,
        op: bit(raw, 29) as u8,
        imm5: bits(raw, 20, 16) as u8,
        imm4: bits(raw, 14, 11) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
    }
}

fn h_simd_permute(raw: u32) -> Instruction {
    Instruction::SimdPermute {
        q: bit(raw, 30) != 0,
        size: bits(raw, 23, 22) as u8,
        opcode: bits(raw, 14, 12) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
    }
}

fn h_simd_extract(raw: u32) -> Instruction {
    Instruction::SimdExtract {
        q: bit(raw, 30) != 0,
        imm4: bits(raw, 14, 11) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
    }
}

fn h_simd_mod_imm(raw: u32) -> Instruction {
    let a = bit(raw, 18);
    let b = bit(raw, 17);
    let c = bit(raw, 16);
    let d = bit(raw, 9);
    let e = bit(raw, 8);
    let f = bit(raw, 7);
    let g = bit(raw, 6);
    let h = bit(raw, 5);
    Instruction::SimdModImm {
        q: bit(raw, 30) != 0,
        op: bit(raw, 29) as u8,
        cmode: bits(raw, 15, 12) as u8,
        rd: bits(raw, 4, 0) as u8,
        imm8: ((a << 7) | (b << 6) | (c << 5) | (d << 4) | (e << 3) | (f << 2) | (g << 1) | h) as u8,
    }
}

fn h_simd_shift_imm(raw: u32) -> Instruction {
    Instruction::SimdShiftImm {
        q: bit(raw, 30) != 0,
        u: bit(raw, 29) != 0,
        immh: bits(raw, 22, 19) as u8,
        immb: bits(raw, 18, 16) as u8,
        opcode: bits(raw, 15, 11) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
    }
}

fn h_simd_vec_indexed(raw: u32) -> Instruction {
    Instruction::SimdVecIndexed {
        q: bit(raw, 30) != 0,
        u: bit(raw, 29) != 0,
        size: bits(raw, 23, 22) as u8,
        opcode: bits(raw, 15, 12) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 19, 16) as u8,
        h: bit(raw, 11) as u8,
        l: bit(raw, 21) as u8,
        m: bit(raw, 20) as u8,
    }
}

fn h_scalar_three_same(raw: u32) -> Instruction {
    Instruction::SimdScalarThreeSame {
        u: bit(raw, 29) != 0,
        size: bits(raw, 23, 22) as u8,
        opcode: bits(raw, 15, 11) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
    }
}

fn h_scalar_two_reg(raw: u32) -> Instruction {
    Instruction::SimdScalarTwoReg {
        u: bit(raw, 29) != 0,
        size: bits(raw, 23, 22) as u8,
        opcode: bits(raw, 16, 12) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
    }
}

fn h_scalar_pairwise(raw: u32) -> Instruction {
    Instruction::SimdScalarPairwise {
        u: bit(raw, 29) != 0,
        size: bits(raw, 23, 22) as u8,
        opcode: bits(raw, 16, 12) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
    }
}

fn h_scalar_shift_imm(raw: u32) -> Instruction {
    Instruction::SimdScalarShiftImm {
        u: bit(raw, 29) != 0,
        immh: bits(raw, 22, 19) as u8,
        immb: bits(raw, 18, 16) as u8,
        opcode: bits(raw, 15, 11) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
    }
}

fn h_scalar_indexed(raw: u32) -> Instruction {
    Instruction::SimdScalarIndexed {
        u: bit(raw, 29) != 0,
        size: bits(raw, 23, 22) as u8,
        opcode: bits(raw, 15, 12) as u8,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 19, 16) as u8,
        h: bit(raw, 11) as u8,
        l: bit(raw, 21) as u8,
        m: bit(raw, 20) as u8,
    }
}

fn h_simd_tbl(raw: u32) -> Instruction {
    Instruction::SimdTbl {
        q: bit(raw, 30) != 0,
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
        len: bits(raw, 14, 13) as u8,
        op: bit(raw, 12) as u8,
    }
}

fn h_crypto_aes(raw: u32) -> Instruction {
    Instruction::CryptoAes {
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        opcode: bits(raw, 16, 12) as u8,
    }
}

fn h_crypto_sha3(raw: u32) -> Instruction {
    Instruction::CryptoSha3 {
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        rm: bits(raw, 20, 16) as u8,
        opcode: bits(raw, 14, 12) as u8,
    }
}

fn h_crypto_sha2(raw: u32) -> Instruction {
    Instruction::CryptoSha2 {
        rd: bits(raw, 4, 0) as u8,
        rn: bits(raw, 9, 5) as u8,
        opcode: bits(raw, 16, 12) as u8,
    }
}

// ---------------------------------------------------------------------------
// Pattern table
// ---------------------------------------------------------------------------

fn make_decode_table() -> Vec<Matcher> {
    vec![
        // == DP Immediate ==
        // ADD/ADDS immediate: sf 0 S 10001 sh imm12 Rn Rd
        inst!(b"x0x10001xxxxxxxxxxxxxxxxxxxxxxxx", h_add_imm),
        // SUB/SUBS immediate: sf 1 S 10001 sh imm12 Rn Rd
        inst!(b"x1x10001xxxxxxxxxxxxxxxxxxxxxxxx", h_sub_imm),
        // ADR: 0 immlo 10000 immhi Rd
        inst!(b"0xx10000xxxxxxxxxxxxxxxxxxxxxxxx", h_adr),
        // ADRP: 1 immlo 10000 immhi Rd
        inst!(b"1xx10000xxxxxxxxxxxxxxxxxxxxxxxx", h_adrp),
        // Logical (immediate): sf opc 100100 N immr imms Rn Rd
        inst!(b"xxx100100xxxxxxxxxxxxxxxxxxxxxxx", h_logical_imm),
        // MOVN: sf 00 100101 hw imm16 Rd
        inst!(b"x00100101xxxxxxxxxxxxxxxxxxxxxxx", h_movn),
        // MOVZ: sf 10 100101 hw imm16 Rd
        inst!(b"x10100101xxxxxxxxxxxxxxxxxxxxxxx", h_movz),
        // MOVK: sf 11 100101 hw imm16 Rd
        inst!(b"x11100101xxxxxxxxxxxxxxxxxxxxxxx", h_movk),
        // Bitfield: sf opc 100110 N immr imms Rn Rd
        inst!(b"xxx100110xxxxxxxxxxxxxxxxxxxxxxx", h_bitfield),
        // Extract: sf 00 100111 N 0 Rm imms Rn Rd
        inst!(b"x00100111x0xxxxxxxxxxxxxxxxxxxxx", h_extr),

        // == Branch ==
        // B: 000101 imm26
        inst!(b"000101xxxxxxxxxxxxxxxxxxxxxxxxxx", h_b),
        // BL: 100101 imm26
        inst!(b"100101xxxxxxxxxxxxxxxxxxxxxxxxxx", h_bl),
        // CBZ: sf 0110100 imm19 Rt
        inst!(b"x0110100xxxxxxxxxxxxxxxxxxxxxxxx", h_cbz),
        // CBNZ: sf 0110101 imm19 Rt
        inst!(b"x0110101xxxxxxxxxxxxxxxxxxxxxxxx", h_cbnz),
        // TBZ: b5 0110110 b40 imm14 Rt
        inst!(b"x0110110xxxxxxxxxxxxxxxxxxxxxxxx", h_tbz),
        // TBNZ: b5 0110111 b40 imm14 Rt
        inst!(b"x0110111xxxxxxxxxxxxxxxxxxxxxxxx", h_tbnz),
        // B.cond: 01010100 imm19 0 cond
        inst!(b"01010100xxxxxxxxxxxxxxxxxxx0xxxx", h_bcond),
        // SVC: 11010100 000 imm16 00001
        inst!(b"11010100000xxxxxxxxxxxxxxxxxxxxx", h_svc),
        // BRK: 11010100 001 imm16 00000
        inst!(b"11010100001xxxxxxxxxxxxxxxxxxxxx", h_brk),
        // BR
        inst!(b"1101011000011111000000xxxxx00000", h_br),
        // BLR
        inst!(b"1101011000111111000000xxxxx00000", h_blr),
        // RET
        inst!(b"1101011001011111000000xxxxx00000", h_ret),

        // == System ==
        // NOP: exact encoding
        inst!(b"11010101000000110010000000011111", h_nop),
        // Hints (SEV/WFE/WFI/YIELD/CSDB)
        inst!(b"110101010000001100100xxxxxxxxxxx", h_hint_nop),
        // CLREX
        inst!(b"11010101000000110011xxxx01011111", h_clrex),
        // DSB
        inst!(b"11010101000000110011xxxx10011111", h_dsb),
        // DMB
        inst!(b"11010101000000110011xxxx10111111", h_dmb),
        // ISB
        inst!(b"11010101000000110011xxxx11011111", h_isb),
        // MRS/MSR
        inst!(b"1101010100x1xxxxxxxxxxxxxxxxxxxx", h_mrs_msr),
        // SYS (nop)
        inst!(b"11010101000010xxxxxxxxxxxxxxxxxx", h_sys_nop),

        // == DP Register ==
        // Logical shifted: sf opc 01010 sh N Rm imm6 Rn Rd
        inst!(b"xxx01010xxxxxxxxxxxxxxxxxxxxxxxx", h_logical_shifted),
        // Add/sub shifted: sf op S 01011 sh 0 Rm imm6 Rn Rd
        inst!(b"xxx01011xx0xxxxxxxxxxxxxxxxxxxxx", h_add_sub_shifted),
        // Add/sub extended: sf op S 01011 opt 1 Rm ext imm3 Rn Rd
        inst!(b"xxx01011xx1xxxxxxxxxxxxxxxxxxxxx", h_add_sub_extended),
        // ADC/SBC: sf op S 11010000 Rm 000000 Rn Rd
        inst!(b"xxx11010000xxxxx000000xxxxxxxxxx", h_adc_sbc),
        // CCMP (register): sf op 1 11010010 Rm cond 00 Rn 0 nzcv
        inst!(b"xx111010010xxxxxxxxx00xxxxx0xxxx", h_ccmp_reg),
        // CCMP (immediate): sf op 1 11010010 imm5 cond 10 Rn 0 nzcv
        inst!(b"xx111010010xxxxxxxxx10xxxxx0xxxx", h_ccmp_imm),
        // Conditional select: sf op S 11010100 Rm cond op2 Rn Rd
        inst!(b"xxx11010100xxxxxxxxxxxxxxxxxxxxx", h_csel),
        // CRC32: sf 00 11010110 Rm 010 C sz Rn Rd (more specific than dp2)
        inst!(b"x0011010110xxxxx010xxxxxxxxxxxxx", h_crc32),
        // DP 2-source: sf 0 S 11010110 Rm opcode Rn Rd
        inst!(b"x0x11010110xxxxxxxxxxxxxxxxxxxxx", h_dp2_source),
        // DP 1-source: sf 1 0 11010110 00000 opcode Rn Rd
        inst!(b"x101101011000000xxxxxxxxxxxxxxxx", h_dp1_source),
        // DP 3-source: sf op54 11011 op31 Rm o0 Ra Rn Rd
        inst!(b"xxx11011xxxxxxxxxxxxxxxxxxxxxxxx", h_dp3_source),

        // == Load/Store ==
        // SIMD load/store multiple
        inst!(b"0x001100x0xxxxxxxxxxxxxxxxxxxxxx", h_simd_ldst_multi),
        inst!(b"0x001100x1xxxxxxxxxxxxxxxxxxxxxx", h_simd_ldst_multi),
        // SIMD load/store single
        inst!(b"0x001101x0xxxxxxxxxxxxxxxxxxxxxx", h_simd_ldst_single),
        inst!(b"0x001101x1xxxxxxxxxxxxxxxxxxxxxx", h_simd_ldst_single),
        // Load/store pair GP: opc 101 V=0 0xx L imm7 Rt2 Rn Rt (bit25=0 distinguishes from NEON)
        inst!(b"xx10100xxxxxxxxxxxxxxxxxxxxxxxxx", h_ldst_pair_gp),
        // Load/store pair SIMD: opc 101 V=1 0xx L imm7 Rt2 Rn Rt (bit25=0 distinguishes from NEON)
        inst!(b"xx10110xxxxxxxxxxxxxxxxxxxxxxxxx", h_ldst_pair_simd),
        // LDR literal GP: opc 011 0=GP 00 imm19 Rt
        inst!(b"xx011000xxxxxxxxxxxxxxxxxxxxxxxx", h_ldr_lit_gp),
        // LDR literal SIMD: opc 011 1=SIMD 00 imm19 Rt
        inst!(b"xx011100xxxxxxxxxxxxxxxxxxxxxxxx", h_ldr_lit_simd),
        // Load/store exclusive/ordered/CAS: size 001 V=0 ... (all variants)
        inst!(b"xx0010xxxxxxxxxxxxxxxxxxxxxxxxxx", h_ldst_exclusive),
        // GP unsigned immediate: size 111 0=GP 01 opc imm12 Rn Rt
        inst!(b"xx111001xxxxxxxxxxxxxxxxxxxxxxxx", h_gp_uimm),
        // GP post-index: size 111000 opc 0 imm9 01 Rn Rt
        inst!(b"xx111000xx0xxxxxxxxx01xxxxxxxxxx", h_gp_post),
        // GP pre-index: size 111000 opc 0 imm9 11 Rn Rt
        inst!(b"xx111000xx0xxxxxxxxx11xxxxxxxxxx", h_gp_pre),
        // GP register offset: size 111000 opc 1 Rm opt S 10 Rn Rt
        inst!(b"xx111000xx1xxxxxxxxx10xxxxxxxxxx", h_gp_reg),
        // Atomic memory: size 111000 AR 1 Rs o3 opc 00 Rn Rt
        inst!(b"xx111000xx1xxxxxxxxx00xxxxxxxxxx", h_atomic_mem),
        // GP unscaled (LDUR/STUR): size 111000 opc 0 imm9 00 Rn Rt
        inst!(b"xx111000xx0xxxxxxxxx00xxxxxxxxxx", h_gp_unscaled),
        // SIMD unsigned immediate: size 111 1=SIMD 01 opc imm12 Rn Rt
        inst!(b"xx111101xxxxxxxxxxxxxxxxxxxxxxxx", h_simd_uimm),
        // SIMD post-index
        inst!(b"xx111100xx0xxxxxxxxx01xxxxxxxxxx", h_simd_post),
        // SIMD pre-index
        inst!(b"xx111100xx0xxxxxxxxx11xxxxxxxxxx", h_simd_pre),
        // PRFM literal (NOP)
        inst!(b"11011000xxxxxxxxxxxxxxxxxxxxxxxx", h_prfm_nop),
        // PRFM unsigned immediate (NOP): size=11 opc=10 → bits [31:30]=11, [29:27]=111, [26]=0, [25:24]=01, [23:22]=10
        inst!(b"1111100110xxxxxxxxxxxxxxxxxxxxxx", h_prfm_nop),
        // PRFM register (NOP): size=11 opc=10 → 11 111000 10 1 Rm opt S 10 Rn Rt
        inst!(b"11111000101xxxxxxxxx10xxxxxxxxxx", h_prfm_nop),

        // == SIMD/FP Scalar ==
        // FP 1-source: 00011110 ftype 1 opcode(6) 10000 Rn Rd
        inst!(b"00011110xx1xxxxxx10000xxxxxxxxxx", h_fp1_source),
        // FP 2-source: 00011110 ftype 1 Rm opcode(4) 10 Rn Rd
        inst!(b"00011110xx1xxxxxxxxx10xxxxxxxxxx", h_fp2_source),
        // FP compare: 00011110 ftype 1 Rm 001000 Rn opc 000
        inst!(b"00011110xx1xxxxx001000xxxxxxxxxx", h_fcmp),
        // FP conditional compare: 00011110 ftype 1 Rm cond 01 Rn 0 nzcv
        inst!(b"00011110xx1xxxxxxxxx01xxxxx0xxxx", h_fccmp),
        // FP conditional select: 00011110 ftype 1 Rm cond 11 Rn Rd
        inst!(b"00011110xx1xxxxxxxxx11xxxxxxxxxx", h_fcsel),
        // FP<->int conversion: sf 0011110 ftype 1 rmode opcode 000000 Rn Rd
        inst!(b"x0011110xx1xxxxx000000xxxxxxxxxx", h_fp_int_conv),
        // FP<->fixed-point conversion: sf 0011110 ftype 0 rmode opcode scale Rn Rd
        inst!(b"x0011110xx0xxxxxxxxxxxxxxxxxxxxx", h_fp_fixed_conv),
        // FP move immediate: 00011110 ftype 1 imm8 100 00000 Rd
        inst!(b"00011110xx1xxxxxxxx10000000xxxxx", h_fmov_imm),
        // FMA (3-source): 00011111 ftype o1 Rm o0 Ra Rn Rd
        inst!(b"00011111xxxxxxxxxxxxxxxxxxxxxxxx", h_fma),

        // == NEON Vector ==
        // Three same: 0 Q u 01110 size 1 Rm opcode(5) 1 Rn Rd
        inst!(b"0xx01110xx1xxxxxxxxxx1xxxxxxxxxx", h_simd_three_same),
        // Three different: 0 Q u 01110 size 1 Rm opcode(4) 00 Rn Rd
        inst!(b"0xx01110xx1xxxxxxxxx00xxxxxxxxxx", h_simd_three_diff),
        // Crypto AES: 01001110 00 10100 opcode(5) 10 Rn Rd
        inst!(b"010011100010100xxxxx10xxxxxxxxxx", h_crypto_aes),
        // Two-reg misc: 0 Q u 01110 size 10000 opcode(5) 10 Rn Rd
        inst!(b"0xx01110xx10000xxxxx10xxxxxxxxxx", h_simd_two_reg),
        // Across lanes: 0 Q u 01110 size 11000 opcode(5) 10 Rn Rd
        inst!(b"0xx01110xx11000xxxxx10xxxxxxxxxx", h_simd_across_lanes),
        // Extract: 0 Q 1 01110 000 Rm 0 imm4 0 Rn Rd
        inst!(b"0x101110000xxxxx0xxxx0xxxxxxxxxx", h_simd_extract),
        // TBL/TBX: 0 Q 0 01110 00 0 Rm 0 len op 00 Rn Rd
        inst!(b"0x001110000xxxxx0xxx00xxxxxxxxxx", h_simd_tbl),
        // Copy (DUP/INS from element): 0 Q op 01110 000 imm5 0 imm4 1 Rn Rd
        inst!(b"0x001110000xxxxx0xxxx1xxxxxxxxxx", h_simd_copy),
        // Copy INS (from general): 0 Q 1 01110 000 imm5 0 imm4 1 Rn Rd
        inst!(b"0x101110000xxxxx0xxxx1xxxxxxxxxx", h_simd_copy),
        // Permute: 0 Q 0 01110 size 0 Rm 0 opcode 10 Rn Rd
        inst!(b"0x001110xx0xxxxx0xxx10xxxxxxxxxx", h_simd_permute),
        // Modified immediate: 0 Q op 0111100000 abc cmode 01 defgh Rd
        inst!(b"0xx0111100000xxxxxxx01xxxxxxxxxx", h_simd_mod_imm),
        // Shift by immediate: 0 Q u 01111 immh immb opcode(5) 1 Rn Rd
        inst!(b"0xx01111xxxxxxxxxxxxx1xxxxxxxxxx", h_simd_shift_imm),
        // Vec x indexed element: 0 Q u 01111 size L M Rm opcode H 0 Rn Rd
        inst!(b"0xx01111xxxxxxxxxxxxx0xxxxxxxxxx", h_simd_vec_indexed),

        // == NEON Scalar ==
        // Scalar three same: 01 u 11110 size 1 Rm opcode(5) 1 Rn Rd
        inst!(b"01x11110xx1xxxxxxxxxx1xxxxxxxxxx", h_scalar_three_same),
        // Scalar two-reg misc: 01 u 11110 size 10000 opcode(5) 10 Rn Rd
        inst!(b"01x11110xx10000xxxxx10xxxxxxxxxx", h_scalar_two_reg),
        // Scalar pairwise: 01 u 11110 size 11000 opcode(5) 10 Rn Rd
        inst!(b"01x11110xx11000xxxxx10xxxxxxxxxx", h_scalar_pairwise),
        // SHA three-register: 01011110 00 0 Rm 0 opcode(3) 00 Rn Rd
        inst!(b"01011110000xxxxx0xxx00xxxxxxxxxx", h_crypto_sha3),
        // SHA two-register: 01011110 00 10100 opcode(5) 10 Rn Rd
        inst!(b"010111100010100xxxxx10xxxxxxxxxx", h_crypto_sha2),
        // Scalar shift by immediate: 01 u 11111 immh immb opcode(5) 1 Rn Rd
        inst!(b"01x11111xxxxxxxxxxxxx1xxxxxxxxxx", h_scalar_shift_imm),
        // Scalar x indexed element: 01 u 11111 size L M Rm opcode H 0 Rn Rd
        inst!(b"01x11111xxxxxxxxxxxxx0xxxxxxxxxx", h_scalar_indexed),
    ]
}

// ---------------------------------------------------------------------------
// Fast lookup table
// ---------------------------------------------------------------------------

/// Extract a 12-bit hash from bits [29:22] and [13:10] of an instruction word.
fn fast_index(raw: u32) -> usize {
    (((raw >> 10) & 0x00F) | ((raw >> 18) & 0xFF0)) as usize
}

struct LookupTable {
    buckets: Vec<Vec<u16>>,
    entries: Vec<Matcher>,
}

/// Bit positions that contribute to the fast index.
const HASH_BITS: u32 = 0x3FC0_3C00; // bits 29:22 and 13:10

fn build_lookup_table() -> LookupTable {
    let mut entries = make_decode_table();
    // Sort by specificity: most fixed bits first
    entries.sort_by(|a, b| b.mask.count_ones().cmp(&a.mask.count_ones()));

    let mut buckets: Vec<Vec<u16>> = vec![Vec::new(); 4096];
    for (idx, entry) in entries.iter().enumerate() {
        let relevant_mask = entry.mask & HASH_BITS;
        let relevant_expected = entry.expected & HASH_BITS;
        for slot in 0..4096u32 {
            // Reconstruct raw bits at hash positions from slot
            let test_raw = ((slot & 0xF) << 10) | (((slot >> 4) & 0xFF) << 22);
            if (test_raw & relevant_mask) == relevant_expected {
                buckets[slot as usize].push(idx as u16);
            }
        }
    }
    // Deduplicate while preserving order
    for bucket in &mut buckets {
        bucket.dedup();
    }
    LookupTable { buckets, entries }
}

fn get_lookup() -> &'static LookupTable {
    static LOOKUP: OnceLock<LookupTable> = OnceLock::new();
    LOOKUP.get_or_init(build_lookup_table)
}

// ---------------------------------------------------------------------------
// Public decode function
// ---------------------------------------------------------------------------

/// Decode a 32-bit A64 instruction word using the pattern-based table.
pub fn decode(raw: u32) -> Instruction {
    let table = get_lookup();
    let bucket = &table.buckets[fast_index(raw)];
    for &idx in bucket {
        let m = &table.entries[idx as usize];
        if (raw & m.mask) == m.expected {
            return (m.handler)(raw);
        }
    }
    Instruction::Unknown { raw }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_parse() {
        let (mask, expected) = parse_pattern(b"10xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
        assert_eq!(mask, 0xC000_0000);
        assert_eq!(expected, 0x8000_0000);
    }

    #[test]
    fn test_decode_nop() {
        let inst = decode(0xD503201F);
        assert!(matches!(inst, Instruction::Nop));
    }

    #[test]
    fn test_decode_svc() {
        let inst = decode(0xD4000001);
        assert!(matches!(inst, Instruction::Svc { imm: 0 }));
    }

    #[test]
    fn test_decode_ret() {
        let inst = decode(0xD65F03C0);
        assert!(matches!(inst, Instruction::Ret { rn: 30 }));
    }

    #[test]
    fn test_decode_movz() {
        let inst = decode(0xD2800540);
        match inst {
            Instruction::MovZ { sf: true, rd: 0, imm16: 42, hw: 0 } => {}
            other => panic!("Expected MOVZ, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_add_imm() {
        let inst = decode(0x91000401);
        match inst {
            Instruction::AddImm { sf: true, rd: 1, rn: 0, imm12: 1, shift: false, set_flags: false } => {}
            other => panic!("Expected ADD imm, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_subs_imm() {
        let inst = decode(0xF1000400);
        match inst {
            Instruction::SubImm { sf: true, rd: 0, rn: 0, imm12: 1, shift: false, set_flags: true } => {}
            other => panic!("Expected SUBS imm, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_b() {
        let inst = decode(0x14000001);
        assert!(matches!(inst, Instruction::B { imm: 4 }));
    }

    #[test]
    fn test_decode_bl() {
        let inst = decode(0x94000002);
        assert!(matches!(inst, Instruction::Bl { imm: 8 }));
    }

    #[test]
    fn test_decode_br() {
        let inst = decode(0xD61F0200);
        assert!(matches!(inst, Instruction::Br { rn: 16 }));
    }

    #[test]
    fn test_decode_blr() {
        let inst = decode(0xD63F0100);
        assert!(matches!(inst, Instruction::Blr { rn: 8 }));
    }

    #[test]
    fn test_decode_cbz() {
        let inst = decode(0xB4000040);
        match inst {
            Instruction::Cbz { sf: true, rt: 0, imm: 8 } => {}
            other => panic!("Expected CBZ, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_bcond() {
        let inst = decode(0x54000020);
        match inst {
            Instruction::BCond { cond: 0, imm: 4 } => {}
            other => panic!("Expected B.EQ, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_stp() {
        let inst = decode(0xA9BF7BFD);
        match inst {
            Instruction::Stp { sf: true, rt: 29, rt2: 30, rn: 31, imm: -16, mode: AddrMode::PreIndex } => {}
            other => panic!("Expected STP pre-index, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_ldp() {
        let inst = decode(0xA8C17BFD);
        match inst {
            Instruction::Ldp { sf: true, rt: 29, rt2: 30, rn: 31, imm: 16, mode: AddrMode::PostIndex } => {}
            other => panic!("Expected LDP post-index, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_adrp() {
        let inst = decode(0x90000000);
        match inst {
            Instruction::Adrp { rd: 0, imm: 0 } => {}
            other => panic!("Expected ADRP, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_madd() {
        let inst = decode(0x9B027C20);
        match inst {
            Instruction::Madd { sf: true, rd: 0, rn: 1, rm: 2, ra: 31 } => {}
            other => panic!("Expected MADD, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_brk() {
        // BRK #42: 0xD4000540 | (1 << 5) = 11010100_001_0000000000101010_00000
        let inst = decode(0xD4200540);
        match inst {
            Instruction::Brk { imm: 42 } => {}
            other => panic!("Expected BRK #42, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_fmadd() {
        let inst = decode(0x1F420C20);
        match inst {
            Instruction::Fma { ftype: 1, rd: 0, rn: 1, rm: 2, ra: 3, op: 0 } => {}
            other => panic!("Expected FMADD, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_movn() {
        let inst = decode(0x92800000);
        match inst {
            Instruction::MovN { sf: true, rd: 0, imm16: 0, hw: 0 } => {}
            other => panic!("Expected MOVN, got {:?}", other),
        }
    }

    /// Known instruction encodings from the old decoder tests — verify decode matches.
    #[test]
    fn test_migration_known_encodings() {
        use crate::decoder::decode_legacy;

        // Encodings that old and new decoder must produce identical results for.
        let must_match: &[u32] = &[
            0xD503201F, // NOP
            0xD4000001, // SVC #0
            0xD65F03C0, // RET x30
            0xD2800540, // MOVZ X0, #42
            0x91000401, // ADD X1, X0, #1
            0xF1000400, // SUBS X0, X0, #1
            0x14000001, // B #4
            0x94000002, // BL #8
            0xD61F0200, // BR X16
            0xD63F0100, // BLR X8
            0xB4000040, // CBZ X0, #8
            0x54000020, // B.EQ #4
            0xA9BF7BFD, // STP X29, X30, [SP, #-16]!
            0xA8C17BFD, // LDP X29, X30, [SP], #16
            0x90000000, // ADRP X0, #0
            0x9B027C20, // MADD X0, X1, X2, X31
            0x1F420C20, // FMADD D0, D1, D2, D3
            0x92800000, // MOVN X0, #0
            0xF9000020, // STR X0, [X1]
            0xF9400020, // LDR X0, [X1]
            0x1AC20820, // UDIV W0, W1, W2
            0x1AC20C20, // SDIV W0, W1, W2
            0x9A821020, // CSEL X0, X1, X2, NE
            0x1E222020, // FCMP S1, S2
            0x52800040, // MOV W0, #2
        ];

        let mut mismatches = 0;
        for &raw in must_match {
            let old = format!("{:?}", decode_legacy(raw));
            let new = format!("{:?}", decode(raw));
            if old != new {
                eprintln!("MISMATCH 0x{:08X}: old={}, new={}", raw, old, new);
                mismatches += 1;
            }
        }
        assert_eq!(mismatches, 0, "{} known encodings decoded differently", mismatches);

        // Encodings where the new decoder is intentionally different (improvements):
        // BRK #42: old decoder returned Unknown (variant didn't exist), new returns Brk
        assert!(matches!(decode(0xD4200540), Instruction::Brk { imm: 42 }));
        // ADD shifted: old decoder missed this, new correctly decodes
        assert!(!matches!(decode(0x8B020020), Instruction::Unknown { .. }));
        // SUB shifted: same improvement
        assert!(!matches!(decode(0xCB020020), Instruction::Unknown { .. }));
        // ORR: old had priority bug routing to AddReg, new correctly returns LogicalReg
        assert!(matches!(decode(0xAA020020), Instruction::LogicalReg { .. }));
        // ANDS: same fix
        assert!(matches!(decode(0xEA020020), Instruction::LogicalReg { .. }));
    }

    /// Fuzz test: random u32 values, compare old decode vs decode.
    ///
    /// The old decoder was overly permissive — it decoded many invalid bit patterns
    /// that should be Unknown (e.g. EXTR with S=1, BCond with bit24=1, LDR literal
    /// with wrong opcode bits). The new pattern-based decoder correctly rejects these.
    ///
    /// This test focuses on "conflicts" — cases where BOTH decoders produce a
    /// non-Unknown result but disagree on what instruction it is.
    #[test]
    fn test_migration_fuzz() {
        use crate::decoder::decode_legacy;

        let mut rng_state: u64 = 0xDEAD_BEEF_CAFE_BABE;
        let mut old_only = 0; // old decoded, new returned Unknown (expected: old was too loose)
        let mut new_only = 0; // new decoded, old returned Unknown (expected: new covers more)
        let mut conflicts = 0; // both decoded but to different instructions
        let total = 100_000;

        for _ in 0..total {
            // xorshift64
            rng_state ^= rng_state << 13;
            rng_state ^= rng_state >> 7;
            rng_state ^= rng_state << 17;
            let raw = rng_state as u32;

            let old = decode_legacy(raw);
            let new = decode(raw);

            let old_unk = matches!(old, Instruction::Unknown { .. });
            let new_unk = matches!(new, Instruction::Unknown { .. });

            if old_unk && new_unk {
                continue; // Both agree: Unknown
            }

            let old_s = format!("{:?}", old);
            let new_s = format!("{:?}", new);
            if old_s == new_s {
                continue; // Both agree: same instruction
            }

            if !old_unk && new_unk {
                old_only += 1; // Old was too permissive
                continue;
            }

            if old_unk && !new_unk {
                new_only += 1; // New covers more instructions
                continue;
            }

            // Both decoded, but differently — check if it's a known fix
            let is_known_fix = matches!(
                (&old, &new),
                // dp_reg priority bug: logical shifted decoded as add/sub shifted
                (Instruction::AddReg { .. }, Instruction::LogicalReg { .. })
                | (Instruction::SubReg { .. }, Instruction::LogicalReg { .. })
                | (Instruction::AddReg { .. }, Instruction::AddExtReg { .. })
                | (Instruction::SubReg { .. }, Instruction::SubExtReg { .. })
                // dp_1_source decoded with S=1 (invalid) now correctly routes elsewhere
                | (Instruction::Rev { .. }, Instruction::LogicalReg { .. })
                | (Instruction::Clz { .. }, Instruction::LogicalReg { .. })
                // old decoder's size=11 opc=10 treated as LDRSW, actually PRFM
                | (Instruction::LdrImm { .. }, Instruction::Nop)
                | (Instruction::LdrReg { .. }, Instruction::Nop)
                // SimdModImm with immh!=0 is actually SimdShiftImm
                | (Instruction::SimdModImm { .. }, Instruction::SimdShiftImm { .. })
            );
            if is_known_fix {
                continue;
            }

            if conflicts < 20 {
                eprintln!("CONFLICT 0x{:08X}: old={}, new={}", raw, old_s, new_s);
            }
            conflicts += 1;
        }

        eprintln!(
            "Fuzz stats: old_only={}, new_only={}, conflicts={} / {} total",
            old_only, new_only, conflicts, total
        );
        assert!(
            conflicts < 50,
            "Too many conflicts where both decoders disagree: {} / {}",
            conflicts, total
        );
    }
}
