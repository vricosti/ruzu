// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! ARM64 (AArch64) instruction decoder.
//!
//! Decodes a 32-bit A64 instruction word into a structured [`Instruction`] enum.
//! Covers the ~120 most common instructions needed for Switch homebrew execution.

// ---------------------------------------------------------------------------
// Supporting types
// ---------------------------------------------------------------------------

/// Shift type used by data-processing (register) instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftType {
    LSL = 0,
    LSR = 1,
    ASR = 2,
    ROR = 3,
}

impl ShiftType {
    pub fn from_u32(val: u32) -> Self {
        match val & 3 {
            0 => Self::LSL,
            1 => Self::LSR,
            2 => Self::ASR,
            _ => Self::ROR,
        }
    }
}

/// Extend type used by load/store and extended-register instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtendType {
    UXTB = 0,
    UXTH = 1,
    UXTW = 2,
    UXTX = 3,
    SXTB = 4,
    SXTH = 5,
    SXTW = 6,
    SXTX = 7,
}

impl ExtendType {
    pub fn from_u32(val: u32) -> Self {
        match val & 7 {
            0 => Self::UXTB,
            1 => Self::UXTH,
            2 => Self::UXTW,
            3 => Self::UXTX,
            4 => Self::SXTB,
            5 => Self::SXTH,
            6 => Self::SXTW,
            _ => Self::SXTX,
        }
    }
}

/// Addressing mode for load/store instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddrMode {
    /// Unsigned offset / base + offset.
    Offset,
    /// Pre-index: base is updated before access.
    PreIndex,
    /// Post-index: base is updated after access.
    PostIndex,
}

/// Floating-point arithmetic operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FpOp {
    Add,
    Sub,
    Mul,
    Div,
    Max,
    Min,
    MaxNum,
    MinNum,
}

// ---------------------------------------------------------------------------
// Instruction enum
// ---------------------------------------------------------------------------

/// Decoded A64 instruction.
#[derive(Debug, Clone)]
pub enum Instruction {
    // -- Data processing (immediate) ----------------------------------------

    AddImm {
        sf: bool, rd: u8, rn: u8, imm12: u32, shift: bool, set_flags: bool,
    },
    SubImm {
        sf: bool, rd: u8, rn: u8, imm12: u32, shift: bool, set_flags: bool,
    },
    MovZ { sf: bool, rd: u8, imm16: u16, hw: u8 },
    MovK { sf: bool, rd: u8, imm16: u16, hw: u8 },
    MovN { sf: bool, rd: u8, imm16: u16, hw: u8 },
    Adr { rd: u8, imm: i64 },
    Adrp { rd: u8, imm: i64 },
    LogicalImm {
        sf: bool, opc: u8, rd: u8, rn: u8, imm: u64,
    },
    BitfieldMove {
        sf: bool, opc: u8, rd: u8, rn: u8, immr: u8, imms: u8,
    },
    Extr { sf: bool, rd: u8, rn: u8, rm: u8, imms: u8 },

    // -- Data processing (register) -----------------------------------------

    AddReg {
        sf: bool, rd: u8, rn: u8, rm: u8, shift: ShiftType, amount: u8,
        set_flags: bool,
    },
    SubReg {
        sf: bool, rd: u8, rn: u8, rm: u8, shift: ShiftType, amount: u8,
        set_flags: bool,
    },
    LogicalReg {
        sf: bool, opc: u8, rd: u8, rn: u8, rm: u8, shift: ShiftType,
        amount: u8, invert: bool,
    },
    AddExtReg {
        sf: bool, rd: u8, rn: u8, rm: u8, ext: ExtendType, amount: u8,
        set_flags: bool,
    },
    SubExtReg {
        sf: bool, rd: u8, rn: u8, rm: u8, ext: ExtendType, amount: u8,
        set_flags: bool,
    },
    Madd { sf: bool, rd: u8, rn: u8, rm: u8, ra: u8 },
    Msub { sf: bool, rd: u8, rn: u8, rm: u8, ra: u8 },
    Smaddl { rd: u8, rn: u8, rm: u8, ra: u8 },
    Umaddl { rd: u8, rn: u8, rm: u8, ra: u8 },
    Smulh { rd: u8, rn: u8, rm: u8 },
    Umulh { rd: u8, rn: u8, rm: u8 },
    Sdiv { sf: bool, rd: u8, rn: u8, rm: u8 },
    Udiv { sf: bool, rd: u8, rn: u8, rm: u8 },
    Csel { sf: bool, rd: u8, rn: u8, rm: u8, cond: u8, op2: u8 },
    Ccmp {
        sf: bool, rn: u8, rm_or_imm: u8, nzcv: u8, cond: u8, is_imm: bool,
        is_neg: bool,
    },
    Cls { sf: bool, rd: u8, rn: u8 },
    Clz { sf: bool, rd: u8, rn: u8 },
    Rev { sf: bool, rd: u8, rn: u8, opc: u8 },
    Rbit { sf: bool, rd: u8, rn: u8 },

    // -- Load/Store ---------------------------------------------------------

    LdrImm {
        sf: bool, rt: u8, rn: u8, imm: i64, size: u8, mode: AddrMode,
        sign_extend: bool,
    },
    StrImm {
        sf: bool, rt: u8, rn: u8, imm: i64, size: u8, mode: AddrMode,
    },
    LdrReg {
        sf: bool, rt: u8, rn: u8, rm: u8, size: u8, extend: ExtendType,
        shift: bool, sign_extend: bool,
    },
    StrReg {
        sf: bool, rt: u8, rn: u8, rm: u8, size: u8, extend: ExtendType,
        shift: bool,
    },
    LdrLit { sf: bool, rt: u8, imm: i64, sign_extend: bool },
    Ldp {
        sf: bool, rt: u8, rt2: u8, rn: u8, imm: i64, mode: AddrMode,
    },
    Stp {
        sf: bool, rt: u8, rt2: u8, rn: u8, imm: i64, mode: AddrMode,
    },
    Ldxr { sf: bool, rt: u8, rn: u8, size: u8 },
    Stxr { sf: bool, rt: u8, rn: u8, rs: u8, size: u8 },
    Ldaxr { sf: bool, rt: u8, rn: u8, size: u8 },
    Stlxr { sf: bool, rt: u8, rn: u8, rs: u8, size: u8 },
    Ldar { sf: bool, rt: u8, rn: u8, size: u8 },
    Stlr { sf: bool, rt: u8, rn: u8, size: u8 },
    Ldaxp { sf: bool, rt: u8, rt2: u8, rn: u8 },
    Stlxp { sf: bool, rt: u8, rt2: u8, rn: u8, rs: u8 },

    // -- Branch -------------------------------------------------------------

    B { imm: i64 },
    Bl { imm: i64 },
    Br { rn: u8 },
    Blr { rn: u8 },
    Ret { rn: u8 },
    BCond { cond: u8, imm: i64 },
    Cbz { sf: bool, rt: u8, imm: i64 },
    Cbnz { sf: bool, rt: u8, imm: i64 },
    Tbz { rt: u8, bit: u8, imm: i64 },
    Tbnz { rt: u8, bit: u8, imm: i64 },

    // -- System -------------------------------------------------------------

    Svc { imm: u16 },
    Mrs { rt: u8, sys_reg: u32 },
    Msr { rt: u8, sys_reg: u32 },
    Nop,
    Clrex,
    Dmb { option: u8 },
    Dsb { option: u8 },
    Isb,

    // -- SIMD/FP (minimal) --------------------------------------------------

    FMovReg { rd: u8, rn: u8, ftype: u8 },
    FMovToGp { rd: u8, rn: u8, sf: bool, ftype: u8 },
    FMovFromGp { rd: u8, rn: u8, sf: bool, ftype: u8 },
    FArith { op: FpOp, rd: u8, rn: u8, rm: u8, ftype: u8 },
    Fcmp { rn: u8, rm: u8, ftype: u8, with_zero: bool },
    Fcsel { rd: u8, rn: u8, rm: u8, ftype: u8, cond: u8 },
    Fcvt { rd: u8, rn: u8, src_type: u8, dst_type: u8 },
    ScvtfInt { sf: bool, rd: u8, rn: u8, ftype: u8 },
    UcvtfInt { sf: bool, rd: u8, rn: u8, ftype: u8 },
    FcvtzsInt { sf: bool, rd: u8, rn: u8, ftype: u8 },
    FcvtzuInt { sf: bool, rd: u8, rn: u8, ftype: u8 },
    LdrSimd { rt: u8, rn: u8, imm: i64, size: u8, mode: AddrMode },
    StrSimd { rt: u8, rn: u8, imm: i64, size: u8, mode: AddrMode },
    LdpSimd { rt: u8, rt2: u8, rn: u8, imm: i64, size: u8, mode: AddrMode },
    StpSimd { rt: u8, rt2: u8, rn: u8, imm: i64, size: u8, mode: AddrMode },
    Fneg { rd: u8, rn: u8, ftype: u8 },
    Fabs { rd: u8, rn: u8, ftype: u8 },
    Fsqrt { rd: u8, rn: u8, ftype: u8 },
    /// FMADD/FMSUB/FNMADD/FNMSUB — fused multiply-add
    /// op: 0=FMADD, 1=FMSUB, 2=FNMADD, 3=FNMSUB
    Fma { ftype: u8, rd: u8, rn: u8, rm: u8, ra: u8, op: u8 },
    /// FRINTN/FRINTP/FRINTM/FRINTZ/FRINTA/FRINTX/FRINTI — FP rounding
    /// mode: 0=N(nearest), 1=P(+inf), 2=M(-inf), 3=Z(zero), 4=A(tie-away), 6=X(exact), 7=I(current)
    Frint { ftype: u8, rd: u8, rn: u8, mode: u8 },

    // -- NEON/Advanced SIMD ---------------------------------------------------

    /// Advanced SIMD three same: vector arithmetic (ADD, SUB, AND, ORR, MUL, FADD, etc.)
    SimdThreeSame { q: bool, u: bool, size: u8, opcode: u8, rd: u8, rn: u8, rm: u8 },
    /// Advanced SIMD two-register misc: unary ops (NOT, ABS, NEG, REV, CNT, etc.)
    SimdTwoReg { q: bool, u: bool, size: u8, opcode: u8, rd: u8, rn: u8 },
    /// Advanced SIMD copy: DUP, INS, UMOV, SMOV
    SimdCopy { q: bool, op: u8, imm5: u8, imm4: u8, rd: u8, rn: u8 },
    /// Advanced SIMD modified immediate: MOVI, MVNI, FMOV vector immediate
    SimdModImm { q: bool, op: u8, cmode: u8, rd: u8, imm8: u8 },
    /// Advanced SIMD shift by immediate: SHL, SSHR, USHR, SSHLL, etc.
    SimdShiftImm { q: bool, u: bool, immh: u8, immb: u8, opcode: u8, rd: u8, rn: u8 },
    /// Advanced SIMD permute: ZIP1/ZIP2, UZP1/UZP2, TRN1/TRN2
    SimdPermute { q: bool, size: u8, opcode: u8, rd: u8, rn: u8, rm: u8 },
    /// Advanced SIMD extract: EXT
    SimdExtract { q: bool, imm4: u8, rd: u8, rn: u8, rm: u8 },
    /// Advanced SIMD across lanes: ADDV, SADDLV, SMAXV, SMINV, etc.
    SimdAcrossLanes { q: bool, u: bool, size: u8, opcode: u8, rd: u8, rn: u8 },
    /// Advanced SIMD three different: widening/narrowing (SADDL, UADDL, SSUBL, etc.)
    SimdThreeDiff { q: bool, u: bool, size: u8, opcode: u8, rd: u8, rn: u8, rm: u8 },
    /// Advanced SIMD vector x indexed element: FMUL/FMLA by element
    SimdVecIndexed { q: bool, u: bool, size: u8, opcode: u8, rd: u8, rn: u8, rm: u8, h: u8, l: u8, m: u8 },
    /// SIMD load/store multiple structures: LD1/ST1/LD2/ST2/LD3/ST3/LD4/ST4
    SimdLdStMulti { q: bool, load: bool, opcode: u8, size: u8, rn: u8, rt: u8, rm: Option<u8> },
    /// SIMD load/store single structure: LD1/ST1 single element to/from lane
    SimdLdStSingle { q: bool, load: bool, r: u8, opcode: u8, s: u8, size: u8, rn: u8, rt: u8, rm: Option<u8> },
    /// Advanced SIMD scalar three same
    SimdScalarThreeSame { u: bool, size: u8, opcode: u8, rd: u8, rn: u8, rm: u8 },
    /// Advanced SIMD scalar two-reg misc
    SimdScalarTwoReg { u: bool, size: u8, opcode: u8, rd: u8, rn: u8 },
    /// Advanced SIMD scalar pairwise (FADDP scalar, FMAXP, FMINP)
    SimdScalarPairwise { u: bool, size: u8, opcode: u8, rd: u8, rn: u8 },

    // -- Atomics ------------------------------------------------------------

    /// CAS/CASA/CASL/CASAL — Compare and Swap
    Cas { size: u8, rs: u8, rt: u8, rn: u8 },
    /// SWP/SWPA/SWPL/SWPAL — Atomic Swap
    Swp { size: u8, rs: u8, rt: u8, rn: u8 },
    /// LDADD/LDCLR/LDEOR/LDSET — Atomic arithmetic
    /// op: 0=ADD, 1=CLR(BIC), 2=EOR, 3=SET(ORR)
    AtomicOp { size: u8, rs: u8, rt: u8, rn: u8, op: u8 },

    // -- Fallback -----------------------------------------------------------

    Unknown { raw: u32 },
}

// ---------------------------------------------------------------------------
// Bitmask immediate decoder
// ---------------------------------------------------------------------------

/// Decode an A64 logical immediate from (N, immr, imms) fields.
///
/// Returns `None` if the encoding is reserved.
pub fn decode_bitmask_imm(sf: bool, n: u8, immr: u8, imms: u8) -> Option<u64> {
    let imms = imms as u32;
    let immr = immr as u32;

    // Concatenate N and NOT(imms) to determine element size.
    let nimms = ((n as u32) << 6) | ((imms ^ 0x3F) & 0x3F);
    if nimms == 0 {
        return None;
    }

    let len = 31 - nimms.leading_zeros(); // position of highest set bit
    let esize = 1u32 << len;
    let levels = esize - 1;

    // For 32-bit, N must be 0.
    if !sf && n != 0 {
        return None;
    }

    let s = imms & levels;
    let r = immr & levels;

    // s == levels means all-ones for the element size (reserved).
    if s == levels {
        return None;
    }

    // Base element: (s+1) ones.
    let welem: u64 = (1u64 << (s + 1)) - 1;

    // Rotate right by r within esize bits.
    let mask = if esize == 64 {
        u64::MAX
    } else {
        (1u64 << esize) - 1
    };
    let rotated = if r == 0 {
        welem
    } else {
        ((welem >> r) | (welem << (esize - r))) & mask
    };

    // Replicate to fill 64 bits.
    let mut result = rotated;
    let mut size = esize;
    while size < 64 {
        result |= result << size;
        size *= 2;
    }

    if !sf {
        result &= 0xFFFF_FFFF;
    }

    Some(result)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Sign-extend `value` from `bits` width to i64.
#[inline]
fn sign_extend(value: u32, bits: u32) -> i64 {
    let shift = 64 - bits;
    ((value as i64) << shift) >> shift
}

/// Sign-extend a u64 value from `bits` width to i64.
#[inline]
#[allow(dead_code)]
fn sign_extend64(value: u64, bits: u32) -> i64 {
    let shift = 64 - bits;
    ((value as i64) << shift) >> shift
}

/// Extract a bit field from an instruction word.
#[inline]
fn bits(raw: u32, hi: u32, lo: u32) -> u32 {
    (raw >> lo) & ((1 << (hi - lo + 1)) - 1)
}

/// Extract a single bit.
#[inline]
fn bit(raw: u32, pos: u32) -> u32 {
    (raw >> pos) & 1
}

// ---------------------------------------------------------------------------
// Top-level decoder
// ---------------------------------------------------------------------------

/// Decode a 32-bit A64 instruction word.
pub fn decode(raw: u32) -> Instruction {
    let op0 = bits(raw, 28, 25);

    match op0 {
        // Data processing - immediate
        0b1000 | 0b1001 => decode_dp_imm(raw),
        // Branches, exception generating, system
        0b1010 | 0b1011 => decode_branch_system(raw),
        // Loads and stores
        0b0100 | 0b0110 | 0b1100 | 0b1110 => decode_load_store(raw),
        // Data processing - register
        0b0101 | 0b1101 => decode_dp_reg(raw),
        // Data processing - SIMD and FP
        0b0111 | 0b1111 => decode_simd_fp(raw),
        _ => Instruction::Unknown { raw },
    }
}

// ---------------------------------------------------------------------------
// Data processing (immediate)
// ---------------------------------------------------------------------------

fn decode_dp_imm(raw: u32) -> Instruction {
    let op = bits(raw, 25, 23);

    match op {
        // PC-rel addressing: ADR/ADRP
        0b000 | 0b001 => {
            let rd = bits(raw, 4, 0) as u8;
            let immhi = bits(raw, 23, 5);
            let immlo = bits(raw, 30, 29);
            let op = bit(raw, 31);

            if op == 0 {
                // ADR: PC + immhi:immlo
                let imm = ((immhi << 2) | immlo) as u32;
                let imm = sign_extend(imm, 21);
                Instruction::Adr { rd, imm }
            } else {
                // ADRP: PC + immhi:immlo:000000000000 (page-aligned)
                let imm = ((immhi << 2) | immlo) as u32;
                let imm = sign_extend(imm, 21) << 12;
                Instruction::Adrp { rd, imm }
            }
        }
        // Add/subtract (immediate)
        0b010 => {
            let sf = bit(raw, 31) != 0;
            let op = bit(raw, 30);
            let s = bit(raw, 29);
            let sh = bit(raw, 22);
            let imm12 = bits(raw, 21, 10);
            let rn = bits(raw, 9, 5) as u8;
            let rd = bits(raw, 4, 0) as u8;

            if op == 0 {
                Instruction::AddImm {
                    sf, rd, rn, imm12, shift: sh != 0, set_flags: s != 0,
                }
            } else {
                Instruction::SubImm {
                    sf, rd, rn, imm12, shift: sh != 0, set_flags: s != 0,
                }
            }
        }
        // Logical (immediate)
        0b100 => {
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
        // Move wide (immediate)
        0b101 => {
            let sf = bit(raw, 31) != 0;
            let opc = bits(raw, 30, 29);
            let hw = bits(raw, 22, 21) as u8;
            let imm16 = bits(raw, 20, 5) as u16;
            let rd = bits(raw, 4, 0) as u8;

            match opc {
                0b00 => Instruction::MovN { sf, rd, imm16, hw },
                0b10 => Instruction::MovZ { sf, rd, imm16, hw },
                0b11 => Instruction::MovK { sf, rd, imm16, hw },
                _ => Instruction::Unknown { raw },
            }
        }
        // Bitfield
        0b110 => {
            let sf = bit(raw, 31) != 0;
            let opc = bits(raw, 30, 29) as u8;
            let _n = bit(raw, 22);
            let immr = bits(raw, 21, 16) as u8;
            let imms = bits(raw, 15, 10) as u8;
            let rn = bits(raw, 9, 5) as u8;
            let rd = bits(raw, 4, 0) as u8;

            Instruction::BitfieldMove { sf, opc, rd, rn, immr, imms }
        }
        // Extract
        0b111 => {
            let sf = bit(raw, 31) != 0;
            let rm = bits(raw, 20, 16) as u8;
            let imms = bits(raw, 15, 10) as u8;
            let rn = bits(raw, 9, 5) as u8;
            let rd = bits(raw, 4, 0) as u8;

            Instruction::Extr { sf, rd, rn, rm, imms }
        }
        _ => Instruction::Unknown { raw },
    }
}

// ---------------------------------------------------------------------------
// Branches, exception generating, and system
// ---------------------------------------------------------------------------

fn decode_branch_system(raw: u32) -> Instruction {
    // Unconditional branch (immediate): B / BL
    // bits [31] = op, bits [30:26] = 00101
    if bits(raw, 30, 26) == 0b00101 {
        let op = bit(raw, 31);
        let imm26 = bits(raw, 25, 0);
        let offset = sign_extend(imm26, 26) << 2;
        return if op == 0 {
            Instruction::B { imm: offset }
        } else {
            Instruction::Bl { imm: offset }
        };
    }

    // Compare & branch (CBZ/CBNZ): bits [30:25] = 011010
    if bits(raw, 30, 25) == 0b011010 {
        let sf = bit(raw, 31) != 0;
        let op = bit(raw, 24);
        let imm19 = bits(raw, 23, 5);
        let rt = bits(raw, 4, 0) as u8;
        let offset = sign_extend(imm19, 19) << 2;
        return if op == 0 {
            Instruction::Cbz { sf, rt, imm: offset }
        } else {
            Instruction::Cbnz { sf, rt, imm: offset }
        };
    }

    // Test & branch (TBZ/TBNZ): bits [30:25] = 011011
    if bits(raw, 30, 25) == 0b011011 {
        let b5 = bit(raw, 31);
        let op = bit(raw, 24);
        let b40 = bits(raw, 23, 19);
        let imm14 = bits(raw, 18, 5);
        let rt = bits(raw, 4, 0) as u8;
        let bit_pos = ((b5 << 5) | b40) as u8;
        let offset = sign_extend(imm14, 14) << 2;
        return if op == 0 {
            Instruction::Tbz { rt, bit: bit_pos, imm: offset }
        } else {
            Instruction::Tbnz { rt, bit: bit_pos, imm: offset }
        };
    }

    // Conditional branch: B.cond: bits [31:25] = 0101010, bit [4] = 0
    if bits(raw, 31, 25) == 0b0101010 && bit(raw, 4) == 0 {
        let imm19 = bits(raw, 23, 5);
        let cond = bits(raw, 3, 0) as u8;
        let offset = sign_extend(imm19, 19) << 2;
        return Instruction::BCond { cond, imm: offset };
    }

    // Exception generation (SVC): bits [31:21] = 11010100_000, bits [4:2] = 000, bits [1:0] = 01
    if bits(raw, 31, 21) == 0b11010100_000 && bits(raw, 4, 0) == 0b00001 {
        let imm16 = bits(raw, 20, 5) as u16;
        return Instruction::Svc { imm: imm16 };
    }

    // Unconditional branch (register): bits [31:25] = 1101011
    if bits(raw, 31, 25) == 0b1101011 {
        let opc = bits(raw, 24, 21);
        let rn = bits(raw, 9, 5) as u8;
        return match opc {
            0b0000 => Instruction::Br { rn },
            0b0001 => Instruction::Blr { rn },
            0b0010 => Instruction::Ret { rn },
            _ => Instruction::Unknown { raw },
        };
    }

    // System instructions: MSR/MRS/NOP/barriers
    // bits [31:22] = 1101010100
    if bits(raw, 31, 22) == 0b1101010100 {
        return decode_system(raw);
    }

    Instruction::Unknown { raw }
}

fn decode_system(raw: u32) -> Instruction {
    let l = bit(raw, 21); // 0=MSR, 1=MRS
    let op0 = bits(raw, 20, 19);
    let op1 = bits(raw, 18, 16);
    let crn = bits(raw, 15, 12);
    let crm = bits(raw, 11, 8);
    let op2 = bits(raw, 7, 5);
    let rt = bits(raw, 4, 0) as u8;

    // NOP / CLREX / DMB / DSB / ISB: op0=0, l=0
    if op0 == 0 && l == 0 {
        // Hints: CRn=0010, op1=011
        if crn == 0b0010 && op1 == 0b011 {
            // CRm:op2 determines the hint
            if crm == 0 && op2 == 0 {
                return Instruction::Nop;
            }
            return Instruction::Nop; // Treat all hints as NOP
        }
        // Barriers: CRn=0011
        if crn == 0b0011 {
            return match op2 {
                0b010 => Instruction::Clrex,
                0b011 => Instruction::Dsb { option: crm as u8 },
                0b100 => Instruction::Dmb { option: crm as u8 },
                0b110 => Instruction::Isb,
                _ => Instruction::Nop,
            };
        }
    }

    // MSR (immediate): op0=0, l=0 - handled above for barriers/hints

    // MRS/MSR (register): op0 = 1x
    if op0 >= 2 {
        // Encode system register as a single u32
        let sys_reg = (op0 << 14) | (op1 << 11) | (crn << 7) | (crm << 3) | op2;
        return if l == 1 {
            Instruction::Mrs { rt, sys_reg }
        } else {
            Instruction::Msr { rt, sys_reg }
        };
    }

    Instruction::Unknown { raw }
}

// ---------------------------------------------------------------------------
// Load/Store
// ---------------------------------------------------------------------------

fn decode_load_store(raw: u32) -> Instruction {
    let _op0 = bits(raw, 31, 28);
    let _op1 = bit(raw, 26);
    let _op2 = bits(raw, 24, 23);
    let _op3 = bits(raw, 21, 16);
    let _op4 = bits(raw, 11, 10);

    // SIMD load/store multiple structures: bit[31]=0, bits[29:23]=0011000 or 0011001
    // Note: bit[26]=1 for this group (part of the 0011000 pattern)
    if bit(raw, 31) == 0 && (bits(raw, 29, 23) == 0b0011000 || bits(raw, 29, 23) == 0b0011001)
    {
        return decode_simd_ldst_multi(raw);
    }

    // SIMD load/store single structure: bit[31]=0, bits[29:23]=0011010 or 0011011
    if bit(raw, 31) == 0 && (bits(raw, 29, 23) == 0b0011010 || bits(raw, 29, 23) == 0b0011011)
    {
        return decode_simd_ldst_single(raw);
    }

    // Load/store pair (LDP/STP/LDP SIMD/STP SIMD)
    // bits [29:27] = 101
    if bits(raw, 29, 27) == 0b101 {
        return decode_ldst_pair(raw);
    }

    // Load register (literal) - LDR (literal)
    // bits [29:27] = 011, bit [26] = 0 for GP, 1 for SIMD
    if bits(raw, 29, 27) == 0b011 {
        return decode_ldr_literal(raw);
    }

    // Load/store exclusive (LDXR/STXR/LDAXR/STLXR/LDAR/STLR)
    // bits [29:27] = 001
    if bits(raw, 29, 27) == 0b001 && bit(raw, 26) == 0 {
        return decode_ldst_exclusive(raw);
    }

    // Load/store register (unsigned immediate, pre/post-index, register offset)
    // bits [29:27] = 111 or 110
    if bits(raw, 29, 27) == 0b111 || bits(raw, 29, 27) == 0b110 {
        let is_simd = bit(raw, 26) != 0;
        if is_simd {
            return decode_ldst_simd(raw);
        }
        return decode_ldst_gp(raw);
    }

    // Also catch some patterns with bits[29:27] = 110
    if bits(raw, 29, 27) == 0b100 {
        let is_simd = bit(raw, 26) != 0;
        if is_simd {
            return decode_ldst_simd(raw);
        }
        return decode_ldst_gp(raw);
    }

    // Fallback: try GP load/store decoding for remaining patterns
    if bit(raw, 26) == 0 {
        decode_ldst_gp(raw)
    } else {
        decode_ldst_simd(raw)
    }
}

fn decode_ldr_literal(raw: u32) -> Instruction {
    let opc = bits(raw, 31, 30);
    let v = bit(raw, 26);
    let imm19 = bits(raw, 23, 5);
    let rt = bits(raw, 4, 0) as u8;
    let offset = sign_extend(imm19, 19) << 2;

    if v == 1 {
        // SIMD literal load
        let size = match opc {
            0b00 => 2, // 32-bit (S register)
            0b01 => 3, // 64-bit (D register)
            0b10 => 4, // 128-bit (Q register)
            _ => return Instruction::Unknown { raw },
        };
        return Instruction::LdrSimd {
            rt, rn: 0xFF, // sentinel: use PC
            imm: offset, size, mode: AddrMode::Offset,
        };
    }

    match opc {
        0b00 => Instruction::LdrLit {
            sf: false, rt, imm: offset, sign_extend: false,
        },
        0b01 => Instruction::LdrLit {
            sf: true, rt, imm: offset, sign_extend: false,
        },
        0b10 => Instruction::LdrLit {
            sf: true, rt, imm: offset, sign_extend: true,
        },
        _ => Instruction::Unknown { raw },
    }
}

fn decode_ldst_exclusive(raw: u32) -> Instruction {
    let size = bits(raw, 31, 30) as u8;
    let l = bit(raw, 22);
    let o0 = bit(raw, 15);
    let rs = bits(raw, 20, 16) as u8;
    let rt2 = bits(raw, 14, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let o1 = bit(raw, 21);
    let sf = size >= 2;

    // CAS/CASA/CASL/CASAL: bit[23]=1, o1(bit[21])=1, Rt2=11111
    // Must check before the exclusive match below, since CAS overlaps with
    // STLR/LDAR/LDAXP encodings when bit[23] is not examined.
    if bit(raw, 23) == 1 && o1 == 1 && rt2 == 0b11111 {
        return Instruction::Cas { size, rs, rt, rn };
    }

    // Ordered/exclusive combinations
    match (o1, l, o0) {
        // STXR
        (0, 0, 0) => Instruction::Stxr { sf, rt, rn, rs, size },
        // STLXR
        (0, 0, 1) => Instruction::Stlxr { sf, rt, rn, rs, size },
        // LDXR
        (0, 1, 0) => Instruction::Ldxr { sf, rt, rn, size },
        // LDAXR
        (0, 1, 1) => Instruction::Ldaxr { sf, rt, rn, size },
        // STLR
        (1, 0, 0) => Instruction::Stlr { sf, rt, rn, size },
        // STLXP - handled via specific encoding below
        // LDAR
        (1, 1, 0) => Instruction::Ldar { sf, rt, rn, size },
        // LDAXP
        (1, 1, 1) => Instruction::Ldaxp { sf, rt, rt2, rn },
        _ => Instruction::Unknown { raw },
    }
}

/// Decode atomic memory operations: SWP, LDADD, LDCLR, LDEOR, LDSET.
/// Encoding: size 111000 A R 1 Rs o3 opc[14:12] 00 Rn Rt
fn decode_atomic_mem(raw: u32) -> Instruction {
    let size = bits(raw, 31, 30) as u8;
    let rs = bits(raw, 20, 16) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let o3 = bit(raw, 15);
    let opc = bits(raw, 14, 12) as u8;

    if o3 == 1 {
        // SWP family: o3=1, opc=000
        if opc == 0b000 {
            return Instruction::Swp { size, rs, rt, rn };
        }
        return Instruction::Unknown { raw };
    }

    // Atomic arithmetic: o3=0
    match opc {
        0b000 => Instruction::AtomicOp { size, rs, rt, rn, op: 0 }, // LDADD
        0b001 => Instruction::AtomicOp { size, rs, rt, rn, op: 1 }, // LDCLR
        0b010 => Instruction::AtomicOp { size, rs, rt, rn, op: 2 }, // LDEOR
        0b011 => Instruction::AtomicOp { size, rs, rt, rn, op: 3 }, // LDSET
        _ => Instruction::Unknown { raw },
    }
}

fn decode_ldst_pair(raw: u32) -> Instruction {
    let opc = bits(raw, 31, 30);
    let v = bit(raw, 26);
    let mode_bits = bits(raw, 24, 23);
    let l = bit(raw, 22);
    let imm7 = bits(raw, 21, 15);
    let rt2 = bits(raw, 14, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;

    let mode = match mode_bits {
        0b01 => AddrMode::PostIndex,
        0b10 => AddrMode::Offset,
        0b11 => AddrMode::PreIndex,
        _ => return Instruction::Unknown { raw },
    };

    if v == 1 {
        // SIMD pair
        let size = match opc {
            0b00 => 2, // 32-bit
            0b01 => 3, // 64-bit
            0b10 => 4, // 128-bit
            _ => return Instruction::Unknown { raw },
        };
        let scale = match opc {
            0b00 => 2, // 4 bytes
            0b01 => 3, // 8 bytes
            0b10 => 4, // 16 bytes
            _ => unreachable!(),
        };
        let offset = sign_extend(imm7, 7) << scale;
        return if l == 1 {
            Instruction::LdpSimd { rt, rt2, rn, imm: offset, size, mode }
        } else {
            Instruction::StpSimd { rt, rt2, rn, imm: offset, size, mode }
        };
    }

    let _sf = opc & 1 != 0 || opc >= 2;
    let scale = if opc == 0b00 { 2u32 } else { 3u32 }; // 4 or 8 bytes
    let offset = sign_extend(imm7, 7) << scale;

    if l == 1 {
        Instruction::Ldp {
            sf: opc != 0b00, rt, rt2, rn, imm: offset, mode,
        }
    } else {
        Instruction::Stp {
            sf: opc != 0b00, rt, rt2, rn, imm: offset, mode,
        }
    }
}

fn decode_ldst_gp(raw: u32) -> Instruction {
    let size = bits(raw, 31, 30) as u8;
    let opc = bits(raw, 23, 22);
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;

    // Unsigned immediate: bit [24] = 1, bits [29:28] = 11
    if bit(raw, 24) == 1 && bits(raw, 29, 28) == 0b11 {
        let imm12 = bits(raw, 21, 10);
        let scale = size as u32;
        let offset = (imm12 as i64) << scale;
        let sf = size >= 2;
        let sign_ext = opc >= 2;

        return if opc & 1 == 0 && opc < 2 {
            // STR
            Instruction::StrImm { sf, rt, rn, imm: offset, size, mode: AddrMode::Offset }
        } else {
            // LDR / LDRS*
            Instruction::LdrImm {
                sf: if sign_ext && opc == 2 { size == 3 } else { size >= 2 },
                rt, rn, imm: offset, size, mode: AddrMode::Offset, sign_extend: sign_ext,
            }
        };
    }

    // Pre/Post-index or unscaled: bit [24] = 0, bits [29:28] = 11
    if bit(raw, 24) == 0 && bits(raw, 29, 28) == 0b11 {
        let imm9 = bits(raw, 20, 12);
        let offset = sign_extend(imm9, 9);
        let idx = bits(raw, 11, 10);

        // Atomic memory operations: bit[21]=1, idx=0b00
        // Encoding: size 111000 A R 1 Rs o3 opc 00 Rn Rt
        // bit[21]=1 distinguishes atomics from LDUR/STUR (bit[21]=0).
        if idx == 0b00 && bit(raw, 21) == 1 {
            return decode_atomic_mem(raw);
        }

        // Register offset: idx = 10, bit [21] = 1
        if idx == 0b10 && bit(raw, 21) == 1 {
            let rm = bits(raw, 20, 16) as u8;
            let option = bits(raw, 15, 13);
            let s = bit(raw, 12);
            let ext = ExtendType::from_u32(option);
            let sign_ext = opc >= 2;

            return if opc == 0 {
                Instruction::StrReg {
                    sf: size >= 2, rt, rn, rm, size, extend: ext, shift: s != 0,
                }
            } else {
                Instruction::LdrReg {
                    sf: if sign_ext { size == 3 } else { size >= 2 },
                    rt, rn, rm, size, extend: ext, shift: s != 0,
                    sign_extend: sign_ext,
                }
            };
        }

        let mode = match idx {
            0b01 => AddrMode::PostIndex,
            0b11 => AddrMode::PreIndex,
            0b00 => AddrMode::Offset, // LDUR/STUR (unscaled)
            _ => return Instruction::Unknown { raw },
        };

        let sign_ext = opc >= 2;
        return if opc == 0 {
            Instruction::StrImm { sf: size >= 2, rt, rn, imm: offset, size, mode }
        } else {
            Instruction::LdrImm {
                sf: if sign_ext && opc == 2 { size == 3 } else { size >= 2 },
                rt, rn, imm: offset, size, mode, sign_extend: sign_ext,
            }
        };
    }

    Instruction::Unknown { raw }
}

fn decode_ldst_simd(raw: u32) -> Instruction {
    let opc = bits(raw, 23, 22);
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    let size_bits = bits(raw, 31, 30);

    // Determine access size from size:opc
    let size = match (size_bits, opc) {
        (0b00, 0b00 | 0b01) => 2, // 32-bit (S)
        (0b01, 0b00 | 0b01) => 3, // 64-bit (D)
        (0b00, 0b10 | 0b11) => 4, // 128-bit (Q)
        _ => size_bits as u8,
    };

    // Unsigned immediate
    if bit(raw, 24) == 1 && bits(raw, 29, 28) == 0b11 {
        let imm12 = bits(raw, 21, 10);
        let scale = size as u32;
        let offset = (imm12 as i64) << scale;

        return if opc & 1 == 0 {
            Instruction::StrSimd { rt, rn, imm: offset, size, mode: AddrMode::Offset }
        } else {
            Instruction::LdrSimd { rt, rn, imm: offset, size, mode: AddrMode::Offset }
        };
    }

    // Pre/post-index or unscaled
    if bit(raw, 24) == 0 && bits(raw, 29, 28) == 0b11 {
        let imm9 = bits(raw, 20, 12);
        let offset = sign_extend(imm9, 9);
        let idx = bits(raw, 11, 10);

        let mode = match idx {
            0b01 => AddrMode::PostIndex,
            0b11 => AddrMode::PreIndex,
            0b00 => AddrMode::Offset,
            _ => return Instruction::Unknown { raw },
        };

        return if opc & 1 == 0 {
            Instruction::StrSimd { rt, rn, imm: offset, size, mode }
        } else {
            Instruction::LdrSimd { rt, rn, imm: offset, size, mode }
        };
    }

    Instruction::Unknown { raw }
}

// ---------------------------------------------------------------------------
// Data processing (register)
// ---------------------------------------------------------------------------

fn decode_dp_reg(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let _op0 = bit(raw, 30);
    let op1 = bit(raw, 28);
    let op2 = bits(raw, 24, 21);

    // op1 = 1: Data-processing (3-source), multiply
    if op1 == 1 {
        return decode_dp3_source(raw);
    }

    // op1 = 0: Various
    match op2 {
        // Add/subtract (shifted register): op2 = 0b1xx0 or 0bx000..0b0110
        _ if bits(raw, 24, 21) & 0b1001 == 0b0000
            && bit(raw, 24) == 0 =>
        {
            return decode_add_sub_shifted(raw);
        }
        _ => {}
    }

    // Add/subtract (extended register): bits [24:21] = 0b1001 area
    // Format: sf op S 01011 opt(2) 1 Rm option(3) imm3(3) Rn Rd
    if bits(raw, 28, 24) == 0b01011 && bit(raw, 21) == 1 {
        return decode_add_sub_extended(raw);
    }

    // Logical (shifted register): bits [28:24] = 01010
    if bits(raw, 28, 24) == 0b01010 {
        return decode_logical_shifted(raw);
    }

    // Data processing (2-source): bits [28:21] = 11010110
    if bits(raw, 28, 21) == 0b11010110 {
        return decode_dp2_source(raw);
    }

    // Data processing (1-source): bits [30:21] = 1101010110
    if bits(raw, 30, 21) == 0b1101010110 {
        return decode_dp1_source(raw);
    }

    // Conditional select: bits [28:21] = 11010100
    if bits(raw, 28, 21) == 0b11010100 {
        let rm = bits(raw, 20, 16) as u8;
        let cond = bits(raw, 15, 12) as u8;
        let op2 = bits(raw, 11, 10) as u8;
        let rn = bits(raw, 9, 5) as u8;
        let rd = bits(raw, 4, 0) as u8;
        return Instruction::Csel { sf, rd, rn, rm, cond, op2 };
    }

    // Conditional compare (register): bits [29:21] = 110100100
    // Conditional compare (immediate): bits [29:21] = 110100010
    if bits(raw, 29, 21) == 0b110100100 || bits(raw, 29, 21) == 0b110100010 {
        let is_imm = bit(raw, 11) != 0;
        let is_neg = bit(raw, 30) != 0;
        let rm_or_imm = bits(raw, 20, 16) as u8;
        let cond = bits(raw, 15, 12) as u8;
        let rn = bits(raw, 9, 5) as u8;
        let nzcv = bits(raw, 3, 0) as u8;
        return Instruction::Ccmp {
            sf, rn, rm_or_imm, nzcv, cond, is_imm, is_neg,
        };
    }

    Instruction::Unknown { raw }
}

fn decode_add_sub_shifted(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let op = bit(raw, 30);
    let s = bit(raw, 29);
    let shift = ShiftType::from_u32(bits(raw, 23, 22));
    let rm = bits(raw, 20, 16) as u8;
    let imm6 = bits(raw, 15, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;

    if op == 0 {
        Instruction::AddReg {
            sf, rd, rn, rm, shift, amount: imm6, set_flags: s != 0,
        }
    } else {
        Instruction::SubReg {
            sf, rd, rn, rm, shift, amount: imm6, set_flags: s != 0,
        }
    }
}

fn decode_add_sub_extended(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let op = bit(raw, 30);
    let s = bit(raw, 29);
    let rm = bits(raw, 20, 16) as u8;
    let option = bits(raw, 15, 13);
    let imm3 = bits(raw, 12, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    let ext = ExtendType::from_u32(option);

    if op == 0 {
        Instruction::AddExtReg { sf, rd, rn, rm, ext, amount: imm3, set_flags: s != 0 }
    } else {
        Instruction::SubExtReg { sf, rd, rn, rm, ext, amount: imm3, set_flags: s != 0 }
    }
}

fn decode_logical_shifted(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let opc = bits(raw, 30, 29) as u8;
    let shift = ShiftType::from_u32(bits(raw, 23, 22));
    let n = bit(raw, 21) != 0;
    let rm = bits(raw, 20, 16) as u8;
    let imm6 = bits(raw, 15, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;

    Instruction::LogicalReg {
        sf, opc, rd, rn, rm, shift, amount: imm6, invert: n,
    }
}

fn decode_dp2_source(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let opcode = bits(raw, 15, 10);
    let rm = bits(raw, 20, 16) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;

    match opcode {
        0b000010 => Instruction::Udiv { sf, rd, rn, rm },
        0b000011 => Instruction::Sdiv { sf, rd, rn, rm },
        _ => Instruction::Unknown { raw },
    }
}

fn decode_dp1_source(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let opcode = bits(raw, 15, 10);
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;

    match opcode {
        0b000000 => Instruction::Rbit { sf, rd, rn },
        0b000001 => Instruction::Rev { sf, rd, rn, opc: 1 }, // REV16
        0b000010 if !sf => Instruction::Rev { sf, rd, rn, opc: 2 }, // REV (32-bit)
        0b000010 if sf => Instruction::Rev { sf, rd, rn, opc: 2 }, // REV32 (64-bit)
        0b000011 if sf => Instruction::Rev { sf, rd, rn, opc: 3 }, // REV (64-bit)
        0b000100 => Instruction::Clz { sf, rd, rn },
        0b000101 => Instruction::Cls { sf, rd, rn },
        _ => Instruction::Unknown { raw },
    }
}

fn decode_dp3_source(raw: u32) -> Instruction {
    let sf = bit(raw, 31) != 0;
    let op31 = bits(raw, 23, 21);
    let rm = bits(raw, 20, 16) as u8;
    let o0 = bit(raw, 15);
    let ra = bits(raw, 14, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    let op54 = bits(raw, 30, 29);

    match (op54, op31, o0) {
        (0b00, 0b000, 0) => Instruction::Madd { sf, rd, rn, rm, ra },
        (0b00, 0b000, 1) => Instruction::Msub { sf, rd, rn, rm, ra },
        (0b00, 0b001, 0) if sf => Instruction::Smaddl { rd, rn, rm, ra },
        (0b00, 0b101, 0) if sf => Instruction::Umaddl { rd, rn, rm, ra },
        (0b00, 0b010, 0) if sf => Instruction::Smulh { rd, rn, rm },
        (0b00, 0b110, 0) if sf => Instruction::Umulh { rd, rn, rm },
        _ => Instruction::Unknown { raw },
    }
}

// ---------------------------------------------------------------------------
// SIMD and Floating-point
// ---------------------------------------------------------------------------

fn decode_simd_fp(raw: u32) -> Instruction {
    // 1. Check for Advanced SIMD groups FIRST
    //    These share the op0=0111/1111 space with scalar FP.

    let b31_30 = bits(raw, 31, 30);
    let b28_24 = bits(raw, 28, 24);

    // Advanced SIMD vector: bits[31]=0, bits[28:24]=01110 (Q bit is [30])
    // These are the main NEON vector instructions
    if bit(raw, 31) == 0 && b28_24 == 0b01110 {
        return decode_advsimd_01110(raw);
    }

    // Advanced SIMD: bits[31]=0, bits[28:24]=01111
    // Modified immediate, shift by immediate, or vector x indexed element
    if bit(raw, 31) == 0 && b28_24 == 0b01111 {
        return decode_advsimd_01111(raw);
    }

    // Advanced SIMD scalar: bits[31:30]=01, bits[28:24]=11110
    if b31_30 == 0b01 && b28_24 == 0b11110 {
        return decode_advsimd_scalar(raw);
    }

    // Advanced SIMD scalar x indexed element: bits[31:30]=01, bits[28:24]=11111
    if b31_30 == 0b01 && b28_24 == 0b11111 {
        return decode_advsimd_scalar_indexed(raw);
    }

    // 2. Scalar FP (existing code)

    // Floating-point data-processing (3 source): FMADD/FMSUB/FNMADD/FNMSUB
    // [31]=0, [30]=0, [29]=0, [28:24]=11111, [23:22]=ftype, [21]=o1, [20:16]=Rm
    // [15]=o0, [14:10]=Ra, [9:5]=Rn, [4:0]=Rd
    if b28_24 == 0b11111 && bit(raw, 31) == 0 && bit(raw, 30) == 0 && bit(raw, 29) == 0
    {
        let ftype = bits(raw, 23, 22) as u8;
        let o1 = bit(raw, 21);
        let rm = bits(raw, 20, 16) as u8;
        let o0 = bit(raw, 15);
        let ra = bits(raw, 14, 10) as u8;
        let rn = bits(raw, 9, 5) as u8;
        let rd = bits(raw, 4, 0) as u8;
        let op = ((o1 << 1) | o0) as u8;
        return Instruction::Fma { ftype, rd, rn, rm, ra, op };
    }

    // Floating-point data-processing (2 source)
    // 0 0 0 11110 xx 1 Rm opcode 10 Rn Rd
    if b28_24 == 0b11110 && bits(raw, 21, 21) == 1
        && bits(raw, 11, 10) == 0b10 && bit(raw, 31) == 0
    {
        let ftype = bits(raw, 23, 22) as u8;
        let rm = bits(raw, 20, 16) as u8;
        let opcode = bits(raw, 15, 12);
        let rn = bits(raw, 9, 5) as u8;
        let rd = bits(raw, 4, 0) as u8;

        let op = match opcode {
            0b0000 => Some(FpOp::Mul),
            0b0001 => Some(FpOp::Div),
            0b0010 => Some(FpOp::Add),
            0b0011 => Some(FpOp::Sub),
            0b0100 => Some(FpOp::Max),
            0b0101 => Some(FpOp::Min),
            0b0110 => Some(FpOp::MaxNum),
            0b0111 => Some(FpOp::MinNum),
            _ => None,
        };

        if let Some(op) = op {
            return Instruction::FArith { op, rd, rn, rm, ftype };
        }
    }

    // Floating-point data-processing (1 source)
    // 0 0 0 11110 xx 1 opcode 10000 Rn Rd
    if b28_24 == 0b11110 && bit(raw, 21) == 1
        && bits(raw, 14, 10) == 0b10000 && bit(raw, 31) == 0
    {
        let ftype = bits(raw, 23, 22) as u8;
        let opcode = bits(raw, 20, 15);
        let rn = bits(raw, 9, 5) as u8;
        let rd = bits(raw, 4, 0) as u8;

        return match opcode {
            0b000000 => Instruction::FMovReg { rd, rn, ftype },
            0b000001 => Instruction::Fabs { rd, rn, ftype },
            0b000010 => Instruction::Fneg { rd, rn, ftype },
            0b000011 => Instruction::Fsqrt { rd, rn, ftype },
            // FCVT: opcode[2:0] encodes dst type
            0b000100 => Instruction::Fcvt { rd, rn, src_type: ftype, dst_type: 0 },
            0b000101 => Instruction::Fcvt { rd, rn, src_type: ftype, dst_type: 1 },
            0b000111 => Instruction::Fcvt { rd, rn, src_type: ftype, dst_type: 3 },
            // FRINT*: opcode = 001xxx
            _ if (opcode >> 3) == 0b001 => {
                let mode = (opcode & 0x7) as u8;
                Instruction::Frint { ftype, rd, rn, mode }
            }
            _ => Instruction::Unknown { raw },
        };
    }

    // Floating-point compare
    // 0 0 0 11110 xx 1 Rm 00 1000 Rn opcode2
    if b28_24 == 0b11110 && bit(raw, 21) == 1
        && bits(raw, 13, 10) == 0b1000 && bits(raw, 15, 14) == 0b00
        && bit(raw, 31) == 0
    {
        let ftype = bits(raw, 23, 22) as u8;
        let rm = bits(raw, 20, 16) as u8;
        let rn = bits(raw, 9, 5) as u8;
        let opc = bits(raw, 4, 3);
        let with_zero = opc & 1 != 0;
        return Instruction::Fcmp { rn, rm, ftype, with_zero };
    }

    // Floating-point conditional select (FCSEL)
    // 0 0 0 11110 xx 1 Rm cond 11 Rn Rd
    if b28_24 == 0b11110 && bit(raw, 21) == 1
        && bits(raw, 11, 10) == 0b11 && bit(raw, 31) == 0
    {
        let ftype = bits(raw, 23, 22) as u8;
        let rm = bits(raw, 20, 16) as u8;
        let cond = bits(raw, 15, 12) as u8;
        let rn = bits(raw, 9, 5) as u8;
        let rd = bits(raw, 4, 0) as u8;
        return Instruction::Fcsel { rd, rn, rm, ftype, cond };
    }

    // Conversion between FP and integer
    // sf 0 0 11110 xx 1 rmode opcode 000000 Rn Rd
    if b28_24 == 0b11110 && bit(raw, 21) == 1
        && bits(raw, 15, 10) == 0b000000
    {
        let sf = bit(raw, 31) != 0;
        let ftype = bits(raw, 23, 22) as u8;
        let rmode = bits(raw, 20, 19);
        let opcode = bits(raw, 18, 16);
        let rn = bits(raw, 9, 5) as u8;
        let rd = bits(raw, 4, 0) as u8;

        return match (rmode, opcode) {
            (0b11, 0b000) => Instruction::FcvtzsInt { sf, rd, rn, ftype },
            (0b11, 0b001) => Instruction::FcvtzuInt { sf, rd, rn, ftype },
            (0b00, 0b010) => Instruction::ScvtfInt { sf, rd, rn, ftype },
            (0b00, 0b011) => Instruction::UcvtfInt { sf, rd, rn, ftype },
            (0b00, 0b110) => Instruction::FMovToGp { rd, rn, sf, ftype },
            (0b00, 0b111) => Instruction::FMovFromGp { rd, rn, sf, ftype },
            _ => Instruction::Unknown { raw },
        };
    }

    Instruction::Unknown { raw }
}

// ---------------------------------------------------------------------------
// Advanced SIMD decoders
// ---------------------------------------------------------------------------

/// Decode Advanced SIMD instructions with bits[28:24] = 01110.
/// Covers: three same, three different, two-reg misc, across lanes, copy, permute, extract.
fn decode_advsimd_01110(raw: u32) -> Instruction {
    let q = bit(raw, 30) != 0;
    let u = bit(raw, 29) != 0;
    let size = bits(raw, 23, 22) as u8;
    let rd = bits(raw, 4, 0) as u8;
    let rn = bits(raw, 9, 5) as u8;

    // Three same: bit[21]=1, bit[10]=1
    if bit(raw, 21) == 1 && bit(raw, 10) == 1 {
        let rm = bits(raw, 20, 16) as u8;
        let opcode = bits(raw, 15, 11) as u8;
        return Instruction::SimdThreeSame { q, u, size, opcode, rd, rn, rm };
    }

    // Three different: bit[21]=1, bits[11:10]=00
    if bit(raw, 21) == 1 && bits(raw, 11, 10) == 0b00 {
        let rm = bits(raw, 20, 16) as u8;
        let opcode = bits(raw, 15, 12) as u8;
        return Instruction::SimdThreeDiff { q, u, size, opcode, rd, rn, rm };
    }

    // Two-register misc: bits[21:17]=10000, bits[11:10]=10
    if bits(raw, 21, 17) == 0b10000 && bits(raw, 11, 10) == 0b10 {
        let opcode = bits(raw, 16, 12) as u8;
        return Instruction::SimdTwoReg { q, u, size, opcode, rd, rn };
    }

    // Across lanes: bits[21:17]=11000, bits[11:10]=10
    if bits(raw, 21, 17) == 0b11000 && bits(raw, 11, 10) == 0b10 {
        let opcode = bits(raw, 16, 12) as u8;
        return Instruction::SimdAcrossLanes { q, u, size, opcode, rd, rn };
    }

    // Extract (EXT): u=1, bits[23:21]=000, bit[15]=0, bit[10]=0
    if u && bits(raw, 23, 21) == 0b000 && bit(raw, 15) == 0 && bit(raw, 10) == 0 {
        let rm = bits(raw, 20, 16) as u8;
        let imm4 = bits(raw, 14, 11) as u8;
        return Instruction::SimdExtract { q, imm4, rd, rn, rm };
    }

    // Copy (DUP, INS, UMOV, SMOV): !u, bits[23:21] varies, bit[10]=1
    // Encoding: 0_Q_op_01110000_imm5_0_imm4_1_Rn_Rd
    if !u && bits(raw, 23, 21) == 0b000 && bit(raw, 10) == 1 {
        let op = bit(raw, 29) as u8;
        let imm5 = bits(raw, 20, 16) as u8;
        let imm4 = bits(raw, 14, 11) as u8;
        return Instruction::SimdCopy { q, op, imm5, imm4, rd, rn };
    }

    // INS (general register): u=true, bits[23:21]=000, bit[10]=1
    // 0_Q_1_01110000_imm5_0_0011_1_Rn_Rd
    if u && bits(raw, 23, 21) == 0b000 && bit(raw, 10) == 1 {
        let op = bit(raw, 29) as u8;
        let imm5 = bits(raw, 20, 16) as u8;
        let imm4 = bits(raw, 14, 11) as u8;
        return Instruction::SimdCopy { q, op, imm5, imm4, rd, rn };
    }

    // Permute (ZIP, UZP, TRN): bit[21]=0, bits[11:10]=10
    if bit(raw, 21) == 0 && bits(raw, 11, 10) == 0b10 {
        let rm = bits(raw, 20, 16) as u8;
        let opcode = bits(raw, 14, 12) as u8;
        return Instruction::SimdPermute { q, size, opcode, rd, rn, rm };
    }

    Instruction::Unknown { raw }
}

/// Decode Advanced SIMD instructions with bits[28:24] = 01111.
/// Covers: modified immediate, shift by immediate, vector x indexed element.
fn decode_advsimd_01111(raw: u32) -> Instruction {
    let immh = bits(raw, 22, 19);

    // bit[10]=1 path: shift-by-immediate OR modified immediate
    if bit(raw, 10) == 1 {
        if immh == 0 {
            // Modified immediate (MOVI/MVNI/FMOV vec imm)
            return decode_simd_mod_imm(raw);
        } else {
            // Shift by immediate (SHL, SSHR, USHR, etc.)
            return decode_simd_shift_imm(raw);
        }
    }

    // bit[10]=0: Vector x indexed element (FMUL/FMLA by element)
    decode_simd_vec_indexed(raw)
}

fn decode_simd_mod_imm(raw: u32) -> Instruction {
    let q = bit(raw, 30) != 0;
    let op = bit(raw, 29) as u8;
    let rd = bits(raw, 4, 0) as u8;
    let cmode = bits(raw, 15, 12) as u8;
    let a = bit(raw, 18);
    let b = bit(raw, 17);
    let c = bit(raw, 16);
    let d = bit(raw, 9);
    let e = bit(raw, 8);
    let f = bit(raw, 7);
    let g = bit(raw, 6);
    let h = bit(raw, 5);
    let imm8 = ((a << 7) | (b << 6) | (c << 5) | (d << 4) | (e << 3) | (f << 2) | (g << 1) | h) as u8;
    Instruction::SimdModImm { q, op, cmode, rd, imm8 }
}

fn decode_simd_shift_imm(raw: u32) -> Instruction {
    let q = bit(raw, 30) != 0;
    let u = bit(raw, 29) != 0;
    let immh = bits(raw, 22, 19) as u8;
    let immb = bits(raw, 18, 16) as u8;
    let opcode = bits(raw, 15, 11) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    Instruction::SimdShiftImm { q, u, immh, immb, opcode, rd, rn }
}

fn decode_simd_vec_indexed(raw: u32) -> Instruction {
    let q = bit(raw, 30) != 0;
    let u = bit(raw, 29) != 0;
    let size = bits(raw, 23, 22) as u8;
    let l = bit(raw, 21) as u8;
    let m = bit(raw, 20) as u8;
    let rm = bits(raw, 19, 16) as u8;
    let opcode = bits(raw, 15, 12) as u8;
    let h = bit(raw, 11) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rd = bits(raw, 4, 0) as u8;
    Instruction::SimdVecIndexed { q, u, size, opcode, rd, rn, rm, h, l, m }
}

/// Decode Advanced SIMD scalar: bits[31:30]=01, bits[28:24]=11110
fn decode_advsimd_scalar(raw: u32) -> Instruction {
    let u = bit(raw, 29) != 0;
    let size = bits(raw, 23, 22) as u8;
    let rd = bits(raw, 4, 0) as u8;
    let rn = bits(raw, 9, 5) as u8;

    // Scalar three same: bit[21]=1, bit[10]=1
    if bit(raw, 21) == 1 && bit(raw, 10) == 1 {
        let rm = bits(raw, 20, 16) as u8;
        let opcode = bits(raw, 15, 11) as u8;
        return Instruction::SimdScalarThreeSame { u, size, opcode, rd, rn, rm };
    }

    // Scalar two-reg misc: bits[21:17]=10000, bits[11:10]=10
    if bits(raw, 21, 17) == 0b10000 && bits(raw, 11, 10) == 0b10 {
        let opcode = bits(raw, 16, 12) as u8;
        return Instruction::SimdScalarTwoReg { u, size, opcode, rd, rn };
    }

    // Scalar pairwise: bits[21:17]=11000, bits[11:10]=10
    if bits(raw, 21, 17) == 0b11000 && bits(raw, 11, 10) == 0b10 {
        let opcode = bits(raw, 16, 12) as u8;
        return Instruction::SimdScalarPairwise { u, size, opcode, rd, rn };
    }

    Instruction::Unknown { raw }
}

/// Decode Advanced SIMD scalar x indexed element: bits[31:30]=01, bits[28:24]=11111
fn decode_advsimd_scalar_indexed(raw: u32) -> Instruction {
    // For now, fall back to Unknown for scalar indexed — rare in initial game code
    Instruction::Unknown { raw }
}

fn decode_simd_ldst_multi(raw: u32) -> Instruction {
    let q = bit(raw, 30) != 0;
    let load = bit(raw, 22) != 0;
    let post_index = bit(raw, 23) != 0;
    let rm = if post_index {
        Some(bits(raw, 20, 16) as u8)
    } else {
        None
    };
    let opcode = bits(raw, 15, 12) as u8;
    let size = bits(raw, 11, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    Instruction::SimdLdStMulti { q, load, opcode, size, rn, rt, rm }
}

fn decode_simd_ldst_single(raw: u32) -> Instruction {
    let q = bit(raw, 30) != 0;
    let load = bit(raw, 22) != 0;
    let post_index = bit(raw, 23) != 0;
    let rm = if post_index {
        Some(bits(raw, 20, 16) as u8)
    } else {
        None
    };
    let r = bit(raw, 21) as u8;
    let opcode = bits(raw, 15, 13) as u8;
    let s = bit(raw, 12) as u8;
    let size = bits(raw, 11, 10) as u8;
    let rn = bits(raw, 9, 5) as u8;
    let rt = bits(raw, 4, 0) as u8;
    Instruction::SimdLdStSingle { q, load, r, opcode, s, size, rn, rt, rm }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode_nop() {
        // NOP = 0xD503201F
        let inst = decode(0xD503201F);
        assert!(matches!(inst, Instruction::Nop));
    }

    #[test]
    fn test_decode_svc() {
        // SVC #0 = 0xD4000001
        let inst = decode(0xD4000001);
        assert!(matches!(inst, Instruction::Svc { imm: 0 }));

        // SVC #1 = 0xD4000021
        let inst = decode(0xD4000021);
        assert!(matches!(inst, Instruction::Svc { imm: 1 }));
    }

    #[test]
    fn test_decode_ret() {
        // RET (X30) = 0xD65F03C0
        let inst = decode(0xD65F03C0);
        assert!(matches!(inst, Instruction::Ret { rn: 30 }));
    }

    #[test]
    fn test_decode_movz() {
        // MOV X0, #42 = MOVZ X0, #42 = 0xD2800540
        let inst = decode(0xD2800540);
        match inst {
            Instruction::MovZ { sf: true, rd: 0, imm16: 42, hw: 0 } => {}
            other => panic!("Expected MOVZ, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_add_imm() {
        // ADD X1, X0, #1 = 0x91000401
        let inst = decode(0x91000401);
        match inst {
            Instruction::AddImm { sf: true, rd: 1, rn: 0, imm12: 1, shift: false, set_flags: false } => {}
            other => panic!("Expected ADD imm, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_sub_imm_with_flags() {
        // SUBS X0, X0, #1 = 0xF1000400
        let inst = decode(0xF1000400);
        match inst {
            Instruction::SubImm { sf: true, rd: 0, rn: 0, imm12: 1, shift: false, set_flags: true } => {}
            other => panic!("Expected SUBS imm, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_b() {
        // B #4 = 0x14000001
        let inst = decode(0x14000001);
        assert!(matches!(inst, Instruction::B { imm: 4 }));
    }

    #[test]
    fn test_decode_bl() {
        // BL #8 = 0x94000002
        let inst = decode(0x94000002);
        assert!(matches!(inst, Instruction::Bl { imm: 8 }));
    }

    #[test]
    fn test_decode_br() {
        // BR X16 = 0xD61F0200
        let inst = decode(0xD61F0200);
        assert!(matches!(inst, Instruction::Br { rn: 16 }));
    }

    #[test]
    fn test_decode_blr() {
        // BLR X8 = 0xD63F0100
        let inst = decode(0xD63F0100);
        assert!(matches!(inst, Instruction::Blr { rn: 8 }));
    }

    #[test]
    fn test_decode_cbz() {
        // CBZ X0, #8 = 0xB4000040
        let inst = decode(0xB4000040);
        match inst {
            Instruction::Cbz { sf: true, rt: 0, imm: 8 } => {}
            other => panic!("Expected CBZ, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_bcond() {
        // B.EQ #4 = 0x54000020
        let inst = decode(0x54000020);
        match inst {
            Instruction::BCond { cond: 0, imm: 4 } => {}
            other => panic!("Expected B.EQ, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_stp() {
        // STP X29, X30, [SP, #-16]! = 0xA9BF7BFD
        let inst = decode(0xA9BF7BFD);
        match inst {
            Instruction::Stp { sf: true, rt: 29, rt2: 30, rn: 31, imm: -16, mode: AddrMode::PreIndex } => {}
            other => panic!("Expected STP pre-index, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_ldp() {
        // LDP X29, X30, [SP], #16 = 0xA8C17BFD
        let inst = decode(0xA8C17BFD);
        match inst {
            Instruction::Ldp { sf: true, rt: 29, rt2: 30, rn: 31, imm: 16, mode: AddrMode::PostIndex } => {}
            other => panic!("Expected LDP post-index, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_adrp() {
        // ADRP X0, #0 = 0x90000000
        let inst = decode(0x90000000);
        match inst {
            Instruction::Adrp { rd: 0, imm: 0 } => {}
            other => panic!("Expected ADRP, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_madd() {
        // MUL X0, X1, X2 = MADD X0, X1, X2, XZR = 0x9B027C20
        let inst = decode(0x9B027C20);
        match inst {
            Instruction::Madd { sf: true, rd: 0, rn: 1, rm: 2, ra: 31 } => {}
            other => panic!("Expected MADD, got {:?}", other),
        }
    }

    #[test]
    fn test_bitmask_imm_all_ones_32() {
        // For 32-bit, 0xFFFFFFFF: n=0, immr=0, imms=0b011111 (but that's levels=31 for esize=32)
        // Actually all-ones is reserved, so this should return None.
        let result = decode_bitmask_imm(false, 0, 0, 0b011111);
        assert!(result.is_none());
    }

    #[test]
    fn test_bitmask_imm_alternating() {
        // 0x5555555555555555 (alternating bits, 64-bit)
        // n=0, immr=0, imms=0b111100 → esize=2, s=0, r=0: 01 replicated
        let result = decode_bitmask_imm(true, 0, 0, 0b111100);
        assert_eq!(result, Some(0x5555555555555555));
    }

    #[test]
    fn test_decode_movn() {
        // MOV X0, #-1 = MOVN X0, #0 = 0x92800000
        let inst = decode(0x92800000);
        match inst {
            Instruction::MovN { sf: true, rd: 0, imm16: 0, hw: 0 } => {}
            other => panic!("Expected MOVN, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_fmadd() {
        // FMADD D0, D1, D2, D3: 0x1F420C20
        let inst = decode(0x1F420C20);
        match inst {
            Instruction::Fma { ftype: 1, rd: 0, rn: 1, rm: 2, ra: 3, op: 0 } => {}
            other => panic!("Expected FMADD, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_fmsub() {
        // FMSUB D0, D1, D2, D3: o1=0, o0=1 → op=1
        let inst = decode(0x1F428C20);
        match inst {
            Instruction::Fma { ftype: 1, rd: 0, rn: 1, rm: 2, ra: 3, op: 1 } => {}
            other => panic!("Expected FMSUB, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_fnmadd() {
        // FNMADD D0, D1, D2, D3: o1=1, o0=0 → op=2
        let inst = decode(0x1F620C20);
        match inst {
            Instruction::Fma { ftype: 1, rd: 0, rn: 1, rm: 2, ra: 3, op: 2 } => {}
            other => panic!("Expected FNMADD, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_fnmsub() {
        // FNMSUB D0, D1, D2, D3: o1=1, o0=1 → op=3
        let inst = decode(0x1F628C20);
        match inst {
            Instruction::Fma { ftype: 1, rd: 0, rn: 1, rm: 2, ra: 3, op: 3 } => {}
            other => panic!("Expected FNMSUB, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_frintn() {
        // FRINTN D0, D1: opcode=001000, ftype=01 → 0x1E644020
        let inst = decode(0x1E644020);
        match inst {
            Instruction::Frint { ftype: 1, rd: 0, rn: 1, mode: 0 } => {}
            other => panic!("Expected FRINTN, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_frintz() {
        // FRINTZ D0, D1: opcode=001011, ftype=01 → 0x1E65C020
        let inst = decode(0x1E65C020);
        match inst {
            Instruction::Frint { ftype: 1, rd: 0, rn: 1, mode: 3 } => {}
            other => panic!("Expected FRINTZ, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_cas_32() {
        // CAS W0, W1, [X2]: 0x88A07C41
        let inst = decode(0x88A07C41);
        match inst {
            Instruction::Cas { size: 2, rs: 0, rt: 1, rn: 2 } => {}
            other => panic!("Expected CAS, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_cas_64() {
        // CAS X0, X1, [X2]: 0xC8A07C41
        let inst = decode(0xC8A07C41);
        match inst {
            Instruction::Cas { size: 3, rs: 0, rt: 1, rn: 2 } => {}
            other => panic!("Expected CAS (64-bit), got {:?}", other),
        }
    }

    #[test]
    fn test_decode_swp() {
        // SWP W0, W1, [X2]: 0xB8208041
        let inst = decode(0xB8208041);
        match inst {
            Instruction::Swp { size: 2, rs: 0, rt: 1, rn: 2 } => {}
            other => panic!("Expected SWP, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_ldadd() {
        // LDADD W0, W1, [X2]: 0xB8200041
        let inst = decode(0xB8200041);
        match inst {
            Instruction::AtomicOp { size: 2, rs: 0, rt: 1, rn: 2, op: 0 } => {}
            other => panic!("Expected LDADD, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_ldclr() {
        // LDCLR W0, W1, [X2]: 0xB8201041
        let inst = decode(0xB8201041);
        match inst {
            Instruction::AtomicOp { size: 2, rs: 0, rt: 1, rn: 2, op: 1 } => {}
            other => panic!("Expected LDCLR, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_ldeor() {
        // LDEOR W0, W1, [X2]: 0xB8202041
        let inst = decode(0xB8202041);
        match inst {
            Instruction::AtomicOp { size: 2, rs: 0, rt: 1, rn: 2, op: 2 } => {}
            other => panic!("Expected LDEOR, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_ldset() {
        // LDSET W0, W1, [X2]: 0xB8203041
        let inst = decode(0xB8203041);
        match inst {
            Instruction::AtomicOp { size: 2, rs: 0, rt: 1, rn: 2, op: 3 } => {}
            other => panic!("Expected LDSET, got {:?}", other),
        }
    }
}
