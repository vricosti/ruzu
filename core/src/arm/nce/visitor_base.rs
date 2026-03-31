// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2023 merryhime <https://mary.rs>
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/nce/visitor_base.h
//! Base visitor trait for ARM64 instruction decoding.
//!
//! Upstream uses Dynarmic's A64 decoder with a virtual visitor pattern.
//! Each method returns `bool` (true = handled, false = not handled).
//! The default implementation returns `false` for all instructions.

/// Immediate value wrapper with a const bit width.
///
/// Corresponds to upstream `Dynarmic::Imm<BitSize>`.
#[derive(Debug, Clone, Copy)]
pub struct Imm<const BITS: u32> {
    value: u32,
}

impl<const BITS: u32> Imm<BITS> {
    pub const fn new(value: u32) -> Self {
        Self {
            value: value & ((1u32 << BITS) - 1),
        }
    }

    /// Zero-extend to u32.
    pub const fn zero_extend(&self) -> u32 {
        self.value
    }

    /// Zero-extend to u64.
    pub const fn zero_extend_u64(&self) -> u64 {
        self.value as u64
    }

    /// Sign-extend to i64.
    pub fn sign_extend_i64(&self) -> i64 {
        let shift = 64 - BITS;
        ((self.value as i64) << shift) >> shift
    }

    /// Sign-extend to u64 (reinterpreted).
    pub fn sign_extend_u64(&self) -> u64 {
        self.sign_extend_i64() as u64
    }

    /// Get a specific bit.
    pub const fn bit(&self, n: u32) -> bool {
        (self.value >> n) & 1 != 0
    }
}

/// General-purpose register identifier (0-30, 31 = SP/ZR).
///
/// Corresponds to upstream `Dynarmic::A64::Reg`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Reg {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    R16,
    R17,
    R18,
    R19,
    R20,
    R21,
    R22,
    R23,
    R24,
    R25,
    R26,
    R27,
    R28,
    R29,
    R30,
    /// Stack pointer or zero register, depending on context.
    R31 = 31,
}

impl Reg {
    /// Alias for R31 when used as stack pointer.
    pub const SP: Reg = Reg::R31;

    pub const fn from_u32(v: u32) -> Self {
        // SAFETY: values 0-31 are all valid variants
        if v > 31 {
            Self::R31
        } else {
            unsafe { std::mem::transmute(v) }
        }
    }

    pub const fn index(&self) -> u32 {
        *self as u32
    }
}

/// Vector register identifier (0-31).
///
/// Corresponds to upstream `Dynarmic::A64::Vec`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Vec {
    V0 = 0,
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7,
    V8,
    V9,
    V10,
    V11,
    V12,
    V13,
    V14,
    V15,
    V16,
    V17,
    V18,
    V19,
    V20,
    V21,
    V22,
    V23,
    V24,
    V25,
    V26,
    V27,
    V28,
    V29,
    V30,
    V31,
}

impl Vec {
    pub const fn from_u32(v: u32) -> Self {
        if v > 31 {
            Self::V31
        } else {
            unsafe { std::mem::transmute(v) }
        }
    }

    pub const fn index(&self) -> u32 {
        *self as u32
    }
}

/// Condition code.
///
/// Corresponds to upstream `Dynarmic::A64::Cond`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Cond {
    Eq = 0,
    Ne,
    Cs,
    Cc,
    Mi,
    Pl,
    Vs,
    Vc,
    Hi,
    Ls,
    Ge,
    Lt,
    Gt,
    Le,
    Al,
    Nv,
}

/// Base visitor trait for ARM64 instruction decoding.
///
/// Corresponds to upstream `Core::VisitorBase`.
/// All methods return `bool` (instruction_return_type).
/// Default implementations return `false` (unhandled).
///
/// NOTE: This is a greatly simplified subset. The full upstream visitor_base.h
/// contains hundreds of virtual methods covering the entire A64 ISA.
/// Only the methods actually overridden by InterpreterVisitor and Patcher
/// are declared here. Additional methods can be added as needed.
#[allow(unused_variables)]
pub trait VisitorBase {
    fn unallocated_encoding(&mut self) -> bool {
        false
    }

    // Loads and stores - Load/Store Exclusive (ordered)
    fn stllr(&mut self, size: Imm<2>, rn: Reg, rt: Reg) -> bool {
        false
    }
    fn stlr(&mut self, size: Imm<2>, rn: Reg, rt: Reg) -> bool {
        false
    }
    fn ldlar(&mut self, size: Imm<2>, rn: Reg, rt: Reg) -> bool {
        false
    }
    fn ldar(&mut self, size: Imm<2>, rn: Reg, rt: Reg) -> bool {
        false
    }

    // Loads and stores - Load register (literal)
    fn ldr_lit_gen(&mut self, opc_0: bool, imm19: Imm<19>, rt: Reg) -> bool {
        false
    }
    fn ldr_lit_fpsimd(&mut self, opc: Imm<2>, imm19: Imm<19>, vt: Vec) -> bool {
        false
    }

    // Loads and stores - Load/Store register pair
    fn stp_ldp_gen(
        &mut self,
        opc: Imm<2>,
        not_postindex: bool,
        wback: bool,
        l: Imm<1>,
        imm7: Imm<7>,
        rt2: Reg,
        rn: Reg,
        rt: Reg,
    ) -> bool {
        false
    }
    fn stp_ldp_fpsimd(
        &mut self,
        opc: Imm<2>,
        not_postindex: bool,
        wback: bool,
        l: Imm<1>,
        imm7: Imm<7>,
        vt2: Vec,
        rn: Reg,
        vt: Vec,
    ) -> bool {
        false
    }

    // Loads and stores - Load/Store register (immediate)
    fn strx_ldrx_imm_1(
        &mut self,
        size: Imm<2>,
        opc: Imm<2>,
        imm9: Imm<9>,
        not_postindex: bool,
        rn: Reg,
        rt: Reg,
    ) -> bool {
        false
    }
    fn strx_ldrx_imm_2(
        &mut self,
        size: Imm<2>,
        opc: Imm<2>,
        imm12: Imm<12>,
        rn: Reg,
        rt: Reg,
    ) -> bool {
        false
    }
    fn sturx_ldurx(&mut self, size: Imm<2>, opc: Imm<2>, imm9: Imm<9>, rn: Reg, rt: Reg) -> bool {
        false
    }

    // Loads and stores - SIMD immediate
    fn str_imm_fpsimd_1(
        &mut self,
        size: Imm<2>,
        opc_1: Imm<1>,
        imm9: Imm<9>,
        not_postindex: bool,
        rn: Reg,
        vt: Vec,
    ) -> bool {
        false
    }
    fn str_imm_fpsimd_2(
        &mut self,
        size: Imm<2>,
        opc_1: Imm<1>,
        imm12: Imm<12>,
        rn: Reg,
        vt: Vec,
    ) -> bool {
        false
    }
    fn ldr_imm_fpsimd_1(
        &mut self,
        size: Imm<2>,
        opc_1: Imm<1>,
        imm9: Imm<9>,
        not_postindex: bool,
        rn: Reg,
        vt: Vec,
    ) -> bool {
        false
    }
    fn ldr_imm_fpsimd_2(
        &mut self,
        size: Imm<2>,
        opc_1: Imm<1>,
        imm12: Imm<12>,
        rn: Reg,
        vt: Vec,
    ) -> bool {
        false
    }
    fn stur_fpsimd(&mut self, size: Imm<2>, opc_1: Imm<1>, imm9: Imm<9>, rn: Reg, vt: Vec) -> bool {
        false
    }
    fn ldur_fpsimd(&mut self, size: Imm<2>, opc_1: Imm<1>, imm9: Imm<9>, rn: Reg, vt: Vec) -> bool {
        false
    }

    // Loads and stores - Load/Store register (register offset)
    fn strx_reg(
        &mut self,
        size: Imm<2>,
        opc_1: Imm<1>,
        rm: Reg,
        option: Imm<3>,
        s: bool,
        rn: Reg,
        rt: Reg,
    ) -> bool {
        false
    }
    fn ldrx_reg(
        &mut self,
        size: Imm<2>,
        opc_1: Imm<1>,
        rm: Reg,
        option: Imm<3>,
        s: bool,
        rn: Reg,
        rt: Reg,
    ) -> bool {
        false
    }

    // Loads and stores - SIMD register offset
    fn str_reg_fpsimd(
        &mut self,
        size: Imm<2>,
        opc_1: Imm<1>,
        rm: Reg,
        option: Imm<3>,
        s: bool,
        rn: Reg,
        vt: Vec,
    ) -> bool {
        false
    }
    fn ldr_reg_fpsimd(
        &mut self,
        size: Imm<2>,
        opc_1: Imm<1>,
        rm: Reg,
        option: Imm<3>,
        s: bool,
        rn: Reg,
        vt: Vec,
    ) -> bool {
        false
    }
}
