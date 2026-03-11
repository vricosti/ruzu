// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-FileCopyrightText: Copyright 2023 merryhime <https://mary.rs>
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/nce/interpreter_visitor.h and interpreter_visitor.cpp
//! Single-instruction interpreter for handling alignment faults in NCE mode.
//!
//! NOTE: This is a structural port. The upstream implementation uses Dynarmic's
//! A64 instruction decoder to dispatch to the visitor methods. Without the
//! Dynarmic decoder available in Rust, the `match_and_execute_one_instruction`
//! function is stubbed. The individual visitor method implementations are
//! faithfully ported for when a decoder becomes available.

use super::guest_context::U128;
use super::visitor_base::{Imm, Reg, Vec, VisitorBase};

// ---------------------------------------------------------------------------
// Sign-extension helpers
// ---------------------------------------------------------------------------

fn sign_extend_to_long_8(value: u64) -> u64 {
    let mask = 1u64 << 7;
    let value = value & 0xFF;
    (value ^ mask).wrapping_sub(mask)
}

fn sign_extend_to_long_16(value: u64) -> u64 {
    let mask = 1u64 << 15;
    let value = value & 0xFFFF;
    (value ^ mask).wrapping_sub(mask)
}

fn sign_extend_to_long_32(value: u64) -> u64 {
    let mask = 1u64 << 31;
    let value = value & 0xFFFF_FFFF;
    (value ^ mask).wrapping_sub(mask)
}

fn sign_extend_to_long(value: u64, bitsize: u64) -> u64 {
    match bitsize {
        8 => sign_extend_to_long_8(value),
        16 => sign_extend_to_long_16(value),
        32 => sign_extend_to_long_32(value),
        _ => value,
    }
}

fn sign_extend_to_word_8(value: u32) -> u32 {
    let mask = 1u32 << 7;
    let value = value & 0xFF;
    (value ^ mask).wrapping_sub(mask)
}

fn sign_extend_to_word_16(value: u32) -> u32 {
    let mask = 1u32 << 15;
    let value = value & 0xFFFF;
    (value ^ mask).wrapping_sub(mask)
}

fn sign_extend_to_word(value: u32, bitsize: u64) -> u32 {
    match bitsize {
        8 => sign_extend_to_word_8(value),
        16 => sign_extend_to_word_16(value),
        _ => value,
    }
}

fn sign_extend(value: u64, bitsize: u64, regsize: u64) -> u64 {
    if regsize == 64 {
        sign_extend_to_long(value, bitsize)
    } else {
        sign_extend_to_word(value as u32, bitsize) as u64
    }
}

fn vector_get_element(value: U128, bitsize: u64) -> U128 {
    match bitsize {
        8 => U128 { lo: value.lo & 0xFF, hi: 0 },
        16 => U128 { lo: value.lo & 0xFFFF, hi: 0 },
        32 => U128 { lo: value.lo & 0xFFFF_FFFF, hi: 0 },
        64 => U128 { lo: value.lo, hi: 0 },
        _ => value,
    }
}

// ---------------------------------------------------------------------------
// Memory trait (placeholder for Core::Memory::Memory)
// ---------------------------------------------------------------------------

/// Trait abstracting memory read/write operations.
///
/// Corresponds to upstream `Core::Memory::Memory` interface used by the interpreter.
pub trait MemoryAccess {
    fn read_block(&self, address: u64, data: &mut [u8]);
    fn write_block(&mut self, address: u64, data: &[u8]);
    fn read32(&self, address: u64) -> u32;
}

// ---------------------------------------------------------------------------
// MemOp enum
// ---------------------------------------------------------------------------

/// Memory operation type.
///
/// Corresponds to upstream `InterpreterVisitor::MemOp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemOp {
    Load,
    Store,
    Prefetch,
}

// ---------------------------------------------------------------------------
// InterpreterVisitor
// ---------------------------------------------------------------------------

/// Single-instruction interpreter visitor.
///
/// Corresponds to upstream `Core::InterpreterVisitor`.
///
/// NOTE: The upstream uses raw pointers to register arrays from the signal
/// handler context (mcontext_t). In Rust, we use mutable slices.
pub struct InterpreterVisitor<'a, M: MemoryAccess> {
    pub memory: &'a mut M,
    pub regs: &'a mut [u64; 31],
    pub fpsimd_regs: &'a mut [U128; 32],
    pub sp: &'a mut u64,
    pub pc: &'a u64,
}

impl<'a, M: MemoryAccess> InterpreterVisitor<'a, M> {
    pub fn get_vec(&self, v: Vec) -> U128 {
        self.fpsimd_regs[v.index() as usize]
    }

    pub fn get_reg(&self, r: Reg) -> u64 {
        self.regs[r.index() as usize]
    }

    pub fn get_sp(&self) -> u64 {
        *self.sp
    }

    pub fn get_pc(&self) -> u64 {
        *self.pc
    }

    pub fn set_vec(&mut self, v: Vec, value: U128) {
        self.fpsimd_regs[v.index() as usize] = value;
    }

    pub fn set_reg(&mut self, r: Reg, value: u64) {
        self.regs[r.index() as usize] = value;
    }

    pub fn set_sp(&mut self, value: u64) {
        *self.sp = value;
    }

    /// Extend a register value according to the given option and shift.
    ///
    /// Corresponds to upstream `InterpreterVisitor::ExtendReg`.
    pub fn extend_reg(&self, bitsize: usize, reg: Reg, option: Imm<3>, shift: u8) -> u64 {
        assert!(shift <= 4);
        assert!(bitsize == 32 || bitsize == 64);

        let mut val = self.get_reg(reg);
        let len: usize;
        let signed_extend: bool;

        match option.zero_extend() {
            0b000 => { val &= 0xFF; len = 8; signed_extend = false; }           // UXTB
            0b001 => { val &= 0xFFFF; len = 16; signed_extend = false; }        // UXTH
            0b010 => { val &= 0xFFFF_FFFF; len = 32; signed_extend = false; }   // UXTW
            0b011 => { len = 64; signed_extend = false; }                        // UXTX
            0b100 => { val &= 0xFF; len = 8; signed_extend = true; }            // SXTB
            0b101 => { val &= 0xFFFF; len = 16; signed_extend = true; }         // SXTH
            0b110 => { val &= 0xFFFF_FFFF; len = 32; signed_extend = true; }    // SXTW
            0b111 => { len = 64; signed_extend = true; }                         // SXTX
            _ => unreachable!(),
        }

        let extended = if len < bitsize && signed_extend {
            sign_extend(val, len as u64, bitsize as u64)
        } else {
            val
        };

        extended << shift
    }

    /// Handle ordered load/store instructions.
    ///
    /// Corresponds to upstream `InterpreterVisitor::Ordered`.
    pub fn ordered(&mut self, size: usize, l: bool, _o0: bool, rn: Reg, rt: Reg) -> bool {
        let memop = if l { MemOp::Load } else { MemOp::Store };
        let elsize = 8 << size;
        let datasize = elsize;
        let dbytes = datasize / 8;

        let address = if rn == Reg::SP {
            self.get_sp()
        } else {
            self.get_reg(rn)
        };

        match memop {
            MemOp::Store => {
                std::sync::atomic::fence(std::sync::atomic::Ordering::SeqCst);
                let value = self.get_reg(rt);
                let bytes = value.to_le_bytes();
                self.memory.write_block(address, &bytes[..dbytes]);
                std::sync::atomic::fence(std::sync::atomic::Ordering::SeqCst);
            }
            MemOp::Load => {
                let mut buf = [0u8; 8];
                self.memory.read_block(address, &mut buf[..dbytes]);
                let value = u64::from_le_bytes(buf);
                self.set_reg(rt, value);
                std::sync::atomic::fence(std::sync::atomic::Ordering::SeqCst);
            }
            MemOp::Prefetch => unreachable!(),
        }

        true
    }

    /// Handle register-immediate load/store.
    ///
    /// Corresponds to upstream `InterpreterVisitor::RegisterImmediate`.
    pub fn register_immediate(
        &mut self, wback: bool, postindex: bool, scale: usize, offset: u64,
        size: Imm<2>, opc: Imm<2>, rn: Reg, rt: Reg,
    ) -> bool {
        let memop: MemOp;
        let mut signed = false;
        let regsize: u64;

        if !opc.bit(1) {
            memop = if opc.bit(0) { MemOp::Load } else { MemOp::Store };
            regsize = if size.zero_extend() == 0b11 { 64 } else { 32 };
        } else if size.zero_extend() == 0b11 {
            memop = MemOp::Prefetch;
            assert!(!opc.bit(0));
            regsize = 64; // unused
        } else {
            memop = MemOp::Load;
            assert!(!(size.zero_extend() == 0b10 && opc.bit(0)));
            regsize = if opc.bit(0) { 32 } else { 64 };
            signed = true;
        }

        if memop == MemOp::Load && wback && rn == rt && rn != Reg::R31 {
            return false;
        }
        if memop == MemOp::Store && wback && rn == rt && rn != Reg::R31 {
            return false;
        }

        let mut address = if rn == Reg::SP { self.get_sp() } else { self.get_reg(rn) };
        if !postindex {
            address = address.wrapping_add(offset);
        }

        let datasize = 8u64 << scale;
        let dbytes = (datasize / 8) as usize;

        match memop {
            MemOp::Store => {
                let data = self.get_reg(rt);
                let bytes = data.to_le_bytes();
                self.memory.write_block(address, &bytes[..dbytes]);
            }
            MemOp::Load => {
                let mut buf = [0u8; 8];
                self.memory.read_block(address, &mut buf[..dbytes]);
                let data = u64::from_le_bytes(buf);
                if signed {
                    self.set_reg(rt, sign_extend(data, datasize, regsize));
                } else {
                    self.set_reg(rt, data);
                }
            }
            MemOp::Prefetch => {}
        }

        if wback {
            let final_addr = if postindex { address.wrapping_add(offset) } else { address };
            if rn == Reg::SP { self.set_sp(final_addr); } else { self.set_reg(rn, final_addr); }
        }

        true
    }

    /// Handle SIMD immediate load/store.
    ///
    /// Corresponds to upstream `InterpreterVisitor::SIMDImmediate`.
    pub fn simd_immediate(
        &mut self, wback: bool, postindex: bool, scale: usize, offset: u64,
        memop: MemOp, rn: Reg, vt: Vec,
    ) -> bool {
        let datasize = 8u64 << scale;
        let dbytes = (datasize / 8) as usize;

        let mut address = if rn == Reg::SP { self.get_sp() } else { self.get_reg(rn) };
        if !postindex {
            address = address.wrapping_add(offset);
        }

        match memop {
            MemOp::Store => {
                let data = vector_get_element(self.get_vec(vt), datasize);
                let mut buf = [0u8; 16];
                buf[..8].copy_from_slice(&data.lo.to_le_bytes());
                buf[8..16].copy_from_slice(&data.hi.to_le_bytes());
                self.memory.write_block(address, &buf[..dbytes]);
            }
            MemOp::Load => {
                let mut buf = [0u8; 16];
                self.memory.read_block(address, &mut buf[..dbytes]);
                let lo = u64::from_le_bytes(buf[..8].try_into().unwrap_or([0; 8]));
                let hi = if dbytes > 8 {
                    u64::from_le_bytes(buf[8..16].try_into().unwrap_or([0; 8]))
                } else {
                    0
                };
                self.set_vec(vt, U128 { lo, hi });
            }
            _ => unreachable!(),
        }

        if wback {
            let final_addr = if postindex { address.wrapping_add(offset) } else { address };
            if rn == Reg::SP { self.set_sp(final_addr); } else { self.set_reg(rn, final_addr); }
        }

        true
    }

    /// Handle register-offset load/store.
    ///
    /// Corresponds to upstream `InterpreterVisitor::RegisterOffset`.
    pub fn register_offset(
        &mut self, scale: usize, shift: u8, size: Imm<2>, opc_1: Imm<1>, opc_0: Imm<1>,
        rm: Reg, option: Imm<3>, rn: Reg, rt: Reg,
    ) -> bool {
        let memop: MemOp;
        let regsize: u64;
        let mut signed = false;

        if opc_1.zero_extend() == 0 {
            memop = if opc_0.zero_extend() == 1 { MemOp::Load } else { MemOp::Store };
            regsize = if size.zero_extend() == 0b11 { 64 } else { 32 };
        } else if size.zero_extend() == 0b11 {
            memop = MemOp::Prefetch;
            if opc_0.zero_extend() == 1 { return false; }
            regsize = 64;
        } else {
            memop = MemOp::Load;
            if size.zero_extend() == 0b10 && opc_0.zero_extend() == 1 { return false; }
            regsize = if opc_0.zero_extend() == 1 { 32 } else { 64 };
            signed = true;
        }

        let datasize = 8u64 << scale;
        let dbytes = (datasize / 8) as usize;
        let offset = self.extend_reg(64, rm, option, shift);

        let mut address = if rn == Reg::SP { self.get_sp() } else { self.get_reg(rn) };
        address = address.wrapping_add(offset);

        match memop {
            MemOp::Store => {
                let data = self.get_reg(rt);
                let bytes = data.to_le_bytes();
                self.memory.write_block(address, &bytes[..dbytes]);
            }
            MemOp::Load => {
                let mut buf = [0u8; 8];
                self.memory.read_block(address, &mut buf[..dbytes]);
                let data = u64::from_le_bytes(buf);
                if signed {
                    self.set_reg(rt, sign_extend(data, datasize, regsize));
                } else {
                    self.set_reg(rt, data);
                }
            }
            MemOp::Prefetch => {}
        }

        true
    }

    /// Handle SIMD register-offset load/store.
    ///
    /// Corresponds to upstream `InterpreterVisitor::SIMDOffset`.
    pub fn simd_offset(
        &mut self, scale: usize, shift: u8, opc_0: Imm<1>, rm: Reg, option: Imm<3>,
        rn: Reg, vt: Vec,
    ) -> bool {
        let memop = if opc_0.zero_extend() == 1 { MemOp::Load } else { MemOp::Store };
        let datasize = 8u64 << scale;
        let dbytes = (datasize / 8) as usize;

        let offset = self.extend_reg(64, rm, option, shift);
        let mut address = if rn == Reg::SP { self.get_sp() } else { self.get_reg(rn) };
        address = address.wrapping_add(offset);

        match memop {
            MemOp::Store => {
                let data = vector_get_element(self.get_vec(vt), datasize);
                let mut buf = [0u8; 16];
                buf[..8].copy_from_slice(&data.lo.to_le_bytes());
                buf[8..16].copy_from_slice(&data.hi.to_le_bytes());
                self.memory.write_block(address, &buf[..dbytes]);
            }
            MemOp::Load => {
                let mut buf = [0u8; 16];
                self.memory.read_block(address, &mut buf[..dbytes]);
                let lo = u64::from_le_bytes(buf[..8].try_into().unwrap_or([0; 8]));
                let hi = if dbytes > 8 {
                    u64::from_le_bytes(buf[8..16].try_into().unwrap_or([0; 8]))
                } else {
                    0
                };
                self.set_vec(vt, U128 { lo, hi });
            }
            _ => unreachable!(),
        }

        true
    }
}

// ---------------------------------------------------------------------------
// VisitorBase trait implementation
// ---------------------------------------------------------------------------

impl<'a, M: MemoryAccess> VisitorBase for InterpreterVisitor<'a, M> {
    fn stllr(&mut self, sz: Imm<2>, rn: Reg, rt: Reg) -> bool {
        self.ordered(sz.zero_extend() as usize, false, false, rn, rt)
    }

    fn stlr(&mut self, sz: Imm<2>, rn: Reg, rt: Reg) -> bool {
        self.ordered(sz.zero_extend() as usize, false, true, rn, rt)
    }

    fn ldlar(&mut self, sz: Imm<2>, rn: Reg, rt: Reg) -> bool {
        self.ordered(sz.zero_extend() as usize, true, false, rn, rt)
    }

    fn ldar(&mut self, sz: Imm<2>, rn: Reg, rt: Reg) -> bool {
        self.ordered(sz.zero_extend() as usize, true, true, rn, rt)
    }

    fn ldr_lit_gen(&mut self, opc_0: bool, imm19: Imm<19>, rt: Reg) -> bool {
        let size = if !opc_0 { 4 } else { 8 };
        let offset = (imm19.sign_extend_i64() << 2) as u64;
        let address = self.get_pc().wrapping_add(offset);

        let mut buf = [0u8; 8];
        self.memory.read_block(address, &mut buf[..size]);
        let data = u64::from_le_bytes(buf);
        self.set_reg(rt, data);
        true
    }

    fn ldr_lit_fpsimd(&mut self, opc: Imm<2>, imm19: Imm<19>, vt: Vec) -> bool {
        if opc.zero_extend() == 0b11 {
            return false;
        }
        let size = (4usize << opc.zero_extend()) as usize;
        let offset = (imm19.sign_extend_u64()) << 2;
        let address = self.get_pc().wrapping_add(offset);

        let mut buf = [0u8; 16];
        self.memory.read_block(address, &mut buf[..size]);
        let lo = u64::from_le_bytes(buf[..8].try_into().unwrap_or([0; 8]));
        let hi = if size > 8 {
            u64::from_le_bytes(buf[8..16].try_into().unwrap_or([0; 8]))
        } else {
            0
        };
        self.set_vec(vt, U128 { lo, hi });
        true
    }

    fn stp_ldp_gen(
        &mut self, opc: Imm<2>, not_postindex: bool, wback: bool, l: Imm<1>,
        imm7: Imm<7>, rt2: Reg, rn: Reg, rt: Reg,
    ) -> bool {
        if (l.zero_extend() == 0 && opc.bit(0)) || opc.zero_extend() == 0b11 {
            return false;
        }

        let memop = if l.zero_extend() == 1 { MemOp::Load } else { MemOp::Store };
        if memop == MemOp::Load && wback && (rt == rn || rt2 == rn) && rn != Reg::R31 {
            return false;
        }
        if memop == MemOp::Store && wback && (rt == rn || rt2 == rn) && rn != Reg::R31 {
            return false;
        }
        if memop == MemOp::Load && rt == rt2 {
            return false;
        }

        let mut address = if rn == Reg::SP { self.get_sp() } else { self.get_reg(rn) };
        let postindex = !not_postindex;
        let signed = opc.bit(0);
        let scale = 2 + if opc.bit(1) { 1 } else { 0 };
        let datasize = 8u64 << scale;
        let offset = imm7.sign_extend_u64() << scale;
        let dbytes = (datasize / 8) as usize;

        if !postindex {
            address = address.wrapping_add(offset);
        }

        match memop {
            MemOp::Store => {
                let d1 = self.get_reg(rt);
                let d2 = self.get_reg(rt2);
                let b1 = d1.to_le_bytes();
                let b2 = d2.to_le_bytes();
                self.memory.write_block(address, &b1[..dbytes]);
                self.memory.write_block(address + dbytes as u64, &b2[..dbytes]);
            }
            MemOp::Load => {
                let mut b1 = [0u8; 8];
                let mut b2 = [0u8; 8];
                self.memory.read_block(address, &mut b1[..dbytes]);
                self.memory.read_block(address + dbytes as u64, &mut b2[..dbytes]);
                let d1 = u64::from_le_bytes(b1);
                let d2 = u64::from_le_bytes(b2);
                if signed {
                    self.set_reg(rt, sign_extend(d1, datasize, 64));
                    self.set_reg(rt2, sign_extend(d2, datasize, 64));
                } else {
                    self.set_reg(rt, d1);
                    self.set_reg(rt2, d2);
                }
            }
            _ => unreachable!(),
        }

        if wback {
            let final_addr = if postindex { address.wrapping_add(offset) } else { address };
            if rn == Reg::SP { self.set_sp(final_addr); } else { self.set_reg(rn, final_addr); }
        }

        true
    }

    fn stp_ldp_fpsimd(
        &mut self, opc: Imm<2>, not_postindex: bool, wback: bool, l: Imm<1>,
        imm7: Imm<7>, vt2: Vec, rn: Reg, vt: Vec,
    ) -> bool {
        if opc.zero_extend() == 0b11 {
            return false;
        }
        let memop = if l.zero_extend() == 1 { MemOp::Load } else { MemOp::Store };
        if memop == MemOp::Load && vt == vt2 {
            return false;
        }

        let mut address = if rn == Reg::SP { self.get_sp() } else { self.get_reg(rn) };
        let postindex = !not_postindex;
        let scale = 2 + opc.zero_extend() as usize;
        let datasize = 8u64 << scale;
        let offset = imm7.sign_extend_u64() << scale;
        let dbytes = (datasize / 8) as usize;

        if !postindex {
            address = address.wrapping_add(offset);
        }

        match memop {
            MemOp::Store => {
                let d1 = vector_get_element(self.get_vec(vt), datasize);
                let d2 = vector_get_element(self.get_vec(vt2), datasize);
                let mut b1 = [0u8; 16];
                b1[..8].copy_from_slice(&d1.lo.to_le_bytes());
                b1[8..16].copy_from_slice(&d1.hi.to_le_bytes());
                let mut b2 = [0u8; 16];
                b2[..8].copy_from_slice(&d2.lo.to_le_bytes());
                b2[8..16].copy_from_slice(&d2.hi.to_le_bytes());
                self.memory.write_block(address, &b1[..dbytes]);
                self.memory.write_block(address + dbytes as u64, &b2[..dbytes]);
            }
            MemOp::Load => {
                let mut b1 = [0u8; 16];
                let mut b2 = [0u8; 16];
                self.memory.read_block(address, &mut b1[..dbytes]);
                self.memory.read_block(address + dbytes as u64, &mut b2[..dbytes]);
                let lo1 = u64::from_le_bytes(b1[..8].try_into().unwrap_or([0; 8]));
                let hi1 = if dbytes > 8 { u64::from_le_bytes(b1[8..16].try_into().unwrap_or([0; 8])) } else { 0 };
                let lo2 = u64::from_le_bytes(b2[..8].try_into().unwrap_or([0; 8]));
                let hi2 = if dbytes > 8 { u64::from_le_bytes(b2[8..16].try_into().unwrap_or([0; 8])) } else { 0 };
                self.set_vec(vt, U128 { lo: lo1, hi: hi1 });
                self.set_vec(vt2, U128 { lo: lo2, hi: hi2 });
            }
            _ => unreachable!(),
        }

        if wback {
            let final_addr = if postindex { address.wrapping_add(offset) } else { address };
            if rn == Reg::SP { self.set_sp(final_addr); } else { self.set_reg(rn, final_addr); }
        }

        true
    }

    fn strx_ldrx_imm_1(
        &mut self, size: Imm<2>, opc: Imm<2>, imm9: Imm<9>, not_postindex: bool,
        rn: Reg, rt: Reg,
    ) -> bool {
        let wback = true;
        let postindex = !not_postindex;
        let scale = size.zero_extend() as usize;
        let offset = imm9.sign_extend_u64();
        self.register_immediate(wback, postindex, scale, offset, size, opc, rn, rt)
    }

    fn strx_ldrx_imm_2(
        &mut self, size: Imm<2>, opc: Imm<2>, imm12: Imm<12>, rn: Reg, rt: Reg,
    ) -> bool {
        let scale = size.zero_extend() as usize;
        let offset = imm12.zero_extend_u64() << scale;
        self.register_immediate(false, false, scale, offset, size, opc, rn, rt)
    }

    fn sturx_ldurx(
        &mut self, size: Imm<2>, opc: Imm<2>, imm9: Imm<9>, rn: Reg, rt: Reg,
    ) -> bool {
        let scale = size.zero_extend() as usize;
        let offset = imm9.sign_extend_u64();
        self.register_immediate(false, false, scale, offset, size, opc, rn, rt)
    }

    fn str_imm_fpsimd_1(
        &mut self, size: Imm<2>, opc_1: Imm<1>, imm9: Imm<9>, not_postindex: bool,
        rn: Reg, vt: Vec,
    ) -> bool {
        let scale = ((opc_1.zero_extend() << 2) | size.zero_extend()) as usize;
        if scale > 4 { return false; }
        let offset = imm9.sign_extend_u64();
        self.simd_immediate(true, !not_postindex, scale, offset, MemOp::Store, rn, vt)
    }

    fn str_imm_fpsimd_2(
        &mut self, size: Imm<2>, opc_1: Imm<1>, imm12: Imm<12>, rn: Reg, vt: Vec,
    ) -> bool {
        let scale = ((opc_1.zero_extend() << 2) | size.zero_extend()) as usize;
        if scale > 4 { return false; }
        let offset = imm12.zero_extend_u64() << scale;
        self.simd_immediate(false, false, scale, offset, MemOp::Store, rn, vt)
    }

    fn ldr_imm_fpsimd_1(
        &mut self, size: Imm<2>, opc_1: Imm<1>, imm9: Imm<9>, not_postindex: bool,
        rn: Reg, vt: Vec,
    ) -> bool {
        let scale = ((opc_1.zero_extend() << 2) | size.zero_extend()) as usize;
        if scale > 4 { return false; }
        let offset = imm9.sign_extend_u64();
        self.simd_immediate(true, !not_postindex, scale, offset, MemOp::Load, rn, vt)
    }

    fn ldr_imm_fpsimd_2(
        &mut self, size: Imm<2>, opc_1: Imm<1>, imm12: Imm<12>, rn: Reg, vt: Vec,
    ) -> bool {
        let scale = ((opc_1.zero_extend() << 2) | size.zero_extend()) as usize;
        if scale > 4 { return false; }
        let offset = imm12.zero_extend_u64() << scale;
        self.simd_immediate(false, false, scale, offset, MemOp::Load, rn, vt)
    }

    fn stur_fpsimd(
        &mut self, size: Imm<2>, opc_1: Imm<1>, imm9: Imm<9>, rn: Reg, vt: Vec,
    ) -> bool {
        let scale = ((opc_1.zero_extend() << 2) | size.zero_extend()) as usize;
        if scale > 4 { return false; }
        let offset = imm9.sign_extend_u64();
        self.simd_immediate(false, false, scale, offset, MemOp::Store, rn, vt)
    }

    fn ldur_fpsimd(
        &mut self, size: Imm<2>, opc_1: Imm<1>, imm9: Imm<9>, rn: Reg, vt: Vec,
    ) -> bool {
        let scale = ((opc_1.zero_extend() << 2) | size.zero_extend()) as usize;
        if scale > 4 { return false; }
        let offset = imm9.sign_extend_u64();
        self.simd_immediate(false, false, scale, offset, MemOp::Load, rn, vt)
    }

    fn strx_reg(
        &mut self, size: Imm<2>, opc_1: Imm<1>, rm: Reg, option: Imm<3>, s: bool,
        rn: Reg, rt: Reg,
    ) -> bool {
        let opc_0 = Imm::<1>::new(0);
        let scale = size.zero_extend() as usize;
        let shift = if s { scale as u8 } else { 0 };
        if !option.bit(1) { return false; }
        self.register_offset(scale, shift, size, opc_1, opc_0, rm, option, rn, rt)
    }

    fn ldrx_reg(
        &mut self, size: Imm<2>, opc_1: Imm<1>, rm: Reg, option: Imm<3>, s: bool,
        rn: Reg, rt: Reg,
    ) -> bool {
        let opc_0 = Imm::<1>::new(1);
        let scale = size.zero_extend() as usize;
        let shift = if s { scale as u8 } else { 0 };
        if !option.bit(1) { return false; }
        self.register_offset(scale, shift, size, opc_1, opc_0, rm, option, rn, rt)
    }

    fn str_reg_fpsimd(
        &mut self, size: Imm<2>, opc_1: Imm<1>, rm: Reg, option: Imm<3>, s: bool,
        rn: Reg, vt: Vec,
    ) -> bool {
        let opc_0 = Imm::<1>::new(0);
        let scale = ((opc_1.zero_extend() << 2) | size.zero_extend()) as usize;
        if scale > 4 { return false; }
        let shift = if s { scale as u8 } else { 0 };
        if !option.bit(1) { return false; }
        self.simd_offset(scale, shift, opc_0, rm, option, rn, vt)
    }

    fn ldr_reg_fpsimd(
        &mut self, size: Imm<2>, opc_1: Imm<1>, rm: Reg, option: Imm<3>, s: bool,
        rn: Reg, vt: Vec,
    ) -> bool {
        let opc_0 = Imm::<1>::new(1);
        let scale = ((opc_1.zero_extend() << 2) | size.zero_extend()) as usize;
        if scale > 4 { return false; }
        let shift = if s { scale as u8 } else { 0 };
        if !option.bit(1) { return false; }
        self.simd_offset(scale, shift, opc_0, rm, option, rn, vt)
    }
}

// ---------------------------------------------------------------------------
// Top-level match-and-execute function
// ---------------------------------------------------------------------------

/// Match and execute a single instruction from a signal handler context.
///
/// Corresponds to upstream `Core::MatchAndExecuteOneInstruction`.
///
/// NOTE: This function is stubbed. The upstream uses Dynarmic's A64 instruction
/// decoder (`Dynarmic::A64::Decode<VisitorBase>`) to dispatch. Without that
/// decoder available in Rust, this returns `None` (instruction not handled).
/// When a Rust A64 decoder is available, this function should be completed.
pub fn match_and_execute_one_instruction<M: MemoryAccess>(
    _memory: &mut M,
    _regs: &mut [u64; 31],
    _fpsimd_regs: &mut [U128; 32],
    _sp: &mut u64,
    _pc: &u64,
) -> Option<u64> {
    // TODO: Implement instruction decoding and dispatch.
    // Upstream reads the instruction at pc, decodes it via Dynarmic::A64::Decode,
    // and calls the matching visitor method. If handled, returns pc + 4.
    log::warn!("MatchAndExecuteOneInstruction: not yet implemented (no A64 decoder)");
    None
}
