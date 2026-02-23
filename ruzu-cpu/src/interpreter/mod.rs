// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Pure-Rust ARM64 interpreter.
//!
//! Implements the [`CpuExecutor`] trait via a fetch-decode-execute loop.
//! Covers the ~120 most common A64 instructions needed for Switch homebrew.

pub mod alu;
pub mod branch;
pub mod crypto;
pub mod mem;
pub mod neon;
pub mod simd;
pub mod system;

use crate::decoder::Instruction;
use crate::memory::MemoryAccess;
use crate::state::{CpuExecutor, CpuState, HaltReason};
use ruzu_common::VAddr;

use std::sync::atomic::{AtomicBool, Ordering};

/// Result of executing a single instruction.
pub enum StepResult {
    /// Instruction executed normally; advance PC by 4.
    Continue,
    /// A branch was taken; PC has already been updated.
    BranchTaken,
    /// SVC instruction: return to kernel for syscall dispatch.
    Svc(u16),
    /// Breakpoint hit.
    Breakpoint,
    /// Memory fault at the given address.
    MemoryFault(u64),
    /// Unknown or unimplemented instruction.
    InvalidInstruction(u32),
    /// CPU was halted externally.
    Halted,
}

/// Pure-Rust ARM64 interpreter.
pub struct Interpreter {
    halted: AtomicBool,
    /// Instruction budget: number of instructions remaining in this time slice.
    /// 0 means unlimited (no budget).
    budget: u64,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            halted: AtomicBool::new(false),
            budget: 0,
        }
    }

    /// Set the instruction budget for the next `run` call.
    /// When the budget reaches 0, `run` returns `HaltReason::BudgetExhausted`.
    /// A budget of 0 means unlimited.
    pub fn set_budget(&mut self, n: u64) {
        self.budget = n;
    }

    /// Execute a single decoded instruction.
    fn execute(
        &self,
        state: &mut CpuState,
        mem_access: &mut dyn MemoryAccess,
        inst: Instruction,
    ) -> StepResult {
        use Instruction::*;

        match inst {
            // -- Data processing (immediate) --------------------------------
            AddImm { sf, rd, rn, imm12, shift, set_flags } => {
                alu::exec_add_imm(state, sf, rd, rn, imm12, shift, set_flags)
            }
            SubImm { sf, rd, rn, imm12, shift, set_flags } => {
                alu::exec_sub_imm(state, sf, rd, rn, imm12, shift, set_flags)
            }
            MovZ { sf, rd, imm16, hw } => alu::exec_movz(state, sf, rd, imm16, hw),
            MovK { sf, rd, imm16, hw } => alu::exec_movk(state, sf, rd, imm16, hw),
            MovN { sf, rd, imm16, hw } => alu::exec_movn(state, sf, rd, imm16, hw),
            Adr { rd, imm } => alu::exec_adr(state, rd, imm),
            Adrp { rd, imm } => alu::exec_adrp(state, rd, imm),
            LogicalImm { sf, opc, rd, rn, imm } => {
                alu::exec_logical_imm(state, sf, opc, rd, rn, imm)
            }
            BitfieldMove { sf, opc, rd, rn, immr, imms } => {
                alu::exec_bitfield_move(state, sf, opc, rd, rn, immr, imms)
            }
            Extr { sf, rd, rn, rm, imms } => {
                alu::exec_extr(state, sf, rd, rn, rm, imms)
            }

            // -- Data processing (register) ---------------------------------
            AddReg { sf, rd, rn, rm, shift, amount, set_flags } => {
                alu::exec_add_reg(state, sf, rd, rn, rm, shift, amount, set_flags)
            }
            SubReg { sf, rd, rn, rm, shift, amount, set_flags } => {
                alu::exec_sub_reg(state, sf, rd, rn, rm, shift, amount, set_flags)
            }
            LogicalReg { sf, opc, rd, rn, rm, shift, amount, invert } => {
                alu::exec_logical_reg(state, sf, opc, rd, rn, rm, shift, amount, invert)
            }
            AddExtReg { sf, rd, rn, rm, ext, amount, set_flags } => {
                alu::exec_add_ext_reg(state, sf, rd, rn, rm, ext, amount, set_flags)
            }
            SubExtReg { sf, rd, rn, rm, ext, amount, set_flags } => {
                alu::exec_sub_ext_reg(state, sf, rd, rn, rm, ext, amount, set_flags)
            }
            Madd { sf, rd, rn, rm, ra } => alu::exec_madd(state, sf, rd, rn, rm, ra),
            Msub { sf, rd, rn, rm, ra } => alu::exec_msub(state, sf, rd, rn, rm, ra),
            Smaddl { rd, rn, rm, ra } => alu::exec_smaddl(state, rd, rn, rm, ra),
            Umaddl { rd, rn, rm, ra } => alu::exec_umaddl(state, rd, rn, rm, ra),
            Smulh { rd, rn, rm } => alu::exec_smulh(state, rd, rn, rm),
            Umulh { rd, rn, rm } => alu::exec_umulh(state, rd, rn, rm),
            Sdiv { sf, rd, rn, rm } => alu::exec_sdiv(state, sf, rd, rn, rm),
            Udiv { sf, rd, rn, rm } => alu::exec_udiv(state, sf, rd, rn, rm),
            Csel { sf, rd, rn, rm, cond, op2 } => {
                alu::exec_csel(state, sf, rd, rn, rm, cond, op2)
            }
            Ccmp { sf, rn, rm_or_imm, nzcv, cond, is_imm, is_neg } => {
                alu::exec_ccmp(state, sf, rn, rm_or_imm, nzcv, cond, is_imm, is_neg)
            }
            Clz { sf, rd, rn } => alu::exec_clz(state, sf, rd, rn),
            Cls { sf, rd, rn } => alu::exec_cls(state, sf, rd, rn),
            Rev { sf, rd, rn, opc } => alu::exec_rev(state, sf, rd, rn, opc),
            Rbit { sf, rd, rn } => alu::exec_rbit(state, sf, rd, rn),
            Adc { sf, rd, rn, rm, set_flags } => alu::exec_adc(state, sf, rd, rn, rm, set_flags),
            Sbc { sf, rd, rn, rm, set_flags } => alu::exec_sbc(state, sf, rd, rn, rm, set_flags),
            VarShift { sf, rd, rn, rm, shift_type } => alu::exec_var_shift(state, sf, rd, rn, rm, shift_type),
            Smsubl { rd, rn, rm, ra } => alu::exec_smsubl(state, rd, rn, rm, ra),
            Umsubl { rd, rn, rm, ra } => alu::exec_umsubl(state, rd, rn, rm, ra),

            // -- Load/Store -------------------------------------------------
            LdrImm { sf, rt, rn, imm, size, mode, sign_extend } => {
                mem::exec_ldr_imm(state, mem_access, sf, rt, rn, imm, size, mode, sign_extend)
            }
            StrImm { sf, rt, rn, imm, size, mode } => {
                mem::exec_str_imm(state, mem_access, sf, rt, rn, imm, size, mode)
            }
            LdrReg { sf, rt, rn, rm, size, extend, shift, sign_extend } => {
                mem::exec_ldr_reg(state, mem_access, sf, rt, rn, rm, size, extend, shift, sign_extend)
            }
            StrReg { sf, rt, rn, rm, size, extend, shift } => {
                mem::exec_str_reg(state, mem_access, sf, rt, rn, rm, size, extend, shift)
            }
            LdrLit { sf, rt, imm, sign_extend } => {
                mem::exec_ldr_lit(state, mem_access, sf, rt, imm, sign_extend)
            }
            Ldp { sf, rt, rt2, rn, imm, mode } => {
                mem::exec_ldp(state, mem_access, sf, rt, rt2, rn, imm, mode)
            }
            Stp { sf, rt, rt2, rn, imm, mode } => {
                mem::exec_stp(state, mem_access, sf, rt, rt2, rn, imm, mode)
            }
            Ldxr { sf, rt, rn, size } => {
                mem::exec_ldxr(state, mem_access, sf, rt, rn, size)
            }
            Stxr { sf, rt, rn, rs, size } => {
                mem::exec_stxr(state, mem_access, sf, rt, rn, rs, size)
            }
            Ldaxr { sf, rt, rn, size } => {
                mem::exec_ldaxr(state, mem_access, sf, rt, rn, size)
            }
            Stlxr { sf, rt, rn, rs, size } => {
                mem::exec_stlxr(state, mem_access, sf, rt, rn, rs, size)
            }
            Ldar { sf, rt, rn, size } => {
                mem::exec_ldar(state, mem_access, sf, rt, rn, size)
            }
            Stlr { sf, rt, rn, size } => {
                mem::exec_stlr(state, mem_access, sf, rt, rn, size)
            }
            Ldaxp { sf, rt, rt2, rn } => {
                mem::exec_ldaxp(state, mem_access, sf, rt, rt2, rn)
            }
            Stlxp { sf, rt, rt2, rn, rs } => {
                mem::exec_stlxp(state, mem_access, sf, rt, rt2, rn, rs)
            }
            Cas { size, rs, rt, rn } => {
                mem::exec_cas(state, mem_access, size, rs, rt, rn)
            }
            Swp { size, rs, rt, rn } => {
                mem::exec_swp(state, mem_access, size, rs, rt, rn)
            }
            AtomicOp { size, rs, rt, rn, op } => {
                mem::exec_atomic_op(state, mem_access, size, rs, rt, rn, op)
            }

            // -- Branch -----------------------------------------------------
            B { imm } => branch::exec_b(state, imm),
            Bl { imm } => branch::exec_bl(state, imm),
            Br { rn } => branch::exec_br(state, rn),
            Blr { rn } => branch::exec_blr(state, rn),
            Ret { rn } => branch::exec_ret(state, rn),
            BCond { cond, imm } => branch::exec_bcond(state, cond, imm),
            Cbz { sf, rt, imm } => branch::exec_cbz(state, sf, rt, imm),
            Cbnz { sf, rt, imm } => branch::exec_cbnz(state, sf, rt, imm),
            Tbz { rt, bit, imm } => branch::exec_tbz(state, rt, bit, imm),
            Tbnz { rt, bit, imm } => branch::exec_tbnz(state, rt, bit, imm),
            Brk { imm } => branch::exec_brk(imm),

            // -- System -----------------------------------------------------
            Svc { imm } => system::exec_svc(imm),
            Mrs { rt, sys_reg } => system::exec_mrs(state, rt, sys_reg),
            Msr { rt, sys_reg } => system::exec_msr(state, rt, sys_reg),
            Nop => system::exec_nop(),
            Clrex => system::exec_clrex(state),
            Dmb { .. } => system::exec_dmb(),
            Dsb { .. } => system::exec_dsb(),
            Isb => system::exec_isb(),

            // -- SIMD/FP ----------------------------------------------------
            FMovReg { rd, rn, ftype } => simd::exec_fmov_reg(state, rd, rn, ftype),
            FMovToGp { rd, rn, sf, ftype } => {
                simd::exec_fmov_to_gp(state, rd, rn, sf, ftype)
            }
            FMovFromGp { rd, rn, sf, ftype } => {
                simd::exec_fmov_from_gp(state, rd, rn, sf, ftype)
            }
            FArith { op, rd, rn, rm, ftype } => {
                simd::exec_farith(state, op, rd, rn, rm, ftype)
            }
            Fcmp { rn, rm, ftype, with_zero } => {
                simd::exec_fcmp(state, rn, rm, ftype, with_zero)
            }
            Fcsel { rd, rn, rm, ftype, cond } => {
                simd::exec_fcsel(state, rd, rn, rm, ftype, cond)
            }
            Fcvt { rd, rn, src_type, dst_type } => {
                simd::exec_fcvt(state, rd, rn, src_type, dst_type)
            }
            ScvtfInt { sf, rd, rn, ftype } => {
                simd::exec_scvtf_int(state, sf, rd, rn, ftype)
            }
            UcvtfInt { sf, rd, rn, ftype } => {
                simd::exec_ucvtf_int(state, sf, rd, rn, ftype)
            }
            FcvtzsInt { sf, rd, rn, ftype } => {
                simd::exec_fcvtzs_int(state, sf, rd, rn, ftype)
            }
            FcvtzuInt { sf, rd, rn, ftype } => {
                simd::exec_fcvtzu_int(state, sf, rd, rn, ftype)
            }
            Fneg { rd, rn, ftype } => simd::exec_fneg(state, rd, rn, ftype),
            Fabs { rd, rn, ftype } => simd::exec_fabs(state, rd, rn, ftype),
            Fsqrt { rd, rn, ftype } => simd::exec_fsqrt(state, rd, rn, ftype),
            Fma { ftype, rd, rn, rm, ra, op } => {
                simd::exec_fma(state, ftype, rd, rn, rm, ra, op)
            }
            Frint { ftype, rd, rn, mode } => {
                simd::exec_frint(state, ftype, rd, rn, mode)
            }
            Fccmp { rn, rm, nzcv, cond, ftype } => simd::exec_fccmp(state, rn, rm, nzcv, cond, ftype),
            FmovImm { rd, ftype, imm8 } => simd::exec_fmov_imm(state, rd, ftype, imm8),
            FcvtRound { sf, rd, rn, ftype, rmode, unsigned } => simd::exec_fcvt_round(state, sf, rd, rn, ftype, rmode, unsigned),
            FpFixedConv { sf, rd, rn, ftype, fbits, opcode } => simd::exec_fp_fixed_conv(state, sf, rd, rn, ftype, fbits, opcode),
            LdrSimd { rt, rn, imm, size, mode } => {
                simd::exec_ldr_simd(state, mem_access, rt, rn, imm, size, mode, state.pc)
            }
            StrSimd { rt, rn, imm, size, mode } => {
                simd::exec_str_simd(state, mem_access, rt, rn, imm, size, mode)
            }
            LdpSimd { rt, rt2, rn, imm, size, mode } => {
                simd::exec_ldp_simd(state, mem_access, rt, rt2, rn, imm, size, mode)
            }
            StpSimd { rt, rt2, rn, imm, size, mode } => {
                simd::exec_stp_simd(state, mem_access, rt, rt2, rn, imm, size, mode)
            }

            // -- NEON/Advanced SIMD ------------------------------------------
            SimdThreeSame { q, u, size, opcode, rd, rn, rm } => {
                neon::exec_simd_three_same(state, q, u, size, opcode, rd, rn, rm)
            }
            SimdTwoReg { q, u, size, opcode, rd, rn } => {
                neon::exec_simd_two_reg(state, q, u, size, opcode, rd, rn)
            }
            SimdCopy { q, op, imm5, imm4, rd, rn } => {
                neon::exec_simd_copy(state, q, op, imm5, imm4, rd, rn)
            }
            SimdModImm { q, op, cmode, rd, imm8 } => {
                neon::exec_simd_mod_imm(state, q, op, cmode, rd, imm8)
            }
            SimdShiftImm { q, u, immh, immb, opcode, rd, rn } => {
                neon::exec_simd_shift_imm(state, q, u, immh, immb, opcode, rd, rn)
            }
            SimdPermute { q, size, opcode, rd, rn, rm } => {
                neon::exec_simd_permute(state, q, size, opcode, rd, rn, rm)
            }
            SimdExtract { q, imm4, rd, rn, rm } => {
                neon::exec_simd_extract(state, q, imm4, rd, rn, rm)
            }
            SimdAcrossLanes { q, u, size, opcode, rd, rn } => {
                neon::exec_simd_across_lanes(state, q, u, size, opcode, rd, rn)
            }
            SimdThreeDiff { q, u, size, opcode, rd, rn, rm } => {
                neon::exec_simd_three_diff(state, q, u, size, opcode, rd, rn, rm)
            }
            SimdVecIndexed { q, u, size, opcode, rd, rn, rm, h, l, m } => {
                neon::exec_simd_vec_indexed(state, q, u, size, opcode, rd, rn, rm, h, l, m)
            }
            SimdLdStMulti { q, load, opcode, size, rn, rt, rm } => {
                neon::exec_simd_ldst_multi(state, mem_access, q, load, opcode, size, rn, rt, rm)
            }
            SimdLdStSingle { q, load, r, opcode, s, size, rn, rt, rm } => {
                neon::exec_simd_ldst_single(state, mem_access, q, load, r, opcode, s, size, rn, rt, rm)
            }
            SimdScalarThreeSame { u, size, opcode, rd, rn, rm } => {
                neon::exec_simd_scalar_three_same(state, u, size, opcode, rd, rn, rm)
            }
            SimdScalarTwoReg { u, size, opcode, rd, rn } => {
                neon::exec_simd_scalar_two_reg(state, u, size, opcode, rd, rn)
            }
            SimdScalarPairwise { u, size, opcode, rd, rn } => {
                neon::exec_simd_scalar_pairwise(state, u, size, opcode, rd, rn)
            }
            SimdScalarShiftImm { u, immh, immb, opcode, rd, rn } => {
                neon::exec_simd_scalar_shift_imm(state, u, immh, immb, opcode, rd, rn)
            }
            SimdScalarIndexed { u, size, opcode, rd, rn, rm, h, l, m } => {
                neon::exec_simd_scalar_indexed(state, u, size, opcode, rd, rn, rm, h, l, m)
            }
            SimdTbl { q, rd, rn, rm, len, op } => {
                neon::exec_simd_tbl(state, q, rd, rn, rm, len, op)
            }

            // -- Crypto / CRC32 ---------------------------------------------
            Crc32 { sf, sz, c, rd, rn, rm } => {
                crypto::exec_crc32(state, sf, sz, c, rd, rn, rm)
            }
            CryptoAes { rd, rn, opcode } => {
                crypto::exec_crypto_aes(state, rd, rn, opcode)
            }
            CryptoSha3 { rd, rn, rm, opcode } => {
                crypto::exec_crypto_sha3(state, rd, rn, rm, opcode)
            }
            CryptoSha2 { rd, rn, opcode } => {
                crypto::exec_crypto_sha2(state, rd, rn, opcode)
            }

            // -- Fallback ---------------------------------------------------
            Unknown { raw } => StepResult::InvalidInstruction(raw),
        }
    }
}

impl CpuExecutor for Interpreter {
    fn run(
        &mut self,
        state: &mut CpuState,
        mem: &mut dyn MemoryAccess,
    ) -> HaltReason {
        self.halted.store(false, Ordering::Relaxed);

        loop {
            if self.halted.load(Ordering::Relaxed) {
                return HaltReason::ExternalHalt;
            }

            // Check instruction budget
            if self.budget > 0 {
                self.budget -= 1;
                if self.budget == 0 {
                    return HaltReason::BudgetExhausted;
                }
            }

            // Fetch
            let raw = match mem.read_u32(state.pc) {
                Ok(v) => v,
                Err(_) => return HaltReason::InstructionAbort { addr: state.pc },
            };

            // Decode
            let inst = crate::pattern_decoder::decode(raw);

            // Execute
            let result = self.execute(state, mem, inst);

            match result {
                StepResult::Continue => {
                    state.pc = state.pc.wrapping_add(4);
                }
                StepResult::BranchTaken => {
                    // PC already updated by the branch instruction
                }
                StepResult::Svc(n) => {
                    state.pc = state.pc.wrapping_add(4);
                    return HaltReason::Svc(n as u32);
                }
                StepResult::Breakpoint => {
                    return HaltReason::Breakpoint;
                }
                StepResult::MemoryFault(addr) => {
                    return HaltReason::DataAbort { addr };
                }
                StepResult::InvalidInstruction(raw) => {
                    log::warn!(
                        "Unknown instruction {:08X} at PC=0x{:016X}",
                        raw, state.pc
                    );
                    state.pc = state.pc.wrapping_add(4);
                }
                StepResult::Halted => {
                    return HaltReason::ExternalHalt;
                }
            }
        }
    }

    fn halt(&mut self) {
        self.halted.store(true, Ordering::Relaxed);
    }

    fn invalidate_cache_range(&mut self, _addr: VAddr, _size: u64) {
        // No cache to invalidate in an interpreter.
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory::{MemoryAccess, MemoryFault};

    /// Simple test memory: 64 KiB flat buffer.
    struct TestMemory {
        data: Vec<u8>,
    }

    impl TestMemory {
        fn new() -> Self {
            Self { data: vec![0u8; 65536] }
        }

        fn write_inst(&mut self, addr: u64, inst: u32) {
            let off = addr as usize;
            self.data[off..off + 4].copy_from_slice(&inst.to_le_bytes());
        }
    }

    impl MemoryAccess for TestMemory {
        fn read_u8(&self, addr: u64) -> Result<u8, MemoryFault> {
            self.data.get(addr as usize).copied().ok_or(MemoryFault::Unmapped(addr))
        }
        fn read_u16(&self, addr: u64) -> Result<u16, MemoryFault> {
            let off = addr as usize;
            let slice = self.data.get(off..off + 2).ok_or(MemoryFault::Unmapped(addr))?;
            Ok(u16::from_le_bytes(slice.try_into().unwrap()))
        }
        fn read_u32(&self, addr: u64) -> Result<u32, MemoryFault> {
            let off = addr as usize;
            let slice = self.data.get(off..off + 4).ok_or(MemoryFault::Unmapped(addr))?;
            Ok(u32::from_le_bytes(slice.try_into().unwrap()))
        }
        fn read_u64(&self, addr: u64) -> Result<u64, MemoryFault> {
            let off = addr as usize;
            let slice = self.data.get(off..off + 8).ok_or(MemoryFault::Unmapped(addr))?;
            Ok(u64::from_le_bytes(slice.try_into().unwrap()))
        }
        fn read_u128(&self, addr: u64) -> Result<u128, MemoryFault> {
            let off = addr as usize;
            let slice = self.data.get(off..off + 16).ok_or(MemoryFault::Unmapped(addr))?;
            Ok(u128::from_le_bytes(slice.try_into().unwrap()))
        }
        fn write_u8(&mut self, addr: u64, val: u8) -> Result<(), MemoryFault> {
            let off = addr as usize;
            *self.data.get_mut(off).ok_or(MemoryFault::Unmapped(addr))? = val;
            Ok(())
        }
        fn write_u16(&mut self, addr: u64, val: u16) -> Result<(), MemoryFault> {
            let off = addr as usize;
            let slice = self.data.get_mut(off..off + 2).ok_or(MemoryFault::Unmapped(addr))?;
            slice.copy_from_slice(&val.to_le_bytes());
            Ok(())
        }
        fn write_u32(&mut self, addr: u64, val: u32) -> Result<(), MemoryFault> {
            let off = addr as usize;
            let slice = self.data.get_mut(off..off + 4).ok_or(MemoryFault::Unmapped(addr))?;
            slice.copy_from_slice(&val.to_le_bytes());
            Ok(())
        }
        fn write_u64(&mut self, addr: u64, val: u64) -> Result<(), MemoryFault> {
            let off = addr as usize;
            let slice = self.data.get_mut(off..off + 8).ok_or(MemoryFault::Unmapped(addr))?;
            slice.copy_from_slice(&val.to_le_bytes());
            Ok(())
        }
        fn write_u128(&mut self, addr: u64, val: u128) -> Result<(), MemoryFault> {
            let off = addr as usize;
            let slice = self.data.get_mut(off..off + 16).ok_or(MemoryFault::Unmapped(addr))?;
            slice.copy_from_slice(&val.to_le_bytes());
            Ok(())
        }
    }

    #[test]
    fn test_simple_program() {
        // MOV X0, #42; SVC #0
        let mut mem = TestMemory::new();
        let base: u64 = 0x1000;
        mem.write_inst(base, 0xD2800540);      // MOVZ X0, #42
        mem.write_inst(base + 4, 0xD4000001);  // SVC #0

        let mut state = CpuState::new();
        state.pc = base;

        let mut interp = Interpreter::new();
        let reason = interp.run(&mut state, &mut mem);

        assert_eq!(state.get_reg(0), 42);
        assert!(matches!(reason, HaltReason::Svc(0)));
    }

    #[test]
    fn test_branch_and_return() {
        // BL +8; NOP (skip); MOV X0, #1; RET
        let mut mem = TestMemory::new();
        let base: u64 = 0x1000;
        mem.write_inst(base, 0x94000002);       // BL +8
        mem.write_inst(base + 4, 0xD4000001);   // SVC #0 (return point)
        mem.write_inst(base + 8, 0xD2800020);   // MOVZ X0, #1
        mem.write_inst(base + 12, 0xD65F03C0);  // RET (X30)

        let mut state = CpuState::new();
        state.pc = base;

        let mut interp = Interpreter::new();
        let reason = interp.run(&mut state, &mut mem);

        assert_eq!(state.get_reg(0), 1);
        assert!(matches!(reason, HaltReason::Svc(0)));
        assert_eq!(state.pc, base + 8); // SVC at base+4, PC advanced to base+8
    }

    #[test]
    fn test_loop_with_sub() {
        // X0 = 3; loop: SUBS X0, X0, #1; CBNZ X0, loop; SVC #0
        let mut mem = TestMemory::new();
        let base: u64 = 0x1000;
        mem.write_inst(base, 0xD2800060);       // MOVZ X0, #3
        mem.write_inst(base + 4, 0xF1000400);   // SUBS X0, X0, #1
        mem.write_inst(base + 8, 0xB5FFFFE0);   // CBNZ X0, -4 (back to SUBS)
        mem.write_inst(base + 12, 0xD4000001);  // SVC #0

        let mut state = CpuState::new();
        state.pc = base;

        let mut interp = Interpreter::new();
        let reason = interp.run(&mut state, &mut mem);

        assert_eq!(state.get_reg(0), 0);
        assert!(matches!(reason, HaltReason::Svc(0)));
    }

    #[test]
    fn test_store_and_load() {
        // MOV X0, #0xBEEF; STR X0, [SP]; LDR X1, [SP]; SVC #0
        let mut mem = TestMemory::new();
        let base: u64 = 0x1000;
        let stack: u64 = 0x8000;

        // MOVZ X0, #0xBEEF
        mem.write_inst(base, 0xD297DDE0);
        // STR X0, [SP] (unsigned offset 0)
        mem.write_inst(base + 4, 0xF90003E0);
        // LDR X1, [SP] (unsigned offset 0)
        mem.write_inst(base + 8, 0xF94003E1);
        // SVC #0
        mem.write_inst(base + 12, 0xD4000001);

        let mut state = CpuState::new();
        state.pc = base;
        state.sp = stack;

        let mut interp = Interpreter::new();
        let reason = interp.run(&mut state, &mut mem);

        assert_eq!(state.get_reg(1), 0xBEEF);
        assert!(matches!(reason, HaltReason::Svc(0)));
    }

    #[test]
    fn test_instruction_abort() {
        let mut mem = TestMemory::new();
        let mut state = CpuState::new();
        state.pc = 0x1_0000_0000; // Way beyond test memory

        let mut interp = Interpreter::new();
        let reason = interp.run(&mut state, &mut mem);

        assert!(matches!(reason, HaltReason::InstructionAbort { .. }));
    }

    #[test]
    fn test_budget_exhausted() {
        // Write a loop: SUBS X0, X0, #1; CBNZ X0, -4 (repeats X0 times)
        // followed by SVC #0. With a budget of 5, it should exhaust before SVC.
        let mut mem = TestMemory::new();
        let base: u64 = 0x1000;
        mem.write_inst(base, 0xD2800140);       // MOVZ X0, #10
        mem.write_inst(base + 4, 0xF1000400);   // SUBS X0, X0, #1
        mem.write_inst(base + 8, 0xB5FFFFE0);   // CBNZ X0, -4 (back to SUBS)
        mem.write_inst(base + 12, 0xD4000001);  // SVC #0

        let mut state = CpuState::new();
        state.pc = base;

        let mut interp = Interpreter::new();
        interp.set_budget(5);
        let reason = interp.run(&mut state, &mut mem);

        assert!(matches!(reason, HaltReason::BudgetExhausted));
        // X0 should not be 0 since we didn't finish the loop
        assert!(state.get_reg(0) > 0);
    }

    #[test]
    fn test_budget_zero_is_unlimited() {
        // Budget of 0 means unlimited — should run until SVC.
        let mut mem = TestMemory::new();
        let base: u64 = 0x1000;
        mem.write_inst(base, 0xD2800540);      // MOVZ X0, #42
        mem.write_inst(base + 4, 0xD4000001);  // SVC #0

        let mut state = CpuState::new();
        state.pc = base;

        let mut interp = Interpreter::new();
        interp.set_budget(0); // unlimited
        let reason = interp.run(&mut state, &mut mem);

        assert_eq!(state.get_reg(0), 42);
        assert!(matches!(reason, HaltReason::Svc(0)));
    }
}
