// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell → IR translator.
//!
//! The `TranslatorVisitor` decodes each Maxwell instruction and emits
//! corresponding IR instructions via the `Emitter`.
//!
//! Organized by instruction category, mirroring zuyu's impl/ directory:
//! - arithmetic_fp.rs: FADD, FMUL, FFMA, FMNMX, MUFU, etc.
//! - arithmetic_int.rs: IADD, IADD3, IMAD, IMUL, XMAD, ISCADD, LEA, etc.
//! - comparison.rs: FSETP, ISETP, FSET, ISET, FCMP, ICMP, etc.
//! - conversion.rs: F2I, I2F, F2F, I2I
//! - bitwise.rs: LOP, LOP3, SHL, SHR, SHF, BFE, BFI, POPC, FLO, PRMT
//! - predicate.rs: PSETP, PSET, CSETP, CSET, P2R, R2P
//! - move_sel.rs: MOV, SEL, S2R, CS2R
//! - memory.rs: LDG, STG, LDL, STL, LDS, STS, LD, ST, LDC
//! - texture.rs: TEX, TEXS, TLD, TLDS, TLD4, TLD4S, TXQ, TMML, TXD, TXA
//! - attribute.rs: ALD, AST, IPA, OUT

pub mod arithmetic_fp;
pub mod arithmetic_int;
pub mod comparison;
pub mod conversion;
pub mod bitwise;
pub mod predicate;
pub mod move_sel;
pub mod memory;
pub mod texture;
pub mod attribute;

use crate::shader_recompiler::frontend::maxwell_opcodes::{MaxwellOpcode, SrcType};
use crate::shader_recompiler::ir::emitter::Emitter;
use crate::shader_recompiler::ir::program::{Program, ShaderInfo};
use crate::shader_recompiler::ir::types::ShaderStage;
use crate::shader_recompiler::ir::value::{Reg, Value};
use crate::shader_recompiler::frontend::maxwell_opcodes;

/// Maxwell instruction bit field extraction helpers.
pub fn field(insn: u64, start: u32, len: u32) -> u32 {
    ((insn >> start) & ((1u64 << len) - 1)) as u32
}

pub fn bit(insn: u64, pos: u32) -> bool {
    (insn >> pos) & 1 != 0
}

pub fn sfield(insn: u64, start: u32, len: u32) -> i32 {
    let val = field(insn, start, len);
    let sign_bit = 1u32 << (len - 1);
    if val & sign_bit != 0 {
        (val | !((1u32 << len) - 1)) as i32
    } else {
        val as i32
    }
}

/// The translator visitor: holds state during translation of a single shader.
pub struct TranslatorVisitor<'a> {
    pub ir: Emitter<'a>,
    pub stage: ShaderStage,
}

impl<'a> TranslatorVisitor<'a> {
    pub fn new(program: &'a mut Program, block: u32) -> Self {
        let stage = program.stage;
        Self {
            ir: Emitter::new(program, block),
            stage,
        }
    }

    /// Get a register value as U32.
    pub fn x(&mut self, reg_idx: u32) -> Value {
        let reg = Reg(reg_idx as u8);
        if reg.is_zero() {
            Value::ImmU32(0)
        } else {
            self.ir.get_reg(reg)
        }
    }

    /// Get a register value interpreted as F32.
    pub fn f(&mut self, reg_idx: u32) -> Value {
        let u = self.x(reg_idx);
        self.ir.bit_cast_f32_u32(u)
    }

    /// Set a register to a U32 value.
    pub fn set_x(&mut self, reg_idx: u32, value: Value) {
        let reg = Reg(reg_idx as u8);
        if !reg.is_zero() {
            self.ir.set_reg(reg, value);
        }
    }

    /// Set a register to an F32 value (via bitcast).
    pub fn set_f(&mut self, reg_idx: u32, value: Value) {
        let bits = self.ir.bit_cast_u32_f32(value);
        self.set_x(reg_idx, bits);
    }

    /// Decode src_b operand based on opcode variant (register, cbuf, immediate).
    pub fn decode_src_b(&mut self, insn: u64, opcode: MaxwellOpcode) -> Value {
        match opcode.src_type() {
            SrcType::Register => {
                let reg_idx = field(insn, 20, 8);
                self.x(reg_idx)
            }
            SrcType::ConstantBuffer => {
                let cb_index = field(insn, 34, 5);
                let cb_offset = field(insn, 20, 14) << 2;
                let binding = Value::ImmU32(cb_index);
                let offset = Value::ImmU32(cb_offset);
                // Register in shader info
                self.ir.program.info.register_cbuf(cb_index);
                self.ir.get_cbuf_u32(binding, offset)
            }
            SrcType::Immediate => {
                let imm = field(insn, 20, 19);
                // Sign-extend from 19 bits
                let sign_ext = if imm & (1 << 18) != 0 {
                    imm | !((1u32 << 19) - 1)
                } else {
                    imm
                };
                Value::ImmU32(sign_ext)
            }
        }
    }

    /// Decode src_b as F32 (for floating-point instructions).
    pub fn decode_src_b_f32(&mut self, insn: u64, opcode: MaxwellOpcode) -> Value {
        match opcode.src_type() {
            SrcType::Register => {
                let reg_idx = field(insn, 20, 8);
                self.f(reg_idx)
            }
            SrcType::ConstantBuffer => {
                let cb_index = field(insn, 34, 5);
                let cb_offset = field(insn, 20, 14) << 2;
                let binding = Value::ImmU32(cb_index);
                let offset = Value::ImmU32(cb_offset);
                self.ir.program.info.register_cbuf(cb_index);
                self.ir.get_cbuf_f32(binding, offset)
            }
            SrcType::Immediate => {
                let imm_bits = field(insn, 20, 19) << 13; // FP immediate in upper bits
                let f = f32::from_bits(imm_bits);
                Value::ImmF32(f)
            }
        }
    }

    /// Decode the 32-bit immediate for 32I-type instructions.
    pub fn decode_imm32(&self, insn: u64) -> u32 {
        (insn & 0xFFFFFFFF) as u32
    }

    /// Decode the destination register index from bits [7:0].
    pub fn dst_reg(&self, insn: u64) -> u32 {
        field(insn, 0, 8)
    }

    /// Decode src_a register index from bits [15:8].
    pub fn src_a_reg(&self, insn: u64) -> u32 {
        field(insn, 8, 8)
    }

    /// Decode predicate register for result from bits [47:44].
    pub fn dst_pred(&self, insn: u64) -> u32 {
        field(insn, 44, 3)
    }

    /// Decode predicate register for secondary result from bits [3:1].
    pub fn dst_pred2(&self, insn: u64) -> u32 {
        field(insn, 1, 3)
    }

    /// Translate a single Maxwell instruction word.
    pub fn translate_instruction(&mut self, insn: u64) {
        let opcode = match maxwell_opcodes::decode_opcode(insn) {
            Some(op) => op,
            None => {
                log::warn!("Unknown Maxwell opcode: 0x{:016X}", insn);
                return;
            }
        };

        match opcode {
            // FP32 arithmetic
            MaxwellOpcode::FADD_reg | MaxwellOpcode::FADD_cbuf | MaxwellOpcode::FADD_imm => {
                self::arithmetic_fp::fadd(self, insn, opcode);
            }
            MaxwellOpcode::FADD32I => {
                self::arithmetic_fp::fadd32i(self, insn);
            }
            MaxwellOpcode::FMUL_reg | MaxwellOpcode::FMUL_cbuf | MaxwellOpcode::FMUL_imm => {
                self::arithmetic_fp::fmul(self, insn, opcode);
            }
            MaxwellOpcode::FMUL32I => {
                self::arithmetic_fp::fmul32i(self, insn);
            }
            MaxwellOpcode::FFMA_reg
            | MaxwellOpcode::FFMA_rc
            | MaxwellOpcode::FFMA_cr
            | MaxwellOpcode::FFMA_imm => {
                self::arithmetic_fp::ffma(self, insn, opcode);
            }
            MaxwellOpcode::FFMA32I => {
                self::arithmetic_fp::ffma32i(self, insn);
            }
            MaxwellOpcode::FMNMX_reg | MaxwellOpcode::FMNMX_cbuf | MaxwellOpcode::FMNMX_imm => {
                self::arithmetic_fp::fmnmx(self, insn, opcode);
            }
            MaxwellOpcode::MUFU => {
                self::arithmetic_fp::mufu(self, insn);
            }

            // Integer arithmetic
            MaxwellOpcode::IADD_reg | MaxwellOpcode::IADD_cbuf | MaxwellOpcode::IADD_imm => {
                self::arithmetic_int::iadd(self, insn, opcode);
            }
            MaxwellOpcode::IADD32I => {
                self::arithmetic_int::iadd32i(self, insn);
            }
            MaxwellOpcode::IADD3_reg | MaxwellOpcode::IADD3_cbuf | MaxwellOpcode::IADD3_imm => {
                self::arithmetic_int::iadd3(self, insn, opcode);
            }
            MaxwellOpcode::IMAD_reg
            | MaxwellOpcode::IMAD_rc
            | MaxwellOpcode::IMAD_cr
            | MaxwellOpcode::IMAD_imm => {
                self::arithmetic_int::imad(self, insn, opcode);
            }
            MaxwellOpcode::IMAD32I => {
                self::arithmetic_int::imad32i(self, insn);
            }
            MaxwellOpcode::IMUL_reg | MaxwellOpcode::IMUL_cbuf | MaxwellOpcode::IMUL_imm => {
                self::arithmetic_int::imul(self, insn, opcode);
            }
            MaxwellOpcode::ISCADD_reg
            | MaxwellOpcode::ISCADD_cbuf
            | MaxwellOpcode::ISCADD_imm => {
                self::arithmetic_int::iscadd(self, insn, opcode);
            }
            MaxwellOpcode::XMAD_reg
            | MaxwellOpcode::XMAD_rc
            | MaxwellOpcode::XMAD_cr
            | MaxwellOpcode::XMAD_imm => {
                self::arithmetic_int::xmad(self, insn, opcode);
            }
            MaxwellOpcode::IMNMX_reg | MaxwellOpcode::IMNMX_cbuf | MaxwellOpcode::IMNMX_imm => {
                self::arithmetic_int::imnmx(self, insn, opcode);
            }

            // Comparison
            MaxwellOpcode::FSETP_reg | MaxwellOpcode::FSETP_cbuf | MaxwellOpcode::FSETP_imm => {
                self::comparison::fsetp(self, insn, opcode);
            }
            MaxwellOpcode::ISETP_reg | MaxwellOpcode::ISETP_cbuf | MaxwellOpcode::ISETP_imm => {
                self::comparison::isetp(self, insn, opcode);
            }
            MaxwellOpcode::FSET_reg | MaxwellOpcode::FSET_cbuf | MaxwellOpcode::FSET_imm => {
                self::comparison::fset(self, insn, opcode);
            }
            MaxwellOpcode::ISET_reg | MaxwellOpcode::ISET_cbuf | MaxwellOpcode::ISET_imm => {
                self::comparison::iset(self, insn, opcode);
            }

            // Conversion
            MaxwellOpcode::F2I_reg | MaxwellOpcode::F2I_cbuf | MaxwellOpcode::F2I_imm => {
                self::conversion::f2i(self, insn, opcode);
            }
            MaxwellOpcode::I2F_reg | MaxwellOpcode::I2F_cbuf | MaxwellOpcode::I2F_imm => {
                self::conversion::i2f(self, insn, opcode);
            }
            MaxwellOpcode::F2F_reg | MaxwellOpcode::F2F_cbuf | MaxwellOpcode::F2F_imm => {
                self::conversion::f2f(self, insn, opcode);
            }
            MaxwellOpcode::I2I_reg | MaxwellOpcode::I2I_cbuf | MaxwellOpcode::I2I_imm => {
                self::conversion::i2i(self, insn, opcode);
            }

            // Bitwise
            MaxwellOpcode::LOP_reg | MaxwellOpcode::LOP_cbuf | MaxwellOpcode::LOP_imm => {
                self::bitwise::lop(self, insn, opcode);
            }
            MaxwellOpcode::LOP3_reg | MaxwellOpcode::LOP3_cbuf | MaxwellOpcode::LOP3_imm => {
                self::bitwise::lop3(self, insn, opcode);
            }
            MaxwellOpcode::LOP32I => {
                self::bitwise::lop32i(self, insn);
            }
            MaxwellOpcode::SHL_reg | MaxwellOpcode::SHL_cbuf | MaxwellOpcode::SHL_imm => {
                self::bitwise::shl(self, insn, opcode);
            }
            MaxwellOpcode::SHR_reg | MaxwellOpcode::SHR_cbuf | MaxwellOpcode::SHR_imm => {
                self::bitwise::shr(self, insn, opcode);
            }
            MaxwellOpcode::BFE_reg | MaxwellOpcode::BFE_cbuf | MaxwellOpcode::BFE_imm => {
                self::bitwise::bfe(self, insn, opcode);
            }
            MaxwellOpcode::BFI_reg
            | MaxwellOpcode::BFI_rc
            | MaxwellOpcode::BFI_cr
            | MaxwellOpcode::BFI_imm => {
                self::bitwise::bfi(self, insn, opcode);
            }
            MaxwellOpcode::POPC_reg | MaxwellOpcode::POPC_cbuf | MaxwellOpcode::POPC_imm => {
                self::bitwise::popc(self, insn, opcode);
            }
            MaxwellOpcode::FLO_reg | MaxwellOpcode::FLO_cbuf | MaxwellOpcode::FLO_imm => {
                self::bitwise::flo(self, insn, opcode);
            }
            MaxwellOpcode::PRMT_reg
            | MaxwellOpcode::PRMT_rc
            | MaxwellOpcode::PRMT_cr
            | MaxwellOpcode::PRMT_imm => {
                self::bitwise::prmt(self, insn, opcode);
            }

            // Move / Select
            MaxwellOpcode::MOV_reg | MaxwellOpcode::MOV_cbuf | MaxwellOpcode::MOV_imm => {
                self::move_sel::mov(self, insn, opcode);
            }
            MaxwellOpcode::MOV32I => {
                self::move_sel::mov32i(self, insn);
            }
            MaxwellOpcode::SEL_reg | MaxwellOpcode::SEL_cbuf | MaxwellOpcode::SEL_imm => {
                self::move_sel::sel(self, insn, opcode);
            }
            MaxwellOpcode::S2R => {
                self::move_sel::s2r(self, insn);
            }

            // Predicate
            MaxwellOpcode::PSETP => {
                self::predicate::psetp(self, insn);
            }
            MaxwellOpcode::PSET => {
                self::predicate::pset(self, insn);
            }

            // Memory
            MaxwellOpcode::LDG => {
                self::memory::ldg(self, insn);
            }
            MaxwellOpcode::STG => {
                self::memory::stg(self, insn);
            }
            MaxwellOpcode::LDC => {
                self::memory::ldc(self, insn);
            }
            MaxwellOpcode::LDL => {
                self::memory::ldl(self, insn);
            }
            MaxwellOpcode::STL => {
                self::memory::stl(self, insn);
            }

            // Texture
            MaxwellOpcode::TEX | MaxwellOpcode::TEX_b => {
                self::texture::tex(self, insn, opcode);
            }
            MaxwellOpcode::TEXS => {
                self::texture::texs(self, insn);
            }
            MaxwellOpcode::TLD | MaxwellOpcode::TLD_b => {
                self::texture::tld(self, insn, opcode);
            }
            MaxwellOpcode::TLDS => {
                self::texture::tlds(self, insn);
            }
            MaxwellOpcode::TLD4 | MaxwellOpcode::TLD4_b => {
                self::texture::tld4(self, insn, opcode);
            }
            MaxwellOpcode::TXQ | MaxwellOpcode::TXQ_b => {
                self::texture::txq(self, insn, opcode);
            }

            // Attribute
            MaxwellOpcode::ALD => {
                self::attribute::ald(self, insn);
            }
            MaxwellOpcode::AST => {
                self::attribute::ast(self, insn);
            }
            MaxwellOpcode::IPA => {
                self::attribute::ipa(self, insn);
            }

            // Control flow — handled by the CFG builder, but we need to
            // translate the predicate conditions.
            MaxwellOpcode::BRA
            | MaxwellOpcode::BRK
            | MaxwellOpcode::SYNC
            | MaxwellOpcode::CONT
            | MaxwellOpcode::EXIT
            | MaxwellOpcode::SSY
            | MaxwellOpcode::PBK
            | MaxwellOpcode::PCNT
            | MaxwellOpcode::PEXIT
            | MaxwellOpcode::CAL
            | MaxwellOpcode::RET => {
                // Control flow is handled by the CFG/structured CF passes.
                // The translator doesn't emit IR for these.
            }

            // NOP / dependency barrier
            MaxwellOpcode::NOP | MaxwellOpcode::DEPBAR => {
                // No-op in IR.
            }

            // Kill
            MaxwellOpcode::KIL => {
                self.ir.demote_to_helper_invocation();
                self.ir.program.info.uses_demote = true;
            }

            // Memory barrier
            MaxwellOpcode::MEMBAR => {
                self.ir.device_memory_barrier();
            }

            // Everything else — log and skip
            _ => {
                log::trace!(
                    "Unimplemented Maxwell opcode in translator: {} (0x{:016X})",
                    opcode,
                    insn
                );
            }
        }
    }
}
