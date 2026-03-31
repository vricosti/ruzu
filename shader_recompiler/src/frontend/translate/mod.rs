// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/
//!
//! Maxwell → IR translator.
//!
//! The `TranslatorVisitor` decodes each Maxwell instruction and emits
//! corresponding IR instructions via the `Emitter`.
//!
//! Each submodule corresponds 1:1 to an upstream `impl/*.cpp` file.

// Instruction translation modules (1:1 with upstream impl/*.cpp files)
pub mod atomic_operations_global_memory;
pub mod atomic_operations_shared_memory;
pub mod attribute_memory_to_physical;
pub mod barrier_operations;
pub mod bitfield_extract;
pub mod bitfield_insert;
pub mod branch_indirect;
pub mod common_encoding;
pub mod common_funcs;
pub mod condition_code_set;
pub mod double_add;
pub mod double_compare_and_set;
pub mod double_fused_multiply_add;
pub mod double_min_max;
pub mod double_multiply;
pub mod double_set_predicate;
pub mod exit_program;
pub mod find_leading_one;
pub mod floating_point_add;
pub mod floating_point_compare;
pub mod floating_point_compare_and_set;
pub mod floating_point_conversion_floating_point;
pub mod floating_point_conversion_integer;
pub mod floating_point_fused_multiply_add;
pub mod floating_point_min_max;
pub mod floating_point_multi_function;
pub mod floating_point_multiply;
pub mod floating_point_range_reduction;
pub mod floating_point_set_predicate;
pub mod floating_point_swizzled_add;
pub mod half_floating_point_add;
pub mod half_floating_point_fused_multiply_add;
pub mod half_floating_point_helper;
pub mod half_floating_point_multiply;
pub mod half_floating_point_set;
pub mod half_floating_point_set_predicate;
pub mod integer_add;
pub mod integer_add_three_input;
pub mod integer_compare;
pub mod integer_compare_and_set;
pub mod integer_floating_point_conversion;
pub mod integer_funnel_shift;
pub mod integer_minimum_maximum;
pub mod integer_popcount;
pub mod integer_scaled_add;
pub mod integer_set_predicate;
pub mod integer_shift_left;
pub mod integer_shift_right;
pub mod integer_short_multiply_add;
pub mod integer_to_integer_conversion;
pub mod internal_stage_buffer_entry_read;
pub mod load_constant;
pub mod load_effective_address;
pub mod load_store_attribute;
pub mod load_store_local_shared;
pub mod load_store_memory;
pub mod logic_operation;
pub mod logic_operation_three_input;
pub mod move_predicate_to_register;
pub mod move_register;
pub mod move_register_to_predicate;
pub mod move_special_register;
pub mod not_implemented;
pub mod output_geometry;
pub mod pixel_load;
pub mod predicate_set_predicate;
pub mod predicate_set_register;
pub mod select_source_with_predicate;
pub mod surface_atomic_operations;
pub mod surface_load_store;
pub mod texture_fetch;
pub mod texture_fetch_swizzled;
pub mod texture_gather;
pub mod texture_gather_swizzled;
pub mod texture_gradient;
pub mod texture_load;
pub mod texture_load_swizzled;
pub mod texture_mipmap_level;
pub mod texture_query;
pub mod video_helper;
pub mod video_minimum_maximum;
pub mod video_multiply_add;
pub mod video_set_predicate;
pub mod vote;
pub mod warp_shuffle;

use crate::frontend::maxwell_opcodes::{self, MaxwellOpcode, SrcType};
use crate::ir::emitter::Emitter;
use crate::ir::program::{Program, ShaderInfo};
use crate::ir::types::ShaderStage;
use crate::ir::value::{Reg, Value};

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
///
/// Corresponds to the `TranslatorVisitor` class in upstream `impl.h` / `impl.cpp`.
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
                self.ir.program.info.register_cbuf(cb_index);
                self.ir.get_cbuf_u32(binding, offset)
            }
            SrcType::Immediate => {
                let imm = field(insn, 20, 19);
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
                let imm_bits = field(insn, 20, 19) << 13;
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

    /// Get a double-precision (F64) value from a register pair at reg_idx and reg_idx+1.
    ///
    /// Corresponds to `TranslatorVisitor::D(IR::Reg reg)` upstream.
    pub fn d(&mut self, reg_idx: u32) -> Value {
        let lo = self.x(reg_idx);
        let hi = self.x(reg_idx + 1);
        let vec = self.ir.composite_construct_u32x2(lo, hi);
        self.ir.pack_double_2x32(vec)
    }

    /// Store a double-precision (F64) value into a register pair.
    ///
    /// Corresponds to `TranslatorVisitor::D(IR::Reg dest, const IR::F64& value)` upstream.
    pub fn set_d(&mut self, reg_idx: u32, value: Value) {
        let unpacked = self.ir.unpack_double_2x32(value);
        let lo = self.ir.composite_extract_u32x2_idx(unpacked.clone(), 0);
        let hi = self.ir.composite_extract_u32x2_idx(unpacked, 1);
        self.set_x(reg_idx, lo);
        self.set_x(reg_idx + 1, hi);
    }

    /// Get a reg from bits [20:27] as U32 (GetReg20 upstream).
    pub fn get_reg20(&mut self, insn: u64) -> Value {
        let idx = field(insn, 20, 8);
        self.x(idx)
    }

    /// Get a reg from bits [39:46] as U32 (GetReg39 upstream).
    pub fn get_reg39(&mut self, insn: u64) -> Value {
        let idx = field(insn, 39, 8);
        self.x(idx)
    }

    /// Get a double from register pair at bits [20:27].
    pub fn get_double_reg20(&mut self, insn: u64) -> Value {
        let idx = field(insn, 20, 8);
        self.d(idx)
    }

    /// Get a double from register pair at bits [39:46].
    pub fn get_double_reg39(&mut self, insn: u64) -> Value {
        let idx = field(insn, 39, 8);
        self.d(idx)
    }

    /// Get a U32 from a constant buffer (GetCbuf upstream).
    pub fn get_cbuf(&mut self, insn: u64) -> Value {
        let cb_index = field(insn, 34, 5);
        let cb_offset = field(insn, 20, 14) << 2;
        let binding = Value::ImmU32(cb_index);
        let offset = Value::ImmU32(cb_offset);
        self.ir.program.info.register_cbuf(cb_index);
        self.ir.get_cbuf_u32(binding, offset)
    }

    /// Get an F32 from a constant buffer.
    pub fn get_float_cbuf(&mut self, insn: u64) -> Value {
        let cb_index = field(insn, 34, 5);
        let cb_offset = field(insn, 20, 14) << 2;
        let binding = Value::ImmU32(cb_index);
        let offset = Value::ImmU32(cb_offset);
        self.ir.program.info.register_cbuf(cb_index);
        self.ir.get_cbuf_f32(binding, offset)
    }

    /// Get an F64 from a constant buffer (two 32-bit reads packed into F64).
    pub fn get_double_cbuf(&mut self, insn: u64) -> Value {
        let cb_index = field(insn, 34, 5);
        let cb_offset = field(insn, 20, 14) << 2;
        let binding = Value::ImmU32(cb_index);
        self.ir.program.info.register_cbuf(cb_index);
        let lo = self
            .ir
            .get_cbuf_u32(binding.clone(), Value::ImmU32(cb_offset));
        let hi = self.ir.get_cbuf_u32(binding, Value::ImmU32(cb_offset + 4));
        let vec = self.ir.composite_construct_u32x2(lo, hi);
        self.ir.pack_double_2x32(vec)
    }

    /// Get a sign-extended 20-bit immediate as U32 (GetImm20 upstream).
    pub fn get_imm20(&self, insn: u64) -> Value {
        let imm = sfield(insn, 20, 20);
        Value::ImmU32(imm as u32)
    }

    /// Get a 20-bit immediate as F32 (sign bit at bit 56, mantissa at [20:38]).
    pub fn get_float_imm20(&self, insn: u64) -> Value {
        let imm = field(insn, 20, 19) << 12;
        let sign = if bit(insn, 56) { 1u32 << 31 } else { 0 };
        Value::ImmF32(f32::from_bits(imm | sign))
    }

    /// Get a 20-bit double immediate packed into F64 (upper bits zero).
    pub fn get_double_imm20(&self, insn: u64) -> Value {
        let imm = sfield(insn, 20, 20) as i64;
        Value::ImmF64(imm as f64)
    }

    /// Get a register value from bits [8:15] as F32 (GetFloatReg8 upstream).
    pub fn get_float_reg8(&mut self, insn: u64) -> Value {
        let idx = field(insn, 8, 8);
        self.f(idx)
    }

    /// Get a register value from bits [20:27] as F32 (GetFloatReg20 upstream).
    pub fn get_float_reg20(&mut self, insn: u64) -> Value {
        let idx = field(insn, 20, 8);
        self.f(idx)
    }

    /// Get a register value from bits [39:46] as F32 (GetFloatReg39 upstream).
    pub fn get_float_reg39(&mut self, insn: u64) -> Value {
        let idx = field(insn, 39, 8);
        self.f(idx)
    }

    /// Translate a single Maxwell instruction word.
    ///
    /// Corresponds to the dispatch table in upstream `impl.cpp`.
    pub fn translate_instruction(&mut self, insn: u64) {
        let opcode = match maxwell_opcodes::decode_opcode(insn) {
            Some(op) => op,
            None => {
                log::warn!("Unknown Maxwell opcode: 0x{:016X}", insn);
                return;
            }
        };

        match opcode {
            // FP32 arithmetic — floating_point_add.cpp
            MaxwellOpcode::FADD_reg | MaxwellOpcode::FADD_cbuf | MaxwellOpcode::FADD_imm => {
                self::floating_point_add::fadd(self, insn, opcode);
            }
            MaxwellOpcode::FADD32I => {
                self::floating_point_add::fadd32i(self, insn);
            }

            // floating_point_multiply.cpp
            MaxwellOpcode::FMUL_reg | MaxwellOpcode::FMUL_cbuf | MaxwellOpcode::FMUL_imm => {
                self::floating_point_multiply::fmul(self, insn, opcode);
            }
            MaxwellOpcode::FMUL32I => {
                self::floating_point_multiply::fmul32i(self, insn);
            }

            // floating_point_fused_multiply_add.cpp
            MaxwellOpcode::FFMA_reg
            | MaxwellOpcode::FFMA_rc
            | MaxwellOpcode::FFMA_cr
            | MaxwellOpcode::FFMA_imm => {
                self::floating_point_fused_multiply_add::ffma(self, insn, opcode);
            }
            MaxwellOpcode::FFMA32I => {
                self::floating_point_fused_multiply_add::ffma32i(self, insn);
            }

            // floating_point_min_max.cpp
            MaxwellOpcode::FMNMX_reg | MaxwellOpcode::FMNMX_cbuf | MaxwellOpcode::FMNMX_imm => {
                self::floating_point_min_max::fmnmx(self, insn, opcode);
            }

            // floating_point_multi_function.cpp
            MaxwellOpcode::MUFU => {
                self::floating_point_multi_function::mufu(self, insn);
            }

            // integer_add.cpp
            MaxwellOpcode::IADD_reg | MaxwellOpcode::IADD_cbuf | MaxwellOpcode::IADD_imm => {
                self::integer_add::iadd(self, insn, opcode);
            }
            MaxwellOpcode::IADD32I => {
                self::integer_add::iadd32i(self, insn);
            }

            // integer_add_three_input.cpp
            MaxwellOpcode::IADD3_reg | MaxwellOpcode::IADD3_cbuf | MaxwellOpcode::IADD3_imm => {
                self::integer_add_three_input::iadd3(self, insn, opcode);
            }

            // integer_short_multiply_add.cpp
            MaxwellOpcode::IMAD_reg
            | MaxwellOpcode::IMAD_rc
            | MaxwellOpcode::IMAD_cr
            | MaxwellOpcode::IMAD_imm => {
                self::integer_short_multiply_add::imad(self, insn, opcode);
            }
            MaxwellOpcode::IMAD32I => {
                self::integer_short_multiply_add::imad32i(self, insn);
            }
            MaxwellOpcode::XMAD_reg
            | MaxwellOpcode::XMAD_rc
            | MaxwellOpcode::XMAD_cr
            | MaxwellOpcode::XMAD_imm => {
                self::integer_short_multiply_add::xmad(self, insn, opcode);
            }

            // integer_scaled_add.cpp
            MaxwellOpcode::ISCADD_reg | MaxwellOpcode::ISCADD_cbuf | MaxwellOpcode::ISCADD_imm => {
                self::integer_scaled_add::iscadd(self, insn, opcode);
            }

            // integer_minimum_maximum.cpp
            MaxwellOpcode::IMNMX_reg | MaxwellOpcode::IMNMX_cbuf | MaxwellOpcode::IMNMX_imm => {
                self::integer_minimum_maximum::imnmx(self, insn, opcode);
            }

            // floating_point_set_predicate.cpp
            MaxwellOpcode::FSETP_reg | MaxwellOpcode::FSETP_cbuf | MaxwellOpcode::FSETP_imm => {
                self::floating_point_set_predicate::fsetp(self, insn, opcode);
            }

            // integer_set_predicate.cpp
            MaxwellOpcode::ISETP_reg | MaxwellOpcode::ISETP_cbuf | MaxwellOpcode::ISETP_imm => {
                self::integer_set_predicate::isetp(self, insn, opcode);
            }

            // floating_point_compare_and_set.cpp
            MaxwellOpcode::FSET_reg | MaxwellOpcode::FSET_cbuf | MaxwellOpcode::FSET_imm => {
                self::floating_point_compare_and_set::fset(self, insn, opcode);
            }

            // integer_compare_and_set.cpp
            MaxwellOpcode::ISET_reg | MaxwellOpcode::ISET_cbuf | MaxwellOpcode::ISET_imm => {
                self::integer_compare_and_set::iset(self, insn, opcode);
            }

            // floating_point_conversion_integer.cpp
            MaxwellOpcode::F2I_reg | MaxwellOpcode::F2I_cbuf | MaxwellOpcode::F2I_imm => {
                self::floating_point_conversion_integer::f2i(self, insn, opcode);
            }

            // integer_floating_point_conversion.cpp
            MaxwellOpcode::I2F_reg | MaxwellOpcode::I2F_cbuf | MaxwellOpcode::I2F_imm => {
                self::integer_floating_point_conversion::i2f(self, insn, opcode);
            }

            // floating_point_conversion_floating_point.cpp
            MaxwellOpcode::F2F_reg | MaxwellOpcode::F2F_cbuf | MaxwellOpcode::F2F_imm => {
                self::floating_point_conversion_floating_point::f2f(self, insn, opcode);
            }

            // integer_to_integer_conversion.cpp
            MaxwellOpcode::I2I_reg | MaxwellOpcode::I2I_cbuf | MaxwellOpcode::I2I_imm => {
                self::integer_to_integer_conversion::i2i(self, insn, opcode);
            }

            // logic_operation.cpp
            MaxwellOpcode::LOP_reg | MaxwellOpcode::LOP_cbuf | MaxwellOpcode::LOP_imm => {
                self::logic_operation::lop(self, insn, opcode);
            }
            MaxwellOpcode::LOP32I => {
                self::logic_operation::lop32i(self, insn);
            }

            // logic_operation_three_input.cpp
            MaxwellOpcode::LOP3_reg | MaxwellOpcode::LOP3_cbuf | MaxwellOpcode::LOP3_imm => {
                self::logic_operation_three_input::lop3(self, insn, opcode);
            }

            // integer_shift_left.cpp
            MaxwellOpcode::SHL_reg | MaxwellOpcode::SHL_cbuf | MaxwellOpcode::SHL_imm => {
                self::integer_shift_left::shl(self, insn, opcode);
            }

            // integer_shift_right.cpp
            MaxwellOpcode::SHR_reg | MaxwellOpcode::SHR_cbuf | MaxwellOpcode::SHR_imm => {
                self::integer_shift_right::shr(self, insn, opcode);
            }

            // bitfield_extract.cpp
            MaxwellOpcode::BFE_reg | MaxwellOpcode::BFE_cbuf | MaxwellOpcode::BFE_imm => {
                self::bitfield_extract::bfe(self, insn, opcode);
            }

            // bitfield_insert.cpp
            MaxwellOpcode::BFI_reg
            | MaxwellOpcode::BFI_rc
            | MaxwellOpcode::BFI_cr
            | MaxwellOpcode::BFI_imm => {
                self::bitfield_insert::bfi(self, insn, opcode);
            }

            // integer_popcount.cpp
            MaxwellOpcode::POPC_reg | MaxwellOpcode::POPC_cbuf | MaxwellOpcode::POPC_imm => {
                self::integer_popcount::popc(self, insn, opcode);
            }

            // find_leading_one.cpp
            MaxwellOpcode::FLO_reg | MaxwellOpcode::FLO_cbuf | MaxwellOpcode::FLO_imm => {
                self::find_leading_one::flo(self, insn, opcode);
            }

            // move_register.cpp
            MaxwellOpcode::MOV_reg | MaxwellOpcode::MOV_cbuf | MaxwellOpcode::MOV_imm => {
                self::move_register::mov(self, insn, opcode);
            }
            MaxwellOpcode::MOV32I => {
                self::move_register::mov32i(self, insn);
            }

            // select_source_with_predicate.cpp
            MaxwellOpcode::SEL_reg | MaxwellOpcode::SEL_cbuf | MaxwellOpcode::SEL_imm => {
                self::select_source_with_predicate::sel(self, insn, opcode);
            }

            // move_special_register.cpp
            MaxwellOpcode::S2R => {
                self::move_special_register::s2r(self, insn);
            }

            // predicate_set_predicate.cpp
            MaxwellOpcode::PSETP => {
                self::predicate_set_predicate::psetp(self, insn);
            }

            // predicate_set_register.cpp
            MaxwellOpcode::PSET => {
                self::predicate_set_register::pset(self, insn);
            }

            // load_store_memory.cpp
            MaxwellOpcode::LDG => {
                self::load_store_memory::ldg(self, insn);
            }
            MaxwellOpcode::STG => {
                self::load_store_memory::stg(self, insn);
            }

            // load_constant.cpp
            MaxwellOpcode::LDC => {
                self::load_constant::ldc(self, insn);
            }

            // load_store_local_shared.cpp
            MaxwellOpcode::LDL => {
                self::load_store_local_shared::ldl(self, insn);
            }
            MaxwellOpcode::STL => {
                self::load_store_local_shared::stl(self, insn);
            }

            // texture_fetch.cpp
            MaxwellOpcode::TEX | MaxwellOpcode::TEX_b => {
                self::texture_fetch::tex(self, insn, opcode);
            }

            // texture_fetch_swizzled.cpp
            MaxwellOpcode::TEXS => {
                self::texture_fetch_swizzled::texs(self, insn);
            }

            // texture_load.cpp
            MaxwellOpcode::TLD | MaxwellOpcode::TLD_b => {
                self::texture_load::tld(self, insn, opcode);
            }

            // texture_load_swizzled.cpp
            MaxwellOpcode::TLDS => {
                self::texture_load_swizzled::tlds(self, insn);
            }

            // texture_gather.cpp
            MaxwellOpcode::TLD4 | MaxwellOpcode::TLD4_b => {
                self::texture_gather::tld4(self, insn, opcode);
            }

            // texture_query.cpp
            MaxwellOpcode::TXQ | MaxwellOpcode::TXQ_b => {
                self::texture_query::txq(self, insn, opcode);
            }

            // load_store_attribute.cpp
            MaxwellOpcode::ALD => {
                self::load_store_attribute::ald(self, insn);
            }
            MaxwellOpcode::AST => {
                self::load_store_attribute::ast(self, insn);
            }
            MaxwellOpcode::IPA => {
                self::load_store_attribute::ipa(self, insn);
            }

            // Control flow — handled by the CFG builder
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
            }

            // NOP / dependency barrier
            MaxwellOpcode::NOP | MaxwellOpcode::DEPBAR => {}

            // Kill
            MaxwellOpcode::KIL => {
                self.ir.demote_to_helper_invocation();
                self.ir.program.info.uses_demote = true;
            }

            // Memory barrier
            MaxwellOpcode::MEMBAR => {
                self.ir.device_memory_barrier();
            }

            // Everything else
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
