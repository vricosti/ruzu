// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! IR Emitter — builder interface for constructing IR instructions.
//!
//! Matches zuyu's `IREmitter` from `ir_emitter.h`. Provides typed methods for
//! emitting IR instructions with correct argument types.

use super::instruction::Inst;
use super::opcodes::Opcode;
use super::program::Program;
use super::types::FpControl;
use super::value::{Attribute, InstRef, Patch, Pred, Reg, Value};

/// IR builder that emits instructions into a specific block.
pub struct Emitter<'a> {
    pub program: &'a mut Program,
    pub block: u32,
}

impl<'a> Emitter<'a> {
    /// Create a new emitter targeting the given block.
    pub fn new(program: &'a mut Program, block: u32) -> Self {
        Self { program, block }
    }

    /// Change the target block.
    pub fn set_block(&mut self, block: u32) {
        self.block = block;
    }

    /// Emit a raw instruction and return a reference to its result.
    fn emit(&mut self, inst: Inst) -> Value {
        let inst_idx = self.program.block_mut(self.block).append_inst(inst);
        Value::Inst(InstRef {
            block: self.block,
            inst: inst_idx,
        })
    }

    /// Emit a void instruction (no result value used).
    fn emit_void(&mut self, inst: Inst) {
        self.program.block_mut(self.block).append_inst(inst);
    }

    // ── Immediates ────────────────────────────────────────────────────

    pub fn imm_u1(&self, v: bool) -> Value {
        Value::ImmU1(v)
    }

    pub fn imm_u32(&self, v: u32) -> Value {
        Value::ImmU32(v)
    }

    pub fn imm_f32(&self, v: f32) -> Value {
        Value::ImmF32(v)
    }

    pub fn imm_u64(&self, v: u64) -> Value {
        Value::ImmU64(v)
    }

    pub fn imm_f64(&self, v: f64) -> Value {
        Value::ImmF64(v)
    }

    // ── Control / Meta ────────────────────────────────────────────────

    pub fn phi(&mut self) -> Value {
        self.emit(Inst::phi())
    }

    pub fn identity(&mut self, value: Value) -> Value {
        self.emit(Inst::new(Opcode::Identity, vec![value]))
    }

    pub fn void_inst(&mut self) {
        self.emit_void(Inst::new(Opcode::Void, vec![]));
    }

    pub fn condition_ref(&mut self, cond: Value) -> Value {
        self.emit(Inst::new(Opcode::ConditionRef, vec![cond]))
    }

    pub fn reference(&mut self, value: Value) {
        self.emit_void(Inst::new(Opcode::Reference, vec![value]));
    }

    pub fn prologue(&mut self) {
        self.emit_void(Inst::new(Opcode::Prologue, vec![]));
    }

    pub fn epilogue(&mut self) {
        self.emit_void(Inst::new(Opcode::Epilogue, vec![]));
    }

    pub fn demote_to_helper_invocation(&mut self) {
        self.emit_void(Inst::new(Opcode::DemoteToHelperInvocation, vec![]));
    }

    // ── Barriers ──────────────────────────────────────────────────────

    pub fn barrier(&mut self) {
        self.emit_void(Inst::new(Opcode::Barrier, vec![]));
    }

    pub fn workgroup_memory_barrier(&mut self) {
        self.emit_void(Inst::new(Opcode::WorkgroupMemoryBarrier, vec![]));
    }

    pub fn device_memory_barrier(&mut self) {
        self.emit_void(Inst::new(Opcode::DeviceMemoryBarrier, vec![]));
    }

    // ── Register / Predicate access ───────────────────────────────────

    pub fn get_reg(&mut self, reg: Reg) -> Value {
        self.emit(Inst::new(Opcode::GetRegister, vec![Value::Reg(reg)]))
    }

    pub fn set_reg(&mut self, reg: Reg, value: Value) {
        self.emit_void(Inst::new(Opcode::SetRegister, vec![Value::Reg(reg), value]));
    }

    pub fn get_pred(&mut self, pred: Pred, is_negated: bool) -> Value {
        let raw = self.emit(Inst::new(Opcode::GetPred, vec![Value::Pred(pred)]));
        if is_negated {
            self.logical_not(raw)
        } else {
            raw
        }
    }

    pub fn set_pred(&mut self, pred: Pred, value: Value) {
        self.emit_void(Inst::new(Opcode::SetPred, vec![Value::Pred(pred), value]));
    }

    pub fn get_goto_variable(&mut self, index: u32) -> Value {
        self.emit(Inst::new(Opcode::GetGotoVariable, vec![Value::ImmU32(index)]))
    }

    pub fn set_goto_variable(&mut self, index: u32, value: Value) {
        self.emit_void(Inst::new(Opcode::SetGotoVariable, vec![Value::ImmU32(index), value]));
    }

    // ── Constant buffer ───────────────────────────────────────────────

    pub fn get_cbuf_u32(&mut self, binding: Value, offset: Value) -> Value {
        self.emit(Inst::new(Opcode::GetCbufU32, vec![binding, offset]))
    }

    pub fn get_cbuf_f32(&mut self, binding: Value, offset: Value) -> Value {
        self.emit(Inst::new(Opcode::GetCbufF32, vec![binding, offset]))
    }

    pub fn get_cbuf_s16(&mut self, binding: Value, offset: Value) -> Value {
        self.emit(Inst::new(Opcode::GetCbufS16, vec![binding, offset]))
    }

    pub fn get_cbuf_u16(&mut self, binding: Value, offset: Value) -> Value {
        self.emit(Inst::new(Opcode::GetCbufU16, vec![binding, offset]))
    }

    pub fn get_cbuf_u8(&mut self, binding: Value, offset: Value) -> Value {
        self.emit(Inst::new(Opcode::GetCbufU8, vec![binding, offset]))
    }

    pub fn get_cbuf_s8(&mut self, binding: Value, offset: Value) -> Value {
        self.emit(Inst::new(Opcode::GetCbufS8, vec![binding, offset]))
    }

    // ── Attributes ────────────────────────────────────────────────────

    pub fn get_attribute(&mut self, attr: Attribute, vertex: Value) -> Value {
        self.emit(Inst::new(Opcode::GetAttribute, vec![Value::Attribute(attr), vertex]))
    }

    pub fn get_attribute_u32(&mut self, attr: Attribute, vertex: Value) -> Value {
        self.emit(Inst::new(Opcode::GetAttributeU32, vec![Value::Attribute(attr), vertex]))
    }

    pub fn set_attribute(&mut self, attr: Attribute, value: Value, vertex: Value) {
        self.emit_void(Inst::new(
            Opcode::SetAttribute,
            vec![Value::Attribute(attr), value, vertex],
        ));
    }

    pub fn get_attribute_indexed(&mut self, offset: Value, vertex: Value) -> Value {
        self.emit(Inst::new(Opcode::GetAttributeIndexed, vec![offset, vertex]))
    }

    pub fn set_attribute_indexed(&mut self, offset: Value, value: Value, vertex: Value) {
        self.emit_void(Inst::new(Opcode::SetAttributeIndexed, vec![offset, value, vertex]));
    }

    pub fn get_patch(&mut self, patch: Patch) -> Value {
        self.emit(Inst::new(Opcode::GetPatch, vec![Value::Patch(patch)]))
    }

    pub fn set_patch(&mut self, patch: Patch, value: Value) {
        self.emit_void(Inst::new(Opcode::SetPatch, vec![Value::Patch(patch), value]));
    }

    pub fn set_frag_color(&mut self, rt_index: Value, component: Value, value: Value) {
        self.emit_void(Inst::new(Opcode::SetFragColor, vec![rt_index, component, value]));
    }

    pub fn set_sample_mask(&mut self, value: Value) {
        self.emit_void(Inst::new(Opcode::SetSampleMask, vec![value]));
    }

    pub fn set_frag_depth(&mut self, value: Value) {
        self.emit_void(Inst::new(Opcode::SetFragDepth, vec![value]));
    }

    // ── Condition flags ───────────────────────────────────────────────

    pub fn get_zero_from_op(&mut self, op: Value) -> Value {
        self.emit(Inst::new(Opcode::GetZeroFromOp, vec![op]))
    }

    pub fn get_sign_from_op(&mut self, op: Value) -> Value {
        self.emit(Inst::new(Opcode::GetSignFromOp, vec![op]))
    }

    pub fn get_carry_from_op(&mut self, op: Value) -> Value {
        self.emit(Inst::new(Opcode::GetCarryFromOp, vec![op]))
    }

    pub fn get_overflow_from_op(&mut self, op: Value) -> Value {
        self.emit(Inst::new(Opcode::GetOverflowFromOp, vec![op]))
    }

    // ── System values ─────────────────────────────────────────────────

    pub fn workgroup_id(&mut self) -> Value {
        self.emit(Inst::new(Opcode::WorkgroupId, vec![]))
    }

    pub fn local_invocation_id(&mut self) -> Value {
        self.emit(Inst::new(Opcode::LocalInvocationId, vec![]))
    }

    pub fn invocation_id(&mut self) -> Value {
        self.emit(Inst::new(Opcode::InvocationId, vec![]))
    }

    pub fn invocation_info(&mut self) -> Value {
        self.emit(Inst::new(Opcode::InvocationInfo, vec![]))
    }

    pub fn is_helper_invocation(&mut self) -> Value {
        self.emit(Inst::new(Opcode::IsHelperInvocation, vec![]))
    }

    // ── Undefined ─────────────────────────────────────────────────────

    pub fn undef_u1(&mut self) -> Value {
        self.emit(Inst::new(Opcode::UndefU1, vec![]))
    }

    pub fn undef_u32(&mut self) -> Value {
        self.emit(Inst::new(Opcode::UndefU32, vec![]))
    }

    pub fn undef_f32(&mut self) -> Value {
        self.emit(Inst::new(Opcode::UndefU32, vec![]))
    }

    // ── FP32 arithmetic ───────────────────────────────────────────────

    pub fn fp_abs_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPAbs32, vec![a]))
    }

    pub fn fp_neg_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPNeg32, vec![a]))
    }

    pub fn fp_add_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPAdd32, vec![a, b]))
    }

    pub fn fp_add_32_with_control(&mut self, a: Value, b: Value, control: FpControl) -> Value {
        self.emit(Inst::with_flags(Opcode::FPAdd32, vec![a, b], control.to_u32()))
    }

    pub fn fp_sub_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPSub32, vec![a, b]))
    }

    pub fn fp_mul_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPMul32, vec![a, b]))
    }

    pub fn fp_mul_32_with_control(&mut self, a: Value, b: Value, control: FpControl) -> Value {
        self.emit(Inst::with_flags(Opcode::FPMul32, vec![a, b], control.to_u32()))
    }

    pub fn fp_div_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPDiv32, vec![a, b]))
    }

    pub fn fp_fma_32(&mut self, a: Value, b: Value, c: Value) -> Value {
        self.emit(Inst::new(Opcode::FPFma32, vec![a, b, c]))
    }

    pub fn fp_fma_32_with_control(
        &mut self,
        a: Value,
        b: Value,
        c: Value,
        control: FpControl,
    ) -> Value {
        self.emit(Inst::with_flags(Opcode::FPFma32, vec![a, b, c], control.to_u32()))
    }

    pub fn fp_min_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPMin32, vec![a, b]))
    }

    pub fn fp_max_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPMax32, vec![a, b]))
    }

    pub fn fp_saturate_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPSaturate32, vec![a]))
    }

    pub fn fp_clamp_32(&mut self, value: Value, min: Value, max: Value) -> Value {
        self.emit(Inst::new(Opcode::FPClamp32, vec![value, min, max]))
    }

    pub fn fp_round_even_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPRoundEven32, vec![a]))
    }

    pub fn fp_floor_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPFloor32, vec![a]))
    }

    pub fn fp_ceil_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPCeil32, vec![a]))
    }

    pub fn fp_trunc_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPTrunc32, vec![a]))
    }

    pub fn fp_recip_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPRecip32, vec![a]))
    }

    pub fn fp_recip_sqrt_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPRecipSqrt32, vec![a]))
    }

    pub fn fp_sqrt_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPSqrt32, vec![a]))
    }

    pub fn fp_sin(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPSin, vec![a]))
    }

    pub fn fp_cos(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPCos, vec![a]))
    }

    pub fn fp_exp2(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPExp2, vec![a]))
    }

    pub fn fp_log2(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPLog2, vec![a]))
    }

    // ── FP32 comparison ───────────────────────────────────────────────

    pub fn fp_ord_equal_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPOrdEqual32, vec![a, b]))
    }

    pub fn fp_ord_not_equal_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPOrdNotEqual32, vec![a, b]))
    }

    pub fn fp_ord_less_than_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPOrdLessThan32, vec![a, b]))
    }

    pub fn fp_ord_greater_than_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPOrdGreaterThan32, vec![a, b]))
    }

    pub fn fp_ord_less_than_equal_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPOrdLessThanEqual32, vec![a, b]))
    }

    pub fn fp_ord_greater_than_equal_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPOrdGreaterThanEqual32, vec![a, b]))
    }

    pub fn fp_unord_equal_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPUnordEqual32, vec![a, b]))
    }

    pub fn fp_unord_not_equal_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPUnordNotEqual32, vec![a, b]))
    }

    pub fn fp_unord_less_than_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPUnordLessThan32, vec![a, b]))
    }

    pub fn fp_unord_greater_than_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPUnordGreaterThan32, vec![a, b]))
    }

    pub fn fp_is_nan_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPIsNan32, vec![a]))
    }

    // ── FP64 arithmetic ───────────────────────────────────────────────

    pub fn fp_add_64(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPAdd64, vec![a, b]))
    }

    pub fn fp_mul_64(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPMul64, vec![a, b]))
    }

    pub fn fp_fma_64(&mut self, a: Value, b: Value, c: Value) -> Value {
        self.emit(Inst::new(Opcode::FPFma64, vec![a, b, c]))
    }

    pub fn fp_min_64(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPMin64, vec![a, b]))
    }

    pub fn fp_max_64(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::FPMax64, vec![a, b]))
    }

    pub fn fp_neg_64(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPNeg64, vec![a]))
    }

    pub fn fp_abs_64(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FPAbs64, vec![a]))
    }

    // ── Integer arithmetic ────────────────────────────────────────────

    pub fn iadd_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::IAdd32, vec![a, b]))
    }

    pub fn iadd_64(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::IAdd64, vec![a, b]))
    }

    pub fn isub_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::ISub32, vec![a, b]))
    }

    pub fn imul_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::IMul32, vec![a, b]))
    }

    pub fn ineg_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::INeg32, vec![a]))
    }

    pub fn iabs_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::IAbs32, vec![a]))
    }

    pub fn shift_left_logical_32(&mut self, base: Value, shift: Value) -> Value {
        self.emit(Inst::new(Opcode::ShiftLeftLogical32, vec![base, shift]))
    }

    pub fn shift_right_logical_32(&mut self, base: Value, shift: Value) -> Value {
        self.emit(Inst::new(Opcode::ShiftRightLogical32, vec![base, shift]))
    }

    pub fn shift_right_arithmetic_32(&mut self, base: Value, shift: Value) -> Value {
        self.emit(Inst::new(Opcode::ShiftRightArithmetic32, vec![base, shift]))
    }

    pub fn bitwise_and_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::BitwiseAnd32, vec![a, b]))
    }

    pub fn bitwise_or_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::BitwiseOr32, vec![a, b]))
    }

    pub fn bitwise_xor_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::BitwiseXor32, vec![a, b]))
    }

    pub fn bitwise_not_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::BitwiseNot32, vec![a]))
    }

    pub fn bit_field_insert(&mut self, base: Value, insert: Value, offset: Value, count: Value) -> Value {
        self.emit(Inst::new(Opcode::BitFieldInsert, vec![base, insert, offset, count]))
    }

    pub fn bit_field_s_extract(&mut self, base: Value, offset: Value, count: Value) -> Value {
        self.emit(Inst::new(Opcode::BitFieldSExtract, vec![base, offset, count]))
    }

    pub fn bit_field_u_extract(&mut self, base: Value, offset: Value, count: Value) -> Value {
        self.emit(Inst::new(Opcode::BitFieldUExtract, vec![base, offset, count]))
    }

    pub fn bit_reverse_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::BitReverse32, vec![a]))
    }

    pub fn bit_count_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::BitCount32, vec![a]))
    }

    pub fn find_s_msb_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FindSMsb32, vec![a]))
    }

    pub fn find_u_msb_32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::FindUMsb32, vec![a]))
    }

    pub fn s_min_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::SMin32, vec![a, b]))
    }

    pub fn u_min_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::UMin32, vec![a, b]))
    }

    pub fn s_max_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::SMax32, vec![a, b]))
    }

    pub fn u_max_32(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::UMax32, vec![a, b]))
    }

    // ── Integer comparison ────────────────────────────────────────────

    pub fn i_equal(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::IEqual, vec![a, b]))
    }

    pub fn i_not_equal(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::INotEqual, vec![a, b]))
    }

    pub fn s_less_than(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::SLessThan, vec![a, b]))
    }

    pub fn u_less_than(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::ULessThan, vec![a, b]))
    }

    pub fn s_less_than_equal(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::SLessThanEqual, vec![a, b]))
    }

    pub fn u_less_than_equal(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::ULessThanEqual, vec![a, b]))
    }

    pub fn s_greater_than(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::SGreaterThan, vec![a, b]))
    }

    pub fn u_greater_than(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::UGreaterThan, vec![a, b]))
    }

    pub fn s_greater_than_equal(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::SGreaterThanEqual, vec![a, b]))
    }

    pub fn u_greater_than_equal(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::UGreaterThanEqual, vec![a, b]))
    }

    // ── Logic ─────────────────────────────────────────────────────────

    pub fn logical_or(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::LogicalOr, vec![a, b]))
    }

    pub fn logical_and(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::LogicalAnd, vec![a, b]))
    }

    pub fn logical_xor(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::LogicalXor, vec![a, b]))
    }

    pub fn logical_not(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::LogicalNot, vec![a]))
    }

    // ── Select ────────────────────────────────────────────────────────

    pub fn select_u32(&mut self, cond: Value, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::SelectU32, vec![cond, a, b]))
    }

    pub fn select_f32(&mut self, cond: Value, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::SelectF32, vec![cond, a, b]))
    }

    pub fn select_u1(&mut self, cond: Value, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::SelectU1, vec![cond, a, b]))
    }

    // ── Bitcast ───────────────────────────────────────────────────────

    pub fn bit_cast_u32_f32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::BitCastU32F32, vec![a]))
    }

    pub fn bit_cast_f32_u32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::BitCastF32U32, vec![a]))
    }

    pub fn bit_cast_u64_f64(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::BitCastU64F64, vec![a]))
    }

    pub fn bit_cast_f64_u64(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::BitCastF64U64, vec![a]))
    }

    pub fn pack_half_2x16(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::PackHalf2x16, vec![a]))
    }

    pub fn unpack_half_2x16(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::UnpackHalf2x16, vec![a]))
    }

    // ── Conversion ────────────────────────────────────────────────────

    pub fn convert_f32_from_s32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::ConvertF32S32, vec![a]))
    }

    pub fn convert_f32_from_u32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::ConvertF32U32, vec![a]))
    }

    pub fn convert_s32_from_f32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::ConvertS32F32, vec![a]))
    }

    pub fn convert_u32_from_f32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::ConvertU32F32, vec![a]))
    }

    pub fn convert_f16_from_f32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::ConvertF16F32, vec![a]))
    }

    pub fn convert_f32_from_f16(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::ConvertF32F16, vec![a]))
    }

    pub fn convert_f64_from_f32(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::ConvertF64F32, vec![a]))
    }

    pub fn convert_f32_from_f64(&mut self, a: Value) -> Value {
        self.emit(Inst::new(Opcode::ConvertF32F64, vec![a]))
    }

    // ── Composite (vector) ────────────────────────────────────────────

    pub fn composite_construct_u32x2(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::CompositeConstructU32x2, vec![a, b]))
    }

    pub fn composite_construct_u32x4(
        &mut self,
        a: Value,
        b: Value,
        c: Value,
        d: Value,
    ) -> Value {
        self.emit(Inst::new(Opcode::CompositeConstructU32x4, vec![a, b, c, d]))
    }

    pub fn composite_extract_u32x2(&mut self, vector: Value, index: Value) -> Value {
        self.emit(Inst::new(Opcode::CompositeExtractU32x2, vec![vector, index]))
    }

    pub fn composite_extract_u32x4(&mut self, vector: Value, index: Value) -> Value {
        self.emit(Inst::new(Opcode::CompositeExtractU32x4, vec![vector, index]))
    }

    pub fn composite_construct_f32x2(&mut self, a: Value, b: Value) -> Value {
        self.emit(Inst::new(Opcode::CompositeConstructF32x2, vec![a, b]))
    }

    pub fn composite_construct_f32x4(
        &mut self,
        a: Value,
        b: Value,
        c: Value,
        d: Value,
    ) -> Value {
        self.emit(Inst::new(Opcode::CompositeConstructF32x4, vec![a, b, c, d]))
    }

    pub fn composite_extract_f32x4(&mut self, vector: Value, index: Value) -> Value {
        self.emit(Inst::new(Opcode::CompositeExtractF32x4, vec![vector, index]))
    }

    // ── Memory ────────────────────────────────────────────────────────

    pub fn load_global_32(&mut self, addr: Value) -> Value {
        self.emit(Inst::new(Opcode::LoadGlobal32, vec![addr]))
    }

    pub fn write_global_32(&mut self, addr: Value, value: Value) {
        self.emit_void(Inst::new(Opcode::WriteGlobal32, vec![addr, value]));
    }

    pub fn load_local(&mut self, offset: Value) -> Value {
        self.emit(Inst::new(Opcode::LoadLocal, vec![offset]))
    }

    pub fn write_local(&mut self, offset: Value, value: Value) {
        self.emit_void(Inst::new(Opcode::WriteLocal, vec![offset, value]));
    }

    pub fn load_storage_32(&mut self, binding: Value, offset: Value) -> Value {
        self.emit(Inst::new(Opcode::LoadStorage32, vec![binding, offset]))
    }

    pub fn write_storage_32(&mut self, binding: Value, offset: Value, value: Value) {
        self.emit_void(Inst::new(Opcode::WriteStorage32, vec![binding, offset, value]));
    }

    // ── Texture ───────────────────────────────────────────────────────

    pub fn image_sample_implicit_lod(
        &mut self,
        handle: Value,
        coords: Value,
        info: u32,
    ) -> Value {
        self.emit(Inst::with_flags(
            Opcode::ImageSampleImplicitLod,
            vec![handle, coords],
            info,
        ))
    }

    pub fn image_sample_explicit_lod(
        &mut self,
        handle: Value,
        coords: Value,
        lod: Value,
        info: u32,
    ) -> Value {
        self.emit(Inst::with_flags(
            Opcode::ImageSampleExplicitLod,
            vec![handle, coords, lod],
            info,
        ))
    }

    pub fn image_fetch(
        &mut self,
        handle: Value,
        coords: Value,
        lod: Value,
        info: u32,
    ) -> Value {
        self.emit(Inst::with_flags(
            Opcode::ImageFetch,
            vec![handle, coords, lod],
            info,
        ))
    }

    pub fn image_query_dimensions(&mut self, handle: Value, lod: Value, info: u32) -> Value {
        self.emit(Inst::with_flags(
            Opcode::ImageQueryDimensions,
            vec![handle, lod],
            info,
        ))
    }

    pub fn image_gather(
        &mut self,
        handle: Value,
        coords: Value,
        info: u32,
    ) -> Value {
        self.emit(Inst::with_flags(
            Opcode::ImageGather,
            vec![handle, coords],
            info,
        ))
    }

    pub fn image_gather_dref(
        &mut self,
        handle: Value,
        coords: Value,
        dref: Value,
        info: u32,
    ) -> Value {
        self.emit(Inst::with_flags(
            Opcode::ImageGatherDref,
            vec![handle, coords, dref],
            info,
        ))
    }

    // ── Warp / Subgroup ───────────────────────────────────────────────

    pub fn vote_all(&mut self, pred: Value) -> Value {
        self.emit(Inst::new(Opcode::VoteAll, vec![pred]))
    }

    pub fn vote_any(&mut self, pred: Value) -> Value {
        self.emit(Inst::new(Opcode::VoteAny, vec![pred]))
    }

    pub fn vote_equal(&mut self, pred: Value) -> Value {
        self.emit(Inst::new(Opcode::VoteEqual, vec![pred]))
    }

    pub fn subgroup_ballot(&mut self, pred: Value) -> Value {
        self.emit(Inst::new(Opcode::SubgroupBallot, vec![pred]))
    }

    pub fn shuffle_index(&mut self, value: Value, index: Value, seg_mask: Value) -> Value {
        self.emit(Inst::new(Opcode::ShuffleIndex, vec![value, index, seg_mask]))
    }

    // ── Geometry ──────────────────────────────────────────────────────

    pub fn emit_vertex(&mut self, stream: Value) -> Value {
        self.emit(Inst::new(Opcode::EmitVertex, vec![stream]))
    }

    pub fn end_primitive(&mut self, stream: Value) -> Value {
        self.emit(Inst::new(Opcode::EndPrimitive, vec![stream]))
    }

    // ── System values ────────────────────────────────────────────────

    pub fn sample_id(&mut self) -> Value {
        self.emit(Inst::new(Opcode::SampleId, vec![]))
    }

    // ── Helper: apply abs/neg modifiers ───────────────────────────────

    /// Apply floating-point absolute value and negation modifiers.
    pub fn fp_abs_neg_32(&mut self, value: Value, abs: bool, neg: bool) -> Value {
        let mut result = value;
        if abs {
            result = self.fp_abs_32(result);
        }
        if neg {
            result = self.fp_neg_32(result);
        }
        result
    }
}
