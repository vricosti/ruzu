// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/half_floating_point_multiply.cpp

use super::half_floating_point_helper::{
    extract, half_precision_to_fmz_mode, merge_result, HalfPrecision, Merge, Swizzle,
};
use super::{bit, field, TranslatorVisitor};
use crate::ir::types::{FpControl, FpRounding};
use crate::ir::value::Value;

/// Core HMUL2 implementation (inner `HMUL2` overloads from upstream).
fn hmul2_inner(
    tv: &mut TranslatorVisitor,
    insn: u64,
    merge: Merge,
    sat: bool,
    abs_a: bool,
    neg_a: bool,
    swizzle_a: Swizzle,
    abs_b: bool,
    neg_b: bool,
    swizzle_b: Swizzle,
    src_b: Value,
    precision: HalfPrecision,
) {
    let dest_reg = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);
    let src_a_val = tv.x(src_a_reg);

    let (mut lhs_a, mut rhs_a) = extract(tv, src_a_val, swizzle_a);
    let (mut lhs_b, mut rhs_b) = extract(tv, src_b, swizzle_b);

    let a_is_f32 = swizzle_a == Swizzle::F32;
    let b_is_f32 = swizzle_b == Swizzle::F32;
    let promotion = a_is_f32 != b_is_f32;
    if promotion {
        if !a_is_f32 {
            lhs_a = tv.ir.convert_f32_from_f16(lhs_a);
            rhs_a = tv.ir.convert_f32_from_f16(rhs_a);
        }
        if !b_is_f32 {
            lhs_b = tv.ir.convert_f32_from_f16(lhs_b);
            rhs_b = tv.ir.convert_f32_from_f16(rhs_b);
        }
    }

    let use_f32 = a_is_f32 || promotion;

    if use_f32 {
        lhs_a = tv.ir.fp_abs_neg_32(lhs_a, abs_a, neg_a);
        rhs_a = tv.ir.fp_abs_neg_32(rhs_a, abs_a, neg_a);
        lhs_b = tv.ir.fp_abs_neg_32(lhs_b, abs_b, neg_b);
        rhs_b = tv.ir.fp_abs_neg_32(rhs_b, abs_b, neg_b);
    } else {
        lhs_a = tv.ir.fp_abs_neg_16(lhs_a, abs_a, neg_a);
        rhs_a = tv.ir.fp_abs_neg_16(rhs_a, abs_a, neg_a);
        lhs_b = tv.ir.fp_abs_neg_16(lhs_b, abs_b, neg_b);
        rhs_b = tv.ir.fp_abs_neg_16(rhs_b, abs_b, neg_b);
    }

    let fp_control = FpControl {
        no_contraction: true,
        rounding: FpRounding::DontCare,
        fmz_mode: half_precision_to_fmz_mode(precision),
    };
    let (mut lhs, mut rhs) = if use_f32 {
        (
            tv.ir.fp_mul_32_with_control(lhs_a, lhs_b, fp_control),
            tv.ir.fp_mul_32_with_control(rhs_a, rhs_b, fp_control),
        )
    } else {
        (
            tv.ir.fp_mul_16_with_control(lhs_a, lhs_b, fp_control),
            tv.ir.fp_mul_16_with_control(rhs_a, rhs_b, fp_control),
        )
    };

    // FMZ mode: if enabled and not saturating, replace result with zero when either operand is zero.
    // Value is Copy so operands can be re-read after the multiply.
    if precision == HalfPrecision::Fmz && !sat {
        if use_f32 {
            let zero = Value::ImmF32(0.0);
            let lhs_zero_a = tv.ir.fp_ord_equal_32(lhs_a, zero);
            let lhs_zero_b = tv.ir.fp_ord_equal_32(lhs_b, zero);
            let lhs_any_zero = tv.ir.logical_or(lhs_zero_a, lhs_zero_b);
            lhs = tv.ir.select_f32(lhs_any_zero, zero, lhs);

            let rhs_zero_a = tv.ir.fp_ord_equal_32(rhs_a, zero);
            let rhs_zero_b = tv.ir.fp_ord_equal_32(rhs_b, zero);
            let rhs_any_zero = tv.ir.logical_or(rhs_zero_a, rhs_zero_b);
            rhs = tv.ir.select_f32(rhs_any_zero, zero, rhs);
        } else {
            let zero = Value::ImmF16(0);
            let lhs_zero_a = tv.ir.fp_ord_equal_16(lhs_a, zero);
            let lhs_zero_b = tv.ir.fp_ord_equal_16(lhs_b, zero);
            let lhs_any_zero = tv.ir.logical_or(lhs_zero_a, lhs_zero_b);
            lhs = tv.ir.select_f16(lhs_any_zero, zero, lhs);

            let rhs_zero_a = tv.ir.fp_ord_equal_16(rhs_a, zero);
            let rhs_zero_b = tv.ir.fp_ord_equal_16(rhs_b, zero);
            let rhs_any_zero = tv.ir.logical_or(rhs_zero_a, rhs_zero_b);
            rhs = tv.ir.select_f16(rhs_any_zero, zero, rhs);
        }
    }

    if sat {
        if use_f32 {
            lhs = tv.ir.fp_saturate_32(lhs);
            rhs = tv.ir.fp_saturate_32(rhs);
        } else {
            lhs = tv.ir.fp_saturate_16(lhs);
            rhs = tv.ir.fp_saturate_16(rhs);
        }
    }

    if promotion {
        lhs = tv.ir.convert_f16_from_f32(lhs);
        rhs = tv.ir.convert_f16_from_f32(rhs);
    }

    let result = merge_result(tv, dest_reg, lhs, rhs, merge, use_f32 && !promotion);
    tv.set_x(dest_reg, result);
}

/// HMUL2_reg — source B from register.
pub fn hmul2_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let sat = bit(insn, 32);
    let neg_b = bit(insn, 31);
    let abs_b = bit(insn, 30);
    let abs_a = bit(insn, 44);
    let swizzle_b = Swizzle::from_u32(field(insn, 28, 2));
    let merge = Merge::from_u32(field(insn, 49, 2));
    let swizzle_a = Swizzle::from_u32(field(insn, 47, 2));
    let precision = HalfPrecision::from_u32(field(insn, 39, 2));
    let src_b = tv.get_reg20(insn);
    hmul2_inner(
        tv, insn, merge, sat, abs_a, false, swizzle_a, abs_b, neg_b, swizzle_b, src_b, precision,
    );
}

/// HMUL2_cbuf — source B from constant buffer.
pub fn hmul2_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let sat = bit(insn, 52);
    let abs_b = bit(insn, 54);
    let neg_a = bit(insn, 43);
    let abs_a = bit(insn, 44);
    let merge = Merge::from_u32(field(insn, 49, 2));
    let swizzle_a = Swizzle::from_u32(field(insn, 47, 2));
    let precision = HalfPrecision::from_u32(field(insn, 39, 2));
    let src_b = tv.get_cbuf(insn);
    hmul2_inner(
        tv,
        insn,
        merge,
        sat,
        abs_a,
        neg_a,
        swizzle_a,
        abs_b,
        false,
        Swizzle::F32,
        src_b,
        precision,
    );
}

/// HMUL2_imm — source B from 16-bit immediate pair.
pub fn hmul2_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let sat = bit(insn, 52);
    let neg_high = bit(insn, 56);
    let high = field(insn, 30, 9);
    let neg_low = bit(insn, 29);
    let low = field(insn, 20, 9);
    let neg_a = bit(insn, 43);
    let abs_a = bit(insn, 44);
    let imm: u32 = (low << 6)
        | (if neg_low { 1u32 } else { 0u32 } << 15)
        | (high << 22)
        | (if neg_high { 1u32 } else { 0u32 } << 31);
    let merge = Merge::from_u32(field(insn, 49, 2));
    let swizzle_a = Swizzle::from_u32(field(insn, 47, 2));
    let precision = HalfPrecision::from_u32(field(insn, 39, 2));
    let src_b = Value::ImmU32(imm);
    hmul2_inner(
        tv,
        insn,
        merge,
        sat,
        abs_a,
        neg_a,
        swizzle_a,
        false,
        false,
        Swizzle::H1H0,
        src_b,
        precision,
    );
}

/// HMUL2_32I — source B from 32-bit immediate.
pub fn hmul2_32i(tv: &mut TranslatorVisitor, insn: u64) {
    let precision = HalfPrecision::from_u32(field(insn, 55, 2));
    let sat = bit(insn, 52);
    let swizzle_a = Swizzle::from_u32(field(insn, 53, 2));
    let imm: u32 = field(insn, 20, 32);
    let src_b = Value::ImmU32(imm);
    hmul2_inner(
        tv,
        insn,
        Merge::H1H0,
        sat,
        false,
        false,
        swizzle_a,
        false,
        false,
        Swizzle::H1H0,
        src_b,
        precision,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::{FmzMode, FpControl, ShaderStage};

    #[test]
    fn hmul2_f16_preserves_upstream_fp_control() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let mut visitor = TranslatorVisitor::new(&mut program, 0);
        let insn = 1u64 | 2u64 << 8 | 3u64 << 20 | 1u64 << 39;

        hmul2_reg(&mut visitor, insn);

        let multiply = visitor.ir.program.blocks[0]
            .iter()
            .find(|inst| inst.opcode == Opcode::FPMul16)
            .expect("HMUL2 must emit an F16 multiply");
        let control = FpControl::from_u32(multiply.flags);
        assert!(control.no_contraction);
        assert_eq!(control.fmz_mode, FmzMode::FTZ);
    }
}
