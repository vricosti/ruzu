// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_conversion_floating_point.cpp
//!
//! Translates the F2F (float-to-float) Maxwell instruction family across
//! the 3×3 source/destination width matrix (F16, F32, F64), with FTZ,
//! NaN-preserving Pass via FPAdd-by-zero, F16 selector / packing, and
//! `FpControl` plumbing through `fp_convert`.
//!
//! Upstream split into three entry points (F2F_reg / F2F_cbuf / F2F_imm)
//! that decode the source operand per `src_size`, then call the inner
//! `F2F` helper. This port mirrors that structure.

use super::common_encoding::{cast_fp_rounding, MaxwellFpRounding};
use super::half_floating_point_helper::{extract, Swizzle};
use super::TranslatorVisitor;
use super::{bit, field};
use crate::ir::types::{FmzMode, FpControl, FpRounding};
use crate::ir::value::Value;

/// Maxwell `FloatFormat` field — 2-bit width selector in F2F.
///
/// Port of upstream `FloatFormat` (anonymous-namespace enum in F2F.cpp).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FloatFormat {
    F16 = 1,
    F32 = 2,
    F64 = 3,
}

impl FloatFormat {
    fn from_field(value: u32) -> Self {
        match value & 0x3 {
            1 => Self::F16,
            2 => Self::F32,
            3 => Self::F64,
            other => panic!("Invalid width {}", other),
        }
    }

    /// Port of upstream `WidthSize(FloatFormat)`.
    fn width_bits(self) -> u32 {
        match self {
            Self::F16 => 16,
            Self::F32 => 32,
            Self::F64 => 64,
        }
    }
}

/// Maxwell `RoundingOp` field — 4-bit rounding selector in F2F, masked to
/// 0x0B in `RoundingOperation()` upstream.
///
/// Port of upstream `RoundingOp` (anonymous-namespace enum in F2F.cpp).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RoundingOp {
    None = 0,
    Pass = 3,
    Round = 8,
    Floor = 9,
    Ceil = 10,
    Trunc = 11,
}

impl RoundingOp {
    /// Mirror upstream's `f2f.RoundingOperation()` mask.
    fn from_field(rounding_op_field: u32) -> Self {
        match rounding_op_field & 0x0B {
            0 => Self::None,
            3 => Self::Pass,
            8 => Self::Round,
            9 => Self::Floor,
            10 => Self::Ceil,
            11 => Self::Trunc,
            other => panic!("Unimplemented rounding mode {}", other),
        }
    }
}

/// Inner F2F body, mirroring upstream's anonymous-namespace
/// `void F2F(TranslatorVisitor& v, u64 insn, const IR::F16F32F64& src_a, bool abs)`.
fn f2f_impl(tv: &mut TranslatorVisitor, insn: u64, src_a: Value, abs: bool) {
    let dest_reg = tv.dst_reg(insn);
    let ftz = bit(insn, 44);
    let neg = bit(insn, 45);
    let cc = bit(insn, 47);
    let sat = bit(insn, 50);
    let rounding_op_field = field(insn, 39, 4);
    let rounding_field = field(insn, 39, 2);
    let src_size = FloatFormat::from_field(field(insn, 10, 2) as u32);
    let dst_size = FloatFormat::from_field(field(insn, 8, 2) as u32);

    if cc {
        // Upstream: throw NotImplementedException("F2F CC")
        panic!("F2F CC not implemented");
    }

    // Apply abs/neg using the width-typed helper matching `src_size`.
    let mut input = match src_size {
        FloatFormat::F16 => tv.ir.fp_abs_neg_16(src_a, abs, neg),
        FloatFormat::F32 => tv.ir.fp_abs_neg_32(src_a, abs, neg),
        FloatFormat::F64 => tv.ir.fp_abs_neg_64(src_a, abs, neg),
    };

    let any_fp64 = src_size == FloatFormat::F64 || dst_size == FloatFormat::F64;
    let mut fp_control = FpControl {
        no_contraction: false,
        rounding: FpRounding::DontCare,
        fmz_mode: if ftz && !any_fp64 {
            FmzMode::FTZ
        } else {
            FmzMode::None
        },
    };

    if src_size != dst_size {
        // Width-change path: thread the rounding mode through `FpControl`
        // and dispatch via the polymorphic `fp_convert`.
        fp_control.rounding =
            cast_fp_rounding(MaxwellFpRounding::from_field(rounding_field as u32));
        input = tv.ir.fp_convert(
            dst_size.width_bits(),
            input,
            src_size.width_bits(),
            fp_control,
        );
    } else {
        // Same-width path: select the rounding op and emit the matching
        // width-typed helper. `None`/`Pass` add zero so NaNs are
        // preserved exactly as upstream does.
        match RoundingOp::from_field(rounding_op_field as u32) {
            RoundingOp::None | RoundingOp::Pass => {
                input = match src_size {
                    FloatFormat::F16 => {
                        let zero =
                            tv.ir
                                .fp_convert(16, Value::ImmF32(0.0), 32, FpControl::default());
                        tv.ir.fp_add_16_with_control(input, zero, fp_control)
                    }
                    FloatFormat::F32 => {
                        tv.ir
                            .fp_add_32_with_control(input, Value::ImmF32(0.0), fp_control)
                    }
                    FloatFormat::F64 => {
                        tv.ir
                            .fp_add_64_with_control(input, Value::ImmF64(0.0), fp_control)
                    }
                };
            }
            RoundingOp::Round => {
                input = match src_size {
                    FloatFormat::F16 => tv.ir.fp_round_even_16_with_control(input, fp_control),
                    FloatFormat::F32 => tv.ir.fp_round_even_32_with_control(input, fp_control),
                    FloatFormat::F64 => tv.ir.fp_round_even_64_with_control(input, fp_control),
                };
            }
            RoundingOp::Floor => {
                input = match src_size {
                    FloatFormat::F16 => tv.ir.fp_floor_16_with_control(input, fp_control),
                    FloatFormat::F32 => tv.ir.fp_floor_32_with_control(input, fp_control),
                    FloatFormat::F64 => tv.ir.fp_floor_64_with_control(input, fp_control),
                };
            }
            RoundingOp::Ceil => {
                input = match src_size {
                    FloatFormat::F16 => tv.ir.fp_ceil_16_with_control(input, fp_control),
                    FloatFormat::F32 => tv.ir.fp_ceil_32_with_control(input, fp_control),
                    FloatFormat::F64 => tv.ir.fp_ceil_64_with_control(input, fp_control),
                };
            }
            RoundingOp::Trunc => {
                input = match src_size {
                    FloatFormat::F16 => tv.ir.fp_trunc_16_with_control(input, fp_control),
                    FloatFormat::F32 => tv.ir.fp_trunc_32_with_control(input, fp_control),
                    FloatFormat::F64 => tv.ir.fp_trunc_64_with_control(input, fp_control),
                };
            }
        }
    }

    if sat && !any_fp64 {
        // After convert/rounding, `input` has the `dst_size` width.
        input = match dst_size {
            FloatFormat::F16 => tv.ir.fp_saturate_16(input),
            FloatFormat::F32 => tv.ir.fp_saturate_32(input),
            FloatFormat::F64 => unreachable!("any_fp64 guard would have skipped saturate"),
        };
    }

    match dst_size {
        FloatFormat::F16 => {
            // Upstream: PackFloat2x16(CompositeConstruct(input, imm_zero_h))
            // where imm_zero_h = FPConvert(16, Imm32(0.0f)). The zero
            // half-float occupies the high lane of the packed u32.
            let zero_h = tv
                .ir
                .fp_convert(16, Value::ImmF32(0.0), 32, FpControl::default());
            let vec = tv.ir.composite_construct_f16x2(input, zero_h);
            let packed = tv.ir.pack_float_2x16(vec);
            tv.set_x(dest_reg, packed);
        }
        FloatFormat::F32 => tv.set_f(dest_reg, input),
        FloatFormat::F64 => tv.set_d(dest_reg, input),
    }
}

/// Decode the F2F source operand from a register pair, then dispatch
/// `f2f_impl`. Port of upstream `TranslatorVisitor::F2F_reg`.
pub fn f2f_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let abs = bit(insn, 49);
    let selector = bit(insn, 41);
    let src_size = FloatFormat::from_field(field(insn, 10, 2) as u32);

    let src_a = match src_size {
        FloatFormat::F16 => {
            let packed = tv.get_reg20(insn);
            let (lhs, rhs) = extract(tv, packed, Swizzle::H1H0);
            if selector {
                rhs
            } else {
                lhs
            }
        }
        FloatFormat::F32 => {
            let idx = field(insn, 20, 8);
            tv.f(idx)
        }
        FloatFormat::F64 => tv.get_double_reg20(insn),
    };
    f2f_impl(tv, insn, src_a, abs);
}

/// Decode the F2F source operand from a constant-buffer slot, then
/// dispatch `f2f_impl`. Port of upstream `TranslatorVisitor::F2F_cbuf`.
pub fn f2f_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let abs = bit(insn, 49);
    let selector = bit(insn, 41);
    let src_size = FloatFormat::from_field(field(insn, 10, 2) as u32);

    let src_a = match src_size {
        FloatFormat::F16 => {
            let packed = tv.get_cbuf(insn);
            let (lhs, rhs) = extract(tv, packed, Swizzle::H1H0);
            if selector {
                rhs
            } else {
                lhs
            }
        }
        FloatFormat::F32 => tv.get_float_cbuf(insn),
        FloatFormat::F64 => tv.get_double_cbuf(insn),
    };
    f2f_impl(tv, insn, src_a, abs);
}

/// Decode the F2F source operand from a 19-bit immediate, then dispatch
/// `f2f_impl`. Port of upstream `TranslatorVisitor::F2F_imm`.
pub fn f2f_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let abs = bit(insn, 49);
    let selector = bit(insn, 41);
    let imm_neg = bit(insn, 56);
    let src_size = FloatFormat::from_field(field(insn, 10, 2) as u32);

    let src_a = match src_size {
        FloatFormat::F16 => {
            // Upstream: imm is the low 16 bits of the 19-bit field. The
            // value is duplicated into both halves of a u32, then
            // unpacked and the per-`selector` half is extracted.
            // Selector inversion (`f2f.selector != 0 ? 0 : 1`) is
            // preserved verbatim from upstream.
            if imm_neg {
                panic!("Neg bit on F16 immediate not implemented");
            }
            let imm16 = (field(insn, 20, 19) & 0xFFFF) as u32;
            let packed = Value::ImmU32(imm16 | (imm16 << 16));
            let vec = tv.ir.unpack_float_2x16(packed);
            let index = if selector { 0 } else { 1 };
            tv.ir.composite_extract_f16x2(vec, index)
        }
        FloatFormat::F32 => tv.get_float_imm20(insn),
        FloatFormat::F64 => tv.get_double_imm20(insn),
    };
    f2f_impl(tv, insn, src_a, abs);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::maxwell_opcodes::MaxwellOpcode;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    fn fresh_program() -> Program {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program
    }

    fn opcodes_emitted(program: &Program) -> Vec<Opcode> {
        program.blocks[0].iter().map(|inst| inst.opcode).collect()
    }

    /// Encode the F2F bitfields shared by all 3 entry points.
    fn encode(src_size: u32, dst_size: u32, rounding_op_field: u64) -> u64 {
        // Place a dummy non-zero dest reg (bits 0..8) so set_x has somewhere
        // to write; harmless because the test only inspects emitted opcodes.
        1u64 | ((src_size as u64) << 10) | ((dst_size as u64) << 8) | (rounding_op_field << 39)
    }

    #[test]
    fn f2f_pass_rounding_does_not_truncate() {
        // F32→F32 with Pass — must NOT emit any rounding op; should emit
        // an FPAdd32 with the zero immediate for NaN preservation.
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = encode(2, 2, 3) | (2u64 << 20);
        f2f_reg(&mut tv, insn);
        let opcodes = opcodes_emitted(&program);
        assert!(!opcodes.contains(&Opcode::FPTrunc32));
        assert!(!opcodes.contains(&Opcode::FPFloor32));
        assert!(!opcodes.contains(&Opcode::FPCeil32));
        assert!(!opcodes.contains(&Opcode::FPRoundEven32));
    }

    #[test]
    fn f2f_pass_same_width_emits_add_zero_for_nan_preservation() {
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = encode(2, 2, 3) | (2u64 << 20);
        f2f_reg(&mut tv, insn);
        let opcodes = opcodes_emitted(&program);
        assert!(opcodes.contains(&Opcode::FPAdd32));
    }

    #[test]
    fn f2f_f16_to_f32_emits_convert_with_control() {
        // F16→F32 with default rounding — must emit ConvertF32F16.
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = encode(1, 2, 0) | (2u64 << 20);
        f2f_reg(&mut tv, insn);
        let opcodes = opcodes_emitted(&program);
        assert!(opcodes.contains(&Opcode::ConvertF32F16));
        // Source was F16, so an Unpack should appear (via extract H1_H0).
        assert!(opcodes.contains(&Opcode::UnpackFloat2x16));
    }

    #[test]
    fn f2f_f32_to_f64_emits_convert_with_control() {
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = encode(2, 3, 0) | (2u64 << 20);
        f2f_reg(&mut tv, insn);
        let opcodes = opcodes_emitted(&program);
        assert!(opcodes.contains(&Opcode::ConvertF64F32));
    }

    #[test]
    fn f2f_f64_to_f32_emits_convert_with_control() {
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = encode(3, 2, 0) | (2u64 << 20);
        f2f_reg(&mut tv, insn);
        let opcodes = opcodes_emitted(&program);
        assert!(opcodes.contains(&Opcode::ConvertF32F64));
    }

    #[test]
    fn f2f_f16_dst_packs_with_zero_half() {
        // F32→F16 — must emit ConvertF16F32 (for the result) AND
        // CompositeConstructF16x2 + PackFloat2x16 for destination packing
        // with a zero half (so the high lane is FPConvert(16, Imm32(0))).
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = encode(2, 1, 0) | (2u64 << 20);
        f2f_reg(&mut tv, insn);
        let opcodes = opcodes_emitted(&program);
        assert!(opcodes.contains(&Opcode::ConvertF16F32));
        assert!(opcodes.contains(&Opcode::CompositeConstructF16x2));
        assert!(opcodes.contains(&Opcode::PackFloat2x16));
    }

    #[test]
    fn f2f_saturate_skipped_when_any_fp64() {
        // F32→F64 with sat=1 — saturate must be SKIPPED (any_fp64==true).
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = encode(2, 3, 0) | (1u64 << 50) | (2u64 << 20);
        f2f_reg(&mut tv, insn);
        let opcodes = opcodes_emitted(&program);
        assert!(!opcodes.contains(&Opcode::FPSaturate32));
        assert!(!opcodes.contains(&Opcode::FPSaturate16));
        assert!(!opcodes.contains(&Opcode::FPSaturate64));
    }

    #[test]
    fn f2f_saturate_emitted_for_f32_when_no_fp64() {
        // F32→F32 with sat=1 + Pass — saturate must be emitted.
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = encode(2, 2, 3) | (1u64 << 50) | (2u64 << 20);
        f2f_reg(&mut tv, insn);
        let opcodes = opcodes_emitted(&program);
        assert!(opcodes.contains(&Opcode::FPSaturate32));
    }

    #[test]
    fn f2f_imm_f16_uses_selector_bit_41() {
        // F16-imm with selector=0 — should compose the imm and unpack
        // it through CompositeExtractF16x2 with index=1 (selector inversion).
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        // src=F16, dst=F32 (avoid F16-dst packing noise); imm bits at [20,19].
        let insn = encode(1, 2, 0) | (0x7C00u64 << 20);
        f2f_imm(&mut tv, insn);
        let opcodes = opcodes_emitted(&program);
        assert!(opcodes.contains(&Opcode::UnpackFloat2x16));
        assert!(opcodes.contains(&Opcode::CompositeExtractF16x2));
    }

    #[test]
    #[should_panic(expected = "F2F CC")]
    fn f2f_cc_panics() {
        // CC bit (47) set must panic to match upstream's
        // `throw NotImplementedException("F2F CC")`.
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = encode(2, 2, 0) | (1u64 << 47) | (2u64 << 20);
        f2f_reg(&mut tv, insn);
    }

    #[test]
    #[should_panic(expected = "Neg bit on F16")]
    fn f2f_imm_f16_neg_panics() {
        // imm_neg bit (56) set on F16-imm must panic to match upstream's
        // `throw NotImplementedException("Neg bit on F16")`.
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = encode(1, 2, 0) | (1u64 << 56);
        f2f_imm(&mut tv, insn);
    }

    #[test]
    fn fp_convert_dispatch_f16_to_f64_panics() {
        // FPConvert dispatcher must panic on F16↔F64 to match upstream
        // `throw LogicError("Illegal conversion from F16 to F64")`.
        // (Tested separately because F2F never emits this case — upstream
        // would have already thrown on the F2F.cpp side.)
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            tv.ir
                .fp_convert(64, Value::ImmF32(0.0), 16, FpControl::default());
        }));
        assert!(result.is_err(), "fp_convert(64, F16) should panic");
    }
}
