//! Port of upstream
//! `dynarmic/frontend/A64/translate/impl/simd_shift_by_immediate.cpp`
//! (subset: the right-shift family, `SHL_2`, the narrowing right shifts,
//! `SSHLL`, and `USHLL`).
//!
//! Vector shift-by-immediate. The 7-bit immediate `immh:immb` encodes
//! both the element size (highest set bit of `immh`) and the shift
//! amount (`immh:immb - esize` for left, `2*esize - immh:immb` for
//! right).

use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Vec;

#[derive(Clone, Copy)]
enum Narrowing {
    Truncation,
    SaturateToUnsigned,
    SaturateToSigned,
}

#[derive(Clone, Copy)]
enum Rounding {
    None,
    Round,
}

#[derive(Clone, Copy)]
enum Accumulating {
    None,
    Accumulate,
}

#[derive(Clone, Copy)]
enum Signedness {
    Signed,
    Unsigned,
}

fn highest_set_bit(x: u32) -> u32 {
    debug_assert!(x != 0);
    31 - x.leading_zeros()
}

impl<'a> TranslatorVisitor<'a> {
    fn perform_rounding_correction(
        &mut self,
        esize: usize,
        round_value: u64,
        original: crate::ir::value::Value,
        shifted: crate::ir::value::Value,
    ) -> crate::ir::value::Value {
        let round_imm = self.i(esize, round_value);
        let round_const = self.ir.ir().vector_broadcast(esize, round_imm);
        let masked = self.ir.ir().vector_and(original, round_const);
        let round_correction = self.ir.ir().vector_equal(esize, masked, round_const);
        self.ir.ir().vector_sub(esize, shifted, round_correction)
    }

    fn shift_right(
        &mut self,
        inst: &DecodedInst,
        rounding: Rounding,
        accumulating: Accumulating,
        signedness: Signedness,
    ) -> bool {
        let q = inst.bit(30);
        let immh = inst.bits(22, 19);
        let immb = inst.bits(18, 16);
        if immh == 0 {
            return self.decode_error();
        }
        if (immh & 0b1000) != 0 && !q {
            return self.reserved_value();
        }

        let esize = 8usize << highest_set_bit(immh) as usize;
        let datasize = if q { 128 } else { 64 };
        let imm7 = ((immh << 3) | immb) as u8;
        let shift_amount = (2 * esize as u8) - imm7;
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());

        let operand = self.v_read(datasize, vn);
        let mut result = match signedness {
            Signedness::Signed => {
                self.ir
                    .ir()
                    .vector_arithmetic_shift_right(esize, operand, shift_amount)
            }
            Signedness::Unsigned => {
                self.ir
                    .ir()
                    .vector_logical_shift_right(esize, operand, shift_amount)
            }
        };

        if matches!(rounding, Rounding::Round) {
            let round_value = 1u64 << (shift_amount - 1);
            result = self.perform_rounding_correction(esize, round_value, operand, result);
        }

        if matches!(accumulating, Accumulating::Accumulate) {
            let accumulator = self.v_read(datasize, vd);
            result = self.ir.ir().vector_add(esize, result, accumulator);
        }

        self.v_write(datasize, vd, result);
        true
    }

    fn shift_left_long(&mut self, inst: &DecodedInst, signed: bool) -> bool {
        let q = inst.bit(30);
        let immh = inst.bits(22, 19);
        let immb = inst.bits(18, 16);
        if immh == 0 {
            self.decode_error();
            return false;
        }
        if (immh & 0b1000) != 0 {
            return self.reserved_value();
        }

        let esize = 8usize << highest_set_bit(immh) as usize;
        let part = if q { 1 } else { 0 };
        let imm7 = ((immh << 3) | immb) as u8;
        let shift_amount = imm7 - esize as u8;
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());

        let operand = self.vpart_read_64(vn, part);
        let expanded_operand = if signed {
            self.ir.ir().vector_sign_extend(esize, operand)
        } else {
            self.ir.ir().vector_zero_extend(esize, operand)
        };
        let result =
            self.ir
                .ir()
                .vector_logical_shift_left(2 * esize, expanded_operand, shift_amount);
        self.v_write(128, vd, result);
        true
    }

    /// SHL (immediate, vector). `0Q0011110IIIIiii010101nnnnnddddd`.
    pub fn shl_2(&mut self, inst: &DecodedInst) -> bool {
        let q = inst.bit(30);
        let immh = inst.bits(22, 19);
        let immb = inst.bits(18, 16);
        if immh == 0 {
            return self.decode_error();
        }
        if (immh & 0b1000) != 0 && !q {
            return self.reserved_value();
        }
        let esize = 8usize << highest_set_bit(immh) as usize;
        let datasize = if q { 128 } else { 64 };
        let imm7 = ((immh << 3) | immb) as u8;
        let shift_amount = imm7 - esize as u8;
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let operand = self.v_read(datasize, vn);
        let result = self
            .ir
            .ir()
            .vector_logical_shift_left(esize, operand, shift_amount);
        self.v_write(datasize, vd, result);
        true
    }

    /// USHR (vector). `0Q1011110IIIIiii000001nnnnnddddd`.
    pub fn ushr_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right(
            inst,
            Rounding::None,
            Accumulating::None,
            Signedness::Unsigned,
        )
    }

    /// SSHR (vector). `0Q0011110IIIIiii000001nnnnnddddd`.
    pub fn sshr_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right(inst, Rounding::None, Accumulating::None, Signedness::Signed)
    }

    /// SRSHR (vector). `0Q0011110IIIIiii001001nnnnnddddd`.
    pub fn srshr_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right(
            inst,
            Rounding::Round,
            Accumulating::None,
            Signedness::Signed,
        )
    }

    /// SRSRA (vector). `0Q0011110IIIIiii001101nnnnnddddd`.
    pub fn srsra_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right(
            inst,
            Rounding::Round,
            Accumulating::Accumulate,
            Signedness::Signed,
        )
    }

    /// SSRA (vector). `0Q0011110IIIIiii000101nnnnnddddd`.
    pub fn ssra_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right(
            inst,
            Rounding::None,
            Accumulating::Accumulate,
            Signedness::Signed,
        )
    }

    /// URSHR (vector). `0Q1011110IIIIiii001001nnnnnddddd`.
    pub fn urshr_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right(
            inst,
            Rounding::Round,
            Accumulating::None,
            Signedness::Unsigned,
        )
    }

    /// URSRA (vector). `0Q1011110IIIIiii001101nnnnnddddd`.
    pub fn ursra_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right(
            inst,
            Rounding::Round,
            Accumulating::Accumulate,
            Signedness::Unsigned,
        )
    }

    /// USRA (vector). `0Q1011110IIIIiii000101nnnnnddddd`.
    pub fn usra_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right(
            inst,
            Rounding::None,
            Accumulating::Accumulate,
            Signedness::Unsigned,
        )
    }

    fn shift_right_narrowing(
        &mut self,
        inst: &DecodedInst,
        round: bool,
        narrowing: Narrowing,
        signedness: Signedness,
    ) -> bool {
        let q = inst.bit(30);
        let immh = inst.bits(22, 19);
        let immb = inst.bits(18, 16);
        if immh == 0 {
            self.decode_error();
            return false;
        }
        if (immh & 0b1000) != 0 {
            return self.reserved_value();
        }

        let esize = 8usize << highest_set_bit(immh) as usize;
        let source_esize = 2 * esize;
        let part = if q { 1 } else { 0 };
        let shift_amount = source_esize as u8 - ((immh << 3) | immb) as u8;
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());

        let operand = self.v_read(128, vn);
        let mut wide_result = match signedness {
            Signedness::Signed => {
                self.ir
                    .ir()
                    .vector_arithmetic_shift_right(source_esize, operand, shift_amount)
            }
            Signedness::Unsigned => {
                self.ir
                    .ir()
                    .vector_logical_shift_right(source_esize, operand, shift_amount)
            }
        };

        if round {
            let round_value = 1u64 << (shift_amount - 1);
            wide_result =
                self.perform_rounding_correction(source_esize, round_value, operand, wide_result);
        }

        let result = match narrowing {
            Narrowing::Truncation => self.ir.ir().vector_narrow(source_esize, wide_result),
            Narrowing::SaturateToUnsigned => match signedness {
                Signedness::Signed => self
                    .ir
                    .ir()
                    .vector_signed_saturated_narrow_to_unsigned(source_esize, wide_result),
                Signedness::Unsigned => self
                    .ir
                    .ir()
                    .vector_unsigned_saturated_narrow(source_esize, wide_result),
            },
            Narrowing::SaturateToSigned => {
                debug_assert!(matches!(signedness, Signedness::Signed));
                self.ir
                    .ir()
                    .vector_signed_saturated_narrow_to_signed(source_esize, wide_result)
            }
        };
        self.vpart_write_64(vd, part, result);
        true
    }

    pub fn shrn(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right_narrowing(inst, false, Narrowing::Truncation, Signedness::Unsigned)
    }

    pub fn rshrn(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right_narrowing(inst, true, Narrowing::Truncation, Signedness::Unsigned)
    }

    pub fn sqshrn_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right_narrowing(inst, false, Narrowing::SaturateToSigned, Signedness::Signed)
    }

    pub fn sqrshrn_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right_narrowing(inst, true, Narrowing::SaturateToSigned, Signedness::Signed)
    }

    pub fn sqshrun_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right_narrowing(
            inst,
            false,
            Narrowing::SaturateToUnsigned,
            Signedness::Signed,
        )
    }

    pub fn sqrshrun_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right_narrowing(
            inst,
            true,
            Narrowing::SaturateToUnsigned,
            Signedness::Signed,
        )
    }

    pub fn uqshrn_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right_narrowing(
            inst,
            false,
            Narrowing::SaturateToUnsigned,
            Signedness::Unsigned,
        )
    }

    pub fn uqrshrn_2(&mut self, inst: &DecodedInst) -> bool {
        self.shift_right_narrowing(
            inst,
            true,
            Narrowing::SaturateToUnsigned,
            Signedness::Unsigned,
        )
    }

    /// SSHLL/SSHLL2. `0Q0011110IIIIiii101001nnnnnddddd`.
    pub fn sshll(&mut self, inst: &DecodedInst) -> bool {
        self.shift_left_long(inst, true)
    }

    /// USHLL/USHLL2. `0Q1011110IIIIiii101001nnnnnddddd`.
    pub fn ushll(&mut self, inst: &DecodedInst) -> bool {
        self.shift_left_long(inst, false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::a64::decoder::decode;
    use crate::ir::block::Block;
    use crate::ir::location::A64LocationDescriptor;
    use crate::ir::opcode::Opcode;
    use crate::ir::terminal::Terminal;

    fn translate_one(raw: u32) -> (Block, bool) {
        let decoded = decode(raw).expect("instruction should decode");
        let mut block = Block::new(A64LocationDescriptor::new(0x1000, 0, false).to_location());
        let mut visitor = TranslatorVisitor::new(
            &mut block,
            A64LocationDescriptor::new(0x1000, 0, false),
            crate::frontend::a64::translate::visitor::TranslationOptions::default(),
        );
        let should_continue = visitor.dispatch(&decoded);
        drop(visitor);
        (block, should_continue)
    }

    #[test]
    fn shrn_stk_encoding_translates_without_interpret_terminal() {
        let (block, should_continue) = translate_one(0x0F0C8400);
        assert!(should_continue);
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::VectorNarrow16));
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::VectorZeroExtend64));
        assert!(!block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::ZeroExtendLongToQuad));
        assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
    }

    #[test]
    fn saturated_shift_right_narrowing_family_uses_matching_ir_opcodes() {
        let cases = [
            (0x0F0E_945C, Opcode::VectorSignedSaturatedNarrowToSigned16),
            (0x0F0E_9C5C, Opcode::VectorSignedSaturatedNarrowToSigned16),
            (0x2F0E_845C, Opcode::VectorSignedSaturatedNarrowToUnsigned16),
            (0x2F0E_8C5C, Opcode::VectorSignedSaturatedNarrowToUnsigned16),
            (0x2F0E_945C, Opcode::VectorUnsignedSaturatedNarrow16),
            (0x2F0E_9C5C, Opcode::VectorUnsignedSaturatedNarrow16),
        ];

        for (encoding, expected_opcode) in cases {
            let (block, should_continue) = translate_one(encoding);
            assert!(should_continue, "encoding 0x{encoding:08X}");
            assert!(
                block
                    .instructions
                    .iter()
                    .any(|inst| inst.opcode == expected_opcode),
                "encoding 0x{encoding:08X} did not emit {expected_opcode:?}"
            );
            assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
        }
    }

    #[test]
    fn sshll_spacecadet_encoding_translates_without_interpret_terminal() {
        let (block, should_continue) = translate_one(0x0F10A7FF);
        assert!(should_continue);
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::VectorSignExtend16));
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::VectorLogicalShiftLeft32));
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::A64SetQ));
        assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
    }

    #[test]
    fn vector_shift_right_family_matches_upstream_variant_semantics() {
        let cases = [
            (
                0x4F34_07BC,
                Opcode::VectorArithmeticShiftRight32,
                false,
                false,
            ),
            (
                0x4F34_17BC,
                Opcode::VectorArithmeticShiftRight32,
                false,
                true,
            ),
            (
                0x4F34_27BC,
                Opcode::VectorArithmeticShiftRight32,
                true,
                false,
            ),
            (
                0x4F34_37BC,
                Opcode::VectorArithmeticShiftRight32,
                true,
                true,
            ),
            (0x6F34_07BC, Opcode::VectorLogicalShiftRight32, false, false),
            (0x6F34_17BC, Opcode::VectorLogicalShiftRight32, false, true),
            (0x6F34_27BC, Opcode::VectorLogicalShiftRight32, true, false),
            (0x6F34_37BC, Opcode::VectorLogicalShiftRight32, true, true),
        ];

        for (encoding, shift_opcode, rounded, accumulating) in cases {
            let (block, should_continue) = translate_one(encoding);
            assert!(should_continue, "encoding 0x{encoding:08X}");
            assert!(
                block
                    .instructions
                    .iter()
                    .any(|inst| inst.opcode == shift_opcode),
                "encoding 0x{encoding:08X} did not emit {shift_opcode:?}"
            );
            assert_eq!(
                block
                    .instructions
                    .iter()
                    .any(|inst| inst.opcode == Opcode::VectorEqual32),
                rounded,
                "encoding 0x{encoding:08X} rounding mismatch"
            );
            assert_eq!(
                block
                    .instructions
                    .iter()
                    .any(|inst| inst.opcode == Opcode::VectorAdd32),
                accumulating,
                "encoding 0x{encoding:08X} accumulation mismatch"
            );
            assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
        }
    }
}
