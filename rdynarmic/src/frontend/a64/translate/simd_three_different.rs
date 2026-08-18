//! Port of upstream `dynarmic/frontend/A64/translate/impl/simd_three_different.cpp`
//! (subset: absolute-difference-long and long add/subtract families).

use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Vec;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LongOperationBehavior {
    Addition,
    Subtraction,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WideOperationBehavior {
    Addition,
    Subtraction,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum MultiplyLongBehavior {
    None,
    Accumulate,
    Subtract,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AbsoluteDifferenceBehavior {
    None,
    Accumulate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Signedness {
    Signed,
    Unsigned,
}

impl<'a> TranslatorVisitor<'a> {
    fn absolute_difference_long(
        &mut self,
        inst: &DecodedInst,
        behavior: AbsoluteDifferenceBehavior,
        sign: Signedness,
    ) -> bool {
        let q = inst.bit(30);
        let size = inst.bits(23, 22);
        if size == 0b11 {
            return self.reserved_value();
        }

        let esize = 8usize << size;
        let vm = Vec::from_u32(inst.bits(20, 16));
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());

        let operand1 = self.vpart_read_64(vn, q as usize);
        let operand1 = self.ir.ir().vector_zero_extend(esize, operand1);
        let operand2 = self.vpart_read_64(vm, q as usize);
        let operand2 = self.ir.ir().vector_zero_extend(esize, operand2);
        let mut result = match sign {
            Signedness::Signed => self
                .ir
                .ir()
                .vector_signed_absolute_difference(esize, operand1, operand2),
            Signedness::Unsigned => self
                .ir
                .ir()
                .vector_unsigned_absolute_difference(esize, operand1, operand2),
        };

        if behavior == AbsoluteDifferenceBehavior::Accumulate {
            let accumulator = self.v_read(128, vd);
            result = self.ir.ir().vector_add(2 * esize, result, accumulator);
        }

        self.v_write(128, vd, result);
        true
    }

    fn long_operation(
        &mut self,
        inst: &DecodedInst,
        behavior: LongOperationBehavior,
        sign: Signedness,
    ) -> bool {
        let q = inst.bit(30);
        let size = inst.bits(23, 22);
        if size == 0b11 {
            return self.reserved_value();
        }

        let esize = 8usize << size;
        let part = if q { 1 } else { 0 };
        let vm = Vec::from_u32(inst.bits(20, 16));
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());

        let operand1 = self.vpart_read_64(vn, part);
        let operand1 = match sign {
            Signedness::Signed => self.ir.ir().vector_sign_extend(esize, operand1),
            Signedness::Unsigned => self.ir.ir().vector_zero_extend(esize, operand1),
        };
        let operand2 = self.vpart_read_64(vm, part);
        let operand2 = match sign {
            Signedness::Signed => self.ir.ir().vector_sign_extend(esize, operand2),
            Signedness::Unsigned => self.ir.ir().vector_zero_extend(esize, operand2),
        };

        let result = match behavior {
            LongOperationBehavior::Addition => {
                self.ir.ir().vector_add(2 * esize, operand1, operand2)
            }
            LongOperationBehavior::Subtraction => {
                self.ir.ir().vector_sub(2 * esize, operand1, operand2)
            }
        };
        self.v_write(128, vd, result);
        true
    }

    fn multiply_long(
        &mut self,
        inst: &DecodedInst,
        behavior: MultiplyLongBehavior,
        sign: Signedness,
    ) -> bool {
        let q = inst.bit(30);
        let size = inst.bits(23, 22);
        if size == 0b11 {
            return self.reserved_value();
        }

        let esize = 8usize << size;
        let part = if q { 1 } else { 0 };
        let vm = Vec::from_u32(inst.bits(20, 16));
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());

        let operand1 = self.vpart_read_64(vn, part);
        let operand2 = self.vpart_read_64(vm, part);
        let mut result = match sign {
            Signedness::Signed => self
                .ir
                .ir()
                .vector_multiply_signed_widen(esize, operand1, operand2),
            Signedness::Unsigned => self
                .ir
                .ir()
                .vector_multiply_unsigned_widen(esize, operand1, operand2),
        };

        match behavior {
            MultiplyLongBehavior::None => {}
            MultiplyLongBehavior::Accumulate => {
                let addend = self.v_read(128, vd);
                result = self.ir.ir().vector_add(2 * esize, addend, result);
            }
            MultiplyLongBehavior::Subtract => {
                let minuend = self.v_read(128, vd);
                result = self.ir.ir().vector_sub(2 * esize, minuend, result);
            }
        }

        self.v_write(128, vd, result);
        true
    }

    fn wide_operation(
        &mut self,
        inst: &DecodedInst,
        behavior: WideOperationBehavior,
        sign: Signedness,
    ) -> bool {
        let q = inst.bit(30);
        let size = inst.bits(23, 22);
        if size == 0b11 {
            return self.reserved_value();
        }

        let esize = 8usize << size;
        let part = if q { 1 } else { 0 };
        let vm = Vec::from_u32(inst.bits(20, 16));
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());

        let operand1 = self.v_read(128, vn);
        let operand2 = self.vpart_read_64(vm, part);
        let operand2 = match sign {
            Signedness::Signed => self.ir.ir().vector_sign_extend(esize, operand2),
            Signedness::Unsigned => self.ir.ir().vector_zero_extend(esize, operand2),
        };
        let result = match behavior {
            WideOperationBehavior::Addition => {
                self.ir.ir().vector_add(2 * esize, operand1, operand2)
            }
            WideOperationBehavior::Subtraction => {
                self.ir.ir().vector_sub(2 * esize, operand1, operand2)
            }
        };

        self.v_write(128, vd, result);
        true
    }

    pub fn saddl(&mut self, inst: &DecodedInst) -> bool {
        self.long_operation(inst, LongOperationBehavior::Addition, Signedness::Signed)
    }

    pub fn ssubl(&mut self, inst: &DecodedInst) -> bool {
        self.long_operation(inst, LongOperationBehavior::Subtraction, Signedness::Signed)
    }

    pub fn uaddl(&mut self, inst: &DecodedInst) -> bool {
        self.long_operation(inst, LongOperationBehavior::Addition, Signedness::Unsigned)
    }

    pub fn usubl(&mut self, inst: &DecodedInst) -> bool {
        self.long_operation(
            inst,
            LongOperationBehavior::Subtraction,
            Signedness::Unsigned,
        )
    }

    pub fn saddw(&mut self, inst: &DecodedInst) -> bool {
        self.wide_operation(inst, WideOperationBehavior::Addition, Signedness::Signed)
    }

    pub fn ssubw(&mut self, inst: &DecodedInst) -> bool {
        self.wide_operation(inst, WideOperationBehavior::Subtraction, Signedness::Signed)
    }

    pub fn uaddw(&mut self, inst: &DecodedInst) -> bool {
        self.wide_operation(inst, WideOperationBehavior::Addition, Signedness::Unsigned)
    }

    pub fn usubw(&mut self, inst: &DecodedInst) -> bool {
        self.wide_operation(
            inst,
            WideOperationBehavior::Subtraction,
            Signedness::Unsigned,
        )
    }

    pub fn smlal_vec(&mut self, inst: &DecodedInst) -> bool {
        self.multiply_long(inst, MultiplyLongBehavior::Accumulate, Signedness::Signed)
    }

    pub fn smlsl_vec(&mut self, inst: &DecodedInst) -> bool {
        self.multiply_long(inst, MultiplyLongBehavior::Subtract, Signedness::Signed)
    }

    pub fn smull_vec(&mut self, inst: &DecodedInst) -> bool {
        self.multiply_long(inst, MultiplyLongBehavior::None, Signedness::Signed)
    }

    pub fn umlal_vec(&mut self, inst: &DecodedInst) -> bool {
        self.multiply_long(inst, MultiplyLongBehavior::Accumulate, Signedness::Unsigned)
    }

    pub fn umlsl_vec(&mut self, inst: &DecodedInst) -> bool {
        self.multiply_long(inst, MultiplyLongBehavior::Subtract, Signedness::Unsigned)
    }

    pub fn umull_vec(&mut self, inst: &DecodedInst) -> bool {
        self.multiply_long(inst, MultiplyLongBehavior::None, Signedness::Unsigned)
    }

    pub fn sabal(&mut self, inst: &DecodedInst) -> bool {
        self.absolute_difference_long(
            inst,
            AbsoluteDifferenceBehavior::Accumulate,
            Signedness::Signed,
        )
    }

    pub fn sabdl(&mut self, inst: &DecodedInst) -> bool {
        self.absolute_difference_long(inst, AbsoluteDifferenceBehavior::None, Signedness::Signed)
    }

    pub fn uabal(&mut self, inst: &DecodedInst) -> bool {
        self.absolute_difference_long(
            inst,
            AbsoluteDifferenceBehavior::Accumulate,
            Signedness::Unsigned,
        )
    }

    pub fn uabdl(&mut self, inst: &DecodedInst) -> bool {
        self.absolute_difference_long(inst, AbsoluteDifferenceBehavior::None, Signedness::Unsigned)
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
    fn saddl_spacecadet_encoding_translates_without_interpret_terminal() {
        let (block, should_continue) = translate_one(0x0E650000);
        assert!(should_continue);
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::VectorSignExtend16));
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::VectorAdd32));
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::A64SetQ));
        assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
    }

    #[test]
    fn uaddw_encoding_translates_without_interpret_terminal() {
        let (block, should_continue) = translate_one(0x2E24_1046);
        assert!(should_continue);
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::VectorZeroExtend8));
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::VectorAdd16));
        assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
    }

    #[test]
    fn absolute_difference_long_family_uses_matching_ir_opcodes() {
        let cases = [
            (0x0E27_5085, Opcode::VectorSignedAbsoluteDifference8),
            (0x0E27_7085, Opcode::VectorSignedAbsoluteDifference8),
            (0x2E27_5085, Opcode::VectorUnsignedAbsoluteDifference8),
            (0x2E27_7085, Opcode::VectorUnsignedAbsoluteDifference8),
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
    fn multiply_long_family_uses_matching_ir_opcodes() {
        let cases = [
            (
                0x0E22_8001,
                Opcode::VectorMultiplySignedWiden8,
                Some(Opcode::VectorAdd16),
            ),
            (
                0x0E22_A001,
                Opcode::VectorMultiplySignedWiden8,
                Some(Opcode::VectorSub16),
            ),
            (0x0E22_C001, Opcode::VectorMultiplySignedWiden8, None),
            (
                0x2E22_8001,
                Opcode::VectorMultiplyUnsignedWiden8,
                Some(Opcode::VectorAdd16),
            ),
            (
                0x2E22_A001,
                Opcode::VectorMultiplyUnsignedWiden8,
                Some(Opcode::VectorSub16),
            ),
            (0x2E22_C001, Opcode::VectorMultiplyUnsignedWiden8, None),
        ];

        for (encoding, multiply_opcode, combine_opcode) in cases {
            let (block, should_continue) = translate_one(encoding);
            assert!(should_continue, "encoding 0x{encoding:08X}");
            assert!(
                block
                    .instructions
                    .iter()
                    .any(|inst| inst.opcode == multiply_opcode),
                "encoding 0x{encoding:08X} did not emit {multiply_opcode:?}"
            );
            if let Some(combine_opcode) = combine_opcode {
                assert!(
                    block
                        .instructions
                        .iter()
                        .any(|inst| inst.opcode == combine_opcode),
                    "encoding 0x{encoding:08X} did not emit {combine_opcode:?}"
                );
            }
            assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
        }
    }
}
