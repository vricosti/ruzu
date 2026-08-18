//! Port of upstream
//! `dynarmic/frontend/A64/translate/impl/simd_scalar_two_register_misc.cpp`
//! (subset: ABS_1, NEG_1, FCMEQ_zero_1, FCMGE_zero_2, FCMGT_zero_2,
//! FCVTAS_2, FCVTAU_2, FCVTMS_2, FCVTMU_2, FCVTNS_2, FCVTNU_2,
//! FCVTPS_2, FCVTPU_2, FCVTZS_int_2, FCVTZU_int_2, SCVTF_int_2,
//! UCVTF_int_2, FRECPE_1, FRECPE_2, FRECPX_1, FRECPX_2, FRSQRTE_1,
//! FRSQRTE_2).

use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Vec;

const ROUND_TO_NEAREST_TIE_EVEN: u8 = 0;
const ROUND_TOWARDS_PLUS_INFINITY: u8 = 1;
const ROUND_TOWARDS_MINUS_INFINITY: u8 = 2;
const ROUND_TOWARDS_ZERO: u8 = 3;
const ROUND_TO_NEAREST_TIE_AWAY_FROM_ZERO: u8 = 4;

fn current_fpcr_rounding_mode(visitor: &TranslatorVisitor<'_>) -> u8 {
    ((visitor
        .ir
        .current_location
        .expect("current_location not set")
        .fpcr()
        >> 22)
        & 0x3) as u8
}

#[derive(Copy, Clone)]
enum FpZeroCmp {
    Eq,
    Ge,
    Gt,
}

#[derive(Copy, Clone)]
enum SaturatedNarrowKind {
    SignedToSigned,
    SignedToUnsigned,
    Unsigned,
}

impl<'a> TranslatorVisitor<'a> {
    fn saturated_narrow_1(&mut self, inst: &DecodedInst, kind: SaturatedNarrowKind) -> bool {
        let size = inst.bits(23, 22);
        if size == 0b11 {
            return self.reserved_value();
        }

        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let original_esize = 16usize << size as usize;
        let source = self.v_scalar_read(128, vn);
        let scalar = self.ir.ir().vector_get_element(original_esize, source, 0);
        let operand = match original_esize {
            16 => self.ir.ir().zero_extend_half_to_long(scalar),
            32 => self.ir.ir().zero_extend_word_to_long(scalar),
            64 => scalar,
            _ => unreachable!(),
        };
        let operand = self.ir.ir().zero_extend_to_quad(operand);
        let result = match kind {
            SaturatedNarrowKind::SignedToSigned => self
                .ir
                .ir()
                .vector_signed_saturated_narrow_to_signed(original_esize, operand),
            SaturatedNarrowKind::SignedToUnsigned => self
                .ir
                .ir()
                .vector_signed_saturated_narrow_to_unsigned(original_esize, operand),
            SaturatedNarrowKind::Unsigned => self
                .ir
                .ir()
                .vector_unsigned_saturated_narrow(original_esize, operand),
        };
        let result = self.ir.ir().vector_get_element(64, result, 0);
        self.v_scalar_write(64, vd, result);
        true
    }

    /// ABS (scalar). `01011110zz100000101110nnnnnddddd`. Only size==0b11 is valid.
    pub fn abs_1(&mut self, inst: &DecodedInst) -> bool {
        let size = inst.bits(23, 22);
        if size != 0b11 {
            return self.reserved_value();
        }
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let operand = self.v_scalar_read(64, vn);
        let elem = self.ir.ir().vector_get_element(64, operand, 0);
        // For 64-bit two's complement abs: x ^ (x>>63) - (x>>63).
        let shift = self.ir.ir().imm8(63);
        let mask = self.ir.ir().arithmetic_shift_right_64(elem, shift);
        let xored = self.ir.ir().eor_64(elem, mask);
        let one = self.ir.ir().imm1(true);
        let result = self.ir.ir().sub_64(xored, mask, one);
        self.v_scalar_write(64, vd, result);
        true
    }

    /// NEG (scalar). `01111110zz100000101110nnnnnddddd`. size==0b11 only.
    pub fn neg_1(&mut self, inst: &DecodedInst) -> bool {
        let size = inst.bits(23, 22);
        if size != 0b11 {
            return self.reserved_value();
        }
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let operand = self.v_scalar_read(64, vn);
        let elem = self.ir.ir().vector_get_element(64, operand, 0);
        let zero = self.ir.ir().imm64(0);
        let one = self.ir.ir().imm1(true);
        let result = self.ir.ir().sub_64(zero, elem, one);
        self.v_scalar_write(64, vd, result);
        true
    }

    pub fn sqxtn_1(&mut self, inst: &DecodedInst) -> bool {
        self.saturated_narrow_1(inst, SaturatedNarrowKind::SignedToSigned)
    }

    pub fn sqxtun_1(&mut self, inst: &DecodedInst) -> bool {
        self.saturated_narrow_1(inst, SaturatedNarrowKind::SignedToUnsigned)
    }

    pub fn uqxtn_1(&mut self, inst: &DecodedInst) -> bool {
        self.saturated_narrow_1(inst, SaturatedNarrowKind::Unsigned)
    }

    /// FCMEQ (zero, scalar, half-precision).
    /// `0101111011111000110110nnnnnddddd` — esize=16.
    pub fn fcmeq_zero_1(&mut self, inst: &DecodedInst) -> bool {
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let elem = self.v_scalar_read(16, vn);
        let operand = self.ir.ir().zero_extend_to_quad(elem);
        let zero = self.ir.ir().zero_vector();
        let result = self.ir.ir().fp_vector_equal(16, operand, zero, true);
        let r0 = self.ir.ir().vector_get_element(16, result, 0);
        self.v_scalar_write(16, vd, r0);
        true
    }

    fn scalar_fp_compare_against_zero(
        &mut self,
        sz: bool,
        vn: Vec,
        vd: Vec,
        kind: FpZeroCmp,
    ) -> bool {
        let esize = if sz { 64 } else { 32 };
        let operand = self.v_scalar_read(esize, vn);
        let zero = self.ir.ir().zero_vector();
        let result = match kind {
            FpZeroCmp::Eq => self.ir.ir().fp_vector_equal(esize, operand, zero, true),
            FpZeroCmp::Ge => self
                .ir
                .ir()
                .fp_vector_greater_equal(esize, operand, zero, true),
            FpZeroCmp::Gt => self.ir.ir().fp_vector_greater(esize, operand, zero, true),
        };
        let r0 = self.ir.ir().vector_get_element(esize, result, 0);
        self.v_scalar_write(esize, vd, r0);
        true
    }

    /// FCMGE (zero, scalar). `011111101z100000110010nnnnnddddd`. sz at bit 22.
    pub fn fcmge_zero_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_compare_against_zero(sz, vn, vd, FpZeroCmp::Ge)
    }

    /// FCMGT (zero, scalar). `010111101z100000110010nnnnnddddd`. sz at bit 22.
    pub fn fcmgt_zero_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_compare_against_zero(sz, vn, vd, FpZeroCmp::Gt)
    }

    fn scalar_fp_to_fixed(
        &mut self,
        sz: bool,
        vn: Vec,
        vd: Vec,
        signed: bool,
        rounding: u8,
    ) -> bool {
        let esize = if sz { 64 } else { 32 };
        let operand = self.v_scalar_read(esize, vn);
        let result = match (sz, signed) {
            (true, true) => self.ir.ir().fp_to_fixed_s64(operand, 64, 0, rounding),
            (true, false) => self.ir.ir().fp_to_fixed_u64(operand, 64, 0, rounding),
            (false, true) => self.ir.ir().fp_to_fixed_s32(operand, 32, 0, rounding),
            (false, false) => self.ir.ir().fp_to_fixed_u32(operand, 32, 0, rounding),
        };
        self.v_scalar_write(esize, vd, result);
        true
    }

    /// FCVTAS (vector, scalar). `010111100z100001110010nnnnnddddd`.
    pub fn fcvtas_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_to_fixed(sz, vn, vd, true, ROUND_TO_NEAREST_TIE_AWAY_FROM_ZERO)
    }

    /// FCVTAU (vector, scalar). `011111100z100001110010nnnnnddddd`.
    pub fn fcvtau_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_to_fixed(sz, vn, vd, false, ROUND_TO_NEAREST_TIE_AWAY_FROM_ZERO)
    }

    /// FCVTMS (vector, scalar). `010111100z100001101110nnnnnddddd`.
    pub fn fcvtms_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_to_fixed(sz, vn, vd, true, ROUND_TOWARDS_MINUS_INFINITY)
    }

    /// FCVTMU (vector, scalar). `011111100z100001101110nnnnnddddd`.
    pub fn fcvtmu_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_to_fixed(sz, vn, vd, false, ROUND_TOWARDS_MINUS_INFINITY)
    }

    /// FCVTNS (vector, scalar). `010111100z100001101010nnnnnddddd`.
    pub fn fcvtns_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_to_fixed(sz, vn, vd, true, ROUND_TO_NEAREST_TIE_EVEN)
    }

    /// FCVTNU (vector, scalar). `011111100z100001101010nnnnnddddd`.
    pub fn fcvtnu_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_to_fixed(sz, vn, vd, false, ROUND_TO_NEAREST_TIE_EVEN)
    }

    /// FCVTPS (vector, scalar). `010111101z100001101010nnnnnddddd`.
    pub fn fcvtps_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_to_fixed(sz, vn, vd, true, ROUND_TOWARDS_PLUS_INFINITY)
    }

    /// FCVTPU (vector, scalar). `011111101z100001101010nnnnnddddd`.
    pub fn fcvtpu_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_to_fixed(sz, vn, vd, false, ROUND_TOWARDS_PLUS_INFINITY)
    }

    /// FCVTZS (vector, integer, scalar). `010111101z100001101110nnnnnddddd`.
    pub fn fcvtzs_int_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_to_fixed(sz, vn, vd, true, ROUND_TOWARDS_ZERO)
    }

    /// FCVTZU (vector, integer, scalar). `011111101z100001101110nnnnnddddd`.
    pub fn fcvtzu_int_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fp_to_fixed(sz, vn, vd, false, ROUND_TOWARDS_ZERO)
    }

    /// FRECPE (scalar, half-precision). `0101111011111001110110nnnnnddddd`.
    pub fn frecpe_1(&mut self, inst: &DecodedInst) -> bool {
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let operand = self.v_scalar_read(16, vn);
        let result = self.ir.ir().fp_recip_estimate(16, operand);
        self.v_scalar_write(16, vd, result);
        true
    }

    /// FRECPE (scalar, single/double). `010111101z100001110110nnnnnddddd`.
    pub fn frecpe_2(&mut self, inst: &DecodedInst) -> bool {
        let esize = if inst.bit(22) { 64 } else { 32 };
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let operand = self.v_scalar_read(esize, vn);
        let result = self.ir.ir().fp_recip_estimate(esize, operand);
        self.v_scalar_write(esize, vd, result);
        true
    }

    /// FRECPX (scalar, half-precision). `0101111011111001111110nnnnnddddd`.
    pub fn frecpx_1(&mut self, inst: &DecodedInst) -> bool {
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let operand = self.v_scalar_read(16, vn);
        let result = self.ir.ir().fp_recip_exponent(16, operand);
        self.v_scalar_write(16, vd, result);
        true
    }

    /// FRECPX (scalar, single/double). `010111101z100001111110nnnnnddddd`.
    pub fn frecpx_2(&mut self, inst: &DecodedInst) -> bool {
        let esize = if inst.bit(22) { 64 } else { 32 };
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let operand = self.v_scalar_read(esize, vn);
        let result = self.ir.ir().fp_recip_exponent(esize, operand);
        self.v_scalar_write(esize, vd, result);
        true
    }

    /// FRSQRTE (scalar, half-precision). `0111111011111001110110nnnnnddddd`.
    pub fn frsqrte_1(&mut self, inst: &DecodedInst) -> bool {
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let operand = self.v_scalar_read(16, vn);
        let result = self.ir.ir().fp_rsqrt_estimate(16, operand);
        self.v_scalar_write(16, vd, result);
        true
    }

    /// FRSQRTE (scalar, single/double). `011111101z100001110110nnnnnddddd`.
    pub fn frsqrte_2(&mut self, inst: &DecodedInst) -> bool {
        let esize = if inst.bit(22) { 64 } else { 32 };
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let operand = self.v_scalar_read(esize, vn);
        let result = self.ir.ir().fp_rsqrt_estimate(esize, operand);
        self.v_scalar_write(esize, vd, result);
        true
    }

    fn scalar_fixed_to_fp(&mut self, sz: bool, vn: Vec, vd: Vec, signed: bool) -> bool {
        let esize = if sz { 64 } else { 32 };
        let rounding = current_fpcr_rounding_mode(self);
        let operand = self.v_scalar_read(esize, vn);
        let result = if sz {
            self.ir
                .ir()
                .fp_fixed_to_double(operand, 64, signed, 0, rounding)
        } else {
            self.ir
                .ir()
                .fp_fixed_to_single(operand, 32, signed, 0, rounding)
        };
        self.v_scalar_write(esize, vd, result);
        true
    }

    /// SCVTF (vector, integer, scalar). `010111100z100001110110nnnnnddddd`.
    pub fn scvtf_int_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fixed_to_fp(sz, vn, vd, true)
    }

    /// UCVTF (vector, integer, scalar). `011111100z100001110110nnnnnddddd`.
    pub fn ucvtf_int_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        self.scalar_fixed_to_fp(sz, vn, vd, false)
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
    fn saturated_narrow_1_family_uses_matching_ir_opcodes() {
        let cases = [
            (0x5E21_49F0, Opcode::VectorSignedSaturatedNarrowToSigned16),
            (0x7E21_29F0, Opcode::VectorSignedSaturatedNarrowToUnsigned16),
            (0x7E21_49F0, Opcode::VectorUnsignedSaturatedNarrow16),
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
    fn observed_fcvtns_scalar_encoding_translates_instead_of_interpreting() {
        let (block, should_continue) = translate_one(0x5E21_A800);

        assert!(should_continue);
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::FPSingleToFixedS32));
        assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
    }

    #[test]
    fn scalar_fp_to_integer_rounding_family_uses_matching_ir_opcodes() {
        let cases = [
            (0x5E21_A800, Opcode::FPSingleToFixedS32), // FCVTNS S0, S0
            (0x5E21_B800, Opcode::FPSingleToFixedS32), // FCVTMS S0, S0
            (0x5E21_C800, Opcode::FPSingleToFixedS32), // FCVTAS S0, S0
            (0x5EA1_A800, Opcode::FPSingleToFixedS32), // FCVTPS S0, S0
            (0x7E21_A800, Opcode::FPSingleToFixedU32), // FCVTNU S0, S0
            (0x7E21_B800, Opcode::FPSingleToFixedU32), // FCVTMU S0, S0
            (0x7E21_C800, Opcode::FPSingleToFixedU32), // FCVTAU S0, S0
            (0x7EA1_A800, Opcode::FPSingleToFixedU32), // FCVTPU S0, S0
            (0x5E61_A800, Opcode::FPDoubleToFixedS64), // FCVTNS D0, D0
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
    fn observed_frsqrte_scalar_encoding_translates_instead_of_interpreting() {
        let (block, should_continue) = translate_one(0x7EA1_DA11);

        assert!(should_continue);
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::FPRSqrtEstimate32));
        assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
    }

    #[test]
    fn scalar_fp_estimate_family_uses_matching_ir_opcodes() {
        let cases = [
            (0x5EF9_D800, Opcode::FPRecipEstimate16),
            (0x5EA1_D800, Opcode::FPRecipEstimate32),
            (0x5EE1_D800, Opcode::FPRecipEstimate64),
            (0x5EF9_F800, Opcode::FPRecipExponent16),
            (0x5EA1_F800, Opcode::FPRecipExponent32),
            (0x5EE1_F800, Opcode::FPRecipExponent64),
            (0x7EF9_D800, Opcode::FPRSqrtEstimate16),
            (0x7EA1_D800, Opcode::FPRSqrtEstimate32),
            (0x7EE1_D800, Opcode::FPRSqrtEstimate64),
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
}
