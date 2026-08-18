//! Port of upstream
//! `dynarmic/frontend/A64/translate/impl/simd_scalar_shift_by_immediate.cpp`
//! (subset: `SHL_1`, `SSHR_1`, `USHR_1`, `FCVTZS_fix_1`, `FCVTZU_fix_1`,
//! `SCVTF_fix_1`, `UCVTF_fix_1`).

use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Vec;

const ROUND_TOWARDS_ZERO: u8 = 3;

fn current_fpcr_rounding_mode(visitor: &TranslatorVisitor<'_>) -> u8 {
    ((visitor
        .ir
        .current_location
        .expect("current_location not set")
        .fpcr()
        >> 22)
        & 0x3) as u8
}

impl<'a> TranslatorVisitor<'a> {
    /// Decode `(immh, immb, Vn, Vd)` and the 7-bit `immh:immb` immediate.
    fn scalar_shift_imm_decode(&mut self, inst: &DecodedInst) -> (u32, u32, Vec, Vec, u32) {
        let immh = inst.bits(22, 19);
        let immb = inst.bits(18, 16);
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());
        let imm7 = (immh << 3) | immb;
        (immh, immb, vn, vd, imm7)
    }

    /// SSHR (scalar). `010111110IIIIiii000001nnnnnddddd`. Requires immh<3>=1.
    pub fn sshr_1(&mut self, inst: &DecodedInst) -> bool {
        let (immh, _immb, vn, vd, imm7) = self.scalar_shift_imm_decode(inst);
        if (immh & 0b1000) == 0 {
            return self.reserved_value();
        }
        let esize = 64usize;
        let shift_amount = ((esize as u32 * 2) - imm7) as u8;
        let operand = self.v_scalar_read(esize, vn);
        let elem = self.ir.ir().vector_get_element(esize, operand, 0);
        let shift = self.ir.ir().imm8(shift_amount);
        let result = self.ir.ir().arithmetic_shift_right_64(elem, shift);
        self.v_scalar_write(esize, vd, result);
        true
    }

    /// USHR (scalar). `011111110IIIIiii000001nnnnnddddd`. Requires immh<3>=1.
    pub fn ushr_1(&mut self, inst: &DecodedInst) -> bool {
        let (immh, _immb, vn, vd, imm7) = self.scalar_shift_imm_decode(inst);
        if (immh & 0b1000) == 0 {
            return self.reserved_value();
        }
        let esize = 64usize;
        let shift_amount = ((esize as u32 * 2) - imm7) as u8;
        let operand = self.v_scalar_read(esize, vn);
        let elem = self.ir.ir().vector_get_element(esize, operand, 0);
        let shift = self.ir.ir().imm8(shift_amount);
        let result = self.ir.ir().logical_shift_right_64(elem, shift);
        self.v_scalar_write(esize, vd, result);
        true
    }

    /// SHL (scalar). `010111110IIIIiii010101nnnnnddddd`. Requires immh<3>=1.
    pub fn shl_1(&mut self, inst: &DecodedInst) -> bool {
        let (immh, _immb, vn, vd, imm7) = self.scalar_shift_imm_decode(inst);
        if (immh & 0b1000) == 0 {
            return self.reserved_value();
        }
        let esize = 64usize;
        let shift_amount = (imm7 - esize as u32) as u8;
        let operand = self.v_scalar_read(esize, vn);
        let elem = self.ir.ir().vector_get_element(esize, operand, 0);
        let shift = self.ir.ir().imm8(shift_amount);
        let result = self.ir.ir().logical_shift_left_64(elem, shift);
        self.v_scalar_write(esize, vd, result);
        true
    }

    fn scalar_fp_convert_with_round(
        &mut self,
        inst: &DecodedInst,
        signed: bool,
        float_to_fixed: bool,
        rounding: u8,
    ) -> bool {
        let (immh, _immb, vn, vd, imm7) = self.scalar_shift_imm_decode(inst);
        let immh_value = immh as u8;
        if (immh_value & 0b1110) == 0b0000 {
            return self.reserved_value();
        }
        // No FP16 path.
        if (immh_value & 0b1110) == 0b0010 {
            return self.reserved_value();
        }
        let esize = if (immh_value & 0b1000) != 0 { 64 } else { 32 };
        let fbits = ((esize as u32 * 2) - imm7) as u8;

        let operand = self.v_scalar_read(esize, vn);
        let elem = self.ir.ir().vector_get_element(esize, operand, 0);

        let result = if float_to_fixed {
            match (esize, signed) {
                (64, true) => self.ir.ir().fp_to_fixed_s64(elem, 64, fbits, rounding),
                (64, false) => self.ir.ir().fp_to_fixed_u64(elem, 64, fbits, rounding),
                (32, true) => self.ir.ir().fp_to_fixed_s32(elem, 32, fbits, rounding),
                (32, false) => self.ir.ir().fp_to_fixed_u32(elem, 32, fbits, rounding),
                _ => unreachable!(),
            }
        } else if esize == 64 {
            self.ir
                .ir()
                .fp_fixed_to_double(elem, 64, signed, fbits, rounding)
        } else {
            self.ir
                .ir()
                .fp_fixed_to_single(elem, 32, signed, fbits, rounding)
        };
        self.v_scalar_write(esize, vd, result);
        true
    }

    /// FCVTZS (vector, fixed-point, scalar). `010111110IIIIiii111111nnnnnddddd`.
    pub fn fcvtzs_fix_1(&mut self, inst: &DecodedInst) -> bool {
        self.scalar_fp_convert_with_round(inst, true, true, ROUND_TOWARDS_ZERO)
    }

    /// FCVTZU (vector, fixed-point, scalar). `011111110IIIIiii111111nnnnnddddd`.
    pub fn fcvtzu_fix_1(&mut self, inst: &DecodedInst) -> bool {
        self.scalar_fp_convert_with_round(inst, false, true, ROUND_TOWARDS_ZERO)
    }

    /// SCVTF (vector, fixed-point, scalar). `010111110IIIIiii111001nnnnnddddd`.
    pub fn scvtf_fix_1(&mut self, inst: &DecodedInst) -> bool {
        let rounding = current_fpcr_rounding_mode(self);
        self.scalar_fp_convert_with_round(inst, true, false, rounding)
    }

    /// UCVTF (vector, fixed-point, scalar). `011111110IIIIiii111001nnnnnddddd`.
    pub fn ucvtf_fix_1(&mut self, inst: &DecodedInst) -> bool {
        let rounding = current_fpcr_rounding_mode(self);
        self.scalar_fp_convert_with_round(inst, false, false, rounding)
    }
}
