use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::helpers::decode_bit_masks;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Reg;

// ---------------------------------------------------------------------------
// SBFM aliases — ASR_1/2, SXTB_1/2, SXTH_1/2, SXTW.
//
// Mirrors upstream `data_processing_bitfield.cpp:87-134`. Each alias has its
// own translator that emits a focused IR sequence (rather than going through
// the generic SBFM path), matching upstream's structure 1:1.
// ---------------------------------------------------------------------------

impl<'a> TranslatorVisitor<'a> {
    /// SBFM - Signed Bitfield Move
    pub fn sbfm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let n = inst.n();
        let immr = inst.immr();
        let imms = inst.imms();
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if sf != n {
            return self.reserved_value();
        }

        let masks = match decode_bit_masks(n, imms, immr, false) {
            Some(m) => m,
            None => return self.reserved_value(),
        };

        let src = self.x(datasize, rn);
        let r = immr as u8;
        let s = imms as u8;

        // bot = ROR(src, R) AND wmask
        let shift_val = self.ir.ir().imm8(r);
        let carry_in = self.ir.ir().imm1(false);
        let rotated = match datasize {
            32 => self.ir.ir().rotate_right_32(src, shift_val, carry_in),
            _ => self.ir.ir().rotate_right_64(src, shift_val),
        };
        let wmask_val = self.i(datasize, masks.wmask);
        let bot = match datasize {
            32 => self.ir.ir().and_32(rotated, wmask_val),
            _ => self.ir.ir().and_64(rotated, wmask_val),
        };

        // top = ReplicateBit(src, S)
        let s_val = self.ir.ir().imm8(s);
        let top = match datasize {
            32 => self.ir.ir().replicate_bit_32(src, s_val),
            _ => self.ir.ir().replicate_bit_64(src, s_val),
        };

        // result = (top AND NOT tmask) OR (bot AND tmask)
        let tmask_val = self.i(datasize, masks.tmask);
        let not_tmask = self.i(datasize, !masks.tmask);
        let top_part = match datasize {
            32 => self.ir.ir().and_32(top, not_tmask),
            _ => self.ir.ir().and_64(top, not_tmask),
        };
        let bot_part = match datasize {
            32 => self.ir.ir().and_32(bot, tmask_val),
            _ => self.ir.ir().and_64(bot, tmask_val),
        };
        let result = match datasize {
            32 => self.ir.ir().or_32(top_part, bot_part),
            _ => self.ir.ir().or_64(top_part, bot_part),
        };

        self.set_x(datasize, rd, result);
        true
    }

    /// BFM - Bitfield Move
    pub fn bfm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let n = inst.n();
        let immr = inst.immr();
        let imms = inst.imms();
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if sf != n {
            return self.reserved_value();
        }

        let masks = match decode_bit_masks(n, imms, immr, false) {
            Some(m) => m,
            None => return self.reserved_value(),
        };

        let dst = self.x(datasize, rd);
        let src = self.x(datasize, rn);
        let r = immr as u8;

        // bot = (dst AND NOT wmask) OR (ROR(src, R) AND wmask)
        let shift_val = self.ir.ir().imm8(r);
        let carry_in = self.ir.ir().imm1(false);
        let rotated = match datasize {
            32 => self.ir.ir().rotate_right_32(src, shift_val, carry_in),
            _ => self.ir.ir().rotate_right_64(src, shift_val),
        };
        let wmask_val = self.i(datasize, masks.wmask);
        let not_wmask = self.i(datasize, !masks.wmask);

        let dst_part = match datasize {
            32 => self.ir.ir().and_32(dst, not_wmask),
            _ => self.ir.ir().and_64(dst, not_wmask),
        };
        let src_part = match datasize {
            32 => self.ir.ir().and_32(rotated, wmask_val),
            _ => self.ir.ir().and_64(rotated, wmask_val),
        };
        let bot = match datasize {
            32 => self.ir.ir().or_32(dst_part, src_part),
            _ => self.ir.ir().or_64(dst_part, src_part),
        };

        // result = (dst AND NOT tmask) OR (bot AND tmask)
        let tmask_val = self.i(datasize, masks.tmask);
        let not_tmask = self.i(datasize, !masks.tmask);
        let top_part = match datasize {
            32 => self.ir.ir().and_32(dst, not_tmask),
            _ => self.ir.ir().and_64(dst, not_tmask),
        };
        let bot_masked = match datasize {
            32 => self.ir.ir().and_32(bot, tmask_val),
            _ => self.ir.ir().and_64(bot, tmask_val),
        };
        let result = match datasize {
            32 => self.ir.ir().or_32(top_part, bot_masked),
            _ => self.ir.ir().or_64(top_part, bot_masked),
        };

        self.set_x(datasize, rd, result);
        true
    }

    /// UBFM - Unsigned Bitfield Move
    pub fn ubfm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let n = inst.n();
        let immr = inst.immr();
        let imms = inst.imms();
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if sf != n {
            return self.reserved_value();
        }

        let masks = match decode_bit_masks(n, imms, immr, false) {
            Some(m) => m,
            None => return self.reserved_value(),
        };

        let src = self.x(datasize, rn);
        let r = immr as u8;

        // bot = ROR(src, R) AND wmask
        let shift_val = self.ir.ir().imm8(r);
        let carry_in = self.ir.ir().imm1(false);
        let rotated = match datasize {
            32 => self.ir.ir().rotate_right_32(src, shift_val, carry_in),
            _ => self.ir.ir().rotate_right_64(src, shift_val),
        };
        let wmask_val = self.i(datasize, masks.wmask);
        let bot = match datasize {
            32 => self.ir.ir().and_32(rotated, wmask_val),
            _ => self.ir.ir().and_64(rotated, wmask_val),
        };

        // result = bot AND tmask (zeros extend)
        let tmask_val = self.i(datasize, masks.tmask);
        let result = match datasize {
            32 => self.ir.ir().and_32(bot, tmask_val),
            _ => self.ir.ir().and_64(bot, tmask_val),
        };

        self.set_x(datasize, rd, result);
        true
    }

    /// EXTR - Extract register
    pub fn extr(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let n = inst.n();
        let rm = Reg::from_u32(inst.rm());
        let imms = inst.imms();
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if sf != n {
            return self.reserved_value();
        }
        if !sf && imms >= 32 {
            return self.reserved_value();
        }

        let lsb = imms as u8;
        let operand1 = self.x(datasize, rm);
        let operand2 = self.x(datasize, rn);
        let lsb_val = self.ir.ir().imm8(lsb);

        let result = match datasize {
            32 => self
                .ir
                .ir()
                .extract_register_32(operand1, operand2, lsb_val),
            _ => self
                .ir
                .ir()
                .extract_register_64(operand1, operand2, lsb_val),
        };

        self.set_x(datasize, rd, result);
        true
    }

    // ----- SBFM aliases (mirror upstream data_processing_bitfield.cpp:87-134) -----

    /// ASR (immediate, 32-bit) — alias `SBFM Wd, Wn, #immr, #31`.
    pub fn asr_1(&mut self, inst: &DecodedInst) -> bool {
        let immr = inst.immr();
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let src = self.x(32, rn);
        let shift = self.ir.ir().imm32(immr);
        let result = self.ir.ir().arithmetic_shift_right_masked_32(src, shift);
        self.set_x(32, rd, result);
        true
    }

    /// ASR (immediate, 64-bit) — alias `SBFM Xd, Xn, #immr, #63`.
    pub fn asr_2(&mut self, inst: &DecodedInst) -> bool {
        let immr = inst.immr();
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let src = self.x(64, rn);
        let shift = self.ir.ir().imm64(immr as u64);
        let result = self.ir.ir().arithmetic_shift_right_masked_64(src, shift);
        self.set_x(64, rd, result);
        true
    }

    /// SXTB (32-bit) — alias `SBFM Wd, Wn, #0, #7`.
    pub fn sxtb_1(&mut self, inst: &DecodedInst) -> bool {
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let src = self.x(32, rn);
        let byte = self.ir.ir().least_significant_byte(src);
        let result = self.ir.ir().sign_extend_byte_to_word(byte);
        self.set_x(32, rd, result);
        true
    }

    /// SXTB (64-bit) — alias `SBFM Xd, Wn, #0, #7`.
    pub fn sxtb_2(&mut self, inst: &DecodedInst) -> bool {
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let src = self.x(64, rn);
        let byte = self.ir.ir().least_significant_byte(src);
        let result = self.ir.ir().sign_extend_byte_to_long(byte);
        self.set_x(64, rd, result);
        true
    }

    /// SXTH (32-bit) — alias `SBFM Wd, Wn, #0, #15`.
    pub fn sxth_1(&mut self, inst: &DecodedInst) -> bool {
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let src = self.x(32, rn);
        let half = self.ir.ir().least_significant_half(src);
        let result = self.ir.ir().sign_extend_half_to_word(half);
        self.set_x(32, rd, result);
        true
    }

    /// SXTH (64-bit) — alias `SBFM Xd, Wn, #0, #15`.
    pub fn sxth_2(&mut self, inst: &DecodedInst) -> bool {
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let src = self.x(64, rn);
        let half = self.ir.ir().least_significant_half(src);
        let result = self.ir.ir().sign_extend_half_to_long(half);
        self.set_x(64, rd, result);
        true
    }

    /// SXTW — alias `SBFM Xd, Wn, #0, #31`.
    pub fn sxtw(&mut self, inst: &DecodedInst) -> bool {
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let src = self.x(64, rn);
        let word = self.ir.ir().least_significant_word(src);
        let result = self.ir.ir().sign_extend_word_to_long(word);
        self.set_x(64, rd, result);
        true
    }
}
