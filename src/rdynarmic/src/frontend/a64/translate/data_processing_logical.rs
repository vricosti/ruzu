use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::helpers::decode_bit_masks;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::{Reg, ShiftType};

impl<'a> TranslatorVisitor<'a> {
    // --- Logical immediate ---

    pub fn and_imm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let n = inst.n();
        let immr = inst.immr();
        let imms = inst.imms();
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if !sf && n {
            return self.reserved_value();
        }

        let masks = match decode_bit_masks(n, imms, immr, true) {
            Some(m) => m,
            None => return self.reserved_value(),
        };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.i(datasize, masks.wmask);
        let result = match datasize {
            32 => self.ir.ir().and_32(operand1, operand2),
            _ => self.ir.ir().and_64(operand1, operand2),
        };

        if rd == Reg::SP {
            self.set_sp(datasize, result);
        } else {
            self.set_x(datasize, rd, result);
        }
        true
    }

    pub fn orr_imm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let n = inst.n();
        let immr = inst.immr();
        let imms = inst.imms();
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if !sf && n {
            return self.reserved_value();
        }

        let masks = match decode_bit_masks(n, imms, immr, true) {
            Some(m) => m,
            None => return self.reserved_value(),
        };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.i(datasize, masks.wmask);
        let result = match datasize {
            32 => self.ir.ir().or_32(operand1, operand2),
            _ => self.ir.ir().or_64(operand1, operand2),
        };

        if rd == Reg::SP {
            self.set_sp(datasize, result);
        } else {
            self.set_x(datasize, rd, result);
        }
        true
    }

    pub fn eor_imm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let n = inst.n();
        let immr = inst.immr();
        let imms = inst.imms();
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if !sf && n {
            return self.reserved_value();
        }

        let masks = match decode_bit_masks(n, imms, immr, true) {
            Some(m) => m,
            None => return self.reserved_value(),
        };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.i(datasize, masks.wmask);
        let result = match datasize {
            32 => self.ir.ir().eor_32(operand1, operand2),
            _ => self.ir.ir().eor_64(operand1, operand2),
        };

        if rd == Reg::SP {
            self.set_sp(datasize, result);
        } else {
            self.set_x(datasize, rd, result);
        }
        true
    }

    pub fn ands_imm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let n = inst.n();
        let immr = inst.immr();
        let imms = inst.imms();
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if !sf && n {
            return self.reserved_value();
        }

        let masks = match decode_bit_masks(n, imms, immr, true) {
            Some(m) => m,
            None => return self.reserved_value(),
        };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.i(datasize, masks.wmask);
        let result = match datasize {
            32 => self.ir.ir().and_32(operand1, operand2),
            _ => self.ir.ir().and_64(operand1, operand2),
        };

        let nzcv = self.ir.ir().get_nzcv_from_op(result);
        self.ir.set_nzcv(nzcv);
        self.set_x(datasize, rd, result); // ANDS writes to Xd, never SP
        true
    }

    // --- Logical shifted register ---

    pub fn and_shift(&mut self, inst: &DecodedInst) -> bool {
        self.logical_shift_impl(inst, LogicalOp::And, false, false)
    }

    pub fn bic_shift(&mut self, inst: &DecodedInst) -> bool {
        self.logical_shift_impl(inst, LogicalOp::And, true, false)
    }

    pub fn orr_shift(&mut self, inst: &DecodedInst) -> bool {
        self.logical_shift_impl(inst, LogicalOp::Or, false, false)
    }

    pub fn orn_shift(&mut self, inst: &DecodedInst) -> bool {
        self.logical_shift_impl(inst, LogicalOp::Or, true, false)
    }

    pub fn eor_shift(&mut self, inst: &DecodedInst) -> bool {
        self.logical_shift_impl(inst, LogicalOp::Eor, false, false)
    }

    pub fn eon_shift(&mut self, inst: &DecodedInst) -> bool {
        self.logical_shift_impl(inst, LogicalOp::Eor, true, false)
    }

    pub fn ands_shift(&mut self, inst: &DecodedInst) -> bool {
        self.logical_shift_impl(inst, LogicalOp::And, false, true)
    }

    pub fn bics_shift(&mut self, inst: &DecodedInst) -> bool {
        self.logical_shift_impl(inst, LogicalOp::And, true, true)
    }

    fn logical_shift_impl(
        &mut self,
        inst: &DecodedInst,
        op: LogicalOp,
        invert: bool,
        set_flags: bool,
    ) -> bool {
        let sf = inst.sf();
        let shift_type = ShiftType::from_u8(inst.shift() as u8);
        let rm = Reg::from_u32(inst.rm());
        let imm6 = inst.bits(15, 10) as u8;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let rm_val = self.x(datasize, rm);
        let shift_amount = self.ir.ir().imm8(imm6);
        let mut operand2 = self.shift_reg(datasize, rm_val, shift_type, shift_amount);

        if invert {
            operand2 = match datasize {
                32 => self.ir.ir().not_32(operand2),
                _ => self.ir.ir().not_64(operand2),
            };
        }

        let result = match (datasize, op) {
            (32, LogicalOp::And) => self.ir.ir().and_32(operand1, operand2),
            (64, LogicalOp::And) => self.ir.ir().and_64(operand1, operand2),
            (32, LogicalOp::Or) => self.ir.ir().or_32(operand1, operand2),
            (64, LogicalOp::Or) => self.ir.ir().or_64(operand1, operand2),
            (32, LogicalOp::Eor) => self.ir.ir().eor_32(operand1, operand2),
            (64, LogicalOp::Eor) => self.ir.ir().eor_64(operand1, operand2),
            _ => unreachable!(),
        };

        if set_flags {
            let nzcv = self.ir.ir().get_nzcv_from_op(result);
            self.ir.set_nzcv(nzcv);
        }

        self.set_x(datasize, rd, result);
        true
    }
}

enum LogicalOp {
    And,
    Or,
    Eor,
}
