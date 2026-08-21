use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::{Reg, ShiftType};

impl<'a> TranslatorVisitor<'a> {
    // --- ADD/SUB immediate ---

    pub fn add_imm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let shift = inst.bits(23, 22); // shift field
        let imm12 = inst.imm12() as u64;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if shift > 1 {
            return self.reserved_value();
        }
        let imm = imm12 << (shift * 12);

        let operand1 = if rn == Reg::SP {
            self.sp(datasize)
        } else {
            self.x(datasize, rn)
        };
        let operand2 = self.i(datasize, imm);
        let carry = self.ir.ir().imm1(false);
        let result = match datasize {
            32 => self.ir.ir().add_32(operand1, operand2, carry),
            _ => self.ir.ir().add_64(operand1, operand2, carry),
        };

        if rd == Reg::SP {
            self.set_sp(datasize, result);
        } else {
            self.set_x(datasize, rd, result);
        }
        true
    }

    pub fn adds_imm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let shift = inst.bits(23, 22);
        let imm12 = inst.imm12() as u64;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if shift > 1 {
            return self.reserved_value();
        }
        let imm = imm12 << (shift * 12);

        let operand1 = if rn == Reg::SP {
            self.sp(datasize)
        } else {
            self.x(datasize, rn)
        };
        let operand2 = self.i(datasize, imm);
        let carry = self.ir.ir().imm1(false);
        let result = match datasize {
            32 => self.ir.ir().add_32(operand1, operand2, carry),
            _ => self.ir.ir().add_64(operand1, operand2, carry),
        };

        let nzcv = self.ir.ir().get_nzcv_from_op(result);
        self.ir.set_nzcv(nzcv);
        self.set_x(datasize, rd, result); // ADDS always writes to Xd, never SP
        true
    }

    pub fn sub_imm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let shift = inst.bits(23, 22);
        let imm12 = inst.imm12() as u64;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if shift > 1 {
            return self.reserved_value();
        }
        let imm = imm12 << (shift * 12);

        let operand1 = if rn == Reg::SP {
            self.sp(datasize)
        } else {
            self.x(datasize, rn)
        };
        let operand2 = self.i(datasize, imm);
        let carry = self.ir.ir().imm1(true); // carry=1 for SUB
        let result = match datasize {
            32 => self.ir.ir().sub_32(operand1, operand2, carry),
            _ => self.ir.ir().sub_64(operand1, operand2, carry),
        };

        if rd == Reg::SP {
            self.set_sp(datasize, result);
        } else {
            self.set_x(datasize, rd, result);
        }
        true
    }

    pub fn subs_imm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let shift = inst.bits(23, 22);
        let imm12 = inst.imm12() as u64;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if shift > 1 {
            return self.reserved_value();
        }
        let imm = imm12 << (shift * 12);

        let operand1 = if rn == Reg::SP {
            self.sp(datasize)
        } else {
            self.x(datasize, rn)
        };
        let operand2 = self.i(datasize, imm);
        let carry = self.ir.ir().imm1(true);
        let result = match datasize {
            32 => self.ir.ir().sub_32(operand1, operand2, carry),
            _ => self.ir.ir().sub_64(operand1, operand2, carry),
        };

        let nzcv = self.ir.ir().get_nzcv_from_op(result);
        self.ir.set_nzcv(nzcv);
        self.set_x(datasize, rd, result);
        true
    }

    // --- ADD/SUB shifted register ---

    pub fn add_shift(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let shift_type = ShiftType::from_u8(inst.shift() as u8);
        let rm = Reg::from_u32(inst.rm());
        let imm6 = inst.bits(15, 10) as u8;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if shift_type == ShiftType::ROR {
            return self.reserved_value();
        }

        let operand1 = self.x(datasize, rn);
        let rm_val = self.x(datasize, rm);
        let shift_amount = self.ir.ir().imm8(imm6);
        let operand2 = self.shift_reg(datasize, rm_val, shift_type, shift_amount);
        let carry = self.ir.ir().imm1(false);
        let result = match datasize {
            32 => self.ir.ir().add_32(operand1, operand2, carry),
            _ => self.ir.ir().add_64(operand1, operand2, carry),
        };

        self.set_x(datasize, rd, result);
        true
    }

    pub fn adds_shift(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let shift_type = ShiftType::from_u8(inst.shift() as u8);
        let rm = Reg::from_u32(inst.rm());
        let imm6 = inst.bits(15, 10) as u8;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if shift_type == ShiftType::ROR {
            return self.reserved_value();
        }

        let operand1 = self.x(datasize, rn);
        let rm_val = self.x(datasize, rm);
        let shift_amount = self.ir.ir().imm8(imm6);
        let operand2 = self.shift_reg(datasize, rm_val, shift_type, shift_amount);
        let carry = self.ir.ir().imm1(false);
        let result = match datasize {
            32 => self.ir.ir().add_32(operand1, operand2, carry),
            _ => self.ir.ir().add_64(operand1, operand2, carry),
        };

        let nzcv = self.ir.ir().get_nzcv_from_op(result);
        self.ir.set_nzcv(nzcv);
        self.set_x(datasize, rd, result);
        true
    }

    pub fn sub_shift(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let shift_type = ShiftType::from_u8(inst.shift() as u8);
        let rm = Reg::from_u32(inst.rm());
        let imm6 = inst.bits(15, 10) as u8;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if shift_type == ShiftType::ROR {
            return self.reserved_value();
        }

        let operand1 = self.x(datasize, rn);
        let rm_val = self.x(datasize, rm);
        let shift_amount = self.ir.ir().imm8(imm6);
        let operand2 = self.shift_reg(datasize, rm_val, shift_type, shift_amount);
        let carry = self.ir.ir().imm1(true);
        let result = match datasize {
            32 => self.ir.ir().sub_32(operand1, operand2, carry),
            _ => self.ir.ir().sub_64(operand1, operand2, carry),
        };

        self.set_x(datasize, rd, result);
        true
    }

    pub fn subs_shift(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let shift_type = ShiftType::from_u8(inst.shift() as u8);
        let rm = Reg::from_u32(inst.rm());
        let imm6 = inst.bits(15, 10) as u8;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if shift_type == ShiftType::ROR {
            return self.reserved_value();
        }

        let operand1 = self.x(datasize, rn);
        let rm_val = self.x(datasize, rm);
        let shift_amount = self.ir.ir().imm8(imm6);
        let operand2 = self.shift_reg(datasize, rm_val, shift_type, shift_amount);
        let carry = self.ir.ir().imm1(true);
        let result = match datasize {
            32 => self.ir.ir().sub_32(operand1, operand2, carry),
            _ => self.ir.ir().sub_64(operand1, operand2, carry),
        };

        let nzcv = self.ir.ir().get_nzcv_from_op(result);
        self.ir.set_nzcv(nzcv);
        self.set_x(datasize, rd, result);
        true
    }

    // --- ADD/SUB extended register ---

    pub fn add_ext(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let option = inst.option();
        let imm3 = inst.bits(12, 10) as u8;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if imm3 > 4 {
            return self.reserved_value();
        }

        let operand1 = if rn == Reg::SP {
            self.sp(datasize)
        } else {
            self.x(datasize, rn)
        };
        let rm_val = self.x(datasize, rm);
        let operand2 = self.extend_reg(datasize, rm_val, option, imm3);
        let carry = self.ir.ir().imm1(false);
        let result = match datasize {
            32 => self.ir.ir().add_32(operand1, operand2, carry),
            _ => self.ir.ir().add_64(operand1, operand2, carry),
        };

        if rd == Reg::SP {
            self.set_sp(datasize, result);
        } else {
            self.set_x(datasize, rd, result);
        }
        true
    }

    pub fn adds_ext(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let option = inst.option();
        let imm3 = inst.bits(12, 10) as u8;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if imm3 > 4 {
            return self.reserved_value();
        }

        let operand1 = if rn == Reg::SP {
            self.sp(datasize)
        } else {
            self.x(datasize, rn)
        };
        let rm_val = self.x(datasize, rm);
        let operand2 = self.extend_reg(datasize, rm_val, option, imm3);
        let carry = self.ir.ir().imm1(false);
        let result = match datasize {
            32 => self.ir.ir().add_32(operand1, operand2, carry),
            _ => self.ir.ir().add_64(operand1, operand2, carry),
        };

        let nzcv = self.ir.ir().get_nzcv_from_op(result);
        self.ir.set_nzcv(nzcv);
        self.set_x(datasize, rd, result);
        true
    }

    pub fn sub_ext(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let option = inst.option();
        let imm3 = inst.bits(12, 10) as u8;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if imm3 > 4 {
            return self.reserved_value();
        }

        let operand1 = if rn == Reg::SP {
            self.sp(datasize)
        } else {
            self.x(datasize, rn)
        };
        let rm_val = self.x(datasize, rm);
        let operand2 = self.extend_reg(datasize, rm_val, option, imm3);
        let carry = self.ir.ir().imm1(true);
        let result = match datasize {
            32 => self.ir.ir().sub_32(operand1, operand2, carry),
            _ => self.ir.ir().sub_64(operand1, operand2, carry),
        };

        if rd == Reg::SP {
            self.set_sp(datasize, result);
        } else {
            self.set_x(datasize, rd, result);
        }
        true
    }

    pub fn subs_ext(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let option = inst.option();
        let imm3 = inst.bits(12, 10) as u8;
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if imm3 > 4 {
            return self.reserved_value();
        }

        let operand1 = if rn == Reg::SP {
            self.sp(datasize)
        } else {
            self.x(datasize, rn)
        };
        let rm_val = self.x(datasize, rm);
        let operand2 = self.extend_reg(datasize, rm_val, option, imm3);
        let carry = self.ir.ir().imm1(true);
        let result = match datasize {
            32 => self.ir.ir().sub_32(operand1, operand2, carry),
            _ => self.ir.ir().sub_64(operand1, operand2, carry),
        };

        let nzcv = self.ir.ir().get_nzcv_from_op(result);
        self.ir.set_nzcv(nzcv);
        self.set_x(datasize, rd, result);
        true
    }

    // --- Add/subtract with carry ---

    pub fn adc(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.x(datasize, rm);
        let carry = self.ir.get_c_flag();
        let result = match datasize {
            32 => self.ir.ir().add_32(operand1, operand2, carry),
            _ => self.ir.ir().add_64(operand1, operand2, carry),
        };

        self.set_x(datasize, rd, result);
        true
    }

    pub fn adcs(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.x(datasize, rm);
        let carry = self.ir.get_c_flag();
        let result = match datasize {
            32 => self.ir.ir().add_32(operand1, operand2, carry),
            _ => self.ir.ir().add_64(operand1, operand2, carry),
        };

        let nzcv = self.ir.ir().get_nzcv_from_op(result);
        self.ir.set_nzcv(nzcv);
        self.set_x(datasize, rd, result);
        true
    }

    pub fn sbc(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.x(datasize, rm);
        let carry = self.ir.get_c_flag();
        let result = match datasize {
            32 => self.ir.ir().sub_32(operand1, operand2, carry),
            _ => self.ir.ir().sub_64(operand1, operand2, carry),
        };

        self.set_x(datasize, rd, result);
        true
    }

    pub fn sbcs(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.x(datasize, rm);
        let carry = self.ir.get_c_flag();
        let result = match datasize {
            32 => self.ir.ir().sub_32(operand1, operand2, carry),
            _ => self.ir.ir().sub_64(operand1, operand2, carry),
        };

        let nzcv = self.ir.ir().get_nzcv_from_op(result);
        self.ir.set_nzcv(nzcv);
        self.set_x(datasize, rd, result);
        true
    }
}
