use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Reg;

impl<'a> TranslatorVisitor<'a> {
    /// MADD - Multiply-Add (Rd = Ra + Rn * Rm)
    pub fn madd(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let ra = Reg::from_u32(inst.ra());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.x(datasize, rm);
        let addend = self.x(datasize, ra);

        let product = match datasize {
            32 => self.ir.ir().mul_32(operand1, operand2),
            _ => self.ir.ir().mul_64(operand1, operand2),
        };
        let carry = self.ir.ir().imm1(false);
        let result = match datasize {
            32 => self.ir.ir().add_32(addend, product, carry),
            _ => self.ir.ir().add_64(addend, product, carry),
        };

        self.set_x(datasize, rd, result);
        true
    }

    /// MSUB - Multiply-Subtract (Rd = Ra - Rn * Rm)
    pub fn msub(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let ra = Reg::from_u32(inst.ra());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.x(datasize, rm);
        let minuend = self.x(datasize, ra);

        let product = match datasize {
            32 => self.ir.ir().mul_32(operand1, operand2),
            _ => self.ir.ir().mul_64(operand1, operand2),
        };
        let carry = self.ir.ir().imm1(true);
        let result = match datasize {
            32 => self.ir.ir().sub_32(minuend, product, carry),
            _ => self.ir.ir().sub_64(minuend, product, carry),
        };

        self.set_x(datasize, rd, result);
        true
    }

    /// SMADDL - Signed Multiply-Add Long (Xd = Xa + Wn * Wm, signed)
    pub fn smaddl(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let ra = Reg::from_u32(inst.ra());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let operand1 = self.x(32, rn);
        let operand2 = self.x(32, rm);
        let addend = self.x(64, ra);

        // Sign-extend to 64-bit then multiply
        let op1_64 = self.ir.ir().sign_extend_word_to_long(operand1);
        let op2_64 = self.ir.ir().sign_extend_word_to_long(operand2);
        let product = self.ir.ir().mul_64(op1_64, op2_64);
        let carry = self.ir.ir().imm1(false);
        let result = self.ir.ir().add_64(addend, product, carry);

        self.set_x(64, rd, result);
        true
    }

    /// SMSUBL - Signed Multiply-Subtract Long
    pub fn smsubl(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let ra = Reg::from_u32(inst.ra());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let operand1 = self.x(32, rn);
        let operand2 = self.x(32, rm);
        let minuend = self.x(64, ra);

        let op1_64 = self.ir.ir().sign_extend_word_to_long(operand1);
        let op2_64 = self.ir.ir().sign_extend_word_to_long(operand2);
        let product = self.ir.ir().mul_64(op1_64, op2_64);
        let carry = self.ir.ir().imm1(true);
        let result = self.ir.ir().sub_64(minuend, product, carry);

        self.set_x(64, rd, result);
        true
    }

    /// SMULH - Signed Multiply High
    pub fn smulh(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let operand1 = self.x(64, rn);
        let operand2 = self.x(64, rm);
        let result = self.ir.ir().signed_multiply_high_64(operand1, operand2);

        self.set_x(64, rd, result);
        true
    }

    /// UMADDL - Unsigned Multiply-Add Long
    pub fn umaddl(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let ra = Reg::from_u32(inst.ra());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let operand1 = self.x(32, rn);
        let operand2 = self.x(32, rm);
        let addend = self.x(64, ra);

        let op1_64 = self.ir.ir().zero_extend_word_to_long(operand1);
        let op2_64 = self.ir.ir().zero_extend_word_to_long(operand2);
        let product = self.ir.ir().mul_64(op1_64, op2_64);
        let carry = self.ir.ir().imm1(false);
        let result = self.ir.ir().add_64(addend, product, carry);

        self.set_x(64, rd, result);
        true
    }

    /// UMSUBL - Unsigned Multiply-Subtract Long
    pub fn umsubl(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let ra = Reg::from_u32(inst.ra());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let operand1 = self.x(32, rn);
        let operand2 = self.x(32, rm);
        let minuend = self.x(64, ra);

        let op1_64 = self.ir.ir().zero_extend_word_to_long(operand1);
        let op2_64 = self.ir.ir().zero_extend_word_to_long(operand2);
        let product = self.ir.ir().mul_64(op1_64, op2_64);
        let carry = self.ir.ir().imm1(true);
        let result = self.ir.ir().sub_64(minuend, product, carry);

        self.set_x(64, rd, result);
        true
    }

    /// UMULH - Unsigned Multiply High
    pub fn umulh(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let operand1 = self.x(64, rn);
        let operand2 = self.x(64, rm);
        let result = self.ir.ir().unsigned_multiply_high_64(operand1, operand2);

        self.set_x(64, rd, result);
        true
    }
}
