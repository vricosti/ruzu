use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Reg;
use crate::ir::cond::Cond;
use crate::ir::value::Value;

impl<'a> TranslatorVisitor<'a> {
    pub fn csel(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let cond = Cond::from_u8(inst.cond_field() as u8);
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn); // if cond true
        let operand2 = self.x(datasize, rm); // if cond false
        let cond_val = Value::ImmCond(cond);

        let result = match datasize {
            32 => self
                .ir
                .ir()
                .conditional_select_32(cond_val, operand1, operand2),
            _ => self
                .ir
                .ir()
                .conditional_select_64(cond_val, operand1, operand2),
        };

        self.set_x(datasize, rd, result);
        true
    }

    pub fn csinc(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let cond = Cond::from_u8(inst.cond_field() as u8);
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.x(datasize, rm);
        let one = self.i(datasize, 1);
        let carry = self.ir.ir().imm1(false);
        let incremented = match datasize {
            32 => self.ir.ir().add_32(operand2, one, carry),
            _ => self.ir.ir().add_64(operand2, one, carry),
        };
        let cond_val = Value::ImmCond(cond);

        let result = match datasize {
            32 => self
                .ir
                .ir()
                .conditional_select_32(cond_val, operand1, incremented),
            _ => self
                .ir
                .ir()
                .conditional_select_64(cond_val, operand1, incremented),
        };

        self.set_x(datasize, rd, result);
        true
    }

    pub fn csinv(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let cond = Cond::from_u8(inst.cond_field() as u8);
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.x(datasize, rm);
        let inverted = match datasize {
            32 => self.ir.ir().not_32(operand2),
            _ => self.ir.ir().not_64(operand2),
        };
        let cond_val = Value::ImmCond(cond);

        let result = match datasize {
            32 => self
                .ir
                .ir()
                .conditional_select_32(cond_val, operand1, inverted),
            _ => self
                .ir
                .ir()
                .conditional_select_64(cond_val, operand1, inverted),
        };

        self.set_x(datasize, rd, result);
        true
    }

    pub fn csneg(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let cond = Cond::from_u8(inst.cond_field() as u8);
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.x(datasize, rm);
        // negate = NOT(operand2) + 1
        let inverted = match datasize {
            32 => self.ir.ir().not_32(operand2),
            _ => self.ir.ir().not_64(operand2),
        };
        let one = self.i(datasize, 1);
        let carry = self.ir.ir().imm1(false);
        let negated = match datasize {
            32 => self.ir.ir().add_32(inverted, one, carry),
            _ => self.ir.ir().add_64(inverted, one, carry),
        };
        let cond_val = Value::ImmCond(cond);

        let result = match datasize {
            32 => self
                .ir
                .ir()
                .conditional_select_32(cond_val, operand1, negated),
            _ => self
                .ir
                .ir()
                .conditional_select_64(cond_val, operand1, negated),
        };

        self.set_x(datasize, rd, result);
        true
    }
}
