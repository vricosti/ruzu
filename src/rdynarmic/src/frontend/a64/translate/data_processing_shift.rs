use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Reg;

impl<'a> TranslatorVisitor<'a> {
    pub fn lslv(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand = self.x(datasize, rn);
        let shift_amount = self.x(datasize, rm);
        let result = match datasize {
            32 => self
                .ir
                .ir()
                .logical_shift_left_masked_32(operand, shift_amount),
            _ => self
                .ir
                .ir()
                .logical_shift_left_masked_64(operand, shift_amount),
        };
        self.set_x(datasize, rd, result);
        true
    }

    pub fn lsrv(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand = self.x(datasize, rn);
        let shift_amount = self.x(datasize, rm);
        let result = match datasize {
            32 => self
                .ir
                .ir()
                .logical_shift_right_masked_32(operand, shift_amount),
            _ => self
                .ir
                .ir()
                .logical_shift_right_masked_64(operand, shift_amount),
        };
        self.set_x(datasize, rd, result);
        true
    }

    pub fn asrv(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand = self.x(datasize, rn);
        let shift_amount = self.x(datasize, rm);
        let result = match datasize {
            32 => self
                .ir
                .ir()
                .arithmetic_shift_right_masked_32(operand, shift_amount),
            _ => self
                .ir
                .ir()
                .arithmetic_shift_right_masked_64(operand, shift_amount),
        };
        self.set_x(datasize, rd, result);
        true
    }

    pub fn rorv(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        let operand = self.x(datasize, rn);
        let shift_amount = self.x(datasize, rm);
        let result = match datasize {
            32 => self.ir.ir().rotate_right_masked_32(operand, shift_amount),
            _ => self.ir.ir().rotate_right_masked_64(operand, shift_amount),
        };
        self.set_x(datasize, rd, result);
        true
    }
}
