use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Reg;
use crate::ir::cond::Cond;
use crate::ir::value::Value;

impl<'a> TranslatorVisitor<'a> {
    /// CCMN (immediate) - Conditional Compare Negative (immediate)
    pub fn ccmn_imm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let imm5 = inst.bits(20, 16) as u64;
        let cond = Cond::from_u8(inst.cond_field() as u8);
        let rn = Reg::from_u32(inst.rn());
        let nzcv_imm = inst.bits(3, 0);
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.i(datasize, imm5);
        let carry = self.ir.ir().imm1(false);

        // If condition true: ADDS(Rn, imm5), else: NZCV = nzcv_imm
        let result_flags = match datasize {
            32 => {
                let result = self.ir.ir().add_32(operand1, operand2, carry);
                self.ir.ir().get_nzcv_from_op(result)
            }
            _ => {
                let result = self.ir.ir().add_64(operand1, operand2, carry);
                self.ir.ir().get_nzcv_from_op(result)
            }
        };

        let packed = self.ir.ir().imm32(nzcv_imm << 28);
        let imm_flags = self.ir.ir().nzcv_from_packed_flags(packed);
        let cond_val = Value::ImmCond(cond);
        let nzcv = self
            .ir
            .ir()
            .conditional_select_nzcv(cond_val, result_flags, imm_flags);
        self.ir.set_nzcv(nzcv);
        true
    }

    /// CCMP (immediate) - Conditional Compare (immediate)
    pub fn ccmp_imm(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let imm5 = inst.bits(20, 16) as u64;
        let cond = Cond::from_u8(inst.cond_field() as u8);
        let rn = Reg::from_u32(inst.rn());
        let nzcv_imm = inst.bits(3, 0);
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.i(datasize, imm5);
        let carry = self.ir.ir().imm1(true); // SUB uses carry=1

        let result_flags = match datasize {
            32 => {
                let result = self.ir.ir().sub_32(operand1, operand2, carry);
                self.ir.ir().get_nzcv_from_op(result)
            }
            _ => {
                let result = self.ir.ir().sub_64(operand1, operand2, carry);
                self.ir.ir().get_nzcv_from_op(result)
            }
        };

        let packed = self.ir.ir().imm32(nzcv_imm << 28);
        let imm_flags = self.ir.ir().nzcv_from_packed_flags(packed);
        let cond_val = Value::ImmCond(cond);
        let nzcv = self
            .ir
            .ir()
            .conditional_select_nzcv(cond_val, result_flags, imm_flags);
        self.ir.set_nzcv(nzcv);
        true
    }

    /// CCMN (register) - Conditional Compare Negative (register)
    pub fn ccmn_reg(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let cond = Cond::from_u8(inst.cond_field() as u8);
        let rn = Reg::from_u32(inst.rn());
        let nzcv_imm = inst.bits(3, 0);
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.x(datasize, rm);
        let carry = self.ir.ir().imm1(false);

        let result_flags = match datasize {
            32 => {
                let result = self.ir.ir().add_32(operand1, operand2, carry);
                self.ir.ir().get_nzcv_from_op(result)
            }
            _ => {
                let result = self.ir.ir().add_64(operand1, operand2, carry);
                self.ir.ir().get_nzcv_from_op(result)
            }
        };

        let packed = self.ir.ir().imm32(nzcv_imm << 28);
        let imm_flags = self.ir.ir().nzcv_from_packed_flags(packed);
        let cond_val = Value::ImmCond(cond);
        let nzcv = self
            .ir
            .ir()
            .conditional_select_nzcv(cond_val, result_flags, imm_flags);
        self.ir.set_nzcv(nzcv);
        true
    }

    /// CCMP (register) - Conditional Compare (register)
    pub fn ccmp_reg(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let rm = Reg::from_u32(inst.rm());
        let cond = Cond::from_u8(inst.cond_field() as u8);
        let rn = Reg::from_u32(inst.rn());
        let nzcv_imm = inst.bits(3, 0);
        let datasize = if sf { 64 } else { 32 };

        let operand1 = self.x(datasize, rn);
        let operand2 = self.x(datasize, rm);
        let carry = self.ir.ir().imm1(true);

        let result_flags = match datasize {
            32 => {
                let result = self.ir.ir().sub_32(operand1, operand2, carry);
                self.ir.ir().get_nzcv_from_op(result)
            }
            _ => {
                let result = self.ir.ir().sub_64(operand1, operand2, carry);
                self.ir.ir().get_nzcv_from_op(result)
            }
        };

        let packed = self.ir.ir().imm32(nzcv_imm << 28);
        let imm_flags = self.ir.ir().nzcv_from_packed_flags(packed);
        let cond_val = Value::ImmCond(cond);
        let nzcv = self
            .ir
            .ir()
            .conditional_select_nzcv(cond_val, result_flags, imm_flags);
        self.ir.set_nzcv(nzcv);
        true
    }
}
