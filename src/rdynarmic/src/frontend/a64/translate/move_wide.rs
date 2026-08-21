use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Reg;

impl<'a> TranslatorVisitor<'a> {
    /// MOVZ - Move wide with zero
    pub fn movz(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let hw = inst.hw();
        let imm16 = inst.imm16() as u64;
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if !sf && (hw & 2) != 0 {
            return self.unallocated_encoding();
        }

        let pos = (hw as u64) * 16;
        let value = imm16 << pos;

        let val = self.i(datasize, value);
        self.set_x(datasize, rd, val);
        true
    }

    /// MOVN - Move wide with NOT
    pub fn movn(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let hw = inst.hw();
        let imm16 = inst.imm16() as u64;
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if !sf && (hw & 2) != 0 {
            return self.unallocated_encoding();
        }

        let pos = (hw as u64) * 16;
        let value = !(imm16 << pos);
        let value = if !sf { value & 0xFFFF_FFFF } else { value };

        let val = self.i(datasize, value);
        self.set_x(datasize, rd, val);
        true
    }

    /// MOVK - Move wide with keep
    pub fn movk(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.sf();
        let hw = inst.hw();
        let imm16 = inst.imm16() as u64;
        let rd = Reg::from_u32(inst.rd());
        let datasize = if sf { 64 } else { 32 };

        if !sf && (hw & 2) != 0 {
            return self.unallocated_encoding();
        }

        let pos = (hw as u64) * 16;
        let mask = 0xFFFFu64 << pos;
        let bits = imm16 << pos;

        let old_val = self.x(datasize, rd);
        let not_mask = self.i(datasize, !mask);
        let masked = match datasize {
            32 => self.ir.ir().and_32(old_val, not_mask),
            _ => self.ir.ir().and_64(old_val, not_mask),
        };
        let bits_val = self.i(datasize, bits);
        let result = match datasize {
            32 => self.ir.ir().or_32(masked, bits_val),
            _ => self.ir.ir().or_64(masked, bits_val),
        };

        self.set_x(datasize, rd, result);
        true
    }
}
