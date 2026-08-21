use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Reg;

impl<'a> TranslatorVisitor<'a> {
    /// ADR - Form PC-relative address
    pub fn adr(&mut self, inst: &DecodedInst) -> bool {
        let rd = Reg::from_u32(inst.rd());
        let immhi = inst.bits(23, 5); // 19 bits
        let immlo = inst.bits(30, 29); // 2 bits

        // 21-bit signed immediate = immhi:immlo
        let imm21 = (immhi << 2) | immlo;
        let offset = ((imm21 as i32) << 11 >> 11) as i64; // sign-extend from 21 bits

        let pc = self.ir.pc();
        let result = (pc as i64).wrapping_add(offset) as u64;

        let result_val = self.ir.ir().imm64(result);
        self.set_x(64, rd, result_val);
        true
    }

    /// ADRP - Form PC-relative address to 4KB page
    pub fn adrp(&mut self, inst: &DecodedInst) -> bool {
        let rd = Reg::from_u32(inst.rd());
        let immhi = inst.bits(23, 5);
        let immlo = inst.bits(30, 29);

        let imm21 = (immhi << 2) | immlo;
        let offset = (((imm21 as i32) << 11 >> 11) as i64) << 12; // sign-extend, shift left 12

        let base = self.ir.pc() & !0xFFF; // align to 4KB page
        let result = (base as i64).wrapping_add(offset) as u64;

        let result_val = self.ir.ir().imm64(result);
        self.set_x(64, rd, result_val);
        true
    }
}
