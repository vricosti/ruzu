use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::{AccType, Reg, Vec};

impl<'a> TranslatorVisitor<'a> {
    /// LDR (literal) - PC-relative load
    pub fn ldr_lit_gen(&mut self, inst: &DecodedInst) -> bool {
        let opc_0 = inst.bit(30);
        let imm19 = inst.imm19_sext();
        let rt = Reg::from_u32(inst.rd());

        let pc = self.ir.pc();
        let offset = imm19 * 4;
        let address = (pc as i64).wrapping_add(offset) as u64;
        let (datasize, regsize) = if opc_0 { (64, 64) } else { (32, 32) };

        let addr_val = self.ir.ir().imm64(address);
        let data = self.mem_read(addr_val, datasize / 8, AccType::Normal);
        self.set_x(regsize, rt, data);
        true
    }

    /// LDRSW (literal) - PC-relative, sign-extend 32 to 64
    pub fn ldrsw_lit(&mut self, inst: &DecodedInst) -> bool {
        let imm19 = inst.imm19_sext();
        let rt = Reg::from_u32(inst.rd());

        let pc = self.ir.pc();
        let offset = imm19 * 4;
        let address = (pc as i64).wrapping_add(offset) as u64;

        let addr_val = self.ir.ir().imm64(address);
        let data = self.mem_read(addr_val, 4, AccType::Normal);
        let extended = self.ir.ir().sign_extend_word_to_long(data);
        self.set_x(64, rt, extended);
        true
    }

    /// LDR (literal, SIMD&FP)
    pub fn ldr_lit_fpsimd(&mut self, inst: &DecodedInst) -> bool {
        let opc = inst.opc();
        let imm19 = inst.imm19_sext();
        let rt = Vec::from_u32(inst.rd());

        let datasize = match opc {
            0 => 32,
            1 => 64,
            2 => 128,
            _ => return self.unallocated_encoding(),
        };

        let pc = self.ir.pc();
        let offset = imm19 * 4;
        let address = (pc as i64).wrapping_add(offset) as u64;

        let addr_val = self.ir.ir().imm64(address);
        let data = self.mem_read(addr_val, datasize / 8, AccType::Vec);
        self.v_scalar_write(datasize, rt, data);
        true
    }
}
