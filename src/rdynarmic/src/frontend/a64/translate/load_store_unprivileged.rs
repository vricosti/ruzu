use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::{AccType, Reg};

impl<'a> TranslatorVisitor<'a> {
    pub fn sttrb(&mut self, inst: &DecodedInst) -> bool {
        self.unprivileged_store(inst, 8)
    }
    pub fn ldtrb(&mut self, inst: &DecodedInst) -> bool {
        self.unprivileged_load(inst, 8, 32, false)
    }

    pub fn ldtrsb(&mut self, inst: &DecodedInst) -> bool {
        let opc = inst.opc();
        let regsize = if (opc & 1) != 0 { 32 } else { 64 };
        self.unprivileged_load(inst, 8, regsize, true)
    }

    pub fn sttrh(&mut self, inst: &DecodedInst) -> bool {
        self.unprivileged_store(inst, 16)
    }
    pub fn ldtrh(&mut self, inst: &DecodedInst) -> bool {
        self.unprivileged_load(inst, 16, 32, false)
    }

    pub fn ldtrsh(&mut self, inst: &DecodedInst) -> bool {
        let opc = inst.opc();
        let regsize = if (opc & 1) != 0 { 32 } else { 64 };
        self.unprivileged_load(inst, 16, regsize, true)
    }

    pub fn sttr(&mut self, inst: &DecodedInst) -> bool {
        let size = inst.size();
        self.unprivileged_store(inst, 8usize << (size as usize))
    }

    pub fn ldtr(&mut self, inst: &DecodedInst) -> bool {
        let size = inst.size();
        let datasize = 8usize << (size as usize);
        let regsize = if size == 3 { 64 } else { 32 };
        self.unprivileged_load(inst, datasize, regsize, false)
    }

    pub fn ldtrsw(&mut self, inst: &DecodedInst) -> bool {
        self.unprivileged_load(inst, 32, 64, true)
    }

    fn unprivileged_store(&mut self, inst: &DecodedInst, datasize: usize) -> bool {
        let imm9 = inst.imm9_sext();
        let rn = Reg::from_u32(inst.rn());
        let rt = Reg::from_u32(inst.rd());

        let base = self.base_address(rn);
        let offset = self.ir.ir().imm64(imm9 as u64);
        let address = self.addr_add(base, offset);

        let data = self.x(datasize.min(64), rt);
        self.mem_write(address, data, datasize / 8, AccType::Unpriv);
        true
    }

    fn unprivileged_load(
        &mut self,
        inst: &DecodedInst,
        datasize: usize,
        regsize: usize,
        signed: bool,
    ) -> bool {
        let imm9 = inst.imm9_sext();
        let rn = Reg::from_u32(inst.rn());
        let rt = Reg::from_u32(inst.rd());

        let base = self.base_address(rn);
        let offset = self.ir.ir().imm64(imm9 as u64);
        let address = self.addr_add(base, offset);

        let data = self.mem_read(address, datasize / 8, AccType::Unpriv);
        let extended = self.sign_or_zero_extend(data, datasize, regsize, signed);
        self.set_x(regsize, rt, extended);
        true
    }
}
