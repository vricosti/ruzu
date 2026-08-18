use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Reg;

impl<'a> TranslatorVisitor<'a> {
    /// Dispatch CRC32 based on the `sz` field (bits [11:10]).
    pub fn crc32_dispatch(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.bit(31);
        let size = inst.bits(11, 10);
        if (sf && size != 0b11) || (!sf && size == 0b11) {
            return self.unallocated_encoding();
        }
        match size {
            0 => self.crc32b(inst),
            1 => self.crc32h(inst),
            2 => self.crc32w(inst),
            3 => self.crc32x(inst),
            _ => self.reserved_value(),
        }
    }

    /// Dispatch CRC32C based on size field
    pub fn crc32c_dispatch(&mut self, inst: &DecodedInst) -> bool {
        let sf = inst.bit(31);
        let size = inst.bits(11, 10);
        if (sf && size != 0b11) || (!sf && size == 0b11) {
            return self.unallocated_encoding();
        }
        match size {
            0 => self.crc32cb(inst),
            1 => self.crc32ch(inst),
            2 => self.crc32cw(inst),
            3 => self.crc32cx(inst),
            _ => self.reserved_value(),
        }
    }

    pub fn crc32b(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let crc = self.x(32, rn);
        let data = self.x(32, rm);
        let result = self.ir.ir().crc32_iso_8(crc, data);
        self.set_x(32, rd, result);
        true
    }

    pub fn crc32h(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let crc = self.x(32, rn);
        let data = self.x(32, rm);
        let result = self.ir.ir().crc32_iso_16(crc, data);
        self.set_x(32, rd, result);
        true
    }

    pub fn crc32w(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let crc = self.x(32, rn);
        let data = self.x(32, rm);
        let result = self.ir.ir().crc32_iso_32(crc, data);
        self.set_x(32, rd, result);
        true
    }

    pub fn crc32x(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let crc = self.x(32, rn);
        let data = self.x(64, rm);
        let result = self.ir.ir().crc32_iso_64(crc, data);
        self.set_x(32, rd, result);
        true
    }

    pub fn crc32cb(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let crc = self.x(32, rn);
        let data = self.x(32, rm);
        let result = self.ir.ir().crc32_castagnoli_8(crc, data);
        self.set_x(32, rd, result);
        true
    }

    pub fn crc32ch(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let crc = self.x(32, rn);
        let data = self.x(32, rm);
        let result = self.ir.ir().crc32_castagnoli_16(crc, data);
        self.set_x(32, rd, result);
        true
    }

    pub fn crc32cw(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let crc = self.x(32, rn);
        let data = self.x(32, rm);
        let result = self.ir.ir().crc32_castagnoli_32(crc, data);
        self.set_x(32, rd, result);
        true
    }

    pub fn crc32cx(&mut self, inst: &DecodedInst) -> bool {
        let rm = Reg::from_u32(inst.rm());
        let rn = Reg::from_u32(inst.rn());
        let rd = Reg::from_u32(inst.rd());

        let crc = self.x(32, rn);
        let data = self.x(64, rm);
        let result = self.ir.ir().crc32_castagnoli_64(crc, data);
        self.set_x(32, rd, result);
        true
    }
}
