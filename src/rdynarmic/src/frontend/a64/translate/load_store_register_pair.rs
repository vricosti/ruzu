use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::{AccType, Reg, Vec};

impl<'a> TranslatorVisitor<'a> {
    /// STP/LDP (general) - Store/Load pair of registers
    pub fn stp_ldp_gen(&mut self, inst: &DecodedInst) -> bool {
        // opc field for LDP/STP is at bits[31:30], not bits[30:29]. The default
        // `inst.opc()` helper extracts bits[30:29] which is the wrong field —
        // bits[31:30] is the "size" field per the ARM ARM (00=32-bit LDP/STP,
        // 01=LDPSW, 10=64-bit LDP/STP, 11=reserved). Using the wrong field
        // miscompiled `STP x29,x30,[sp,#-0x20]!` as `LDPSW`, truncating x30 to
        // 32-bit and sign-extending on reload — corrupted libnx return addresses.
        let opc = inst.size();
        let not_postindex = inst.bit(24);
        let wback = inst.bit(23);
        let is_load = inst.bit(22);
        let imm7 = inst.imm7_sext();
        let rt2 = Reg::from_u32(inst.rt2());
        let rn = Reg::from_u32(inst.rn());
        let rt = Reg::from_u32(inst.rd());

        let (datasize, signed) = match opc {
            0 => (32, false),
            1 => (32, true), // LDPSW
            2 => (64, false),
            _ => return self.unallocated_encoding(),
        };

        let regsize = if signed { 64 } else { datasize };
        let dbytes = datasize / 8;
        let scale = if datasize == 64 { 3usize } else { 2 };
        let offset = imm7 << scale;

        let base = self.base_address(rn);
        let offset_val = self.ir.ir().imm64(offset as u64);

        let address = if not_postindex {
            self.addr_add(base, offset_val)
        } else {
            base
        };

        let dbytes_val = self.ir.ir().imm64(dbytes as u64);

        if is_load {
            let data1 = self.mem_read(address, dbytes, AccType::Normal);
            let ext1 = if signed {
                self.sign_or_zero_extend(data1, datasize, regsize, true)
            } else {
                data1
            };
            self.set_x(regsize, rt, ext1);

            let addr2 = self.addr_add(address, dbytes_val);
            let data2 = self.mem_read(addr2, dbytes, AccType::Normal);
            let ext2 = if signed {
                self.sign_or_zero_extend(data2, datasize, regsize, true)
            } else {
                data2
            };
            self.set_x(regsize, rt2, ext2);
        } else {
            let data1 = self.x(datasize, rt);
            self.mem_write(address, data1, dbytes, AccType::Normal);

            let addr2 = self.addr_add(address, dbytes_val);
            let data2 = self.x(datasize, rt2);
            self.mem_write(addr2, data2, dbytes, AccType::Normal);
        }

        if wback || !not_postindex {
            let wb_addr = if not_postindex {
                address
            } else {
                self.addr_add(address, offset_val)
            };
            self.writeback_address(rn, wb_addr);
        }

        true
    }

    /// STP/LDP (SIMD&FP) - Store/Load pair of FP/SIMD registers
    pub fn stp_ldp_fpsimd(&mut self, inst: &DecodedInst) -> bool {
        // opc field is at bits[31:30] (= `size()`), not bits[30:29] (= `opc()`).
        // For SIMD&FP LDP/STP: 00=32-bit, 01=64-bit, 10=128-bit, 11=reserved.
        let opc = inst.size();
        let not_postindex = inst.bit(24);
        let wback = inst.bit(23);
        let is_load = inst.bit(22);
        let imm7 = inst.imm7_sext();
        let rt2 = Vec::from_u32(inst.rt2());
        let rn = Reg::from_u32(inst.rn());
        let rt = Vec::from_u32(inst.rd());

        let datasize = match opc {
            0 => 32,
            1 => 64,
            2 => 128,
            _ => return self.unallocated_encoding(),
        };

        let dbytes = datasize / 8;
        let scale = match opc {
            0 => 2usize,
            1 => 3,
            2 => 4,
            _ => unreachable!(),
        };
        let offset = imm7 << scale;

        let base = self.base_address(rn);
        let offset_val = self.ir.ir().imm64(offset as u64);

        let address = if not_postindex {
            self.addr_add(base, offset_val)
        } else {
            base
        };

        let dbytes_val = self.ir.ir().imm64(dbytes as u64);

        if is_load {
            let data1 = self.mem_read(address, dbytes, AccType::Vec);
            self.v_scalar_write(datasize, rt, data1);

            let addr2 = self.addr_add(address, dbytes_val);
            let data2 = self.mem_read(addr2, dbytes, AccType::Vec);
            self.v_scalar_write(datasize, rt2, data2);
        } else {
            let data1 = self.v_scalar_read(datasize, rt);
            self.mem_write(address, data1, dbytes, AccType::Vec);

            let addr2 = self.addr_add(address, dbytes_val);
            let data2 = self.v_scalar_read(datasize, rt2);
            self.mem_write(addr2, data2, dbytes, AccType::Vec);
        }

        if wback || !not_postindex {
            let wb_addr = if not_postindex {
                address
            } else {
                self.addr_add(address, offset_val)
            };
            self.writeback_address(rn, wb_addr);
        }

        true
    }

    /// STNP/LDNP (general) - Non-temporal pair
    pub fn stnp_ldnp_gen(&mut self, inst: &DecodedInst) -> bool {
        self.stp_ldp_gen(inst)
    }

    /// STNP/LDNP (SIMD&FP) - Non-temporal pair
    pub fn stnp_ldnp_fpsimd(&mut self, inst: &DecodedInst) -> bool {
        self.stp_ldp_fpsimd(inst)
    }
}
