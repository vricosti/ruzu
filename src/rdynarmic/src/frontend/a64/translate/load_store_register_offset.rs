use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::{AccType, Reg, Vec};

impl<'a> TranslatorVisitor<'a> {
    /// Shared decode+operation for the GPR "Load/store register (register
    /// offset)" group. Port of upstream `RegSharedDecodeAndOperation` in
    /// load_store_register_register_offset.cpp.
    ///
    /// The two decode patterns (`STRx_reg` = `zz111000o01…`, `LDRx_reg` =
    /// `zz111000o11…`) split on bit22 (= `opc_0`), so each handler receives
    /// only `opc_1` (bit23) and supplies its fixed `opc_0`. The FULL opc then
    /// selects STR(00)/LDR(01)/LDRSW-family(10)/signed-32(11). Crucially
    /// `opc=10` (LDRSW etc.) is routed to the STR pattern (bit22=0) and MUST be
    /// handled as a signed LOAD, not rejected — that omission made every
    /// `ldrsw x,[x,x,lsl#2]` (PIC jump tables) an UnallocatedEncoding, which
    /// crashed AArch64 titles at boot.
    fn reg_shared_decode_and_operation(
        &mut self,
        scale: usize,
        shift: u8,
        size: u32,
        opc_1: bool,
        opc_0: bool,
        rm: Reg,
        option: u32,
        rn: Reg,
        rt: Reg,
    ) -> bool {
        // Shared decode.
        let mut regsize: usize = 64;
        let mut signed_ = false;
        // memop: 0 = store, 1 = load, 2 = prefetch.
        let memop: u8;
        if !opc_1 {
            memop = if opc_0 { 1 } else { 0 };
            regsize = if size == 0b11 { 64 } else { 32 };
            signed_ = false;
        } else if size == 0b11 {
            memop = 2; // PREFETCH
            if opc_0 {
                return self.unallocated_encoding();
            }
        } else {
            memop = 1; // LOAD
            if size == 0b10 && opc_0 {
                return self.unallocated_encoding();
            }
            regsize = if opc_0 { 32 } else { 64 };
            signed_ = true;
        }

        let datasize = 8usize << scale;

        // Operation.
        let base = self.base_address(rn);
        let rm_val = self.x(64, rm);
        let offset = self.extend_reg(64, rm_val, option, shift);
        let address = self.addr_add(base, offset);

        match memop {
            0 => {
                // STORE: read `datasize` bits from Rt, write datasize/8 bytes.
                let read_size = if size == 3 { 64 } else { 32 };
                let data = self.x(read_size, rt);
                self.mem_write(address, data, datasize / 8, AccType::Normal);
            }
            1 => {
                // LOAD: read datasize/8 bytes, sign/zero-extend to regsize.
                let data = self.mem_read(address, datasize / 8, AccType::Normal);
                let extended = self.sign_or_zero_extend(data, datasize, regsize, signed_);
                self.set_x(regsize, rt, extended);
            }
            _ => {
                // PREFETCH: no-op (matches upstream TODO).
            }
        }
        true
    }

    /// STRx (register) — decode pattern `zz111000o01…` (opc_0 = 0).
    /// Covers STR (opc=00) and LDRSW-family (opc=10).
    pub fn strx_reg(&mut self, inst: &DecodedInst) -> bool {
        let size = inst.size();
        let opc_1 = inst.bit(23);
        let rm = Reg::from_u32(inst.rm());
        let option = inst.bits(15, 13);
        let s = inst.bit(12);
        let rn = Reg::from_u32(inst.rn());
        let rt = Reg::from_u32(inst.rd());

        if (option & 2) == 0 {
            return self.unallocated_encoding();
        }

        let scale = size as usize;
        let shift = if s { scale as u8 } else { 0 };
        self.reg_shared_decode_and_operation(scale, shift, size, opc_1, false, rm, option, rn, rt)
    }

    /// LDRx (register) — decode pattern `zz111000o11…` (opc_0 = 1).
    /// Covers LDR (opc=01) and signed-32 loads (opc=11).
    pub fn ldrx_reg(&mut self, inst: &DecodedInst) -> bool {
        let size = inst.size();
        let opc_1 = inst.bit(23);
        let rm = Reg::from_u32(inst.rm());
        let option = inst.bits(15, 13);
        let s = inst.bit(12);
        let rn = Reg::from_u32(inst.rn());
        let rt = Reg::from_u32(inst.rd());

        if (option & 2) == 0 {
            return self.unallocated_encoding();
        }

        let scale = size as usize;
        let shift = if s { scale as u8 } else { 0 };
        self.reg_shared_decode_and_operation(scale, shift, size, opc_1, true, rm, option, rn, rt)
    }

    /// STR (register, SIMD&FP)
    pub fn str_reg_fpsimd(&mut self, inst: &DecodedInst) -> bool {
        let size = inst.size();
        let opc_bit = inst.bit(23);
        let rm = Reg::from_u32(inst.rm());
        let option = inst.bits(15, 13);
        let s = inst.bit(12);
        let rn = Reg::from_u32(inst.rn());
        let rt = Vec::from_u32(inst.rd());

        if (option & 2) == 0 {
            return self.unallocated_encoding();
        }

        let scale = if opc_bit { 4 } else { size as usize };
        let datasize = 8usize << scale;
        let shift = if s { scale as u8 } else { 0 };

        let base = self.base_address(rn);
        let rm_val = self.x(64, rm);
        let offset = self.extend_reg(64, rm_val, option, shift);
        let address = self.addr_add(base, offset);

        let data = self.v_scalar_read(datasize, rt);
        self.mem_write(address, data, datasize / 8, AccType::Vec);
        true
    }

    /// LDR (register, SIMD&FP)
    pub fn ldr_reg_fpsimd(&mut self, inst: &DecodedInst) -> bool {
        let size = inst.size();
        let opc_bit = inst.bit(23);
        let rm = Reg::from_u32(inst.rm());
        let option = inst.bits(15, 13);
        let s = inst.bit(12);
        let rn = Reg::from_u32(inst.rn());
        let rt = Vec::from_u32(inst.rd());

        if (option & 2) == 0 {
            return self.unallocated_encoding();
        }

        let scale = if opc_bit { 4 } else { size as usize };
        let datasize = 8usize << scale;
        let shift = if s { scale as u8 } else { 0 };

        let base = self.base_address(rn);
        let rm_val = self.x(64, rm);
        let offset = self.extend_reg(64, rm_val, option, shift);
        let address = self.addr_add(base, offset);

        let data = self.mem_read(address, datasize / 8, AccType::Vec);
        self.v_scalar_write(datasize, rt, data);
        true
    }
}
