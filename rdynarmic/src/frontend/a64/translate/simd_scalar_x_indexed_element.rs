//! Port of dynarmic/frontend/A64/translate/impl/simd_scalar_x_indexed_element.cpp.
//!
//! SIMD scalar x indexed element instructions.

use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Vec;

enum ExtraBehavior {
    None,
    Accumulate,
    Subtract,
    MultiplyExtended,
}

impl<'a> TranslatorVisitor<'a> {
    fn multiply_by_element(
        &mut self,
        sz: bool,
        l: u32,
        m: u32,
        vmlo: u32,
        h: u32,
        vn: Vec,
        vd: Vec,
        extra_behavior: ExtraBehavior,
    ) -> bool {
        if sz && l == 1 {
            return self.reserved_value();
        }

        let idxdsize = if h == 1 { 128 } else { 64 };
        let index = if sz { h } else { (h << 1) | l } as u8;
        let vm = Vec::from_u32((m << 4) | vmlo);
        let esize = if sz { 64 } else { 32 };

        let element = {
            let operand = self.v_scalar_read(idxdsize, vm);
            self.ir.ir().vector_get_element(esize, operand, index)
        };
        let mut operand1 = self.v_scalar_read(esize, vn);
        let result = match extra_behavior {
            ExtraBehavior::None => self.ir.ir().fp_mul(esize, operand1, element),
            ExtraBehavior::MultiplyExtended => self.ir.ir().fp_mulx(esize, operand1, element),
            ExtraBehavior::Accumulate | ExtraBehavior::Subtract => {
                if matches!(extra_behavior, ExtraBehavior::Subtract) {
                    operand1 = self.ir.ir().fp_neg(esize, operand1);
                }
                let operand2 = self.v_scalar_read(esize, vd);
                self.ir.ir().fp_mul_add(esize, operand2, operand1, element)
            }
        };

        self.v_scalar_write(esize, vd, result);
        true
    }

    fn multiply_by_element_half_precision(
        &mut self,
        l: u32,
        m: u32,
        vmlo: u32,
        h: u32,
        vn: Vec,
        vd: Vec,
        extra_behavior: ExtraBehavior,
    ) -> bool {
        let esize = 16;
        let idxdsize = if h == 1 { 128 } else { 64 };
        let index = ((h << 2) | (l << 1) | m) as u8;
        let vm = Vec::from_u32(vmlo);
        let operand = self.v_scalar_read(idxdsize, vm);
        let element = self.ir.ir().vector_get_element(esize, operand, index);
        let mut operand1 = self.v_scalar_read(esize, vn);
        if matches!(extra_behavior, ExtraBehavior::Subtract) {
            operand1 = self.ir.ir().fp_neg(esize, operand1);
        }
        let operand2 = self.v_scalar_read(esize, vd);
        let result = self.ir.ir().fp_mul_add(esize, operand2, operand1, element);
        self.v_scalar_write(esize, vd, result);
        true
    }

    pub fn fmla_elt_1(&mut self, inst: &DecodedInst) -> bool {
        let l = inst.bit(21) as u32;
        let m = inst.bit(20) as u32;
        let vmlo = inst.bits(19, 16);
        let h = inst.bit(11) as u32;
        let vn = Vec::from_u32(inst.rn());
        let vd = Vec::from_u32(inst.rd());
        self.multiply_by_element_half_precision(l, m, vmlo, h, vn, vd, ExtraBehavior::Accumulate)
    }

    pub fn fmla_elt_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let l = inst.bit(21) as u32;
        let m = inst.bit(20) as u32;
        let vmlo = inst.bits(19, 16);
        let h = inst.bit(11) as u32;
        let vn = Vec::from_u32(inst.rn());
        let vd = Vec::from_u32(inst.rd());
        self.multiply_by_element(sz, l, m, vmlo, h, vn, vd, ExtraBehavior::Accumulate)
    }

    pub fn fmls_elt_1(&mut self, inst: &DecodedInst) -> bool {
        let l = inst.bit(21) as u32;
        let m = inst.bit(20) as u32;
        let vmlo = inst.bits(19, 16);
        let h = inst.bit(11) as u32;
        let vn = Vec::from_u32(inst.rn());
        let vd = Vec::from_u32(inst.rd());
        self.multiply_by_element_half_precision(l, m, vmlo, h, vn, vd, ExtraBehavior::Subtract)
    }

    pub fn fmls_elt_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let l = inst.bit(21) as u32;
        let m = inst.bit(20) as u32;
        let vmlo = inst.bits(19, 16);
        let h = inst.bit(11) as u32;
        let vn = Vec::from_u32(inst.rn());
        let vd = Vec::from_u32(inst.rd());
        self.multiply_by_element(sz, l, m, vmlo, h, vn, vd, ExtraBehavior::Subtract)
    }

    /// FMUL (scalar, by element), single/double.
    /// Upstream: `TranslatorVisitor::FMUL_elt_2`.
    pub fn fmul_elt_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let l = inst.bits(21, 21);
        let m = inst.bits(20, 20);
        let vmlo = inst.bits(19, 16);
        let h = inst.bits(11, 11);
        let vn = Vec::from_u32(inst.rn());
        let vd = Vec::from_u32(inst.rd());
        self.multiply_by_element(sz, l, m, vmlo, h, vn, vd, ExtraBehavior::None)
    }

    /// FMULX (scalar, by element), single/double.
    /// Upstream: `TranslatorVisitor::FMULX_elt_2`.
    pub fn fmulx_elt_2(&mut self, inst: &DecodedInst) -> bool {
        let sz = inst.bit(22);
        let l = inst.bits(21, 21);
        let m = inst.bits(20, 20);
        let vmlo = inst.bits(19, 16);
        let h = inst.bits(11, 11);
        let vn = Vec::from_u32(inst.rn());
        let vd = Vec::from_u32(inst.rd());
        self.multiply_by_element(sz, l, m, vmlo, h, vn, vd, ExtraBehavior::MultiplyExtended)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::a64::decoder::decode;
    use crate::ir::block::Block;
    use crate::ir::location::A64LocationDescriptor;
    use crate::ir::opcode::Opcode;
    use crate::ir::terminal::Terminal;

    fn translate_one(raw: u32) -> (Block, bool) {
        let decoded = decode(raw).expect("instruction should decode");
        let mut block = Block::new(A64LocationDescriptor::new(0x1000, 0, false).to_location());
        let mut visitor = TranslatorVisitor::new(
            &mut block,
            A64LocationDescriptor::new(0x1000, 0, false),
            crate::frontend::a64::translate::visitor::TranslationOptions::default(),
        );
        let should_continue = visitor.dispatch(&decoded);
        drop(visitor);
        (block, should_continue)
    }

    #[test]
    fn fmul_s_by_element_encoding_translates_without_interpret_terminal() {
        let (block, should_continue) = translate_one(0x5FA0_9010);
        assert!(should_continue);
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::FPMul32));
        assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
    }

    #[test]
    fn fmulx_s_by_element_translates_without_interpret_terminal() {
        // FMULX Sd, Sn, Vm.S[0] — sz=0 (esize=32).
        let (block, should_continue) = translate_one(0x7F82_9020);
        assert!(should_continue);
        assert!(block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::FPMulX32));
        assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
    }

    #[test]
    fn scalar_fmla_fmls_by_element_family_matches_upstream_opcodes() {
        let cases = [
            (0x5F87_1871, Opcode::FPMulAdd32),
            (0x5FC6_18A4, Opcode::FPMulAdd64),
            (0x5FA3_5041, Opcode::FPMulAdd32),
            (0x5F13_1841, Opcode::FPMulAdd16),
            (0x5F26_50A4, Opcode::FPMulAdd16),
        ];

        for (encoding, expected_opcode) in cases {
            let (block, should_continue) = translate_one(encoding);
            assert!(should_continue, "encoding 0x{encoding:08X}");
            assert!(
                block
                    .instructions
                    .iter()
                    .any(|instruction| instruction.opcode == expected_opcode),
                "encoding 0x{encoding:08X} did not emit {expected_opcode:?}"
            );
            assert!(!matches!(block.terminal, Terminal::Interpret { .. }));
        }
    }
}
