//! Port of upstream
//! `dynarmic/frontend/A64/translate/impl/simd_extract.cpp`.
//!
//! EXT (vector): concatenate two vectors and extract a vector starting
//! at a byte position.

use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::Vec;

impl<'a> TranslatorVisitor<'a> {
    /// EXT (vector). Encoding: `0Q101110000mmmmm0iiii0nnnnnddddd` with
    /// `i` = imm4 (4 bits at [14:11]).
    pub fn ext(&mut self, inst: &DecodedInst) -> bool {
        let q = inst.bit(30);
        let imm4 = inst.bits(14, 11);
        if !q && (imm4 & 0b1000) != 0 {
            return self.reserved_value();
        }
        let vm = Vec::from_u32(inst.bits(20, 16));
        let vn = Vec::from_u32(inst.bits(9, 5));
        let vd = Vec::from_u32(inst.rd());

        let datasize = if q { 128 } else { 64 };
        let position = (imm4 as u8) << 3;

        let lo = self.v_read(datasize, vn);
        let hi = self.v_read(datasize, vm);
        let result = if datasize == 64 {
            self.ir.ir().vector_extract_lower(lo, hi, position)
        } else {
            self.ir.ir().vector_extract(lo, hi, position)
        };
        self.v_write(datasize, vd, result);
        true
    }
}
