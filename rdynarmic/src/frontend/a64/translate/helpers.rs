use crate::frontend::a64::translate::visitor::TranslatorVisitor;
use crate::frontend::a64::types::ShiftType;

/// Result of DecodeBitMasks.
pub struct BitMasks {
    pub wmask: u64,
    pub tmask: u64,
}

/// Decode bitmask immediates for logical and bitfield instructions.
/// See ARM ARM "DecodeBitMasks" pseudocode.
pub fn decode_bit_masks(imm_n: bool, imms: u32, immr: u32, immediate: bool) -> Option<BitMasks> {
    // Determine element size
    let combined = if imm_n {
        (1u32 << 6) | (!imms & 0x3F)
    } else {
        !imms & 0x3F
    };
    let len = 31u32.checked_sub(combined.leading_zeros())?;
    if len == 0 && !imm_n {
        return None;
    }
    let len = if imm_n { 6 } else { highest_set_bit(combined)? };

    if len < 1 {
        return None;
    }

    let levels = (1u64 << len) - 1;
    let s = (imms as u64) & levels;
    let r = (immr as u64) & levels;

    // For immediate, all-ones S is reserved
    if immediate && s == levels {
        return None;
    }

    let d = (s.wrapping_sub(r)) & levels;
    let esize = 1u64 << len;
    let welem = ones(s + 1);
    let telem = ones(d + 1);

    let wmask = replicate(
        rotate_right_bits(welem, r as u32, esize as u32),
        esize as u32,
    );
    let tmask = replicate(telem, esize as u32);

    Some(BitMasks { wmask, tmask })
}

/// Find the highest set bit position (0-indexed).
fn highest_set_bit(value: u32) -> Option<u32> {
    if value == 0 {
        None
    } else {
        Some(31 - value.leading_zeros())
    }
}

/// Create a mask of `n` ones.
fn ones(n: u64) -> u64 {
    if n >= 64 {
        u64::MAX
    } else if n == 0 {
        0
    } else {
        (1u64 << n) - 1
    }
}

/// Rotate `value` right by `amount` bits within an `esize`-bit element.
fn rotate_right_bits(value: u64, amount: u32, esize: u32) -> u64 {
    if esize == 0 || amount == 0 {
        return value;
    }
    let amount = amount % esize;
    let mask = ones(esize as u64);
    let value = value & mask;
    ((value >> amount) | (value << (esize - amount))) & mask
}

/// Replicate an `esize`-bit pattern to fill 64 bits.
fn replicate(value: u64, esize: u32) -> u64 {
    if esize == 0 {
        return 0;
    }
    let mask = ones(esize as u64);
    let value = value & mask;
    let mut result = 0u64;
    let mut shift = 0u32;
    while shift < 64 {
        result |= value << shift;
        shift += esize;
    }
    result
}

use crate::ir::value::Value;

impl<'a> TranslatorVisitor<'a> {
    pub(crate) fn fp_datasize(&self, ftype: u32) -> Option<usize> {
        match ftype {
            0 => Some(32),
            1 => Some(64),
            3 => Some(16),
            _ => None,
        }
    }

    /// Apply a barrel shift to a register value.
    /// Used by data processing (shifted register) instructions.
    pub fn shift_reg(
        &mut self,
        datasize: usize,
        reg_val: Value,
        shift: ShiftType,
        amount: Value,
    ) -> Value {
        // Create carry_in once to avoid double-borrow
        let carry_in = self.ir.ir().imm1(false);
        match (datasize, shift) {
            (32, ShiftType::LSL) => self
                .ir
                .ir()
                .logical_shift_left_32(reg_val, amount, carry_in),
            (64, ShiftType::LSL) => self.ir.ir().logical_shift_left_64(reg_val, amount),
            (32, ShiftType::LSR) => self
                .ir
                .ir()
                .logical_shift_right_32(reg_val, amount, carry_in),
            (64, ShiftType::LSR) => self.ir.ir().logical_shift_right_64(reg_val, amount),
            (32, ShiftType::ASR) => self
                .ir
                .ir()
                .arithmetic_shift_right_32(reg_val, amount, carry_in),
            (64, ShiftType::ASR) => self.ir.ir().arithmetic_shift_right_64(reg_val, amount),
            (32, ShiftType::ROR) => self.ir.ir().rotate_right_32(reg_val, amount, carry_in),
            (64, ShiftType::ROR) => self.ir.ir().rotate_right_64(reg_val, amount),
            _ => panic!("Invalid shift for datasize {}", datasize),
        }
    }

    /// Extend and shift a register value.
    /// Used by ADD_ext, SUB_ext, etc.
    pub fn extend_reg(&mut self, datasize: usize, reg_val: Value, option: u32, shift: u8) -> Value {
        let extended = match option {
            0 => {
                // UXTB
                let word = self.ir.ir().least_significant_word(reg_val);
                let byte = self.ir.ir().least_significant_byte(word);
                match datasize {
                    32 => self.ir.ir().zero_extend_byte_to_word(byte),
                    64 => self.ir.ir().zero_extend_byte_to_long(byte),
                    _ => panic!(),
                }
            }
            1 => {
                // UXTH
                let word = self.ir.ir().least_significant_word(reg_val);
                let half = self.ir.ir().least_significant_half(word);
                match datasize {
                    32 => self.ir.ir().zero_extend_half_to_word(half),
                    64 => self.ir.ir().zero_extend_half_to_long(half),
                    _ => panic!(),
                }
            }
            2 => {
                // UXTW
                match datasize {
                    32 => reg_val, // already 32-bit
                    64 => {
                        let w = self.ir.ir().least_significant_word(reg_val);
                        self.ir.ir().zero_extend_word_to_long(w)
                    }
                    _ => panic!(),
                }
            }
            3 => {
                // UXTX
                reg_val // 64-bit, no extension
            }
            4 => {
                // SXTB
                let word = self.ir.ir().least_significant_word(reg_val);
                let byte = self.ir.ir().least_significant_byte(word);
                match datasize {
                    32 => self.ir.ir().sign_extend_byte_to_word(byte),
                    64 => self.ir.ir().sign_extend_byte_to_long(byte),
                    _ => panic!(),
                }
            }
            5 => {
                // SXTH
                let word = self.ir.ir().least_significant_word(reg_val);
                let half = self.ir.ir().least_significant_half(word);
                match datasize {
                    32 => self.ir.ir().sign_extend_half_to_word(half),
                    64 => self.ir.ir().sign_extend_half_to_long(half),
                    _ => panic!(),
                }
            }
            6 => {
                // SXTW
                match datasize {
                    32 => reg_val,
                    64 => {
                        let w = self.ir.ir().least_significant_word(reg_val);
                        self.ir.ir().sign_extend_word_to_long(w)
                    }
                    _ => panic!(),
                }
            }
            7 => {
                // SXTX
                reg_val
            }
            _ => panic!("Invalid extend option {}", option),
        };

        if shift > 0 {
            let shift_val = self.ir.ir().imm8(shift);
            let carry_in = self.ir.ir().imm1(false);
            match datasize {
                32 => self
                    .ir
                    .ir()
                    .logical_shift_left_32(extended, shift_val, carry_in),
                64 => self.ir.ir().logical_shift_left_64(extended, shift_val),
                _ => panic!(),
            }
        } else {
            extended
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode_bit_masks_basic() {
        // AND X0, X0, #0xFF => N=1 (64-bit element), imms=0b000111, immr=0
        let masks = decode_bit_masks(true, 0b000111, 0, true);
        assert!(masks.is_some());
        let m = masks.unwrap();
        assert_eq!(m.wmask, 0x0000_0000_0000_00FF);

        // N=0, imms=0b000111 => 32-bit element, replicates: 0x00FF_0000_00FF
        let masks2 = decode_bit_masks(false, 0b000111, 0, true);
        assert!(masks2.is_some());
        let m2 = masks2.unwrap();
        assert_eq!(m2.wmask, 0x0000_00FF_0000_00FF);
    }

    #[test]
    fn test_decode_bit_masks_64bit() {
        // N=1, imms=0b111100, immr=0 => 64-bit element, 0x1FFF_FFFF_FFFF_FFFF
        let masks = decode_bit_masks(true, 0b111100, 0, true);
        assert!(masks.is_some());
        let m = masks.unwrap();
        // S=60, R=0, d=60, esize=64
        // welem = ones(61) = 0x1FFF_FFFF_FFFF_FFFF
        assert_eq!(m.wmask, 0x1FFF_FFFF_FFFF_FFFF);
    }

    #[test]
    fn test_decode_bit_masks_all_ones_reserved() {
        // N=1, imms=0b111111, immr=0, immediate=true => reserved
        let masks = decode_bit_masks(true, 0b111111, 0, true);
        assert!(masks.is_none());
    }

    #[test]
    fn test_replicate() {
        assert_eq!(replicate(0xFF, 8), 0xFFFF_FFFF_FFFF_FFFF);
        assert_eq!(replicate(0x0F, 8), 0x0F0F_0F0F_0F0F_0F0F);
        assert_eq!(replicate(0xFFFF, 16), 0xFFFF_FFFF_FFFF_FFFF);
    }
}
