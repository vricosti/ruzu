// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: Apache-2.0

//! Port of `video_core/textures/astc.h` and `astc.cpp`.
//!
//! ASTC (Adaptive Scalable Texture Compression) block decoder.
//! The original C++ code is derived from the FasTC reference implementation
//! (University of North Carolina at Chapel Hill).
//!
//! This module decodes ASTC-encoded texture data into RGBA8 output.

// ── Bit stream helpers ───────────────────────────────────────────────────────

/// Input bit stream that reads bits from a byte slice in LSB-first order.
///
/// Port of the anonymous `InputBitStream` class in `astc.cpp`.
struct InputBitStream<'a> {
    data: &'a [u8],
    byte_pos: usize,
    next_bit: usize,
    bits_read: usize,
}

impl<'a> InputBitStream<'a> {
    fn new(data: &'a [u8], start_offset: usize) -> Self {
        Self {
            data,
            byte_pos: start_offset / 8,
            next_bit: start_offset % 8,
            bits_read: 0,
        }
    }

    fn bits_read(&self) -> usize {
        self.bits_read
    }

    fn read_bit(&mut self) -> bool {
        if self.bits_read >= self.data.len() * 8 {
            return false;
        }
        let bit = (self.data[self.byte_pos] >> self.next_bit) & 1 != 0;
        self.next_bit += 1;
        while self.next_bit >= 8 {
            self.next_bit -= 8;
            self.byte_pos += 1;
        }
        self.bits_read += 1;
        bit
    }

    fn read_bits(&mut self, n_bits: usize) -> u32 {
        let mut ret = 0u32;
        for i in 0..n_bits {
            ret |= (self.read_bit() as u32) << i;
        }
        ret
    }
}

/// Output bit stream that writes bits to a byte slice in LSB-first order.
///
/// Port of the anonymous `OutputBitStream` class in `astc.cpp`.
struct OutputBitStream<'a> {
    data: &'a mut [u8],
    byte_pos: usize,
    num_bits: usize,
    bits_written: usize,
    next_bit: usize,
}

impl<'a> OutputBitStream<'a> {
    fn new(data: &'a mut [u8], num_bits: usize, start_offset: usize) -> Self {
        Self {
            data,
            byte_pos: start_offset / 8,
            num_bits,
            bits_written: 0,
            next_bit: start_offset % 8,
        }
    }

    #[allow(dead_code)]
    fn bits_written(&self) -> usize {
        self.bits_written
    }

    fn write_bit(&mut self, b: bool) {
        if self.bits_written >= self.num_bits {
            return;
        }
        let mask = 1u8 << self.next_bit;
        // Clear the bit
        self.data[self.byte_pos] &= !mask;
        // Write the bit if set
        if b {
            self.data[self.byte_pos] |= mask;
        }
        self.next_bit += 1;
        if self.next_bit >= 8 {
            self.byte_pos += 1;
            self.next_bit = 0;
        }
        self.bits_written += 1;
    }

    fn write_bits(&mut self, val: u32, n_bits: u32) {
        for i in 0..n_bits {
            self.write_bit(((val >> i) & 1) != 0);
        }
    }

    #[allow(dead_code)]
    fn write_bits_r(&mut self, val: u32, n_bits: u32) {
        for i in 0..n_bits {
            self.write_bit(((val >> (n_bits - i - 1)) & 1) != 0);
        }
    }
}

// ── Integer encoding types ───────────────────────────────────────────────────

/// Encoding type for bounded integer sequences in ASTC.
///
/// Port of `IntegerEncoding` enum from `astc.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IntegerEncoding {
    JustBits,
    Quint,
    Trit,
}

/// A single integer value decoded from a bounded integer sequence.
///
/// Port of `IntegerEncodedValue` struct from `astc.cpp`.
#[derive(Debug, Clone, Copy)]
struct IntegerEncodedValue {
    encoding: IntegerEncoding,
    num_bits: u32,
    bit_value: u32,
    /// Holds either quint_value or trit_value (union in C++).
    quint_trit_value: u32,
}

impl IntegerEncodedValue {
    const fn new(encoding: IntegerEncoding, num_bits: u32) -> Self {
        Self {
            encoding,
            num_bits,
            bit_value: 0,
            quint_trit_value: 0,
        }
    }

    const fn matches_encoding(&self, other: &IntegerEncodedValue) -> bool {
        self.encoding as u32 == other.encoding as u32 && self.num_bits == other.num_bits
    }

    /// Returns the number of bits required to encode `num_vals` values.
    fn get_bit_length(&self, num_vals: u32) -> u32 {
        let mut total_bits = self.num_bits * num_vals;
        match self.encoding {
            IntegerEncoding::Trit => {
                total_bits += (num_vals * 8 + 4) / 5;
            }
            IntegerEncoding::Quint => {
                total_bits += (num_vals * 7 + 2) / 3;
            }
            IntegerEncoding::JustBits => {}
        }
        total_bits
    }
}

/// Port of `CreateEncoding(u32)` from `astc.cpp`.
const fn create_encoding(mut max_value: u32) -> IntegerEncodedValue {
    while max_value > 0 {
        let check = max_value + 1;
        // Is max_value a power of two minus one?
        if (check & (check - 1)) == 0 {
            return IntegerEncodedValue::new(IntegerEncoding::JustBits, max_value.count_ones());
        }
        // Is max_value of the type 3*2^n - 1?
        if check % 3 == 0 && ((check / 3) & ((check / 3) - 1)) == 0 {
            return IntegerEncodedValue::new(IntegerEncoding::Trit, (check / 3 - 1).count_ones());
        }
        // Is max_value of the type 5*2^n - 1?
        if check % 5 == 0 && ((check / 5) & ((check / 5) - 1)) == 0 {
            return IntegerEncodedValue::new(IntegerEncoding::Quint, (check / 5 - 1).count_ones());
        }
        max_value -= 1;
    }
    IntegerEncodedValue::new(IntegerEncoding::JustBits, 0)
}

/// Pre-computed encoding table for max values 0..255.
///
/// Port of `ASTC_ENCODINGS_VALUES` from `astc.cpp`.
const ASTC_ENCODINGS_VALUES: [IntegerEncodedValue; 256] = {
    let mut arr = [IntegerEncodedValue::new(IntegerEncoding::JustBits, 0); 256];
    let mut i = 0usize;
    while i < 256 {
        arr[i] = create_encoding(i as u32);
        i += 1;
    }
    arr
};

// ── Public API ───────────────────────────────────────────────────────────────

/// Decompress ASTC-encoded texture data into RGBA8 output.
///
/// Port of `Tegra::Texture::ASTC::Decompress`.
///
/// # Parameters
/// - `data`: ASTC compressed input data
/// - `width`, `height`, `depth`: texture dimensions in pixels
/// - `block_width`, `block_height`: ASTC block dimensions
/// - `output`: pre-allocated RGBA8 output buffer
pub fn decompress(
    _data: &[u8],
    _width: u32,
    _height: u32,
    _depth: u32,
    _block_width: u32,
    _block_height: u32,
    _output: &mut [u8],
) {
    // The full ASTC decompressor is ~1200 lines of bit manipulation.
    // Ported structurally; bodies use todo!() pending full implementation.
    todo!("ASTC decompression not yet implemented")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_bit_stream_basic() {
        let data = [0b10110100u8, 0b11001010u8];
        let mut stream = InputBitStream::new(&data, 0);
        // LSB first: bit0 = 0, bit1 = 0, bit2 = 1, bit3 = 0, bit4 = 1, bit5 = 1, bit6 = 0, bit7 = 1
        assert!(!stream.read_bit()); // bit 0
        assert!(!stream.read_bit()); // bit 1
        assert!(stream.read_bit()); // bit 2
        assert_eq!(stream.bits_read(), 3);
    }

    #[test]
    fn create_encoding_power_of_two() {
        let enc = create_encoding(255);
        assert_eq!(enc.encoding, IntegerEncoding::JustBits);
        assert_eq!(enc.num_bits, 8);
    }

    #[test]
    fn create_encoding_trit() {
        // 2 = 3*1 - 1, so trit with 0 extra bits
        let enc = create_encoding(2);
        assert_eq!(enc.encoding, IntegerEncoding::Trit);
        assert_eq!(enc.num_bits, 0);
    }

    #[test]
    fn create_encoding_quint() {
        // 4 = 5*1 - 1, so quint with 0 extra bits
        let enc = create_encoding(4);
        assert_eq!(enc.encoding, IntegerEncoding::Quint);
        assert_eq!(enc.num_bits, 0);
    }

    #[test]
    fn encodings_table_populated() {
        assert_eq!(ASTC_ENCODINGS_VALUES.len(), 256);
        // Entry 0 should be JustBits with 0 bits
        assert_eq!(ASTC_ENCODINGS_VALUES[0].encoding, IntegerEncoding::JustBits);
        assert_eq!(ASTC_ENCODINGS_VALUES[0].num_bits, 0);
    }

    #[test]
    fn integer_encoded_value_bit_length() {
        // JustBits with 8 bits, 10 values = 80 bits
        let enc = IntegerEncodedValue::new(IntegerEncoding::JustBits, 8);
        assert_eq!(enc.get_bit_length(10), 80);

        // Trit with 4 bits, 5 values = 5*4 + (5*8+4)/5 = 20 + 8 = 28
        let enc_trit = IntegerEncodedValue::new(IntegerEncoding::Trit, 4);
        assert_eq!(enc_trit.get_bit_length(5), 28);
    }
}
