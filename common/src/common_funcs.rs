// SPDX-FileCopyrightText: 2026 reden contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Rust counterparts of the non-macro helpers in upstream `common/common_funcs.h`.

/// Construct a four-byte little-endian magic value.
///
/// Maps to upstream `Common::MakeMagic(char, char, char, char)`.
#[must_use]
pub const fn make_magic(a: u8, b: u8, c: u8, d: u8) -> u32 {
    (a as u32) | ((b as u32) << 8) | ((c as u32) << 16) | ((d as u32) << 24)
}

/// Construct an eight-byte little-endian magic value.
///
/// Rust cannot overload `make_magic`, so this maps to upstream's eight-argument
/// `Common::MakeMagic` overload under a width-qualified name.
#[must_use]
pub const fn make_magic_64(a: u8, b: u8, c: u8, d: u8, e: u8, f: u8, g: u8, h: u8) -> u64 {
    (a as u64)
        | ((b as u64) << 8)
        | ((c as u64) << 16)
        | ((d as u64) << 24)
        | ((e as u64) << 32)
        | ((f as u64) << 40)
        | ((g as u64) << 48)
        | ((h as u64) << 56)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn magic_values_match_little_endian_byte_order() {
        assert_eq!(make_magic(b'N', b'S', b'O', b'0'), 0x304f_534e);
        assert_eq!(
            make_magic_64(b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8'),
            0x3837_3635_3433_3231
        );
    }
}
