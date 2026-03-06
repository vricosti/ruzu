//! Port of zuyu/src/common/hex_util.h and zuyu/src/common/hex_util.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

/// Converts a single hex character to its numeric value (0-15).
///
/// Supports '0'-'9', 'A'-'F', 'a'-'f'.
#[inline]
pub const fn to_hex_nibble(c: u8) -> u8 {
    if c >= b'A' && c <= b'F' {
        c - b'A' + 10
    } else if c >= b'a' && c <= b'f' {
        c - b'a' + 10
    } else {
        // Assumes '0'-'9'
        c - b'0'
    }
}

/// Converts a hex string to a vector of bytes.
///
/// If `little_endian` is true, the bytes are reversed from their string order.
pub fn hex_string_to_vector(s: &str, little_endian: bool) -> Vec<u8> {
    let bytes = s.as_bytes();
    let len = bytes.len() / 2;
    let mut out = vec![0u8; len];

    if little_endian {
        // Process from the end of the string
        let mut j = 0;
        let mut i = bytes.len().wrapping_sub(2);
        while j < len {
            out[j] = (to_hex_nibble(bytes[i]) << 4) | to_hex_nibble(bytes[i + 1]);
            j += 1;
            i = i.wrapping_sub(2);
        }
    } else {
        for i in 0..len {
            out[i] = (to_hex_nibble(bytes[i * 2]) << 4) | to_hex_nibble(bytes[i * 2 + 1]);
        }
    }

    out
}

/// Converts a hex string to a fixed-size byte array.
///
/// Panics if the string is too short (less than `N * 2` characters).
pub fn hex_string_to_array<const N: usize>(s: &str) -> [u8; N] {
    assert!(
        s.len() >= N * 2,
        "Invalid string size: expected at least {} chars, got {}",
        N * 2,
        s.len()
    );

    let bytes = s.as_bytes();
    let mut out = [0u8; N];
    for i in 0..N {
        out[i] = (to_hex_nibble(bytes[i * 2]) << 4) | to_hex_nibble(bytes[i * 2 + 1]);
    }
    out
}

/// Converts a hex string to a fixed-size byte array in little-endian order.
pub fn hex_string_to_array_le<const N: usize>(s: &str) -> [u8; N] {
    assert!(
        s.len() >= N * 2,
        "Invalid string size: expected at least {} chars, got {}",
        N * 2,
        s.len()
    );

    let bytes = s.as_bytes();
    let mut out = [0u8; N];
    let mut si = N * 2 - 2;
    for i in 0..N {
        out[i] = (to_hex_nibble(bytes[si]) << 4) | to_hex_nibble(bytes[si + 1]);
        si = si.wrapping_sub(2);
    }
    out
}

/// Converts a byte slice to a hex string.
///
/// If `upper` is true, uses uppercase hex digits (A-F), otherwise lowercase (a-f).
pub fn hex_to_string(data: &[u8], upper: bool) -> String {
    let mut out = String::with_capacity(data.len() * 2);
    for &byte in data {
        if upper {
            out.push_str(&format!("{:02X}", byte));
        } else {
            out.push_str(&format!("{:02x}", byte));
        }
    }
    out
}

/// Convenience: convert a 32-char hex string to a 16-byte array.
pub fn as_array_16(data: &str) -> [u8; 16] {
    hex_string_to_array::<16>(data)
}

/// Convenience: convert a 64-char hex string to a 32-byte array.
pub fn as_array_32(data: &str) -> [u8; 32] {
    hex_string_to_array::<32>(data)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_hex_nibble() {
        assert_eq!(to_hex_nibble(b'0'), 0);
        assert_eq!(to_hex_nibble(b'9'), 9);
        assert_eq!(to_hex_nibble(b'a'), 10);
        assert_eq!(to_hex_nibble(b'f'), 15);
        assert_eq!(to_hex_nibble(b'A'), 10);
        assert_eq!(to_hex_nibble(b'F'), 15);
    }

    #[test]
    fn test_hex_string_to_vector() {
        assert_eq!(hex_string_to_vector("deadbeef", false), vec![0xde, 0xad, 0xbe, 0xef]);
        assert_eq!(hex_string_to_vector("0102", true), vec![0x02, 0x01]);
    }

    #[test]
    fn test_hex_to_string() {
        assert_eq!(hex_to_string(&[0xde, 0xad], true), "DEAD");
        assert_eq!(hex_to_string(&[0xde, 0xad], false), "dead");
    }

    #[test]
    fn test_hex_string_to_array() {
        let arr: [u8; 4] = hex_string_to_array("deadbeef");
        assert_eq!(arr, [0xde, 0xad, 0xbe, 0xef]);
    }
}
