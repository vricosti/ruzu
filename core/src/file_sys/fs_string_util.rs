// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fs_string_util.h

/// Compute the length of a null-terminated byte slice.
/// Matches C++ `Strlen<T>`.
pub fn strlen(s: &[u8]) -> usize {
    s.iter().position(|&b| b == 0).unwrap_or(s.len())
}

/// Compute the length of a null-terminated byte slice, up to `count` bytes.
/// Matches C++ `Strnlen<T>`.
pub fn strnlen(s: &[u8], count: usize) -> usize {
    let limit = count.min(s.len());
    s[..limit]
        .iter()
        .position(|&b| b == 0)
        .unwrap_or(limit)
}

/// Compare two null-terminated byte slices up to `count` bytes.
/// Returns negative, zero, or positive like C `strncmp`.
/// Matches C++ `Strncmp<T>`.
pub fn strncmp(lhs: &[u8], rhs: &[u8], count: usize) -> i32 {
    if count == 0 {
        return 0;
    }

    let mut remaining = count;
    let mut li = 0;
    let mut ri = 0;

    loop {
        let l = if li < lhs.len() { lhs[li] } else { 0 };
        let r = if ri < rhs.len() { rhs[ri] } else { 0 };

        if l != r || l == 0 {
            return l as i32 - r as i32;
        }

        li += 1;
        ri += 1;
        remaining -= 1;
        if remaining == 0 {
            return 0;
        }
    }
}

/// Copy a null-terminated string from `src` into `dst`, ensuring null termination.
/// Returns the length of `src` (not including null terminator).
/// Matches C++ `Strlcpy<T>`.
pub fn strlcpy(dst: &mut [u8], src: &[u8], count: usize) -> usize {
    let actual_count = count.min(dst.len());

    if actual_count > 0 {
        let mut i = 0;
        let max_copy = actual_count - 1; // Leave room for null terminator
        while i < max_copy {
            let b = if i < src.len() { src[i] } else { 0 };
            if b == 0 {
                break;
            }
            dst[i] = b;
            i += 1;
        }
        dst[i] = 0;
    }

    // Return the full length of src (up to its null terminator)
    strlen(src)
}

/// Character encoding result values.
/// Corresponds to C++ `CharacterEncodingResult` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum CharacterEncodingResult {
    Success = 0,
    InsufficientLength = 1,
    InvalidFormat = 2,
}

/// UTF-8 byte count lookup table.
/// Corresponds to C++ `CharacterEncodingHelper::Utf8NBytesInnerTable`.
#[rustfmt::skip]
const UTF8_N_BYTES_INNER_TABLE: [i8; 0x100 + 1] = [
    -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1,  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1,  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1,  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1,  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2,  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
    3,  3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 7, 8,
];

/// Get the number of bytes in a UTF-8 character given its first byte.
/// Returns the value from the lookup table (0 = continuation, 1-4 = valid start byte count).
fn get_utf8_n_bytes(byte: u8) -> i8 {
    UTF8_N_BYTES_INNER_TABLE[1 + byte as usize]
}

/// Convert a UTF-8 character sequence to a UTF-32 code point.
/// Corresponds to C++ `ConvertCharacterUtf8ToUtf32`.
pub fn convert_character_utf8_to_utf32(src: &[u8]) -> (CharacterEncodingResult, u32) {
    if src.is_empty() {
        return (CharacterEncodingResult::InvalidFormat, 0);
    }

    match get_utf8_n_bytes(src[0]) {
        1 => (CharacterEncodingResult::Success, src[0] as u32),
        2 => {
            if src.len() < 2 {
                return (CharacterEncodingResult::InvalidFormat, 0);
            }
            if (src[0] as u32 & 0x1E) != 0 && get_utf8_n_bytes(src[1]) == 0 {
                let c = ((src[0] as u32 & 0x1F) << 6) | (src[1] as u32 & 0x3F);
                (CharacterEncodingResult::Success, c)
            } else {
                (CharacterEncodingResult::InvalidFormat, 0)
            }
        }
        3 => {
            if src.len() < 3 {
                return (CharacterEncodingResult::InvalidFormat, 0);
            }
            if get_utf8_n_bytes(src[1]) == 0 && get_utf8_n_bytes(src[2]) == 0 {
                let c = ((src[0] as u32 & 0xF) << 12)
                    | ((src[1] as u32 & 0x3F) << 6)
                    | (src[2] as u32 & 0x3F);
                if (c & 0xF800) != 0 && (c & 0xF800) != 0xD800 {
                    return (CharacterEncodingResult::Success, c);
                }
            }
            (CharacterEncodingResult::InvalidFormat, 0)
        }
        4 => {
            if src.len() < 4 {
                return (CharacterEncodingResult::InvalidFormat, 0);
            }
            if get_utf8_n_bytes(src[1]) == 0
                && get_utf8_n_bytes(src[2]) == 0
                && get_utf8_n_bytes(src[3]) == 0
            {
                let c = ((src[0] as u32 & 0x7) << 18)
                    | ((src[1] as u32 & 0x3F) << 12)
                    | ((src[2] as u32 & 0x3F) << 6)
                    | (src[3] as u32 & 0x3F);
                if c >= 0x10000 && c < 0x110000 {
                    return (CharacterEncodingResult::Success, c);
                }
            }
            (CharacterEncodingResult::InvalidFormat, 0)
        }
        _ => (CharacterEncodingResult::InvalidFormat, 0),
    }
}

/// Pick out a single UTF-8 character from a string, advancing the position.
/// `dst` receives up to 4 bytes of the character (null-padded).
/// Returns the encoding result and the number of bytes consumed.
/// Corresponds to C++ `PickOutCharacterFromUtf8String`.
pub fn pick_out_character_from_utf8_string(src: &[u8], pos: usize) -> (CharacterEncodingResult, [u8; 4], usize) {
    let mut dst = [0u8; 4];

    if pos >= src.len() {
        return (CharacterEncodingResult::InvalidFormat, dst, 0);
    }

    let c = src[pos];
    match get_utf8_n_bytes(c) {
        1 => {
            dst[0] = src[pos];
            (CharacterEncodingResult::Success, dst, 1)
        }
        2 => {
            if pos + 1 >= src.len() {
                return (CharacterEncodingResult::InvalidFormat, dst, 0);
            }
            if (src[pos] as u32 & 0x1E) != 0 && get_utf8_n_bytes(src[pos + 1]) == 0 {
                dst[0] = src[pos];
                dst[1] = src[pos + 1];
                (CharacterEncodingResult::Success, dst, 2)
            } else {
                (CharacterEncodingResult::InvalidFormat, dst, 0)
            }
        }
        3 => {
            if pos + 2 >= src.len() {
                return (CharacterEncodingResult::InvalidFormat, dst, 0);
            }
            if get_utf8_n_bytes(src[pos + 1]) == 0 && get_utf8_n_bytes(src[pos + 2]) == 0 {
                let code = ((src[pos] as u32 & 0xF) << 12)
                    | ((src[pos + 1] as u32 & 0x3F) << 6)
                    | (src[pos + 2] as u32 & 0x3F);
                if (code & 0xF800) != 0 && (code & 0xF800) != 0xD800 {
                    dst[0] = src[pos];
                    dst[1] = src[pos + 1];
                    dst[2] = src[pos + 2];
                    return (CharacterEncodingResult::Success, dst, 3);
                }
            }
            (CharacterEncodingResult::InvalidFormat, dst, 0)
        }
        4 => {
            if pos + 3 >= src.len() {
                return (CharacterEncodingResult::InvalidFormat, dst, 0);
            }
            if get_utf8_n_bytes(src[pos + 1]) == 0
                && get_utf8_n_bytes(src[pos + 2]) == 0
                && get_utf8_n_bytes(src[pos + 3]) == 0
            {
                let code = ((src[pos] as u32 & 0x7) << 18)
                    | ((src[pos + 1] as u32 & 0x3F) << 12)
                    | ((src[pos + 2] as u32 & 0x3F) << 6)
                    | (src[pos + 3] as u32 & 0x3F);
                if code >= 0x10000 && code < 0x110000 {
                    dst[0] = src[pos];
                    dst[1] = src[pos + 1];
                    dst[2] = src[pos + 2];
                    dst[3] = src[pos + 3];
                    return (CharacterEncodingResult::Success, dst, 4);
                }
            }
            (CharacterEncodingResult::InvalidFormat, dst, 0)
        }
        _ => (CharacterEncodingResult::InvalidFormat, dst, 0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strlen() {
        assert_eq!(strlen(b"hello\0world"), 5);
        assert_eq!(strlen(b""), 0);
        assert_eq!(strlen(b"\0"), 0);
        assert_eq!(strlen(b"abc"), 3);
    }

    #[test]
    fn test_strnlen() {
        assert_eq!(strnlen(b"hello\0", 10), 5);
        assert_eq!(strnlen(b"hello\0", 3), 3);
        assert_eq!(strnlen(b"hello", 3), 3);
    }

    #[test]
    fn test_strncmp() {
        assert_eq!(strncmp(b"abc", b"abc", 3), 0);
        assert!(strncmp(b"abd", b"abc", 3) > 0);
        assert!(strncmp(b"abc", b"abd", 3) < 0);
        assert_eq!(strncmp(b"abc", b"abd", 2), 0);
        assert_eq!(strncmp(b"abc", b"abc", 0), 0);
    }

    #[test]
    fn test_strlcpy() {
        let mut dst = [0u8; 10];
        let len = strlcpy(&mut dst, b"hello", 10);
        assert_eq!(len, 5);
        assert_eq!(&dst[..6], b"hello\0");

        let mut dst2 = [0u8; 4];
        let len2 = strlcpy(&mut dst2, b"hello", 4);
        assert_eq!(len2, 5); // returns source length
        assert_eq!(&dst2[..4], b"hel\0");
    }

    #[test]
    fn test_convert_utf8_to_utf32_ascii() {
        let (result, cp) = convert_character_utf8_to_utf32(b"A");
        assert_eq!(result, CharacterEncodingResult::Success);
        assert_eq!(cp, 0x41);
    }

    #[test]
    fn test_convert_utf8_to_utf32_multibyte() {
        // U+00E9 (e-acute) = 0xC3 0xA9
        let (result, cp) = convert_character_utf8_to_utf32(&[0xC3, 0xA9]);
        assert_eq!(result, CharacterEncodingResult::Success);
        assert_eq!(cp, 0xE9);
    }
}
