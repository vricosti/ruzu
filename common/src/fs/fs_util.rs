// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/common/fs/fs_util.h and zuyu/src/common/fs/fs_util.cpp
//! Filesystem utility functions for encoding conversions.
//!
//! In Rust, strings are always valid UTF-8, so the C++ distinctions between
//! `std::string`, `std::u8string`, `std::string_view`, and `std::u8string_view`
//! all collapse to `String` and `&str`. The buffer conversion functions still
//! serve a purpose: converting raw byte buffers to strings, stopping at the
//! first null terminator.

use std::path::Path;

/// Converts a UTF-8 encoded string to a UTF-8 string (identity in Rust).
///
/// Maps to upstream `ToU8String`.
pub fn to_u8_string(utf8_string: &str) -> String {
    utf8_string.to_string()
}

/// Converts a buffer of bytes to a UTF-8 encoded `String`.
/// Converts from the start of the buffer until the first encountered null terminator.
/// If no null terminator is found, converts the entire buffer.
///
/// Maps to upstream `BufferToU8String` / `BufferToUTF8String`.
pub fn buffer_to_utf8_string(buffer: &[u8]) -> String {
    let end = buffer.iter().position(|&b| b == 0).unwrap_or(buffer.len());
    String::from_utf8_lossy(&buffer[..end]).into_owned()
}

/// Converts a buffer of bytes to a UTF-8 encoded string slice.
/// Returns a `&str` view of the buffer up to the first null terminator.
/// If no null terminator is found, returns the entire buffer as a string.
///
/// Maps to upstream `BufferToU8StringView` / `BufferToUTF8StringView`.
///
/// Note: If the buffer contains invalid UTF-8, this will return up to the
/// first invalid byte or null terminator. For guaranteed results, use
/// `buffer_to_utf8_string` instead.
pub fn buffer_to_utf8_string_view(buffer: &[u8]) -> &str {
    let end = buffer.iter().position(|&b| b == 0).unwrap_or(buffer.len());
    std::str::from_utf8(&buffer[..end]).unwrap_or("")
}

/// Converts a filesystem path to a UTF-8 encoded `String`.
///
/// Maps to upstream `PathToUTF8String`.
pub fn path_to_utf8_string(path: &Path) -> String {
    path.to_string_lossy().into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_u8_string() {
        assert_eq!(to_u8_string("hello"), "hello");
        assert_eq!(to_u8_string(""), "");
    }

    #[test]
    fn test_buffer_to_utf8_string() {
        // Normal string with null terminator
        assert_eq!(
            buffer_to_utf8_string(&[b'h', b'e', b'l', b'l', b'o', 0, b'x']),
            "hello"
        );

        // No null terminator
        assert_eq!(
            buffer_to_utf8_string(&[b'h', b'e', b'l', b'l', b'o']),
            "hello"
        );

        // Empty buffer
        assert_eq!(buffer_to_utf8_string(&[]), "");

        // Just a null terminator
        assert_eq!(buffer_to_utf8_string(&[0]), "");
    }

    #[test]
    fn test_buffer_to_utf8_string_view() {
        let buf = b"hello\0world";
        assert_eq!(buffer_to_utf8_string_view(buf), "hello");

        let buf2 = b"hello";
        assert_eq!(buffer_to_utf8_string_view(buf2), "hello");
    }

    #[test]
    fn test_path_to_utf8_string() {
        let path = Path::new("/some/path/to/file.txt");
        assert_eq!(path_to_utf8_string(path), "/some/path/to/file.txt");
    }
}
