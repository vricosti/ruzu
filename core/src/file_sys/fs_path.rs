// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fs_path.h

use common::alignment::align_up;
use common::ResultCode;

use super::errors::*;
use super::fs_memory_management;
use super::fs_path_utility::string_traits::*;
use super::fs_path_utility::*;
use super::fs_string_util::*;

/// Write buffer alignment length.
const WRITE_BUFFER_ALIGNMENT_LENGTH: usize = 8;

/// Internal write buffer used by `Path`.
/// Owns a heap-allocated char buffer with a packed length+normalized flag.
/// Corresponds to C++ `Path::WriteBuffer`.
struct WriteBuffer {
    buffer: *mut u8,
    /// Low bit = is_normalized, remaining bits = length << 1.
    length_and_is_normalized: usize,
}

impl WriteBuffer {
    fn new() -> Self {
        Self {
            buffer: std::ptr::null_mut(),
            length_and_is_normalized: 0,
        }
    }

    fn reset_buffer(&mut self) {
        self.buffer = std::ptr::null_mut();
        self.set_length(0);
    }

    fn get(&self) -> *mut u8 {
        self.buffer
    }

    fn get_length(&self) -> usize {
        self.length_and_is_normalized >> 1
    }

    fn is_normalized(&self) -> bool {
        (self.length_and_is_normalized & 1) != 0
    }

    fn set_normalized(&mut self) {
        self.length_and_is_normalized |= 1;
    }

    fn set_not_normalized(&mut self) {
        self.length_and_is_normalized &= !1usize;
    }

    fn set_length(&mut self, size: usize) {
        self.length_and_is_normalized = (self.length_and_is_normalized & 1) | (size << 1);
    }

    fn make(length: usize) -> Self {
        if length == 0 {
            return Self::new();
        }
        unsafe {
            let ptr = fs_memory_management::allocate(length);
            if ptr.is_null() {
                Self::new()
            } else {
                let mut buf = Self {
                    buffer: ptr,
                    length_and_is_normalized: 0,
                };
                buf.set_length(length);
                buf
            }
        }
    }
}

impl Drop for WriteBuffer {
    fn drop(&mut self) {
        if !self.buffer.is_null() {
            unsafe {
                fs_memory_management::deallocate(self.buffer, self.get_length());
            }
            self.reset_buffer();
        }
    }
}

/// Helper to read a null-terminated string from a raw pointer as a byte slice.
/// # Safety
/// `ptr` must point to a valid null-terminated string.
unsafe fn raw_to_slice(ptr: *const u8, max_len: usize) -> &'static [u8] {
    if ptr.is_null() {
        return &[];
    }
    let mut len = 0;
    while len < max_len && *ptr.add(len) != 0 {
        len += 1;
    }
    std::slice::from_raw_parts(ptr, len + 1) // include the null terminator
}

/// Filesystem path type.
/// Owns an internal write buffer and a read pointer.
/// Corresponds to C++ `FileSys::Path`.
pub struct Path {
    /// Points to either the write buffer or a static/external string.
    str_ptr: *const u8,
    write_buffer: WriteBuffer,
}

// The C++ Path is non-copyable and non-moveable. We allow move in Rust for ergonomics,
// but not Clone.

impl Path {
    const EMPTY_PATH: &'static [u8] = b"\0";

    /// Create a new empty path.
    pub fn new() -> Self {
        Self {
            str_ptr: Self::EMPTY_PATH.as_ptr(),
            write_buffer: WriteBuffer::new(),
        }
    }

    /// Create a path from a static string literal (shallow, no allocation).
    /// The string must be null-terminated.
    pub fn from_static(s: &'static [u8]) -> Self {
        let mut p = Self {
            str_ptr: s.as_ptr(),
            write_buffer: WriteBuffer::new(),
        };
        p.write_buffer.set_normalized();
        p
    }

    /// Set a shallow buffer (no copy, no allocation).
    pub fn set_shallow_buffer(&mut self, buffer: &[u8]) -> Result<(), ResultCode> {
        debug_assert!(self.write_buffer.get_length() == 0);

        if buffer.is_empty() {
            return Err(RESULT_NULLPTR_ARGUMENT);
        }

        self.set_read_only_buffer(buffer.as_ptr());
        self.set_normalized();
        Ok(())
    }

    /// Get the path as a byte slice (up to null terminator).
    pub fn get_string(&self) -> &[u8] {
        debug_assert!(self.is_normalized());
        unsafe {
            let slice = raw_to_slice(self.str_ptr, usize::MAX / 2);
            // Return without the null terminator for Rust convenience,
            // but include it since callers expect it.
            slice
        }
    }

    /// Get the path as a Rust string (without null terminator).
    pub fn as_str(&self) -> &str {
        let bytes = self.get_string();
        let end = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
        std::str::from_utf8(&bytes[..end]).unwrap_or("")
    }

    /// Get the length of the path (not including null terminator).
    pub fn get_length(&self) -> usize {
        strlen(self.get_string())
    }

    /// Check if the path is empty.
    pub fn is_empty(&self) -> bool {
        unsafe { *self.str_ptr == 0 }
    }

    /// Check if the path starts with the given prefix.
    pub fn is_match_head(&self, prefix: &[u8]) -> bool {
        strncmp(self.get_string(), prefix, prefix.len()) == 0
    }

    /// Initialize from another path (copy).
    pub fn initialize_from(&mut self, rhs: &Path) -> Result<(), ResultCode> {
        if !rhs.is_normalized() {
            return Err(RESULT_NOT_NORMALIZED);
        }

        let len = rhs.get_length();
        self.preallocate(len + 1)?;

        let copied = unsafe {
            strlcpy(
                std::slice::from_raw_parts_mut(self.write_buffer.get(), len + 1),
                rhs.get_string(),
                len + 1,
            )
        };
        if copied != len {
            return Err(RESULT_UNEXPECTED_IN_PATH_A);
        }

        self.set_normalized();
        Ok(())
    }

    /// Initialize from a raw path string with a given length.
    pub fn initialize(&mut self, path: &[u8], len: usize) -> Result<(), ResultCode> {
        if path.is_empty() {
            return Err(RESULT_NULLPTR_ARGUMENT);
        }

        self.initialize_impl(path, len)?;
        self.set_not_normalized();
        Ok(())
    }

    /// Initialize from a null-terminated path string.
    pub fn initialize_str(&mut self, path: &[u8]) -> Result<(), ResultCode> {
        if path.is_empty() {
            return Err(RESULT_NULLPTR_ARGUMENT);
        }
        let len = strlen(path);
        self.initialize(path, len)
    }

    /// Initialize, replacing backslashes with forward slashes.
    pub fn initialize_with_replace_backslash(&mut self, path: &[u8]) -> Result<(), ResultCode> {
        if path.is_empty() {
            return Err(RESULT_NULLPTR_ARGUMENT);
        }

        let len = strlen(path);
        self.initialize_impl(path, len)?;

        // Replace slashes
        let buf_len = self.write_buffer.get_length();
        if buf_len > 1 {
            unsafe {
                let buf = std::slice::from_raw_parts_mut(self.write_buffer.get(), buf_len);
                replace(buf, buf_len - 1, b'\\', b'/');
            }
        }

        self.set_not_normalized();
        Ok(())
    }

    /// Initialize, replacing leading forward slash pairs with backslash pairs.
    pub fn initialize_with_replace_forward_slashes(
        &mut self,
        path: &[u8],
    ) -> Result<(), ResultCode> {
        if path.is_empty() {
            return Err(RESULT_NULLPTR_ARGUMENT);
        }

        let len = strlen(path);
        self.initialize_impl(path, len)?;

        let buf_len = self.write_buffer.get_length();
        if buf_len > 1 {
            unsafe {
                let p = self.write_buffer.get();
                if *p == b'/' && *p.add(1) == b'/' {
                    *p = b'\\';
                    *p.add(1) = b'\\';
                }
            }
        }

        self.set_not_normalized();
        Ok(())
    }

    /// Initialize with normalization.
    pub fn initialize_with_normalization(
        &mut self,
        path: &[u8],
        size: usize,
    ) -> Result<(), ResultCode> {
        if path.is_empty() {
            return Err(RESULT_NULLPTR_ARGUMENT);
        }

        self.initialize_impl(path, size)?;
        self.set_not_normalized();

        // Perform normalization
        let str_bytes = self.get_string_raw();
        let mut path_flags = PathFlags::new();
        if is_path_relative(str_bytes) {
            path_flags.allow_relative_path();
        } else if is_windows_path(str_bytes, true) {
            path_flags.allow_windows_path();
        } else {
            let (normalized, _len) = PathNormalizer::is_normalized(str_bytes, false)?;
            let _ = normalized;
            self.set_normalized();
            return Ok(());
        }

        self.normalize(&path_flags)?;
        self.set_normalized();
        Ok(())
    }

    /// Initialize with normalization from a null-terminated path.
    pub fn initialize_with_normalization_str(&mut self, path: &[u8]) -> Result<(), ResultCode> {
        if path.is_empty() {
            return Err(RESULT_NULLPTR_ARGUMENT);
        }
        let len = strlen(path);
        self.initialize_with_normalization(path, len)
    }

    /// Initialize as an empty path.
    pub fn initialize_as_empty(&mut self) -> Result<(), ResultCode> {
        self.clear_buffer();
        self.set_normalized();
        Ok(())
    }

    /// Append a child path component.
    pub fn append_child(&mut self, child: &[u8]) -> Result<(), ResultCode> {
        if child.is_empty() {
            return Err(RESULT_NULLPTR_ARGUMENT);
        }

        let mut c_offset = 0;
        let str_bytes = self.get_string_raw();
        if !str_bytes.is_empty() && str_bytes[0] != 0 {
            // Skip early separator
            if c_offset < child.len() && child[c_offset] == b'/' {
                c_offset += 1;
            }
            if c_offset >= child.len() || child[c_offset] == 0 {
                return Ok(());
            }
        }

        let cur_len_raw = strlen(str_bytes);
        if cur_len_raw == 0 {
            return self.initialize_str(child);
        }

        let mut cur_len = cur_len_raw;
        // Remove trailing separator
        if str_bytes[cur_len - 1] == b'/' || str_bytes[cur_len - 1] == b'\\' {
            cur_len -= 1;
        }

        let child_part = &child[c_offset..];
        let child_len = strlen(child_part);

        // Save old write buffer contents
        let old_data: Vec<u8> = if !self.write_buffer.get().is_null() {
            unsafe {
                let len = strlen(std::slice::from_raw_parts(
                    self.write_buffer.get(),
                    self.write_buffer.get_length(),
                ));
                std::slice::from_raw_parts(self.write_buffer.get(), len).to_vec()
            }
        } else {
            Vec::new()
        };

        if !self.write_buffer.get().is_null() {
            self.clear_buffer();
        }

        // Pre-allocate new buffer
        self.preallocate(cur_len + 1 + child_len + 1)?;

        // Copy old content
        unsafe {
            let dst_buf = std::slice::from_raw_parts_mut(
                self.write_buffer.get(),
                self.write_buffer.get_length(),
            );

            if !old_data.is_empty() && cur_len > 0 {
                let copy_len = cur_len.min(old_data.len());
                dst_buf[..copy_len].copy_from_slice(&old_data[..copy_len]);
            }

            // Add separator
            dst_buf[cur_len] = b'/';

            // Copy child path
            let copied = strlcpy(&mut dst_buf[cur_len + 1..], child_part, child_len + 1);
            if copied != child_len {
                return Err(RESULT_UNEXPECTED_IN_PATH_A);
            }
        }

        Ok(())
    }

    /// Append a child Path.
    pub fn append_child_path(&mut self, rhs: &Path) -> Result<(), ResultCode> {
        self.append_child(rhs.get_string())
    }

    /// Combine two paths (parent + child).
    pub fn combine(&mut self, parent: &Path, child: &Path) -> Result<(), ResultCode> {
        let p_len = parent.get_length();
        let c_len = child.get_length();

        self.preallocate(p_len + c_len + 1)?;
        self.initialize_from(parent)?;

        if self.is_empty() {
            self.initialize_from(child)?;
        } else {
            self.append_child(child.get_string())?;
        }

        Ok(())
    }

    /// Remove the last child component from the path.
    pub fn remove_child(&mut self) -> Result<(), ResultCode> {
        // Ensure we have a write buffer
        if self.write_buffer.get().is_null() {
            // Copy the string data before mutating self
            let str_copy: Vec<u8> = self.get_string_raw().to_vec();
            let len = strlen(&str_copy);
            if len > 0 {
                self.preallocate(len)?;
                unsafe {
                    let dst = std::slice::from_raw_parts_mut(
                        self.write_buffer.get(),
                        self.write_buffer.get_length(),
                    );
                    strlcpy(dst, &str_copy, len + 1);
                }
            }
        }

        let p = unsafe {
            std::slice::from_raw_parts_mut(self.write_buffer.get(), self.write_buffer.get_length())
        };
        let mut len = strlen(p) as i32;

        // Check single-character paths
        if len == 1 && (p[0] == b'/' || p[0] == b'.') {
            return Err(RESULT_NOT_IMPLEMENTED);
        }

        // Handle trailing separator
        if len > 0 && (p[len as usize - 1] == b'\\' || p[len as usize - 1] == b'/') {
            len -= 1;
        }

        // Remove the child path segment
        len -= 1;
        while len >= 0 {
            if p[len as usize] == b'/' || p[len as usize] == b'\\' {
                if len > 0 {
                    p[len as usize] = 0;
                } else {
                    p[1] = 0;
                    len = 1;
                }
                break;
            }
            len -= 1;
        }

        if len <= 0 {
            return Err(RESULT_NOT_IMPLEMENTED);
        }

        Ok(())
    }

    /// Normalize the path using the given flags.
    pub fn normalize(&mut self, flags: &PathFlags) -> Result<(), ResultCode> {
        if self.is_normalized() {
            return Ok(());
        }

        // Check if already normalized
        let str_bytes = self.get_string_raw();
        let (normalized, _) = PathNormalizer::is_normalized(str_bytes, false).unwrap_or((false, 0));

        if !normalized {
            let mut len = self.write_buffer.get_length();
            if flags.is_relative_path_allowed() && is_path_relative(str_bytes) {
                len += 2;
            }
            if flags.is_windows_path_allowed() && is_windows_path(str_bytes, true) {
                len += 1;
            }

            let size = align_up(len as u64, WRITE_BUFFER_ALIGNMENT_LENGTH as u64) as usize;
            let mut buf = WriteBuffer::make(size);
            if buf.get().is_null() {
                return Err(RESULT_ALLOCATION_MEMORY_FAILED_MAKE_UNIQUE);
            }

            // Normalize into the new buffer
            let buf_slice = unsafe { std::slice::from_raw_parts_mut(buf.get(), size) };
            let src_slice = unsafe {
                std::slice::from_raw_parts(self.write_buffer.get(), self.write_buffer.get_length())
            };

            PathFormatter::normalize(
                buf_slice,
                size,
                src_slice,
                self.write_buffer.get_length(),
                flags,
            )?;

            self.set_modifiable_buffer(buf);
        }

        self.set_normalized();
        Ok(())
    }

    // -- Private helpers --

    fn clear_buffer(&mut self) {
        self.write_buffer.reset_buffer();
        self.str_ptr = Self::EMPTY_PATH.as_ptr();
    }

    fn set_modifiable_buffer(&mut self, mut buffer: WriteBuffer) {
        debug_assert!(!buffer.get().is_null());
        debug_assert!(buffer.get_length() > 0);

        if self.write_buffer.is_normalized() {
            buffer.set_normalized();
        } else {
            buffer.set_not_normalized();
        }

        // Replace the old write buffer
        self.write_buffer = buffer;
        self.str_ptr = self.write_buffer.get();
    }

    fn set_read_only_buffer(&mut self, buffer: *const u8) {
        self.str_ptr = buffer;
        self.write_buffer.reset_buffer();
    }

    fn preallocate(&mut self, length: usize) -> Result<(), ResultCode> {
        if length > self.write_buffer.get_length() {
            let size = align_up(length as u64, WRITE_BUFFER_ALIGNMENT_LENGTH as u64) as usize;
            let buf = WriteBuffer::make(size);
            if buf.get().is_null() {
                return Err(RESULT_ALLOCATION_MEMORY_FAILED_MAKE_UNIQUE);
            }
            self.set_modifiable_buffer(buf);
        }
        Ok(())
    }

    fn initialize_impl(&mut self, path: &[u8], size: usize) -> Result<(), ResultCode> {
        if size > 0 && !path.is_empty() && path[0] != 0 {
            self.preallocate(size + 1)?;

            unsafe {
                let dst = std::slice::from_raw_parts_mut(
                    self.write_buffer.get(),
                    self.write_buffer.get_length(),
                );
                let copied = strlcpy(dst, path, size + 1);
                if (copied as usize) < size {
                    return Err(RESULT_UNEXPECTED_IN_PATH_A);
                }
            }
        } else {
            self.clear_buffer();
        }
        Ok(())
    }

    fn is_normalized(&self) -> bool {
        self.write_buffer.is_normalized()
    }

    fn set_normalized(&mut self) {
        self.write_buffer.set_normalized();
    }

    fn set_not_normalized(&mut self) {
        self.write_buffer.set_not_normalized();
    }

    /// Get the raw string bytes (may not be normalized, for internal use).
    fn get_string_raw(&self) -> &[u8] {
        unsafe { raw_to_slice(self.str_ptr, usize::MAX / 2) }
    }
}

impl Default for Path {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<&[u8]> for Path {
    fn eq(&self, other: &&[u8]) -> bool {
        let s = self.get_string();
        let s_len = strlen(s);
        let o_len = strlen(other);
        s_len == o_len && strncmp(s, other, s_len) == 0
    }
}

/// Set up a fixed (pre-normalized) path.
/// Corresponds to C++ `SetUpFixedPath`.
pub fn set_up_fixed_path(out: &mut Path, s: &[u8]) -> Result<(), ResultCode> {
    let (normalized, _) = PathNormalizer::is_normalized(s, false)?;
    if !normalized {
        return Err(RESULT_INVALID_PATH_FORMAT);
    }
    out.set_shallow_buffer(s)
}

/// Check if a path is a Windows drive root path (e.g., `C:/`).
/// Corresponds to C++ `IsWindowsDriveRootPath`.
pub fn is_windows_drive_root_path(path: &Path) -> bool {
    let s = path.get_string();
    is_windows_drive(s)
        && s.len() > 2
        && (s[2] == DIRECTORY_SEPARATOR || s[2] == ALTERNATE_DIRECTORY_SEPARATOR)
        && (s.len() <= 3 || s[3] == NULL_TERMINATOR)
}
