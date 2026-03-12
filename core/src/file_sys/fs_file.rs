// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fs_file.h

/// Read option flag values.
/// Corresponds to C++ `ReadOptionFlag` enum.
#[repr(u32)]
pub enum ReadOptionFlag {
    None = 0,
}

/// Read option for file operations.
/// Corresponds to C++ `ReadOption` struct.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ReadOption {
    pub value: u32,
}

impl ReadOption {
    pub const NONE: Self = Self {
        value: ReadOptionFlag::None as u32,
    };
}

const _: () = assert!(std::mem::size_of::<ReadOption>() == std::mem::size_of::<u32>());

/// Write option flag values.
/// Corresponds to C++ `WriteOptionFlag` enum.
#[repr(u32)]
pub enum WriteOptionFlag {
    None = 0,
    Flush = 1 << 0,
}

/// Write option for file operations.
/// Corresponds to C++ `WriteOption` struct.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct WriteOption {
    pub value: u32,
}

impl WriteOption {
    pub const NONE: Self = Self {
        value: WriteOptionFlag::None as u32,
    };
    pub const FLUSH: Self = Self {
        value: WriteOptionFlag::Flush as u32,
    };

    /// Check if the flush flag is set.
    pub const fn has_flush_flag(&self) -> bool {
        (self.value & WriteOptionFlag::Flush as u32) != 0
    }
}

const _: () = assert!(std::mem::size_of::<WriteOption>() == std::mem::size_of::<u32>());

/// Opaque file handle. Corresponds to C++ `FileHandle`.
#[derive(Debug)]
pub struct FileHandle {
    pub handle: *mut std::ffi::c_void,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_option_size() {
        assert_eq!(std::mem::size_of::<ReadOption>(), 4);
    }

    #[test]
    fn test_write_option_size() {
        assert_eq!(std::mem::size_of::<WriteOption>(), 4);
    }

    #[test]
    fn test_write_option_none() {
        assert_eq!(WriteOption::NONE.value, 0);
        assert!(!WriteOption::NONE.has_flush_flag());
    }

    #[test]
    fn test_write_option_flush() {
        assert_eq!(WriteOption::FLUSH.value, 1);
        assert!(WriteOption::FLUSH.has_flush_flag());
    }

    #[test]
    fn test_read_option_none() {
        assert_eq!(ReadOption::NONE.value, 0);
    }
}
