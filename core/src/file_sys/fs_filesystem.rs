// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fs_filesystem.h

use bitflags::bitflags;

bitflags! {
    /// File open mode flags.
    /// Corresponds to C++ `OpenMode` enum.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct OpenMode: u32 {
        const READ = 1 << 0;
        const WRITE = 1 << 1;
        const ALLOW_APPEND = 1 << 2;

        const READ_WRITE = Self::READ.bits() | Self::WRITE.bits();
        const ALL = Self::READ_WRITE.bits() | Self::ALLOW_APPEND.bits();
    }
}

bitflags! {
    /// Directory open mode flags.
    /// Corresponds to C++ `OpenDirectoryMode` enum.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct OpenDirectoryMode: u64 {
        const DIRECTORY = 1 << 0;
        const FILE = 1 << 1;

        const ALL = Self::DIRECTORY.bits() | Self::FILE.bits();

        const NOT_REQUIRE_FILE_SIZE = 1u64 << 31;
    }
}

/// Directory entry type.
/// Corresponds to C++ `DirectoryEntryType` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum DirectoryEntryType {
    Directory = 0,
    File = 1,
}

/// Create option flags.
/// Corresponds to C++ `CreateOption` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CreateOption {
    None = 0,
    BigFile = 1,
}

/// Filesystem attribute structure.
/// Binary layout must match C++ `FileSystemAttribute` exactly (0xC0 bytes).
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct FileSystemAttribute {
    pub dir_entry_name_length_max_defined: u8,
    pub file_entry_name_length_max_defined: u8,
    pub dir_path_name_length_max_defined: u8,
    pub file_path_name_length_max_defined: u8,
    pub _padding0: [u8; 0x5],
    pub utf16_dir_entry_name_length_max_defined: u8,
    pub utf16_file_entry_name_length_max_defined: u8,
    pub utf16_dir_path_name_length_max_defined: u8,
    pub utf16_file_path_name_length_max_defined: u8,
    pub _padding1: [u8; 0x18],
    pub dir_entry_name_length_max: i32,
    pub file_entry_name_length_max: i32,
    pub dir_path_name_length_max: i32,
    pub file_path_name_length_max: i32,
    pub _padding2: [u32; 0x5],
    pub utf16_dir_entry_name_length_max: i32,
    pub utf16_file_entry_name_length_max: i32,
    pub utf16_dir_path_name_length_max: i32,
    pub utf16_file_path_name_length_max: i32,
    pub _padding3: [u32; 0x18],
    pub _padding4: [u32; 0x1],
}

const _: () = assert!(
    std::mem::size_of::<FileSystemAttribute>() == 0xC0,
    "FileSystemAttribute has incorrect size"
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_open_mode_flags() {
        assert_eq!(OpenMode::READ.bits(), 1);
        assert_eq!(OpenMode::WRITE.bits(), 2);
        assert_eq!(OpenMode::ALLOW_APPEND.bits(), 4);
        assert_eq!(OpenMode::READ_WRITE.bits(), 3);
        assert_eq!(OpenMode::ALL.bits(), 7);
    }

    #[test]
    fn test_open_mode_contains() {
        let rw = OpenMode::READ_WRITE;
        assert!(rw.contains(OpenMode::READ));
        assert!(rw.contains(OpenMode::WRITE));
        assert!(!rw.contains(OpenMode::ALLOW_APPEND));

        let all = OpenMode::ALL;
        assert!(all.contains(OpenMode::READ));
        assert!(all.contains(OpenMode::WRITE));
        assert!(all.contains(OpenMode::ALLOW_APPEND));
    }

    #[test]
    fn test_open_directory_mode_flags() {
        assert_eq!(OpenDirectoryMode::DIRECTORY.bits(), 1);
        assert_eq!(OpenDirectoryMode::FILE.bits(), 2);
        assert_eq!(OpenDirectoryMode::ALL.bits(), 3);
        assert_eq!(OpenDirectoryMode::NOT_REQUIRE_FILE_SIZE.bits(), 1u64 << 31);
    }

    #[test]
    fn test_open_directory_mode_contains() {
        let all = OpenDirectoryMode::ALL;
        assert!(all.contains(OpenDirectoryMode::DIRECTORY));
        assert!(all.contains(OpenDirectoryMode::FILE));
        assert!(!all.contains(OpenDirectoryMode::NOT_REQUIRE_FILE_SIZE));
    }

    #[test]
    fn test_directory_entry_type() {
        assert_eq!(DirectoryEntryType::Directory as u8, 0);
        assert_eq!(DirectoryEntryType::File as u8, 1);
    }

    #[test]
    fn test_create_option() {
        assert_eq!(CreateOption::None as u8, 0);
        assert_eq!(CreateOption::BigFile as u8, 1);
    }

    #[test]
    fn test_filesystem_attribute_size() {
        assert_eq!(std::mem::size_of::<FileSystemAttribute>(), 0xC0);
    }
}
