// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/common/fs/fs_types.h
//! Filesystem operation enums and types.

use std::path::Path;

use bitflags::bitflags;

/// File access mode.
///
/// Maps to upstream `FileAccessMode`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileAccessMode {
    /// If the file at path exists, it opens the file for reading.
    /// If the file at path does not exist, it fails to open the file.
    Read,
    /// If the file at path exists, the existing contents of the file are erased.
    /// The empty file is then opened for writing.
    /// If the file at path does not exist, it creates and opens a new empty file for writing.
    Write,
    /// If the file at path exists, it opens the file for reading and writing.
    /// If the file at path does not exist, it fails to open the file.
    ReadWrite,
    /// If the file at path exists, it opens the file for appending.
    /// If the file at path does not exist, it creates and opens a new empty file for appending.
    Append,
    /// If the file at path exists, it opens the file for both reading and appending.
    /// If the file at path does not exist, it creates and opens a new empty file for both
    /// reading and appending.
    ReadAppend,
}

/// File type (binary vs text).
///
/// Maps to upstream `FileType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    BinaryFile,
    TextFile,
}

impl Default for FileType {
    fn default() -> Self {
        FileType::BinaryFile
    }
}

/// File sharing flags.
///
/// Maps to upstream `FileShareFlag`. On non-Windows platforms this is largely
/// informational; Windows uses it for `_wfsopen` sharing mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileShareFlag {
    /// Provides exclusive access to the file.
    ShareNone,
    /// Provides read only shared access to the file.
    ShareReadOnly,
    /// Provides write only shared access to the file.
    ShareWriteOnly,
    /// Provides read and write shared access to the file.
    ShareReadWrite,
}

impl Default for FileShareFlag {
    fn default() -> Self {
        FileShareFlag::ShareReadOnly
    }
}

bitflags! {
    /// Directory entry filter flags.
    ///
    /// Maps to upstream `DirEntryFilter`.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct DirEntryFilter: u32 {
        const FILE = 1 << 0;
        const DIRECTORY = 1 << 1;
        const ALL = Self::FILE.bits() | Self::DIRECTORY.bits();
    }
}

/// A callback function which takes in the path of a directory entry.
///
/// Returns `true` to indicate success/continue, `false` to stop iteration.
///
/// Maps to upstream `DirEntryCallable`.
pub type DirEntryCallable = dyn Fn(&Path, bool /* is_dir */) -> bool;

/// Display implementations for logging.
impl std::fmt::Display for FileAccessMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileAccessMode::Read => write!(f, "Read"),
            FileAccessMode::Write => write!(f, "Write"),
            FileAccessMode::ReadWrite => write!(f, "ReadWrite"),
            FileAccessMode::Append => write!(f, "Append"),
            FileAccessMode::ReadAppend => write!(f, "ReadAppend"),
        }
    }
}

impl std::fmt::Display for FileType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileType::BinaryFile => write!(f, "BinaryFile"),
            FileType::TextFile => write!(f, "TextFile"),
        }
    }
}

impl std::fmt::Display for FileShareFlag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileShareFlag::ShareNone => write!(f, "ShareNone"),
            FileShareFlag::ShareReadOnly => write!(f, "ShareReadOnly"),
            FileShareFlag::ShareWriteOnly => write!(f, "ShareWriteOnly"),
            FileShareFlag::ShareReadWrite => write!(f, "ShareReadWrite"),
        }
    }
}
