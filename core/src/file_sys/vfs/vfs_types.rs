// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/file_sys/vfs/vfs_types.h
//! Type aliases and shared data types for the VFS layer.

use std::sync::Arc;

use super::vfs::{VfsDirectory, VfsFile, VfsFilesystem};

/// Shared pointer to a VFS file.
/// Corresponds to C++ `VirtualFile = std::shared_ptr<VfsFile>`.
pub type VirtualFile = Arc<dyn VfsFile>;

/// Shared pointer to a VFS directory.
/// Corresponds to C++ `VirtualDir = std::shared_ptr<VfsDirectory>`.
pub type VirtualDir = Arc<dyn VfsDirectory>;

/// Shared pointer to a VFS filesystem.
/// Corresponds to C++ `VirtualFilesystem = std::shared_ptr<VfsFilesystem>`.
pub type VirtualFilesystem = Arc<dyn VfsFilesystem>;

/// File timestamp structure.
/// Corresponds to C++ `FileTimeStampRaw` struct.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[repr(C)]
pub struct FileTimeStampRaw {
    pub created: u64,
    pub accessed: u64,
    pub modified: u64,
    pub padding: u64,
}
