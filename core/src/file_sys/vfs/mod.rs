// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/file_sys/vfs/
//! Virtual filesystem abstraction with multiple backend implementations.

pub mod vfs;
pub mod vfs_cached;
pub mod vfs_concat;
pub mod vfs_layered;
pub mod vfs_offset;
pub mod vfs_real;
pub mod vfs_static;
pub mod vfs_types;
pub mod vfs_vector;
