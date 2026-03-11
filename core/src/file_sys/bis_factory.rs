// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/bis_factory.h / .cpp

use super::registered_cache::{PlaceholderCache, RegisteredCache};
use super::vfs::vfs_types::{VirtualDir, VirtualFile, VirtualFilesystem};

/// BIS partition identifiers.
/// Corresponds to upstream `BisPartitionId`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum BisPartitionId {
    UserDataRoot = 20,
    CalibrationBinary = 27,
    CalibrationFile = 28,
    BootConfigAndPackage2Part1 = 21,
    BootConfigAndPackage2Part2 = 22,
    BootConfigAndPackage2Part3 = 23,
    BootConfigAndPackage2Part4 = 24,
    BootConfigAndPackage2Part5 = 25,
    BootConfigAndPackage2Part6 = 26,
    SafeMode = 29,
    System = 31,
    SystemProperEncryption = 32,
    SystemProperPartition = 33,
    User = 30,
}

/// File system interface to Built-In Storage (NAND).
/// Corresponds to upstream `BISFactory`.
pub struct BisFactory {
    nand_root: VirtualDir,
    load_root: VirtualDir,
    dump_root: VirtualDir,
    sysnand_cache: Option<Box<RegisteredCache>>,
    usrnand_cache: Option<Box<RegisteredCache>>,
    sysnand_placeholder: Option<Box<PlaceholderCache>>,
    usrnand_placeholder: Option<Box<PlaceholderCache>>,
}

impl BisFactory {
    pub fn new(
        nand_root: VirtualDir,
        load_root: VirtualDir,
        dump_root: VirtualDir,
    ) -> Self {
        // TODO: initialize caches from nand_root subdirectories.
        Self {
            nand_root,
            load_root,
            dump_root,
            sysnand_cache: None,
            usrnand_cache: None,
            sysnand_placeholder: None,
            usrnand_placeholder: None,
        }
    }

    pub fn get_system_nand_content_directory(&self) -> Option<VirtualDir> {
        // TODO: open Contents directory under system NAND.
        None
    }

    pub fn get_user_nand_content_directory(&self) -> Option<VirtualDir> {
        // TODO: open Contents directory under user NAND.
        None
    }

    pub fn get_system_nand_contents(&self) -> Option<&RegisteredCache> {
        self.sysnand_cache.as_deref()
    }

    pub fn get_user_nand_contents(&self) -> Option<&RegisteredCache> {
        self.usrnand_cache.as_deref()
    }

    pub fn get_system_nand_placeholder(&self) -> Option<&PlaceholderCache> {
        self.sysnand_placeholder.as_deref()
    }

    pub fn get_user_nand_placeholder(&self) -> Option<&PlaceholderCache> {
        self.usrnand_placeholder.as_deref()
    }

    pub fn get_modification_load_root(&self, _title_id: u64) -> Option<VirtualDir> {
        // TODO: open load_root/<title_id_hex>
        None
    }

    pub fn get_modification_dump_root(&self, _title_id: u64) -> Option<VirtualDir> {
        // TODO: open dump_root/<title_id_hex>
        None
    }

    pub fn open_partition(&self, _id: BisPartitionId) -> Option<VirtualDir> {
        // TODO: map partition IDs to NAND subdirectories.
        None
    }

    pub fn open_partition_storage(
        &self,
        _id: BisPartitionId,
        _file_system: VirtualFilesystem,
    ) -> Option<VirtualFile> {
        None
    }

    pub fn get_image_directory(&self) -> Option<VirtualDir> {
        None
    }

    pub fn get_system_nand_free_space(&self) -> u64 {
        0
    }

    pub fn get_system_nand_total_space(&self) -> u64 {
        0
    }

    pub fn get_user_nand_free_space(&self) -> u64 {
        0
    }

    pub fn get_user_nand_total_space(&self) -> u64 {
        0
    }

    pub fn get_full_nand_total_space(&self) -> u64 {
        0
    }

    pub fn get_bcat_directory(&self, _title_id: u64) -> Option<VirtualDir> {
        None
    }
}
