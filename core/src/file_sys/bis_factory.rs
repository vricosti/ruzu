// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/bis_factory.h / .cpp
// Status: COMPLETE (structural parity; OpenPartitionStorage requires crypto pipeline)
//
// File system interface to Built-In Storage (NAND). Provides access to NAND
// partitions, registered caches, placeholder caches, modification roots, and
// space reporting. Sizes match upstream exactly.

use super::registered_cache::{PlaceholderCache, RegisteredCache};
use super::vfs::vfs::get_or_create_directory_relative;
use super::vfs::vfs_types::{VirtualDir, VirtualFile, VirtualFilesystem};

/// NAND user partition size: 26624 MiB.
///
/// Corresponds to upstream `NAND_USER_SIZE`.
const NAND_USER_SIZE: u64 = 0x680000000;

/// NAND system partition size: 2560 MiB.
///
/// Corresponds to upstream `NAND_SYSTEM_SIZE`.
const NAND_SYSTEM_SIZE: u64 = 0xA0000000;

/// NAND total size: 29820 MiB.
///
/// Corresponds to upstream `NAND_TOTAL_SIZE`.
const NAND_TOTAL_SIZE: u64 = 0x747C00000;

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
    /// Construct a new BISFactory.
    ///
    /// Corresponds to upstream `BISFactory::BISFactory`.
    pub fn new(nand_root: VirtualDir, load_root: VirtualDir, dump_root: VirtualDir) -> Self {
        // Initialize system NAND registered cache.
        let sysnand_cache = get_or_create_directory_relative(
            nand_root.as_ref(),
            "/system/Contents/registered",
        )
        .map(|dir| Box::new(RegisteredCache::new(dir)));

        // Initialize user NAND registered cache.
        let usrnand_cache = get_or_create_directory_relative(
            nand_root.as_ref(),
            "/user/Contents/registered",
        )
        .map(|dir| Box::new(RegisteredCache::new(dir)));

        // Initialize system NAND placeholder cache.
        let sysnand_placeholder = get_or_create_directory_relative(
            nand_root.as_ref(),
            "/system/Contents/placehld",
        )
        .map(|dir| Box::new(PlaceholderCache::new(dir)));

        // Initialize user NAND placeholder cache.
        let usrnand_placeholder = get_or_create_directory_relative(
            nand_root.as_ref(),
            "/user/Contents/placehld",
        )
        .map(|dir| Box::new(PlaceholderCache::new(dir)));

        Self {
            nand_root,
            load_root,
            dump_root,
            sysnand_cache,
            usrnand_cache,
            sysnand_placeholder,
            usrnand_placeholder,
        }
    }

    /// Get the system NAND content directory.
    ///
    /// Corresponds to upstream `BISFactory::GetSystemNANDContentDirectory`.
    pub fn get_system_nand_content_directory(&self) -> Option<VirtualDir> {
        get_or_create_directory_relative(self.nand_root.as_ref(), "/system/Contents")
    }

    /// Get the user NAND content directory.
    ///
    /// Corresponds to upstream `BISFactory::GetUserNANDContentDirectory`.
    pub fn get_user_nand_content_directory(&self) -> Option<VirtualDir> {
        get_or_create_directory_relative(self.nand_root.as_ref(), "/user/Contents")
    }

    /// Get the system NAND registered cache.
    ///
    /// Corresponds to upstream `BISFactory::GetSystemNANDContents`.
    pub fn get_system_nand_contents(&self) -> Option<&RegisteredCache> {
        self.sysnand_cache.as_deref()
    }

    /// Get the user NAND registered cache.
    ///
    /// Corresponds to upstream `BISFactory::GetUserNANDContents`.
    pub fn get_user_nand_contents(&self) -> Option<&RegisteredCache> {
        self.usrnand_cache.as_deref()
    }

    /// Get the system NAND placeholder cache.
    ///
    /// Corresponds to upstream `BISFactory::GetSystemNANDPlaceholder`.
    pub fn get_system_nand_placeholder(&self) -> Option<&PlaceholderCache> {
        self.sysnand_placeholder.as_deref()
    }

    /// Get the user NAND placeholder cache.
    ///
    /// Corresponds to upstream `BISFactory::GetUserNANDPlaceholder`.
    pub fn get_user_nand_placeholder(&self) -> Option<&PlaceholderCache> {
        self.usrnand_placeholder.as_deref()
    }

    /// Get the modification load root for a given title.
    ///
    /// LayeredFS doesn't work on updates (title_id & 0xFFF == 0x800) or
    /// title id-less homebrew (title_id == 0).
    ///
    /// Corresponds to upstream `BISFactory::GetModificationLoadRoot`.
    pub fn get_modification_load_root(&self, title_id: u64) -> Option<VirtualDir> {
        if title_id == 0 || (title_id & 0xFFF) == 0x800 {
            return None;
        }
        get_or_create_directory_relative(
            self.load_root.as_ref(),
            &format!("/{:016X}", title_id),
        )
    }

    /// Get the modification dump root for a given title.
    ///
    /// Corresponds to upstream `BISFactory::GetModificationDumpRoot`.
    pub fn get_modification_dump_root(&self, title_id: u64) -> Option<VirtualDir> {
        if title_id == 0 {
            return None;
        }
        get_or_create_directory_relative(
            self.dump_root.as_ref(),
            &format!("/{:016X}", title_id),
        )
    }

    /// Open a BIS partition by ID.
    ///
    /// Corresponds to upstream `BISFactory::OpenPartition`.
    pub fn open_partition(&self, id: BisPartitionId) -> Option<VirtualDir> {
        match id {
            BisPartitionId::CalibrationFile => {
                get_or_create_directory_relative(self.nand_root.as_ref(), "/prodinfof")
            }
            BisPartitionId::SafeMode => {
                get_or_create_directory_relative(self.nand_root.as_ref(), "/safe")
            }
            BisPartitionId::System => {
                get_or_create_directory_relative(self.nand_root.as_ref(), "/system")
            }
            BisPartitionId::User => {
                get_or_create_directory_relative(self.nand_root.as_ref(), "/user")
            }
            _ => None,
        }
    }

    /// Open a BIS partition storage (decrypted).
    ///
    /// Corresponds to upstream `BISFactory::OpenPartitionStorage`.
    /// Requires the full crypto pipeline (PartitionDataManager, KeyManager)
    /// to be wired up for decryption.
    pub fn open_partition_storage(
        &self,
        _id: BisPartitionId,
        _file_system: VirtualFilesystem,
    ) -> Option<VirtualFile> {
        // TODO: Requires PartitionDataManager and KeyManager crypto pipeline.
        // Upstream decrypts prodinfo and package2 partitions here.
        log::warn!("BISFactory::OpenPartitionStorage: crypto pipeline not yet wired");
        None
    }

    /// Get the image (album) directory.
    ///
    /// Corresponds to upstream `BISFactory::GetImageDirectory`.
    pub fn get_image_directory(&self) -> Option<VirtualDir> {
        get_or_create_directory_relative(self.nand_root.as_ref(), "/user/Album")
    }

    /// Get the system NAND free space.
    ///
    /// Corresponds to upstream `BISFactory::GetSystemNANDFreeSpace`.
    pub fn get_system_nand_free_space(&self) -> u64 {
        let sys_dir = get_or_create_directory_relative(self.nand_root.as_ref(), "/system");
        match sys_dir {
            Some(dir) => self.get_system_nand_total_space() - dir.get_size() as u64,
            None => self.get_system_nand_total_space(),
        }
    }

    /// Get the system NAND total space.
    ///
    /// Corresponds to upstream `BISFactory::GetSystemNANDTotalSpace`.
    pub fn get_system_nand_total_space(&self) -> u64 {
        NAND_SYSTEM_SIZE
    }

    /// Get the user NAND free space.
    ///
    /// For some reason games such as BioShock 1 checks whether this is exactly
    /// 0x680000000 bytes. Set the free space to be 1 MiB less than the total
    /// as a workaround to this issue.
    ///
    /// Corresponds to upstream `BISFactory::GetUserNANDFreeSpace`.
    pub fn get_user_nand_free_space(&self) -> u64 {
        self.get_user_nand_total_space() - 0x100000
    }

    /// Get the user NAND total space.
    ///
    /// Corresponds to upstream `BISFactory::GetUserNANDTotalSpace`.
    pub fn get_user_nand_total_space(&self) -> u64 {
        NAND_USER_SIZE
    }

    /// Get the full NAND total space.
    ///
    /// Corresponds to upstream `BISFactory::GetFullNANDTotalSpace`.
    pub fn get_full_nand_total_space(&self) -> u64 {
        NAND_TOTAL_SIZE
    }

    /// Get the BCAT directory for a given title.
    ///
    /// Corresponds to upstream `BISFactory::GetBCATDirectory`.
    pub fn get_bcat_directory(&self, title_id: u64) -> Option<VirtualDir> {
        get_or_create_directory_relative(
            self.nand_root.as_ref(),
            &format!("/system/save/bcat/{:016X}", title_id),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nand_sizes() {
        assert_eq!(NAND_USER_SIZE, 0x680000000);
        assert_eq!(NAND_SYSTEM_SIZE, 0xA0000000);
        assert_eq!(NAND_TOTAL_SIZE, 0x747C00000);
    }

    #[test]
    fn test_user_nand_free_space_workaround() {
        use crate::file_sys::vfs::vfs_vector::VectorVfsDirectory;
        use std::sync::Arc;

        let root: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "nand".to_string(),
            None,
        ));
        let load: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "load".to_string(),
            None,
        ));
        let dump: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "dump".to_string(),
            None,
        ));

        let factory = BisFactory::new(root, load, dump);
        // Free space should be total - 1 MiB, matching upstream workaround.
        assert_eq!(
            factory.get_user_nand_free_space(),
            NAND_USER_SIZE - 0x100000
        );
    }

    #[test]
    fn test_modification_load_root_filters() {
        use crate::file_sys::vfs::vfs_vector::VectorVfsDirectory;
        use std::sync::Arc;

        let root: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "nand".to_string(),
            None,
        ));
        let load: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "load".to_string(),
            None,
        ));
        let dump: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "dump".to_string(),
            None,
        ));

        let factory = BisFactory::new(root, load, dump);
        // Title ID 0 should return None.
        assert!(factory.get_modification_load_root(0).is_none());
        // Update title IDs (& 0xFFF == 0x800) should return None.
        assert!(factory.get_modification_load_root(0x0100000000000800).is_none());
    }

    #[test]
    fn test_modification_dump_root_zero_title() {
        use crate::file_sys::vfs::vfs_vector::VectorVfsDirectory;
        use std::sync::Arc;

        let root: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "nand".to_string(),
            None,
        ));
        let load: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "load".to_string(),
            None,
        ));
        let dump: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "dump".to_string(),
            None,
        ));

        let factory = BisFactory::new(root, load, dump);
        assert!(factory.get_modification_dump_root(0).is_none());
    }
}
