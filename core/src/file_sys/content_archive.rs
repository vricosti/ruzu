// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/content_archive.h and content_archive.cpp
// NCA (Nintendo Content Archive) parsing.

use std::sync::Arc;

use super::partition_filesystem::ResultStatus;
use super::vfs::vfs::{VfsDirectory, VfsFile};
use super::vfs::vfs_types::{VirtualDir, VirtualFile};

// ============================================================================
// Types and enums
// ============================================================================

/// Describes the type of content within an NCA archive.
/// Corresponds to upstream `NCAContentType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum NCAContentType {
    /// Executable-related data
    Program = 0,
    /// Metadata
    Meta = 1,
    /// Access control data
    Control = 2,
    /// Information related to the game manual
    Manual = 3,
    /// System data
    Data = 4,
    /// Data that can be accessed by applications
    PublicData = 5,
}

/// Rights ID — 16 bytes.
pub type RightsId = [u8; 0x10];

/// Check whether a VFS directory is an ExeFS partition.
/// Corresponds to upstream `IsDirectoryExeFS`.
pub fn is_directory_exefs(pfs: &VirtualDir) -> bool {
    pfs.get_file("main").is_some() && pfs.get_file("main.npdm").is_some()
}

/// Check whether a VFS directory is a Logo partition.
/// Corresponds to upstream `IsDirectoryLogoPartition`.
pub fn is_directory_logo_partition(pfs: &VirtualDir) -> bool {
    pfs.get_file("NintendoLogo.png").is_some() && pfs.get_file("StartupMovie.gif").is_some()
}

// ============================================================================
// NCA
// ============================================================================

/// An implementation of VfsDirectory that represents a Nintendo Content Archive (NCA) container.
/// After construction, use get_status() to determine if the file is valid and ready to be used.
///
/// Corresponds to upstream `NCA`.
///
/// NOTE: Full NCA parsing requires the crypto subsystem (fssystem_nca_file_system_driver,
/// key_manager, etc.) which are not yet fully ported. This is a structural port that
/// provides the correct interface and stores state, but the constructor is stubbed to
/// return ErrorBadNCAHeader until the crypto layer is available.
pub struct NCA {
    dirs: Vec<VirtualDir>,
    files: Vec<VirtualFile>,

    romfs: Option<VirtualFile>,
    exefs: Option<VirtualDir>,
    logo: Option<VirtualDir>,
    file: VirtualFile,

    status: ResultStatus,

    encrypted: bool,
    is_update: bool,

    title_id: u64,
    content_type: NCAContentType,
    rights_id: RightsId,
    sdk_version: u32,
    key_generation: u8,
}

impl NCA {
    /// Construct an NCA from a VFS file.
    ///
    /// NOTE: This is currently a stub. Full NCA parsing requires the crypto subsystem
    /// (NcaReader, NcaFileSystemDriver, KeyManager). Until those are ported, this will
    /// parse what it can and set status accordingly.
    pub fn new(file: VirtualFile, _base_nca: Option<&NCA>) -> Self {
        // TODO: Implement full NCA parsing once crypto subsystem is ported.
        // For now, return a minimal stub that reports ErrorBadNCAHeader.
        Self {
            dirs: Vec::new(),
            files: Vec::new(),
            romfs: None,
            exefs: None,
            logo: None,
            file,
            status: ResultStatus::ErrorBadNCAHeader,
            encrypted: false,
            is_update: false,
            title_id: 0,
            content_type: NCAContentType::Program,
            rights_id: [0u8; 0x10],
            sdk_version: 0,
            key_generation: 0,
        }
    }

    pub fn get_status(&self) -> ResultStatus {
        self.status
    }

    pub fn get_type(&self) -> NCAContentType {
        self.content_type
    }

    pub fn get_title_id(&self) -> u64 {
        if self.is_update {
            self.title_id | 0x800
        } else {
            self.title_id
        }
    }

    pub fn get_rights_id(&self) -> RightsId {
        self.rights_id
    }

    pub fn get_sdk_version(&self) -> u32 {
        self.sdk_version
    }

    pub fn get_key_generation(&self) -> u8 {
        self.key_generation
    }

    pub fn is_update(&self) -> bool {
        self.is_update
    }

    pub fn get_romfs(&self) -> Option<VirtualFile> {
        self.romfs.clone()
    }

    pub fn get_exefs(&self) -> Option<VirtualDir> {
        self.exefs.clone()
    }

    pub fn get_base_file(&self) -> VirtualFile {
        self.file.clone()
    }

    pub fn get_logo_partition(&self) -> Option<VirtualDir> {
        self.logo.clone()
    }

    pub fn get_name(&self) -> String {
        self.file.get_name()
    }
}

impl VfsDirectory for NCA {
    fn get_files(&self) -> Vec<VirtualFile> {
        if self.status != ResultStatus::Success {
            return vec![];
        }
        self.files.clone()
    }

    fn get_subdirectories(&self) -> Vec<VirtualDir> {
        if self.status != ResultStatus::Success {
            return vec![];
        }
        self.dirs.clone()
    }

    fn get_name(&self) -> String {
        self.file.get_name()
    }

    fn get_parent_directory(&self) -> Option<VirtualDir> {
        self.file.get_containing_directory()
    }

    fn is_writable(&self) -> bool {
        false
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn create_subdirectory(&self, _name: &str) -> Option<VirtualDir> {
        None
    }

    fn create_file(&self, _name: &str) -> Option<VirtualFile> {
        None
    }

    fn delete_subdirectory(&self, _name: &str) -> bool {
        false
    }

    fn delete_file(&self, _name: &str) -> bool {
        false
    }

    fn rename(&self, _name: &str) -> bool {
        false
    }
}
