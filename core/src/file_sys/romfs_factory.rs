// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/romfs_factory.h and romfs_factory.cpp
// RomFS factory for opening patched/unpatched RomFS images.

use super::nca_metadata::ContentRecordType;
use super::vfs::vfs_types::VirtualFile;

// ============================================================================
// StorageId
// ============================================================================

/// Storage identifier for content.
/// Corresponds to upstream `StorageId`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum StorageId {
    None = 0,
    Host = 1,
    GameCard = 2,
    NandSystem = 3,
    NandUser = 4,
    SdCard = 5,
}

// ============================================================================
// RomFSFactory
// ============================================================================

/// File system interface to the RomFS archive.
/// Corresponds to upstream `RomFSFactory`.
///
/// NOTE: Full implementation requires Loader::AppLoader, ContentProvider, and
/// FileSystemController which are not yet ported. This provides the structural
/// skeleton with the correct API surface.
pub struct RomFSFactory {
    file: Option<VirtualFile>,
    packed_update_raw: Option<VirtualFile>,
    base: Option<VirtualFile>,
    updatable: bool,
    // TODO: content_provider and filesystem_controller references
}

impl RomFSFactory {
    /// Create a new RomFSFactory.
    /// In the full implementation, this would read the RomFS from the app loader.
    pub fn new(file: Option<VirtualFile>, updatable: bool) -> Self {
        Self {
            file,
            packed_update_raw: None,
            base: None,
            updatable,
        }
    }

    pub fn set_packed_update(&mut self, update_raw_file: VirtualFile) {
        self.packed_update_raw = Some(update_raw_file);
    }

    /// Open the RomFS for the current process.
    /// Corresponds to upstream `RomFSFactory::OpenCurrentProcess`.
    pub fn open_current_process(&self, _current_process_title_id: u64) -> Option<VirtualFile> {
        if !self.updatable {
            return self.file.clone();
        }

        // TODO: Implement patching once PatchManager and ContentProvider are ported.
        self.file.clone()
    }

    /// Open a patched RomFS for a given title.
    /// Corresponds to upstream `RomFSFactory::OpenPatchedRomFS`.
    pub fn open_patched_romfs(&self, _title_id: u64, _type_: ContentRecordType) -> Option<VirtualFile> {
        // TODO: Implement once ContentProvider and PatchManager are ported.
        None
    }

    /// Open a patched RomFS with program index.
    /// Corresponds to upstream `RomFSFactory::OpenPatchedRomFSWithProgramIndex`.
    pub fn open_patched_romfs_with_program_index(
        &self,
        _title_id: u64,
        _program_index: u8,
        _type_: ContentRecordType,
    ) -> Option<VirtualFile> {
        // TODO: Implement once ContentProvider is ported.
        None
    }

    /// Open a RomFS by title ID, storage, and type.
    /// Corresponds to upstream `RomFSFactory::Open`.
    pub fn open(
        &self,
        _title_id: u64,
        _storage: StorageId,
        _type_: ContentRecordType,
    ) -> Option<VirtualFile> {
        // TODO: Implement once ContentProvider is ported.
        None
    }
}
