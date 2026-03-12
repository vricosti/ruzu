// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/romfs_factory.h and romfs_factory.cpp
// RomFS factory for opening patched/unpatched RomFS images.

use super::common_funcs::get_base_title_id_with_program_index;
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
    ///
    /// Uses `GetBaseTitleIDWithProgramIndex` to compute the resolved title ID,
    /// then delegates to `open_patched_romfs`.
    ///
    /// Corresponds to upstream `RomFSFactory::OpenPatchedRomFSWithProgramIndex`.
    pub fn open_patched_romfs_with_program_index(
        &self,
        title_id: u64,
        program_index: u8,
        type_: ContentRecordType,
    ) -> Option<VirtualFile> {
        let res_title_id = get_base_title_id_with_program_index(title_id, program_index as u64);
        self.open_patched_romfs(res_title_id, type_)
    }

    /// Open a RomFS by title ID, storage, and type.
    ///
    /// Retrieves the NCA entry from the appropriate storage and returns
    /// its RomFS.
    ///
    /// Corresponds to upstream `RomFSFactory::Open`.
    pub fn open(
        &self,
        _title_id: u64,
        _storage: StorageId,
        _type_: ContentRecordType,
    ) -> Option<VirtualFile> {
        // TODO: Implement once ContentProvider and FileSystemController are ported.
        // Upstream dispatches based on StorageId:
        //   None -> content_provider.GetEntry(title_id, type)
        //   NandSystem -> filesystem_controller.GetSystemNANDContents()->GetEntry(...)
        //   NandUser -> filesystem_controller.GetUserNANDContents()->GetEntry(...)
        //   SdCard -> filesystem_controller.GetSDMCContents()->GetEntry(...)
        //   Host, GameCard -> unimplemented
        log::warn!(
            "RomFSFactory::open: not yet implemented (requires ContentProvider)"
        );
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::VectorVfsFile;
    use std::sync::Arc;

    fn make_test_file() -> VirtualFile {
        Arc::new(VectorVfsFile::new(
            vec![0u8; 100],
            "test.romfs".to_string(),
            None,
        ))
    }

    #[test]
    fn test_storage_id_values() {
        assert_eq!(StorageId::None as u8, 0);
        assert_eq!(StorageId::Host as u8, 1);
        assert_eq!(StorageId::GameCard as u8, 2);
        assert_eq!(StorageId::NandSystem as u8, 3);
        assert_eq!(StorageId::NandUser as u8, 4);
        assert_eq!(StorageId::SdCard as u8, 5);
    }

    #[test]
    fn test_new() {
        let factory = RomFSFactory::new(Some(make_test_file()), true);
        assert!(factory.file.is_some());
        assert!(factory.updatable);
    }

    #[test]
    fn test_open_current_process_not_updatable() {
        let file = make_test_file();
        let factory = RomFSFactory::new(Some(file.clone()), false);
        // When not updatable, should return the file directly.
        let result = factory.open_current_process(0x0100000000001000);
        assert!(result.is_some());
    }

    #[test]
    fn test_set_packed_update() {
        let mut factory = RomFSFactory::new(None, false);
        assert!(factory.packed_update_raw.is_none());
        factory.set_packed_update(make_test_file());
        assert!(factory.packed_update_raw.is_some());
    }

    #[test]
    fn test_open_patched_romfs_stub() {
        let factory = RomFSFactory::new(None, false);
        // Currently a stub, should return None.
        assert!(factory.open_patched_romfs(0x0100000000001000, ContentRecordType::Program).is_none());
    }
}
