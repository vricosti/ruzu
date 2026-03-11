// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/kip.h and kip.cpp
//!
//! KIP (Kernel Initial Process) loader.

use crate::file_sys::vfs::vfs_types::VirtualFile;

use super::loader::{
    make_magic, AppLoader, FileType, FileTypeIdentifier, KProcess, LoadResult, ResultStatus,
    System,
};
use super::nso::read_object;

// ============================================================================
// Constants
// ============================================================================

/// Page-align a size value.
///
/// Maps to upstream anonymous `PageAlignSize` in kip.cpp.
#[allow(dead_code)]
const YUZU_PAGEMASK: u32 = 0xFFF;

#[allow(dead_code)]
const fn page_align_size(size: u32) -> u32 {
    (size + YUZU_PAGEMASK) & !YUZU_PAGEMASK
}

// ============================================================================
// AppLoaderKip
// ============================================================================

/// Loads a KIP file.
///
/// Maps to upstream `Loader::AppLoader_KIP`.
pub struct AppLoaderKip {
    file: VirtualFile,
    is_loaded: bool,
    // TODO: Replace with actual FileSys::KIP when ported.
    // kip: Option<FileSys::KIP>,
}

impl FileTypeIdentifier for AppLoaderKip {
    /// Identifies whether or not the given file is a KIP file.
    ///
    /// Maps to upstream `AppLoader_KIP::IdentifyType`.
    fn identify_type(in_file: &VirtualFile) -> FileType {
        if in_file.get_size() < std::mem::size_of::<u32>() {
            return FileType::Error;
        }
        let magic: Option<u32> = read_object::<u32>(in_file.as_ref(), 0);
        match magic {
            Some(m) if m == make_magic(b'K', b'I', b'P', b'1') => FileType::KIP,
            _ => FileType::Error,
        }
    }
}

impl AppLoaderKip {
    /// Create a new KIP loader.
    ///
    /// Maps to upstream `AppLoader_KIP::AppLoader_KIP`.
    pub fn new(file: VirtualFile) -> Self {
        // TODO: Create FileSys::KIP from file when that type is ported.
        Self {
            file,
            is_loaded: false,
        }
    }
}

impl AppLoader for AppLoaderKip {
    /// Maps to upstream `AppLoader_KIP::GetFileType`.
    ///
    /// Upstream checks kip status rather than re-reading magic:
    ///   (kip != nullptr && kip->GetStatus() == ResultStatus::Success)
    ///       ? FileType::KIP : FileType::Error
    fn get_file_type(&self) -> FileType {
        // TODO: When FileSys::KIP is ported, check kip status instead.
        Self::identify_type(&self.file)
    }

    /// Maps to upstream `AppLoader_KIP::Load`.
    fn load(&mut self, _process: &mut KProcess, _system: &mut System) -> LoadResult {
        if self.is_loaded {
            return (ResultStatus::ErrorAlreadyLoaded, None);
        }

        // TODO: Full implementation requires:
        // - FileSys::KIP to parse KIP content
        // - Check KIP status
        // - Determine address space type from KIP flags
        // - Create ProgramMetadata with KIP parameters
        // - Build CodeSet from text/rodata/data sections
        // - Page-align segments
        // - Setup process code layout via LoadFromMetadata
        // - Load module via codeset

        log::warn!("KIP loader: Load not fully implemented (requires FileSys::KIP)");
        (ResultStatus::ErrorNotImplemented, None)
    }
}
