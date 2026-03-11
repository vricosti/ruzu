// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/patch_manager.h / .cpp

use super::nca_metadata::ContentRecordType;
use super::vfs::vfs_types::{VirtualDir, VirtualFile};

/// Patch type.
/// Corresponds to upstream `PatchType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatchType {
    Update,
    DLC,
    Mod,
}

/// A single patch entry.
/// Corresponds to upstream `Patch`.
#[derive(Debug, Clone)]
pub struct Patch {
    pub enabled: bool,
    pub name: String,
    pub version: String,
    pub patch_type: PatchType,
    pub program_id: u64,
    pub title_id: u64,
}

/// Build ID — 0x20 bytes.
pub type BuildId = [u8; 0x20];

/// Centralized manager for patches to games.
/// Corresponds to upstream `PatchManager`.
///
/// Stub: full implementation requires FileSystemController and ContentProvider ports.
pub struct PatchManager {
    title_id: u64,
    // TODO: references to fs_controller and content_provider.
}

impl PatchManager {
    pub fn new(title_id: u64) -> Self {
        Self { title_id }
    }

    pub fn get_title_id(&self) -> u64 {
        self.title_id
    }

    /// Patch ExeFS. Stub: returns input unchanged.
    /// TODO: implement update patching.
    pub fn patch_exefs(&self, exefs: VirtualDir) -> VirtualDir {
        exefs
    }

    /// Patch NSO data. Stub: returns input unchanged.
    /// TODO: implement IPS/IPSwitch patching.
    pub fn patch_nso(&self, nso: Vec<u8>, _name: &str) -> Vec<u8> {
        nso
    }

    /// Check if PatchNSO would have any effect.
    /// Stub: returns false.
    pub fn has_nso_patch(&self, _build_id: &BuildId, _name: &str) -> bool {
        false
    }

    /// Get the list of patches.
    /// Stub: returns empty.
    pub fn get_patches(&self, _update_raw: Option<VirtualFile>) -> Vec<Patch> {
        Vec::new()
    }

    /// Get the game version from update meta NCA, falling back to base.
    /// Stub: returns None.
    pub fn get_game_version(&self) -> Option<u32> {
        None
    }

    /// Patch RomFS with updates and LayeredFS.
    /// Stub: returns base_romfs unchanged.
    pub fn patch_romfs(
        &self,
        _base_romfs: VirtualFile,
        _record_type: ContentRecordType,
        _packed_update_raw: Option<VirtualFile>,
        _apply_layeredfs: bool,
    ) -> VirtualFile {
        todo!("PatchManager::patch_romfs not yet implemented")
    }
}
