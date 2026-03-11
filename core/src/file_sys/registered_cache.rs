// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/registered_cache.h / .cpp

use std::collections::BTreeMap;

use super::nca_metadata::{ContentRecordType, TitleType};
use super::vfs::vfs_types::{VirtualDir, VirtualFile};

/// NCA ID — first 16 bytes of SHA-256 over the entire file.
pub type NcaId = [u8; 0x10];

/// Result of an install operation.
/// Corresponds to upstream `InstallResult`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstallResult {
    Success,
    OverwriteExisting,
    ErrorAlreadyExists,
    ErrorCopyFailed,
    ErrorMetaFailed,
    ErrorBaseInstall,
}

/// An entry in a content provider.
/// Corresponds to upstream `ContentProviderEntry`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ContentProviderEntry {
    pub title_id: u64,
    pub record_type: ContentRecordType,
}

impl ContentProviderEntry {
    pub fn debug_info(&self) -> String {
        format!(
            "GitID={:016X}, Type={:?}",
            self.title_id, self.record_type
        )
    }
}

/// Get the update title ID for a base title.
/// Corresponds to upstream `GetUpdateTitleID`.
pub fn get_update_title_id(base_title_id: u64) -> u64 {
    base_title_id | 0x800
}

/// Convert NCA content type to content record type.
/// Corresponds to upstream `GetCRTypeFromNCAType`.
/// Stub: returns Meta as default.
pub fn get_cr_type_from_nca_type(_nca_type: u8) -> ContentRecordType {
    // TODO: implement proper mapping.
    ContentRecordType::Meta
}

// ============================================================================
// ContentProvider trait
// ============================================================================

/// Content provider interface.
/// Corresponds to upstream `ContentProvider`.
pub trait ContentProvider: Send + Sync {
    fn refresh(&mut self);
    fn has_entry(&self, title_id: u64, record_type: ContentRecordType) -> bool;
    fn get_entry_version(&self, title_id: u64) -> Option<u32>;
    fn get_entry_unparsed(
        &self,
        title_id: u64,
        record_type: ContentRecordType,
    ) -> Option<VirtualFile>;
    fn get_entry_raw(
        &self,
        title_id: u64,
        record_type: ContentRecordType,
    ) -> Option<VirtualFile>;
    fn list_entries_filter(
        &self,
        title_type: Option<TitleType>,
        record_type: Option<ContentRecordType>,
        title_id: Option<u64>,
    ) -> Vec<ContentProviderEntry>;

    fn has_entry_by_provider_entry(&self, entry: ContentProviderEntry) -> bool {
        self.has_entry(entry.title_id, entry.record_type)
    }

    fn list_entries(&self) -> Vec<ContentProviderEntry> {
        self.list_entries_filter(None, None, None)
    }
}

// ============================================================================
// PlaceholderCache
// ============================================================================

/// Placeholder cache for install operations.
/// Corresponds to upstream `PlaceholderCache`.
pub struct PlaceholderCache {
    dir: VirtualDir,
}

impl PlaceholderCache {
    pub fn new(dir: VirtualDir) -> Self {
        Self { dir }
    }

    pub fn create(&self, _id: &NcaId, _size: u64) -> bool {
        todo!("PlaceholderCache::create")
    }

    pub fn delete(&self, _id: &NcaId) -> bool {
        todo!("PlaceholderCache::delete")
    }

    pub fn exists(&self, _id: &NcaId) -> bool {
        false
    }

    pub fn list(&self) -> Vec<NcaId> {
        Vec::new()
    }

    pub fn generate() -> NcaId {
        [0u8; 0x10]
    }
}

// ============================================================================
// RegisteredCache
// ============================================================================

/// Registered cache — catalogues NCAs in the registered directory structure.
/// Corresponds to upstream `RegisteredCache`.
pub struct RegisteredCache {
    dir: VirtualDir,
    meta_id: BTreeMap<u64, NcaId>,
}

impl RegisteredCache {
    pub fn new(dir: VirtualDir) -> Self {
        let mut cache = Self {
            dir,
            meta_id: BTreeMap::new(),
        };
        cache.refresh();
        cache
    }

    pub fn remove_existing_entry(&self, _title_id: u64) -> bool {
        // TODO: implement.
        false
    }
}

impl ContentProvider for RegisteredCache {
    fn refresh(&mut self) {
        // TODO: scan directory for NCAs and build metadata index.
    }

    fn has_entry(&self, _title_id: u64, _record_type: ContentRecordType) -> bool {
        false
    }

    fn get_entry_version(&self, _title_id: u64) -> Option<u32> {
        None
    }

    fn get_entry_unparsed(
        &self,
        _title_id: u64,
        _record_type: ContentRecordType,
    ) -> Option<VirtualFile> {
        None
    }

    fn get_entry_raw(
        &self,
        _title_id: u64,
        _record_type: ContentRecordType,
    ) -> Option<VirtualFile> {
        None
    }

    fn list_entries_filter(
        &self,
        _title_type: Option<TitleType>,
        _record_type: Option<ContentRecordType>,
        _title_id: Option<u64>,
    ) -> Vec<ContentProviderEntry> {
        Vec::new()
    }
}

// ============================================================================
// ContentProviderUnionSlot
// ============================================================================

/// Slot identifier for ContentProviderUnion.
/// Corresponds to upstream `ContentProviderUnionSlot`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ContentProviderUnionSlot {
    SysNAND,
    UserNAND,
    SDMC,
    FrontendManual,
}

// ============================================================================
// ContentProviderUnion
// ============================================================================

/// Combines multiple ContentProviders into one interface.
/// Corresponds to upstream `ContentProviderUnion`.
pub struct ContentProviderUnion {
    providers: BTreeMap<ContentProviderUnionSlot, *mut dyn ContentProvider>,
}

// Safety: the pointers are managed by the caller's lifetime.
unsafe impl Send for ContentProviderUnion {}
unsafe impl Sync for ContentProviderUnion {}

impl ContentProviderUnion {
    pub fn new() -> Self {
        Self {
            providers: BTreeMap::new(),
        }
    }

    /// # Safety
    /// The caller must ensure the provider outlives this ContentProviderUnion.
    pub unsafe fn set_slot(&mut self, slot: ContentProviderUnionSlot, provider: *mut dyn ContentProvider) {
        self.providers.insert(slot, provider);
    }

    pub fn clear_slot(&mut self, slot: ContentProviderUnionSlot) {
        self.providers.remove(&slot);
    }
}

impl ContentProvider for ContentProviderUnion {
    fn refresh(&mut self) {
        for &ptr in self.providers.values() {
            unsafe { (*ptr).refresh() };
        }
    }

    fn has_entry(&self, title_id: u64, record_type: ContentRecordType) -> bool {
        self.providers.values().any(|&ptr| unsafe {
            (*ptr).has_entry(title_id, record_type)
        })
    }

    fn get_entry_version(&self, title_id: u64) -> Option<u32> {
        for &ptr in self.providers.values() {
            if let Some(v) = unsafe { (*ptr).get_entry_version(title_id) } {
                return Some(v);
            }
        }
        None
    }

    fn get_entry_unparsed(
        &self,
        title_id: u64,
        record_type: ContentRecordType,
    ) -> Option<VirtualFile> {
        for &ptr in self.providers.values() {
            if let Some(f) = unsafe { (*ptr).get_entry_unparsed(title_id, record_type) } {
                return Some(f);
            }
        }
        None
    }

    fn get_entry_raw(
        &self,
        title_id: u64,
        record_type: ContentRecordType,
    ) -> Option<VirtualFile> {
        for &ptr in self.providers.values() {
            if let Some(f) = unsafe { (*ptr).get_entry_raw(title_id, record_type) } {
                return Some(f);
            }
        }
        None
    }

    fn list_entries_filter(
        &self,
        title_type: Option<TitleType>,
        record_type: Option<ContentRecordType>,
        title_id: Option<u64>,
    ) -> Vec<ContentProviderEntry> {
        let mut result = Vec::new();
        for &ptr in self.providers.values() {
            result.extend(unsafe {
                (*ptr).list_entries_filter(title_type, record_type, title_id)
            });
        }
        result.sort();
        result.dedup();
        result
    }
}

impl Default for ContentProviderUnion {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// ManualContentProvider
// ============================================================================

/// Manually registered content provider.
/// Corresponds to upstream `ManualContentProvider`.
pub struct ManualContentProvider {
    entries: BTreeMap<(TitleType, ContentRecordType, u64), VirtualFile>,
}

impl ManualContentProvider {
    pub fn new() -> Self {
        Self {
            entries: BTreeMap::new(),
        }
    }

    pub fn add_entry(
        &mut self,
        title_type: TitleType,
        content_type: ContentRecordType,
        title_id: u64,
        file: VirtualFile,
    ) {
        self.entries
            .insert((title_type, content_type, title_id), file);
    }

    pub fn clear_all_entries(&mut self) {
        self.entries.clear();
    }
}

impl ContentProvider for ManualContentProvider {
    fn refresh(&mut self) {
        // No-op for manual provider.
    }

    fn has_entry(&self, title_id: u64, record_type: ContentRecordType) -> bool {
        self.entries
            .keys()
            .any(|&(_, rt, tid)| rt == record_type && tid == title_id)
    }

    fn get_entry_version(&self, _title_id: u64) -> Option<u32> {
        None
    }

    fn get_entry_unparsed(
        &self,
        title_id: u64,
        record_type: ContentRecordType,
    ) -> Option<VirtualFile> {
        self.entries
            .iter()
            .find(|(&(_, rt, tid), _)| rt == record_type && tid == title_id)
            .map(|(_, f)| f.clone())
    }

    fn get_entry_raw(
        &self,
        title_id: u64,
        record_type: ContentRecordType,
    ) -> Option<VirtualFile> {
        self.get_entry_unparsed(title_id, record_type)
    }

    fn list_entries_filter(
        &self,
        title_type: Option<TitleType>,
        record_type: Option<ContentRecordType>,
        title_id: Option<u64>,
    ) -> Vec<ContentProviderEntry> {
        self.entries
            .keys()
            .filter(|&&(tt, rt, tid)| {
                title_type.map_or(true, |t| t == tt)
                    && record_type.map_or(true, |r| r == rt)
                    && title_id.map_or(true, |id| id == tid)
            })
            .map(|&(_, rt, tid)| ContentProviderEntry {
                title_id: tid,
                record_type: rt,
            })
            .collect()
    }
}

impl Default for ManualContentProvider {
    fn default() -> Self {
        Self::new()
    }
}
