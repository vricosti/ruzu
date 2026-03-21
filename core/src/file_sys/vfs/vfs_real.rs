// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/file_sys/vfs/vfs_real.h and vfs_real.cpp
//! RealVfsFilesystem, RealVfsFile, RealVfsDirectory: VFS backed by the real filesystem.

use std::collections::BTreeMap;
use std::path::Path;
use std::sync::{Arc, Mutex, Weak};

use common::fs::file::{IOFile, SeekOrigin};
use common::fs::fs as fs_ops;
use common::fs::fs_types::{FileAccessMode, FileShareFlag, FileType};
use common::fs::path_util;

use super::vfs::{VfsDirectory, VfsEntryType, VfsFile, VfsFilesystem};
use super::vfs_types::{FileTimeStampRaw, VirtualDir, VirtualFile};
use crate::file_sys::fs_filesystem::OpenMode;

// ============================================================================
// Constants
// ============================================================================

/// Maximum number of concurrently open file handles.
///
/// Maps to upstream `MaxOpenFiles`.
const MAX_OPEN_FILES: usize = 512;

// ============================================================================
// Helper: convert OpenMode to FileAccessMode
// ============================================================================

/// Maps to upstream `ModeFlagsToFileAccessMode`.
fn mode_flags_to_file_access_mode(mode: OpenMode) -> FileAccessMode {
    if mode == OpenMode::READ {
        FileAccessMode::Read
    } else {
        FileAccessMode::ReadWrite
    }
}

// ============================================================================
// FileReference
// ============================================================================

/// Tracks an open file handle with reference counting.
///
/// Maps to upstream `FileReference` (intrusive list node).
struct FileReference {
    file: Option<Arc<IOFile>>,
}

impl FileReference {
    fn new() -> Self {
        Self { file: None }
    }
}

// ============================================================================
// RealVfsFilesystem
// ============================================================================

/// VFS implementation backed by the real filesystem. Manages a cache of open
/// file handles with LRU eviction.
///
/// Maps to upstream `RealVfsFilesystem`.
pub struct RealVfsFilesystem {
    cache: Mutex<BTreeMap<String, Weak<dyn VfsFile>>>,
    open_references: Mutex<Vec<(String, FileReference)>>,
    num_open_files: Mutex<usize>,
}

impl RealVfsFilesystem {
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            cache: Mutex::new(BTreeMap::new()),
            open_references: Mutex::new(Vec::new()),
            num_open_files: Mutex::new(0),
        })
    }

    /// Opens a file, using the cache if possible.
    ///
    /// Maps to upstream `RealVfsFilesystem::OpenFileFromEntry`.
    fn open_file_from_entry(
        self: &Arc<Self>,
        path: &str,
        size: Option<u64>,
        parent_path: Option<String>,
        perms: OpenMode,
    ) -> Option<VirtualFile> {
        let sanitized = path_util::sanitize_path(path, path_util::DirectorySeparator::PlatformDefault);

        // Check cache
        {
            let cache = self.cache.lock().unwrap();
            if let Some(weak) = cache.get(&sanitized) {
                if let Some(file) = weak.upgrade() {
                    return Some(file);
                }
            }
        }

        if size.is_none() && !fs_ops::is_file(Path::new(&sanitized)) {
            return None;
        }

        let file: Arc<dyn VfsFile> = Arc::new(RealVfsFile::new(
            Arc::clone(self),
            sanitized.clone(),
            perms,
            size,
            parent_path.unwrap_or_else(|| path_util::get_parent_path(&sanitized)),
        ));

        let mut cache = self.cache.lock().unwrap();
        cache.insert(sanitized, Arc::downgrade(&file));

        Some(file)
    }

    fn evict_if_needed(&self) {
        let num = *self.num_open_files.lock().unwrap();
        if num < MAX_OPEN_FILES {
            return;
        }

        // Evict the oldest open reference
        let mut refs = self.open_references.lock().unwrap();
        if let Some(pos) = refs.iter().position(|(_, r)| r.file.is_some()) {
            refs[pos].1.file = None;
            *self.num_open_files.lock().unwrap() -= 1;
        }
    }

    /// Opens a fresh IOFile handle for the given path and permissions.
    /// Unlike upstream's RefreshReference which reuses handles, we open a fresh one
    /// each time because Rust's IOFile requires &mut self for read/write/seek.
    fn open_fresh_handle(&self, path: &str, perms: OpenMode) -> Option<IOFile> {
        let io_file = IOFile::new(
            Path::new(path),
            mode_flags_to_file_access_mode(perms),
            FileType::BinaryFile,
            FileShareFlag::ShareReadOnly,
        );
        if io_file.is_open() {
            Some(io_file)
        } else {
            None
        }
    }
}

impl Default for RealVfsFilesystem {
    fn default() -> Self {
        Self {
            cache: Mutex::new(BTreeMap::new()),
            open_references: Mutex::new(Vec::new()),
            num_open_files: Mutex::new(0),
        }
    }
}

impl VfsFilesystem for RealVfsFilesystem {
    fn get_name(&self) -> String {
        "Real".to_string()
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn is_writable(&self) -> bool {
        true
    }

    fn get_entry_type(&self, path: &str) -> VfsEntryType {
        let sanitized = path_util::sanitize_path(path, path_util::DirectorySeparator::PlatformDefault);
        let p = Path::new(&sanitized);
        if !fs_ops::exists(p) {
            return VfsEntryType::None;
        }
        if fs_ops::is_dir(p) {
            return VfsEntryType::Directory;
        }
        VfsEntryType::File
    }

    fn open_file(&self, path: &str, _perms: OpenMode) -> Option<VirtualFile> {
        // We need Arc<Self> but only have &self. For the real implementation this would
        // require the filesystem to be stored in an Arc. This is a structural limitation.
        // Users should call open_file_from_entry via Arc<RealVfsFilesystem>.
        None
    }

    fn create_file(&self, _path: &str, _perms: OpenMode) -> Option<VirtualFile> {
        None
    }

    fn copy_file(&self, _old_path: &str, _new_path: &str) -> Option<VirtualFile> {
        // Unused in upstream
        None
    }

    fn move_file(&self, _old_path: &str, _new_path: &str) -> Option<VirtualFile> {
        None
    }

    fn delete_file(&self, path: &str) -> bool {
        let sanitized = path_util::sanitize_path(path, path_util::DirectorySeparator::PlatformDefault);
        {
            let mut cache = self.cache.lock().unwrap();
            cache.remove(&sanitized);
        }
        fs_ops::remove_file(Path::new(&sanitized))
    }

    fn open_directory(&self, _path: &str, _perms: OpenMode) -> Option<VirtualDir> {
        None
    }

    fn create_directory(&self, path: &str, _perms: OpenMode) -> Option<VirtualDir> {
        let sanitized = path_util::sanitize_path(path, path_util::DirectorySeparator::PlatformDefault);
        if !fs_ops::create_dirs(Path::new(&sanitized)) {
            return None;
        }
        None
    }

    fn copy_directory(&self, _old_path: &str, _new_path: &str) -> Option<VirtualDir> {
        // Unused in upstream
        None
    }

    fn move_directory(&self, old_path: &str, new_path: &str) -> Option<VirtualDir> {
        let old = path_util::sanitize_path(old_path, path_util::DirectorySeparator::PlatformDefault);
        let new = path_util::sanitize_path(new_path, path_util::DirectorySeparator::PlatformDefault);
        if !fs_ops::rename_dir(Path::new(&old), Path::new(&new)) {
            return None;
        }
        self.open_directory(&new, OpenMode::READ_WRITE)
    }

    fn delete_directory(&self, path: &str) -> bool {
        let sanitized = path_util::sanitize_path(path, path_util::DirectorySeparator::PlatformDefault);
        fs_ops::remove_dir_recursively(Path::new(&sanitized))
    }
}

/// Arc-based API for RealVfsFilesystem, providing the full set of operations.
///
/// These methods require Arc<Self> because they create RealVfsFile/RealVfsDirectory instances
/// that hold a reference back to the filesystem.
impl RealVfsFilesystem {
    pub fn arc_open_file(self: &Arc<Self>, path: &str, perms: OpenMode) -> Option<VirtualFile> {
        self.open_file_from_entry(path, None, None, perms)
    }

    pub fn arc_create_file(self: &Arc<Self>, path: &str, perms: OpenMode) -> Option<VirtualFile> {
        let sanitized = path_util::sanitize_path(path, path_util::DirectorySeparator::PlatformDefault);
        {
            let mut cache = self.cache.lock().unwrap();
            cache.remove(&sanitized);
        }

        let p = Path::new(&sanitized);

        // Current usages of CreateFile expect to delete the contents of an existing file.
        if fs_ops::is_file(p) {
            let temp = IOFile::new(
                p,
                FileAccessMode::Write,
                FileType::BinaryFile,
                FileShareFlag::ShareReadOnly,
            );
            if !temp.is_open() {
                return None;
            }
            drop(temp);
            return self.arc_open_file(&sanitized, perms);
        }

        if !fs_ops::new_file(p, 0) {
            return None;
        }
        self.arc_open_file(&sanitized, perms)
    }

    pub fn arc_move_file(self: &Arc<Self>, old_path: &str, new_path: &str) -> Option<VirtualFile> {
        let old = path_util::sanitize_path(old_path, path_util::DirectorySeparator::PlatformDefault);
        let new = path_util::sanitize_path(new_path, path_util::DirectorySeparator::PlatformDefault);
        {
            let mut cache = self.cache.lock().unwrap();
            cache.remove(&old);
            cache.remove(&new);
        }
        if !fs_ops::rename_file(Path::new(&old), Path::new(&new)) {
            return None;
        }
        self.arc_open_file(&new, OpenMode::READ_WRITE)
    }

    pub fn arc_open_directory(self: &Arc<Self>, path: &str, perms: OpenMode) -> Option<VirtualDir> {
        let sanitized = path_util::sanitize_path(path, path_util::DirectorySeparator::PlatformDefault);
        Some(Arc::new(RealVfsDirectory::new(
            Arc::clone(self),
            sanitized,
            perms,
        )))
    }

    pub fn arc_create_directory(
        self: &Arc<Self>,
        path: &str,
        perms: OpenMode,
    ) -> Option<VirtualDir> {
        let sanitized = path_util::sanitize_path(path, path_util::DirectorySeparator::PlatformDefault);
        if !fs_ops::create_dirs(Path::new(&sanitized)) {
            return None;
        }
        Some(Arc::new(RealVfsDirectory::new(
            Arc::clone(self),
            sanitized,
            perms,
        )))
    }
}

// ============================================================================
// RealVfsFile
// ============================================================================

/// An implementation of VfsFile that represents a file on the user's computer.
///
/// Maps to upstream `RealVfsFile`.
pub struct RealVfsFile {
    base: Arc<RealVfsFilesystem>,
    path: String,
    parent_path: String,
    path_components: Vec<String>,
    size: Mutex<Option<u64>>,
    perms: OpenMode,
}

impl RealVfsFile {
    fn new(
        base: Arc<RealVfsFilesystem>,
        path: String,
        perms: OpenMode,
        size: Option<u64>,
        parent_path: String,
    ) -> Self {
        let path_components = path_util::split_path_components_copy(&path);
        Self {
            base,
            path,
            parent_path,
            path_components,
            size: Mutex::new(size),
            perms,
        }
    }
}

impl VfsFile for RealVfsFile {
    fn get_name(&self) -> String {
        if self.path_components.is_empty() {
            String::new()
        } else {
            self.path_components.last().unwrap().clone()
        }
    }

    fn get_size(&self) -> usize {
        let size_opt = *self.size.lock().unwrap();
        if let Some(s) = size_opt {
            return s as usize;
        }

        let io_file = match self.base.open_fresh_handle(&self.path, self.perms) {
            Some(f) => f,
            None => return 0,
        };
        io_file.get_size() as usize
    }

    fn resize(&self, new_size: usize) -> bool {
        *self.size.lock().unwrap() = None;
        let io_file = match self.base.open_fresh_handle(&self.path, self.perms) {
            Some(f) => f,
            None => return false,
        };
        io_file.set_size(new_size as u64)
    }

    fn get_containing_directory(&self) -> Option<VirtualDir> {
        self.base.arc_open_directory(&self.parent_path, self.perms)
    }

    fn is_writable(&self) -> bool {
        self.perms.contains(OpenMode::WRITE)
    }

    fn is_readable(&self) -> bool {
        self.perms.contains(OpenMode::READ)
    }

    fn read(&self, data: &mut [u8], length: usize, offset: usize) -> usize {
        let mut io_file = match self.base.open_fresh_handle(&self.path, self.perms) {
            Some(f) => f,
            None => return 0,
        };
        if !io_file.seek(offset as i64, SeekOrigin::SetOrigin) {
            return 0;
        }
        io_file.read_bytes(&mut data[..length])
    }

    fn write(&self, data: &[u8], length: usize, offset: usize) -> usize {
        *self.size.lock().unwrap() = None;
        let mut io_file = match self.base.open_fresh_handle(&self.path, self.perms) {
            Some(f) => f,
            None => return 0,
        };
        if !io_file.seek(offset as i64, SeekOrigin::SetOrigin) {
            return 0;
        }
        io_file.write_bytes(&data[..length])
    }

    fn rename(&self, name: &str) -> bool {
        self.base
            .arc_move_file(&self.path, &format!("{}/{}", self.parent_path, name))
            .is_some()
    }
}

// ============================================================================
// RealVfsDirectory
// ============================================================================

/// An implementation of VfsDirectory that represents a directory on the user's computer.
///
/// Maps to upstream `RealVfsDirectory`.
pub struct RealVfsDirectory {
    base: Arc<RealVfsFilesystem>,
    path: String,
    parent_path: String,
    path_components: Vec<String>,
    perms: OpenMode,
}

impl RealVfsDirectory {
    pub fn new(base: Arc<RealVfsFilesystem>, path: String, perms: OpenMode) -> Self {
        let cleaned = path_util::remove_trailing_slash(&path).to_string();
        let parent_path = path_util::get_parent_path(&cleaned);
        let path_components = path_util::split_path_components_copy(&cleaned);

        if !fs_ops::exists(Path::new(&cleaned)) && perms.contains(OpenMode::WRITE) {
            let _ = fs_ops::create_dirs(Path::new(&cleaned));
        }

        Self {
            base,
            path: cleaned,
            parent_path,
            path_components,
            perms,
        }
    }
}

impl VfsDirectory for RealVfsDirectory {
    fn get_file_relative(&self, relative_path: &str) -> Option<VirtualFile> {
        let full_path = path_util::sanitize_path(
            &format!("{}/{}", self.path, relative_path),
            path_util::DirectorySeparator::ForwardSlash,
        );
        let p = Path::new(&full_path);
        if !fs_ops::exists(p) || fs_ops::is_dir(p) {
            return None;
        }
        self.base.arc_open_file(&full_path, self.perms)
    }

    fn get_directory_relative(&self, relative_path: &str) -> Option<VirtualDir> {
        let full_path = path_util::sanitize_path(
            &format!("{}/{}", self.path, relative_path),
            path_util::DirectorySeparator::ForwardSlash,
        );
        let p = Path::new(&full_path);
        if !fs_ops::exists(p) || !fs_ops::is_dir(p) {
            return None;
        }
        self.base.arc_open_directory(&full_path, self.perms)
    }

    fn get_file(&self, name: &str) -> Option<VirtualFile> {
        self.get_file_relative(name)
    }

    fn get_subdirectory(&self, name: &str) -> Option<VirtualDir> {
        self.get_directory_relative(name)
    }

    fn create_file_relative(&self, relative_path: &str) -> Option<VirtualFile> {
        let full_path = path_util::sanitize_path(
            &format!("{}/{}", self.path, relative_path),
            path_util::DirectorySeparator::ForwardSlash,
        );
        if !fs_ops::create_parent_dirs(Path::new(&full_path)) {
            return None;
        }
        self.base.arc_create_file(&full_path, self.perms)
    }

    fn create_directory_relative(&self, relative_path: &str) -> Option<VirtualDir> {
        let full_path = path_util::sanitize_path(
            &format!("{}/{}", self.path, relative_path),
            path_util::DirectorySeparator::ForwardSlash,
        );
        self.base.arc_create_directory(&full_path, self.perms)
    }

    fn delete_subdirectory_recursive(&self, name: &str) -> bool {
        let full_path = path_util::sanitize_path(
            &format!("{}/{}", self.path, name),
            path_util::DirectorySeparator::ForwardSlash,
        );
        self.base.delete_directory(&full_path)
    }

    fn get_files(&self) -> Vec<VirtualFile> {
        if self.perms == OpenMode::ALLOW_APPEND {
            return Vec::new();
        }

        let mut out = Vec::new();
        let entries = match std::fs::read_dir(&self.path) {
            Ok(e) => e,
            Err(_) => return out,
        };

        for entry_result in entries {
            let entry = match entry_result {
                Ok(e) => e,
                Err(_) => continue,
            };

            let entry_path = entry.path();
            if entry_path.is_file() {
                let full = entry_path.to_string_lossy().to_string();
                let size = fs_ops::get_size(&entry_path);
                if let Some(file) = self.base.open_file_from_entry(
                    &full,
                    Some(size),
                    Some(self.path.clone()),
                    self.perms,
                ) {
                    out.push(file);
                }
            }
        }

        out
    }

    fn get_file_time_stamp(&self, path: &str) -> FileTimeStampRaw {
        let full_path = path_util::sanitize_path(
            &format!("{}/{}", self.path, path),
            path_util::DirectorySeparator::ForwardSlash,
        );

        #[cfg(unix)]
        {
            use std::ffi::CString;
            use std::os::unix::ffi::OsStrExt;

            let c_path = match CString::new(full_path.as_bytes()) {
                Ok(p) => p,
                Err(_) => return FileTimeStampRaw::default(),
            };

            unsafe {
                let mut stat: libc::stat = std::mem::zeroed();
                if libc::stat(c_path.as_ptr(), &mut stat) != 0 {
                    return FileTimeStampRaw::default();
                }

                FileTimeStampRaw {
                    created: stat.st_ctime as u64,
                    accessed: stat.st_atime as u64,
                    modified: stat.st_mtime as u64,
                    padding: 0,
                }
            }
        }

        #[cfg(not(unix))]
        {
            FileTimeStampRaw::default()
        }
    }

    fn get_subdirectories(&self) -> Vec<VirtualDir> {
        if self.perms == OpenMode::ALLOW_APPEND {
            return Vec::new();
        }

        let mut out = Vec::new();
        let entries = match std::fs::read_dir(&self.path) {
            Ok(e) => e,
            Err(_) => return out,
        };

        for entry_result in entries {
            let entry = match entry_result {
                Ok(e) => e,
                Err(_) => continue,
            };

            let entry_path = entry.path();
            if entry_path.is_dir() {
                let full = entry_path.to_string_lossy().to_string();
                if let Some(dir) = self.base.arc_open_directory(&full, self.perms) {
                    out.push(dir);
                }
            }
        }

        out
    }

    fn is_writable(&self) -> bool {
        self.perms.contains(OpenMode::WRITE)
    }

    fn is_readable(&self) -> bool {
        self.perms.contains(OpenMode::READ)
    }

    fn get_name(&self) -> String {
        if self.path_components.is_empty() {
            String::new()
        } else {
            self.path_components.last().unwrap().clone()
        }
    }

    fn get_parent_directory(&self) -> Option<VirtualDir> {
        if self.path_components.len() <= 1 {
            return None;
        }
        self.base.arc_open_directory(&self.parent_path, self.perms)
    }

    fn create_subdirectory(&self, name: &str) -> Option<VirtualDir> {
        let subdir_path = format!("{}/{}", self.path, name);
        self.base.arc_create_directory(&subdir_path, self.perms)
    }

    fn create_file(&self, name: &str) -> Option<VirtualFile> {
        let file_path = format!("{}/{}", self.path, name);
        self.base.arc_create_file(&file_path, self.perms)
    }

    fn delete_subdirectory(&self, name: &str) -> bool {
        let subdir_path = format!("{}/{}", self.path, name);
        self.base.delete_directory(&subdir_path)
    }

    fn delete_file(&self, name: &str) -> bool {
        let file_path = format!("{}/{}", self.path, name);
        self.base.delete_file(&file_path)
    }

    fn rename(&self, name: &str) -> bool {
        let new_name = format!("{}/{}", self.parent_path, name);
        self.base.arc_move_file(&self.path, &new_name).is_some()
    }

    fn get_full_path(&self) -> String {
        self.path.replace('\\', "/")
    }

    fn get_entries(&self) -> BTreeMap<String, VfsEntryType> {
        if self.perms == OpenMode::ALLOW_APPEND {
            return BTreeMap::new();
        }

        let mut out = BTreeMap::new();
        let entries = match std::fs::read_dir(&self.path) {
            Ok(e) => e,
            Err(_) => return out,
        };

        for entry_result in entries {
            let entry = match entry_result {
                Ok(e) => e,
                Err(_) => continue,
            };

            let entry_path = entry.path();
            if let Some(filename) = entry_path.file_name() {
                let name = filename.to_string_lossy().to_string();
                let entry_type = if entry_path.is_dir() {
                    VfsEntryType::Directory
                } else {
                    VfsEntryType::File
                };
                out.insert(name, entry_type);
            }
        }

        out
    }
}
