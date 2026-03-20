// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/file_sys/vfs/vfs.h and vfs.cpp
//! Core VFS traits: VfsFile, VfsDirectory, VfsFilesystem.
//! Also contains ReadOnlyVfsDirectory helper and free functions
//! DeepEquals, VfsRawCopy, VfsRawCopyD, GetOrCreateDirectoryRelative.

use std::collections::BTreeMap;
use std::sync::Arc;

use common::fs::path_util;

use super::vfs_types::{FileTimeStampRaw, VirtualDir, VirtualFile};
use crate::file_sys::fs_filesystem::OpenMode;

// ============================================================================
// VfsEntryType
// ============================================================================

/// An enumeration representing what can be at the end of a path in a VfsFilesystem.
///
/// Maps to upstream `VfsEntryType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VfsEntryType {
    None,
    File,
    Directory,
}

// ============================================================================
// VfsFile trait
// ============================================================================

/// A class representing a file in an abstract filesystem.
///
/// Maps to upstream `VfsFile`.
pub trait VfsFile: Send + Sync {
    /// Retrieves the file name.
    fn get_name(&self) -> String;

    /// Retrieves the extension of the file name.
    fn get_extension(&self) -> String {
        path_util::get_extension_from_filename(&self.get_name()).to_string()
    }

    /// Retrieves the size of the file.
    fn get_size(&self) -> usize;

    /// Resizes the file to new_size. Returns whether or not the operation was successful.
    fn resize(&self, new_size: usize) -> bool;

    /// Gets a pointer to the directory containing this file, returning None if there is none.
    fn get_containing_directory(&self) -> Option<VirtualDir>;

    /// Returns whether or not the file can be written to.
    fn is_writable(&self) -> bool;

    /// Returns whether or not the file can be read from.
    fn is_readable(&self) -> bool;

    /// The primary method of reading from the file. Reads length bytes into data starting at offset
    /// into file. Returns number of bytes successfully read.
    fn read(&self, data: &mut [u8], length: usize, offset: usize) -> usize;

    /// The primary method of writing to the file. Writes length bytes from data starting at offset
    /// into file. Returns number of bytes successfully written.
    fn write(&self, data: &[u8], length: usize, offset: usize) -> usize;

    /// Reads exactly one byte at the offset provided, returning None on error.
    fn read_byte(&self, offset: usize) -> Option<u8> {
        let mut out = [0u8; 1];
        let size = self.read(&mut out, 1, offset);
        if size == 1 {
            Some(out[0])
        } else {
            None
        }
    }

    /// Reads size bytes starting at offset in file into a vector.
    fn read_bytes(&self, size: usize, offset: usize) -> Vec<u8> {
        let mut out = vec![0u8; size];
        let read_size = self.read(&mut out, size, offset);
        out.truncate(read_size);
        out
    }

    /// Reads all the bytes from the file into a vector.
    fn read_all_bytes(&self) -> Vec<u8> {
        self.read_bytes(self.get_size(), 0)
    }

    /// Writes exactly one byte to offset in file and returns whether or not the byte was written
    /// successfully.
    fn write_byte(&self, data: u8, offset: usize) -> bool {
        self.write(&[data], 1, offset) == 1
    }

    /// Writes a vector of bytes to offset in file and returns the number of bytes successfully
    /// written.
    fn write_bytes(&self, data: &[u8], offset: usize) -> usize {
        self.write(data, data.len(), offset)
    }

    /// Renames the file to name. Returns whether or not the operation was successful.
    fn rename(&self, name: &str) -> bool;

    /// Returns the full path of this file as a string, recursively.
    fn get_full_path(&self) -> String {
        match self.get_containing_directory() {
            None => format!("/{}", self.get_name()),
            Some(dir) => format!("{}/{}", dir.get_full_path(), self.get_name()),
        }
    }
}

// ============================================================================
// VfsDirectory trait
// ============================================================================

/// A class representing a directory in an abstract filesystem.
///
/// Maps to upstream `VfsDirectory`.
pub trait VfsDirectory: Send + Sync {
    /// Returns a vector containing all of the files in this directory.
    fn get_files(&self) -> Vec<VirtualFile>;

    /// Returns the file with filename matching name. Returns None if directory doesn't have a
    /// file with name.
    fn get_file(&self, name: &str) -> Option<VirtualFile> {
        self.get_files()
            .into_iter()
            .find(|f| f.get_name() == name)
    }

    /// Returns a struct containing the file's timestamp.
    fn get_file_time_stamp(&self, _path: &str) -> FileTimeStampRaw {
        FileTimeStampRaw::default()
    }

    /// Returns a vector containing all of the subdirectories in this directory.
    fn get_subdirectories(&self) -> Vec<VirtualDir>;

    /// Returns the directory with name matching name. Returns None if directory doesn't have a
    /// directory with name.
    fn get_subdirectory(&self, name: &str) -> Option<VirtualDir> {
        self.get_subdirectories()
            .into_iter()
            .find(|d| d.get_name() == name)
    }

    /// Returns whether or not the directory can be written to.
    fn is_writable(&self) -> bool;

    /// Returns whether of not the directory can be read from.
    fn is_readable(&self) -> bool;

    /// Returns whether or not the directory is the root of the current file tree.
    fn is_root(&self) -> bool {
        self.get_parent_directory().is_none()
    }

    /// Returns the name of the directory.
    fn get_name(&self) -> String;

    /// Returns the total size of all files and subdirectories in this directory.
    fn get_size(&self) -> usize {
        let file_total: usize = self.get_files().iter().map(|f| f.get_size()).sum();
        let subdir_total: usize = self.get_subdirectories().iter().map(|d| d.get_size()).sum();
        file_total + subdir_total
    }

    /// Returns the parent directory of this directory. Returns None if this directory is root or
    /// has no parent.
    fn get_parent_directory(&self) -> Option<VirtualDir>;

    /// Creates a new subdirectory with the given name. Returns None on failure.
    fn create_subdirectory(&self, name: &str) -> Option<VirtualDir>;

    /// Creates a new file with the given name. Returns None on failure.
    fn create_file(&self, name: &str) -> Option<VirtualFile>;

    /// Deletes the subdirectory with the given name and returns true on success.
    fn delete_subdirectory(&self, name: &str) -> bool;

    /// Deletes all subdirectories and files within the provided directory and then deletes
    /// the directory itself. Returns true on success.
    fn delete_subdirectory_recursive(&self, name: &str) -> bool {
        let dir = match self.get_subdirectory(name) {
            Some(d) => d,
            None => return false,
        };

        let mut success = true;
        for file in dir.get_files() {
            if !self.delete_file(&file.get_name()) {
                success = false;
            }
        }

        for sdir in dir.get_subdirectories() {
            if !dir.delete_subdirectory_recursive(&sdir.get_name()) {
                success = false;
            }
        }

        success
    }

    /// Deletes all subdirectories and files within the provided directory.
    /// Unlike delete_subdirectory_recursive, this does not delete the provided directory.
    fn clean_subdirectory_recursive(&self, name: &str) -> bool {
        let dir = match self.get_subdirectory(name) {
            Some(d) => d,
            None => return false,
        };

        let mut success = true;
        for file in dir.get_files() {
            if !dir.delete_file(&file.get_name()) {
                success = false;
            }
        }

        for sdir in dir.get_subdirectories() {
            if !dir.delete_subdirectory_recursive(&sdir.get_name()) {
                success = false;
            }
        }

        success
    }

    /// Returns whether or not the file with name was deleted successfully.
    fn delete_file(&self, name: &str) -> bool;

    /// Returns whether or not this directory was renamed to name.
    fn rename(&self, name: &str) -> bool;

    /// Returns whether or not the file with name src was successfully copied to a new file with
    /// name dest.
    fn copy(&self, src: &str, dest: &str) -> bool {
        let f1 = match self.get_file(src) {
            Some(f) => f,
            None => return false,
        };
        let f2 = match self.create_file(dest) {
            Some(f) => f,
            None => return false,
        };

        if !f2.resize(f1.get_size()) {
            self.delete_file(dest);
            return false;
        }

        let all_bytes = f1.read_all_bytes();
        f2.write_bytes(&all_bytes, 0) == f1.get_size()
    }

    /// Gets all of the entries directly in the directory (files and dirs), returning a map between
    /// item name -> type.
    fn get_entries(&self) -> BTreeMap<String, VfsEntryType> {
        let mut out = BTreeMap::new();
        for dir in self.get_subdirectories() {
            out.insert(dir.get_name(), VfsEntryType::Directory);
        }
        for file in self.get_files() {
            out.insert(file.get_name(), VfsEntryType::File);
        }
        out
    }

    /// Returns the full path of this directory as a string, recursively.
    fn get_full_path(&self) -> String {
        if self.is_root() {
            return self.get_name();
        }

        match self.get_parent_directory() {
            Some(parent) => format!("{}/{}", parent.get_full_path(), self.get_name()),
            None => self.get_name(),
        }
    }

    /// Retrieves the file located at path as if the current directory was root. Returns None if
    /// not found.
    fn get_file_relative(&self, path: &str) -> Option<VirtualFile> {
        let vec = path_util::split_path_components(path);
        if vec.is_empty() {
            return None;
        }

        if vec.len() == 1 {
            return self.get_file(vec[0]);
        }

        let mut dir = self.get_subdirectory(vec[0])?;
        for component in &vec[1..vec.len() - 1] {
            dir = dir.get_subdirectory(component)?;
        }

        dir.get_file(vec.last().unwrap())
    }

    /// Calls get_file_relative(path) on the root of the current directory.
    fn get_file_absolute(&self, path: &str) -> Option<VirtualFile> {
        if self.is_root() {
            return self.get_file_relative(path);
        }

        self.get_parent_directory()?.get_file_absolute(path)
    }

    /// Retrieves the directory located at path as if the current directory was root. Returns None
    /// if not found.
    fn get_directory_relative(&self, path: &str) -> Option<VirtualDir> {
        let vec = path_util::split_path_components(path);
        if vec.is_empty() {
            // Upstream TODO(DarkLordZach): Return this directory if path is '/' or similar.
            // Can't currently because of const-ness
            return None;
        }

        let mut dir = self.get_subdirectory(vec[0])?;
        for component in &vec[1..] {
            dir = dir.get_subdirectory(component)?;
        }

        Some(dir)
    }

    /// Calls get_directory_relative(path) on the root of the current directory.
    fn get_directory_absolute(&self, path: &str) -> Option<VirtualDir> {
        if self.is_root() {
            return self.get_directory_relative(path);
        }

        self.get_parent_directory()?.get_directory_absolute(path)
    }

    /// Creates a new file at the path relative to this directory. Also creates directories if
    /// they do not exist and is supported by this implementation. Returns None on any failure.
    fn create_file_relative(&self, path: &str) -> Option<VirtualFile> {
        let vec = path_util::split_path_components(path);
        if vec.is_empty() {
            return None;
        }

        if vec.len() == 1 {
            return self.create_file(vec[0]);
        }

        let dir = match self.get_subdirectory(vec[0]) {
            Some(d) => d,
            None => self.create_subdirectory(vec[0])?,
        };

        dir.create_file_relative(path_util::get_path_without_top(path))
    }

    /// Creates a new file at the path relative to root of this directory.
    fn create_file_absolute(&self, path: &str) -> Option<VirtualFile> {
        if self.is_root() {
            return self.create_file_relative(path);
        }

        self.get_parent_directory()?.create_file_absolute(path)
    }

    /// Creates a new directory at the path relative to this directory.
    fn create_directory_relative(&self, path: &str) -> Option<VirtualDir> {
        let vec = path_util::split_path_components(path);
        if vec.is_empty() {
            return None;
        }

        if vec.len() == 1 {
            return self.create_subdirectory(vec[0]);
        }

        let dir = match self.get_subdirectory(vec[0]) {
            Some(d) => d,
            None => self.create_subdirectory(vec[0])?,
        };

        dir.create_directory_relative(path_util::get_path_without_top(path))
    }

    /// Creates a new directory at the path relative to root of this directory.
    fn create_directory_absolute(&self, path: &str) -> Option<VirtualDir> {
        if self.is_root() {
            return self.create_directory_relative(path);
        }

        self.get_parent_directory()?.create_directory_absolute(path)
    }
}

// ============================================================================
// VfsFilesystem trait
// ============================================================================

/// A class representing an abstract filesystem.
///
/// Maps to upstream `VfsFilesystem`.
pub trait VfsFilesystem: Send + Sync {
    /// Gets the friendly name for the filesystem.
    fn get_name(&self) -> String;

    /// Return whether or not the user has read permissions on this filesystem.
    fn is_readable(&self) -> bool;

    /// Return whether or not the user has write permission on this filesystem.
    fn is_writable(&self) -> bool;

    /// Determine if the entry at path is non-existent, a file, or a directory.
    fn get_entry_type(&self, path: &str) -> VfsEntryType;

    /// Opens the file with path relative to root.
    fn open_file(&self, path: &str, perms: OpenMode) -> Option<VirtualFile>;

    /// Creates a new, empty file at path.
    fn create_file(&self, path: &str, perms: OpenMode) -> Option<VirtualFile>;

    /// Copies the file from old_path to new_path.
    fn copy_file(&self, old_path: &str, new_path: &str) -> Option<VirtualFile>;

    /// Moves the file from old_path to new_path.
    fn move_file(&self, old_path: &str, new_path: &str) -> Option<VirtualFile>;

    /// Deletes the file with path relative to root, returning true on success.
    fn delete_file(&self, path: &str) -> bool;

    /// Opens the directory with path relative to root.
    fn open_directory(&self, path: &str, perms: OpenMode) -> Option<VirtualDir>;

    /// Creates a new, empty directory at path.
    fn create_directory(&self, path: &str, perms: OpenMode) -> Option<VirtualDir>;

    /// Copies the directory from old_path to new_path.
    fn copy_directory(&self, old_path: &str, new_path: &str) -> Option<VirtualDir>;

    /// Moves the directory from old_path to new_path.
    fn move_directory(&self, old_path: &str, new_path: &str) -> Option<VirtualDir>;

    /// Deletes the directory with path relative to root, returning true on success.
    fn delete_directory(&self, path: &str) -> bool;
}

// ============================================================================
// DefaultVfsFilesystem — default implementation backed by a root VirtualDir
// ============================================================================

/// A default VfsFilesystem implementation backed by a root VirtualDir.
///
/// Maps to upstream `VfsFilesystem` base class default implementations.
pub struct DefaultVfsFilesystem {
    root: VirtualDir,
}

impl DefaultVfsFilesystem {
    pub fn new(root: VirtualDir) -> Self {
        Self { root }
    }
}

impl VfsFilesystem for DefaultVfsFilesystem {
    fn get_name(&self) -> String {
        self.root.get_name()
    }

    fn is_readable(&self) -> bool {
        self.root.is_readable()
    }

    fn is_writable(&self) -> bool {
        self.root.is_writable()
    }

    fn get_entry_type(&self, path: &str) -> VfsEntryType {
        let path = path_util::sanitize_path(path, path_util::DirectorySeparator::ForwardSlash);
        if self.root.get_file_relative(&path).is_some() {
            return VfsEntryType::File;
        }
        if self.root.get_directory_relative(&path).is_some() {
            return VfsEntryType::Directory;
        }
        VfsEntryType::None
    }

    fn open_file(&self, path: &str, _perms: OpenMode) -> Option<VirtualFile> {
        let path = path_util::sanitize_path(path, path_util::DirectorySeparator::ForwardSlash);
        self.root.get_file_relative(&path)
    }

    fn create_file(&self, path: &str, _perms: OpenMode) -> Option<VirtualFile> {
        let path = path_util::sanitize_path(path, path_util::DirectorySeparator::ForwardSlash);
        self.root.create_file_relative(&path)
    }

    fn copy_file(&self, old_path: &str, new_path: &str) -> Option<VirtualFile> {
        let old_path =
            path_util::sanitize_path(old_path, path_util::DirectorySeparator::ForwardSlash);
        let new_path =
            path_util::sanitize_path(new_path, path_util::DirectorySeparator::ForwardSlash);

        // VfsDirectory impls are only required to implement copy across the current directory.
        if path_util::get_parent_path(&old_path) == path_util::get_parent_path(&new_path) {
            if !self.root.copy(
                path_util::get_filename(&old_path),
                path_util::get_filename(&new_path),
            ) {
                return None;
            }
            return self.open_file(&new_path, OpenMode::READ_WRITE);
        }

        // Do it using RawCopy. Non-default impls are encouraged to optimize this.
        let old_file = self.open_file(&old_path, OpenMode::READ)?;
        if self.open_file(&new_path, OpenMode::READ).is_some() {
            return None;
        }
        let new_file = self.create_file(&new_path, OpenMode::WRITE)?;
        if !vfs_raw_copy(old_file.as_ref(), new_file.as_ref(), 0x1000) {
            return None;
        }
        Some(new_file)
    }

    fn move_file(&self, old_path: &str, new_path: &str) -> Option<VirtualFile> {
        let sanitized_old =
            path_util::sanitize_path(old_path, path_util::DirectorySeparator::ForwardSlash);
        let sanitized_new =
            path_util::sanitize_path(new_path, path_util::DirectorySeparator::ForwardSlash);

        let out = self.copy_file(&sanitized_old, &sanitized_new)?;
        if self.delete_file(&sanitized_old) {
            Some(out)
        } else {
            None
        }
    }

    fn delete_file(&self, path: &str) -> bool {
        let path = path_util::sanitize_path(path, path_util::DirectorySeparator::ForwardSlash);
        let parent = match self.open_directory(
            path_util::get_parent_path(&path).as_str(),
            OpenMode::WRITE,
        ) {
            Some(p) => p,
            None => return false,
        };
        parent.delete_file(path_util::get_filename(&path))
    }

    fn open_directory(&self, path: &str, _perms: OpenMode) -> Option<VirtualDir> {
        let path = path_util::sanitize_path(path, path_util::DirectorySeparator::ForwardSlash);
        self.root.get_directory_relative(&path)
    }

    fn create_directory(&self, path: &str, _perms: OpenMode) -> Option<VirtualDir> {
        let path = path_util::sanitize_path(path, path_util::DirectorySeparator::ForwardSlash);
        self.root.create_directory_relative(&path)
    }

    fn copy_directory(&self, old_path: &str, new_path: &str) -> Option<VirtualDir> {
        let old_path =
            path_util::sanitize_path(old_path, path_util::DirectorySeparator::ForwardSlash);
        let new_path =
            path_util::sanitize_path(new_path, path_util::DirectorySeparator::ForwardSlash);

        let old_dir = self.open_directory(&old_path, OpenMode::READ)?;
        if self.open_directory(&new_path, OpenMode::READ).is_some() {
            return None;
        }
        let new_dir = self.create_directory(&new_path, OpenMode::WRITE)?;

        for file in old_dir.get_files() {
            let x = self.copy_file(
                &format!("{}/{}", old_path, file.get_name()),
                &format!("{}/{}", new_path, file.get_name()),
            );
            if x.is_none() {
                return None;
            }
        }

        for dir in old_dir.get_subdirectories() {
            let x = self.copy_directory(
                &format!("{}/{}", old_path, dir.get_name()),
                &format!("{}/{}", new_path, dir.get_name()),
            );
            if x.is_none() {
                return None;
            }
        }

        Some(new_dir)
    }

    fn move_directory(&self, old_path: &str, new_path: &str) -> Option<VirtualDir> {
        let sanitized_old =
            path_util::sanitize_path(old_path, path_util::DirectorySeparator::ForwardSlash);
        let sanitized_new =
            path_util::sanitize_path(new_path, path_util::DirectorySeparator::ForwardSlash);

        let out = self.copy_directory(&sanitized_old, &sanitized_new)?;
        if self.delete_directory(&sanitized_old) {
            Some(out)
        } else {
            None
        }
    }

    fn delete_directory(&self, path: &str) -> bool {
        let path = path_util::sanitize_path(path, path_util::DirectorySeparator::ForwardSlash);
        let parent = match self.open_directory(
            path_util::get_parent_path(&path).as_str(),
            OpenMode::WRITE,
        ) {
            Some(p) => p,
            None => return false,
        };
        parent.delete_subdirectory_recursive(path_util::get_filename(&path))
    }
}

// ============================================================================
// ReadOnlyVfsDirectory — convenience partial-implementation
// ============================================================================

/// A convenience partial-implementation of VfsDirectory that stubs out methods that should only
/// work if writable. This is to avoid redundant empty methods everywhere.
///
/// Maps to upstream `ReadOnlyVfsDirectory`.
///
/// Implementors must still provide: get_files, get_subdirectories, get_name,
/// get_parent_directory. The writable operations all return false/None.
pub trait ReadOnlyVfsDirectory: Send + Sync {
    fn get_files(&self) -> Vec<VirtualFile>;
    fn get_subdirectories(&self) -> Vec<VirtualDir>;
    fn get_name(&self) -> String;
    fn get_parent_directory(&self) -> Option<VirtualDir>;

    // Optional overrides for read-only directories
    fn get_file(&self, name: &str) -> Option<VirtualFile> {
        self.get_files()
            .into_iter()
            .find(|f| f.get_name() == name)
    }

    fn get_subdirectory(&self, name: &str) -> Option<VirtualDir> {
        self.get_subdirectories()
            .into_iter()
            .find(|d| d.get_name() == name)
    }

    fn get_file_time_stamp(&self, _path: &str) -> FileTimeStampRaw {
        FileTimeStampRaw::default()
    }
}

// ============================================================================
// Free functions
// ============================================================================

/// Compare the two files, byte-for-byte, in increments specified by block_size.
///
/// Maps to upstream `DeepEquals`.
pub fn deep_equals(file1: &dyn VfsFile, file2: &dyn VfsFile, block_size: usize) -> bool {
    if file1.get_size() != file2.get_size() {
        return false;
    }

    let mut f1_v = vec![0u8; block_size];
    let mut f2_v = vec![0u8; block_size];
    let total = file1.get_size();
    let mut i = 0;
    while i < total {
        let f1_vs = file1.read(&mut f1_v, block_size, i);
        let f2_vs = file2.read(&mut f2_v, block_size, i);

        if f1_vs != f2_vs {
            return false;
        }
        if f1_v[..f1_vs] != f2_v[..f2_vs] {
            return false;
        }
        i += block_size;
    }

    true
}

/// A method that copies the raw data between two different implementations of VirtualFile.
///
/// Maps to upstream `VfsRawCopy`.
pub fn vfs_raw_copy(src: &dyn VfsFile, dest: &dyn VfsFile, block_size: usize) -> bool {
    if !src.is_readable() || !dest.is_writable() {
        return false;
    }
    if !dest.resize(src.get_size()) {
        return false;
    }

    let src_size = src.get_size();
    let actual_block = block_size.min(src_size);
    if actual_block == 0 && src_size == 0 {
        return true;
    }
    let mut temp = vec![0u8; actual_block];
    let mut i = 0;
    while i < src_size {
        let read = block_size.min(src_size - i);

        if src.read(&mut temp[..read], read, i) != read {
            return false;
        }

        if dest.write(&temp[..read], read, i) != read {
            return false;
        }

        i += block_size;
    }

    true
}

/// A method that performs a similar function to vfs_raw_copy above, but instead copies entire
/// directories.
///
/// Maps to upstream `VfsRawCopyD`.
pub fn vfs_raw_copy_d(src: &dyn VfsDirectory, dest: &dyn VfsDirectory, block_size: usize) -> bool {
    if !src.is_readable() || !dest.is_writable() {
        return false;
    }

    for file in src.get_files() {
        let out = match dest.create_file(&file.get_name()) {
            Some(f) => f,
            None => return false,
        };
        if !vfs_raw_copy(file.as_ref(), out.as_ref(), block_size) {
            return false;
        }
    }

    for dir in src.get_subdirectories() {
        let out = match dest.create_subdirectory(&dir.get_name()) {
            Some(d) => d,
            None => return false,
        };
        if !vfs_raw_copy_d(dir.as_ref(), out.as_ref(), block_size) {
            return false;
        }
    }

    true
}

/// Checks if the directory at path relative to rel exists. If it does, returns that. If it does
/// not it attempts to create it and returns the new dir or None on failure.
///
/// Maps to upstream `GetOrCreateDirectoryRelative`.
pub fn get_or_create_directory_relative(rel: &dyn VfsDirectory, path: &str) -> Option<VirtualDir> {
    let res = rel.get_directory_relative(path);
    if res.is_some() {
        return res;
    }
    rel.create_directory_relative(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::VectorVfsFile;

    #[test]
    fn test_deep_equals_identical() {
        let data = vec![1u8, 2, 3, 4, 5];
        let f1 = VectorVfsFile::new(data.clone(), "a".to_string(), None);
        let f2 = VectorVfsFile::new(data, "b".to_string(), None);
        assert!(deep_equals(&f1, &f2, 0x1000));
    }

    #[test]
    fn test_deep_equals_different() {
        let f1 = VectorVfsFile::new(vec![1, 2, 3], "a".to_string(), None);
        let f2 = VectorVfsFile::new(vec![1, 2, 4], "b".to_string(), None);
        assert!(!deep_equals(&f1, &f2, 0x1000));
    }

    #[test]
    fn test_deep_equals_different_size() {
        let f1 = VectorVfsFile::new(vec![1, 2, 3], "a".to_string(), None);
        let f2 = VectorVfsFile::new(vec![1, 2], "b".to_string(), None);
        assert!(!deep_equals(&f1, &f2, 0x1000));
    }

    #[test]
    fn test_vfs_raw_copy() {
        let src = VectorVfsFile::new(vec![10, 20, 30, 40, 50], "src".to_string(), None);
        let dest = VectorVfsFile::new(vec![], "dest".to_string(), None);
        assert!(vfs_raw_copy(&src, &dest, 0x1000));
        assert_eq!(dest.read_all_bytes(), vec![10, 20, 30, 40, 50]);
    }
}
