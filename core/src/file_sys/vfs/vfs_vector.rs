// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/file_sys/vfs/vfs_vector.h and vfs_vector.cpp
//! VectorVfsFile: in-memory VFS file backed by a Vec<u8>.
//! VectorVfsDirectory: in-memory VFS directory backed by vectors of files and dirs.

use std::sync::{Arc, Mutex};

use super::vfs::{VfsDirectory, VfsFile};
use super::vfs_types::{VirtualDir, VirtualFile};

// ============================================================================
// VectorVfsFile
// ============================================================================

/// An implementation of VfsFile that is backed by a vector optionally supplied upon construction.
///
/// Maps to upstream `VectorVfsFile`.
pub struct VectorVfsFile {
    data: Mutex<Vec<u8>>,
    parent: Option<VirtualDir>,
    name: Mutex<String>,
}

impl VectorVfsFile {
    pub fn new(initial_data: Vec<u8>, name: String, parent: Option<VirtualDir>) -> Self {
        Self {
            data: Mutex::new(initial_data),
            parent,
            name: Mutex::new(name),
        }
    }

    /// Replaces the internal data buffer with new data.
    ///
    /// Maps to upstream `VectorVfsFile::Assign`.
    pub fn assign(&self, new_data: Vec<u8>) {
        let mut data = self.data.lock().unwrap();
        *data = new_data;
    }
}

impl VfsFile for VectorVfsFile {
    fn get_name(&self) -> String {
        self.name.lock().unwrap().clone()
    }

    fn get_size(&self) -> usize {
        self.data.lock().unwrap().len()
    }

    fn resize(&self, new_size: usize) -> bool {
        let mut data = self.data.lock().unwrap();
        data.resize(new_size, 0);
        true
    }

    fn get_containing_directory(&self) -> Option<VirtualDir> {
        self.parent.clone()
    }

    fn is_writable(&self) -> bool {
        true
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn read(&self, data_out: &mut [u8], length: usize, offset: usize) -> usize {
        let data = self.data.lock().unwrap();
        if offset >= data.len() {
            return 0;
        }
        let read = length.min(data.len() - offset);
        data_out[..read].copy_from_slice(&data[offset..offset + read]);
        read
    }

    fn write(&self, data_in: &[u8], length: usize, offset: usize) -> usize {
        let mut data = self.data.lock().unwrap();
        if offset + length > data.len() {
            data.resize(offset + length, 0);
        }
        let write = length.min(data.len() - offset);
        data[offset..offset + write].copy_from_slice(&data_in[..write]);
        write
    }

    fn rename(&self, new_name: &str) -> bool {
        let mut name = self.name.lock().unwrap();
        *name = new_name.to_string();
        true
    }
}

// ============================================================================
// ArrayVfsFile (generic over const size, matching upstream template)
// ============================================================================

/// An implementation of VfsFile that is backed by a fixed-size byte array.
/// Since Rust doesn't support const-generic trait objects the same way C++ templates work,
/// this uses a Vec<u8> internally but is initialized from a fixed array.
///
/// Maps to upstream `ArrayVfsFile<size>`.
pub struct ArrayVfsFile {
    data: Vec<u8>,
    name: Mutex<String>,
    parent: Option<VirtualDir>,
}

impl ArrayVfsFile {
    pub fn new(data: Vec<u8>, name: String, parent: Option<VirtualDir>) -> Self {
        Self {
            data,
            name: Mutex::new(name),
            parent,
        }
    }
}

impl VfsFile for ArrayVfsFile {
    fn get_name(&self) -> String {
        self.name.lock().unwrap().clone()
    }

    fn get_size(&self) -> usize {
        self.data.len()
    }

    fn resize(&self, _new_size: usize) -> bool {
        false
    }

    fn get_containing_directory(&self) -> Option<VirtualDir> {
        self.parent.clone()
    }

    fn is_writable(&self) -> bool {
        false
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn read(&self, data_out: &mut [u8], length: usize, offset: usize) -> usize {
        if offset >= self.data.len() {
            return 0;
        }
        let read = length.min(self.data.len() - offset);
        data_out[..read].copy_from_slice(&self.data[offset..offset + read]);
        read
    }

    fn write(&self, _data_in: &[u8], _length: usize, _offset: usize) -> usize {
        0
    }

    fn rename(&self, new_name: &str) -> bool {
        let mut name = self.name.lock().unwrap();
        *name = new_name.to_string();
        true
    }
}

/// Convenience function to create an ArrayVfsFile.
///
/// Maps to upstream `MakeArrayFile`.
pub fn make_array_file(data: Vec<u8>, name: String, parent: Option<VirtualDir>) -> VirtualFile {
    Arc::new(ArrayVfsFile::new(data, name, parent))
}

// ============================================================================
// VectorVfsDirectory
// ============================================================================

/// An implementation of VfsDirectory that maintains two vectors for subdirectories and files.
/// Vector data is supplied upon construction.
///
/// Maps to upstream `VectorVfsDirectory`.
pub struct VectorVfsDirectory {
    files: Mutex<Vec<VirtualFile>>,
    dirs: Mutex<Vec<VirtualDir>>,
    parent: Option<VirtualDir>,
    name: Mutex<String>,
}

impl VectorVfsDirectory {
    pub fn new(
        files: Vec<VirtualFile>,
        dirs: Vec<VirtualDir>,
        name: String,
        parent: Option<VirtualDir>,
    ) -> Self {
        Self {
            files: Mutex::new(files),
            dirs: Mutex::new(dirs),
            parent,
            name: Mutex::new(name),
        }
    }

    /// Adds a file to this directory.
    ///
    /// Maps to upstream `VectorVfsDirectory::AddFile`.
    pub fn add_file(&self, file: VirtualFile) {
        self.files.lock().unwrap().push(file);
    }

    /// Adds a subdirectory to this directory.
    ///
    /// Maps to upstream `VectorVfsDirectory::AddDirectory`.
    pub fn add_directory(&self, dir: VirtualDir) {
        self.dirs.lock().unwrap().push(dir);
    }
}

impl VfsDirectory for VectorVfsDirectory {
    fn get_files(&self) -> Vec<VirtualFile> {
        self.files.lock().unwrap().clone()
    }

    fn get_subdirectories(&self) -> Vec<VirtualDir> {
        self.dirs.lock().unwrap().clone()
    }

    fn is_writable(&self) -> bool {
        false
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn get_name(&self) -> String {
        self.name.lock().unwrap().clone()
    }

    fn get_parent_directory(&self) -> Option<VirtualDir> {
        self.parent.clone()
    }

    fn delete_subdirectory(&self, subdir_name: &str) -> bool {
        let mut dirs = self.dirs.lock().unwrap();
        if let Some(pos) = dirs.iter().position(|d| d.get_name() == subdir_name) {
            dirs.remove(pos);
            true
        } else {
            false
        }
    }

    fn delete_file(&self, file_name: &str) -> bool {
        let mut files = self.files.lock().unwrap();
        if let Some(pos) = files.iter().position(|f| f.get_name() == file_name) {
            files.remove(pos);
            true
        } else {
            false
        }
    }

    fn rename(&self, new_name: &str) -> bool {
        let mut name = self.name.lock().unwrap();
        *name = new_name.to_string();
        true
    }

    fn create_subdirectory(&self, _name: &str) -> Option<VirtualDir> {
        None
    }

    fn create_file(&self, _name: &str) -> Option<VirtualFile> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_vfs_file_read_write() {
        let file = VectorVfsFile::new(vec![1, 2, 3, 4, 5], "test.bin".to_string(), None);
        assert_eq!(file.get_name(), "test.bin");
        assert_eq!(file.get_size(), 5);
        assert!(file.is_readable());
        assert!(file.is_writable());

        let mut buf = [0u8; 3];
        assert_eq!(file.read(&mut buf, 3, 1), 3);
        assert_eq!(buf, [2, 3, 4]);

        assert_eq!(file.write(&[10, 20], 2, 0), 2);
        let all = file.read_all_bytes();
        assert_eq!(all, vec![10, 20, 3, 4, 5]);
    }

    #[test]
    fn test_vector_vfs_file_resize() {
        let file = VectorVfsFile::new(vec![1, 2, 3], "test.bin".to_string(), None);
        assert!(file.resize(5));
        assert_eq!(file.get_size(), 5);
        assert!(file.resize(1));
        assert_eq!(file.get_size(), 1);
    }

    #[test]
    fn test_vector_vfs_file_assign() {
        let file = VectorVfsFile::new(vec![1, 2, 3], "test.bin".to_string(), None);
        file.assign(vec![10, 20]);
        assert_eq!(file.get_size(), 2);
        assert_eq!(file.read_all_bytes(), vec![10, 20]);
    }

    #[test]
    fn test_vector_vfs_file_rename() {
        let file = VectorVfsFile::new(vec![], "old.bin".to_string(), None);
        assert!(file.rename("new.bin"));
        assert_eq!(file.get_name(), "new.bin");
    }

    #[test]
    fn test_vector_vfs_directory() {
        let f1: VirtualFile =
            Arc::new(VectorVfsFile::new(vec![1], "a.bin".to_string(), None));
        let f2: VirtualFile =
            Arc::new(VectorVfsFile::new(vec![2], "b.bin".to_string(), None));
        let dir = VectorVfsDirectory::new(vec![f1, f2], vec![], "root".to_string(), None);

        assert_eq!(dir.get_name(), "root");
        assert_eq!(dir.get_files().len(), 2);
        assert!(dir.is_readable());
        assert!(!dir.is_writable());

        assert!(dir.get_file("a.bin").is_some());
        assert!(dir.get_file("c.bin").is_none());

        assert!(dir.delete_file("a.bin"));
        assert_eq!(dir.get_files().len(), 1);
    }

    #[test]
    fn test_array_vfs_file() {
        let file = ArrayVfsFile::new(vec![5, 10, 15], "arr.bin".to_string(), None);
        assert_eq!(file.get_size(), 3);
        assert!(file.is_readable());
        assert!(!file.is_writable());
        assert!(!file.resize(10));

        let mut buf = [0u8; 2];
        assert_eq!(file.read(&mut buf, 2, 1), 2);
        assert_eq!(buf, [10, 15]);

        assert_eq!(file.write(&[0], 1, 0), 0);
    }
}
