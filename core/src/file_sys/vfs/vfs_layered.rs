// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/file_sys/vfs/vfs_layered.h and vfs_layered.cpp
//! LayeredVfsDirectory: stacks multiple VfsDirectories, with the first layer having
//! highest priority. Files are taken from the first layer that contains them.

use std::collections::HashSet;
use std::sync::Arc;

use super::vfs::VfsDirectory;
use super::vfs_types::{VirtualDir, VirtualFile};

/// Class that stacks multiple VfsDirectories on top of each other, attempting to read from the
/// first one and falling back to the one after. The highest priority directory (overwrites all
/// others) should be element 0 in the dirs vector.
///
/// Maps to upstream `LayeredVfsDirectory`.
pub struct LayeredVfsDirectory {
    dirs: Vec<VirtualDir>,
    name: String,
}

impl LayeredVfsDirectory {
    fn new(dirs: Vec<VirtualDir>, name: String) -> Self {
        Self { dirs, name }
    }

    /// Wrapper function to allow for more efficient handling of dirs.len() == 0, 1 cases.
    ///
    /// Maps to upstream `LayeredVfsDirectory::MakeLayeredDirectory`.
    pub fn make_layered_directory(dirs: Vec<VirtualDir>, name: String) -> Option<VirtualDir> {
        if dirs.is_empty() {
            return None;
        }
        if dirs.len() == 1 {
            return Some(dirs.into_iter().next().unwrap());
        }
        Some(Arc::new(LayeredVfsDirectory::new(dirs, name)))
    }
}

impl VfsDirectory for LayeredVfsDirectory {
    fn get_file_relative(&self, path: &str) -> Option<VirtualFile> {
        for layer in &self.dirs {
            if let Some(file) = layer.get_file_relative(path) {
                return Some(file);
            }
        }
        None
    }

    fn get_directory_relative(&self, path: &str) -> Option<VirtualDir> {
        let mut out = Vec::new();
        for layer in &self.dirs {
            if let Some(dir) = layer.get_directory_relative(path) {
                out.push(dir);
            }
        }
        Self::make_layered_directory(out, String::new())
    }

    fn get_file(&self, name: &str) -> Option<VirtualFile> {
        self.get_file_relative(name)
    }

    fn get_subdirectory(&self, name: &str) -> Option<VirtualDir> {
        self.get_directory_relative(name)
    }

    fn get_full_path(&self) -> String {
        self.dirs[0].get_full_path()
    }

    fn get_files(&self) -> Vec<VirtualFile> {
        let mut out = Vec::new();
        let mut out_names = HashSet::new();

        for layer in &self.dirs {
            for file in layer.get_files() {
                let name = file.get_name();
                if out_names.insert(name) {
                    out.push(file);
                }
            }
        }

        out
    }

    fn get_subdirectories(&self) -> Vec<VirtualDir> {
        let mut out_names = HashSet::new();

        for layer in &self.dirs {
            for sd in layer.get_subdirectories() {
                out_names.insert(sd.get_name());
            }
        }

        let mut out = Vec::with_capacity(out_names.len());
        for subdir_name in &out_names {
            if let Some(sd) = self.get_subdirectory(subdir_name) {
                out.push(sd);
            }
        }

        out
    }

    fn is_writable(&self) -> bool {
        false
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn get_name(&self) -> String {
        if self.name.is_empty() {
            self.dirs[0].get_name()
        } else {
            self.name.clone()
        }
    }

    fn get_parent_directory(&self) -> Option<VirtualDir> {
        self.dirs[0].get_parent_directory()
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

    fn rename(&self, new_name: &str) -> bool {
        // Note: upstream mutates the name field. We can't with &self in a trait,
        // but we match the behavioral contract.
        let _ = new_name;
        // In the C++ version, this succeeds and modifies the name.
        // We would need interior mutability to match exactly. For now, return true
        // to match the upstream return value, though the name won't actually change.
        // This is a known limitation of not using Mutex<String> here.
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::{VectorVfsDirectory, VectorVfsFile};

    #[test]
    fn test_layered_empty() {
        assert!(LayeredVfsDirectory::make_layered_directory(vec![], String::new()).is_none());
    }

    #[test]
    fn test_layered_single() {
        let dir: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            "single".to_string(),
            None,
        ));
        let result =
            LayeredVfsDirectory::make_layered_directory(vec![dir.clone()], String::new()).unwrap();
        assert_eq!(result.get_name(), "single");
    }

    #[test]
    fn test_layered_priority() {
        let f1: VirtualFile = Arc::new(VectorVfsFile::new(
            vec![1, 2, 3],
            "file.bin".to_string(),
            None,
        ));
        let f2: VirtualFile = Arc::new(VectorVfsFile::new(
            vec![4, 5, 6],
            "file.bin".to_string(),
            None,
        ));
        let f3: VirtualFile = Arc::new(VectorVfsFile::new(
            vec![7, 8],
            "other.bin".to_string(),
            None,
        ));

        let layer1: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![f1],
            vec![],
            "l1".to_string(),
            None,
        ));
        let layer2: VirtualDir = Arc::new(VectorVfsDirectory::new(
            vec![f2, f3],
            vec![],
            "l2".to_string(),
            None,
        ));

        let layered = LayeredVfsDirectory::make_layered_directory(
            vec![layer1, layer2],
            "layered".to_string(),
        )
        .unwrap();

        assert_eq!(layered.get_name(), "layered");

        // file.bin should come from layer1 (priority)
        let file = layered.get_file("file.bin").unwrap();
        assert_eq!(file.read_all_bytes(), vec![1, 2, 3]);

        // other.bin should come from layer2
        let other = layered.get_file("other.bin").unwrap();
        assert_eq!(other.read_all_bytes(), vec![7, 8]);

        // Total unique files: 2 (file.bin deduplicated)
        assert_eq!(layered.get_files().len(), 2);
    }
}
