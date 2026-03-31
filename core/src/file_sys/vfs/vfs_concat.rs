// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/file_sys/vfs/vfs_concat.h and vfs_concat.cpp
//! ConcatenatedVfsFile: concatenates multiple VfsFiles into one seamless read-only file.

use std::sync::Arc;

use super::vfs::VfsFile;
use super::vfs_static::StaticVfsFile;
use super::vfs_types::{VirtualDir, VirtualFile};

/// An entry in the concatenation map, mapping a file to its starting offset.
///
/// Maps to upstream `ConcatenatedVfsFile::ConcatenationEntry`.
struct ConcatenationEntry {
    offset: u64,
    file: VirtualFile,
}

/// Class that wraps multiple vfs files and concatenates them, making reads seamless.
/// Currently read-only.
///
/// Maps to upstream `ConcatenatedVfsFile`.
pub struct ConcatenatedVfsFile {
    concatenation_map: Vec<ConcatenationEntry>,
    name: String,
}

impl ConcatenatedVfsFile {
    fn new(name: String, concatenation_map: Vec<ConcatenationEntry>) -> Self {
        debug_assert!(Self::verify_continuity(&concatenation_map));
        Self {
            concatenation_map,
            name,
        }
    }

    fn verify_continuity(map: &[ConcatenationEntry]) -> bool {
        let mut last_offset: u64 = 0;
        for entry in map {
            if entry.offset != last_offset {
                return false;
            }
            last_offset = entry.offset + entry.file.get_size() as u64;
        }
        true
    }

    /// Wrapper function to allow for more efficient handling of files.len() == 0, 1 cases.
    ///
    /// Maps to upstream `ConcatenatedVfsFile::MakeConcatenatedFile` (vector variant).
    pub fn make_concatenated_file(
        name: String,
        mut files: Vec<VirtualFile>,
    ) -> Option<VirtualFile> {
        // Fold trivial cases.
        if files.is_empty() {
            return None;
        }
        if files.len() == 1 {
            return Some(files.remove(0));
        }

        // Make the concatenation map from the input.
        let mut concatenation_map = Vec::with_capacity(files.len());
        let mut last_offset: u64 = 0;

        for file in files {
            let size = file.get_size() as u64;
            concatenation_map.push(ConcatenationEntry {
                offset: last_offset,
                file,
            });
            last_offset += size;
        }

        Some(Arc::new(ConcatenatedVfsFile::new(name, concatenation_map)))
    }

    /// Convenience function that turns a map of offsets to files into a concatenated file, filling
    /// gaps with a given filler byte.
    ///
    /// Maps to upstream `ConcatenatedVfsFile::MakeConcatenatedFile` (offset+filler variant).
    pub fn make_concatenated_file_with_filler(
        filler_byte: u8,
        name: String,
        mut files: Vec<(u64, VirtualFile)>,
    ) -> Option<VirtualFile> {
        // Fold trivial cases.
        if files.is_empty() {
            return None;
        }
        if files.len() == 1 {
            return Some(files.remove(0).1);
        }

        // Sort by offset to ensure ordering.
        files.sort_by_key(|(offset, _)| *offset);

        let mut concatenation_map = Vec::with_capacity(files.len());
        let mut last_offset: u64 = 0;

        for (offset, file) in files {
            let size = file.get_size() as u64;

            if offset > last_offset {
                // Fill gap with static file
                concatenation_map.push(ConcatenationEntry {
                    offset: last_offset,
                    file: Arc::new(StaticVfsFile::with_defaults(
                        filler_byte,
                        (offset - last_offset) as usize,
                    )),
                });
            }

            concatenation_map.push(ConcatenationEntry { offset, file });

            last_offset = offset + size;
        }

        Some(Arc::new(ConcatenatedVfsFile::new(name, concatenation_map)))
    }
}

impl VfsFile for ConcatenatedVfsFile {
    fn get_name(&self) -> String {
        if self.concatenation_map.is_empty() {
            return String::new();
        }
        if !self.name.is_empty() {
            return self.name.clone();
        }
        self.concatenation_map[0].file.get_name()
    }

    fn get_size(&self) -> usize {
        if self.concatenation_map.is_empty() {
            return 0;
        }
        let last = &self.concatenation_map[self.concatenation_map.len() - 1];
        (last.offset as usize) + last.file.get_size()
    }

    fn resize(&self, _new_size: usize) -> bool {
        false
    }

    fn get_containing_directory(&self) -> Option<VirtualDir> {
        if self.concatenation_map.is_empty() {
            return None;
        }
        self.concatenation_map[0].file.get_containing_directory()
    }

    fn is_writable(&self) -> bool {
        false
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn read(&self, data: &mut [u8], length: usize, offset: usize) -> usize {
        // Read nothing if the map is empty.
        if self.concatenation_map.is_empty() {
            return 0;
        }

        // Binary search to find the iterator to the first position we can check.
        let start_idx = match self
            .concatenation_map
            .binary_search_by(|entry| entry.offset.cmp(&(offset as u64)))
        {
            Ok(idx) => idx,
            Err(idx) => {
                if idx == 0 {
                    0
                } else {
                    idx - 1
                }
            }
        };

        let mut cur_length = length as u64;
        let mut cur_offset = offset as u64;

        let mut idx = start_idx;
        while cur_length > 0 && idx < self.concatenation_map.len() {
            let entry = &self.concatenation_map[idx];
            let file = &entry.file;
            let map_offset = entry.offset;
            let file_size = file.get_size() as u64;

            if cur_offset > map_offset + file_size {
                // Entirely out of bounds read.
                break;
            }

            // Read the file at this position.
            let file_seek = cur_offset - map_offset;
            let intended_read_size = cur_length.min(file_size - file_seek);
            let data_offset = (cur_offset - offset as u64) as usize;
            let actual_read_size = file.read(
                &mut data[data_offset..],
                intended_read_size as usize,
                file_seek as usize,
            ) as u64;

            // Update tracking.
            cur_offset += actual_read_size;
            cur_length -= actual_read_size;
            idx += 1;

            // If we encountered a short read, we're done.
            if actual_read_size < intended_read_size {
                break;
            }
        }

        (cur_offset - offset as u64) as usize
    }

    fn write(&self, _data: &[u8], _length: usize, _offset: usize) -> usize {
        0
    }

    fn rename(&self, _new_name: &str) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::VectorVfsFile;

    #[test]
    fn test_concatenated_empty() {
        assert!(ConcatenatedVfsFile::make_concatenated_file("empty".to_string(), vec![]).is_none());
    }

    #[test]
    fn test_concatenated_single() {
        let f: VirtualFile = Arc::new(VectorVfsFile::new(
            vec![1, 2, 3],
            "single".to_string(),
            None,
        ));
        let result =
            ConcatenatedVfsFile::make_concatenated_file("name".to_string(), vec![f]).unwrap();
        // Single file should be returned directly
        assert_eq!(result.get_size(), 3);
    }

    #[test]
    fn test_concatenated_multiple() {
        let f1: VirtualFile = Arc::new(VectorVfsFile::new(vec![1, 2, 3], "a".to_string(), None));
        let f2: VirtualFile = Arc::new(VectorVfsFile::new(vec![4, 5], "b".to_string(), None));
        let f3: VirtualFile = Arc::new(VectorVfsFile::new(vec![6], "c".to_string(), None));

        let concat =
            ConcatenatedVfsFile::make_concatenated_file("concat".to_string(), vec![f1, f2, f3])
                .unwrap();

        assert_eq!(concat.get_name(), "concat");
        assert_eq!(concat.get_size(), 6);
        assert!(concat.is_readable());
        assert!(!concat.is_writable());

        let all = concat.read_all_bytes();
        assert_eq!(all, vec![1, 2, 3, 4, 5, 6]);
    }

    #[test]
    fn test_concatenated_read_across_boundary() {
        let f1: VirtualFile = Arc::new(VectorVfsFile::new(vec![1, 2, 3], "a".to_string(), None));
        let f2: VirtualFile = Arc::new(VectorVfsFile::new(vec![4, 5, 6], "b".to_string(), None));

        let concat =
            ConcatenatedVfsFile::make_concatenated_file("concat".to_string(), vec![f1, f2])
                .unwrap();

        let mut buf = [0u8; 4];
        assert_eq!(concat.read(&mut buf, 4, 1), 4);
        assert_eq!(buf, [2, 3, 4, 5]);
    }

    #[test]
    fn test_concatenated_with_filler() {
        let f1: VirtualFile = Arc::new(VectorVfsFile::new(vec![1, 2], "a".to_string(), None));
        let f2: VirtualFile = Arc::new(VectorVfsFile::new(vec![3, 4], "b".to_string(), None));

        // f1 at offset 0 (size 2), gap at offset 2 (size 2), f2 at offset 4 (size 2)
        let concat = ConcatenatedVfsFile::make_concatenated_file_with_filler(
            0xFF,
            "filled".to_string(),
            vec![(0, f1), (4, f2)],
        )
        .unwrap();

        assert_eq!(concat.get_size(), 6);
        let all = concat.read_all_bytes();
        assert_eq!(all, vec![1, 2, 0xFF, 0xFF, 3, 4]);
    }
}
