// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/file_sys/vfs/vfs_offset.h and vfs_offset.cpp
//! OffsetVfsFile: a view into a portion of another VfsFile.

use super::vfs::VfsFile;
use super::vfs_types::{VirtualDir, VirtualFile};

/// An implementation of VfsFile that wraps around another VfsFile at a certain offset.
/// Similar to seeking to an offset.
/// If the file is writable, operations that would write past the end of the offset file will expand
/// the size of this wrapper.
///
/// Maps to upstream `OffsetVfsFile`.
pub struct OffsetVfsFile {
    file: VirtualFile,
    offset: usize,
    size: std::sync::Mutex<usize>,
    name: String,
}

impl OffsetVfsFile {
    pub fn new(file: VirtualFile, size: usize, offset: usize, name: String) -> Self {
        Self {
            file,
            offset,
            size: std::sync::Mutex::new(size),
            name,
        }
    }

    /// Returns the offset into the backing file.
    ///
    /// Maps to upstream `OffsetVfsFile::GetOffset`.
    pub fn get_offset(&self) -> usize {
        self.offset
    }

    fn trim_to_fit(&self, r_size: usize, r_offset: usize) -> usize {
        let size = *self.size.lock().unwrap();
        if r_offset >= size {
            return 0;
        }
        r_size.min(size - r_offset)
    }
}

impl VfsFile for OffsetVfsFile {
    fn get_name(&self) -> String {
        if self.name.is_empty() {
            self.file.get_name()
        } else {
            self.name.clone()
        }
    }

    fn get_size(&self) -> usize {
        *self.size.lock().unwrap()
    }

    fn resize(&self, new_size: usize) -> bool {
        let mut size = self.size.lock().unwrap();
        if self.offset + new_size < self.file.get_size() {
            *size = new_size;
        } else {
            let res = self.file.resize(self.offset + new_size);
            if !res {
                return false;
            }
            *size = new_size;
        }
        true
    }

    fn get_containing_directory(&self) -> Option<VirtualDir> {
        None
    }

    fn is_writable(&self) -> bool {
        self.file.is_writable()
    }

    fn is_readable(&self) -> bool {
        self.file.is_readable()
    }

    fn read(&self, data: &mut [u8], length: usize, r_offset: usize) -> usize {
        self.file.read(
            data,
            self.trim_to_fit(length, r_offset),
            self.offset + r_offset,
        )
    }

    fn write(&self, data: &[u8], length: usize, r_offset: usize) -> usize {
        self.file.write(
            data,
            self.trim_to_fit(length, r_offset),
            self.offset + r_offset,
        )
    }

    fn read_byte(&self, r_offset: usize) -> Option<u8> {
        let size = *self.size.lock().unwrap();
        if r_offset >= size {
            return None;
        }
        self.file.read_byte(self.offset + r_offset)
    }

    fn read_bytes(&self, r_size: usize, r_offset: usize) -> Vec<u8> {
        self.file
            .read_bytes(self.trim_to_fit(r_size, r_offset), self.offset + r_offset)
    }

    fn read_all_bytes(&self) -> Vec<u8> {
        let size = *self.size.lock().unwrap();
        self.file.read_bytes(size, self.offset)
    }

    fn write_byte(&self, data: u8, r_offset: usize) -> bool {
        let size = *self.size.lock().unwrap();
        if r_offset < size {
            self.file.write_byte(data, self.offset + r_offset)
        } else {
            false
        }
    }

    fn write_bytes(&self, data: &[u8], r_offset: usize) -> usize {
        self.file.write(
            data,
            self.trim_to_fit(data.len(), r_offset),
            self.offset + r_offset,
        )
    }

    fn rename(&self, new_name: &str) -> bool {
        self.file.rename(new_name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::VectorVfsFile;
    use std::sync::Arc;

    #[test]
    fn test_offset_vfs_file_read() {
        let backing: VirtualFile = Arc::new(VectorVfsFile::new(
            vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
            "backing.bin".to_string(),
            None,
        ));
        let offset_file = OffsetVfsFile::new(backing, 5, 3, "offset.bin".to_string());

        assert_eq!(offset_file.get_name(), "offset.bin");
        assert_eq!(offset_file.get_size(), 5);
        assert_eq!(offset_file.get_offset(), 3);

        let mut buf = [0u8; 5];
        assert_eq!(offset_file.read(&mut buf, 5, 0), 5);
        assert_eq!(buf, [3, 4, 5, 6, 7]);
    }

    #[test]
    fn test_offset_vfs_file_read_byte() {
        let backing: VirtualFile = Arc::new(VectorVfsFile::new(
            vec![10, 20, 30, 40, 50],
            "".to_string(),
            None,
        ));
        let offset_file = OffsetVfsFile::new(backing, 3, 1, String::new());

        assert_eq!(offset_file.read_byte(0), Some(20));
        assert_eq!(offset_file.read_byte(2), Some(40));
        assert_eq!(offset_file.read_byte(3), None);
    }

    #[test]
    fn test_offset_vfs_file_read_all_bytes() {
        let backing: VirtualFile = Arc::new(VectorVfsFile::new(
            vec![0, 1, 2, 3, 4, 5],
            "".to_string(),
            None,
        ));
        let offset_file = OffsetVfsFile::new(backing, 3, 2, String::new());
        assert_eq!(offset_file.read_all_bytes(), vec![2, 3, 4]);
    }

    #[test]
    fn test_offset_vfs_file_name_fallback() {
        let backing: VirtualFile =
            Arc::new(VectorVfsFile::new(vec![0], "backing.bin".to_string(), None));
        let offset_file = OffsetVfsFile::new(backing, 1, 0, String::new());
        assert_eq!(offset_file.get_name(), "backing.bin");
    }

    #[test]
    fn test_offset_vfs_file_write() {
        let backing: VirtualFile = Arc::new(VectorVfsFile::new(
            vec![0, 0, 0, 0, 0],
            "".to_string(),
            None,
        ));
        let offset_file = OffsetVfsFile::new(backing.clone(), 3, 1, String::new());

        assert_eq!(offset_file.write(&[10, 20], 2, 0), 2);

        let all = backing.read_all_bytes();
        assert_eq!(all, vec![0, 10, 20, 0, 0]);
    }
}
