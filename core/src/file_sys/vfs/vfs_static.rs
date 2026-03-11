// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/file_sys/vfs/vfs_static.h
//! StaticVfsFile: a read-only VFS file that fills reads with a constant byte value.

use std::sync::Mutex;

use super::vfs::VfsFile;
use super::vfs_types::VirtualDir;

/// A VfsFile implementation that returns a constant byte value for all reads.
/// The "file" has a configurable size but does not actually store data.
///
/// Maps to upstream `StaticVfsFile`.
pub struct StaticVfsFile {
    value: u8,
    size: Mutex<usize>,
    name: Mutex<String>,
    parent: Option<VirtualDir>,
}

impl StaticVfsFile {
    pub fn new(value: u8, size: usize, name: String, parent: Option<VirtualDir>) -> Self {
        Self {
            value,
            size: Mutex::new(size),
            name: Mutex::new(name),
            parent,
        }
    }

    /// Convenience constructor with default name and no parent.
    pub fn with_defaults(value: u8, size: usize) -> Self {
        Self::new(value, size, String::new(), None)
    }
}

impl VfsFile for StaticVfsFile {
    fn get_name(&self) -> String {
        self.name.lock().unwrap().clone()
    }

    fn get_size(&self) -> usize {
        *self.size.lock().unwrap()
    }

    fn resize(&self, new_size: usize) -> bool {
        let mut size = self.size.lock().unwrap();
        *size = new_size;
        true
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

    fn read(&self, data: &mut [u8], length: usize, offset: usize) -> usize {
        let size = *self.size.lock().unwrap();
        if offset >= size {
            return 0;
        }
        let read = length.min(size - offset);
        data[..read].fill(self.value);
        read
    }

    fn write(&self, _data: &[u8], _length: usize, _offset: usize) -> usize {
        0
    }

    fn read_byte(&self, offset: usize) -> Option<u8> {
        let size = *self.size.lock().unwrap();
        if offset >= size {
            None
        } else {
            Some(self.value)
        }
    }

    fn read_bytes(&self, length: usize, offset: usize) -> Vec<u8> {
        let size = *self.size.lock().unwrap();
        if offset >= size {
            return Vec::new();
        }
        let read = length.min(size - offset);
        vec![self.value; read]
    }

    fn rename(&self, new_name: &str) -> bool {
        let mut name = self.name.lock().unwrap();
        *name = new_name.to_string();
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_static_vfs_file_read() {
        let file = StaticVfsFile::new(0xAB, 10, "static.bin".to_string(), None);
        assert_eq!(file.get_name(), "static.bin");
        assert_eq!(file.get_size(), 10);
        assert!(file.is_readable());
        assert!(!file.is_writable());

        let mut buf = [0u8; 5];
        assert_eq!(file.read(&mut buf, 5, 3), 5);
        assert_eq!(buf, [0xAB; 5]);
    }

    #[test]
    fn test_static_vfs_file_read_byte() {
        let file = StaticVfsFile::new(0xFF, 3, "".to_string(), None);
        assert_eq!(file.read_byte(0), Some(0xFF));
        assert_eq!(file.read_byte(2), Some(0xFF));
        assert_eq!(file.read_byte(3), None);
    }

    #[test]
    fn test_static_vfs_file_read_bytes() {
        let file = StaticVfsFile::new(0x42, 5, "".to_string(), None);
        assert_eq!(file.read_bytes(3, 2), vec![0x42; 3]);
        assert_eq!(file.read_bytes(10, 3), vec![0x42; 2]);
    }

    #[test]
    fn test_static_vfs_file_resize() {
        let file = StaticVfsFile::new(0, 5, "".to_string(), None);
        assert!(file.resize(20));
        assert_eq!(file.get_size(), 20);
    }

    #[test]
    fn test_static_vfs_file_write_returns_zero() {
        let file = StaticVfsFile::new(0, 10, "".to_string(), None);
        assert_eq!(file.write(&[1, 2, 3], 3, 0), 0);
    }
}
