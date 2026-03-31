// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_alignment_matching_storage.h

use super::alignment_matching_storage_impl::AlignmentMatchingStorageImpl;
use crate::file_sys::vfs::vfs_types::VirtualFile;

pub struct AlignmentMatchingStorage {
    base_storage: VirtualFile,
    data_align: usize,
    buffer_align: usize,
}

impl AlignmentMatchingStorage {
    pub const DATA_ALIGN_MAX: usize = 0x200;

    pub fn new(base: VirtualFile, data_align: usize, buffer_align: usize) -> Self {
        assert!(data_align.is_power_of_two());
        assert!(buffer_align.is_power_of_two());
        assert!(data_align <= Self::DATA_ALIGN_MAX);
        Self {
            base_storage: base,
            data_align,
            buffer_align,
        }
    }

    pub fn get_size(&self) -> usize {
        self.base_storage.get_size()
    }

    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        if buffer.is_empty() {
            return 0;
        }
        let mut work_buf = vec![0u8; self.data_align];
        AlignmentMatchingStorageImpl::read(
            &self.base_storage,
            &mut work_buf,
            self.data_align,
            self.data_align,
            self.buffer_align,
            offset,
            buffer,
            buffer.len(),
        )
    }

    pub fn write(&self, buffer: &[u8], offset: usize) -> usize {
        if buffer.is_empty() {
            return 0;
        }
        let mut work_buf = vec![0u8; self.data_align];
        AlignmentMatchingStorageImpl::write(
            &self.base_storage,
            &mut work_buf,
            self.data_align,
            self.data_align,
            self.buffer_align,
            offset,
            buffer,
            buffer.len(),
        )
    }
}

pub struct AlignmentMatchingStoragePooledBuffer {
    base_storage: VirtualFile,
    data_align: usize,
    buffer_align: usize,
}

impl AlignmentMatchingStoragePooledBuffer {
    pub fn new(base: VirtualFile, data_align: usize, buffer_align: usize) -> Self {
        assert!(data_align.is_power_of_two());
        assert!(buffer_align.is_power_of_two());
        Self {
            base_storage: base,
            data_align,
            buffer_align,
        }
    }

    pub fn get_size(&self) -> usize {
        self.base_storage.get_size()
    }

    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        if buffer.is_empty() {
            return 0;
        }
        let mut work_buf = vec![0u8; self.data_align];
        AlignmentMatchingStorageImpl::read(
            &self.base_storage,
            &mut work_buf,
            self.data_align,
            self.data_align,
            self.buffer_align,
            offset,
            buffer,
            buffer.len(),
        )
    }

    pub fn write(&self, buffer: &[u8], offset: usize) -> usize {
        if buffer.is_empty() {
            return 0;
        }
        let mut work_buf = vec![0u8; self.data_align];
        AlignmentMatchingStorageImpl::write(
            &self.base_storage,
            &mut work_buf,
            self.data_align,
            self.data_align,
            self.buffer_align,
            offset,
            buffer,
            buffer.len(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::VectorVfsFile;
    use std::sync::Arc;

    fn make_test_file(size: usize) -> VirtualFile {
        Arc::new(VectorVfsFile::new(
            vec![0xABu8; size],
            "test".to_string(),
            None,
        ))
    }

    #[test]
    fn test_alignment_matching_storage_new() {
        let file = make_test_file(512);
        let storage = AlignmentMatchingStorage::new(file, 0x200, 1);
        assert_eq!(storage.get_size(), 512);
    }

    #[test]
    #[should_panic]
    fn test_alignment_matching_storage_non_power_of_two() {
        let file = make_test_file(512);
        AlignmentMatchingStorage::new(file, 3, 1);
    }

    #[test]
    fn test_alignment_matching_storage_pooled_new() {
        let file = make_test_file(1024);
        let storage = AlignmentMatchingStoragePooledBuffer::new(file, 0x200, 1);
        assert_eq!(storage.get_size(), 1024);
    }

    #[test]
    fn test_read_empty() {
        let file = make_test_file(512);
        let storage = AlignmentMatchingStorage::new(file, 0x200, 1);
        let mut buf = [];
        assert_eq!(storage.read(&mut buf, 0), 0);
    }

    #[test]
    fn test_write_empty() {
        let file = make_test_file(512);
        let storage = AlignmentMatchingStorage::new(file, 0x200, 1);
        assert_eq!(storage.write(&[], 0), 0);
    }
}
