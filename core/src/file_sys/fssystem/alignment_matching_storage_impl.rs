// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_alignment_matching_storage_impl.h / .cpp

use crate::file_sys::vfs::vfs_types::VirtualFile;

pub struct AlignmentMatchingStorageImpl;

impl AlignmentMatchingStorageImpl {
    /// Stub: delegates directly to base storage.
    pub fn read(base_storage: &VirtualFile, _work_buf: &mut [u8], _work_buf_size: usize, _data_alignment: usize, _buffer_alignment: usize, offset: usize, buffer: &mut [u8], size: usize) -> usize {
        base_storage.read(buffer, size, offset)
    }

    /// Stub: delegates directly to base storage.
    pub fn write(base_storage: &VirtualFile, _work_buf: &mut [u8], _work_buf_size: usize, _data_alignment: usize, _buffer_alignment: usize, offset: usize, buffer: &[u8], size: usize) -> usize {
        base_storage.write(buffer, size, offset)
    }
}
