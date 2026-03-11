// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_memory_resource_buffer_hold_storage.h

use crate::file_sys::vfs::vfs_types::VirtualFile;

pub struct MemoryResourceBufferHoldStorage {
    storage: VirtualFile,
    buffer: Vec<u8>,
}

impl MemoryResourceBufferHoldStorage {
    pub fn new(storage: VirtualFile, buffer_size: usize) -> Self {
        Self { storage, buffer: vec![0u8; buffer_size] }
    }

    pub fn is_valid(&self) -> bool { !self.buffer.is_empty() }
    pub fn get_buffer(&self) -> &[u8] { &self.buffer }
    pub fn get_buffer_mut(&mut self) -> &mut [u8] { &mut self.buffer }
    pub fn get_size(&self) -> usize { self.storage.get_size() }

    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        self.storage.read(buffer, buffer.len(), offset)
    }

    pub fn write(&self, buffer: &[u8], offset: usize) -> usize {
        self.storage.write(buffer, buffer.len(), offset)
    }
}
