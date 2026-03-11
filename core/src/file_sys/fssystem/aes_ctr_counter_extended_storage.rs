// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_aes_ctr_counter_extended_storage.h / .cpp

use super::bucket_tree::BucketTree;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use common::ResultCode;

pub const BLOCK_SIZE: usize = 0x10;
pub const KEY_SIZE: usize = 0x10;
pub const IV_SIZE: usize = 0x10;
pub const NODE_SIZE: usize = 16 * 1024;

/// Decryptor trait for AES-CTR counter extended storage.
/// Corresponds to upstream `AesCtrCounterExtendedStorage::IDecryptor`.
pub trait IDecryptor: Send + Sync {
    fn decrypt(&self, buf: &mut [u8], key: &[u8; KEY_SIZE], iv: &[u8; IV_SIZE]);
}

/// Encryption flag for entries.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum EntryEncryption {
    Encrypted = 0,
    NotEncrypted = 1,
}

/// Entry within the bucket tree for AES-CTR counter extended storage.
/// Corresponds to upstream `AesCtrCounterExtendedStorage::Entry`.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Entry {
    pub offset: [u8; 8],
    pub encryption_value: u8,
    pub reserved: [u8; 3],
    pub generation: i32,
}

const _: () = assert!(std::mem::size_of::<Entry>() == 0x10);

impl Entry {
    pub fn set_offset(&mut self, value: i64) {
        self.offset.copy_from_slice(&value.to_le_bytes());
    }

    pub fn get_offset(&self) -> i64 {
        i64::from_le_bytes(self.offset)
    }
}

/// AES-CTR counter extended storage.
/// Corresponds to upstream `AesCtrCounterExtendedStorage`.
pub struct AesCtrCounterExtendedStorage {
    table: BucketTree,
    data_storage: Option<VirtualFile>,
    key: [u8; KEY_SIZE],
    secure_value: u32,
    counter_offset: i64,
    decryptor: Option<Box<dyn IDecryptor>>,
}

impl AesCtrCounterExtendedStorage {
    pub fn new() -> Self {
        Self {
            table: BucketTree::new(),
            data_storage: None,
            key: [0u8; KEY_SIZE],
            secure_value: 0,
            counter_offset: 0,
            decryptor: None,
        }
    }

    pub fn initialize(
        &mut self,
        key: &[u8],
        secure_value: u32,
        counter_offset: i64,
        data_storage: VirtualFile,
        node_storage: VirtualFile,
        entry_storage: VirtualFile,
        entry_count: i32,
        decryptor: Box<dyn IDecryptor>,
    ) -> Result<(), ResultCode> {
        let key_len = std::cmp::min(key.len(), KEY_SIZE);
        self.key[..key_len].copy_from_slice(&key[..key_len]);
        self.secure_value = secure_value;
        self.counter_offset = counter_offset;
        self.data_storage = Some(data_storage);
        self.decryptor = Some(decryptor);

        self.table.initialize(
            node_storage,
            entry_storage,
            NODE_SIZE,
            std::mem::size_of::<Entry>(),
            entry_count,
        )
    }

    pub fn finalize(&mut self) {
        self.table.finalize();
        self.data_storage = None;
        self.decryptor = None;
    }

    pub fn is_initialized(&self) -> bool {
        self.table.is_initialized()
    }

    pub fn get_size(&self) -> usize {
        if let Ok(offsets) = self.table.get_offsets() {
            offsets.end_offset as usize
        } else {
            0
        }
    }

    /// Read from the storage. Stub: reads from underlying data storage.
    /// TODO: implement proper AES-CTR counter extended decryption.
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        if let Some(ref storage) = self.data_storage {
            storage.read(buffer, buffer.len(), offset)
        } else {
            0
        }
    }

    pub fn query_header_storage_size() -> i64 {
        BucketTree::query_header_storage_size()
    }

    pub fn query_node_storage_size(entry_count: i32) -> i64 {
        BucketTree::query_node_storage_size(NODE_SIZE, std::mem::size_of::<Entry>(), entry_count)
    }

    pub fn query_entry_storage_size(entry_count: i32) -> i64 {
        BucketTree::query_entry_storage_size(NODE_SIZE, std::mem::size_of::<Entry>(), entry_count)
    }
}

impl Default for AesCtrCounterExtendedStorage {
    fn default() -> Self {
        Self::new()
    }
}
