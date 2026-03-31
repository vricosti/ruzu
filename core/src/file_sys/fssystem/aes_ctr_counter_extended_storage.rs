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

/// Software AES-CTR decryptor using AesCipher.
/// Corresponds to upstream `SoftwareDecryptor` (fssystem_aes_ctr_counter_extended_storage.cpp:13-18).
pub struct SoftwareDecryptor;

impl IDecryptor for SoftwareDecryptor {
    fn decrypt(&self, buf: &mut [u8], key: &[u8; KEY_SIZE], iv: &[u8; IV_SIZE]) {
        use crate::crypto::aes_util::{AesCipher, Mode, Op};
        let mut cipher = AesCipher::new_128(*key, Mode::CTR);
        cipher.set_iv(iv);
        let mut tmp = buf.to_vec();
        cipher.transcode(buf, &mut tmp, Op::Decrypt);
        buf.copy_from_slice(&tmp);
    }
}

/// Create a software decryptor.
/// Corresponds to upstream `AesCtrCounterExtendedStorage::CreateSoftwareDecryptor`.
pub fn create_software_decryptor() -> Box<dyn IDecryptor> {
    Box::new(SoftwareDecryptor)
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

    /// Read from the storage with per-entry AES-CTR decryption.
    ///
    /// Upstream: `AesCtrCounterExtendedStorage::Read`
    /// (fssystem_aes_ctr_counter_extended_storage.cpp:89-186).
    ///
    /// 1. Read raw (encrypted) data from data_storage
    /// 2. Find the starting bucket tree entry for the offset
    /// 3. For each entry, if encryption_value == Encrypted:
    ///    a. Compute counter_offset = m_counter_offset + entry_offset + data_offset
    ///    b. Build upper_iv from entry.generation and m_secure_value
    ///    c. MakeIv(iv, upper_iv.value, counter_offset)
    ///    d. Decrypt data in-place using AES-128-CTR with m_key and iv
    pub fn read(&self, buffer: &mut [u8], offset: usize) -> usize {
        use super::aes_ctr_storage::AesCtrStorage;
        use super::nca_header::NcaAesCtrUpperIv;

        let size = buffer.len();
        if size == 0 {
            return 0;
        }

        let data_storage = match self.data_storage {
            Some(ref s) => s,
            None => return 0,
        };

        // Read the raw (encrypted) data.
        let read = data_storage.read(buffer, size, offset);
        if read == 0 {
            return 0;
        }

        // Find the offset in our tree.
        let mut visitor = match self.table.find(offset as i64) {
            Ok(v) => v,
            Err(_) => return read,
        };

        // Prepare to decrypt in chunks per bucket tree entry.
        let mut cur_data_offset = 0usize; // offset into buffer
        let mut cur_offset = offset as i64;
        let end_offset = (offset + size) as i64;

        while cur_offset < end_offset {
            // Get the current entry.
            let cur_entry: Entry = unsafe { *visitor.get::<Entry>() };
            let cur_entry_offset = cur_entry.get_offset();

            // Get the next entry offset.
            let next_entry_offset = if visitor.can_move_next() {
                let _ = visitor.move_next();
                let next: &Entry = unsafe { visitor.get::<Entry>() };
                next.get_offset()
            } else {
                // Use table end offset if no more entries.
                if let Ok(offsets) = self.table.get_offsets() {
                    offsets.end_offset
                } else {
                    end_offset
                }
            };

            // Compute the data range for this entry.
            let data_offset_in_entry = cur_offset - cur_entry_offset;
            let data_size_in_entry = (next_entry_offset - cur_entry_offset) - data_offset_in_entry;
            let remaining = end_offset - cur_offset;
            let cur_size = std::cmp::min(remaining, data_size_in_entry) as usize;

            if cur_size == 0 {
                break;
            }

            // Decrypt if entry is marked as encrypted.
            if cur_entry.encryption_value == EntryEncryption::Encrypted as u8 {
                if let Some(ref decryptor) = self.decryptor {
                    // Build the IV matching upstream logic:
                    //   counter_offset = m_counter_offset + cur_entry_offset + data_offset_in_entry
                    //   upper_iv.generation = cur_entry.generation
                    //   upper_iv.secure_value = m_secure_value
                    //   MakeIv(iv, upper_iv.value, counter_offset)
                    let counter_offset =
                        self.counter_offset + cur_entry_offset + data_offset_in_entry;

                    let mut upper_iv = NcaAesCtrUpperIv::default();
                    upper_iv.set_generation(cur_entry.generation as u32);
                    upper_iv.value =
                        (upper_iv.value & 0xFFFF_FFFF) | ((self.secure_value as u64) << 32);

                    let mut iv = [0u8; IV_SIZE];
                    AesCtrStorage::make_iv(&mut iv, upper_iv.value, counter_offset);

                    // Decrypt the data in-place.
                    let buf_slice = &mut buffer[cur_data_offset..cur_data_offset + cur_size];
                    decryptor.decrypt(buf_slice, &self.key, &iv);
                }
            }

            cur_data_offset += cur_size;
            cur_offset += cur_size as i64;
        }

        read
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constants() {
        assert_eq!(BLOCK_SIZE, 16);
        assert_eq!(KEY_SIZE, 16);
        assert_eq!(IV_SIZE, 16);
        assert_eq!(NODE_SIZE, 16384);
    }

    #[test]
    fn test_entry_size() {
        assert_eq!(std::mem::size_of::<Entry>(), 0x10);
    }

    #[test]
    fn test_entry_offset_round_trip() {
        let mut entry = Entry {
            offset: [0u8; 8],
            encryption_value: 0,
            reserved: [0u8; 3],
            generation: 0,
        };
        entry.set_offset(0xDEAD_BEEF);
        assert_eq!(entry.get_offset(), 0xDEAD_BEEF);

        entry.set_offset(-42);
        assert_eq!(entry.get_offset(), -42);
    }

    #[test]
    fn test_entry_encryption() {
        assert_eq!(EntryEncryption::Encrypted as u8, 0);
        assert_eq!(EntryEncryption::NotEncrypted as u8, 1);
    }

    #[test]
    fn test_new() {
        let storage = AesCtrCounterExtendedStorage::new();
        assert!(!storage.is_initialized());
    }

    #[test]
    fn test_default() {
        let storage = AesCtrCounterExtendedStorage::default();
        assert!(!storage.is_initialized());
    }
}
