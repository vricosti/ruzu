// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/types/store_data.h
//! Port of zuyu/src/core/hle/service/mii/types/store_data.cpp
//!
//! StoreData: the on-disk Mii storage format.
//! Size: 0x44 bytes in upstream.

use super::core_data::CoreData;

/// StoreData is the serialized form of a Mii stored in the database.
///
/// Layout:
/// - core_data: CoreData (0x30 bytes)
/// - create_id: u128 (0x10 bytes)
/// - crc: u16
/// - padding: u16
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct StoreData {
    pub core_data: CoreData,
    /// Create ID stored as raw bytes to avoid u128 alignment padding.
    pub create_id: [u8; 16],
    pub crc: u16,
    pub padding: u16,
}

impl StoreData {
    pub fn new() -> Self {
        Self {
            core_data: CoreData::default(),
            create_id: [0u8; 16],
            crc: 0,
            padding: 0,
        }
    }

    pub fn create_id_as_u128(&self) -> u128 {
        u128::from_le_bytes(self.create_id)
    }

    pub fn set_create_id(&mut self, id: u128) {
        self.create_id = id.to_le_bytes();
    }
}

impl Default for StoreData {
    fn default() -> Self {
        Self::new()
    }
}

const _: () = assert!(core::mem::size_of::<StoreData>() == 0x44);
