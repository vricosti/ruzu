// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/types/ver3_store_data.h
//! Port of zuyu/src/core/hle/service/mii/types/ver3_store_data.cpp
//!
//! Ver3StoreData: the version 3 (3DS-compatible) Mii storage format.
//! Size: 0x60 bytes in upstream.

/// Ver3StoreData is the legacy Mii format compatible with 3DS.
///
/// This is a raw byte representation; accessor methods will be added
/// as the port progresses.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Ver3StoreData {
    pub data: [u8; 0x60],
}

impl Ver3StoreData {
    pub fn new() -> Self {
        Self { data: [0u8; 0x60] }
    }
}

impl Default for Ver3StoreData {
    fn default() -> Self {
        Self::new()
    }
}

const _: () = assert!(core::mem::size_of::<Ver3StoreData>() == 0x60);
