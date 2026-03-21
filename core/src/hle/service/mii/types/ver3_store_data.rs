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

/// NfpStoreDataExtension stores additional Mii color/type data for NFP.
///
/// Corresponds to `NfpStoreDataExtension` in upstream ver3_store_data.h.
/// Size: 0x8 bytes.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NfpStoreDataExtension {
    pub faceline_color: u8,
    pub hair_color: u8,
    pub eye_color: u8,
    pub eyebrow_color: u8,
    pub mouth_color: u8,
    pub beard_color: u8,
    pub glass_color: u8,
    pub glass_type: u8,
}

impl Default for NfpStoreDataExtension {
    fn default() -> Self {
        // SAFETY: NfpStoreDataExtension is repr(C) and all-zeros is valid
        unsafe { core::mem::zeroed() }
    }
}

const _: () = assert!(core::mem::size_of::<NfpStoreDataExtension>() == 0x8);
