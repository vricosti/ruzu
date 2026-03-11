// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/spl_module.h
//! Port of zuyu/src/core/hle/service/spl/spl_module.cpp
//!
//! IGeneralInterface — SPL general service interface.

/// IPC command table for IGeneralInterface.
pub mod commands {
    pub const GET_CONFIG: u32 = 0;
    pub const MOD_CRYPT_AES: u32 = 1;
    pub const GENERATE_AESKEY: u32 = 2;
    pub const SET_CONFIG: u32 = 3;
    pub const GENERATE_RANDOM_BYTES: u32 = 4;
    pub const IMPORT_LOTUS_KEY: u32 = 5;
    pub const DECRYPT_LOTUS_MESSAGE: u32 = 6;
    pub const IS_DEVELOPMENT: u32 = 7;
    pub const GENERATE_SPECIFIC_AESKEY: u32 = 8;
    pub const DECRYPT_DEVICE_UNIQUE_DATA: u32 = 9;
    pub const DECRYPT_AESKEY: u32 = 10;
    pub const CRYPT_AESCTR: u32 = 11;
    pub const COMPUTE_CMAC: u32 = 12;
    pub const IMPORT_ESKEY: u32 = 13;
    pub const UNWRAP_TITLEKEY_ETC: u32 = 14;
    pub const LOAD_TITLEKEY: u32 = 15;
    pub const UNWRAP_COMMON_TITLEKEY: u32 = 16;
    pub const ALLOCATE_AESKEY_SLOT: u32 = 17;
    pub const DEALLOC_AESKEY_SLOT: u32 = 18;
    pub const GET_AESKEY_SLOT_AVAILABILITY: u32 = 19;
    pub const SET_BOOT_REASON: u32 = 20;
    pub const GET_BOOT_REASON: u32 = 21;
}

/// IGeneralInterface — SPL general interface.
///
/// Corresponds to `IGeneralInterface` in upstream spl_module.h / spl_module.cpp.
pub struct IGeneralInterface;

impl IGeneralInterface {
    pub fn new() -> Self {
        Self
    }

    /// GetConfig (cmd 0).
    pub fn get_config(&self, config_item: u32) -> u64 {
        log::debug!("IGeneralInterface::get_config (STUBBED) called, config_item={}", config_item);
        // TODO: implement config items matching upstream
        0
    }

    /// GenerateRandomBytes (cmd 4).
    pub fn generate_random_bytes(&self, buf: &mut [u8]) {
        log::debug!("IGeneralInterface::generate_random_bytes called, size={}", buf.len());
        // TODO: use proper RNG
        for byte in buf.iter_mut() {
            *byte = 0;
        }
    }
}
