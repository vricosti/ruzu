// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/common/amiibo_crypto.h
//! Port of zuyu/src/core/hle/service/nfc/common/amiibo_crypto.cpp
//!
//! Amiibo encryption/decryption utilities.
//! Status: Stub

/// Validates an amiibo encryption key file.
pub fn is_amiibo_crypto_available() -> bool {
    // TODO: check for key file availability
    false
}

/// Decodes encrypted amiibo data.
pub fn decode_amiibo(_encrypted_data: &[u8]) -> Option<Vec<u8>> {
    // TODO: implement amiibo decryption
    log::warn!("(STUBBED) amiibo_crypto::decode_amiibo");
    None
}

/// Encodes amiibo data for writing.
pub fn encode_amiibo(_data: &[u8]) -> Option<Vec<u8>> {
    // TODO: implement amiibo encryption
    log::warn!("(STUBBED) amiibo_crypto::encode_amiibo");
    None
}
