// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/
//! Upstream files:
//!   - nfc.h / nfc.cpp
//!   - nfc_types.h
//!   - nfc_result.h
//!   - nfc_interface.h / nfc_interface.cpp
//!   - mifare_types.h
//!   - mifare_result.h
//!   - common/device.h / common/device.cpp
//!   - common/device_manager.h / common/device_manager.cpp
//!   - common/amiibo_crypto.h / common/amiibo_crypto.cpp

pub mod common;
pub mod mifare_result;
pub mod mifare_types;
pub mod nfc;
pub mod nfc_interface;
pub mod nfc_result;
pub mod nfc_types;
