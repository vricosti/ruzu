// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/nfc.h` and `nfc.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! NFC protocol implementation for reading/writing amiibo and Mifare tags.

use common::input::DriverResult;

use super::common_protocol::JoyconCommonProtocol;
use super::joycon_types::*;

/// Number of times the function will be delayed until it outputs valid data.
const AMIIBO_UPDATE_DELAY: usize = 15;

/// Port of NfcProtocol::TagFoundData from nfc.h
#[derive(Debug, Clone)]
struct TagFoundData {
    tag_type: u8,
    uuid_size: u8,
    uuid: TagUUID,
}

/// Port of `NfcProtocol` class from nfc.h / nfc.cpp
pub struct NfcProtocol {
    protocol: JoyconCommonProtocol,
    is_enabled: bool,
    is_polling: bool,
    update_counter: usize,
}

impl NfcProtocol {
    /// Port of NfcProtocol::NfcProtocol
    pub fn new() -> Self {
        Self {
            protocol: JoyconCommonProtocol::new(),
            is_enabled: false,
            is_polling: false,
            update_counter: 0,
        }
    }

    /// Port of NfcProtocol::EnableNfc
    pub fn enable_nfc(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of NfcProtocol::DisableNfc
    pub fn disable_nfc(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of NfcProtocol::StartNFCPollingMode
    pub fn start_nfc_polling_mode(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of NfcProtocol::StopNFCPollingMode
    pub fn stop_nfc_polling_mode(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of NfcProtocol::GetTagInfo
    pub fn get_tag_info(&mut self, _tag_info: &mut TagInfo) -> DriverResult {
        todo!()
    }

    /// Port of NfcProtocol::ReadAmiibo
    pub fn read_amiibo(&mut self, _data: &mut Vec<u8>) -> DriverResult {
        todo!()
    }

    /// Port of NfcProtocol::WriteAmiibo
    pub fn write_amiibo(&mut self, _data: &[u8]) -> DriverResult {
        todo!()
    }

    /// Port of NfcProtocol::ReadMifare
    pub fn read_mifare(
        &mut self,
        _read_request: &[MifareReadChunk],
        _out_data: &mut [MifareReadData],
    ) -> DriverResult {
        todo!()
    }

    /// Port of NfcProtocol::WriteMifare
    pub fn write_mifare(&mut self, _write_request: &[MifareWriteChunk]) -> DriverResult {
        todo!()
    }

    /// Port of NfcProtocol::HasAmiibo
    pub fn has_amiibo(&self) -> bool {
        todo!()
    }

    /// Port of NfcProtocol::IsEnabled
    pub fn is_enabled(&self) -> bool {
        self.is_enabled
    }

    /// Port of NfcProtocol::IsPolling
    pub fn is_polling(&self) -> bool {
        self.is_polling
    }
}
