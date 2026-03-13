// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/nfc.h` and `nfc.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! NFC protocol implementation for reading/writing amiibo and Mifare tags.

use common::input::DriverResult;

use super::common_protocol::{JoyconCommonProtocol, ScopedSetBlocking};
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
    ///
    /// Sequence: SetReportMode(NFC_IR_MODE_60HZ) -> EnableMCU(true) ->
    /// WaitSetMCUMode(Standby) -> ConfigureMCU(NFC) -> WaitSetMCUMode(NFC) ->
    /// WaitUntilNfcIs(Ready) -> SendStopPollingRequest -> WaitUntilNfcIs(Ready).
    pub fn enable_nfc(&mut self) -> DriverResult {
        log::info!("enable_nfc: NFC enable requested");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;

        if result == DriverResult::Success {
            result = self.protocol.set_report_mode(ReportMode::NfcIrMode60Hz);
        }
        if result == DriverResult::Success {
            result = self.protocol.enable_mcu(true);
        }
        if result == DriverResult::Success {
            result = self
                .protocol
                .wait_set_mcu_mode(ReportMode::NfcIrMode60Hz, McuMode::Standby);
        }
        if result == DriverResult::Success {
            // ConfigureMCU(NFC mode) — depends on MCUConfig struct not yet in joycon_types.rs
            result = self.protocol.configure_mcu();
        }
        if result == DriverResult::Success {
            result = self
                .protocol
                .wait_set_mcu_mode(ReportMode::NfcIrMode60Hz, McuMode::Nfc);
        }
        if result == DriverResult::Success {
            result = self.wait_until_nfc_is(JoyconNfcStatus::Ready);
        }
        if result == DriverResult::Success {
            result = self.send_stop_polling_request();
        }
        if result == DriverResult::Success {
            result = self.wait_until_nfc_is(JoyconNfcStatus::Ready);
        }
        if result == DriverResult::Success {
            self.is_enabled = true;
        }

        result
    }

    /// Port of NfcProtocol::DisableNfc
    pub fn disable_nfc(&mut self) -> DriverResult {
        log::debug!("disable_nfc: NFC disable requested");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;

        if result == DriverResult::Success {
            result = self.protocol.enable_mcu(false);
        }

        self.is_enabled = false;
        self.is_polling = false;

        result
    }

    /// Port of NfcProtocol::StartNFCPollingMode
    pub fn start_nfc_polling_mode(&mut self) -> DriverResult {
        log::debug!("start_nfc_polling_mode: starting NFC polling");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;

        if result == DriverResult::Success {
            result = self.send_start_polling_request(false);
        }
        if result == DriverResult::Success {
            result = self.wait_until_nfc_is(JoyconNfcStatus::Polling);
        }
        if result == DriverResult::Success {
            self.is_polling = true;
        }

        result
    }

    /// Port of NfcProtocol::StopNFCPollingMode
    pub fn stop_nfc_polling_mode(&mut self) -> DriverResult {
        log::debug!("stop_nfc_polling_mode: stopping NFC polling");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;

        if result == DriverResult::Success {
            result = self.send_stop_polling_request();
        }
        if result == DriverResult::Success {
            result = self.wait_until_nfc_is(JoyconNfcStatus::WriteReady);
        }
        if result == DriverResult::Success {
            self.is_polling = false;
        }

        result
    }

    /// Port of NfcProtocol::GetTagInfo
    pub fn get_tag_info(&mut self, tag_info: &mut TagInfo) -> DriverResult {
        if self.update_counter < AMIIBO_UPDATE_DELAY {
            self.update_counter += 1;
            return DriverResult::Delayed;
        }
        self.update_counter = 0;

        log::debug!("get_tag_info: scanning for amiibos");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;
        let mut tag_data = TagFoundData {
            tag_type: 0,
            uuid_size: 0,
            uuid: [0u8; 7],
        };

        if result == DriverResult::Success {
            result = self.is_tag_in_range(&mut tag_data, 1);
        }

        if result == DriverResult::Success {
            *tag_info = TagInfo {
                uuid_length: tag_data.uuid_size,
                protocol: 1,
                tag_type: tag_data.tag_type,
                uuid: [0u8; 10],
            };

            let copy_len = tag_data.uuid_size as usize;
            let copy_len = copy_len.min(10).min(tag_data.uuid.len());
            tag_info.uuid[..copy_len].copy_from_slice(&tag_data.uuid[..copy_len]);

            // Investigate why mifare type is not correct
            if tag_info.tag_type == 144 {
                tag_info.tag_type = 1u8 << 6;
            }

            log::info!(
                "get_tag_info: tag detected, type={}, uuid={:02x?}",
                tag_data.tag_type,
                &tag_data.uuid
            );
        }

        result
    }

    /// Port of NfcProtocol::ReadAmiibo
    pub fn read_amiibo(&mut self, data: &mut Vec<u8>) -> DriverResult {
        log::debug!("read_amiibo: scanning for amiibos");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;
        let mut tag_data = TagFoundData {
            tag_type: 0,
            uuid_size: 0,
            uuid: [0u8; 7],
        };

        if result == DriverResult::Success {
            result = self.is_tag_in_range(&mut tag_data, 7);
        }

        if result == DriverResult::Success {
            result = self.get_amiibo_data(data);
        }

        result
    }

    /// Port of NfcProtocol::WriteAmiibo
    pub fn write_amiibo(&mut self, data: &[u8]) -> DriverResult {
        log::debug!("write_amiibo: writing amiibo");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;
        let tag_uuid = self.get_tag_uuid(data);
        let mut tag_data = TagFoundData {
            tag_type: 0,
            uuid_size: 0,
            uuid: [0u8; 7],
        };

        if result == DriverResult::Success {
            result = self.is_tag_in_range(&mut tag_data, 7);
        }
        if result == DriverResult::Success {
            if tag_data.uuid != tag_uuid {
                result = DriverResult::InvalidParameters;
            }
        }
        if result == DriverResult::Success {
            result = self.send_stop_polling_request();
        }
        if result == DriverResult::Success {
            result = self.wait_until_nfc_is(JoyconNfcStatus::Ready);
        }
        if result == DriverResult::Success {
            result = self.send_start_polling_request(true);
        }
        if result == DriverResult::Success {
            result = self.wait_until_nfc_is(JoyconNfcStatus::WriteReady);
        }
        if result == DriverResult::Success {
            result = self.write_amiibo_data(&tag_uuid, data);
        }
        if result == DriverResult::Success {
            result = self.wait_until_nfc_is(JoyconNfcStatus::WriteDone);
        }
        if result == DriverResult::Success {
            result = self.send_stop_polling_request();
        }

        result
    }

    /// Port of NfcProtocol::ReadMifare
    pub fn read_mifare(
        &mut self,
        read_request: &[MifareReadChunk],
        out_data: &mut [MifareReadData],
    ) -> DriverResult {
        log::debug!("read_mifare: reading mifare");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;
        let mut tag_data = TagFoundData {
            tag_type: 0,
            uuid_size: 0,
            uuid: [0u8; 7],
        };

        if result == DriverResult::Success {
            result = self.is_tag_in_range(&mut tag_data, 7);
        }
        if result == DriverResult::Success {
            let mut tag_uuid = [0u8; 4];
            let copy_len = 4.min(tag_data.uuid.len());
            tag_uuid[..copy_len].copy_from_slice(&tag_data.uuid[..copy_len]);
            result = self.get_mifare_data(&tag_uuid, read_request, out_data);
        }
        if result == DriverResult::Success {
            result = self.send_stop_polling_request();
        }
        if result == DriverResult::Success {
            result = self.wait_until_nfc_is(JoyconNfcStatus::Ready);
        }
        if result == DriverResult::Success {
            result = self.send_start_polling_request(true);
        }
        if result == DriverResult::Success {
            result = self.wait_until_nfc_is(JoyconNfcStatus::WriteReady);
        }

        result
    }

    /// Port of NfcProtocol::WriteMifare
    pub fn write_mifare(&mut self, write_request: &[MifareWriteChunk]) -> DriverResult {
        log::debug!("write_mifare: writing mifare");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;
        let mut tag_data = TagFoundData {
            tag_type: 0,
            uuid_size: 0,
            uuid: [0u8; 7],
        };

        if result == DriverResult::Success {
            result = self.is_tag_in_range(&mut tag_data, 7);
        }
        if result == DriverResult::Success {
            let mut tag_uuid = [0u8; 4];
            let copy_len = 4.min(tag_data.uuid.len());
            tag_uuid[..copy_len].copy_from_slice(&tag_data.uuid[..copy_len]);
            result = self.write_mifare_data(&tag_uuid, write_request);
        }
        if result == DriverResult::Success {
            result = self.send_stop_polling_request();
        }
        if result == DriverResult::Success {
            result = self.wait_until_nfc_is(JoyconNfcStatus::Ready);
        }
        if result == DriverResult::Success {
            result = self.send_start_polling_request(true);
        }
        if result == DriverResult::Success {
            result = self.wait_until_nfc_is(JoyconNfcStatus::WriteReady);
        }

        result
    }

    /// Port of NfcProtocol::HasAmiibo
    ///
    /// NOTE: In C++ this is non-const; Rust requires &mut self because of update_counter
    /// and ScopedSetBlocking.
    pub fn has_amiibo(&mut self) -> bool {
        if self.update_counter < AMIIBO_UPDATE_DELAY {
            self.update_counter += 1;
            return true;
        }
        self.update_counter = 0;

        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut tag_data = TagFoundData {
            tag_type: 0,
            uuid_size: 0,
            uuid: [0u8; 7],
        };

        let result = self.is_tag_in_range(&mut tag_data, 7);
        result == DriverResult::Success
    }

    /// Port of NfcProtocol::IsEnabled
    pub fn is_enabled(&self) -> bool {
        self.is_enabled
    }

    /// Port of NfcProtocol::IsPolling
    pub fn is_polling(&self) -> bool {
        self.is_polling
    }

    // ---- Private methods ----

    /// Port of NfcProtocol::WaitUntilNfcIs
    ///
    /// NOTE: Polls SendNextPackageRequest (MCU data read) until mcu_data matches.
    /// Depends on the hidapi handle.
    fn wait_until_nfc_is(&mut self, _status: JoyconNfcStatus) -> DriverResult {
        log::warn!("wait_until_nfc_is: no hidapi handle available");
        DriverResult::Timeout
    }

    /// Port of NfcProtocol::IsTagInRange
    ///
    /// NOTE: Polls SendNextPackageRequest in a loop reading MCUCommandResponse.
    /// Depends on the hidapi handle.
    fn is_tag_in_range(
        &mut self,
        _data: &mut TagFoundData,
        _timeout_limit: usize,
    ) -> DriverResult {
        log::warn!("is_tag_in_range: no hidapi handle available");
        DriverResult::Timeout
    }

    /// Port of NfcProtocol::GetAmiiboData
    ///
    /// NOTE: Sends SendReadAmiiboRequest then polls SendNextPackageRequest reading
    /// MCUCommandResponse data. Depends on the hidapi handle.
    fn get_amiibo_data(&mut self, _ntag_data: &mut Vec<u8>) -> DriverResult {
        log::warn!("get_amiibo_data: no hidapi handle available");
        DriverResult::Timeout
    }

    /// Port of NfcProtocol::WriteAmiiboData
    ///
    /// NOTE: Sends write packets to the device via MCU data channel.
    /// Depends on the hidapi handle.
    fn write_amiibo_data(&mut self, _tag_uuid: &TagUUID, _data: &[u8]) -> DriverResult {
        log::warn!("write_amiibo_data: no hidapi handle available");
        DriverResult::Timeout
    }

    /// Port of NfcProtocol::GetMifareData
    ///
    /// NOTE: Sends Mifare read packets and collects responses via MCU data channel.
    /// Depends on the hidapi handle.
    fn get_mifare_data(
        &mut self,
        _tag_uuid: &MifareUUID,
        _read_request: &[MifareReadChunk],
        _out_data: &mut [MifareReadData],
    ) -> DriverResult {
        log::warn!("get_mifare_data: no hidapi handle available");
        DriverResult::Timeout
    }

    /// Port of NfcProtocol::WriteMifareData
    ///
    /// NOTE: Sends Mifare write packets via MCU data channel.
    /// Depends on the hidapi handle.
    fn write_mifare_data(
        &mut self,
        _tag_uuid: &MifareUUID,
        _write_request: &[MifareWriteChunk],
    ) -> DriverResult {
        log::warn!("write_mifare_data: no hidapi handle available");
        DriverResult::Timeout
    }

    /// Port of NfcProtocol::SendStartPollingRequest
    ///
    /// NOTE: Builds an NFCRequestState with NFCCommand::StartPolling and sends it via
    /// SendMCUData. Depends on the hidapi handle.
    fn send_start_polling_request(&mut self, _is_second_attempt: bool) -> DriverResult {
        log::warn!("send_start_polling_request: no hidapi handle available");
        DriverResult::ErrorWritingData
    }

    /// Port of NfcProtocol::SendStopPollingRequest
    ///
    /// NOTE: Builds an NFCRequestState with NFCCommand::StopPolling and sends it via
    /// SendMCUData. Depends on the hidapi handle.
    fn send_stop_polling_request(&mut self) -> DriverResult {
        log::warn!("send_stop_polling_request: no hidapi handle available");
        DriverResult::ErrorWritingData
    }

    /// Port of NfcProtocol::GetTagUUID
    ///
    /// Returns a 7-byte UUID from the amiibo data buffer.
    /// crc byte 3 is omitted in this operation.
    fn get_tag_uuid(&self, data: &[u8]) -> TagUUID {
        if data.len() < 10 {
            return [0u8; 7];
        }
        [
            data[0], data[1], data[2], data[4], data[5], data[6], data[7],
        ]
    }
}
