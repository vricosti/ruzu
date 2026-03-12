// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `input_common/drivers/virtual_amiibo.h` and `input_common/drivers/virtual_amiibo.cpp`.
//!
//! Virtual amiibo input driver for loading and managing NFC tag data.

use common::input::{DriverResult, MifareRequest, NfcState, NfcStatus, PollingMode};

use crate::input_engine::{InputEngine, PadIdentifier};

/// Port of VirtualAmiibo::State enum from virtual_amiibo.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum State {
    Disabled,
    Initialized,
    WaitingForAmiibo,
    TagNearby,
}

/// Port of VirtualAmiibo::Info enum from virtual_amiibo.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Info {
    Success,
    UnableToLoad,
    NotAnAmiibo,
    WrongDeviceState,
    Unknown,
}

/// Size constants from virtual_amiibo.h
const AMIIBO_SIZE: usize = 0x21C;
const AMIIBO_SIZE_WITHOUT_PASSWORD: usize = AMIIBO_SIZE - 0x8;
const AMIIBO_SIZE_WITH_SIGNATURE: usize = AMIIBO_SIZE + 0x20;
const MIFARE_SIZE: usize = 0x400;

/// Port of `VirtualAmiibo` class from virtual_amiibo.h / virtual_amiibo.cpp
pub struct VirtualAmiibo {
    engine: InputEngine,
    file_path: String,
    state: State,
    nfc_data: Vec<u8>,
    status: NfcStatus,
    polling_mode: PollingMode,
}

impl VirtualAmiibo {
    /// Port of VirtualAmiibo::VirtualAmiibo
    pub fn new(input_engine: String) -> Self {
        Self {
            engine: InputEngine::new(input_engine),
            file_path: String::new(),
            state: State::Disabled,
            nfc_data: Vec::new(),
            status: NfcStatus::default(),
            polling_mode: PollingMode::Passive,
        }
    }

    /// Returns a reference to the underlying input engine.
    pub fn engine(&self) -> &InputEngine {
        &self.engine
    }

    /// Returns a mutable reference to the underlying input engine.
    pub fn engine_mut(&mut self) -> &mut InputEngine {
        &mut self.engine
    }

    /// Port of VirtualAmiibo::SetPollingMode (override)
    pub fn set_polling_mode(
        &mut self,
        _identifier: &PadIdentifier,
        _polling_mode: PollingMode,
    ) -> DriverResult {
        todo!()
    }

    /// Port of VirtualAmiibo::SupportsNfc (override)
    pub fn supports_nfc(&self, _identifier: &PadIdentifier) -> NfcState {
        todo!()
    }

    /// Port of VirtualAmiibo::StartNfcPolling (override)
    pub fn start_nfc_polling(&mut self, _identifier: &PadIdentifier) -> NfcState {
        todo!()
    }

    /// Port of VirtualAmiibo::StopNfcPolling (override)
    pub fn stop_nfc_polling(&mut self, _identifier: &PadIdentifier) -> NfcState {
        todo!()
    }

    /// Port of VirtualAmiibo::ReadAmiiboData (override)
    pub fn read_amiibo_data(
        &mut self,
        _identifier: &PadIdentifier,
        _out_data: &mut Vec<u8>,
    ) -> NfcState {
        todo!()
    }

    /// Port of VirtualAmiibo::WriteNfcData (override)
    pub fn write_nfc_data(&mut self, _identifier: &PadIdentifier, _data: &[u8]) -> NfcState {
        todo!()
    }

    /// Port of VirtualAmiibo::ReadMifareData (override)
    pub fn read_mifare_data(
        &mut self,
        _identifier: &PadIdentifier,
        _data: &MifareRequest,
        _out_data: &mut MifareRequest,
    ) -> NfcState {
        todo!()
    }

    /// Port of VirtualAmiibo::WriteMifareData (override)
    pub fn write_mifare_data(
        &mut self,
        _identifier: &PadIdentifier,
        _data: &MifareRequest,
    ) -> NfcState {
        todo!()
    }

    /// Port of VirtualAmiibo::GetCurrentState
    pub fn get_current_state(&self) -> State {
        self.state
    }

    /// Port of VirtualAmiibo::LoadAmiibo (file path version)
    pub fn load_amiibo_from_file(&mut self, _amiibo_file: &str) -> Info {
        todo!()
    }

    /// Port of VirtualAmiibo::LoadAmiibo (data version)
    pub fn load_amiibo_from_data(&mut self, _data: &mut [u8]) -> Info {
        todo!()
    }

    /// Port of VirtualAmiibo::ReloadAmiibo
    pub fn reload_amiibo(&mut self) -> Info {
        todo!()
    }

    /// Port of VirtualAmiibo::CloseAmiibo
    pub fn close_amiibo(&mut self) -> Info {
        todo!()
    }

    /// Port of VirtualAmiibo::GetLastFilePath
    pub fn get_last_file_path(&self) -> &str {
        &self.file_path
    }
}
