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

fn amiibo_identifier() -> PadIdentifier {
    PadIdentifier {
        guid: Default::default(),
        port: 0,
        pad: 0,
    }
}

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
        polling_mode: PollingMode,
    ) -> DriverResult {
        self.polling_mode = polling_mode;

        match polling_mode {
            PollingMode::NFC => {
                self.state = State::Initialized;
                DriverResult::Success
            }
            _ => {
                if self.state == State::TagNearby {
                    self.close_amiibo();
                }
                self.state = State::Disabled;
                DriverResult::NotSupported
            }
        }
    }

    /// Port of VirtualAmiibo::SupportsNfc (override)
    pub fn supports_nfc(&self, _identifier: &PadIdentifier) -> NfcState {
        NfcState::Success
    }

    /// Port of VirtualAmiibo::StartNfcPolling (override)
    pub fn start_nfc_polling(&mut self, _identifier: &PadIdentifier) -> NfcState {
        if self.state != State::Initialized {
            return NfcState::WrongDeviceState;
        }
        self.state = State::WaitingForAmiibo;
        NfcState::Success
    }

    /// Port of VirtualAmiibo::StopNfcPolling (override)
    pub fn stop_nfc_polling(&mut self, _identifier: &PadIdentifier) -> NfcState {
        if self.state == State::Disabled {
            return NfcState::WrongDeviceState;
        }
        if self.state == State::TagNearby {
            self.close_amiibo();
        }
        self.state = State::Initialized;
        NfcState::Success
    }

    /// Port of VirtualAmiibo::ReadAmiiboData (override)
    pub fn read_amiibo_data(
        &mut self,
        _identifier: &PadIdentifier,
        out_data: &mut Vec<u8>,
    ) -> NfcState {
        if self.state != State::TagNearby {
            return NfcState::WrongDeviceState;
        }

        if self.status.tag_type != (1 << 1) {
            return NfcState::InvalidTagType;
        }

        out_data.resize(self.nfc_data.len(), 0);
        out_data.copy_from_slice(&self.nfc_data);
        NfcState::Success
    }

    /// Port of VirtualAmiibo::WriteNfcData (override)
    pub fn write_nfc_data(&mut self, _identifier: &PadIdentifier, data: &[u8]) -> NfcState {
        // Write to file
        if self.file_path.is_empty() {
            log::error!("Amiibo file path is empty");
            return NfcState::WriteFailed;
        }

        match std::fs::write(&self.file_path, data) {
            Ok(()) => {
                self.nfc_data = data.to_vec();
                NfcState::Success
            }
            Err(e) => {
                log::error!("Error writing to file: {}", e);
                NfcState::WriteFailed
            }
        }
    }

    /// Port of VirtualAmiibo::ReadMifareData (override)
    pub fn read_mifare_data(
        &mut self,
        _identifier: &PadIdentifier,
        data: &MifareRequest,
        out_data: &mut MifareRequest,
    ) -> NfcState {
        if self.state != State::TagNearby {
            return NfcState::WrongDeviceState;
        }

        if self.status.tag_type != (1 << 6) {
            return NfcState::InvalidTagType;
        }

        for i in 0..data.data.len() {
            if data.data[i].command == 0 {
                continue;
            }
            out_data.data[i].command = data.data[i].command;
            out_data.data[i].sector = data.data[i].sector;

            let sector_index = data.data[i].sector as usize * out_data.data[i].data.len();

            if self.nfc_data.len() < sector_index + out_data.data[i].data.len() {
                return NfcState::WriteFailed;
            }

            let data_len = out_data.data[i].data.len();
            out_data.data[i].data.copy_from_slice(
                &self.nfc_data[sector_index..sector_index + data_len],
            );
        }

        NfcState::Success
    }

    /// Port of VirtualAmiibo::WriteMifareData (override)
    pub fn write_mifare_data(
        &mut self,
        _identifier: &PadIdentifier,
        data: &MifareRequest,
    ) -> NfcState {
        if self.state != State::TagNearby {
            return NfcState::WrongDeviceState;
        }

        if self.status.tag_type != (1 << 6) {
            return NfcState::WriteFailed;
        }

        for i in 0..data.data.len() {
            if data.data[i].command == 0 {
                continue;
            }

            let data_len = data.data[i].data.len();
            let sector_index = data.data[i].sector as usize * data_len;

            if self.nfc_data.len() < sector_index + data_len {
                return NfcState::WriteFailed;
            }

            self.nfc_data[sector_index..sector_index + data_len]
                .copy_from_slice(&data.data[i].data);
        }

        NfcState::Success
    }

    /// Port of VirtualAmiibo::GetCurrentState
    pub fn get_current_state(&self) -> State {
        self.state
    }

    /// Port of VirtualAmiibo::LoadAmiibo (file path version)
    pub fn load_amiibo_from_file(&mut self, amiibo_file: &str) -> Info {
        let data = match std::fs::read(amiibo_file) {
            Ok(d) => d,
            Err(_) => return Info::UnableToLoad,
        };

        let mut resized_data = match data.len() {
            AMIIBO_SIZE | AMIIBO_SIZE_WITHOUT_PASSWORD | AMIIBO_SIZE_WITH_SIGNATURE => {
                let mut buf = vec![0u8; AMIIBO_SIZE];
                let copy_len = data.len().min(AMIIBO_SIZE);
                buf[..copy_len].copy_from_slice(&data[..copy_len]);
                if copy_len < AMIIBO_SIZE_WITHOUT_PASSWORD {
                    return Info::NotAnAmiibo;
                }
                buf
            }
            MIFARE_SIZE => data,
            _ => return Info::NotAnAmiibo,
        };

        self.file_path = amiibo_file.to_string();
        self.load_amiibo_from_data(&mut resized_data)
    }

    /// Port of VirtualAmiibo::LoadAmiibo (data version)
    pub fn load_amiibo_from_data(&mut self, data: &mut [u8]) -> Info {
        if self.state != State::WaitingForAmiibo {
            return Info::WrongDeviceState;
        }

        match data.len() {
            AMIIBO_SIZE | AMIIBO_SIZE_WITHOUT_PASSWORD | AMIIBO_SIZE_WITH_SIGNATURE => {
                self.nfc_data = vec![0u8; AMIIBO_SIZE];
                let copy_len = data.len().min(AMIIBO_SIZE);
                self.nfc_data[..copy_len].copy_from_slice(&data[..copy_len]);
                self.status.tag_type = 1 << 1;
                self.status.uuid_length = 7;
            }
            MIFARE_SIZE => {
                self.nfc_data = vec![0u8; MIFARE_SIZE];
                self.nfc_data.copy_from_slice(data);
                self.status.tag_type = 1 << 6;
                self.status.uuid_length = 4;
            }
            _ => return Info::NotAnAmiibo,
        }

        self.status.uuid = [0u8; 10];
        self.status.protocol = 1;
        self.state = State::TagNearby;
        self.status.state = NfcState::NewAmiibo;

        let uuid_len = self.status.uuid_length as usize;
        self.status.uuid[..uuid_len].copy_from_slice(&self.nfc_data[..uuid_len]);
        self.engine.set_nfc(&amiibo_identifier(), &self.status);
        Info::Success
    }

    /// Port of VirtualAmiibo::ReloadAmiibo
    pub fn reload_amiibo(&mut self) -> Info {
        if self.state == State::TagNearby {
            self.engine.set_nfc(&amiibo_identifier(), &self.status);
            return Info::Success;
        }

        self.load_amiibo_from_file(&self.file_path.clone())
    }

    /// Port of VirtualAmiibo::CloseAmiibo
    pub fn close_amiibo(&mut self) -> Info {
        if self.state != State::TagNearby {
            return Info::Success;
        }

        self.state = State::WaitingForAmiibo;
        self.status.state = NfcState::AmiiboRemoved;
        self.engine.set_nfc(&amiibo_identifier(), &self.status);
        self.status.tag_type = 0;
        Info::Success
    }

    /// Port of VirtualAmiibo::GetLastFilePath
    pub fn get_last_file_path(&self) -> &str {
        &self.file_path
    }
}
