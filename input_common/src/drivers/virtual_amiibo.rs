// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `input_common/drivers/virtual_amiibo.h` and `input_common/drivers/virtual_amiibo.cpp`.
//!
//! Virtual amiibo input driver for loading and managing NFC tag data.

use std::sync::{Arc, Weak};

use common::input::{DriverResult, MifareRequest, NfcState, NfcStatus, PollingMode};
use parking_lot::Mutex;

use crate::input_engine::{InputEngine, InputEngineOutput, PadIdentifier};

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
#[derive(Clone)]
pub struct VirtualAmiibo {
    engine: Arc<Mutex<InputEngine>>,
    data: Arc<Mutex<VirtualAmiiboData>>,
}

struct VirtualAmiiboData {
    file_path: String,
    state: State,
    nfc_data: Vec<u8>,
    status: NfcStatus,
    polling_mode: PollingMode,
}

struct VirtualAmiiboOutput {
    engine: Weak<Mutex<InputEngine>>,
    data: Weak<Mutex<VirtualAmiiboData>>,
}

impl VirtualAmiiboOutput {
    fn driver(&self) -> Option<VirtualAmiibo> {
        Some(VirtualAmiibo {
            engine: self.engine.upgrade()?,
            data: self.data.upgrade()?,
        })
    }
}

impl InputEngineOutput for VirtualAmiiboOutput {
    fn set_polling_mode(&self, identifier: &PadIdentifier, mode: PollingMode) -> DriverResult {
        self.driver()
            .map_or(DriverResult::NotSupported, |mut driver| {
                driver.set_polling_mode(identifier, mode)
            })
    }

    fn supports_nfc(&self, identifier: &PadIdentifier) -> NfcState {
        self.driver().map_or(NfcState::NotSupported, |driver| {
            driver.supports_nfc(identifier)
        })
    }

    fn start_nfc_polling(&self, identifier: &PadIdentifier) -> NfcState {
        self.driver().map_or(NfcState::NotSupported, |mut driver| {
            driver.start_nfc_polling(identifier)
        })
    }

    fn stop_nfc_polling(&self, identifier: &PadIdentifier) -> NfcState {
        self.driver().map_or(NfcState::NotSupported, |mut driver| {
            driver.stop_nfc_polling(identifier)
        })
    }

    fn read_amiibo_data(&self, identifier: &PadIdentifier, out_data: &mut Vec<u8>) -> NfcState {
        self.driver().map_or(NfcState::NotSupported, |mut driver| {
            driver.read_amiibo_data(identifier, out_data)
        })
    }

    fn write_nfc_data(&self, identifier: &PadIdentifier, data: &[u8]) -> NfcState {
        self.driver().map_or(NfcState::NotSupported, |mut driver| {
            driver.write_nfc_data(identifier, data)
        })
    }

    fn read_mifare_data(
        &self,
        identifier: &PadIdentifier,
        request: &MifareRequest,
        out_data: &mut MifareRequest,
    ) -> NfcState {
        self.driver().map_or(NfcState::NotSupported, |mut driver| {
            driver.read_mifare_data(identifier, request, out_data)
        })
    }

    fn write_mifare_data(&self, identifier: &PadIdentifier, request: &MifareRequest) -> NfcState {
        self.driver().map_or(NfcState::NotSupported, |mut driver| {
            driver.write_mifare_data(identifier, request)
        })
    }
}

impl VirtualAmiibo {
    /// Port of VirtualAmiibo::VirtualAmiibo
    pub fn new(input_engine: String) -> Self {
        let engine = Arc::new(Mutex::new(InputEngine::new(input_engine)));
        let data = Arc::new(Mutex::new(VirtualAmiiboData {
            file_path: String::new(),
            state: State::Disabled,
            nfc_data: Vec::new(),
            status: NfcStatus::default(),
            polling_mode: PollingMode::Passive,
        }));
        engine
            .lock()
            .set_output_handler(Arc::new(VirtualAmiiboOutput {
                engine: Arc::downgrade(&engine),
                data: Arc::downgrade(&data),
            }));
        Self { engine, data }
    }

    /// Returns the shared underlying input engine.
    pub fn engine(&self) -> Arc<Mutex<InputEngine>> {
        Arc::clone(&self.engine)
    }

    /// Port of VirtualAmiibo::SetPollingMode (override)
    pub fn set_polling_mode(
        &mut self,
        _identifier: &PadIdentifier,
        polling_mode: PollingMode,
    ) -> DriverResult {
        self.data.lock().polling_mode = polling_mode;

        match polling_mode {
            PollingMode::NFC => {
                self.data.lock().state = State::Initialized;
                DriverResult::Success
            }
            _ => {
                if self.data.lock().state == State::TagNearby {
                    self.close_amiibo();
                }
                self.data.lock().state = State::Disabled;
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
        let mut data = self.data.lock();
        if data.state != State::Initialized {
            return NfcState::WrongDeviceState;
        }
        data.state = State::WaitingForAmiibo;
        NfcState::Success
    }

    /// Port of VirtualAmiibo::StopNfcPolling (override)
    pub fn stop_nfc_polling(&mut self, _identifier: &PadIdentifier) -> NfcState {
        let state = self.data.lock().state;
        if state == State::Disabled {
            return NfcState::WrongDeviceState;
        }
        if state == State::TagNearby {
            self.close_amiibo();
        }
        self.data.lock().state = State::Initialized;
        NfcState::Success
    }

    /// Port of VirtualAmiibo::ReadAmiiboData (override)
    pub fn read_amiibo_data(
        &mut self,
        _identifier: &PadIdentifier,
        out_data: &mut Vec<u8>,
    ) -> NfcState {
        let data = self.data.lock();
        if data.state != State::TagNearby {
            return NfcState::WrongDeviceState;
        }

        if data.status.tag_type != (1 << 1) {
            return NfcState::InvalidTagType;
        }

        out_data.resize(data.nfc_data.len(), 0);
        out_data.copy_from_slice(&data.nfc_data);
        NfcState::Success
    }

    /// Port of VirtualAmiibo::WriteNfcData (override)
    pub fn write_nfc_data(&mut self, _identifier: &PadIdentifier, data: &[u8]) -> NfcState {
        let file_path = self.data.lock().file_path.clone();
        if file_path.is_empty() {
            log::error!("Amiibo file path is empty");
            return NfcState::WriteFailed;
        }

        match std::fs::write(&file_path, data) {
            Ok(()) => {
                self.data.lock().nfc_data = data.to_vec();
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
        let shared = self.data.lock();
        if shared.state != State::TagNearby {
            return NfcState::WrongDeviceState;
        }

        if shared.status.tag_type != (1 << 6) {
            return NfcState::InvalidTagType;
        }

        for i in 0..data.data.len() {
            if data.data[i].command == 0 {
                continue;
            }
            out_data.data[i].command = data.data[i].command;
            out_data.data[i].sector = data.data[i].sector;

            let sector_index = data.data[i].sector as usize * out_data.data[i].data.len();

            if shared.nfc_data.len() < sector_index + out_data.data[i].data.len() {
                return NfcState::WriteFailed;
            }

            let data_len = out_data.data[i].data.len();
            out_data.data[i]
                .data
                .copy_from_slice(&shared.nfc_data[sector_index..sector_index + data_len]);
        }

        NfcState::Success
    }

    /// Port of VirtualAmiibo::WriteMifareData (override)
    pub fn write_mifare_data(
        &mut self,
        _identifier: &PadIdentifier,
        data: &MifareRequest,
    ) -> NfcState {
        let mut shared = self.data.lock();
        if shared.state != State::TagNearby {
            return NfcState::WrongDeviceState;
        }

        if shared.status.tag_type != (1 << 6) {
            return NfcState::WriteFailed;
        }

        for i in 0..data.data.len() {
            if data.data[i].command == 0 {
                continue;
            }

            let data_len = data.data[i].data.len();
            let sector_index = data.data[i].sector as usize * data_len;

            if shared.nfc_data.len() < sector_index + data_len {
                return NfcState::WriteFailed;
            }

            shared.nfc_data[sector_index..sector_index + data_len]
                .copy_from_slice(&data.data[i].data);
        }

        NfcState::Success
    }

    /// Port of VirtualAmiibo::GetCurrentState
    pub fn get_current_state(&self) -> State {
        self.data.lock().state
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

        self.data.lock().file_path = amiibo_file.to_string();
        self.load_amiibo_from_data(&mut resized_data)
    }

    /// Port of VirtualAmiibo::LoadAmiibo (data version)
    pub fn load_amiibo_from_data(&mut self, data: &mut [u8]) -> Info {
        let mut shared = self.data.lock();
        if shared.state != State::WaitingForAmiibo {
            return Info::WrongDeviceState;
        }

        match data.len() {
            AMIIBO_SIZE | AMIIBO_SIZE_WITHOUT_PASSWORD | AMIIBO_SIZE_WITH_SIGNATURE => {
                shared.nfc_data = vec![0u8; AMIIBO_SIZE];
                let copy_len = data.len().min(AMIIBO_SIZE);
                shared.nfc_data[..copy_len].copy_from_slice(&data[..copy_len]);
                shared.status.tag_type = 1 << 1;
                shared.status.uuid_length = 7;
            }
            MIFARE_SIZE => {
                shared.nfc_data = vec![0u8; MIFARE_SIZE];
                shared.nfc_data.copy_from_slice(data);
                shared.status.tag_type = 1 << 6;
                shared.status.uuid_length = 4;
            }
            _ => return Info::NotAnAmiibo,
        }

        shared.status.uuid = [0u8; 10];
        shared.status.protocol = 1;
        shared.state = State::TagNearby;
        shared.status.state = NfcState::NewAmiibo;

        let uuid_len = shared.status.uuid_length as usize;
        let uuid = shared.nfc_data[..uuid_len].to_vec();
        shared.status.uuid[..uuid_len].copy_from_slice(&uuid);
        let status = shared.status.clone();
        drop(shared);
        let callbacks = self.engine.lock().set_nfc(&amiibo_identifier(), &status);
        callbacks.dispatch();
        Info::Success
    }

    /// Port of VirtualAmiibo::ReloadAmiibo
    pub fn reload_amiibo(&mut self) -> Info {
        let (state, status, file_path) = {
            let data = self.data.lock();
            (data.state, data.status.clone(), data.file_path.clone())
        };
        if state == State::TagNearby {
            let callbacks = self.engine.lock().set_nfc(&amiibo_identifier(), &status);
            callbacks.dispatch();
            return Info::Success;
        }

        self.load_amiibo_from_file(&file_path)
    }

    /// Port of VirtualAmiibo::CloseAmiibo
    pub fn close_amiibo(&mut self) -> Info {
        let status = {
            let mut data = self.data.lock();
            if data.state != State::TagNearby {
                return Info::Success;
            }

            data.state = State::WaitingForAmiibo;
            data.status.state = NfcState::AmiiboRemoved;
            let status = data.status.clone();
            data.status.tag_type = 0;
            status
        };
        let callbacks = self.engine.lock().set_nfc(&amiibo_identifier(), &status);
        callbacks.dispatch();
        Info::Success
    }

    /// Port of VirtualAmiibo::GetLastFilePath
    pub fn get_last_file_path(&self) -> String {
        self.data.lock().file_path.clone()
    }
}
