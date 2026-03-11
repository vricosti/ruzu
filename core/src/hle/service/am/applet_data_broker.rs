// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/applet_data_broker.h
//! Port of zuyu/src/core/hle/service/am/applet_data_broker.cpp

use std::collections::VecDeque;
use std::sync::Mutex;

use super::am_results;
use crate::hle::result::ResultCode;

/// Port of AppletStorageChannel
pub struct AppletStorageChannel {
    lock: Mutex<()>,
    data: Mutex<VecDeque<Vec<u8>>>,
    signaled: Mutex<bool>,
}

impl AppletStorageChannel {
    pub fn new() -> Self {
        Self {
            lock: Mutex::new(()),
            data: Mutex::new(VecDeque::new()),
            signaled: Mutex::new(false),
        }
    }

    pub fn push(&self, storage_data: Vec<u8>) {
        let mut data = self.data.lock().unwrap();
        data.push_back(storage_data);
        *self.signaled.lock().unwrap() = true;
    }

    pub fn pop(&self) -> Result<Vec<u8>, ResultCode> {
        let mut data = self.data.lock().unwrap();

        if data.is_empty() {
            *self.signaled.lock().unwrap() = false;
            return Err(am_results::RESULT_NO_DATA_IN_CHANNEL);
        }

        let result = data.pop_front().unwrap();

        if data.is_empty() {
            *self.signaled.lock().unwrap() = false;
        }

        Ok(result)
    }
}

/// Port of AppletDataBroker
pub struct AppletDataBroker {
    in_data: AppletStorageChannel,
    interactive_in_data: AppletStorageChannel,
    out_data: AppletStorageChannel,
    interactive_out_data: AppletStorageChannel,
}

impl AppletDataBroker {
    pub fn new() -> Self {
        Self {
            in_data: AppletStorageChannel::new(),
            interactive_in_data: AppletStorageChannel::new(),
            out_data: AppletStorageChannel::new(),
            interactive_out_data: AppletStorageChannel::new(),
        }
    }

    pub fn get_in_data(&self) -> &AppletStorageChannel {
        &self.in_data
    }

    pub fn get_interactive_in_data(&self) -> &AppletStorageChannel {
        &self.interactive_in_data
    }

    pub fn get_out_data(&self) -> &AppletStorageChannel {
        &self.out_data
    }

    pub fn get_interactive_out_data(&self) -> &AppletStorageChannel {
        &self.interactive_out_data
    }
}
