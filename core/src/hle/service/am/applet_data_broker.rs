// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/applet_data_broker.h
//! Port of zuyu/src/core/hle/service/am/applet_data_broker.cpp

use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use super::am_results;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::HLERequestContext;
use crate::hle::service::os::event::Event;

/// Port of AppletStorageChannel
pub struct AppletStorageChannel {
    data: Mutex<VecDeque<Vec<u8>>>,
    event: Arc<Event>,
}

impl AppletStorageChannel {
    pub fn new() -> Self {
        Self {
            data: Mutex::new(VecDeque::new()),
            event: Arc::new(Event::new()),
        }
    }

    pub fn push(&self, storage_data: Vec<u8>) {
        let mut data = self.data.lock().unwrap();
        data.push_back(storage_data);
        self.event.signal();
    }

    pub fn pop(&self) -> Result<Vec<u8>, ResultCode> {
        let mut data = self.data.lock().unwrap();

        if data.is_empty() {
            self.event.clear();
            return Err(am_results::RESULT_NO_DATA_IN_CHANNEL);
        }

        let result = data.pop_front().unwrap();

        if data.is_empty() {
            self.event.clear();
        }

        Ok(result)
    }

    /// Port of `AppletStorageChannel::GetEvent()` followed by IPC copy-handle
    /// translation in `ILibraryAppletAccessor`.
    pub fn get_event_object_id(&self, ctx: &HLERequestContext) -> Option<u64> {
        self.event.copy_object_id(ctx)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn channel_event_tracks_queue_occupancy() {
        let channel = AppletStorageChannel::new();
        assert!(!channel.event.is_signaled());

        channel.push(vec![1]);
        channel.push(vec![2]);
        assert!(channel.event.is_signaled());

        assert_eq!(channel.pop().unwrap(), vec![1]);
        assert!(channel.event.is_signaled());
        assert_eq!(channel.pop().unwrap(), vec![2]);
        assert!(!channel.event.is_signaled());
    }
}
