// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/hidbus/stubbed.h and stubbed.cpp

use super::hidbus_base::HidbusBase;

const DEVICE_ID: u8 = 0xFF;

pub struct HidbusStubbed {
    base: HidbusBase,
}

impl HidbusStubbed {
    pub fn new() -> Self {
        Self {
            base: HidbusBase::new(),
        }
    }

    pub fn on_init(&mut self) {
        // No initialization needed for stubbed device
    }

    pub fn on_release(&mut self) {
        // No release needed for stubbed device
    }

    pub fn on_update(&mut self) {
        if !self.base.is_activated {
            return;
        }
        if !self.base.device_enabled {
            return;
        }
        if !self.base.polling_mode_enabled || self.base.transfer_memory == 0 {
            return;
        }

        log::error!("Polling mode not supported {:?}", self.base.polling_mode);
    }

    pub fn get_device_id(&self) -> u8 {
        DEVICE_ID
    }

    pub fn set_command(&mut self, _data: &[u8]) -> bool {
        log::error!("Command not implemented");
        false
    }

    pub fn get_reply(&self, _out_data: &mut [u8]) -> u64 {
        0
    }

    pub fn base(&self) -> &HidbusBase {
        &self.base
    }

    pub fn base_mut(&mut self) -> &mut HidbusBase {
        &mut self.base
    }
}

impl Default for HidbusStubbed {
    fn default() -> Self {
        Self::new()
    }
}
