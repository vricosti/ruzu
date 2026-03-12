// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/hidbus/stubbed.h and stubbed.cpp

use super::hidbus_base::HidbusBase;

pub struct HidbusStubbed {
    base: HidbusBase,
}

impl HidbusStubbed {
    pub fn new() -> Self {
        Self {
            base: HidbusBase::new(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    pub fn on_update(&mut self) {
        todo!()
    }

    pub fn get_device_id(&self) -> u8 {
        0
    }

    pub fn set_command(&mut self, _data: &[u8]) -> bool {
        false
    }

    pub fn get_reply(&self, _out_data: &mut [u8]) -> u64 {
        0
    }
}

impl Default for HidbusStubbed {
    fn default() -> Self {
        Self::new()
    }
}
