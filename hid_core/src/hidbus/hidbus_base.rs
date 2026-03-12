// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/hidbus/hidbus_base.h and hidbus_base.cpp

use common::ResultCode;

/// This is nn::hidbus::JoyPollingMode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum JoyPollingMode {
    #[default]
    SixAxisSensorDisable = 0,
    SixAxisSensorEnable = 1,
    ButtonOnly = 2,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct DataAccessorHeader {
    pub result: u32, // Result
    pub _padding: u32,
    pub unused: [u8; 0x18],
    pub latest_entry: u64,
    pub total_entries: u64,
}
const _: () = assert!(std::mem::size_of::<DataAccessorHeader>() == 0x30);

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct JoyDisableSixAxisPollingData {
    pub data: [u8; 0x26],
    pub out_size: u8,
    pub _padding: u8,
    pub sampling_number: u64,
}
const _: () = assert!(std::mem::size_of::<JoyDisableSixAxisPollingData>() == 0x30);

impl Default for JoyDisableSixAxisPollingData {
    fn default() -> Self {
        // SAFETY: All fields are plain data types, zero is valid.
        unsafe { std::mem::zeroed() }
    }
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct JoyEnableSixAxisPollingData {
    pub data: [u8; 0x8],
    pub out_size: u8,
    pub _padding: [u8; 0x7],
    pub sampling_number: u64,
}
const _: () = assert!(std::mem::size_of::<JoyEnableSixAxisPollingData>() == 0x18);

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct JoyButtonOnlyPollingData {
    pub data: [u8; 0x2c],
    pub out_size: u8,
    pub _padding: [u8; 0x3],
    pub sampling_number: u64,
}
const _: () = assert!(std::mem::size_of::<JoyButtonOnlyPollingData>() == 0x38);

impl Default for JoyButtonOnlyPollingData {
    fn default() -> Self {
        // SAFETY: All fields are plain data types, zero is valid.
        unsafe { std::mem::zeroed() }
    }
}

/// Base trait for hidbus devices
pub trait HidbusDevice {
    fn activate_device(&mut self);
    fn deactivate_device(&mut self);
    fn is_device_activated(&self) -> bool;
    fn enable(&mut self, enable: bool);
    fn is_enabled(&self) -> bool;
    fn is_polling_mode(&self) -> bool;
    fn get_polling_mode(&self) -> JoyPollingMode;
    fn set_polling_mode(&mut self, mode: JoyPollingMode);
    fn disable_polling_mode(&mut self);

    fn on_init(&mut self) {}
    fn on_release(&mut self) {}
    fn on_update(&mut self) {}
    fn get_device_id(&self) -> u8 {
        0
    }
    fn set_command(&mut self, _data: &[u8]) -> bool {
        false
    }
    fn get_reply(&self, _out_data: &mut [u8]) -> u64 {
        0
    }
}

/// Base implementation for hidbus devices
pub struct HidbusBase {
    pub is_activated: bool,
    pub device_enabled: bool,
    pub polling_mode_enabled: bool,
    pub polling_mode: JoyPollingMode,
    pub transfer_memory: u64,
}

impl HidbusBase {
    pub fn new() -> Self {
        Self {
            is_activated: false,
            device_enabled: false,
            polling_mode_enabled: false,
            polling_mode: JoyPollingMode::default(),
            transfer_memory: 0,
        }
    }

    pub fn activate_device(&mut self) {
        self.is_activated = true;
    }

    pub fn deactivate_device(&mut self) {
        self.is_activated = false;
    }

    pub fn is_device_activated(&self) -> bool {
        self.is_activated
    }

    pub fn enable(&mut self, enable: bool) {
        self.device_enabled = enable;
    }

    pub fn is_enabled(&self) -> bool {
        self.device_enabled
    }

    pub fn is_polling_mode(&self) -> bool {
        self.polling_mode_enabled
    }

    pub fn get_polling_mode(&self) -> JoyPollingMode {
        self.polling_mode
    }

    pub fn set_polling_mode(&mut self, mode: JoyPollingMode) {
        self.polling_mode = mode;
        self.polling_mode_enabled = true;
    }

    pub fn disable_polling_mode(&mut self) {
        self.polling_mode_enabled = false;
    }

    pub fn set_transfer_memory_address(&mut self, t_mem: u64) {
        self.transfer_memory = t_mem;
    }
}

impl Default for HidbusBase {
    fn default() -> Self {
        Self::new()
    }
}
