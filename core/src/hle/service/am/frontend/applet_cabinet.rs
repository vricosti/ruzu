// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_cabinet.h
//! Port of zuyu/src/core/hle/service/am/frontend/applet_cabinet.cpp

/// Port of CabinetAppletVersion
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CabinetAppletVersion {
    Version1 = 0x1,
}

/// Port of CabinetFlags
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CabinetFlags {
    None = 0,
    DeviceHandle = 1,
    TagInfo = 2,
    RegisterInfo = 4,
}

/// Port of CabinetResult
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CabinetResult {
    Cancel = 0,
    TagInfo = 2,
    RegisterInfo = 4,
}

/// Port of AmiiboSettingsStartParam (nn::nfp::AmiiboSettingsStartParam)
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct AmiiboSettingsStartParam {
    pub device_handle: u64,
    pub param_1: [u8; 0x20],
    pub param_2: u8,
    _padding: [u8; 7],
}
const _: () = assert!(std::mem::size_of::<AmiiboSettingsStartParam>() == 0x30);

impl Default for AmiiboSettingsStartParam {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// Cabinet frontend applet stub.
pub struct Cabinet {
    is_complete: bool,
}

impl Cabinet {
    pub fn new() -> Self {
        Self { is_complete: false }
    }
}
