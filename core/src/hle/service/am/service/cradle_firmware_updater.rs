// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/cradle_firmware_updater.h
//! Port of zuyu/src/core/hle/service/am/service/cradle_firmware_updater.cpp

/// Port of CradleDeviceInfo
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct CradleDeviceInfo {
    pub unknown0: bool,
    pub unknown1: bool,
    pub unknown2: bool,
    _pad: [u8; 5],
    pub unknown3: u64,
}
const _: () = assert!(std::mem::size_of::<CradleDeviceInfo>() == 0x10);

/// IPC command table for ICradleFirmwareUpdater:
/// - 0: StartUpdate
/// - 1: FinishUpdate
/// - 2: GetCradleDeviceInfo
/// - 3: GetCradleDeviceInfoChangeEvent
/// - 4: GetUpdateProgressInfo (unimplemented)
/// - 5: GetLastInternalResult (unimplemented)
pub struct ICradleFirmwareUpdater {
    // TODO: ServiceContext, Event
}

impl ICradleFirmwareUpdater {
    pub fn new() -> Self {
        Self {}
    }

    /// Port of ICradleFirmwareUpdater::StartUpdate
    pub fn start_update(&self) {
        log::warn!("(STUBBED) StartUpdate called");
    }

    /// Port of ICradleFirmwareUpdater::FinishUpdate
    pub fn finish_update(&self) {
        log::warn!("(STUBBED) FinishUpdate called");
    }

    /// Port of ICradleFirmwareUpdater::GetCradleDeviceInfo
    pub fn get_cradle_device_info(&self) -> CradleDeviceInfo {
        log::warn!("(STUBBED) GetCradleDeviceInfo called");
        CradleDeviceInfo::default()
    }
}
