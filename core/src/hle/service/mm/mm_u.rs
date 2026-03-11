// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mm/mm_u.cpp
//!
//! MM_U service ("mm:u").

/// IPC command IDs for MM_U
pub mod commands {
    pub const INITIALIZE_OLD: u32 = 0;
    pub const FINALIZE_OLD: u32 = 1;
    pub const SET_AND_WAIT_OLD: u32 = 2;
    pub const GET_OLD: u32 = 3;
    pub const INITIALIZE: u32 = 4;
    pub const FINALIZE: u32 = 5;
    pub const SET_AND_WAIT: u32 = 6;
    pub const GET: u32 = 7;
}

/// MM_U service ("mm:u").
///
/// Corresponds to `MM_U` class in upstream `mm_u.cpp`.
pub struct MmU {
    min: u32,
    max: u32,
    current: u32,
    id: u32,
}

impl MmU {
    pub fn new() -> Self {
        Self {
            min: 0,
            max: 0,
            current: 0,
            id: 1,
        }
    }

    pub fn initialize_old(&self) {
        log::warn!("(STUBBED) MmU::initialize_old called");
    }

    pub fn finalize_old(&self) {
        log::warn!("(STUBBED) MmU::finalize_old called");
    }

    pub fn set_and_wait_old(&mut self, min: u32, max: u32) {
        log::debug!("(STUBBED) MmU::set_and_wait_old called, min={:#x}, max={:#x}", min, max);
        self.min = min;
        self.max = max;
        self.current = min;
    }

    pub fn get_old(&self) -> u32 {
        log::debug!("(STUBBED) MmU::get_old called");
        self.current
    }

    pub fn initialize(&self) -> u32 {
        log::warn!("(STUBBED) MmU::initialize called");
        self.id // Any non-zero value
    }

    pub fn finalize(&self) {
        log::warn!("(STUBBED) MmU::finalize called");
    }

    pub fn set_and_wait(&mut self, _input_id: u32, min: u32, max: u32) {
        log::debug!(
            "(STUBBED) MmU::set_and_wait called, min={:#x}, max={:#x}",
            min,
            max
        );
        self.min = min;
        self.max = max;
        self.current = min;
    }

    pub fn get(&self) -> u32 {
        log::debug!("(STUBBED) MmU::get called");
        self.current
    }
}

/// Registers "mm:u" service.
///
/// Corresponds to `LoopProcess` in upstream `mm_u.cpp`.
pub fn loop_process() {
    // TODO: register "mm:u" -> MmU with ServerManager
}
