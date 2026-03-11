// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pcv/pcv.h
//! Port of zuyu/src/core/hle/service/pcv/pcv.cpp
//!
//! PCV, CLKRST, and CLKRST_A services.

/// DeviceCode enum. Upstream: `DeviceCode` in `pcv.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum DeviceCode {
    Cpu = 0x40000001,
    Gpu = 0x40000002,
    I2s1 = 0x40000003,
    I2s2 = 0x40000004,
    I2s3 = 0x40000005,
    Pwm = 0x40000006,
    I2c1 = 0x02000001,
    I2c2 = 0x02000002,
    I2c3 = 0x02000003,
    I2c4 = 0x02000004,
    I2c5 = 0x02000005,
    I2c6 = 0x02000006,
    Spi1 = 0x07000000,
    Spi2 = 0x07000001,
    Spi3 = 0x07000002,
    Spi4 = 0x07000003,
}

/// IPC command IDs for CLKRST
pub mod clkrst_commands {
    pub const OPEN_SESSION: u32 = 0;
    pub const GET_TEMPERATURE_THRESHOLDS: u32 = 1;
    pub const SET_TEMPERATURE: u32 = 2;
}

/// IPC command IDs for IClkrstSession
pub mod clkrst_session_commands {
    pub const SET_CLOCK_RATE: u32 = 7;
    pub const GET_CLOCK_RATE: u32 = 8;
}

/// PCV service ("pcv"). All commands are unimplemented stubs.
pub struct PCV;
impl PCV {
    pub fn new() -> Self { Self }
}

/// IClkrstSession.
pub struct IClkrstSession {
    device_code: u32,
    clock_rate: u32,
}

impl IClkrstSession {
    pub fn new(device_code: u32) -> Self {
        Self {
            device_code,
            clock_rate: 0,
        }
    }

    pub fn set_clock_rate(&mut self, clock_rate: u32) {
        log::debug!("(STUBBED) IClkrstSession::set_clock_rate called, clock_rate={}", clock_rate);
        self.clock_rate = clock_rate;
    }

    pub fn get_clock_rate(&self) -> u32 {
        log::debug!("(STUBBED) IClkrstSession::get_clock_rate called");
        self.clock_rate
    }
}

/// CLKRST service ("clkrst", "clkrst:i").
pub struct CLKRST {
    name: String,
}

impl CLKRST {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }

    pub fn open_session(&self, device_code: u32, _unknown_input: u32) -> IClkrstSession {
        log::debug!(
            "CLKRST({})::open_session called, device_code={:#x}",
            self.name,
            device_code
        );
        IClkrstSession::new(device_code)
    }
}

/// CLKRST_A service ("clkrst:a"). All stubs.
pub struct ClkrstA;
impl ClkrstA {
    pub fn new() -> Self { Self }
}

/// Registers "pcv", "clkrst", "clkrst:i", "clkrst:a" services.
///
/// Corresponds to `LoopProcess` in upstream `pcv.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
}
