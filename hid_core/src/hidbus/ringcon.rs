// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/hidbus/ringcon.h and ringcon.cpp

use super::hidbus_base::HidbusBase;

const IDLE_VALUE: i16 = 2280;
const IDLE_DEADZONE: i16 = 120;
const RANGE: i16 = 2500;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
enum RingConCommands {
    GetFirmwareVersion = 0x00020000,
    ReadId = 0x00020100,
    JoyPolling = 0x00020101,
    Unknown1 = 0x00020104,
    C020105 = 0x00020105,
    Unknown2 = 0x00020204,
    Unknown3 = 0x00020304,
    Unknown4 = 0x00020404,
    ReadUnkCal = 0x00020504,
    ReadFactoryCal = 0x00020A04,
    Unknown5 = 0x00021104,
    Unknown6 = 0x00021204,
    Unknown7 = 0x00021304,
    ReadUserCal = 0x00021A04,
    ReadRepCount = 0x00023104,
    ReadTotalPushCount = 0x00023204,
    ResetRepCount = 0x04013104,
    Unknown8 = 0x04011104,
    Unknown9 = 0x04011204,
    Unknown10 = 0x04011304,
    SaveCalData = 0x10011A04,
    Error = 0xFFFFFFFF,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
enum DataValid {
    Valid = 0,
    BadCRC = 1,
    Cal = 2,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct RingConFirmwareVersion {
    sub: u8,
    main: u8,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct FactoryCalibration {
    os_max: i32,
    hk_max: i32,
    zero_min: i32,
    zero_max: i32,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct CalibrationValue {
    value: i16,
    crc: u16,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct UserCalibration {
    os_max: CalibrationValue,
    hk_max: CalibrationValue,
    zero: CalibrationValue,
}

pub struct RingController {
    base: HidbusBase,
    command: u32,
    total_rep_count: u8,
    total_push_count: u8,
    device_id: u8,
    version: RingConFirmwareVersion,
    factory_calibration: FactoryCalibration,
    user_calibration: UserCalibration,
}

impl RingController {
    pub fn new() -> Self {
        Self {
            base: HidbusBase::new(),
            command: RingConCommands::Error as u32,
            total_rep_count: 0,
            total_push_count: 0,
            device_id: 0x20,
            version: RingConFirmwareVersion {
                sub: 0x0,
                main: 0x2c,
            },
            factory_calibration: FactoryCalibration {
                os_max: (IDLE_VALUE + RANGE + IDLE_DEADZONE) as i32,
                hk_max: (IDLE_VALUE - RANGE - IDLE_DEADZONE) as i32,
                zero_min: (IDLE_VALUE - IDLE_DEADZONE) as i32,
                zero_max: (IDLE_VALUE + IDLE_DEADZONE) as i32,
            },
            user_calibration: UserCalibration {
                os_max: CalibrationValue {
                    value: RANGE,
                    crc: 228,
                },
                hk_max: CalibrationValue {
                    value: -RANGE,
                    crc: 239,
                },
                zero: CalibrationValue {
                    value: IDLE_VALUE,
                    crc: 225,
                },
            },
        }
    }

    pub fn on_init(&mut self) {
        // TODO
    }

    pub fn on_release(&mut self) {
        // TODO
    }

    pub fn on_update(&mut self) {
        todo!()
    }

    pub fn get_device_id(&self) -> u8 {
        self.device_id
    }

    pub fn set_command(&mut self, _data: &[u8]) -> bool {
        todo!()
    }

    pub fn get_reply(&self, _out_data: &mut [u8]) -> u64 {
        todo!()
    }
}

impl Default for RingController {
    fn default() -> Self {
        Self::new()
    }
}
