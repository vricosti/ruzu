// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/standard_steady_clock_resource.h
//! Port of zuyu/src/core/hle/service/glue/time/standard_steady_clock_resource.cpp
//!
//! StandardSteadyClockResource: manages RTC time and boot time for the steady clock.

use std::sync::Mutex;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::psc::time::common::ClockSourceId;

/// Constants matching upstream.
///
/// Max77620PmicSession and Max77620RtcSession in upstream.
#[allow(dead_code)]
const MAX77620_PMIC_SESSION: u32 = 0x3A000001;
#[allow(dead_code)]
const MAX77620_RTC_SESSION: u32 = 0x3B000001;

/// Get the current wall-clock time in seconds since epoch.
///
/// Corresponds to `GetTimeInSeconds` in upstream standard_steady_clock_resource.cpp.
fn get_time_in_seconds() -> Result<i64, ResultCode> {
    let time_s = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs() as i64;
    // TODO: Apply custom_rtc_offset from Settings if custom_rtc_enabled
    Ok(time_s)
}

/// StandardSteadyClockResource manages the RTC-derived boot time.
///
/// Corresponds to `StandardSteadyClockResource` in upstream.
pub struct StandardSteadyClockResource {
    mutex: Mutex<()>,
    clock_source_id: ClockSourceId,
    time: i64,
    set_time_result: ResultCode,
    rtc_reset: bool,
}

impl StandardSteadyClockResource {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            clock_source_id: [0u8; 16],
            time: 0,
            set_time_result: RESULT_SUCCESS,
            rtc_reset: false,
        }
    }

    /// Initialize the resource, attempting to read the RTC.
    ///
    /// Corresponds to `StandardSteadyClockResource::Initialize` in upstream.
    pub fn initialize(
        &mut self,
        out_source_id: Option<&mut ClockSourceId>,
        external_source_id: &ClockSourceId,
    ) {
        const NUM_TRIES: usize = 20;

        let mut succeeded = false;
        let mut last_result = RESULT_SUCCESS;

        for _ in 0..NUM_TRIES {
            last_result = self.set_current_time();
            if last_result.is_success() {
                succeeded = true;
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(1));
        }

        if succeeded {
            self.set_time_result = RESULT_SUCCESS;
            let empty_id: ClockSourceId = [0u8; 16];
            if *external_source_id != empty_id {
                self.clock_source_id = *external_source_id;
            } else {
                // Generate a random UUID
                self.clock_source_id = rand_clock_source_id();
            }
        } else {
            self.set_time_result = last_result;
            // Use a negative boot-time offset
            self.time = 0; // Simplified -- upstream uses CoreTiming ticks
            self.clock_source_id = rand_clock_source_id();
        }

        if let Some(out) = out_source_id {
            *out = self.clock_source_id;
        }
    }

    /// Get the current boot-time offset.
    ///
    /// Corresponds to `StandardSteadyClockResource::GetTime` in upstream.
    pub fn get_time(&self) -> i64 {
        self.time
    }

    /// Check if an RTC reset was detected.
    ///
    /// Corresponds to `StandardSteadyClockResource::GetResetDetected` in upstream.
    pub fn get_reset_detected(&mut self) -> bool {
        // TODO: Call Rtc::GetRtcResetDetected, clear if detected
        // For now, always report no reset
        self.rtc_reset = false;
        self.rtc_reset
    }

    /// Set the current boot time from RTC.
    ///
    /// Corresponds to `StandardSteadyClockResource::SetCurrentTime` in upstream.
    pub fn set_current_time(&mut self) -> ResultCode {
        let rtc_time_s = match get_time_in_seconds() {
            Ok(t) => t,
            Err(e) => return e,
        };

        // Compute boot time: rtc_time_ns - elapsed_ns
        // Simplified: we don't have CoreTiming ticks, so use wall clock directly
        let one_second_ns: i64 = 1_000_000_000;
        let boot_time = rtc_time_s.saturating_mul(one_second_ns);

        let _lock = self.mutex.lock().unwrap();
        self.time = boot_time;
        RESULT_SUCCESS
    }

    /// Get the RTC time in seconds.
    ///
    /// Corresponds to `StandardSteadyClockResource::GetRtcTimeInSeconds` in upstream.
    pub fn get_rtc_time_in_seconds(&self) -> Result<i64, ResultCode> {
        get_time_in_seconds()
    }

    /// Update the boot time (called periodically).
    ///
    /// Corresponds to `StandardSteadyClockResource::UpdateTime` in upstream.
    pub fn update_time(&mut self) {
        const NUM_TRIES: usize = 3;

        for _ in 0..NUM_TRIES {
            let res = self.set_current_time();
            if res.is_success() {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
    }
}

/// Generate a random clock source ID (UUID).
fn rand_clock_source_id() -> ClockSourceId {
    let mut id = [0u8; 16];
    // Simple pseudo-random: use system time as seed
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let seed = now.as_nanos();
    for (i, byte) in id.iter_mut().enumerate() {
        *byte = ((seed >> (i * 8)) & 0xFF) as u8;
    }
    // Set UUID version 4 bits
    id[6] = (id[6] & 0x0F) | 0x40;
    id[8] = (id[8] & 0x3F) | 0x80;
    id
}
