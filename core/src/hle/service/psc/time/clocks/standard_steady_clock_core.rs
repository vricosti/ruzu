// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/clocks/standard_steady_clock_core.h/.cpp
//!
//! StandardSteadyClockCore: the primary steady clock with RTC offset and continuous adjustment.

use std::sync::Mutex;

use super::steady_clock_core::{SteadyClockCoreImpl, SteadyClockCoreState};
use crate::hle::result::ResultCode;
use crate::hle::service::psc::time::common::{
    ClockSourceId, ContinuousAdjustmentTimePoint, SteadyClockTimePoint,
};
use crate::hle::service::psc::time::errors::RESULT_NOT_IMPLEMENTED;

pub struct StandardSteadyClockCore {
    pub state: SteadyClockCoreState,
    mutex: Mutex<()>,
    test_offset: i64,
    internal_offset: i64,
    clock_source_id: ClockSourceId,
    rtc_offset: i64,
    cached_time_point: i64,
    continuous_adjustment_time_point: ContinuousAdjustmentTimePoint,
    /// Callback to get clock ticks in nanoseconds from CoreTiming.
    get_ticks_ns: Box<dyn Fn() -> i64 + Send + Sync>,
}

impl StandardSteadyClockCore {
    pub fn new(get_ticks_ns: Box<dyn Fn() -> i64 + Send + Sync>) -> Self {
        Self {
            state: SteadyClockCoreState::new(),
            mutex: Mutex::new(()),
            test_offset: 0,
            internal_offset: 0,
            clock_source_id: [0u8; 16],
            rtc_offset: 0,
            cached_time_point: 0,
            continuous_adjustment_time_point: ContinuousAdjustmentTimePoint::default(),
            get_ticks_ns,
        }
    }

    pub fn new_default() -> Self {
        Self::new(Box::new(|| 0))
    }

    /// Initialize the clock (matching upstream Initialize).
    pub fn initialize(
        &mut self,
        clock_source_id: ClockSourceId,
        rtc_offset: i64,
        internal_offset: i64,
        test_offset: i64,
        is_rtc_reset_detected: bool,
    ) {
        self.clock_source_id = clock_source_id;
        self.rtc_offset = rtc_offset;
        self.internal_offset = internal_offset;
        self.test_offset = test_offset;
        if is_rtc_reset_detected {
            self.state.set_reset_detected();
        }
        self.state.set_initialized();
    }

    pub fn set_rtc_offset(&mut self, offset: i64) {
        self.rtc_offset = offset;
    }

    pub fn set_continuous_adjustment(&mut self, clock_source_id: ClockSourceId, time: i64) {
        let ticks_ns = (self.get_ticks_ns)();
        self.continuous_adjustment_time_point.rtc_offset = ticks_ns;
        self.continuous_adjustment_time_point.diff_scale = 0;
        self.continuous_adjustment_time_point.shift_amount = 0;
        self.continuous_adjustment_time_point.lower = time;
        self.continuous_adjustment_time_point.upper = time;
        self.continuous_adjustment_time_point.clock_source_id = clock_source_id;
    }

    pub fn get_continuous_adjustment(&self) -> ContinuousAdjustmentTimePoint {
        self.continuous_adjustment_time_point
    }

    pub fn update_continuous_adjustment_time(&mut self, in_time: i64) {
        let uptime_ns = (self.get_ticks_ns)();
        let adj = &self.continuous_adjustment_time_point;
        let adjusted_time = if adj.shift_amount != 0 {
            ((uptime_ns - adj.rtc_offset) * adj.diff_scale) >> adj.shift_amount
        } else {
            (uptime_ns - adj.rtc_offset) * adj.diff_scale
        };
        let expected_time = adjusted_time + adj.lower;

        let last_time_point = adj.upper;
        self.continuous_adjustment_time_point.upper = in_time;

        let t1 = expected_time.min(last_time_point);
        let expected_time_max = expected_time.max(last_time_point);
        let expected_time = if self.continuous_adjustment_time_point.diff_scale >= 0 {
            t1
        } else {
            expected_time_max
        };

        let new_diff: i64 = if in_time < expected_time { -55 } else { 55 };

        self.continuous_adjustment_time_point.rtc_offset = uptime_ns;
        self.continuous_adjustment_time_point.shift_amount =
            if expected_time == in_time { 0 } else { 14 };
        self.continuous_adjustment_time_point.diff_scale = if expected_time == in_time {
            0
        } else {
            new_diff
        };
        self.continuous_adjustment_time_point.lower = expected_time;
    }
}

impl SteadyClockCoreImpl for StandardSteadyClockCore {
    fn get_current_time_point_impl(&self) -> Result<SteadyClockTimePoint, ResultCode> {
        let current_time_ns = self.get_current_raw_time_point_impl();
        // Convert nanoseconds to seconds
        let current_time_s = current_time_ns / 1_000_000_000;
        Ok(SteadyClockTimePoint {
            time_point: current_time_s,
            clock_source_id: self.clock_source_id,
        })
    }

    fn get_current_raw_time_point_impl(&self) -> i64 {
        let _lock = self.mutex.lock().unwrap();
        let ticks_ns = (self.get_ticks_ns)();
        let current_time_ns = self.rtc_offset + ticks_ns;
        // Note: upstream caches and returns max of current vs cached.
        // We can't mutate through &self with the Mutex<()> pattern,
        // so we just return the computed value. For full parity,
        // cached_time_point tracking would need interior mutability.
        current_time_ns.max(self.cached_time_point)
    }

    fn get_test_offset_impl(&self) -> i64 {
        self.test_offset
    }

    fn set_test_offset_impl(&mut self, offset: i64) {
        self.test_offset = offset;
    }

    fn get_internal_offset_impl(&self) -> i64 {
        self.internal_offset
    }

    fn set_internal_offset_impl(&mut self, offset: i64) {
        self.internal_offset = offset;
    }

    fn get_rtc_value_impl(&self) -> Result<i64, ResultCode> {
        Err(RESULT_NOT_IMPLEMENTED)
    }

    fn get_setup_result_value_impl(&self) -> ResultCode {
        crate::hle::result::RESULT_SUCCESS
    }
}
