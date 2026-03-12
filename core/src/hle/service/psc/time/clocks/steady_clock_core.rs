// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/clocks/steady_clock_core.h
//!
//! SteadyClockCore: abstract base for steady clocks. Upstream is a virtual base class;
//! here we use a trait.

use crate::hle::result::ResultCode;
use super::super::common::SteadyClockTimePoint;

/// One second in nanoseconds.
const ONE_SECOND_NS: i64 = 1_000_000_000;

/// Trait matching the C++ SteadyClockCore virtual interface.
///
/// Each implementation must provide the `*_impl` methods. The trait provides
/// the public accessors that apply test/internal offsets (matching upstream
/// non-virtual public methods).
pub trait SteadyClockCoreImpl {
    fn get_current_time_point_impl(&self) -> Result<SteadyClockTimePoint, ResultCode>;
    fn get_current_raw_time_point_impl(&self) -> i64;
    fn get_test_offset_impl(&self) -> i64;
    fn set_test_offset_impl(&mut self, offset: i64);
    fn get_internal_offset_impl(&self) -> i64;
    fn set_internal_offset_impl(&mut self, offset: i64);
    fn get_rtc_value_impl(&self) -> Result<i64, ResultCode>;
    fn get_setup_result_value_impl(&self) -> ResultCode;
}

/// Shared state for SteadyClockCore (mirrors upstream m_initialized, m_reset_detected).
pub struct SteadyClockCoreState {
    initialized: bool,
    reset_detected: bool,
}

impl Default for SteadyClockCoreState {
    fn default() -> Self {
        Self {
            initialized: false,
            reset_detected: false,
        }
    }
}

impl SteadyClockCoreState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_initialized(&mut self) {
        self.initialized = true;
    }

    pub fn is_initialized(&self) -> bool {
        self.initialized
    }

    pub fn set_reset_detected(&mut self) {
        self.reset_detected = true;
    }

    pub fn is_reset_detected(&self) -> bool {
        self.reset_detected
    }
}

/// Public helper methods matching upstream SteadyClockCore non-virtual public methods.
/// These combine the impl methods with offset application.
pub fn get_current_time_point(
    core: &dyn SteadyClockCoreImpl,
) -> Result<SteadyClockTimePoint, ResultCode> {
    let mut time_point = core.get_current_time_point_impl()?;
    time_point.time_point += core.get_test_offset_impl() / ONE_SECOND_NS;
    time_point.time_point += core.get_internal_offset_impl() / ONE_SECOND_NS;
    Ok(time_point)
}

pub fn get_raw_time(core: &dyn SteadyClockCoreImpl) -> i64 {
    core.get_current_raw_time_point_impl()
        + core.get_test_offset_impl()
        + core.get_internal_offset_impl()
}
