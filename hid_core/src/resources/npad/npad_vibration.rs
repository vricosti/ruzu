// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/npad/npad_vibration.h and npad_vibration.cpp

use std::sync::Mutex;

use common::ResultCode;

use crate::hid_result;

/// Handles vibration master volume and permit sessions
pub struct NpadVibration {
    inner: Mutex<NpadVibrationInner>,
}

struct NpadVibrationInner {
    volume: f32,
    session_aruid: u64,
}

impl Default for NpadVibration {
    fn default() -> Self {
        Self {
            inner: Mutex::new(NpadVibrationInner {
                volume: 0.0,
                session_aruid: 0,
            }),
        }
    }
}

impl NpadVibration {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn activate(&self) -> ResultCode {
        let mut inner = self.inner.lock().unwrap();
        // Upstream reads master volume from system settings service.
        // Default to 1.0 when system settings are not available.
        let master_volume = 1.0f32;
        if master_volume < 0.0 || master_volume > 1.0 {
            return hid_result::RESULT_VIBRATION_STRENGTH_OUT_OF_RANGE;
        }
        inner.volume = master_volume;
        ResultCode::SUCCESS
    }

    pub fn deactivate(&self) -> ResultCode {
        ResultCode::SUCCESS
    }

    pub fn set_vibration_master_volume(&self, master_volume: f32) -> ResultCode {
        let mut inner = self.inner.lock().unwrap();
        if master_volume < 0.0 && master_volume > 1.0 {
            return hid_result::RESULT_VIBRATION_STRENGTH_OUT_OF_RANGE;
        }
        inner.volume = master_volume;
        // Upstream also writes to system settings service
        ResultCode::SUCCESS
    }

    pub fn get_vibration_volume(&self) -> Result<f32, ResultCode> {
        let inner = self.inner.lock().unwrap();
        Ok(inner.volume)
    }

    pub fn get_vibration_master_volume(&self) -> Result<f32, ResultCode> {
        let inner = self.inner.lock().unwrap();
        // Upstream reads from system settings; we return cached volume
        Ok(inner.volume)
    }

    pub fn begin_permit_vibration_session(&self, aruid: u64) -> ResultCode {
        let mut inner = self.inner.lock().unwrap();
        inner.session_aruid = aruid;
        inner.volume = 1.0;
        ResultCode::SUCCESS
    }

    pub fn end_permit_vibration_session(&self) -> ResultCode {
        let mut inner = self.inner.lock().unwrap();
        // Upstream reads master volume from system settings
        let master_volume = 1.0f32;
        if master_volume < 0.0 || master_volume > 1.0 {
            return hid_result::RESULT_VIBRATION_STRENGTH_OUT_OF_RANGE;
        }
        inner.volume = master_volume;
        inner.session_aruid = 0;
        ResultCode::SUCCESS
    }

    pub fn get_session_aruid(&self) -> u64 {
        let inner = self.inner.lock().unwrap();
        inner.session_aruid
    }
}
