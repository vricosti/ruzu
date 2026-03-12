// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/touch_screen/touch_screen.h and touch_screen.cpp

use std::sync::Mutex;
use common::ResultCode;

/// Handles touch requests from HID interfaces.
/// Delegates all operations to TouchResource.
pub struct TouchScreen {
    mutex: Mutex<()>,
    is_active: bool,
}

impl TouchScreen {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            is_active: false,
        }
    }

    pub fn activate(&mut self) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        // Upstream: CreateThread(), then touch_resource->ActivateTouch()
        self.is_active = true;
        ResultCode::SUCCESS
    }

    pub fn activate_with_aruid(&mut self, _aruid: u64) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        self.is_active = true;
        ResultCode::SUCCESS
    }

    pub fn deactivate(&mut self) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        self.is_active = false;
        ResultCode::SUCCESS
    }

    pub fn is_active(&self) -> bool {
        self.is_active
    }

    pub fn set_touch_screen_resolution(
        &self,
        _width: u32,
        _height: u32,
        _aruid: u64,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        // Upstream: touch_resource->SetTouchScreenResolution(width, height, aruid)
        ResultCode::SUCCESS
    }

    pub fn on_touch_update(&self, _timestamp: u64) {
        let _lock = self.mutex.lock().unwrap();
        // Upstream: touch_resource->OnTouchUpdate(timestamp)
    }
}

impl Default for TouchScreen {
    fn default() -> Self {
        Self::new()
    }
}
