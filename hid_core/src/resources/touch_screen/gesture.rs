// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/touch_screen/gesture.h and gesture.cpp

use std::sync::Mutex;
use common::ResultCode;

/// Handles gesture requests from HID interfaces.
/// Delegates to TouchResource for actual activation/deactivation.
pub struct Gesture {
    mutex: Mutex<()>,
    // In the full system this holds Arc<TouchResource>.
    is_active: bool,
}

impl Gesture {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            is_active: false,
        }
    }

    /// Port of Gesture::Activate().
    pub fn activate(&mut self) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        // Upstream: CreateThread(), then touch_resource->ActivateGesture()
        self.is_active = true;
        ResultCode::SUCCESS
    }

    /// Port of Gesture::Activate(aruid, basic_gesture_id).
    pub fn activate_with_aruid(&mut self, _aruid: u64, _basic_gesture_id: u32) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        // Upstream: touch_resource->ActivateGesture(aruid, basic_gesture_id)
        self.is_active = true;
        ResultCode::SUCCESS
    }

    /// Port of Gesture::Deactivate().
    pub fn deactivate(&mut self) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        // Upstream: touch_resource->DeactivateGesture(), then StopThread()
        self.is_active = false;
        ResultCode::SUCCESS
    }

    /// Port of Gesture::IsActive().
    pub fn is_active(&self) -> bool {
        self.is_active
    }
}

impl Default for Gesture {
    fn default() -> Self {
        Self::new()
    }
}
