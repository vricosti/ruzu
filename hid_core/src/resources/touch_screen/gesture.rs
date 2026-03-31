// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/touch_screen/gesture.h and gesture.cpp

use std::sync::Mutex;

use common::ResultCode;

use crate::resources::touch_screen::touch_screen_driver::TouchScreenDriver;
use crate::resources::touch_screen::touch_screen_resource::TouchResource;

/// Handles gesture requests from HID interfaces.
/// Delegates to TouchResource for actual activation/deactivation.
///
/// Upstream holds `shared_ptr<TouchResource>` — in the Rust port, the caller
/// passes mutable references to TouchResource and TouchScreenDriver where needed.
pub struct Gesture {
    mutex: Mutex<()>,
}

impl Gesture {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
        }
    }

    /// Port of Gesture::Activate().
    pub fn activate(
        &self,
        resource: &mut TouchResource,
        driver: &mut TouchScreenDriver,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        // Upstream: CreateThread(), then touch_resource->ActivateGesture()
        let result = resource.activate_gesture(driver);
        if result.is_error() {
            // Upstream: StopThread()
        }
        result
    }

    /// Port of Gesture::Activate(aruid, basic_gesture_id).
    pub fn activate_with_aruid(
        &self,
        resource: &mut TouchResource,
        aruid: u64,
        basic_gesture_id: u32,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        resource.activate_gesture_with_aruid(aruid, basic_gesture_id)
    }

    /// Port of Gesture::Deactivate().
    pub fn deactivate(
        &self,
        resource: &mut TouchResource,
        driver: &mut TouchScreenDriver,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        let result = resource.deactivate_gesture(driver);
        if result.is_error() {
            return result;
        }
        // Upstream: StopThread()
        ResultCode::SUCCESS
    }

    /// Port of Gesture::IsActive().
    pub fn is_active(&self, resource: &TouchResource) -> bool {
        resource.is_gesture_active()
    }
}

impl Default for Gesture {
    fn default() -> Self {
        Self::new()
    }
}
