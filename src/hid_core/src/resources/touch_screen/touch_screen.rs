// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/touch_screen/touch_screen.h and touch_screen.cpp

use std::sync::Mutex;

use common::ResultCode;

use crate::hid_types::TouchScreenConfigurationForNx;
use crate::resources::touch_screen::touch_screen_driver::TouchScreenDriver;
use crate::resources::touch_screen::touch_screen_resource::TouchResource;
use crate::resources::touch_screen::touch_types::AutoPilotState;

/// Handles touch requests from HID interfaces.
/// Delegates all operations to TouchResource, with a mutex guard on each call.
///
/// Upstream holds `shared_ptr<TouchResource>` — in the Rust port, the caller
/// passes mutable references to both TouchResource and TouchScreenDriver where
/// needed, since Rust ownership rules prevent shared mutable pointers.
pub struct TouchScreen {
    mutex: Mutex<()>,
}

impl TouchScreen {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
        }
    }

    /// Port of TouchScreen::Activate().
    pub fn activate(
        &self,
        resource: &mut TouchResource,
        driver: &mut TouchScreenDriver,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        // Upstream: CreateThread(), then touch_resource->ActivateTouch()
        resource.activate_touch(driver)
    }

    /// Port of TouchScreen::Activate(u64 aruid).
    pub fn activate_with_aruid(&self, resource: &mut TouchResource, aruid: u64) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        resource.activate_touch_with_aruid(aruid)
    }

    /// Port of TouchScreen::Deactivate().
    pub fn deactivate(
        &self,
        resource: &mut TouchResource,
        driver: &mut TouchScreenDriver,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        let result = resource.deactivate_touch(driver);
        if result.is_error() {
            return result;
        }
        // Upstream: StopThread()
        ResultCode::SUCCESS
    }

    /// Port of TouchScreen::IsActive(bool& out_is_active).
    pub fn is_active(&self, resource: &TouchResource) -> bool {
        resource.is_touch_active()
    }

    /// Port of TouchScreen::SetTouchScreenAutoPilotState.
    pub fn set_touch_screen_auto_pilot_state(
        &self,
        resource: &mut TouchResource,
        auto_pilot_state: &AutoPilotState,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        resource.set_touch_screen_auto_pilot_state(auto_pilot_state)
    }

    /// Port of TouchScreen::UnsetTouchScreenAutoPilotState.
    pub fn unset_touch_screen_auto_pilot_state(&self, resource: &mut TouchResource) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        resource.unset_touch_screen_auto_pilot_state()
    }

    /// Port of TouchScreen::RequestNextTouchInput.
    pub fn request_next_touch_input(
        &self,
        resource: &mut TouchResource,
        driver: &mut TouchScreenDriver,
        is_handheld_hid_enabled: bool,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        resource.request_next_touch_input(driver, is_handheld_hid_enabled)
    }

    /// Port of TouchScreen::RequestNextDummyInput.
    pub fn request_next_dummy_input(
        &self,
        resource: &mut TouchResource,
        driver: &mut TouchScreenDriver,
        is_handheld_hid_enabled: bool,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        resource.request_next_dummy_input(driver, is_handheld_hid_enabled)
    }

    /// Port of TouchScreen::ProcessTouchScreenAutoTune.
    pub fn process_touch_screen_auto_tune(
        &self,
        resource: &TouchResource,
        driver: &TouchScreenDriver,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        resource.process_touch_screen_auto_tune(driver)
    }

    /// Port of TouchScreen::SetTouchScreenMagnification.
    pub fn set_touch_screen_magnification(
        &self,
        resource: &mut TouchResource,
        point1_x: f32,
        point1_y: f32,
        point2_x: f32,
        point2_y: f32,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        resource.set_touch_screen_magnification(point1_x, point1_y, point2_x, point2_y);
        ResultCode::SUCCESS
    }

    /// Port of TouchScreen::SetTouchScreenResolution.
    pub fn set_touch_screen_resolution(
        &self,
        resource: &mut TouchResource,
        width: u32,
        height: u32,
        aruid: u64,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        resource.set_touch_screen_resolution(width, height, aruid)
    }

    /// Port of TouchScreen::SetTouchScreenConfiguration.
    pub fn set_touch_screen_configuration(
        &self,
        resource: &mut TouchResource,
        mode: &TouchScreenConfigurationForNx,
        aruid: u64,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        resource.set_touch_screen_configuration(mode.mode, aruid)
    }

    /// Port of TouchScreen::GetTouchScreenConfiguration.
    pub fn get_touch_screen_configuration(
        &self,
        resource: &TouchResource,
        aruid: u64,
    ) -> (ResultCode, TouchScreenConfigurationForNx) {
        let _lock = self.mutex.lock().unwrap();
        match resource.get_touch_screen_configuration(aruid) {
            Ok(mode) => (
                ResultCode::SUCCESS,
                TouchScreenConfigurationForNx {
                    mode,
                    _padding: [0u8; 0xF],
                },
            ),
            Err(result) => (result, TouchScreenConfigurationForNx::default()),
        }
    }

    /// Port of TouchScreen::SetTouchScreenDefaultConfiguration.
    pub fn set_touch_screen_default_configuration(
        &self,
        resource: &mut TouchResource,
        mode: &TouchScreenConfigurationForNx,
    ) -> ResultCode {
        let _lock = self.mutex.lock().unwrap();
        resource.set_touch_screen_default_configuration(mode.mode)
    }

    /// Port of TouchScreen::GetTouchScreenDefaultConfiguration.
    pub fn get_touch_screen_default_configuration(
        &self,
        resource: &TouchResource,
    ) -> (ResultCode, TouchScreenConfigurationForNx) {
        let _lock = self.mutex.lock().unwrap();
        let mode = resource.get_touch_screen_default_configuration();
        (
            ResultCode::SUCCESS,
            TouchScreenConfigurationForNx {
                mode,
                _padding: [0u8; 0xF],
            },
        )
    }

    /// Port of TouchScreen::OnTouchUpdate.
    pub fn on_touch_update(
        &self,
        resource: &mut TouchResource,
        driver: &mut TouchScreenDriver,
        is_handheld_hid_enabled: bool,
        timestamp: i64,
    ) {
        let _lock = self.mutex.lock().unwrap();
        resource.on_touch_update(driver, is_handheld_hid_enabled, timestamp);
    }
}

impl Default for TouchScreen {
    fn default() -> Self {
        Self::new()
    }
}
