// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/touch_screen/touch_screen_resource.h and
//! touch_screen_resource.cpp
//!
//! TouchResource is the central resource that manages touch and gesture
//! activation per-aruid, processes raw touch input from TouchScreenDriver,
//! and writes into shared memory via the gesture handler.

use common::ResultCode;

use super::gesture_handler::GestureHandler;
use super::touch_screen_driver::TouchScreenDriver;
use super::touch_types::*;
use crate::hid_result;
use crate::hid_types::TouchScreenModeForNx;
use crate::resources::applet_resource::ARUID_INDEX_MAX;

/// Gesture update period: 4ms (1000Hz), matching upstream GestureUpdatePeriod.
pub const GESTURE_UPDATE_PERIOD_NS: u64 = 4 * 1000 * 1000;

/// Central resource for touch screen and gesture management.
/// Port of upstream `TouchResource`.
pub struct TouchResource {
    global_ref_counter: i32,
    gesture_ref_counter: i32,
    touch_ref_counter: i32,
    is_initialized: bool,
    sample_number: u64,

    // Internal state
    current_touch_state: TouchScreenState,
    previous_touch_state: TouchScreenState,
    gesture_state: GestureState,
    is_auto_pilot_initialized: bool,
    auto_pilot: AutoPilotState,
    gesture_handler: GestureHandler,
    aruid_data: [TouchAruidData; ARUID_INDEX_MAX],
    magnification_x: f32,
    magnification_y: f32,
    offset_x: f32,
    offset_y: f32,
    default_touch_screen_mode: TouchScreenModeForNx,
}

impl TouchResource {
    pub fn new() -> Self {
        Self {
            global_ref_counter: 0,
            gesture_ref_counter: 0,
            touch_ref_counter: 0,
            is_initialized: false,
            sample_number: 0,
            current_touch_state: TouchScreenState::default(),
            previous_touch_state: TouchScreenState::default(),
            gesture_state: GestureState::default(),
            is_auto_pilot_initialized: false,
            auto_pilot: AutoPilotState::default(),
            gesture_handler: GestureHandler::new(),
            aruid_data: [TouchAruidData::default(); ARUID_INDEX_MAX],
            magnification_x: 1.0,
            magnification_y: 1.0,
            offset_x: 0.0,
            offset_y: 0.0,
            default_touch_screen_mode: TouchScreenModeForNx::Finger,
        }
    }

    /// Port of TouchResource::ActivateTouch() (no-aruid version).
    pub fn activate_touch(&mut self, touch_driver: &mut TouchScreenDriver) -> ResultCode {
        if self.global_ref_counter == i32::MAX - 1 || self.touch_ref_counter == i32::MAX - 1 {
            return hid_result::RESULT_TOUCH_OVERFLOW;
        }

        if self.global_ref_counter == 0 {
            let result = touch_driver.start_touch_sensor();
            if !result.is_success() {
                return result;
            }

            self.is_initialized = true;
            // Upstream: system.CoreTiming().ScheduleLoopingEvent(...)
            self.current_touch_state = TouchScreenState::default();
            // Upstream: ReadTouchInput()
            self.gesture_handler.set_touch_state(
                &self.current_touch_state.states,
                self.current_touch_state.entry_count as u32,
                0,
            );
        }

        // Upstream: reads touch mode from system settings service
        // default_touch_screen_mode = static_cast<TouchScreenModeForNx>(touch_mode);

        self.global_ref_counter += 1;
        self.touch_ref_counter += 1;
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::ActivateTouch(u64 aruid).
    /// In upstream, this iterates over applet resource aruid data and initializes
    /// touch shared memory for the specified aruid.
    pub fn activate_touch_with_aruid(&mut self, _aruid: u64) -> ResultCode {
        // Upstream: locks shared_mutex, iterates AruidIndexMax entries,
        // validates applet_data, initializes touch_screen_lifo for matching aruid.
        // Requires AppletResource integration which is not yet wired up.
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::ActivateGesture() (no-aruid version).
    pub fn activate_gesture(&mut self, touch_driver: &mut TouchScreenDriver) -> ResultCode {
        if self.global_ref_counter == i32::MAX - 1 || self.gesture_ref_counter == i32::MAX - 1 {
            return hid_result::RESULT_GESTURE_OVERFLOW;
        }

        if self.global_ref_counter == 0 {
            let result = touch_driver.start_touch_sensor();
            if !result.is_success() {
                return result;
            }

            self.is_initialized = true;
            // Upstream: system.CoreTiming().ScheduleLoopingEvent(...)
            self.current_touch_state = TouchScreenState::default();
            // Upstream: ReadTouchInput()
            self.gesture_handler.set_touch_state(
                &self.current_touch_state.states,
                self.current_touch_state.entry_count as u32,
                0,
            );
        }

        self.global_ref_counter += 1;
        self.gesture_ref_counter += 1;
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::ActivateGesture(u64 aruid, u32 basic_gesture_id).
    pub fn activate_gesture_with_aruid(
        &mut self,
        _aruid: u64,
        _basic_gesture_id: u32,
    ) -> ResultCode {
        // Upstream: locks shared_mutex, iterates AruidIndexMax entries,
        // validates applet_data, initializes gesture_lifo for matching aruid.
        // Requires AppletResource integration which is not yet wired up.
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::DeactivateTouch.
    pub fn deactivate_touch(&mut self, touch_driver: &mut TouchScreenDriver) -> ResultCode {
        if self.touch_ref_counter == 0 || self.global_ref_counter == 0 {
            return hid_result::RESULT_TOUCH_NOT_INITIALIZED;
        }

        self.global_ref_counter -= 1;
        self.touch_ref_counter -= 1;

        if self.touch_ref_counter + self.global_ref_counter != 0 {
            return ResultCode::SUCCESS;
        }

        self.finalize(touch_driver)
    }

    /// Port of TouchResource::DeactivateGesture.
    pub fn deactivate_gesture(&mut self, touch_driver: &mut TouchScreenDriver) -> ResultCode {
        if self.gesture_ref_counter == 0 || self.global_ref_counter == 0 {
            return hid_result::RESULT_GESTURE_NOT_INITIALIZED;
        }

        self.global_ref_counter -= 1;
        self.gesture_ref_counter -= 1;

        if self.touch_ref_counter + self.global_ref_counter != 0 {
            return ResultCode::SUCCESS;
        }

        self.finalize(touch_driver)
    }

    /// Port of TouchResource::IsTouchActive.
    pub fn is_touch_active(&self) -> bool {
        self.touch_ref_counter != 0
    }

    /// Port of TouchResource::IsGestureActive.
    pub fn is_gesture_active(&self) -> bool {
        self.gesture_ref_counter != 0
    }

    /// Port of TouchResource::SetTouchScreenAutoPilotState.
    pub fn set_touch_screen_auto_pilot_state(
        &mut self,
        auto_pilot_state: &AutoPilotState,
    ) -> ResultCode {
        if self.global_ref_counter == 0 {
            return hid_result::RESULT_TOUCH_NOT_INITIALIZED;
        }

        if !self.is_auto_pilot_initialized {
            self.is_auto_pilot_initialized = true;
            self.auto_pilot = AutoPilotState::default();
        }

        let mut state = TouchScreenState {
            entry_count: auto_pilot_state.count as i32,
            states: auto_pilot_state.state,
            ..Default::default()
        };

        sanitize_touch_input(&mut state);

        self.auto_pilot.count = state.entry_count as u64;
        self.auto_pilot.state = state.states;
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::UnsetTouchScreenAutoPilotState.
    pub fn unset_touch_screen_auto_pilot_state(&mut self) -> ResultCode {
        if self.global_ref_counter == 0 {
            return hid_result::RESULT_TOUCH_NOT_INITIALIZED;
        }

        self.is_auto_pilot_initialized = false;
        self.auto_pilot = AutoPilotState::default();
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::RequestNextTouchInput.
    pub fn request_next_touch_input(
        &mut self,
        touch_driver: &mut TouchScreenDriver,
        is_handheld_hid_enabled: bool,
    ) -> ResultCode {
        if self.global_ref_counter == 0 {
            return hid_result::RESULT_TOUCH_NOT_INITIALIZED;
        }

        if is_handheld_hid_enabled {
            let result = touch_driver.wait_for_input();
            if !result.is_success() {
                return result;
            }
        }

        self.is_initialized = true;
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::RequestNextDummyInput.
    pub fn request_next_dummy_input(
        &mut self,
        touch_driver: &mut TouchScreenDriver,
        is_handheld_hid_enabled: bool,
    ) -> ResultCode {
        if self.global_ref_counter == 0 {
            return hid_result::RESULT_TOUCH_NOT_INITIALIZED;
        }

        if is_handheld_hid_enabled {
            let result = touch_driver.wait_for_dummy_input();
            if !result.is_success() {
                return result;
            }
        }

        self.is_initialized = false;
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::ProcessTouchScreenAutoTune.
    pub fn process_touch_screen_auto_tune(&self, touch_driver: &TouchScreenDriver) -> ResultCode {
        touch_driver.process_touch_screen_auto_tune();
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::SetTouchScreenMagnification.
    pub fn set_touch_screen_magnification(
        &mut self,
        point1_x: f32,
        point1_y: f32,
        point2_x: f32,
        point2_y: f32,
    ) {
        self.offset_x = point1_x;
        self.offset_y = point1_y;
        self.magnification_x = point2_x;
        self.magnification_y = point2_y;
    }

    /// Port of TouchResource::SetTouchScreenResolution.
    pub fn set_touch_screen_resolution(
        &mut self,
        width: u32,
        height: u32,
        aruid: u64,
    ) -> ResultCode {
        for aruid_index in 0..ARUID_INDEX_MAX {
            let data = &mut self.aruid_data[aruid_index];
            if aruid != data.aruid {
                continue;
            }
            data.resolution_width = width as u16;
            data.resolution_height = height as u16;
        }
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::SetTouchScreenConfiguration.
    pub fn set_touch_screen_configuration(
        &mut self,
        mode: TouchScreenModeForNx,
        aruid: u64,
    ) -> ResultCode {
        for aruid_index in 0..ARUID_INDEX_MAX {
            let data = &mut self.aruid_data[aruid_index];
            if aruid != data.aruid {
                continue;
            }
            data.finger_map.touch_mode = mode;
        }
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::GetTouchScreenConfiguration.
    pub fn get_touch_screen_configuration(
        &self,
        aruid: u64,
    ) -> Result<TouchScreenModeForNx, ResultCode> {
        for aruid_index in 0..ARUID_INDEX_MAX {
            let data = &self.aruid_data[aruid_index];
            if aruid != data.aruid {
                continue;
            }
            return Ok(data.finger_map.touch_mode);
        }
        Ok(TouchScreenModeForNx::UseSystemSetting)
    }

    /// Port of TouchResource::SetTouchScreenDefaultConfiguration.
    pub fn set_touch_screen_default_configuration(
        &mut self,
        mode: TouchScreenModeForNx,
    ) -> ResultCode {
        self.default_touch_screen_mode = mode;
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::GetTouchScreenDefaultConfiguration.
    pub fn get_touch_screen_default_configuration(&self) -> TouchScreenModeForNx {
        self.default_touch_screen_mode
    }

    /// Port of TouchResource::OnTouchUpdate.
    pub fn on_touch_update(
        &mut self,
        touch_driver: &mut TouchScreenDriver,
        is_handheld_hid_enabled: bool,
        timestamp: i64,
    ) {
        if self.global_ref_counter == 0 {
            return;
        }

        self.read_touch_input(touch_driver, is_handheld_hid_enabled);
        self.gesture_handler.set_touch_state(
            &self.current_touch_state.states,
            self.current_touch_state.entry_count as u32,
            timestamp,
        );

        // Upstream: locks shared_mutex and iterates aruid data, writing to
        // shared memory lifos for each aruid. This requires full AppletResource
        // integration which is not yet wired up.
    }

    /// Port of TouchResource::Finalize.
    fn finalize(&mut self, touch_driver: &mut TouchScreenDriver) -> ResultCode {
        self.is_auto_pilot_initialized = false;
        self.auto_pilot = AutoPilotState::default();
        // Upstream: system.CoreTiming().UnscheduleEvent(timer_event)

        let result = touch_driver.stop_touch_sensor();
        if !result.is_success() {
            return result;
        }

        self.is_initialized = false;
        ResultCode::SUCCESS
    }

    /// Port of TouchResource::StorePreviousTouchState.
    fn store_previous_touch_state(
        out_previous_touch: &mut TouchScreenState,
        out_finger_map: &mut TouchFingerMap,
        current_touch: &TouchScreenState,
        is_touch_enabled: bool,
    ) {
        let mut finger_count: i32 = 0;

        if is_touch_enabled {
            finger_count = current_touch.entry_count;
            if finger_count < 1 {
                out_finger_map.finger_count = 0;
                out_finger_map.finger_ids = [0; MAX_FINGERS];
                out_previous_touch.sampling_number = current_touch.sampling_number;
                out_previous_touch.entry_count = 0;
                out_previous_touch.states = [TouchState::default(); MAX_FINGERS];
                return;
            }
            for i in 0..(finger_count as usize) {
                out_finger_map.finger_ids[i] = current_touch.states[i].finger;
                out_previous_touch.states[i] = current_touch.states[i];
            }
            out_finger_map.finger_count = finger_count;
            return;
        }

        if !is_touch_enabled && out_finger_map.finger_count > 0 && current_touch.entry_count > 0 {
            // Upstream TODO: not yet implemented in C++ upstream
        }

        // Zero out unused entries
        for i in (finger_count as usize)..MAX_FINGERS {
            out_finger_map.finger_ids[i] = 0;
            out_previous_touch.states[i] = TouchState::default();
        }

        out_previous_touch.sampling_number = current_touch.sampling_number;
        out_previous_touch.entry_count = finger_count;
    }

    /// Port of TouchResource::ReadTouchInput.
    fn read_touch_input(
        &mut self,
        touch_driver: &mut TouchScreenDriver,
        is_handheld_hid_enabled: bool,
    ) {
        self.previous_touch_state = self.current_touch_state;

        if !self.is_initialized || !is_handheld_hid_enabled || !touch_driver.is_running() {
            touch_driver.wait_for_dummy_input();
        } else {
            touch_driver.wait_for_input();
        }

        touch_driver.get_next_touch_state(&mut self.current_touch_state);
        sanitize_touch_input(&mut self.current_touch_state);
        self.current_touch_state.sampling_number = self.sample_number as i64;
        self.sample_number += 1;

        // Apply auto-pilot state if initialized and no real touches
        if self.is_auto_pilot_initialized && self.current_touch_state.entry_count == 0 {
            let finger_count = self.auto_pilot.count as usize;
            self.current_touch_state.entry_count = finger_count as i32;
            for i in 0..finger_count {
                self.current_touch_state.states[i] = self.auto_pilot.state[i];
            }

            let mut index = 0usize;
            for i in 0..finger_count {
                if self.auto_pilot.state[i].attribute.end_touch() {
                    continue;
                }
                let mut state = self.auto_pilot.state[i];
                state.attribute = crate::hid_types::TouchAttribute::default();
                self.auto_pilot.state[index] = state;
                index += 1;
            }

            self.auto_pilot.count = index as u64;
            for i in index..self.auto_pilot.state.len() {
                self.auto_pilot.state[i] = TouchState::default();
            }
        }

        // Apply magnification and offset
        for i in 0..(self.current_touch_state.entry_count as usize) {
            let state = &mut self.current_touch_state.states[i];
            state.position_x = ((self.magnification_y * state.position_x as f32)
                + (self.offset_x * TOUCH_SENSOR_WIDTH as f32))
                as u32;
            state.position_y = ((self.magnification_y * state.position_y as f32)
                + (self.offset_x * TOUCH_SENSOR_HEIGHT as f32))
                as u32;
            state.diameter_x = (self.magnification_x * state.diameter_x as f32) as u32;
            state.diameter_y = (self.magnification_y * state.diameter_y as f32) as u32;
        }

        // Filter out-of-bounds touches
        let entry_count = self.current_touch_state.entry_count as usize;
        let mut index = 0usize;
        // Copy states into a temporary buffer to avoid aliasing issues
        let old_states = self.current_touch_state.states;
        for i in 0..entry_count {
            let old_state = &old_states[i];
            if TOUCH_SENSOR_WIDTH <= old_state.position_x
                || TOUCH_SENSOR_HEIGHT <= old_state.position_y
            {
                continue;
            }
            self.current_touch_state.states[index] = *old_state;
            index += 1;
        }
        self.current_touch_state.entry_count = index as i32;

        sanitize_touch_input(&mut self.current_touch_state);

        // Upstream: locks input_mutex, checks if touch changed, signals input_event
        if self.current_touch_state.entry_count == self.previous_touch_state.entry_count {
            if self.current_touch_state.entry_count < 1 {
                return;
            }
            let mut has_moved = false;
            for i in 0..(self.current_touch_state.entry_count as usize) {
                let delta_x = (self.current_touch_state.states[i].position_x as i32
                    - self.previous_touch_state.states[i].position_x as i32)
                    .abs();
                let delta_y = (self.current_touch_state.states[i].position_y as i32
                    - self.previous_touch_state.states[i].position_y as i32)
                    .abs();
                if delta_x > 1 || delta_y > 1 {
                    has_moved = true;
                }
            }
            if !has_moved {
                return;
            }
        }

        // Upstream: input_event->Signal()
    }

    /// Access the current touch state (for testing or external consumers).
    pub fn current_touch_state(&self) -> &TouchScreenState {
        &self.current_touch_state
    }

    /// Access the gesture handler.
    pub fn gesture_handler(&self) -> &GestureHandler {
        &self.gesture_handler
    }

    /// Access the gesture handler mutably.
    pub fn gesture_handler_mut(&mut self) -> &mut GestureHandler {
        &mut self.gesture_handler
    }
}

/// Port of TouchResource::SanitizeInput.
/// Standalone function to avoid borrow checker issues when called on self fields.
fn sanitize_touch_input(state: &mut TouchScreenState) {
    for i in 0..(state.entry_count as usize) {
        let entry = &mut state.states[i];
        entry.position_x = entry
            .position_x
            .clamp(TOUCH_BORDERS, TOUCH_SENSOR_WIDTH - TOUCH_BORDERS - 1);
        entry.position_y = entry
            .position_y
            .clamp(TOUCH_BORDERS, TOUCH_SENSOR_HEIGHT - TOUCH_BORDERS - 1);
        entry.diameter_x = entry
            .diameter_x
            .clamp(0, TOUCH_SENSOR_WIDTH - MAX_TOUCH_DIAMETER);
        entry.diameter_y = entry
            .diameter_y
            .clamp(0, TOUCH_SENSOR_HEIGHT - MAX_TOUCH_DIAMETER);
        entry.rotation_angle = entry
            .rotation_angle
            .clamp(-MAX_ROTATION_ANGLE, MAX_ROTATION_ANGLE);
    }
}

impl Default for TouchResource {
    fn default() -> Self {
        Self::new()
    }
}
