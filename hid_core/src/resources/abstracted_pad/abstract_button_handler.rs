// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_button_handler.h and abstract_button_handler.cpp

use common::ResultCode;

use crate::hid_result;
use crate::hid_types::*;

/// GC trigger state tracked by the button handler
struct GcTrigger {
    left: f32,
    right: f32,
}

impl Default for GcTrigger {
    fn default() -> Self {
        Self {
            left: 0.0,
            right: 0.0,
        }
    }
}

/// Handles Npad button request from HID interfaces
pub struct NpadAbstractButtonHandler {
    ref_counter: i32,
    is_button_pressed_on_console_mode: bool,
    gc_sampling_number: u64,
    gc_trigger_state: GcTrigger,
}

impl Default for NpadAbstractButtonHandler {
    fn default() -> Self {
        Self {
            ref_counter: 0,
            is_button_pressed_on_console_mode: false,
            gc_sampling_number: 0,
            gc_trigger_state: GcTrigger::default(),
        }
    }
}

impl NpadAbstractButtonHandler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn increment_ref_counter(&mut self) -> ResultCode {
        if self.ref_counter == i32::MAX - 1 {
            return hid_result::RESULT_NPAD_HANDLER_OVERFLOW;
        }
        self.ref_counter += 1;
        ResultCode::SUCCESS
    }

    pub fn decrement_ref_counter(&mut self) -> ResultCode {
        if self.ref_counter == 0 {
            return hid_result::RESULT_NPAD_HANDLER_NOT_INITIALIZED;
        }
        self.ref_counter -= 1;
        ResultCode::SUCCESS
    }

    pub fn update_all_button_lifo(&mut self) {
        // Upstream iterates over AruidIndexMax aruid data entries and calls
        // UpdateButtonLifo for each
    }

    pub fn update_core_battery_state(&mut self) {
        // Upstream iterates over AruidIndexMax aruid data entries and calls
        // UpdateButtonLifo for each
    }

    pub fn update_button_state(&mut self, _aruid: u64) {
        // Upstream gets aruid data and calls UpdateButtonLifo
    }

    pub fn is_button_pressed_on_console_mode(&self) -> bool {
        self.is_button_pressed_on_console_mode
    }

    pub fn enable_center_clamp(&self) {
        // Upstream iterates abstract pads and sets internal_flags.use_center_clamp = true
    }

    // The following methods update specific lifo buffers per style.
    // Upstream C++ also has TODO stubs for these.

    fn update_npad_fullkey_lifo(&mut self, _style_tag: NpadStyleTag, _style_index: i32, _aruid: u64) {
        // TODO: upstream also has TODO here
    }

    fn update_handheld_lifo(&mut self, _style_tag: NpadStyleTag, _style_index: i32, _aruid: u64) {
        // TODO: upstream also has TODO here
    }

    fn update_joycon_dual_lifo(&mut self, _style_tag: NpadStyleTag, _style_index: i32, _aruid: u64) {
        // TODO: upstream also has TODO here
    }

    fn update_joycon_left_lifo(&mut self, _style_tag: NpadStyleTag, _style_index: i32, _aruid: u64) {
        // TODO: upstream also has TODO here
    }

    fn update_joycon_right_lifo(&mut self, _style_tag: NpadStyleTag, _style_index: i32, _aruid: u64) {
        // TODO: upstream also has TODO here
    }

    fn update_system_ext_lifo(&mut self, _style_tag: NpadStyleTag, _style_index: i32, _aruid: u64) {
        // TODO: upstream also has TODO here
    }

    fn update_palma_lifo(&mut self, _style_tag: NpadStyleTag, _style_index: i32, _aruid: u64) {
        // TODO: upstream also has TODO here
    }
}
