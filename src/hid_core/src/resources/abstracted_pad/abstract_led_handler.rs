// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_led_handler.h and abstract_led_handler.cpp

use common::ResultCode;

use crate::hid_result;
use crate::hid_types::*;

/// Handles Npad LED request from HID interfaces
pub struct NpadAbstractLedHandler {
    ref_counter: i32,
    led_blinking: LedPattern,
    left_pattern: LedPattern,
    right_pattern: LedPattern,
    led_interval: u64,
}

impl Default for NpadAbstractLedHandler {
    fn default() -> Self {
        Self {
            ref_counter: 0,
            led_blinking: LedPattern::new(0, 0, 0, 0),
            left_pattern: LedPattern::new(0, 0, 0, 0),
            right_pattern: LedPattern::new(0, 0, 0, 0),
            led_interval: 0,
        }
    }
}

impl NpadAbstractLedHandler {
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

    pub fn set_npad_led_handler_led_pattern(&mut self, npad_id: NpadIdType) {
        match npad_id {
            NpadIdType::Player1 => {
                self.left_pattern = LedPattern::new(1, 0, 0, 0);
            }
            NpadIdType::Player2 => {
                self.left_pattern = LedPattern::new(1, 1, 0, 0);
            }
            NpadIdType::Player3 => {
                self.left_pattern = LedPattern::new(1, 1, 1, 0);
            }
            NpadIdType::Player4 => {
                self.left_pattern = LedPattern::new(1, 1, 1, 1);
            }
            NpadIdType::Player5 => {
                self.left_pattern = LedPattern::new(1, 0, 0, 1);
            }
            NpadIdType::Player6 => {
                self.left_pattern = LedPattern::new(1, 0, 1, 0);
            }
            NpadIdType::Player7 => {
                self.left_pattern = LedPattern::new(1, 0, 1, 1);
            }
            NpadIdType::Player8 => {
                self.left_pattern = LedPattern::new(0, 1, 1, 0);
            }
            NpadIdType::Other | NpadIdType::Handheld => {
                self.left_pattern = LedPattern::new(0, 0, 0, 0);
            }
            _ => {
                // Invalid npad id type
            }
        }

        match npad_id {
            NpadIdType::Player1 => {
                self.right_pattern = LedPattern::new(0, 0, 0, 1);
            }
            NpadIdType::Player2 => {
                self.right_pattern = LedPattern::new(0, 1, 1, 1);
            }
            NpadIdType::Player3 => {
                self.right_pattern = LedPattern::new(0, 1, 1, 1);
            }
            NpadIdType::Player4 => {
                self.right_pattern = LedPattern::new(1, 1, 1, 1);
            }
            NpadIdType::Player5 => {
                self.right_pattern = LedPattern::new(1, 0, 0, 1);
            }
            NpadIdType::Player6 => {
                self.right_pattern = LedPattern::new(0, 1, 0, 1);
            }
            NpadIdType::Player7 => {
                self.right_pattern = LedPattern::new(1, 1, 0, 1);
            }
            NpadIdType::Player8 => {
                self.right_pattern = LedPattern::new(0, 1, 1, 0);
            }
            NpadIdType::Other | NpadIdType::Handheld => {
                self.right_pattern = LedPattern::new(0, 0, 0, 0);
            }
            _ => {
                // Invalid npad id type
            }
        }
    }

    pub fn set_led_blinking_device(&mut self, pattern: LedPattern) {
        self.led_blinking = pattern;
    }
}
