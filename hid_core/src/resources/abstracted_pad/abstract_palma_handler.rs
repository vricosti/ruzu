// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_palma_handler.h and abstract_palma_handler.cpp

use common::ResultCode;

use crate::hid_result;

/// Handles Npad Palma (Pokeball) request from HID interfaces
pub struct NpadAbstractPalmaHandler {
    ref_counter: i32,
}

impl Default for NpadAbstractPalmaHandler {
    fn default() -> Self {
        Self { ref_counter: 0 }
    }
}

impl NpadAbstractPalmaHandler {
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

    pub fn update_palma_state(&mut self) {
        // Upstream TODO: not yet implemented in C++ upstream
    }
}
