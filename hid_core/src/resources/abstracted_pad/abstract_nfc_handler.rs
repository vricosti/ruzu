// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_nfc_handler.h and abstract_nfc_handler.cpp

use common::ResultCode;

use crate::hid_result;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum NpadNfcState {
    #[default]
    Unavailable = 0,
    Available = 1,
    Active = 2,
}

/// Handles Npad NFC request from HID interfaces
pub struct NpadAbstractNfcHandler {
    ref_counter: i32,
    xcd_handle: u64,
    sensor_state: NpadNfcState,
}

impl Default for NpadAbstractNfcHandler {
    fn default() -> Self {
        Self {
            ref_counter: 0,
            xcd_handle: 0,
            sensor_state: NpadNfcState::Unavailable,
        }
    }
}

impl NpadAbstractNfcHandler {
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

    pub fn update_nfc_state(&mut self) {
        // Upstream iterates properties_handler->GetAbstractedPads() and:
        // - If no pads: signal if Active, set Unavailable and signal input_event
        // - If found pad with NFC: set Available if not already Active
        // - Otherwise: same as no pads
    }

    pub fn has_nfc_sensor(&self) -> bool {
        self.sensor_state != NpadNfcState::Unavailable
    }

    pub fn is_nfc_activated(&self) -> bool {
        self.sensor_state == NpadNfcState::Active
    }

    pub fn activate_nfc(&mut self, is_enabled: bool) -> ResultCode {
        if self.sensor_state == NpadNfcState::Active {
            return hid_result::RESULT_NFC_IS_NOT_READY;
        }

        let new_state = if is_enabled {
            NpadNfcState::Active
        } else {
            NpadNfcState::Available
        };

        if self.sensor_state != new_state {
            self.sensor_state = new_state;
            // Upstream signals nfc_activate_event here
        }
        ResultCode::SUCCESS
    }

    pub fn get_xcd_handle_with_nfc(&self) -> Result<u64, ResultCode> {
        if self.sensor_state == NpadNfcState::Unavailable {
            return Err(hid_result::RESULT_NFC_IS_NOT_READY);
        }
        if self.xcd_handle == 0 {
            return Err(hid_result::RESULT_NFC_XCD_HANDLE_IS_NOT_INITIALIZED);
        }
        Ok(self.xcd_handle)
    }
}
