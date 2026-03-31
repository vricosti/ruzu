// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_pad.h and abstract_pad.cpp

use common::ResultCode;

use crate::hid_result;
use crate::hid_types::*;
use crate::resources::abstracted_pad::abstract_battery_handler::NpadAbstractBatteryHandler;
use crate::resources::abstracted_pad::abstract_button_handler::NpadAbstractButtonHandler;
use crate::resources::abstracted_pad::abstract_ir_sensor_handler::NpadAbstractIrSensorHandler;
use crate::resources::abstracted_pad::abstract_led_handler::NpadAbstractLedHandler;
use crate::resources::abstracted_pad::abstract_mcu_handler::NpadAbstractMcuHandler;
use crate::resources::abstracted_pad::abstract_nfc_handler::NpadAbstractNfcHandler;
use crate::resources::abstracted_pad::abstract_pad_holder::NpadAbstractedPadHolder;
use crate::resources::abstracted_pad::abstract_palma_handler::NpadAbstractPalmaHandler;
use crate::resources::abstracted_pad::abstract_properties_handler::NpadAbstractPropertiesHandler;
use crate::resources::abstracted_pad::abstract_sixaxis_handler::NpadAbstractSixAxisHandler;
use crate::resources::abstracted_pad::abstract_vibration_handler::NpadAbstractVibrationHandler;
use crate::resources::npad::npad_types::MAX_SUPPORTED_NPAD_ID_TYPES;

/// Handles Npad request from HID interfaces
pub struct AbstractPad {
    abstract_pad_holder: NpadAbstractedPadHolder,
    properties_handler: NpadAbstractPropertiesHandler,
    led_handler: NpadAbstractLedHandler,
    ir_sensor_handler: NpadAbstractIrSensorHandler,
    nfc_handler: NpadAbstractNfcHandler,
    mcu_handler: NpadAbstractMcuHandler,
    vibration_handler: NpadAbstractVibrationHandler,
    sixaxis_handler: NpadAbstractSixAxisHandler,
    button_handler: NpadAbstractButtonHandler,
    battery_handler: NpadAbstractBatteryHandler,
    palma_handler: NpadAbstractPalmaHandler,
    ref_counter: i32,
    interface_type: NpadInterfaceType,
}

impl Default for AbstractPad {
    fn default() -> Self {
        Self {
            abstract_pad_holder: NpadAbstractedPadHolder::default(),
            properties_handler: NpadAbstractPropertiesHandler::default(),
            led_handler: NpadAbstractLedHandler::default(),
            ir_sensor_handler: NpadAbstractIrSensorHandler::default(),
            nfc_handler: NpadAbstractNfcHandler::default(),
            mcu_handler: NpadAbstractMcuHandler::default(),
            vibration_handler: NpadAbstractVibrationHandler::default(),
            sixaxis_handler: NpadAbstractSixAxisHandler::default(),
            button_handler: NpadAbstractButtonHandler::default(),
            battery_handler: NpadAbstractBatteryHandler::default(),
            palma_handler: NpadAbstractPalmaHandler::default(),
            ref_counter: 0,
            interface_type: NpadInterfaceType::None,
        }
    }
}

impl AbstractPad {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_npad_id(&mut self, npad_id: NpadIdType) {
        self.properties_handler.set_npad_id(npad_id);
    }

    pub fn activate(&mut self) -> ResultCode {
        if self.ref_counter == i32::MAX - 1 {
            return hid_result::RESULT_NPAD_HANDLER_OVERFLOW;
        }

        if self.ref_counter != 0 {
            self.ref_counter += 1;
            return ResultCode::SUCCESS;
        }

        let mut stage: usize = 0;
        let mut result = ResultCode::SUCCESS;

        if result.is_success() {
            stage += 1;
            result = self.properties_handler.increment_ref_counter();
        }
        if result.is_success() {
            stage += 1;
            result = self.led_handler.increment_ref_counter();
        }
        if result.is_success() {
            stage += 1;
            result = self.ir_sensor_handler.increment_ref_counter();
        }
        if result.is_success() {
            stage += 1;
            result = self.mcu_handler.increment_ref_counter();
        }
        if result.is_success() {
            stage += 1;
            result = self.nfc_handler.increment_ref_counter();
        }
        if result.is_success() {
            stage += 1;
            result = self.vibration_handler.increment_ref_counter();
        }
        if result.is_success() {
            stage += 1;
            result = self.sixaxis_handler.increment_ref_counter();
        }
        if result.is_success() {
            stage += 1;
            result = self.button_handler.increment_ref_counter();
        }
        if result.is_success() {
            stage += 1;
            result = self.battery_handler.increment_ref_counter();
        }
        if result.is_success() {
            stage += 1;
            result = self.palma_handler.increment_ref_counter();
        }

        if result.is_success() {
            self.ref_counter += 1;
            return result;
        }

        // Rollback on failure
        if stage > 9 {
            self.battery_handler.decrement_ref_counter();
        }
        if stage > 8 {
            self.button_handler.decrement_ref_counter();
        }
        if stage > 7 {
            self.sixaxis_handler.decrement_ref_counter();
        }
        if stage > 6 {
            self.vibration_handler.decrement_ref_counter();
        }
        if stage > 5 {
            self.nfc_handler.decrement_ref_counter();
        }
        if stage > 4 {
            self.mcu_handler.decrement_ref_counter();
        }
        if stage > 3 {
            self.ir_sensor_handler.decrement_ref_counter();
        }
        if stage > 2 {
            self.led_handler.decrement_ref_counter();
        }
        if stage > 1 {
            self.properties_handler.decrement_ref_counter();
        }

        result
    }

    pub fn deactivate(&mut self) -> ResultCode {
        if self.ref_counter == 0 {
            return hid_result::RESULT_NPAD_RESOURCE_NOT_INITIALIZED;
        }

        self.ref_counter -= 1;
        self.battery_handler.decrement_ref_counter();
        self.button_handler.decrement_ref_counter();
        self.sixaxis_handler.decrement_ref_counter();
        self.vibration_handler.decrement_ref_counter();
        self.nfc_handler.decrement_ref_counter();
        self.ir_sensor_handler.decrement_ref_counter();
        self.mcu_handler.decrement_ref_counter();
        self.led_handler.decrement_ref_counter();
        self.properties_handler.decrement_ref_counter();
        self.palma_handler.decrement_ref_counter();

        ResultCode::SUCCESS
    }

    pub fn activate_npad(&mut self, _aruid: u64) -> ResultCode {
        // Upstream calls properties_handler.ActivateNpadUnknown0x88(aruid),
        // sixaxis_handler.UpdateSixAxisState2(aruid),
        // battery_handler.UpdateBatteryState(aruid)
        ResultCode::SUCCESS
    }

    pub fn get_last_active_npad(&self) -> NpadIdType {
        self.properties_handler.get_npad_id()
    }

    pub fn update_interface_type(&mut self) {
        if self.interface_type != self.properties_handler.get_interface_type() {
            self.update();
        }
        self.battery_handler.update_battery_state();
    }

    pub fn update(&mut self) {
        self.properties_handler.update_device_type();
        let npad_id = self.properties_handler.get_npad_id();
        self.led_handler.set_npad_led_handler_led_pattern(npad_id);
        self.vibration_handler.update_vibration_state();
        self.sixaxis_handler.update_six_axis_state();
        self.nfc_handler.update_nfc_state();
        self.ir_sensor_handler.update_ir_sensor_state();
        self.mcu_handler.update_mcu_state();
        self.palma_handler.update_palma_state();
        self.battery_handler.update_battery_state();
        self.button_handler.enable_center_clamp();

        self.interface_type = self.properties_handler.get_interface_type();

        self.properties_handler.update_all_device_properties();
        self.battery_handler.update_core_battery_state();
        self.button_handler.update_core_battery_state();
    }

    pub fn update_pad_state(&mut self) {
        self.button_handler.update_all_button_lifo();
        self.sixaxis_handler.update_six_axis_state();
        self.battery_handler.update_core_battery_state();
    }

    pub fn enable_applet_to_get_input(&mut self, aruid: u64) {
        self.button_handler.update_button_state(aruid);
        self.sixaxis_handler.update_six_axis_state_for_aruid(aruid);
    }

    pub fn properties_handler(&self) -> &NpadAbstractPropertiesHandler {
        &self.properties_handler
    }

    pub fn properties_handler_mut(&mut self) -> &mut NpadAbstractPropertiesHandler {
        &mut self.properties_handler
    }
}

pub type FullAbstractPad = [AbstractPad; MAX_SUPPORTED_NPAD_ID_TYPES];
