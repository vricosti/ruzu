// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_pad.h and abstract_pad.cpp

use std::sync::Arc;

use common::ResultCode;
use parking_lot::Mutex;

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
use crate::resources::applet_resource::AppletResource;
use crate::resources::npad::npad_types::MAX_SUPPORTED_NPAD_ID_TYPES;

/// Handles Npad request from HID interfaces
pub struct AbstractPad {
    abstract_pad_holder: Arc<Mutex<NpadAbstractedPadHolder>>,
    properties_handler: Arc<Mutex<NpadAbstractPropertiesHandler>>,
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
        let abstract_pad_holder = Arc::new(Mutex::new(NpadAbstractedPadHolder::default()));
        let properties_handler = Arc::new(Mutex::new(NpadAbstractPropertiesHandler::default()));
        properties_handler
            .lock()
            .set_abstract_pad_holder(Arc::clone(&abstract_pad_holder));
        let mut mcu_handler = NpadAbstractMcuHandler::default();
        mcu_handler.set_properties_handler(Arc::clone(&properties_handler));
        let mut sixaxis_handler = NpadAbstractSixAxisHandler::default();
        sixaxis_handler.set_externals(None, Arc::clone(&properties_handler));
        let mut button_handler = NpadAbstractButtonHandler::default();
        button_handler.set_externals(
            None,
            Arc::clone(&abstract_pad_holder),
            Arc::clone(&properties_handler),
        );
        let mut battery_handler = NpadAbstractBatteryHandler::default();
        battery_handler.set_externals(
            None,
            Arc::clone(&abstract_pad_holder),
            Arc::clone(&properties_handler),
        );
        Self {
            abstract_pad_holder,
            properties_handler,
            led_handler: NpadAbstractLedHandler::default(),
            ir_sensor_handler: NpadAbstractIrSensorHandler::default(),
            nfc_handler: NpadAbstractNfcHandler::default(),
            mcu_handler,
            vibration_handler: NpadAbstractVibrationHandler::default(),
            sixaxis_handler,
            button_handler,
            battery_handler,
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
        self.properties_handler.lock().set_npad_id(npad_id);
    }

    pub fn set_applet_resource(&mut self, applet_resource: Option<Arc<Mutex<AppletResource>>>) {
        self.sixaxis_handler.set_externals(
            applet_resource.clone(),
            Arc::clone(&self.properties_handler),
        );
        self.button_handler.set_externals(
            applet_resource.clone(),
            Arc::clone(&self.abstract_pad_holder),
            Arc::clone(&self.properties_handler),
        );
        self.battery_handler.set_externals(
            applet_resource,
            Arc::clone(&self.abstract_pad_holder),
            Arc::clone(&self.properties_handler),
        );
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
            result = self.properties_handler.lock().increment_ref_counter();
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
            self.properties_handler.lock().decrement_ref_counter();
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
        self.properties_handler.lock().decrement_ref_counter();
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
        self.properties_handler.lock().get_npad_id()
    }

    pub fn update_interface_type(&mut self) {
        let interface_type = self.properties_handler.lock().get_interface_type();
        if self.interface_type != interface_type {
            self.update();
        }
        self.battery_handler.update_battery_state();
    }

    pub fn update(&mut self) {
        self.properties_handler.lock().update_device_type();
        let npad_id = self.properties_handler.lock().get_npad_id();
        self.led_handler.set_npad_led_handler_led_pattern(npad_id);
        self.vibration_handler.update_vibration_state();
        self.sixaxis_handler.update_six_axis_state();
        self.nfc_handler.update_nfc_state();
        self.ir_sensor_handler.update_ir_sensor_state();
        self.mcu_handler.update_mcu_state();
        self.palma_handler.update_palma_state();
        self.battery_handler.update_battery_state();
        self.button_handler.enable_center_clamp();

        self.interface_type = self.properties_handler.lock().get_interface_type();

        self.properties_handler
            .lock()
            .update_all_device_properties();
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
        self.battery_handler.update_battery_state_for_aruid(aruid);
    }

    pub fn abstract_pad_holder(&self) -> Arc<Mutex<NpadAbstractedPadHolder>> {
        Arc::clone(&self.abstract_pad_holder)
    }

    pub fn properties_handler(&self) -> Arc<Mutex<NpadAbstractPropertiesHandler>> {
        Arc::clone(&self.properties_handler)
    }

    pub fn mcu_handler(&self) -> &NpadAbstractMcuHandler {
        &self.mcu_handler
    }

    pub fn mcu_handler_mut(&mut self) -> &mut NpadAbstractMcuHandler {
        &mut self.mcu_handler
    }

    pub fn battery_handler(&self) -> &NpadAbstractBatteryHandler {
        &self.battery_handler
    }

    pub fn battery_handler_mut(&mut self) -> &mut NpadAbstractBatteryHandler {
        &mut self.battery_handler
    }
}

pub type FullAbstractPad = [AbstractPad; MAX_SUPPORTED_NPAD_ID_TYPES];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resources::abstracted_pad::abstract_mcu_handler::NpadMcuState;
    use crate::resources::npad::npad_types::{AssignmentStyle, IAbstractedPad};

    #[test]
    fn externals_share_live_pads_between_properties_and_mcu_handlers() {
        let mut abstract_pad = AbstractPad::new();
        let physical_pad = Arc::new(Mutex::new(IAbstractedPad {
            controller_id: 7,
            device_type: NpadStyleIndex::Fullkey,
            interface_type: NpadInterfaceType::Bluetooth,
            assignment_style: AssignmentStyle { raw: 1 },
            ..IAbstractedPad::default()
        }));
        {
            let mut physical_pad_state = physical_pad.lock();
            physical_pad_state.internal_flags.set_is_connected(true);
            physical_pad_state.disabled_feature_set.raw = (1 << 9) | (1 << 18) | (1 << 22);
            physical_pad_state.power_info = NpadPowerInfo {
                is_powered: true,
                is_charging: false,
                battery_level: NpadBatteryLevel::Low,
                ..NpadPowerInfo::default()
            };
        }
        assert!(abstract_pad
            .abstract_pad_holder()
            .lock()
            .register_abstract_pad(Arc::clone(&physical_pad))
            .is_success());

        assert_eq!(
            abstract_pad
                .properties_handler()
                .lock()
                .get_interface_type(),
            NpadInterfaceType::Bluetooth
        );
        abstract_pad.mcu_handler_mut().update_mcu_state();
        assert_eq!(
            abstract_pad.mcu_handler().get_mcu_state(0),
            NpadMcuState::Available
        );
        assert!(Arc::ptr_eq(
            &abstract_pad.mcu_handler().get_abstracted_pad(0).unwrap(),
            &physical_pad
        ));

        assert!(abstract_pad.activate().is_success());
        abstract_pad.battery_handler_mut().update_battery_state();
        assert_eq!(
            abstract_pad
                .battery_handler()
                .get_dual_battery()
                .battery_level,
            NpadBatteryLevel::Low
        );
        assert!(abstract_pad.battery_handler().has_battery());
    }
}
