// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_battery_handler.h and abstract_battery_handler.cpp

use std::sync::Arc;

use common::ResultCode;
use parking_lot::Mutex;

use crate::hid_result;
use crate::hid_types::*;
use crate::hid_util;
use crate::resources::abstracted_pad::abstract_pad_holder::{
    AbstractPadRef, NpadAbstractedPadHolder,
};
use crate::resources::abstracted_pad::abstract_properties_handler::NpadAbstractPropertiesHandler;
use crate::resources::applet_resource::AppletResource;

/// Handles Npad battery request from HID interfaces
pub struct NpadAbstractBatteryHandler {
    applet_resource: Option<Arc<Mutex<AppletResource>>>,
    abstract_pad_holder: Option<Arc<Mutex<NpadAbstractedPadHolder>>>,
    properties_handler: Option<Arc<Mutex<NpadAbstractPropertiesHandler>>>,
    ref_counter: i32,
    dual_battery: NpadPowerInfo,
    left_battery: NpadPowerInfo,
    right_battery: NpadPowerInfo,
    has_new_battery_data: bool,
}

impl Default for NpadAbstractBatteryHandler {
    fn default() -> Self {
        Self {
            applet_resource: None,
            abstract_pad_holder: None,
            properties_handler: None,
            ref_counter: 0,
            dual_battery: NpadPowerInfo::default(),
            left_battery: NpadPowerInfo::default(),
            right_battery: NpadPowerInfo::default(),
            has_new_battery_data: false,
        }
    }
}

impl NpadAbstractBatteryHandler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_externals(
        &mut self,
        applet_resource: Option<Arc<Mutex<AppletResource>>>,
        abstract_pad_holder: Arc<Mutex<NpadAbstractedPadHolder>>,
        properties_handler: Arc<Mutex<NpadAbstractPropertiesHandler>>,
    ) {
        self.applet_resource = applet_resource;
        self.abstract_pad_holder = Some(abstract_pad_holder);
        self.properties_handler = Some(properties_handler);
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

    pub fn update_battery_state(&mut self) {
        if self.ref_counter == 0 {
            return;
        }
        self.has_new_battery_data = self.get_new_battery_state();
    }

    pub fn update_battery_state_for_aruid(&mut self, aruid: u64) -> ResultCode {
        let Some(applet_resource) = &self.applet_resource else {
            return ResultCode::SUCCESS;
        };
        let Some(properties_handler) = &self.properties_handler else {
            return ResultCode::SUCCESS;
        };
        let npad_index = hid_util::npad_id_type_to_index(properties_handler.lock().get_npad_id());
        let mut resource = applet_resource.lock();
        let Some(shared_memory) = resource.get_shared_memory_format_mut(aruid) else {
            return ResultCode::SUCCESS;
        };
        let internal_state = &mut shared_memory.npad.npad_entry[npad_index].internal_state;
        let system_properties = &mut internal_state.system_properties;
        system_properties.set_is_charging_joy_dual(self.dual_battery.is_charging);
        system_properties.set_is_powered_joy_dual(self.dual_battery.is_powered);
        system_properties.set_is_charging_joy_left(self.left_battery.is_charging);
        system_properties.set_is_powered_joy_left(self.left_battery.is_powered);
        system_properties.set_is_charging_joy_right(self.right_battery.is_charging);
        system_properties.set_is_powered_joy_right(self.right_battery.is_powered);
        internal_state.battery_level_dual = self.dual_battery.battery_level;
        internal_state.battery_level_left = self.left_battery.battery_level;
        internal_state.battery_level_right = self.right_battery.battery_level;
        ResultCode::SUCCESS
    }

    /// Checks abstracted pads for new battery data and updates internal state.
    /// Returns true if any battery data changed.
    fn get_new_battery_state(&mut self) -> bool {
        let Some(holder) = &self.abstract_pad_holder else {
            return false;
        };
        let mut new_dual_battery_state = NpadPowerInfo::default();
        let mut new_left_battery_state = NpadPowerInfo::default();
        let mut new_right_battery_state = NpadPowerInfo::default();
        let mut pads: [Option<AbstractPadRef>; 5] = std::array::from_fn(|_| None);
        let count = holder.lock().get_abstracted_pads(&mut pads) as usize;
        for abstract_pad in pads.into_iter().take(count).flatten() {
            let mut abstract_pad = abstract_pad.lock();
            if !abstract_pad.internal_flags.is_connected() {
                continue;
            }
            let power_info = abstract_pad.power_info;
            if power_info.battery_level as u32 > NpadBatteryLevel::Full as u32 {
                continue;
            }
            let style = abstract_pad.assignment_style;
            if style.is_external_assigned() || style.is_handheld_assigned() {
                new_dual_battery_state = power_info;
            }
            if style.is_external_left_assigned() || style.is_handheld_left_assigned() {
                new_left_battery_state = power_info;
            }
            if style.is_external_right_assigned() || style.is_handheld_right_assigned() {
                new_right_battery_state = power_info;
            }
            if abstract_pad.internal_flags.is_battery_low_ovln_required() {
                // Upstream rail notification handling is TODO.
                abstract_pad
                    .internal_flags
                    .set_is_battery_low_ovln_required(false);
            }
        }

        let mut has_changed = false;
        if Self::power_info_changed(self.dual_battery, new_dual_battery_state) {
            has_changed = true;
            self.dual_battery = new_dual_battery_state;
        }
        if Self::power_info_changed(self.left_battery, new_left_battery_state) {
            has_changed = true;
            self.left_battery = new_left_battery_state;
        }
        if Self::power_info_changed(self.right_battery, new_right_battery_state) {
            has_changed = true;
            self.right_battery = new_right_battery_state;
        }
        has_changed
    }

    fn power_info_changed(left: NpadPowerInfo, right: NpadPowerInfo) -> bool {
        left.battery_level != right.battery_level
            || left.is_charging != right.is_charging
            || left.is_powered != right.is_powered
    }

    pub fn update_core_battery_state(&mut self) {
        if self.ref_counter == 0 {
            return;
        }
        if !self.has_new_battery_data {
            return;
        }
        self.update_battery_state_for_aruid(0);
    }

    pub fn initialize_battery_state(&mut self, aruid: u64) {
        self.update_battery_state_for_aruid(aruid);
    }

    pub fn has_battery(&self) -> bool {
        let Some(holder) = &self.abstract_pad_holder else {
            return false;
        };
        let mut pads: [Option<AbstractPadRef>; 5] = std::array::from_fn(|_| None);
        let count = holder.lock().get_abstracted_pads(&mut pads) as usize;
        for abstract_pad in pads.into_iter().take(count).flatten() {
            let abstract_pad = abstract_pad.lock();
            if !abstract_pad.internal_flags.is_connected() {
                continue;
            }
            return abstract_pad.disabled_feature_set.has_fullkey_battery()
                || abstract_pad
                    .disabled_feature_set
                    .has_left_right_joy_battery();
        }
        false
    }

    pub fn has_left_right_battery(&self) -> (bool, bool) {
        let Some(holder) = &self.abstract_pad_holder else {
            return (false, false);
        };
        let mut has_left = false;
        let mut has_right = false;
        let mut pads: [Option<AbstractPadRef>; 5] = std::array::from_fn(|_| None);
        let count = holder.lock().get_abstracted_pads(&mut pads) as usize;
        for abstract_pad in pads.into_iter().take(count).flatten() {
            let abstract_pad = abstract_pad.lock();
            if !abstract_pad.internal_flags.is_connected() {
                continue;
            }
            if !abstract_pad.disabled_feature_set.has_fullkey_battery()
                && !abstract_pad
                    .disabled_feature_set
                    .has_left_right_joy_battery()
            {
                continue;
            }
            has_left = abstract_pad.assignment_style.is_external_left_assigned()
                || abstract_pad.assignment_style.is_handheld_left_assigned();
            has_right = abstract_pad.assignment_style.is_external_right_assigned()
                || abstract_pad.assignment_style.is_handheld_right_assigned();
        }
        (has_left, has_right)
    }

    pub fn get_dual_battery(&self) -> &NpadPowerInfo {
        &self.dual_battery
    }

    pub fn get_left_battery(&self) -> &NpadPowerInfo {
        &self.left_battery
    }

    pub fn get_right_battery(&self) -> &NpadPowerInfo {
        &self.right_battery
    }
}
