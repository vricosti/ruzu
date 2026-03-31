// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_properties_handler.h and abstract_properties_handler.cpp

use common::ResultCode;

use crate::hid_result;
use crate::hid_types::*;
use crate::hid_util;
use crate::resources::npad::npad_types::*;

/// Handles Npad properties request from HID interfaces
pub struct NpadAbstractPropertiesHandler {
    npad_id_type: NpadIdType,
    ref_counter: i32,
    device_type: DeviceIndex,
    applet_ui_type: AppletDetailedUiType,
    applet_ui_attributes: AppletFooterUiAttributes,
    is_vertical: bool,
    is_horizontal: bool,
    use_plus: bool,
    use_minus: bool,
    has_directional_buttons: bool,
    fullkey_color: ColorProperties,
    left_color: ColorProperties,
    right_color: ColorProperties,
}

impl Default for NpadAbstractPropertiesHandler {
    fn default() -> Self {
        Self {
            npad_id_type: NpadIdType::Invalid,
            ref_counter: 0,
            device_type: DeviceIndex::None,
            applet_ui_type: AppletDetailedUiType::default(),
            applet_ui_attributes: AppletFooterUiAttributes::default(),
            is_vertical: false,
            is_horizontal: false,
            use_plus: false,
            use_minus: false,
            has_directional_buttons: false,
            fullkey_color: ColorProperties::default(),
            left_color: ColorProperties::default(),
            right_color: ColorProperties::default(),
        }
    }
}

impl NpadAbstractPropertiesHandler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_npad_id(&mut self, npad_id: NpadIdType) {
        if !hid_util::is_npad_id_valid(npad_id) {
            // Upstream asserts here
            return;
        }
        self.npad_id_type = npad_id;
    }

    pub fn get_npad_id(&self) -> NpadIdType {
        self.npad_id_type
    }

    pub fn increment_ref_counter(&mut self) -> ResultCode {
        if self.ref_counter == i32::MAX - 1 {
            return hid_result::RESULT_NPAD_HANDLER_OVERFLOW;
        }

        if self.ref_counter != 0 {
            self.ref_counter += 1;
            return ResultCode::SUCCESS;
        }

        // First activation: initialize shared memory npad entries
        // Upstream resets all lifo buffer_counts, style_tag, assignment_mode, colors,
        // system_properties, button_properties, device_type, battery levels,
        // applet_footer_type, lark types, and sixaxis properties for all aruid indices.

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

    pub fn update_device_type(&mut self) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    pub fn update_device_color(&mut self) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    pub fn update_footer_attributes(&mut self) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    pub fn update_all_device_properties(&mut self) {
        // Upstream iterates over all aruid data entries and calls
        // UpdateDeviceProperties for each assigned entry
    }

    pub fn get_fullkey_interface_type(&self) -> NpadInterfaceType {
        // Upstream iterates abstract pads looking for connected Fullkey type
        NpadInterfaceType::None
    }

    pub fn get_interface_type(&self) -> NpadInterfaceType {
        // Upstream iterates abstract pads looking for connected pad with identification_code
        NpadInterfaceType::None
    }

    pub fn get_style_set(&self, _aruid: u64) -> NpadStyleSet {
        // Upstream TODO: not yet implemented in C++ upstream
        NpadStyleSet::NONE
    }

    pub fn get_applet_footer_ui_type(&self) -> AppletFooterUiType {
        self.applet_ui_type.footer
    }

    pub fn get_applet_detailed_ui_type(&self) -> AppletDetailedUiType {
        self.applet_ui_type
    }

    pub fn get_npad_interface_type(&self) -> NpadInterfaceType {
        // Upstream iterates abstract pads looking for connected pad
        NpadInterfaceType::None
    }

    pub fn get_npad_full_key_grip_color(&self) -> Result<(NpadColor, NpadColor), ResultCode> {
        if self.applet_ui_type.footer != AppletFooterUiType::SwitchProController {
            return Err(hid_result::RESULT_NPAD_IS_NOT_PRO_CONTROLLER);
        }
        // Upstream iterates abstract pads looking for connected pad
        Err(hid_result::RESULT_NPAD_IS_NOT_PRO_CONTROLLER)
    }

    pub fn get_npad_left_right_interface_type(&self) -> (NpadInterfaceType, NpadInterfaceType) {
        // Upstream iterates abstract pads checking assignment_style
        (NpadInterfaceType::None, NpadInterfaceType::None)
    }
}
