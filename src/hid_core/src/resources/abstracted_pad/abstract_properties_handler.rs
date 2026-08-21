// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_properties_handler.h and abstract_properties_handler.cpp

use std::sync::Arc;

use common::ResultCode;
use parking_lot::Mutex;

use crate::hid_result;
use crate::hid_types::*;
use crate::hid_util;
use crate::resources::abstracted_pad::abstract_pad_holder::{
    AbstractPadRef, NpadAbstractedPadHolder,
};
use crate::resources::npad::npad_types::*;

/// Handles Npad properties request from HID interfaces
pub struct NpadAbstractPropertiesHandler {
    abstract_pad_holder: Option<Arc<Mutex<NpadAbstractedPadHolder>>>,
    npad_id_type: NpadIdType,
    ref_counter: i32,
    applet_ui_type: AppletDetailedUiType,
}

impl Default for NpadAbstractPropertiesHandler {
    fn default() -> Self {
        Self {
            abstract_pad_holder: None,
            npad_id_type: NpadIdType::Invalid,
            ref_counter: 0,
            applet_ui_type: AppletDetailedUiType::default(),
        }
    }
}

impl NpadAbstractPropertiesHandler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_abstract_pad_holder(&mut self, holder: Arc<Mutex<NpadAbstractedPadHolder>>) {
        self.abstract_pad_holder = Some(holder);
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
        for abstract_pad in self.get_abstracted_pads() {
            let abstract_pad = abstract_pad.lock();
            if !abstract_pad.internal_flags.is_connected() {
                continue;
            }
            if abstract_pad.device_type != NpadStyleIndex::Fullkey {
                continue;
            }
            if abstract_pad.interface_type as u8 >= NpadInterfaceType::Embedded as u8 {
                continue;
            }
            return abstract_pad.interface_type;
        }
        NpadInterfaceType::None
    }

    pub fn get_interface_type(&self) -> NpadInterfaceType {
        for abstract_pad in self.get_abstracted_pads() {
            let abstract_pad = abstract_pad.lock();
            if !abstract_pad.internal_flags.is_connected() {
                continue;
            }
            if !abstract_pad.disabled_feature_set.has_identification_code() {
                continue;
            }
            if abstract_pad.interface_type as u8 >= NpadInterfaceType::Embedded as u8 {
                continue;
            }
            return abstract_pad.interface_type;
        }
        NpadInterfaceType::None
    }

    pub fn get_style_set(&self, _aruid: u64) -> NpadStyleSet {
        // Upstream TODO: not yet implemented in C++ upstream
        NpadStyleSet::NONE
    }

    pub fn get_abstracted_pads_with_style_tag(&self, _style: NpadStyleTag) -> Vec<AbstractPadRef> {
        let Some(holder) = &self.abstract_pad_holder else {
            return Vec::new();
        };
        let mut pads: [Option<AbstractPadRef>; 5] = std::array::from_fn(|_| None);
        let count = holder.lock().get_abstracted_pads(&mut pads) as usize;
        pads.into_iter().take(count).flatten().collect()
    }

    pub fn get_abstracted_pads(&self) -> Vec<AbstractPadRef> {
        self.get_abstracted_pads_with_style_tag(NpadStyleTag {
            raw: self.get_style_set(0),
        })
    }

    pub fn get_applet_footer_ui_type(&self) -> AppletFooterUiType {
        self.applet_ui_type.footer
    }

    pub fn get_applet_detailed_ui_type(&self) -> AppletDetailedUiType {
        self.applet_ui_type
    }

    pub fn get_npad_interface_type(&self) -> NpadInterfaceType {
        for abstract_pad in self.get_abstracted_pads() {
            let abstract_pad = abstract_pad.lock();
            if !abstract_pad.internal_flags.is_connected() {
                continue;
            }
            if abstract_pad.interface_type as u8 >= NpadInterfaceType::Embedded as u8 {
                continue;
            }
            return abstract_pad.interface_type;
        }
        NpadInterfaceType::None
    }

    pub fn get_npad_full_key_grip_color(&self) -> Result<(NpadColor, NpadColor), ResultCode> {
        if self.applet_ui_type.footer != AppletFooterUiType::SwitchProController {
            return Err(hid_result::RESULT_NPAD_IS_NOT_PRO_CONTROLLER);
        }
        for abstract_pad in self.get_abstracted_pads() {
            if abstract_pad.lock().internal_flags.is_connected() {
                return Ok((NpadColor::default(), NpadColor::default()));
            }
        }
        Err(hid_result::RESULT_NPAD_IS_NOT_PRO_CONTROLLER)
    }

    pub fn get_npad_left_right_interface_type(&self) -> (NpadInterfaceType, NpadInterfaceType) {
        let mut left = NpadInterfaceType::None;
        let mut right = NpadInterfaceType::None;
        for abstract_pad in self.get_abstracted_pads() {
            let abstract_pad = abstract_pad.lock();
            if !abstract_pad.internal_flags.is_connected() {
                continue;
            }
            if abstract_pad.assignment_style.is_external_left_assigned()
                && abstract_pad.assignment_style.is_handheld_left_assigned()
            {
                if abstract_pad.interface_type as u8 > NpadInterfaceType::Embedded as u8 {
                    continue;
                }
                left = abstract_pad.interface_type;
                continue;
            }
            if abstract_pad.assignment_style.is_external_right_assigned()
                && abstract_pad.assignment_style.is_handheld_right_assigned()
            {
                if abstract_pad.interface_type as u8 > NpadInterfaceType::Embedded as u8 {
                    continue;
                }
                right = abstract_pad.interface_type;
            }
        }
        (left, right)
    }
}
