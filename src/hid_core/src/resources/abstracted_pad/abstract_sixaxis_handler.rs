// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_sixaxis_handler.h and abstract_sixaxis_handler.cpp

use std::sync::Arc;

use common::ResultCode;
use parking_lot::Mutex;

use crate::hid_result;
use crate::hid_types::*;
use crate::hid_util;
use crate::resources::abstracted_pad::abstract_properties_handler::NpadAbstractPropertiesHandler;
use crate::resources::applet_resource::{AppletResource, ARUID_INDEX_MAX};
use crate::resources::shared_memory_format::{NpadSharedMemoryEntry, NpadSixAxisSensorLifo};

/// Handles Npad six-axis sensor request from HID interfaces
pub struct NpadAbstractSixAxisHandler {
    applet_resource: Option<Arc<Mutex<AppletResource>>>,
    properties_handler: Option<Arc<Mutex<NpadAbstractPropertiesHandler>>>,
    ref_counter: i32,
}

impl Default for NpadAbstractSixAxisHandler {
    fn default() -> Self {
        Self {
            applet_resource: None,
            properties_handler: None,
            ref_counter: 0,
        }
    }
}

impl NpadAbstractSixAxisHandler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_externals(
        &mut self,
        applet_resource: Option<Arc<Mutex<AppletResource>>>,
        properties_handler: Arc<Mutex<NpadAbstractPropertiesHandler>>,
    ) {
        self.applet_resource = applet_resource;
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

    pub fn is_firmware_update_available(&self) -> u64 {
        // Upstream TODO: not yet implemented in C++ upstream
        0
    }

    pub fn update_six_axis_state(&mut self) -> ResultCode {
        let Some(applet_resource) = self.applet_resource.clone() else {
            return ResultCode::SUCCESS;
        };
        let Some(properties_handler) = self.properties_handler.clone() else {
            return ResultCode::SUCCESS;
        };
        let npad_id = properties_handler.lock().get_npad_id();
        let npad_index = hid_util::npad_id_type_to_index(npad_id);
        for aruid_index in 0..ARUID_INDEX_MAX {
            let mut resource = applet_resource.lock();
            let data = resource.get_aruid_data_by_index(aruid_index).clone();
            if !data.flag.is_assigned() {
                continue;
            }
            let Some(shared_memory) = resource.get_shared_memory_format_by_index_mut(aruid_index)
            else {
                continue;
            };
            self.update_sixaxis_internal_state(
                &mut shared_memory.npad.npad_entry[npad_index],
                data.aruid,
                data.flag.enable_six_axis_sensor(),
                &properties_handler,
            );
        }
        ResultCode::SUCCESS
    }

    pub fn update_six_axis_state_for_aruid(&mut self, aruid: u64) -> ResultCode {
        self.update_six_axis_state_for_aruid_impl(aruid)
    }

    fn update_six_axis_state_for_aruid_impl(&mut self, aruid: u64) -> ResultCode {
        let Some(applet_resource) = self.applet_resource.clone() else {
            return ResultCode::SUCCESS;
        };
        let Some(properties_handler) = self.properties_handler.clone() else {
            return ResultCode::SUCCESS;
        };
        let npad_id = properties_handler.lock().get_npad_id();
        let npad_index = hid_util::npad_id_type_to_index(npad_id);
        let mut resource = applet_resource.lock();
        let Some(data) = resource.get_aruid_data(aruid).cloned() else {
            return ResultCode::SUCCESS;
        };
        let Some(shared_memory) = resource.get_shared_memory_format_mut(aruid) else {
            return ResultCode::SUCCESS;
        };
        self.update_sixaxis_internal_state(
            &mut shared_memory.npad.npad_entry[npad_index],
            data.aruid,
            data.flag.enable_six_axis_sensor(),
            &properties_handler,
        );
        ResultCode::SUCCESS
    }

    pub fn update_six_axis_state2(&mut self, aruid: u64) -> ResultCode {
        self.update_six_axis_state_for_aruid_impl(aruid)
    }

    fn update_sixaxis_internal_state(
        &mut self,
        npad_entry: &mut NpadSharedMemoryEntry,
        aruid: u64,
        is_sensor_enabled: bool,
        properties_handler: &Arc<Mutex<NpadAbstractPropertiesHandler>>,
    ) {
        let style_tag = NpadStyleTag {
            raw: properties_handler.lock().get_style_set(aruid),
        };
        if !style_tag.raw.contains(NpadStyleSet::PALMA) {
            self.update_sixaxis_fullkey_lifo(
                style_tag,
                &mut npad_entry.internal_state.sixaxis_fullkey_lifo,
                is_sensor_enabled,
            );
        } else {
            self.update_six_axis_palma_lifo(
                style_tag,
                &mut npad_entry.internal_state.sixaxis_fullkey_lifo,
                is_sensor_enabled,
            );
        }
        self.update_sixaxis_handheld_lifo(
            style_tag,
            &mut npad_entry.internal_state.sixaxis_handheld_lifo,
            is_sensor_enabled,
        );
        self.update_sixaxis_dual_lifo(
            style_tag,
            &mut npad_entry.internal_state.sixaxis_dual_left_lifo,
            is_sensor_enabled,
        );
        self.update_sixaxis_dual_lifo(
            style_tag,
            &mut npad_entry.internal_state.sixaxis_dual_right_lifo,
            is_sensor_enabled,
        );
        self.update_sixaxis_left_lifo(
            style_tag,
            &mut npad_entry.internal_state.sixaxis_left_lifo,
            is_sensor_enabled,
        );
        self.update_sixaxis_right_lifo(
            style_tag,
            &mut npad_entry.internal_state.sixaxis_right_lifo,
            is_sensor_enabled,
        );
        // Upstream TODO: set six-axis properties.
    }

    fn update_sixaxis_fullkey_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _sensor_lifo: &mut NpadSixAxisSensorLifo,
        _is_sensor_enabled: bool,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_six_axis_palma_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _sensor_lifo: &mut NpadSixAxisSensorLifo,
        _is_sensor_enabled: bool,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_sixaxis_handheld_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _sensor_lifo: &mut NpadSixAxisSensorLifo,
        _is_sensor_enabled: bool,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_sixaxis_dual_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _sensor_lifo: &mut NpadSixAxisSensorLifo,
        _is_sensor_enabled: bool,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_sixaxis_left_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _sensor_lifo: &mut NpadSixAxisSensorLifo,
        _is_sensor_enabled: bool,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_sixaxis_right_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _sensor_lifo: &mut NpadSixAxisSensorLifo,
        _is_sensor_enabled: bool,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }
}
