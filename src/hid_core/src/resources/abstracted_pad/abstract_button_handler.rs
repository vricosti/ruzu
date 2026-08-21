// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_button_handler.h and abstract_button_handler.cpp

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
use crate::resources::applet_resource::{AppletResource, ARUID_INDEX_MAX};
use crate::resources::shared_memory_format::NpadSharedMemoryEntry;

/// Handles Npad button request from HID interfaces
pub struct NpadAbstractButtonHandler {
    applet_resource: Option<Arc<Mutex<AppletResource>>>,
    abstract_pad_holder: Option<Arc<Mutex<NpadAbstractedPadHolder>>>,
    properties_handler: Option<Arc<Mutex<NpadAbstractPropertiesHandler>>>,
    ref_counter: i32,
    is_button_pressed_on_console_mode: bool,
}

impl Default for NpadAbstractButtonHandler {
    fn default() -> Self {
        Self {
            applet_resource: None,
            abstract_pad_holder: None,
            properties_handler: None,
            ref_counter: 0,
            is_button_pressed_on_console_mode: false,
        }
    }
}

impl NpadAbstractButtonHandler {
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

    pub fn update_all_button_lifo(&mut self) {
        let Some(applet_resource) = self.applet_resource.clone() else {
            return;
        };
        let Some(properties_handler) = self.properties_handler.clone() else {
            return;
        };
        let npad_id = properties_handler.lock().get_npad_id();
        let npad_index = hid_util::npad_id_type_to_index(npad_id);
        for aruid_index in 0..ARUID_INDEX_MAX {
            let mut resource = applet_resource.lock();
            let aruid = resource.get_aruid_data_by_index(aruid_index).aruid;
            let Some(shared_memory) = resource.get_shared_memory_format_by_index_mut(aruid_index)
            else {
                continue;
            };
            self.update_button_lifo(
                &mut shared_memory.npad.npad_entry[npad_index],
                aruid,
                &properties_handler,
            );
        }
    }

    pub fn update_core_battery_state(&mut self) {
        self.update_all_button_lifo();
    }

    pub fn update_button_state(&mut self, aruid: u64) {
        let Some(applet_resource) = self.applet_resource.clone() else {
            return;
        };
        let Some(properties_handler) = self.properties_handler.clone() else {
            return;
        };
        let npad_id = properties_handler.lock().get_npad_id();
        let npad_index = hid_util::npad_id_type_to_index(npad_id);
        let mut resource = applet_resource.lock();
        let Some(shared_memory) = resource.get_shared_memory_format_mut(aruid) else {
            return;
        };
        self.update_button_lifo(
            &mut shared_memory.npad.npad_entry[npad_index],
            aruid,
            &properties_handler,
        );
    }

    pub fn is_button_pressed_on_console_mode(&self) -> bool {
        self.is_button_pressed_on_console_mode
    }

    pub fn enable_center_clamp(&self) {
        let Some(holder) = &self.abstract_pad_holder else {
            return;
        };
        let mut pads: [Option<AbstractPadRef>; 5] = std::array::from_fn(|_| None);
        let count = holder.lock().get_abstracted_pads(&mut pads) as usize;
        for abstract_pad in pads.into_iter().take(count).flatten() {
            let mut abstract_pad = abstract_pad.lock();
            if abstract_pad.internal_flags.is_connected() {
                abstract_pad.internal_flags.set_use_center_clamp(true);
            }
        }
    }

    fn update_button_lifo(
        &mut self,
        shared_memory: &mut NpadSharedMemoryEntry,
        aruid: u64,
        properties_handler: &Arc<Mutex<NpadAbstractPropertiesHandler>>,
    ) {
        let style_tag = NpadStyleTag {
            raw: properties_handler.lock().get_style_set(aruid),
        };
        self.update_npad_fullkey_lifo(style_tag, 0, aruid, shared_memory);
        self.update_handheld_lifo(style_tag, 1, aruid, shared_memory);
        self.update_joycon_dual_lifo(style_tag, 2, aruid, shared_memory);
        self.update_joycon_left_lifo(style_tag, 3, aruid, shared_memory);
        self.update_joycon_right_lifo(style_tag, 4, aruid, shared_memory);
        self.update_palma_lifo(style_tag, 5, aruid, shared_memory);
        self.update_system_ext_lifo(style_tag, 6, aruid, shared_memory);
    }

    fn update_npad_fullkey_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _style_index: i32,
        _aruid: u64,
        _shared_memory: &mut NpadSharedMemoryEntry,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_handheld_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _style_index: i32,
        _aruid: u64,
        _shared_memory: &mut NpadSharedMemoryEntry,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_joycon_dual_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _style_index: i32,
        _aruid: u64,
        _shared_memory: &mut NpadSharedMemoryEntry,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_joycon_left_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _style_index: i32,
        _aruid: u64,
        _shared_memory: &mut NpadSharedMemoryEntry,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_joycon_right_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _style_index: i32,
        _aruid: u64,
        _shared_memory: &mut NpadSharedMemoryEntry,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_system_ext_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _style_index: i32,
        _aruid: u64,
        _shared_memory: &mut NpadSharedMemoryEntry,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }

    fn update_palma_lifo(
        &mut self,
        _style_tag: NpadStyleTag,
        _style_index: i32,
        _aruid: u64,
        _shared_memory: &mut NpadSharedMemoryEntry,
    ) {
        // Upstream TODO: not yet implemented in C++ upstream
    }
}
