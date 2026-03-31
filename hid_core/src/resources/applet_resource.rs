// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/applet_resource.h and applet_resource.cpp

use std::sync::Arc;

use parking_lot::Mutex;

use common::ResultCode;

use crate::hid_result;
use crate::resources::shared_memory_holder::SharedMemoryHolder;

pub const ARUID_INDEX_MAX: usize = 0x20;
pub const SYSTEM_ARUID: u64 = 0;

/// This is RegistrationStatus
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum RegistrationStatus {
    #[default]
    None = 0,
    Initialized = 1,
    PendingDelete = 2,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct DataStatusFlag {
    pub raw: u32,
}

impl DataStatusFlag {
    pub fn is_initialized(&self) -> bool {
        (self.raw & (1 << 0)) != 0
    }
    pub fn is_assigned(&self) -> bool {
        (self.raw & (1 << 1)) != 0
    }
    pub fn enable_pad_input(&self) -> bool {
        (self.raw & (1 << 16)) != 0
    }
    pub fn enable_six_axis_sensor(&self) -> bool {
        (self.raw & (1 << 17)) != 0
    }
    pub fn bit_18(&self) -> bool {
        (self.raw & (1 << 18)) != 0
    }
    pub fn is_palma_connectable(&self) -> bool {
        (self.raw & (1 << 19)) != 0
    }
    pub fn enable_palma_boost_mode(&self) -> bool {
        (self.raw & (1 << 20)) != 0
    }
    pub fn enable_touchscreen(&self) -> bool {
        (self.raw & (1 << 21)) != 0
    }

    fn set_bit(&mut self, bit: u32, value: bool) {
        if value {
            self.raw |= 1 << bit;
        } else {
            self.raw &= !(1 << bit);
        }
    }

    pub fn set_is_initialized(&mut self, value: bool) {
        self.set_bit(0, value);
    }
    pub fn set_is_assigned(&mut self, value: bool) {
        self.set_bit(1, value);
    }
    pub fn set_enable_pad_input(&mut self, value: bool) {
        self.set_bit(16, value);
    }
    pub fn set_enable_six_axis_sensor(&mut self, value: bool) {
        self.set_bit(17, value);
    }
    pub fn set_bit_18(&mut self, value: bool) {
        self.set_bit(18, value);
    }
    pub fn set_is_palma_connectable(&mut self, value: bool) {
        self.set_bit(19, value);
    }
    pub fn set_enable_palma_boost_mode(&mut self, value: bool) {
        self.set_bit(20, value);
    }
    pub fn set_enable_touchscreen(&mut self, value: bool) {
        self.set_bit(21, value);
    }
}

#[derive(Debug, Clone, Default)]
pub struct AruidRegisterList {
    pub flag: [RegistrationStatus; ARUID_INDEX_MAX],
    pub aruid: [u64; ARUID_INDEX_MAX],
}

#[derive(Debug, Clone, Default)]
pub struct AruidData {
    pub flag: DataStatusFlag,
    pub aruid: u64,
    /// Index into the shared_memory_holder array — used to look up shared memory.
    /// Set to ARUID_INDEX_MAX when not assigned.
    pub shared_memory_index: usize,
}

/// Port of upstream HandheldConfig (defined in applet_resource.h).
pub struct HandheldConfig {
    pub is_handheld_hid_enabled: bool,
    pub is_force_handheld: bool,
    pub is_joycon_rail_enabled: bool,
    pub is_force_handheld_style_vibration: bool,
}

/// Port of upstream AppletResourceHolder struct.
///
/// Upstream holds shared pointers and raw pointers to various resources
/// that NPad and other components need. In Rust we use Arc<Mutex<>> for
/// the shared ownership.
pub struct AppletResourceHolder {
    pub applet_resource: Option<Arc<Mutex<AppletResource>>>,
    pub handheld_config: Option<Arc<Mutex<HandheldConfig>>>,
}

impl AppletResourceHolder {
    pub fn new() -> Self {
        Self {
            applet_resource: None,
            handheld_config: None,
        }
    }
}

impl Default for AppletResourceHolder {
    fn default() -> Self {
        Self::new()
    }
}

pub struct AppletResource {
    active_aruid: u64,
    registration_list: AruidRegisterList,
    data: [AruidData; ARUID_INDEX_MAX],
    shared_memory_holder: Vec<SharedMemoryHolder>,
    ref_counter: i32,
    active_vibration_aruid: u64,
}

impl AppletResource {
    pub fn new() -> Self {
        let mut holders = Vec::with_capacity(ARUID_INDEX_MAX);
        for _ in 0..ARUID_INDEX_MAX {
            holders.push(SharedMemoryHolder::new());
        }
        let mut data: [AruidData; ARUID_INDEX_MAX] = std::array::from_fn(|_| AruidData::default());
        for i in 0..ARUID_INDEX_MAX {
            data[i].shared_memory_index = ARUID_INDEX_MAX; // sentinel: not assigned
        }
        Self {
            active_aruid: 0,
            registration_list: AruidRegisterList::default(),
            data,
            shared_memory_holder: holders,
            ref_counter: 0,
            active_vibration_aruid: 0,
        }
    }

    pub fn create_applet_resource(&mut self, aruid: u64) -> ResultCode {
        let index = self.get_index_from_aruid(aruid);

        if index >= ARUID_INDEX_MAX {
            return hid_result::RESULT_ARUID_NOT_REGISTERED;
        }

        if self.data[index].flag.is_assigned() {
            return hid_result::RESULT_ARUID_ALREADY_REGISTERED;
        }

        let shared_memory = &mut self.shared_memory_holder[index];
        if !shared_memory.is_mapped() {
            let result = shared_memory.initialize();
            if result.is_error() {
                return result;
            }
            if shared_memory.get_address().is_none() {
                shared_memory.finalize();
                return hid_result::RESULT_SHARED_MEMORY_NOT_INITIALIZED;
            }
        }

        if let Some(shared_memory_format) = shared_memory.get_address_mut() {
            shared_memory_format.initialize();
        }

        self.data[index].shared_memory_index = index;
        self.data[index].flag.set_is_assigned(true);
        // Upstream TODO: InitializeSixAxisControllerConfig(false) — not yet implemented in C++ upstream
        self.active_aruid = aruid;
        ResultCode::SUCCESS
    }

    pub fn register_applet_resource_user_id(
        &mut self,
        aruid: u64,
        enable_input: bool,
    ) -> ResultCode {
        let index = self.get_index_from_aruid(aruid);

        if index < ARUID_INDEX_MAX {
            return hid_result::RESULT_ARUID_ALREADY_REGISTERED;
        }

        let mut data_index = ARUID_INDEX_MAX;
        for i in 0..ARUID_INDEX_MAX {
            if !self.data[i].flag.is_initialized() {
                data_index = i;
                break;
            }
        }

        if data_index == ARUID_INDEX_MAX {
            return hid_result::RESULT_ARUID_NO_AVAILABLE_ENTRIES;
        }

        let aruid_data = &mut self.data[data_index];

        aruid_data.aruid = aruid;
        aruid_data.flag.set_is_initialized(true);
        if enable_input {
            aruid_data.flag.set_enable_pad_input(true);
            aruid_data.flag.set_enable_six_axis_sensor(true);
            aruid_data.flag.set_bit_18(true);
            aruid_data.flag.set_enable_touchscreen(true);
        }

        data_index = ARUID_INDEX_MAX;
        for i in 0..ARUID_INDEX_MAX {
            if self.registration_list.flag[i] == RegistrationStatus::Initialized {
                if self.registration_list.aruid[i] != aruid {
                    continue;
                }
                data_index = i;
                break;
            }
            // Upstream TODO: Don't handle pending delete here — upstream note preserved as-is
            if self.registration_list.flag[i] == RegistrationStatus::None
                || self.registration_list.flag[i] == RegistrationStatus::PendingDelete
            {
                data_index = i;
                break;
            }
        }

        if data_index == ARUID_INDEX_MAX {
            return ResultCode::SUCCESS;
        }

        self.registration_list.flag[data_index] = RegistrationStatus::Initialized;
        self.registration_list.aruid[data_index] = aruid;

        ResultCode::SUCCESS
    }

    pub fn unregister_applet_resource_user_id(&mut self, aruid: u64) {
        let index = self.get_index_from_aruid(aruid);

        if index >= ARUID_INDEX_MAX {
            return;
        }

        self.free_applet_resource_id(aruid);
        self.destroy_seven_six_axis_transfer_memory();
        self.data[index].flag.raw = 0;
        self.data[index].aruid = 0;

        self.registration_list.flag[index] = RegistrationStatus::PendingDelete;

        for i in 0..ARUID_INDEX_MAX {
            if self.registration_list.flag[i] == RegistrationStatus::Initialized {
                self.active_aruid = self.registration_list.aruid[i];
            }
        }
    }

    pub fn free_applet_resource_id(&mut self, aruid: u64) {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return;
        }

        let aruid_data = &mut self.data[index];
        if aruid_data.flag.is_assigned() {
            aruid_data.shared_memory_index = ARUID_INDEX_MAX;
            aruid_data.flag.set_is_assigned(false);
            self.shared_memory_holder[index].finalize();
        }
    }

    pub fn get_active_aruid(&self) -> u64 {
        self.active_aruid
    }

    /// Port of upstream `AppletResource::GetSharedMemoryHandle`.
    ///
    /// Upstream returns `shared_memory_holder[index].GetHandle()`, which is a
    /// `Kernel::KSharedMemory*`. The hid_core crate cannot own kernel objects
    /// without a dependency cycle into `core`, so the Rust port returns the
    /// validated holder index instead. The `core` service layer uses that
    /// index to mirror the same shared-memory payload into a real
    /// `KSharedMemory` object and hand out the copy handle.
    pub fn get_shared_memory_handle(&self, aruid: u64) -> Result<usize, ResultCode> {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return Err(hid_result::RESULT_ARUID_NOT_REGISTERED);
        }

        Ok(index)
    }

    pub fn get_aruid_data(&self, aruid: u64) -> Option<&AruidData> {
        let aruid_index = self.get_index_from_aruid(aruid);
        if aruid_index == ARUID_INDEX_MAX {
            return None;
        }
        Some(&self.data[aruid_index])
    }

    pub fn get_aruid_data_mut(&mut self, aruid: u64) -> Option<&mut AruidData> {
        let aruid_index = self.get_index_from_aruid(aruid);
        if aruid_index == ARUID_INDEX_MAX {
            return None;
        }
        Some(&mut self.data[aruid_index])
    }

    pub fn get_aruid_data_by_index(&self, aruid_index: usize) -> &AruidData {
        &self.data[aruid_index]
    }

    pub fn get_aruid_data_by_index_mut(&mut self, aruid_index: usize) -> &mut AruidData {
        &mut self.data[aruid_index]
    }

    /// Get a reference to the SharedMemoryFormat for the given aruid.
    /// Port of upstream GetSharedMemoryFormat.
    pub fn get_shared_memory_format(
        &self,
        aruid: u64,
    ) -> Option<&super::shared_memory_format::SharedMemoryFormat> {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return None;
        }
        let smi = self.data[index].shared_memory_index;
        if smi >= ARUID_INDEX_MAX {
            return None;
        }
        self.shared_memory_holder[smi].get_address()
    }

    /// Get a mutable reference to the SharedMemoryFormat for the given aruid.
    pub fn get_shared_memory_format_mut(
        &mut self,
        aruid: u64,
    ) -> Option<&mut super::shared_memory_format::SharedMemoryFormat> {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return None;
        }
        let smi = self.data[index].shared_memory_index;
        if smi >= ARUID_INDEX_MAX {
            return None;
        }
        self.shared_memory_holder[smi].get_address_mut()
    }

    /// Get a mutable reference to the SharedMemoryFormat by aruid index.
    pub fn get_shared_memory_format_by_index_mut(
        &mut self,
        aruid_index: usize,
    ) -> Option<&mut super::shared_memory_format::SharedMemoryFormat> {
        if aruid_index >= ARUID_INDEX_MAX {
            return None;
        }
        let smi = self.data[aruid_index].shared_memory_index;
        if smi >= ARUID_INDEX_MAX {
            return None;
        }
        self.shared_memory_holder[smi].get_address_mut()
    }

    /// Get a reference to the SharedMemoryFormat by aruid index.
    pub fn get_shared_memory_format_by_index(
        &self,
        aruid_index: usize,
    ) -> Option<&super::shared_memory_format::SharedMemoryFormat> {
        if aruid_index >= ARUID_INDEX_MAX {
            return None;
        }
        let smi = self.data[aruid_index].shared_memory_index;
        if smi >= ARUID_INDEX_MAX {
            return None;
        }
        self.shared_memory_holder[smi].get_address()
    }

    pub fn is_vibration_aruid_active(&self, aruid: u64) -> bool {
        aruid == 0 || aruid == self.active_vibration_aruid
    }

    pub fn get_index_from_aruid(&self, aruid: u64) -> usize {
        for i in 0..ARUID_INDEX_MAX {
            if self.registration_list.flag[i] == RegistrationStatus::Initialized
                && self.registration_list.aruid[i] == aruid
            {
                return i;
            }
        }
        ARUID_INDEX_MAX
    }

    fn destroy_seven_six_axis_transfer_memory(&mut self) -> ResultCode {
        // Upstream TODO: not yet implemented in C++ upstream
        ResultCode::SUCCESS
    }

    pub fn enable_input(&mut self, aruid: u64, is_enabled: bool) {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return;
        }

        self.data[index].flag.set_enable_pad_input(is_enabled);
        self.data[index].flag.set_enable_touchscreen(is_enabled);
    }

    pub fn set_aruid_valid_for_vibration(&mut self, aruid: u64, is_enabled: bool) -> bool {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return false;
        }

        if !is_enabled && aruid == self.active_vibration_aruid {
            self.active_vibration_aruid = SYSTEM_ARUID;
            return true;
        }

        if is_enabled && aruid != self.active_vibration_aruid {
            self.active_vibration_aruid = aruid;
            return true;
        }

        false
    }

    pub fn enable_six_axis_sensor(&mut self, aruid: u64, is_enabled: bool) {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return;
        }

        self.data[index].flag.set_enable_six_axis_sensor(is_enabled);
    }

    pub fn enable_pad_input(&mut self, aruid: u64, is_enabled: bool) {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return;
        }

        self.data[index].flag.set_enable_pad_input(is_enabled);
    }

    pub fn enable_touch_screen(&mut self, aruid: u64, is_enabled: bool) {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return;
        }

        self.data[index].flag.set_enable_touchscreen(is_enabled);
    }

    pub fn set_is_palma_connectable(&mut self, aruid: u64, is_connectable: bool) {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return;
        }

        self.data[index]
            .flag
            .set_is_palma_connectable(is_connectable);
    }

    pub fn enable_palma_boost_mode(&mut self, aruid: u64, is_enabled: bool) {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return;
        }

        self.data[index]
            .flag
            .set_enable_palma_boost_mode(is_enabled);
    }

    pub fn register_core_applet_resource(&mut self) -> ResultCode {
        if self.ref_counter == i32::MAX - 1 {
            return hid_result::RESULT_APPLET_RESOURCE_OVERFLOW;
        }
        if self.ref_counter == 0 {
            let index = self.get_index_from_aruid(0);
            if index < ARUID_INDEX_MAX {
                return hid_result::RESULT_ARUID_ALREADY_REGISTERED;
            }

            let mut data_index = ARUID_INDEX_MAX;
            for i in 0..ARUID_INDEX_MAX {
                if !self.data[i].flag.is_initialized() {
                    data_index = i;
                    break;
                }
            }

            if data_index == ARUID_INDEX_MAX {
                return hid_result::RESULT_ARUID_NO_AVAILABLE_ENTRIES;
            }

            let aruid_data = &mut self.data[data_index];

            aruid_data.aruid = 0;
            aruid_data.flag.set_is_initialized(true);
            aruid_data.flag.set_enable_pad_input(true);
            aruid_data.flag.set_enable_six_axis_sensor(true);
            aruid_data.flag.set_bit_18(true);
            aruid_data.flag.set_enable_touchscreen(true);

            data_index = ARUID_INDEX_MAX;
            for i in 0..ARUID_INDEX_MAX {
                if self.registration_list.flag[i] == RegistrationStatus::Initialized {
                    if self.registration_list.aruid[i] != 0 {
                        continue;
                    }
                    data_index = i;
                    break;
                }
                if self.registration_list.flag[i] == RegistrationStatus::None {
                    data_index = i;
                    break;
                }
            }

            let mut result = ResultCode::SUCCESS;

            if data_index == ARUID_INDEX_MAX {
                result = self.create_applet_resource(0);
            } else {
                self.registration_list.flag[data_index] = RegistrationStatus::Initialized;
                self.registration_list.aruid[data_index] = 0;
            }

            if result.is_error() {
                self.unregister_applet_resource_user_id(0);
                return result;
            }
        }
        self.ref_counter += 1;
        ResultCode::SUCCESS
    }

    pub fn unregister_core_applet_resource(&mut self) -> ResultCode {
        if self.ref_counter == 0 {
            return hid_result::RESULT_APPLET_RESOURCE_NOT_INITIALIZED;
        }

        self.ref_counter -= 1;
        if self.ref_counter == 0 {
            self.unregister_applet_resource_user_id(0);
        }

        ResultCode::SUCCESS
    }
}

impl Default for AppletResource {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::{AppletResource, ARUID_INDEX_MAX};

    #[test]
    fn get_shared_memory_handle_returns_registered_holder_index() {
        let mut resource = AppletResource::new();

        assert!(resource
            .register_applet_resource_user_id(0x1234, true)
            .is_success());
        assert!(resource.create_applet_resource(0x1234).is_success());

        let index = resource.get_shared_memory_handle(0x1234).unwrap();
        assert!(index < ARUID_INDEX_MAX);
        assert_eq!(resource.get_shared_memory_format(0x1234).is_some(), true);
    }
}
