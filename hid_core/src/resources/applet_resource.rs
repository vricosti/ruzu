// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/applet_resource.h and applet_resource.cpp

use common::ResultCode;

use crate::hid_result;

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

    pub fn set_is_initialized(&mut self, value: bool) { self.set_bit(0, value); }
    pub fn set_is_assigned(&mut self, value: bool) { self.set_bit(1, value); }
    pub fn set_enable_pad_input(&mut self, value: bool) { self.set_bit(16, value); }
    pub fn set_enable_six_axis_sensor(&mut self, value: bool) { self.set_bit(17, value); }
    pub fn set_bit_18(&mut self, value: bool) { self.set_bit(18, value); }
    pub fn set_is_palma_connectable(&mut self, value: bool) { self.set_bit(19, value); }
    pub fn set_enable_palma_boost_mode(&mut self, value: bool) { self.set_bit(20, value); }
    pub fn set_enable_touchscreen(&mut self, value: bool) { self.set_bit(21, value); }
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
    // Note: shared_memory_format pointer is not directly portable;
    // handled by SharedMemoryHolder instead.
}

pub struct HandheldConfig {
    pub is_handheld_hid_enabled: bool,
    pub is_force_handheld: bool,
    pub is_joycon_rail_enabled: bool,
    pub is_force_handheld_style_vibration: bool,
}

pub struct AppletResource {
    active_aruid: u64,
    registration_list: AruidRegisterList,
    data: [AruidData; ARUID_INDEX_MAX],
    ref_counter: i32,
    active_vibration_aruid: u64,
}

impl AppletResource {
    pub fn new() -> Self {
        Self {
            active_aruid: 0,
            registration_list: AruidRegisterList::default(),
            data: std::array::from_fn(|_| AruidData::default()),
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

        // Note: SharedMemoryHolder initialization would happen here in the full port.
        // For now we mark as assigned since we don't have the kernel shared memory.

        self.data[index].flag.set_is_assigned(true);
        // TODO: InitializeSixAxisControllerConfig(false);
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
            // TODO: Don't Handle pending delete here
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
            aruid_data.flag.set_is_assigned(false);
            // shared_memory_holder[index].finalize() would happen here
        }
    }

    pub fn get_active_aruid(&self) -> u64 {
        self.active_aruid
    }

    pub fn get_aruid_data(&self, aruid: u64) -> Option<&AruidData> {
        let aruid_index = self.get_index_from_aruid(aruid);
        if aruid_index == ARUID_INDEX_MAX {
            return None;
        }
        Some(&self.data[aruid_index])
    }

    pub fn get_aruid_data_by_index(&self, aruid_index: usize) -> &AruidData {
        &self.data[aruid_index]
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
        // TODO
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

        self.data[index].flag.set_is_palma_connectable(is_connectable);
    }

    pub fn enable_palma_boost_mode(&mut self, aruid: u64, is_enabled: bool) {
        let index = self.get_index_from_aruid(aruid);
        if index >= ARUID_INDEX_MAX {
            return;
        }

        self.data[index].flag.set_enable_palma_boost_mode(is_enabled);
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
