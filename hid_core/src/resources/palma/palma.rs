// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/palma/palma.h and palma.cpp

use crate::hid_result::{INVALID_PALMA_HANDLE, PALMA_RESULT_SUCCESS};
use crate::hid_types::NpadIdType;
use crate::resources::controller_base::ControllerActivation;
use common::ResultCode;

pub type PalmaOperationData = [u8; 0x140];
pub type PalmaApplicationSection = [u8; 0x100];
pub type Address = [u8; 0x6];

/// This is nn::hid::PalmaOperationType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u64)]
pub enum PalmaOperationType {
    #[default]
    PlayActivity = 0,
    SetFrModeType = 1,
    ReadStep = 2,
    EnableStep = 3,
    ResetStep = 4,
    ReadApplicationSection = 5,
    WriteApplicationSection = 6,
    ReadUniqueCode = 7,
    SetUniqueCodeInvalid = 8,
    WriteActivityEntry = 9,
    WriteRgbLedPatternEntry = 10,
    WriteWaveEntry = 11,
    ReadDataBaseIdentificationVersion = 12,
    WriteDataBaseIdentificationVersion = 13,
    SuspendFeature = 14,
    ReadPlayLog = 15,
    ResetPlayLog = 16,
}

/// This is nn::hid::PackedPalmaOperationType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum PackedPalmaOperationType {
    #[default]
    PlayActivity = 0,
    SetFrModeType = 1,
    ReadStep = 2,
    EnableStep = 3,
    ResetStep = 4,
    ReadApplicationSection = 5,
    WriteApplicationSection = 6,
    ReadUniqueCode = 7,
    SetUniqueCodeInvalid = 8,
    WriteActivityEntry = 9,
    WriteRgbLedPatternEntry = 10,
    WriteWaveEntry = 11,
    ReadDataBaseIdentificationVersion = 12,
    WriteDataBaseIdentificationVersion = 13,
    SuspendFeature = 14,
    ReadPlayLog = 15,
    ResetPlayLog = 16,
}

/// This is nn::hid::PalmaWaveSet
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u64)]
pub enum PalmaWaveSet {
    #[default]
    Small = 0,
    Medium = 1,
    Large = 2,
}

/// This is nn::hid::PalmaFrModeType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u64)]
pub enum PalmaFrModeType {
    #[default]
    Off = 0,
    B01 = 1,
    B02 = 2,
    B03 = 3,
    Downloaded = 4,
}

/// This is nn::hid::PalmaFeature
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u64)]
pub enum PalmaFeature {
    #[default]
    FrMode = 0,
    RumbleFeedback = 1,
    Step = 2,
    MuteSwitch = 3,
}

/// This is nn::hid::PalmaOperationInfo
#[derive(Debug, Clone)]
pub struct PalmaOperationInfo {
    pub operation: PackedPalmaOperationType,
    pub result: ResultCode,
    pub data: PalmaOperationData,
}

impl Default for PalmaOperationInfo {
    fn default() -> Self {
        Self {
            operation: PackedPalmaOperationType::default(),
            result: PALMA_RESULT_SUCCESS,
            data: [0u8; 0x140],
        }
    }
}

/// This is nn::hid::PalmaActivityEntry
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct PalmaActivityEntry {
    pub rgb_led_pattern_index: u32,
    pub _padding: [u8; 2],
    pub wave_set: u16, // PalmaWaveSet packed
    pub wave_index: u32,
    pub _reserved: [u8; 12],
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C, align(8))]
pub struct PalmaConnectionHandle {
    pub npad_id: NpadIdType,
}

/// Palma controller (Poke Ball Plus) — manages Palma operations, connection
/// handles, and operation events.
pub struct Palma {
    pub activation: ControllerActivation,
    is_connectable: bool,
    database_id_version: i32,
    operation: PalmaOperationInfo,
    fr_mode: PalmaFrModeType,
    active_handle: PalmaConnectionHandle,
}

impl Palma {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
            is_connectable: false,
            database_id_version: 0,
            operation: PalmaOperationInfo::default(),
            fr_mode: PalmaFrModeType::Off,
            active_handle: PalmaConnectionHandle::default(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    pub fn on_update(&mut self) {
        if !self.activation.is_controller_activated() {
            return;
        }
    }

    pub fn get_palma_connection_handle(
        &mut self,
        npad_id: NpadIdType,
    ) -> Result<PalmaConnectionHandle, ResultCode> {
        self.active_handle.npad_id = npad_id;
        Ok(self.active_handle)
    }

    pub fn initialize_palma(&mut self, handle: &PalmaConnectionHandle) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        self.activation.activate();
        ResultCode::SUCCESS
    }

    pub fn get_palma_operation_info(
        &self,
        handle: &PalmaConnectionHandle,
    ) -> Result<(PalmaOperationType, PalmaOperationData), ResultCode> {
        if handle.npad_id != self.active_handle.npad_id {
            return Err(INVALID_PALMA_HANDLE);
        }
        let op_type = unsafe {
            std::mem::transmute::<u64, PalmaOperationType>(self.operation.operation as u64)
        };
        Ok((op_type, self.operation.data))
    }

    pub fn play_palma_activity(
        &mut self,
        handle: &PalmaConnectionHandle,
        _palma_activity: u64,
    ) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        self.operation.operation = PackedPalmaOperationType::PlayActivity;
        self.operation.result = PALMA_RESULT_SUCCESS;
        self.operation.data = [0u8; 0x140];
        // Upstream: operation_complete_event->Signal()
        ResultCode::SUCCESS
    }

    pub fn set_palma_fr_mode_type(
        &mut self,
        handle: &PalmaConnectionHandle,
        fr_mode: PalmaFrModeType,
    ) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        self.fr_mode = fr_mode;
        ResultCode::SUCCESS
    }

    pub fn read_palma_step(&mut self, handle: &PalmaConnectionHandle) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        self.operation.operation = PackedPalmaOperationType::ReadStep;
        self.operation.result = PALMA_RESULT_SUCCESS;
        self.operation.data = [0u8; 0x140];
        ResultCode::SUCCESS
    }

    pub fn enable_palma_step(
        &mut self,
        handle: &PalmaConnectionHandle,
        _is_enabled: bool,
    ) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        ResultCode::SUCCESS
    }

    pub fn reset_palma_step(&mut self, handle: &PalmaConnectionHandle) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        ResultCode::SUCCESS
    }

    pub fn read_palma_unique_code(&mut self, handle: &PalmaConnectionHandle) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        self.operation.operation = PackedPalmaOperationType::ReadUniqueCode;
        self.operation.result = PALMA_RESULT_SUCCESS;
        self.operation.data = [0u8; 0x140];
        ResultCode::SUCCESS
    }

    pub fn set_palma_unique_code_invalid(&mut self, handle: &PalmaConnectionHandle) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        self.operation.operation = PackedPalmaOperationType::SetUniqueCodeInvalid;
        self.operation.result = PALMA_RESULT_SUCCESS;
        self.operation.data = [0u8; 0x140];
        ResultCode::SUCCESS
    }

    pub fn write_palma_rgb_led_pattern_entry(
        &mut self,
        handle: &PalmaConnectionHandle,
        _unknown: u64,
    ) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        self.operation.operation = PackedPalmaOperationType::WriteRgbLedPatternEntry;
        self.operation.result = PALMA_RESULT_SUCCESS;
        self.operation.data = [0u8; 0x140];
        ResultCode::SUCCESS
    }

    pub fn write_palma_wave_entry(
        &mut self,
        handle: &PalmaConnectionHandle,
        _wave: PalmaWaveSet,
        _t_mem: u64,
        _size: u64,
    ) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        self.operation.operation = PackedPalmaOperationType::WriteWaveEntry;
        self.operation.result = PALMA_RESULT_SUCCESS;
        self.operation.data = [0u8; 0x140];
        ResultCode::SUCCESS
    }

    pub fn set_palma_data_base_identification_version(
        &mut self,
        handle: &PalmaConnectionHandle,
        database_id_version: i32,
    ) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        self.database_id_version = database_id_version;
        self.operation.operation = PackedPalmaOperationType::ReadDataBaseIdentificationVersion;
        self.operation.result = PALMA_RESULT_SUCCESS;
        self.operation.data = [0u8; 0x140];
        ResultCode::SUCCESS
    }

    pub fn get_palma_data_base_identification_version(
        &mut self,
        handle: &PalmaConnectionHandle,
    ) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        self.operation.operation = PackedPalmaOperationType::ReadDataBaseIdentificationVersion;
        self.operation.result = PALMA_RESULT_SUCCESS;
        self.operation.data = [0u8; 0x140];
        self.operation.data[0] = self.database_id_version as u8;
        ResultCode::SUCCESS
    }

    pub fn get_palma_operation_result(
        &self,
        handle: &PalmaConnectionHandle,
    ) -> Result<ResultCode, ResultCode> {
        if handle.npad_id != self.active_handle.npad_id {
            return Err(INVALID_PALMA_HANDLE);
        }
        Ok(self.operation.result)
    }

    pub fn set_is_palma_all_connectable(&mut self, is_all_connectable: bool) {
        self.is_connectable = is_all_connectable;
    }

    pub fn pair_palma(&mut self, handle: &PalmaConnectionHandle) -> ResultCode {
        if handle.npad_id != self.active_handle.npad_id {
            return INVALID_PALMA_HANDLE;
        }
        ResultCode::SUCCESS
    }

    pub fn set_palma_boost_mode(&mut self, _boost_mode: bool) {}
}

impl Default for Palma {
    fn default() -> Self {
        Self::new()
    }
}
