// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/hid_util.h

use crate::hid_result::*;
use crate::hid_types::*;
use common::ResultCode;

pub fn is_npad_id_valid(npad_id: NpadIdType) -> bool {
    matches!(
        npad_id,
        NpadIdType::Player1
            | NpadIdType::Player2
            | NpadIdType::Player3
            | NpadIdType::Player4
            | NpadIdType::Player5
            | NpadIdType::Player6
            | NpadIdType::Player7
            | NpadIdType::Player8
            | NpadIdType::Other
            | NpadIdType::Handheld
    )
}

pub fn is_sixaxis_handle_valid(handle: &SixAxisSensorHandle) -> ResultCode {
    // Safety: transmute u8 to NpadIdType for validation
    let npad_id_valid =
        is_npad_id_valid(unsafe { std::mem::transmute::<u32, NpadIdType>(handle.npad_id as u32) });
    let device_index_valid = (handle.device_index as u8) < (DeviceIndex::MaxDeviceIndex as u8);

    if !npad_id_valid {
        return RESULT_INVALID_NPAD_ID;
    }
    if !device_index_valid {
        return NPAD_DEVICE_INDEX_OUT_OF_RANGE;
    }

    ResultCode::SUCCESS
}

pub fn is_vibration_handle_valid(handle: &VibrationDeviceHandle) -> ResultCode {
    match handle.npad_type {
        NpadStyleIndex::Fullkey
        | NpadStyleIndex::Handheld
        | NpadStyleIndex::JoyconDual
        | NpadStyleIndex::JoyconLeft
        | NpadStyleIndex::JoyconRight
        | NpadStyleIndex::GameCube
        | NpadStyleIndex::N64
        | NpadStyleIndex::SystemExt
        | NpadStyleIndex::System => {}
        _ => return RESULT_VIBRATION_INVALID_STYLE_INDEX,
    }

    if !is_npad_id_valid(unsafe { std::mem::transmute::<u32, NpadIdType>(handle.npad_id as u32) }) {
        return RESULT_VIBRATION_INVALID_NPAD_ID;
    }

    if (handle.device_index as u8) >= (DeviceIndex::MaxDeviceIndex as u8) {
        return RESULT_VIBRATION_DEVICE_INDEX_OUT_OF_RANGE;
    }

    ResultCode::SUCCESS
}

/// Converts a NpadIdType to an array index.
pub fn npad_id_type_to_index(npad_id_type: NpadIdType) -> usize {
    match npad_id_type {
        NpadIdType::Player1 => 0,
        NpadIdType::Player2 => 1,
        NpadIdType::Player3 => 2,
        NpadIdType::Player4 => 3,
        NpadIdType::Player5 => 4,
        NpadIdType::Player6 => 5,
        NpadIdType::Player7 => 6,
        NpadIdType::Player8 => 7,
        NpadIdType::Handheld => 8,
        NpadIdType::Other => 9,
        _ => 8,
    }
}

/// Converts an array index to a NpadIdType.
pub fn index_to_npad_id_type(index: usize) -> NpadIdType {
    match index {
        0 => NpadIdType::Player1,
        1 => NpadIdType::Player2,
        2 => NpadIdType::Player3,
        3 => NpadIdType::Player4,
        4 => NpadIdType::Player5,
        5 => NpadIdType::Player6,
        6 => NpadIdType::Player7,
        7 => NpadIdType::Player8,
        8 => NpadIdType::Handheld,
        9 => NpadIdType::Other,
        _ => NpadIdType::Invalid,
    }
}

pub fn get_styleset_by_index(index: usize) -> NpadStyleSet {
    match index {
        0 => NpadStyleSet::FULLKEY,
        1 => NpadStyleSet::HANDHELD,
        2 => NpadStyleSet::JOY_DUAL,
        3 => NpadStyleSet::JOY_LEFT,
        4 => NpadStyleSet::JOY_RIGHT,
        5 => NpadStyleSet::PALMA,
        _ => NpadStyleSet::NONE,
    }
}
