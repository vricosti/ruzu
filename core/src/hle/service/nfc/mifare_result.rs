// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/mifare_result.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_DEVICE_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFCMifare, 64);
pub const RESULT_INVALID_ARGUMENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFCMifare, 65);
pub const RESULT_WRONG_DEVICE_STATE: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFCMifare, 73);
pub const RESULT_NFC_DISABLED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFCMifare, 80);
pub const RESULT_TAG_REMOVED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFCMifare, 97);
pub const RESULT_NOT_A_MIFARE: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFCMifare, 288);
