// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfp/nfp_result.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_DEVICE_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 64);
pub const RESULT_INVALID_ARGUMENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 65);
pub const RESULT_WRONG_APPLICATION_AREA_SIZE: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 68);
pub const RESULT_WRONG_DEVICE_STATE: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 73);
// Note: upstream uses ErrorModule::NFC (not NFP) for this one
pub const RESULT_UNKNOWN_74: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 74);
pub const RESULT_NFC_DISABLED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 80);
pub const RESULT_WRITE_AMIIBO_FAILED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 88);
pub const RESULT_TAG_REMOVED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 97);
pub const RESULT_REGISTRATION_IS_NOT_INITIALIZED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 120);
pub const RESULT_APPLICATION_AREA_IS_NOT_INITIALIZED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 128);
pub const RESULT_CORRUPTED_DATA_WITH_BACKUP: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 136);
pub const RESULT_CORRUPTED_DATA: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 144);
pub const RESULT_WRONG_APPLICATION_AREA_ID: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 152);
pub const RESULT_APPLICATION_AREA_EXIST: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 168);
pub const RESULT_NOT_AN_AMIIBO: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 178);
pub const RESULT_UNABLE_TO_ACCESS_BACKUP_FILE: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFP, 200);
