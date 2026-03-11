// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/nfc_result.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_DEVICE_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 64);
pub const RESULT_INVALID_ARGUMENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 65);
pub const RESULT_WRONG_APPLICATION_AREA_SIZE: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 68);
pub const RESULT_WRONG_DEVICE_STATE: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 73);
pub const RESULT_UNKNOWN_74: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 74);
pub const RESULT_UNKNOWN_76: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 76);
pub const RESULT_NFC_NOT_INITIALIZED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 77);
pub const RESULT_NFC_DISABLED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 80);
pub const RESULT_WRITE_AMIIBO_FAILED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 88);
pub const RESULT_TAG_REMOVED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 97);
pub const RESULT_UNKNOWN_112: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 112);
pub const RESULT_UNABLE_TO_ACCESS_BACKUP_FILE: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 113);
pub const RESULT_UNKNOWN_114: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 114);
pub const RESULT_UNKNOWN_115: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 115);
pub const RESULT_REGISTRATION_IS_NOT_INITIALIZED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 120);
pub const RESULT_APPLICATION_AREA_IS_NOT_INITIALIZED: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 128);
pub const RESULT_CORRUPTED_DATA_WITH_BACKUP: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 136);
pub const RESULT_CORRUPTED_DATA: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 144);
pub const RESULT_WRONG_APPLICATION_AREA_ID: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 152);
pub const RESULT_APPLICATION_AREA_EXIST: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 168);
pub const RESULT_INVALID_TAG_TYPE: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 178);
pub const RESULT_BACKUP_PATH_ALREADY_EXIST: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 216);
pub const RESULT_MIFARE_ERROR_288: ResultCode =
    ResultCode::from_module_description(ErrorModule::NFC, 288);
