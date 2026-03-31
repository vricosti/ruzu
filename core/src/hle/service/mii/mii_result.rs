// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii_result.h
//!
//! Result codes for the Mii service.

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_INVALID_ARGUMENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 1);
pub const RESULT_INVALID_ARGUMENT_SIZE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 2);
pub const RESULT_NOT_UPDATED: ResultCode = ResultCode::from_module_description(ErrorModule::Mii, 3);
pub const RESULT_NOT_FOUND: ResultCode = ResultCode::from_module_description(ErrorModule::Mii, 4);
pub const RESULT_DATABASE_FULL: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 5);
pub const RESULT_INVALID_CHAR_INFO: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 100);
pub const RESULT_INVALID_DATABASE_CHECKSUM: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 101);
pub const RESULT_INVALID_DATABASE_SIGNATURE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 103);
pub const RESULT_INVALID_DATABASE_VERSION: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 104);
pub const RESULT_INVALID_DATABASE_LENGTH: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 105);
pub const RESULT_INVALID_CHAR_INFO2: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 107);
pub const RESULT_INVALID_STORE_DATA: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 109);
pub const RESULT_INVALID_OPERATION: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 202);
pub const RESULT_PERMISSION_DENIED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 203);
pub const RESULT_TEST_MODE_ONLY: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 204);
pub const RESULT_INVALID_CHAR_INFO_TYPE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Mii, 205);
