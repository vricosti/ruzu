// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_result.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_WORK_MEMORY_ERROR: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 3);
pub const RESULT_UNKNOWN_5: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 5);
pub const RESULT_UNKNOWN_6: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 6);
pub const RESULT_UNKNOWN_7: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 7);
pub const RESULT_OUT_OF_RANGE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 8);
pub const RESULT_INVALID_TIMESTAMP: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 12);
pub const RESULT_INVALID_STORAGE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 13);
pub const RESULT_INVALID_FILE_CONTENTS: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 14);
pub const RESULT_IS_NOT_MOUNTED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 21);
pub const RESULT_UNKNOWN_22: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 22);
pub const RESULT_FILE_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 23);
pub const RESULT_INVALID_FILE_DATA: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 24);
pub const RESULT_UNKNOWN_25: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 25);
pub const RESULT_READ_BUFFER_SHORTAGE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 30);
pub const RESULT_UNKNOWN_810: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 810);
pub const RESULT_UNKNOWN_1024: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 1024);
pub const RESULT_UNKNOWN_1202: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 1202);
pub const RESULT_UNKNOWN_1203: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 1203);
pub const RESULT_FILE_COUNT_LIMIT: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 1401);
pub const RESULT_UNKNOWN_1701: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 1701);
pub const RESULT_UNKNOWN_1801: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 1801);
pub const RESULT_UNKNOWN_1802: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 1802);
pub const RESULT_UNKNOWN_1803: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 1803);
pub const RESULT_UNKNOWN_1804: ResultCode =
    ResultCode::from_module_description(ErrorModule::Capture, 1804);
