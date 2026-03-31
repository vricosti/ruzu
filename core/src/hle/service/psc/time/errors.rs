// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/errors.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_PERMISSION_DENIED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Time, 1);
pub const RESULT_CLOCK_MISMATCH: ResultCode =
    ResultCode::from_module_description(ErrorModule::Time, 102);
pub const RESULT_CLOCK_UNINITIALIZED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Time, 103);
pub const RESULT_TIME_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::Time, 200);
pub const RESULT_OVERFLOW: ResultCode = ResultCode::from_module_description(ErrorModule::Time, 201);
pub const RESULT_FAILED: ResultCode = ResultCode::from_module_description(ErrorModule::Time, 801);
pub const RESULT_INVALID_ARGUMENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::Time, 901);
pub const RESULT_TIME_ZONE_OUT_OF_RANGE: ResultCode =
    ResultCode::from_module_description(ErrorModule::Time, 902);
pub const RESULT_TIME_ZONE_PARSE_FAILED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Time, 903);
pub const RESULT_RTC_TIMEOUT: ResultCode =
    ResultCode::from_module_description(ErrorModule::Time, 988);
pub const RESULT_TIME_ZONE_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::Time, 989);
pub const RESULT_NOT_IMPLEMENTED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Time, 990);
pub const RESULT_ALARM_NOT_REGISTERED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Time, 1502);
