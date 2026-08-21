// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/ldn_results.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_ADVERTISE_DATA_TOO_LARGE: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 10);
pub const RESULT_AUTHENTICATION_FAILED: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 20);
pub const RESULT_DISABLED: ResultCode = ResultCode::from_module_description(ErrorModule::LDN, 22);
pub const RESULT_AIRPLANE_MODE_ENABLED: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 23);
pub const RESULT_INVALID_NODE_COUNT: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 30);
pub const RESULT_CONNECTION_FAILED: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 31);
pub const RESULT_BAD_STATE: ResultCode = ResultCode::from_module_description(ErrorModule::LDN, 32);
pub const RESULT_NO_IP_ADDRESS: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 33);
pub const RESULT_INVALID_BUFFER_COUNT: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 50);
pub const RESULT_ACCESS_POINT_CONNECTION_FAILED: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 65);
pub const RESULT_AUTHENTICATION_TIMEOUT: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 66);
pub const RESULT_MAXIMUM_NODE_COUNT: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 67);
pub const RESULT_BAD_INPUT: ResultCode = ResultCode::from_module_description(ErrorModule::LDN, 96);
pub const RESULT_LOCAL_COMMUNICATION_ID_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 97);
pub const RESULT_LOCAL_COMMUNICATION_VERSION_TOO_LOW: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 113);
pub const RESULT_LOCAL_COMMUNICATION_VERSION_TOO_HIGH: ResultCode =
    ResultCode::from_module_description(ErrorModule::LDN, 114);
