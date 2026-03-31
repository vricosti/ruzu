// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ro/ro_results.h
//!
//! Result codes for the RO service.

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_OUT_OF_ADDRESS_SPACE: ResultCode =
    ResultCode::from_module_description(ErrorModule::RO, 2);
pub const RESULT_ALREADY_LOADED: ResultCode =
    ResultCode::from_module_description(ErrorModule::RO, 3);
pub const RESULT_INVALID_NRO: ResultCode = ResultCode::from_module_description(ErrorModule::RO, 4);
pub const RESULT_INVALID_NRR: ResultCode = ResultCode::from_module_description(ErrorModule::RO, 6);
pub const RESULT_TOO_MANY_NRO: ResultCode = ResultCode::from_module_description(ErrorModule::RO, 7);
pub const RESULT_TOO_MANY_NRR: ResultCode = ResultCode::from_module_description(ErrorModule::RO, 8);
pub const RESULT_NOT_AUTHORIZED: ResultCode =
    ResultCode::from_module_description(ErrorModule::RO, 9);
pub const RESULT_INVALID_NRR_KIND: ResultCode =
    ResultCode::from_module_description(ErrorModule::RO, 10);
pub const RESULT_INTERNAL_ERROR: ResultCode =
    ResultCode::from_module_description(ErrorModule::RO, 1023);
pub const RESULT_INVALID_ADDRESS: ResultCode =
    ResultCode::from_module_description(ErrorModule::RO, 1025);
pub const RESULT_INVALID_SIZE: ResultCode =
    ResultCode::from_module_description(ErrorModule::RO, 1026);
pub const RESULT_NOT_LOADED: ResultCode =
    ResultCode::from_module_description(ErrorModule::RO, 1028);
pub const RESULT_NOT_REGISTERED: ResultCode =
    ResultCode::from_module_description(ErrorModule::RO, 1029);
pub const RESULT_INVALID_SESSION: ResultCode =
    ResultCode::from_module_description(ErrorModule::RO, 1030);
pub const RESULT_INVALID_PROCESS: ResultCode =
    ResultCode::from_module_description(ErrorModule::RO, 1031);
