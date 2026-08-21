// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/errors.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_CANCELLED_BY_USER: ResultCode =
    ResultCode::from_module_description(ErrorModule::Account, 1);
pub const RESULT_NO_NOTIFICATIONS: ResultCode =
    ResultCode::from_module_description(ErrorModule::Account, 15);
pub const RESULT_INVALID_USER_ID: ResultCode =
    ResultCode::from_module_description(ErrorModule::Account, 20);
pub const RESULT_INVALID_APPLICATION: ResultCode =
    ResultCode::from_module_description(ErrorModule::Account, 22);
pub const RESULT_NULLPTR: ResultCode =
    ResultCode::from_module_description(ErrorModule::Account, 30);
pub const RESULT_INVALID_ARRAY_LENGTH: ResultCode =
    ResultCode::from_module_description(ErrorModule::Account, 32);
pub const RESULT_APPLICATION_INFO_ALREADY_INITIALIZED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Account, 41);
pub const RESULT_ACCOUNT_UPDATE_FAILED: ResultCode =
    ResultCode::from_module_description(ErrorModule::Account, 100);
