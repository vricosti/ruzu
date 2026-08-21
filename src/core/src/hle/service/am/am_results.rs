// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/am_results.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_NO_DATA_IN_CHANNEL: ResultCode =
    ResultCode::from_module_description(ErrorModule::AM, 2);
pub const RESULT_NO_MESSAGES: ResultCode = ResultCode::from_module_description(ErrorModule::AM, 3);
pub const RESULT_LIBRARY_APPLET_TERMINATED: ResultCode =
    ResultCode::from_module_description(ErrorModule::AM, 22);
pub const RESULT_INVALID_OFFSET: ResultCode =
    ResultCode::from_module_description(ErrorModule::AM, 503);
pub const RESULT_INVALID_STORAGE_TYPE: ResultCode =
    ResultCode::from_module_description(ErrorModule::AM, 511);
pub const RESULT_FATAL_SECTION_COUNT_IMBALANCE: ResultCode =
    ResultCode::from_module_description(ErrorModule::AM, 512);
