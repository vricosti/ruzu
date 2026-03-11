// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/vi_results.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_OPERATION_FAILED: ResultCode =
    ResultCode::from_module_description(ErrorModule::VI, 1);
pub const RESULT_PERMISSION_DENIED: ResultCode =
    ResultCode::from_module_description(ErrorModule::VI, 5);
pub const RESULT_NOT_SUPPORTED: ResultCode =
    ResultCode::from_module_description(ErrorModule::VI, 6);
pub const RESULT_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::VI, 7);
