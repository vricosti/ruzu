// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/bcat_result.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_INVALID_ARGUMENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::BCAT, 1);
pub const RESULT_FAILED_OPEN_ENTITY: ResultCode =
    ResultCode::from_module_description(ErrorModule::BCAT, 2);
pub const RESULT_ENTITY_ALREADY_OPEN: ResultCode =
    ResultCode::from_module_description(ErrorModule::BCAT, 6);
pub const RESULT_NO_OPEN_ENTRY: ResultCode =
    ResultCode::from_module_description(ErrorModule::BCAT, 7);
