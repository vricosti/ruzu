// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/errors.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_INVALID_PROCESS_ID: ResultCode =
    ResultCode::from_module_description(ErrorModule::ARP, 31);
pub const RESULT_ALREADY_BOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::ARP, 42);
pub const RESULT_PROCESS_ID_NOT_REGISTERED: ResultCode =
    ResultCode::from_module_description(ErrorModule::ARP, 102);
