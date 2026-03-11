// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/spl_results.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_SECURITY_ENGINE_COMMUNICATION_FAILED: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 1);
pub const RESULT_DECRYPTION_FAILED: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 2);
pub const RESULT_OUT_OF_KEYSLOTS: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 4);
pub const RESULT_INVALID_KEY_SIZE: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 5);
pub const RESULT_INVALID_SIZE: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 6);
