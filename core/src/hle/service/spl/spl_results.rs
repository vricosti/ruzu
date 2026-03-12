// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/spl_results.h
//!
//! Result codes for the SPL service.

use crate::hle::result::{ErrorModule, ResultCode};

// Description 0 - 99
pub const RESULT_SECURE_MONITOR_ERROR: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 0);
pub const RESULT_SECURE_MONITOR_NOT_IMPLEMENTED: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 1);
pub const RESULT_SECURE_MONITOR_INVALID_ARGUMENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 2);
pub const RESULT_SECURE_MONITOR_BUSY: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 3);
pub const RESULT_SECURE_MONITOR_NO_ASYNC_OPERATION: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 4);
pub const RESULT_SECURE_MONITOR_INVALID_ASYNC_OPERATION: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 5);
pub const RESULT_SECURE_MONITOR_NOT_PERMITTED: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 6);
pub const RESULT_SECURE_MONITOR_NOT_INITIALIZED: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 7);

pub const RESULT_INVALID_SIZE: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 100);
pub const RESULT_UNKNOWN_SECURE_MONITOR_ERROR: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 101);
pub const RESULT_DECRYPTION_FAILED: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 102);

pub const RESULT_OUT_OF_KEY_SLOTS: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 104);
pub const RESULT_INVALID_KEY_SLOT: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 105);
pub const RESULT_BOOT_REASON_ALREADY_SET: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 106);
pub const RESULT_BOOT_REASON_NOT_SET: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 107);
pub const RESULT_INVALID_ARGUMENT: ResultCode =
    ResultCode::from_module_description(ErrorModule::SPL, 108);
