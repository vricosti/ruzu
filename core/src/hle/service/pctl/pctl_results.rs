// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pctl/pctl_results.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_NO_FREE_COMMUNICATION: ResultCode =
    ResultCode::from_module_description(ErrorModule::PCTL, 101);
pub const RESULT_STEREO_VISION_RESTRICTION_CONFIGURED: ResultCode =
    ResultCode::from_module_description(ErrorModule::PCTL, 104);
pub const RESULT_NO_CAPABILITY: ResultCode =
    ResultCode::from_module_description(ErrorModule::PCTL, 131);
pub const RESULT_NO_RESTRICTION_ENABLED: ResultCode =
    ResultCode::from_module_description(ErrorModule::PCTL, 181);
