// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/ns_results.h

use crate::hle::result::{ErrorModule, ResultCode};

pub const RESULT_APPLICATION_LANGUAGE_NOT_FOUND: ResultCode =
    ResultCode::from_module_description(ErrorModule::NS, 300);
