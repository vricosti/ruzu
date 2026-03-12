// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/omm/policy_manager_system.h
//! Port of zuyu/src/core/hle/service/omm/policy_manager_system.cpp

/// IPolicyManagerSystem service ("idle:sys").
///
/// Command IDs (all currently stubbed/unimplemented in upstream):
///   0  GetAutoPowerDownEvent
///   1  IsAutoPowerDownRequested
///   2  Unknown2
///   3  SetHandlingContext
///   4  LoadAndApplySettings
///   5  ReportUserIsActive
pub struct IPolicyManagerSystem;

impl IPolicyManagerSystem {
    pub fn new() -> Self {
        Self
    }
}
