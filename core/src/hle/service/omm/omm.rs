// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/omm/omm.h
//! Port of zuyu/src/core/hle/service/omm/omm.cpp
//!
//! OMM service registration. Registers:
//! - "idle:sys" -> IPolicyManagerSystem
//! - "omm" -> IOperationModeManager
//! - "spsm" -> IPowerStateInterface

/// Register all OMM services.
///
/// In upstream C++, this creates a ServerManager and registers:
/// - idle:sys (IPolicyManagerSystem)
/// - omm (IOperationModeManager)
/// - spsm (IPowerStateInterface)
///
/// Full implementation depends on the ServerManager infrastructure.
pub fn loop_process() {
    log::debug!("OMM::LoopProcess called");
    // Registration of named services depends on ServerManager infrastructure.
}
