// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/fatal/fatal.h
//! Port of zuyu/src/core/hle/service/fatal/fatal.cpp
//!
//! Fatal error service registration.

use super::fatal_p::IService as FatalP;
use super::fatal_u::IService as FatalU;

/// Service name constants
pub const SERVICE_NAME_FATAL_P: &str = "fatal:p";
pub const SERVICE_NAME_FATAL_U: &str = "fatal:u";

/// LoopProcess — registers fatal:p and fatal:u services.
///
/// Corresponds to `Service::Fatal::LoopProcess` in upstream fatal.cpp.
pub fn loop_process() {
    log::debug!("Fatal::LoopProcess called");
    // TODO: Register fatal:p and fatal:u with ServerManager
    let _fatal_p = FatalP::new();
    let _fatal_u = FatalU::new();
}
