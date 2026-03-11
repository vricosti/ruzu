// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pctl/pctl.h
//! Port of zuyu/src/core/hle/service/pctl/pctl.cpp
//!
//! Parental controls service registration.

/// LoopProcess — registers "pctl", "pctl:a", "pctl:s", "pctl:r" services.
///
/// Corresponds to `Service::PCTL::LoopProcess` in upstream pctl.cpp.
pub fn loop_process() {
    log::debug!("PCTL::LoopProcess called");
    // TODO: Register pctl services with ServerManager
}
