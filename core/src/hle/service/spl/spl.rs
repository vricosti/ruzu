// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/spl.h
//! Port of zuyu/src/core/hle/service/spl/spl.cpp
//!
//! SPL service registration.

/// LoopProcess — registers "csrng", "spl:", "spl:mig", "spl:fs", "spl:ssl",
/// "spl:es", "spl:manu" services.
///
/// Corresponds to `Service::SPL::LoopProcess` in upstream spl.cpp.
pub fn loop_process() {
    log::debug!("SPL::LoopProcess called");
    // TODO: Register SPL services with ServerManager
}
