// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ptm/ptm.h
//! Port of zuyu/src/core/hle/service/ptm/ptm.cpp
//!
//! PTM service registration — registers psm, ts services.

/// LoopProcess — registers "psm" and "ts" services.
///
/// Corresponds to `Service::PTM::LoopProcess` in upstream ptm.cpp.
pub fn loop_process() {
    log::debug!("PTM::LoopProcess called");
    // TODO: Register psm and ts with ServerManager
}
