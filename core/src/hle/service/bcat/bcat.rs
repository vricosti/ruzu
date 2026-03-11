// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/bcat.h
//! Port of zuyu/src/core/hle/service/bcat/bcat.cpp
//!
//! Registers BCAT and News services.

/// Registers BCAT services with the server manager.
///
/// Corresponds to `LoopProcess` in upstream `bcat.cpp`.
/// Services registered:
///   bcat:a, bcat:m, bcat:u, bcat:s
///   news:a (permissions=0xffffffff), news:p (0x1), news:c (0x2), news:v (0x4), news:m (0xd)
pub fn loop_process() {
    log::debug!("BCAT::LoopProcess - registering bcat and news services");
    // TODO: create ServerManager, register named services, run server
}
