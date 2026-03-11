// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/glue.h
//! Port of zuyu/src/core/hle/service/glue/glue.cpp

/// Registers Glue services with the server manager.
///
/// Corresponds to `LoopProcess` in upstream `glue.cpp`.
/// Services registered:
///   arp:r, arp:w, bgtc:t, bgtc:sc, ectx:aw,
///   notif:a, notif:s, time:u, time:a, time:r
pub fn loop_process() {
    log::debug!("Glue::LoopProcess - registering arp, bgtc, ectx, notif, time services");
    // TODO: create ServerManager, register named services, run server
}
