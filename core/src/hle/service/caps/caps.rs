// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps.h
//! Port of zuyu/src/core/hle/service/caps/caps.cpp
//!
//! Screenshot/album service registration.

/// LoopProcess — registers "caps:a", "caps:c", "caps:u", "caps:ss", "caps:sc", "caps:su".
///
/// Corresponds to `Service::Capture::LoopProcess` in upstream caps.cpp.
pub fn loop_process() {
    log::debug!("Capture::LoopProcess called");
    // TODO: Create AlbumManager, register all caps services
}
