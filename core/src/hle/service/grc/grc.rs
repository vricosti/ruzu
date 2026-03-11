// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/grc/grc.cpp
//!
//! GRC service ("grc:c"). All commands are unimplemented stubs.

/// IPC command IDs for GRC
pub mod commands {
    pub const OPEN_CONTINUOUS_RECORDER: u32 = 1;
    pub const OPEN_GAME_MOVIE_TRIMMER: u32 = 2;
    pub const OPEN_OFFSCREEN_RECORDER: u32 = 3;
    pub const CREATE_MOVIE_MAKER: u32 = 101;
    pub const SET_OFFSCREEN_RECORDING_MARKER: u32 = 9903;
}

/// GRC service ("grc:c"). All stubs.
pub struct GRC;

impl GRC {
    pub fn new() -> Self {
        Self
    }
}

/// Registers "grc:c" service.
///
/// Corresponds to `LoopProcess` in upstream `grc.cpp`.
pub fn loop_process() {
    // TODO: register "grc:c" -> GRC with ServerManager
}
