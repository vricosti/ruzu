// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/web_service/web_result.h
//!
//! Defines the result type for web service operations.
//!
//! NOTE: The canonical `WebResult` and `WebResultCode` types are defined in
//! `common::announce_multiplayer_room` for dependency reasons. This module
//! re-exports them for file-structure parity with upstream.

pub use common::announce_multiplayer_room::{WebResult, WebResultCode};
