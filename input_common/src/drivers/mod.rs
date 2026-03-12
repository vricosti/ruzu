// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/` subdirectory.
//!
//! Contains all input driver implementations.

pub mod android;
pub mod camera;
pub mod gc_adapter;
pub mod joycon;
pub mod keyboard;
pub mod mouse;
pub mod sdl_driver;
pub mod tas_input;
pub mod touch_screen;
pub mod udp_client;
pub mod virtual_amiibo;
pub mod virtual_gamepad;
