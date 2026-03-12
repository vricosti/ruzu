// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/` subdirectory.
//!
//! Contains helper modules for input processing, including Joy-Con protocol,
//! stick-from-buttons, touch-from-buttons, and UDP protocol.

pub mod joycon_driver;
pub mod joycon_protocol;
pub mod stick_from_buttons;
pub mod touch_from_buttons;
pub mod udp_protocol;
