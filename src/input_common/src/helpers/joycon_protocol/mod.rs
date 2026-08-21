// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/` subdirectory.
//!
//! Contains the Joy-Con protocol implementation modules for communication
//! with Nintendo Joy-Con and Pro Controller devices.

#[cfg(feature = "joycon-hid")]
pub mod calibration;
#[cfg(feature = "joycon-hid")]
pub mod common_protocol;
#[cfg(feature = "joycon-hid")]
pub mod generic_functions;
#[cfg(feature = "joycon-hid")]
pub mod irs;
pub mod joycon_types;
#[cfg(feature = "joycon-hid")]
pub mod nfc;
#[cfg(feature = "joycon-hid")]
pub mod poller;
#[cfg(feature = "joycon-hid")]
pub mod ringcon;
#[cfg(feature = "joycon-hid")]
pub mod rumble;
