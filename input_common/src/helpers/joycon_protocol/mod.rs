// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/` subdirectory.
//!
//! Contains the Joy-Con protocol implementation modules for communication
//! with Nintendo Joy-Con and Pro Controller devices.

pub mod calibration;
pub mod common_protocol;
pub mod generic_functions;
pub mod irs;
pub mod joycon_types;
pub mod nfc;
pub mod poller;
pub mod ringcon;
pub mod rumble;
