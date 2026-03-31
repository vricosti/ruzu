// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/ — Nintendo Switch HID (Human Interface Device) subsystem.

pub mod hid_core;
pub mod hid_result;
pub mod hid_types;
pub mod hid_util;
pub mod resource_manager;

pub mod frontend;
pub mod hidbus;
pub mod irsensor;
pub mod resources;
