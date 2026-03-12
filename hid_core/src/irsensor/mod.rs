// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/irsensor/

pub mod irs_types;
pub mod processor_base;
pub mod clustering_processor;
pub mod image_transfer_processor;
pub mod ir_led_processor;
pub mod moment_processor;
pub mod pointing_processor;
pub mod tera_plugin_processor;
