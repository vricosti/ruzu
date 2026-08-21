// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/
//!
//! Application Manager (AM) service and its submodules.

pub mod am;
pub mod am_results;
pub mod am_types;
pub mod applet;
pub mod applet_data_broker;
pub mod applet_manager;
pub mod button_poller;
pub mod display_layer_manager;
pub mod event_observer;
pub mod frontend;
pub mod hid_registration;
pub mod library_applet_storage;
pub mod lifecycle_manager;
pub mod process_creation;
pub mod process_holder;
pub mod service;
pub mod window_system;
