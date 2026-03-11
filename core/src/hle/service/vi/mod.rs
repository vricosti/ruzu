// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/
//! Status: Stubbed
//!
//! VI (Visual Interface) services: display management, layers, vsync.

pub mod application_display_service;
pub mod application_root_service;
pub mod conductor;
pub mod container;
pub mod display;
pub mod display_list;
pub mod layer;
pub mod layer_list;
pub mod manager_display_service;
pub mod manager_root_service;
pub mod service_creator;
pub mod shared_buffer_manager;
pub mod system_display_service;
pub mod system_root_service;
pub mod vi;
pub mod vi_results;
pub mod vi_types;
pub mod vsync_manager;
