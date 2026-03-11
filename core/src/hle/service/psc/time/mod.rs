// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/
//! Status: Stubbed
//!
//! Time services: clocks, timezone, alarms, shared memory.

pub mod alarms;
pub mod clocks;
pub mod common;
pub mod errors;
pub mod manager;
pub mod power_state_request_manager;
pub mod power_state_service;
pub mod service_manager;
pub mod shared_memory;
pub mod static_service;
pub mod steady_clock;
pub mod system_clock;
pub mod time_zone;
pub mod time_zone_service;
