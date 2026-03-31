// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/time/
//! Upstream files:
//!   - manager.h / manager.cpp
//!   - static.h / static.cpp
//!   - alarm_worker.h / alarm_worker.cpp
//!   - file_timestamp_worker.h / file_timestamp_worker.cpp
//!   - pm_state_change_handler.h / pm_state_change_handler.cpp
//!   - standard_steady_clock_resource.h / standard_steady_clock_resource.cpp
//!   - time_zone.h / time_zone.cpp
//!   - time_zone_binary.h / time_zone_binary.cpp
//!   - worker.h / worker.cpp

pub mod alarm_worker;
pub mod file_timestamp_worker;
pub mod manager;
pub mod pm_state_change_handler;
pub mod standard_steady_clock_resource;
pub mod r#static;
pub mod time_zone;
pub mod time_zone_binary;
pub mod worker;
