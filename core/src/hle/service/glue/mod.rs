// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/
//! Upstream files:
//!   - glue.h / glue.cpp
//!   - errors.h
//!   - arp.h / arp.cpp
//!   - bgtc.h / bgtc.cpp
//!   - ectx.h / ectx.cpp
//!   - notif.h / notif.cpp
//!   - glue_manager.h / glue_manager.cpp
//!   - time/ (manager, static, alarm_worker, file_timestamp_worker,
//!            pm_state_change_handler, standard_steady_clock_resource,
//!            time_zone, time_zone_binary, worker)

pub mod arp;
pub mod bgtc;
pub mod ectx;
pub mod errors;
pub mod glue;
pub mod glue_manager;
pub mod notif;
pub mod time;
