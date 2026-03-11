// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/apm/
//! Upstream files:
//!   - apm.h / apm.cpp
//!   - apm_controller.h / apm_controller.cpp
//!   - apm_interface.h / apm_interface.cpp
//!
//! Application Performance Management services: "apm", "apm:am", "apm:sys".

pub mod apm;
pub mod apm_controller;
pub mod apm_interface;
