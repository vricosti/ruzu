// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pctl/
//! Upstream files:
//!   - pctl.h / pctl.cpp
//!   - parental_control_service.h / parental_control_service.cpp
//!   - parental_control_service_factory.h / parental_control_service_factory.cpp
//!   - pctl_results.h
//!   - pctl_types.h

#[allow(invalid_reference_casting)]
pub mod parental_control_service;
pub mod parental_control_service_factory;
pub mod pctl;
pub mod pctl_results;
pub mod pctl_types;
