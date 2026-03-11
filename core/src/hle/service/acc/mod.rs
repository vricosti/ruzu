// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/
//! Upstream files:
//!   - acc.h / acc.cpp
//!   - errors.h
//!   - acc_aa.h / acc_aa.cpp
//!   - acc_su.h / acc_su.cpp
//!   - acc_u0.h / acc_u0.cpp
//!   - acc_u1.h / acc_u1.cpp
//!   - async_context.h / async_context.cpp
//!   - profile_manager.h / profile_manager.cpp

pub mod acc;
pub mod errors;
pub mod acc_aa;
pub mod acc_su;
pub mod acc_u0;
pub mod acc_u1;
pub mod async_context;
pub mod profile_manager;
