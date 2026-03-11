// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/
//! Upstream files:
//!   - event.h / event.cpp
//!   - multi_wait.h / multi_wait.cpp
//!   - multi_wait_holder.h / multi_wait_holder.cpp
//!   - multi_wait_utils.h
//!   - mutex.h / mutex.cpp
//!   - process.h / process.cpp

pub mod event;
pub mod multi_wait;
pub mod multi_wait_holder;
pub mod multi_wait_utils;
pub mod mutex;
pub mod process;
