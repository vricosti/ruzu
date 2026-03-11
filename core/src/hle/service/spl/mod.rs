// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/
//! Upstream files:
//!   - spl.h / spl.cpp
//!   - spl_module.h / spl_module.cpp
//!   - csrng.h / csrng.cpp
//!   - spl_results.h
//!   - spl_types.h

pub mod csrng;
pub mod spl;
pub mod spl_module;
pub mod spl_results;
pub mod spl_types;
