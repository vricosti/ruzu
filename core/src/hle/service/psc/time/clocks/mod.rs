// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/clocks/

pub mod context_writers;
pub mod ephemeral_network_system_clock_core;
pub mod standard_local_system_clock_core;
pub mod standard_network_system_clock_core;
pub mod standard_steady_clock_core;
pub mod standard_user_system_clock_core;
pub mod steady_clock_core;
pub mod system_clock_core;
pub mod tick_based_steady_clock_core;
