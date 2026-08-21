// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/

pub mod core;
pub mod devices;
pub mod nvdata;
pub mod nvdrv;
pub mod nvdrv_interface;
pub mod nvmemp;

/// Launches Nvidia services.
///
/// Matches upstream `void Nvidia::LoopProcess(Core::System& system)`.
pub fn loop_process(system: crate::core::SystemRef) {
    nvdrv::loop_process(system);
}

/// Backward-compatible alias.
pub fn register_services(system: crate::core::SystemRef) {
    loop_process(system);
}
