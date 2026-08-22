// SPDX-FileCopyrightText: Copyright 2014 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! ARM CPU interface and backend modules.
//! Port of zuyu/src/core/arm/

pub mod arm_interface;
pub mod debug;
pub mod dynarmic;
pub mod exclusive_monitor;
#[cfg(all(
    target_arch = "aarch64",
    any(target_os = "linux", target_os = "android")
))]
pub mod nce;
pub mod symbols;
