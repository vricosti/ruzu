// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Dynarmic CPU backend modules.
//! Port of zuyu/src/core/arm/dynarmic/

pub mod arm_dynarmic;
pub mod arm_dynarmic_32;
pub mod arm_dynarmic_64;
pub mod dynarmic_cp15;
pub mod dynarmic_exclusive_monitor;
