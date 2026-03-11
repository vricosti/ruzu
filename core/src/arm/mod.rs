// SPDX-FileCopyrightText: Copyright 2014 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! ARM CPU interface and backend modules.
//! Port of zuyu/src/core/arm/

pub mod arm_interface;
pub mod debug;
pub mod dynarmic;
pub mod exclusive_monitor;
pub mod nce;
pub mod symbols;
