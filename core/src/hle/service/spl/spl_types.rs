// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/spl_types.h

/// HardwareType — identifies the hardware platform.
///
/// Corresponds to `HardwareType` in upstream spl_types.h.
#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HardwareType {
    Icosa = 0,
    Copper = 1,
    Hoag = 2,
    Iowa = 3,
    Calcio = 4,
    Aula = 5,
}
