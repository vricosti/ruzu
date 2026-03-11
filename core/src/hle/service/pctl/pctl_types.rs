// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pctl/pctl_types.h

/// Capability — parental control capability flags.
///
/// Corresponds to `Capability` in upstream pctl_types.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Capability {
    None = 0,
    Application = 1 << 0,
    SnsPost = 1 << 1,
    FreeCommunication = 1 << 2,
    StereoVision = 1 << 5,
    System = 1 << 6,
}
