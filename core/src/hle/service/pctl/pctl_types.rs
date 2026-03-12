// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pctl/pctl_types.h

use bitflags::bitflags;

bitflags! {
    /// Capability — parental control capability flags.
    ///
    /// Corresponds to `Capability` in upstream pctl_types.h.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Capability: u32 {
        const NONE        = 0;
        const APPLICATION  = 1 << 0;
        const SNS_POST     = 1 << 1;
        const RECOVERY     = 1 << 6;
        const STATUS       = 1 << 8;
        const STEREO_VISION = 1 << 9;
        const SYSTEM       = 1 << 15;
    }
}

/// Application info — stored per-session after Initialize.
///
/// Corresponds to `ApplicationInfo` in upstream pctl_types.h.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct ApplicationInfo {
    pub application_id: u64,
    pub age_rating: [u8; 32],
    pub parental_control_flag: u32,
    pub capability: u32,
}
const _: () = assert!(core::mem::size_of::<ApplicationInfo>() == 0x30);

/// nn::pctl::RestrictionSettings.
///
/// Corresponds to `RestrictionSettings` in upstream pctl_types.h.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct RestrictionSettings {
    pub rating_age: u8,
    pub sns_post_restriction: bool,
    pub free_communication_restriction: bool,
}
const _: () = assert!(core::mem::size_of::<RestrictionSettings>() == 0x3);

/// nn::pctl::PlayTimerSettings.
///
/// Corresponds to `PlayTimerSettings` in upstream pctl_types.h.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct PlayTimerSettings {
    pub settings: [u32; 13],
}
const _: () = assert!(core::mem::size_of::<PlayTimerSettings>() == 0x34);
