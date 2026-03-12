// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii_types.h
//!
//! Type constants, enums, and basic Mii data types.

pub const MAX_NAME_SIZE: usize = 10;
pub const MAX_HEIGHT: u8 = 127;
pub const MAX_BUILD: u8 = 127;
pub const MAX_TYPE: u8 = 1;
pub const MAX_REGION_MOVE: u8 = 3;
pub const MAX_EYE_SCALE: u8 = 7;
pub const MAX_EYE_ASPECT: u8 = 6;
pub const MAX_EYE_ROTATE: u8 = 7;
pub const MAX_EYE_X: u8 = 12;
pub const MAX_EYE_Y: u8 = 18;
pub const MAX_EYEBROW_SCALE: u8 = 8;
pub const MAX_EYEBROW_ASPECT: u8 = 6;
pub const MAX_EYEBROW_ROTATE: u8 = 11;
pub const MAX_EYEBROW_X: u8 = 12;
pub const MAX_EYEBROW_Y: u8 = 15;
pub const MAX_NOSE_SCALE: u8 = 8;
pub const MAX_NOSE_Y: u8 = 18;
pub const MAX_MOUTH_SCALE: u8 = 8;
pub const MAX_MOUTH_ASPECT: u8 = 6;
pub const MAX_MOUTH_Y: u8 = 18;
pub const MAX_MUSTACHE_SCALE: u8 = 8;
pub const MAX_MUSTACHE_Y: u8 = 16;
pub const MAX_GLASS_SCALE: u8 = 7;
pub const MAX_GLASS_Y: u8 = 20;
pub const MAX_MOLE_SCALE: u8 = 8;
pub const MAX_MOLE_X: u8 = 16;
pub const MAX_MOLE_Y: u8 = 30;
pub const MAX_VER3_COMMON_COLOR: u8 = 7;
pub const MAX_VER3_GLASS_TYPE: u8 = 8;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Age {
    Young = 0,
    Normal = 1,
    Old = 2,
    All = 3, // Default
}

impl Default for Age {
    fn default() -> Self {
        Age::All
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Gender {
    Male = 0,
    Female = 1,
    All = 2, // Default
}

impl Default for Gender {
    fn default() -> Self {
        Gender::All
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Race {
    Black = 0,
    White = 1,
    Asian = 2,
    All = 3, // Default
}

impl Default for Race {
    fn default() -> Self {
        Race::All
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FontRegion {
    Standard = 0,
    China = 1,
    Korea = 2,
    Taiwan = 3,
}

impl Default for FontRegion {
    fn default() -> Self {
        FontRegion::Standard
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceFlag {
    Database = 0,
    Default = 1,
}

impl Default for SourceFlag {
    fn default() -> Self {
        SourceFlag::Database
    }
}

/// Nickname type: 10 UTF-16 characters + null terminator = 11 u16 values.
pub type Nickname = [u16; MAX_NAME_SIZE + 1];

/// CreateId is a UUID (128-bit).
pub type CreateId = u128;
