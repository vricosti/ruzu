// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/types/raw_data.h
//! Port of zuyu/src/core/hle/service/mii/types/raw_data.cpp
//!
//! Raw data tables used for building random and default Miis.
//! These are large lookup tables extracted from the Mii sysmodule.
//!
//! Note: The full data tables (1778 lines in C++) will be ported incrementally.
//! Currently this module provides the table structure definitions.

/// Entry in the random Mii face table.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct RandomMiiFacelineInfo {
    pub values_count: u32,
    pub values: [u32; 11],
}

/// Entry in the random Mii hair table.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RandomMiiHairInfo {
    pub values_count: u32,
    pub values: [u32; 52],
}

impl Default for RandomMiiHairInfo {
    fn default() -> Self {
        Self {
            values_count: 0,
            values: [0u32; 52],
        }
    }
}

/// Entry in the random Mii eye table.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RandomMiiEyeInfo {
    pub values_count: u32,
    pub values: [u32; 38],
}

impl Default for RandomMiiEyeInfo {
    fn default() -> Self {
        Self {
            values_count: 0,
            values: [0u32; 38],
        }
    }
}

/// Entry in the random Mii mouth table.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct RandomMiiMouthInfo {
    pub values_count: u32,
    pub values: [u32; 24],
}

/// Random Mii data indices.
#[derive(Debug, Clone, Copy)]
pub struct RandomMiiData {
    pub age: u8,
    pub gender: u8,
    pub race: u8,
    pub values_count: u32,
}

// TODO: Port the actual data tables from upstream raw_data.cpp.
// These are large constant arrays used by MiiManager::BuildRandom and BuildDefault.
// The tables include:
// - RandomMiiFaceline (face shapes per age/gender/race)
// - RandomMiiHairType (hair styles)
// - RandomMiiHairColor (hair colors)
// - RandomMiiEyeType (eye types)
// - RandomMiiEyeColor (eye colors)
// - RandomMiiEyebrowType (eyebrow types)
// - RandomMiiNoseType (nose types)
// - RandomMiiMouthType (mouth types)
// - RandomMiiGlassType (glasses types)
// - DefaultMii (6 default Mii configurations)
