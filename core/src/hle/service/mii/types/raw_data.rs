// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/types/raw_data.h
//! Port of zuyu/src/core/hle/service/mii/types/raw_data.cpp
//!
//! Raw data tables used for building random and default Miis.
//! These are large lookup tables extracted from the Mii sysmodule.

use super::super::mii_types::{
    CommonColor, DefaultMii, FacelineColor, GlassType, Nickname,
};

// ---- Helper macro for constructing [u32; 47] with trailing zeros ----

/// Creates a `[u32; 47]` array from the given values, filling the rest with 0.
macro_rules! arr47 {
    ($($val:expr),* $(,)?) => {{
        {
            let mut arr = [0u32; 47];
            let vals: &[u32] = &[$($val),*];
            let mut i = 0;
            while i < vals.len() {
                arr[i] = vals[i];
                i += 1;
            }
            arr
        }
    }};
}

// ---- Struct definitions matching upstream raw_data.h ----

/// RandomMiiValues: 188 bytes of random selection data.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RandomMiiValues {
    pub values: [u8; 188],
}
const _: () = assert!(std::mem::size_of::<RandomMiiValues>() == 0xbc);

impl Default for RandomMiiValues {
    fn default() -> Self {
        Self { values: [0u8; 188] }
    }
}

/// RandomMiiData4: indexed by gender, age, race.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RandomMiiData4 {
    pub gender: u32,
    pub age: u32,
    pub race: u32,
    pub values_count: u32,
    pub values: [u32; 47],
}
const _: () = assert!(std::mem::size_of::<RandomMiiData4>() == 0xcc);

impl Default for RandomMiiData4 {
    fn default() -> Self {
        Self {
            gender: 0,
            age: 0,
            race: 0,
            values_count: 0,
            values: [0u32; 47],
        }
    }
}

/// RandomMiiData3: indexed by arg_1, arg_2.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RandomMiiData3 {
    pub arg_1: u32,
    pub arg_2: u32,
    pub values_count: u32,
    pub values: [u32; 47],
}
const _: () = assert!(std::mem::size_of::<RandomMiiData3>() == 0xc8);

impl Default for RandomMiiData3 {
    fn default() -> Self {
        Self {
            arg_1: 0,
            arg_2: 0,
            values_count: 0,
            values: [0u32; 47],
        }
    }
}

/// RandomMiiData2: indexed by arg_1.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RandomMiiData2 {
    pub arg_1: u32,
    pub values_count: u32,
    pub values: [u32; 47],
}
const _: () = assert!(std::mem::size_of::<RandomMiiData2>() == 0xc4);

impl Default for RandomMiiData2 {
    fn default() -> Self {
        Self {
            arg_1: 0,
            values_count: 0,
            values: [0u32; 47],
        }
    }
}

// ---- Ver3 conversion tables ----

pub const FROM_VER3_FACELINE_COLOR_TABLE: [u8; FacelineColor::COUNT] = [
    0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x0, 0x1, 0x5, 0x5,
];

pub const FROM_VER3_HAIR_COLOR_TABLE: [u8; CommonColor::COUNT] = [
    0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x0, 0x4, 0x3, 0x5, 0x4, 0x4, 0x6, 0x2, 0x0,
    0x6, 0x4, 0x3, 0x2, 0x2, 0x7, 0x3, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2,
    0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x0, 0x0, 0x4,
    0x4, 0x4, 0x4, 0x4, 0x4, 0x0, 0x0, 0x0, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x5, 0x5, 0x5,
    0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x5, 0x7, 0x5, 0x7, 0x7, 0x7, 0x7, 0x7, 0x6, 0x7,
    0x7, 0x7, 0x7, 0x7, 0x3, 0x7, 0x7, 0x7, 0x7, 0x7, 0x0, 0x4, 0x4, 0x4, 0x4,
];

pub const FROM_VER3_EYE_COLOR_TABLE: [u8; CommonColor::COUNT] = [
    0x0, 0x2, 0x2, 0x2, 0x1, 0x3, 0x2, 0x3, 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x2, 0x2, 0x4,
    0x2, 0x1, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2,
    0x2, 0x2, 0x2, 0x2, 0x0, 0x0, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x1, 0x0, 0x4, 0x4,
    0x4, 0x4, 0x4, 0x4, 0x4, 0x0, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5,
    0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x2, 0x2,
    0x3, 0x3, 0x3, 0x3, 0x2, 0x2, 0x2, 0x2, 0x2, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1,
];

pub const FROM_VER3_MOUTHLINE_COLOR_TABLE: [u8; CommonColor::COUNT] = [
    0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x3, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x1, 0x4,
    0x4, 0x4, 0x0, 0x1, 0x2, 0x3, 0x4, 0x4, 0x2, 0x3, 0x3, 0x4, 0x4, 0x4, 0x4, 0x1, 0x4,
    0x4, 0x2, 0x3, 0x3, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x3, 0x3, 0x3, 0x4, 0x4, 0x4,
    0x3, 0x3, 0x3, 0x3, 0x3, 0x4, 0x4, 0x4, 0x4, 0x4, 0x3, 0x3, 0x3, 0x3, 0x4, 0x4, 0x4,
    0x4, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x4, 0x4, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x4, 0x3,
    0x3, 0x3, 0x3, 0x3, 0x4, 0x0, 0x3, 0x3, 0x3, 0x3, 0x4, 0x3, 0x3, 0x3, 0x3,
];

pub const FROM_VER3_GLASS_COLOR_TABLE: [u8; CommonColor::COUNT] = [
    0x0, 0x1, 0x1, 0x1, 0x5, 0x1, 0x1, 0x4, 0x0, 0x5, 0x1, 0x1, 0x3, 0x5, 0x1, 0x2, 0x3,
    0x4, 0x5, 0x4, 0x2, 0x2, 0x4, 0x4, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2, 0x2,
    0x2, 0x2, 0x2, 0x2, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3,
    0x3, 0x3, 0x3, 0x3, 0x3, 0x0, 0x0, 0x0, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x0, 0x5, 0x5,
    0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5, 0x1, 0x4,
    0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x4, 0x5, 0x5, 0x5, 0x5, 0x5, 0x5,
];

pub const FROM_VER3_GLASS_TYPE_TABLE: [u8; GlassType::COUNT] = [
    0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x1,
    0x2, 0x1, 0x3, 0x7, 0x7, 0x6, 0x7, 0x8, 0x7, 0x7,
];

pub const VER3_FACELINE_COLOR_TABLE: [u8; 8] = [
    0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x0, 0x0,
];

pub const VER3_HAIR_COLOR_TABLE: [u8; 8] = [
    0x8, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7,
];

pub const VER3_EYE_COLOR_TABLE: [u8; 6] = [
    0x8, 0x9, 0xa, 0xb, 0xc, 0xd,
];

pub const VER3_MOUTH_COLOR_TABLE: [u8; 5] = [
    0x13, 0x14, 0x15, 0x16, 0x17,
];

pub const VER3_GLASS_COLOR_TABLE: [u8; 7] = [
    0x8, 0xe, 0xf, 0x10, 0x11, 0x12, 0x0,
];

// ---- Lookup tables ----

pub const EYE_ROTATE_LOOKUP: [u8; 62] = [
    0x03, 0x04, 0x04, 0x04, 0x03, 0x04, 0x04, 0x04, 0x03, 0x04, 0x04, 0x04, 0x04, 0x03, 0x03, 0x04,
    0x04, 0x04, 0x03, 0x03, 0x04, 0x03, 0x04, 0x03, 0x03, 0x04, 0x03, 0x04, 0x04, 0x03, 0x04, 0x04,
    0x04, 0x03, 0x03, 0x03, 0x04, 0x04, 0x03, 0x03, 0x03, 0x04, 0x04, 0x03, 0x03, 0x03, 0x03, 0x03,
    0x03, 0x03, 0x03, 0x03, 0x04, 0x04, 0x04, 0x04, 0x03, 0x04, 0x04, 0x03, 0x04, 0x04,
];

pub const EYEBROW_ROTATE_LOOKUP: [u8; 24] = [
    0x06, 0x06, 0x05, 0x07, 0x06, 0x07, 0x06, 0x07, 0x04, 0x07, 0x06, 0x08,
    0x05, 0x05, 0x06, 0x06, 0x07, 0x07, 0x06, 0x06, 0x05, 0x06, 0x07, 0x05,
];

// ---- Helper function to make a DefaultMii nickname from a string ----

const fn make_nickname(chars: &[u16]) -> Nickname {
    let mut result = Nickname { data: [0u16; 10] };
    let mut i = 0;
    while i < chars.len() && i < 10 {
        result.data[i] = chars[i];
        i += 1;
    }
    result
}

// "no name" as UTF-16
const NO_NAME: Nickname = make_nickname(&[b'n' as u16, b'o' as u16, b' ' as u16, b'n' as u16, b'a' as u16, b'm' as u16, b'e' as u16]);

// ---- BaseMii ----

pub const BASE_MII: [DefaultMii; 2] = [
    DefaultMii {
        face_type: 0, face_color: 0, face_wrinkle: 0, face_makeup: 0,
        hair_type: 33, hair_color: 1, hair_flip: 0,
        eye_type: 2, eye_color: 0, eye_scale: 4, eye_aspect: 3, eye_rotate: 4, eye_x: 2, eye_y: 12,
        eyebrow_type: 6, eyebrow_color: 1, eyebrow_scale: 4, eyebrow_aspect: 3, eyebrow_rotate: 6, eyebrow_x: 2, eyebrow_y: 10,
        nose_type: 1, nose_scale: 4, nose_y: 9,
        mouth_type: 23, mouth_color: 0, mouth_scale: 4, mouth_aspect: 3, mouth_y: 13,
        mustache_type: 0, beard_type: 0, beard_color: 0, mustache_scale: 4, mustache_y: 10,
        glasses_type: 0, glasses_color: 0, glasses_scale: 4, glasses_y: 10,
        mole_type: 0, mole_scale: 4, mole_x: 2, mole_y: 20,
        height: 64, weight: 64, gender: 0, favorite_color: 0, region_move: 0, font_region: 0, r#type: 0,
        nickname: NO_NAME,
    },
    DefaultMii {
        face_type: 0, face_color: 0, face_wrinkle: 0, face_makeup: 0,
        hair_type: 12, hair_color: 1, hair_flip: 0,
        eye_type: 4, eye_color: 0, eye_scale: 4, eye_aspect: 3, eye_rotate: 3, eye_x: 2, eye_y: 12,
        eyebrow_type: 0, eyebrow_color: 1, eyebrow_scale: 4, eyebrow_aspect: 3, eyebrow_rotate: 6, eyebrow_x: 2, eyebrow_y: 10,
        nose_type: 1, nose_scale: 4, nose_y: 9,
        mouth_type: 23, mouth_color: 0, mouth_scale: 4, mouth_aspect: 3, mouth_y: 13,
        mustache_type: 0, beard_type: 0, beard_color: 0, mustache_scale: 4, mustache_y: 10,
        glasses_type: 0, glasses_color: 0, glasses_scale: 4, glasses_y: 10,
        mole_type: 0, mole_scale: 4, mole_x: 2, mole_y: 20,
        height: 64, weight: 64, gender: 1, favorite_color: 0, region_move: 0, font_region: 0, r#type: 0,
        nickname: NO_NAME,
    },
];

// ---- DefaultMii (6 entries) ----

pub const DEFAULT_MII: [DefaultMii; 6] = [
    DefaultMii {
        face_type: 0, face_color: 4, face_wrinkle: 0, face_makeup: 0,
        hair_type: 68, hair_color: 0, hair_flip: 0,
        eye_type: 2, eye_color: 0, eye_scale: 4, eye_aspect: 3, eye_rotate: 4, eye_x: 2, eye_y: 12,
        eyebrow_type: 6, eyebrow_color: 0, eyebrow_scale: 4, eyebrow_aspect: 3, eyebrow_rotate: 6, eyebrow_x: 2, eyebrow_y: 10,
        nose_type: 1, nose_scale: 4, nose_y: 9,
        mouth_type: 23, mouth_color: 0, mouth_scale: 4, mouth_aspect: 3, mouth_y: 13,
        mustache_type: 0, beard_type: 0, beard_color: 0, mustache_scale: 4, mustache_y: 10,
        glasses_type: 0, glasses_color: 0, glasses_scale: 4, glasses_y: 10,
        mole_type: 0, mole_scale: 4, mole_x: 2, mole_y: 20,
        height: 64, weight: 64, gender: 0, favorite_color: 4, region_move: 0, font_region: 0, r#type: 0,
        nickname: NO_NAME,
    },
    DefaultMii {
        face_type: 0, face_color: 0, face_wrinkle: 0, face_makeup: 0,
        hair_type: 55, hair_color: 6, hair_flip: 0,
        eye_type: 2, eye_color: 4, eye_scale: 4, eye_aspect: 3, eye_rotate: 4, eye_x: 2, eye_y: 12,
        eyebrow_type: 6, eyebrow_color: 6, eyebrow_scale: 4, eyebrow_aspect: 3, eyebrow_rotate: 6, eyebrow_x: 2, eyebrow_y: 10,
        nose_type: 1, nose_scale: 4, nose_y: 9,
        mouth_type: 23, mouth_color: 0, mouth_scale: 4, mouth_aspect: 3, mouth_y: 13,
        mustache_type: 0, beard_type: 0, beard_color: 0, mustache_scale: 4, mustache_y: 10,
        glasses_type: 0, glasses_color: 0, glasses_scale: 4, glasses_y: 10,
        mole_type: 0, mole_scale: 4, mole_x: 2, mole_y: 20,
        height: 64, weight: 64, gender: 0, favorite_color: 5, region_move: 0, font_region: 0, r#type: 0,
        nickname: NO_NAME,
    },
    DefaultMii {
        face_type: 0, face_color: 1, face_wrinkle: 0, face_makeup: 0,
        hair_type: 33, hair_color: 1, hair_flip: 0,
        eye_type: 2, eye_color: 0, eye_scale: 4, eye_aspect: 3, eye_rotate: 4, eye_x: 2, eye_y: 12,
        eyebrow_type: 6, eyebrow_color: 1, eyebrow_scale: 4, eyebrow_aspect: 3, eyebrow_rotate: 6, eyebrow_x: 2, eyebrow_y: 10,
        nose_type: 1, nose_scale: 4, nose_y: 9,
        mouth_type: 23, mouth_color: 0, mouth_scale: 4, mouth_aspect: 3, mouth_y: 13,
        mustache_type: 0, beard_type: 0, beard_color: 0, mustache_scale: 4, mustache_y: 10,
        glasses_type: 0, glasses_color: 0, glasses_scale: 4, glasses_y: 10,
        mole_type: 0, mole_scale: 4, mole_x: 2, mole_y: 20,
        height: 64, weight: 64, gender: 0, favorite_color: 0, region_move: 0, font_region: 0, r#type: 0,
        nickname: NO_NAME,
    },
    DefaultMii {
        face_type: 0, face_color: 2, face_wrinkle: 0, face_makeup: 0,
        hair_type: 24, hair_color: 0, hair_flip: 0,
        eye_type: 4, eye_color: 0, eye_scale: 4, eye_aspect: 3, eye_rotate: 3, eye_x: 2, eye_y: 12,
        eyebrow_type: 0, eyebrow_color: 0, eyebrow_scale: 4, eyebrow_aspect: 3, eyebrow_rotate: 6, eyebrow_x: 2, eyebrow_y: 10,
        nose_type: 1, nose_scale: 4, nose_y: 9,
        mouth_type: 23, mouth_color: 0, mouth_scale: 4, mouth_aspect: 3, mouth_y: 13,
        mustache_type: 0, beard_type: 0, beard_color: 0, mustache_scale: 4, mustache_y: 10,
        glasses_type: 0, glasses_color: 0, glasses_scale: 4, glasses_y: 10,
        mole_type: 0, mole_scale: 4, mole_x: 2, mole_y: 20,
        height: 64, weight: 64, gender: 1, favorite_color: 2, region_move: 0, font_region: 0, r#type: 0,
        nickname: NO_NAME,
    },
    DefaultMii {
        face_type: 0, face_color: 0, face_wrinkle: 0, face_makeup: 0,
        hair_type: 14, hair_color: 7, hair_flip: 0,
        eye_type: 4, eye_color: 5, eye_scale: 4, eye_aspect: 3, eye_rotate: 3, eye_x: 2, eye_y: 12,
        eyebrow_type: 0, eyebrow_color: 7, eyebrow_scale: 4, eyebrow_aspect: 3, eyebrow_rotate: 6, eyebrow_x: 2, eyebrow_y: 10,
        nose_type: 1, nose_scale: 4, nose_y: 9,
        mouth_type: 23, mouth_color: 0, mouth_scale: 4, mouth_aspect: 3, mouth_y: 13,
        mustache_type: 0, beard_type: 0, beard_color: 0, mustache_scale: 4, mustache_y: 10,
        glasses_type: 0, glasses_color: 0, glasses_scale: 4, glasses_y: 10,
        mole_type: 0, mole_scale: 4, mole_x: 2, mole_y: 20,
        height: 64, weight: 64, gender: 1, favorite_color: 6, region_move: 0, font_region: 0, r#type: 0,
        nickname: NO_NAME,
    },
    DefaultMii {
        face_type: 0, face_color: 0, face_wrinkle: 0, face_makeup: 0,
        hair_type: 12, hair_color: 1, hair_flip: 0,
        eye_type: 4, eye_color: 0, eye_scale: 4, eye_aspect: 3, eye_rotate: 3, eye_x: 2, eye_y: 12,
        eyebrow_type: 0, eyebrow_color: 1, eyebrow_scale: 4, eyebrow_aspect: 3, eyebrow_rotate: 6, eyebrow_x: 2, eyebrow_y: 10,
        nose_type: 1, nose_scale: 4, nose_y: 9,
        mouth_type: 23, mouth_color: 0, mouth_scale: 4, mouth_aspect: 3, mouth_y: 13,
        mustache_type: 0, beard_type: 0, beard_color: 0, mustache_scale: 4, mustache_y: 10,
        glasses_type: 0, glasses_color: 0, glasses_scale: 4, glasses_y: 10,
        mole_type: 0, mole_scale: 4, mole_x: 2, mole_y: 20,
        height: 64, weight: 64, gender: 1, favorite_color: 7, region_move: 0, font_region: 0, r#type: 0,
        nickname: NO_NAME,
    },
];

// ---- RandomMiiFaceline (18 entries) ----
// Upstream uses Gender::Male=0, Female=1; Age::Young=0, Normal=1, Old=2; Race::Black=0, White=1, Asian=2

pub const RANDOM_MII_FACELINE: [RandomMiiData4; 18] = [
    // Male, Young, Black
    RandomMiiData4 { gender: 0, age: 0, race: 0, values_count: 10, values: arr47![0, 0, 1, 1, 2, 3, 4, 5, 9, 9] },
    // Male, Normal, Black
    RandomMiiData4 { gender: 0, age: 1, race: 0, values_count: 10, values: arr47![0, 0, 1, 1, 2, 3, 4, 5, 9, 9] },
    // Male, Old, Black
    RandomMiiData4 { gender: 0, age: 2, race: 0, values_count: 10, values: arr47![0, 0, 1, 1, 2, 3, 4, 5, 9, 9] },
    // Male, Young, White
    RandomMiiData4 { gender: 0, age: 0, race: 1, values_count: 12, values: arr47![0, 0, 1, 2, 2, 3, 4, 5, 6, 7, 10, 11] },
    // Male, Normal, White
    RandomMiiData4 { gender: 0, age: 1, race: 1, values_count: 13, values: arr47![0, 1, 2, 2, 3, 4, 5, 6, 6, 7, 7, 10, 11] },
    // Male, Old, White
    RandomMiiData4 { gender: 0, age: 2, race: 1, values_count: 12, values: arr47![0, 0, 1, 2, 2, 3, 4, 5, 6, 7, 10, 11] },
    // Male, Young, Asian
    RandomMiiData4 { gender: 0, age: 0, race: 2, values_count: 12, values: arr47![0, 0, 1, 2, 2, 3, 4, 5, 6, 7, 10, 11] },
    // Male, Normal, Asian
    RandomMiiData4 { gender: 0, age: 1, race: 2, values_count: 13, values: arr47![0, 1, 2, 2, 3, 4, 5, 6, 6, 7, 7, 10, 11] },
    // Male, Old, Asian
    RandomMiiData4 { gender: 0, age: 2, race: 2, values_count: 12, values: arr47![0, 0, 1, 2, 2, 3, 4, 5, 6, 7, 10, 11] },
    // Female, Young, Black
    RandomMiiData4 { gender: 1, age: 0, race: 0, values_count: 10, values: arr47![0, 0, 1, 1, 2, 3, 4, 5, 9, 9] },
    // Female, Normal, Black
    RandomMiiData4 { gender: 1, age: 1, race: 0, values_count: 10, values: arr47![0, 0, 1, 1, 2, 3, 4, 5, 9, 9] },
    // Female, Old, Black
    RandomMiiData4 { gender: 1, age: 2, race: 0, values_count: 10, values: arr47![0, 0, 1, 1, 2, 3, 4, 5, 9, 9] },
    // Female, Young, White
    RandomMiiData4 { gender: 1, age: 0, race: 1, values_count: 12, values: arr47![0, 0, 0, 1, 1, 1, 2, 3, 4, 5, 8, 10] },
    // Female, Normal, White
    RandomMiiData4 { gender: 1, age: 1, race: 1, values_count: 12, values: arr47![0, 0, 0, 1, 1, 1, 2, 3, 4, 5, 8, 10] },
    // Female, Old, White
    RandomMiiData4 { gender: 1, age: 2, race: 1, values_count: 12, values: arr47![0, 0, 0, 1, 1, 1, 2, 3, 4, 5, 8, 10] },
    // Female, Young, Asian
    RandomMiiData4 { gender: 1, age: 0, race: 2, values_count: 12, values: arr47![0, 0, 0, 1, 1, 1, 2, 3, 4, 5, 8, 10] },
    // Female, Normal, Asian
    RandomMiiData4 { gender: 1, age: 1, race: 2, values_count: 12, values: arr47![0, 0, 0, 1, 1, 1, 2, 3, 4, 5, 8, 10] },
    // Female, Old, Asian
    RandomMiiData4 { gender: 1, age: 2, race: 2, values_count: 12, values: arr47![0, 0, 0, 1, 1, 1, 2, 3, 4, 5, 8, 10] },
];

// ---- RandomMiiFacelineColor (6 entries) ----

pub const RANDOM_MII_FACELINE_COLOR: [RandomMiiData3; 6] = [
    RandomMiiData3 { arg_1: 0, arg_2: 0, values_count: 10, values: arr47![2, 2, 4, 4, 4, 4, 5, 5, 5, 5] },
    RandomMiiData3 { arg_1: 0, arg_2: 1, values_count: 10, values: arr47![0, 0, 0, 0, 1, 1, 2, 3, 3, 3] },
    RandomMiiData3 { arg_1: 0, arg_2: 2, values_count: 10, values: arr47![0, 0, 1, 1, 1, 1, 1, 1, 1, 2] },
    RandomMiiData3 { arg_1: 1, arg_2: 0, values_count: 10, values: arr47![2, 2, 4, 4, 4, 4, 5, 5, 5, 5] },
    RandomMiiData3 { arg_1: 1, arg_2: 1, values_count: 10, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 1, 3] },
    RandomMiiData3 { arg_1: 1, arg_2: 2, values_count: 10, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 1, 1] },
];

// ---- RandomMiiFacelineWrinkle (18 entries) ----

pub const RANDOM_MII_FACELINE_WRINKLE: [RandomMiiData4; 18] = [
    // Male, Young, Black
    RandomMiiData4 { gender: 0, age: 0, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8] },
    // Male, Normal, Black
    RandomMiiData4 { gender: 0, age: 1, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8] },
    // Male, Old, Black
    RandomMiiData4 { gender: 0, age: 2, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8] },
    // Male, Young, White
    RandomMiiData4 { gender: 0, age: 0, race: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3, 4, 5, 6, 7, 8, 8, 9] },
    // Male, Normal, White
    RandomMiiData4 { gender: 0, age: 1, race: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3, 4, 5, 6, 7, 8, 8, 9] },
    // Male, Old, White
    RandomMiiData4 { gender: 0, age: 2, race: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9] },
    // Male, Young, Asian
    RandomMiiData4 { gender: 0, age: 0, race: 2, values_count: 20, values: arr47![9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11] },
    // Male, Normal, Asian
    RandomMiiData4 { gender: 0, age: 1, race: 2, values_count: 20, values: arr47![9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11] },
    // Male, Old, Asian
    RandomMiiData4 { gender: 0, age: 2, race: 2, values_count: 20, values: arr47![9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11] },
    // Female, Young, Black
    RandomMiiData4 { gender: 1, age: 0, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8] },
    // Female, Normal, Black
    RandomMiiData4 { gender: 1, age: 1, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8] },
    // Female, Old, Black
    RandomMiiData4 { gender: 1, age: 2, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8] },
    // Female, Young, White
    RandomMiiData4 { gender: 1, age: 0, race: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 4, 4, 8, 8] },
    // Female, Normal, White
    RandomMiiData4 { gender: 1, age: 1, race: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 4, 4, 8, 8] },
    // Female, Old, White
    RandomMiiData4 { gender: 1, age: 2, race: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 4, 4] },
    // Female, Young, Asian
    RandomMiiData4 { gender: 1, age: 0, race: 2, values_count: 20, values: arr47![9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11] },
    // Female, Normal, Asian
    RandomMiiData4 { gender: 1, age: 1, race: 2, values_count: 20, values: arr47![9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11] },
    // Female, Old, Asian
    RandomMiiData4 { gender: 1, age: 2, race: 2, values_count: 20, values: arr47![9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11] },
];

// ---- RandomMiiFacelineMakeup (18 entries) ----

pub const RANDOM_MII_FACELINE_MAKEUP: [RandomMiiData4; 18] = [
    // Male, Young, Black
    RandomMiiData4 { gender: 0, age: 0, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] },
    // Male, Normal, Black
    RandomMiiData4 { gender: 0, age: 1, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9, 9] },
    // Male, Old, Black
    RandomMiiData4 { gender: 0, age: 2, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9] },
    // Male, Young, White
    RandomMiiData4 { gender: 0, age: 0, race: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9] },
    // Male, Normal, White
    RandomMiiData4 { gender: 0, age: 1, race: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9] },
    // Male, Old, White
    RandomMiiData4 { gender: 0, age: 2, race: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9] },
    // Male, Young, Asian
    RandomMiiData4 { gender: 0, age: 0, race: 2, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9] },
    // Male, Normal, Asian
    RandomMiiData4 { gender: 0, age: 1, race: 2, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9] },
    // Male, Old, Asian
    RandomMiiData4 { gender: 0, age: 2, race: 2, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 9, 9] },
    // Female, Young, Black
    RandomMiiData4 { gender: 1, age: 0, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 2] },
    // Female, Normal, Black
    RandomMiiData4 { gender: 1, age: 1, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 9, 9] },
    // Female, Old, Black
    RandomMiiData4 { gender: 1, age: 2, race: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 9, 9] },
    // Female, Young, White
    RandomMiiData4 { gender: 1, age: 0, race: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 3, 3, 4, 5, 5, 6, 7, 8, 9] },
    // Female, Normal, White
    RandomMiiData4 { gender: 1, age: 1, race: 1, values_count: 20, values: arr47![0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9] },
    // Female, Old, White
    RandomMiiData4 { gender: 1, age: 2, race: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 6, 7, 8, 9, 9] },
    // Female, Young, Asian
    RandomMiiData4 { gender: 1, age: 0, race: 2, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9] },
    // Female, Normal, Asian
    RandomMiiData4 { gender: 1, age: 1, race: 2, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9] },
    // Female, Old, Asian
    RandomMiiData4 { gender: 1, age: 2, race: 2, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9] },
];

// ---- RandomMiiHairType (18 entries) ----

pub const RANDOM_MII_HAIR_TYPE: [RandomMiiData4; 18] = [
    // Male, Young, Black
    RandomMiiData4 { gender: 0, age: 0, race: 0, values_count: 30, values: arr47![13, 23, 30, 31, 32, 33, 34, 35, 36, 37, 38, 40, 43, 44, 45, 47, 48, 49, 50, 51, 52, 54, 56, 57, 64, 66, 75, 76, 86, 89] },
    // Male, Normal, Black
    RandomMiiData4 { gender: 0, age: 1, race: 0, values_count: 31, values: arr47![13, 23, 30, 31, 32, 33, 34, 35, 36, 37, 38, 40, 43, 44, 45, 47, 48, 49, 50, 51, 52, 54, 56, 57, 64, 66, 73, 75, 81, 86, 87] },
    // Male, Old, Black
    RandomMiiData4 { gender: 0, age: 2, race: 0, values_count: 31, values: arr47![13, 23, 30, 31, 32, 33, 34, 35, 36, 37, 38, 40, 43, 44, 45, 47, 48, 49, 50, 51, 52, 54, 56, 57, 64, 66, 73, 75, 81, 86, 87] },
    // Male, Young, White
    RandomMiiData4 { gender: 0, age: 0, race: 1, values_count: 38, values: arr47![13, 23, 30, 31, 32, 33, 34, 36, 37, 38, 40, 42, 43, 44, 45, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 58, 59, 60, 64, 65, 66, 67, 68, 70, 75, 76, 86, 89] },
    // Male, Normal, White
    RandomMiiData4 { gender: 0, age: 1, race: 1, values_count: 39, values: arr47![13, 23, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 43, 44, 45, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 58, 59, 60, 64, 65, 66, 67, 68, 70, 73, 75, 81, 86, 87] },
    // Male, Old, White
    RandomMiiData4 { gender: 0, age: 2, race: 1, values_count: 39, values: arr47![13, 23, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 43, 44, 45, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 58, 59, 60, 64, 65, 66, 67, 68, 70, 73, 75, 81, 86, 87] },
    // Male, Young, Asian
    RandomMiiData4 { gender: 0, age: 0, race: 2, values_count: 18, values: arr47![13, 23, 30, 36, 37, 41, 45, 47, 51, 53, 54, 55, 58, 59, 65, 67, 86, 88] },
    // Male, Normal, Asian
    RandomMiiData4 { gender: 0, age: 1, race: 2, values_count: 19, values: arr47![13, 23, 30, 36, 37, 39, 41, 45, 47, 51, 53, 54, 55, 58, 59, 65, 67, 86, 88] },
    // Male, Old, Asian
    RandomMiiData4 { gender: 0, age: 2, race: 2, values_count: 19, values: arr47![13, 23, 30, 36, 37, 39, 41, 45, 47, 51, 53, 54, 55, 58, 59, 65, 67, 86, 88] },
    // Female, Young, Black
    RandomMiiData4 { gender: 1, age: 0, race: 0, values_count: 39, values: arr47![0, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 46, 50, 61, 62, 63, 64, 69, 76, 77, 79, 80, 83, 85] },
    // Female, Normal, Black
    RandomMiiData4 { gender: 1, age: 1, race: 0, values_count: 42, values: arr47![0, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 46, 50, 61, 62, 63, 64, 69, 72, 74, 77, 78, 82, 83, 84, 85, 87] },
    // Female, Old, Black
    RandomMiiData4 { gender: 1, age: 2, race: 0, values_count: 42, values: arr47![0, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 46, 50, 61, 62, 63, 64, 69, 72, 74, 77, 78, 82, 83, 84, 85, 87] },
    // Female, Young, White
    RandomMiiData4 { gender: 1, age: 0, race: 1, values_count: 44, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 29, 42, 50, 58, 60, 62, 63, 64, 69, 71, 76, 79, 80, 81, 82, 83, 86] },
    // Female, Normal, White
    RandomMiiData4 { gender: 1, age: 1, race: 1, values_count: 44, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 29, 50, 58, 60, 62, 63, 64, 69, 71, 72, 74, 79, 81, 82, 83, 84, 85] },
    // Female, Old, White
    RandomMiiData4 { gender: 1, age: 2, race: 1, values_count: 44, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 29, 50, 58, 60, 62, 63, 64, 69, 71, 72, 74, 79, 81, 82, 83, 84, 85] },
    // Female, Young, Asian
    RandomMiiData4 { gender: 1, age: 0, race: 2, values_count: 24, values: arr47![0, 1, 2, 3, 4, 5, 6, 10, 11, 12, 13, 14, 16, 17, 18, 20, 21, 24, 25, 58, 62, 69, 76, 83] },
    // Female, Normal, Asian
    RandomMiiData4 { gender: 1, age: 1, race: 2, values_count: 27, values: arr47![0, 1, 2, 3, 4, 5, 6, 10, 11, 12, 13, 14, 16, 17, 18, 20, 21, 24, 25, 58, 62, 69, 74, 76, 81, 83, 85] },
    // Female, Old, Asian
    RandomMiiData4 { gender: 1, age: 2, race: 2, values_count: 27, values: arr47![0, 1, 2, 3, 4, 5, 6, 10, 11, 12, 13, 14, 16, 17, 18, 20, 21, 24, 25, 58, 62, 69, 74, 76, 81, 83, 85] },
];

// ---- RandomMiiHairColor (9 entries) ----

pub const RANDOM_MII_HAIR_COLOR: [RandomMiiData3; 9] = [
    RandomMiiData3 { arg_1: 0, arg_2: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] },
    RandomMiiData3 { arg_1: 0, arg_2: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] },
    RandomMiiData3 { arg_1: 0, arg_2: 2, values_count: 20, values: arr47![0, 0, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4] },
    RandomMiiData3 { arg_1: 1, arg_2: 0, values_count: 20, values: arr47![2, 3, 3, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7] },
    RandomMiiData3 { arg_1: 1, arg_2: 1, values_count: 20, values: arr47![2, 3, 3, 3, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7] },
    RandomMiiData3 { arg_1: 1, arg_2: 2, values_count: 20, values: arr47![2, 3, 3, 4, 4, 4, 4, 4, 4, 5, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7] },
    RandomMiiData3 { arg_1: 2, arg_2: 0, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1] },
    RandomMiiData3 { arg_1: 2, arg_2: 1, values_count: 20, values: arr47![0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 3, 3, 3, 3] },
    RandomMiiData3 { arg_1: 2, arg_2: 2, values_count: 20, values: arr47![4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4] },
];

// ---- RandomMiiEyeType (18 entries) ----

pub const RANDOM_MII_EYE_TYPE: [RandomMiiData4; 18] = [
    // Male, Young, Black
    RandomMiiData4 { gender: 0, age: 0, race: 0, values_count: 26, values: arr47![2, 3, 5, 7, 8, 9, 11, 12, 13, 15, 16, 18, 27, 29, 32, 34, 36, 38, 39, 41, 43, 47, 49, 51, 53, 57] },
    // Male, Normal, Black
    RandomMiiData4 { gender: 0, age: 1, race: 0, values_count: 26, values: arr47![2, 3, 5, 7, 8, 9, 11, 12, 13, 15, 16, 18, 27, 29, 32, 34, 36, 38, 39, 41, 43, 47, 49, 51, 53, 57] },
    // Male, Old, Black
    RandomMiiData4 { gender: 0, age: 2, race: 0, values_count: 27, values: arr47![2, 3, 5, 7, 8, 9, 11, 12, 13, 15, 16, 18, 26, 27, 29, 32, 34, 36, 38, 39, 41, 43, 47, 48, 49, 53, 57] },
    // Male, Young, White
    RandomMiiData4 { gender: 0, age: 0, race: 1, values_count: 35, values: arr47![2, 3, 5, 6, 7, 8, 9, 11, 12, 13, 15, 16, 17, 18, 21, 22, 27, 29, 31, 32, 34, 36, 37, 38, 39, 41, 43, 44, 47, 49, 51, 53, 55, 56, 57] },
    // Male, Normal, White
    RandomMiiData4 { gender: 0, age: 1, race: 1, values_count: 35, values: arr47![2, 3, 5, 6, 7, 8, 9, 11, 12, 13, 15, 16, 17, 18, 21, 22, 27, 29, 31, 32, 34, 36, 37, 38, 39, 41, 43, 44, 47, 49, 51, 53, 55, 56, 57] },
    // Male, Old, White
    RandomMiiData4 { gender: 0, age: 2, race: 1, values_count: 35, values: arr47![2, 3, 5, 6, 7, 8, 9, 11, 12, 13, 15, 16, 18, 21, 22, 26, 27, 29, 31, 32, 34, 36, 37, 38, 39, 41, 43, 44, 47, 48, 49, 50, 53, 56, 57] },
    // Male, Young, Asian
    RandomMiiData4 { gender: 0, age: 0, race: 2, values_count: 30, values: arr47![2, 3, 5, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 21, 22, 31, 32, 34, 36, 37, 39, 41, 44, 49, 51, 53, 55, 56, 57] },
    // Male, Normal, Asian
    RandomMiiData4 { gender: 0, age: 1, race: 2, values_count: 30, values: arr47![2, 3, 5, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 21, 22, 31, 32, 34, 36, 37, 39, 41, 44, 49, 51, 53, 55, 56, 57] },
    // Male, Old, Asian
    RandomMiiData4 { gender: 0, age: 2, race: 2, values_count: 30, values: arr47![2, 3, 5, 7, 8, 9, 11, 12, 13, 14, 15, 16, 18, 21, 22, 26, 31, 32, 34, 36, 37, 39, 41, 44, 48, 49, 50, 51, 53, 57] },
    // Female, Young, Black
    RandomMiiData4 { gender: 1, age: 0, race: 0, values_count: 39, values: arr47![0, 1, 2, 4, 5, 7, 8, 9, 10, 11, 12, 13, 15, 16, 18, 19, 23, 24, 25, 27, 28, 29, 32, 33, 34, 35, 38, 39, 40, 41, 42, 45, 46, 47, 48, 53, 54, 57, 59] },
    // Female, Normal, Black
    RandomMiiData4 { gender: 1, age: 1, race: 0, values_count: 39, values: arr47![0, 1, 2, 4, 5, 7, 8, 9, 10, 11, 12, 13, 15, 16, 18, 19, 23, 24, 25, 27, 28, 29, 32, 33, 34, 35, 38, 39, 40, 41, 42, 45, 46, 47, 48, 53, 54, 57, 59] },
    // Female, Old, Black
    RandomMiiData4 { gender: 1, age: 2, race: 0, values_count: 40, values: arr47![0, 1, 2, 4, 5, 7, 8, 9, 10, 11, 12, 13, 15, 16, 18, 19, 23, 24, 25, 26, 27, 28, 29, 32, 33, 34, 35, 38, 39, 40, 41, 42, 45, 46, 47, 48, 53, 54, 57, 59] },
    // Female, Young, White
    RandomMiiData4 { gender: 1, age: 0, race: 1, values_count: 46, values: arr47![0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25, 27, 28, 29, 30, 32, 33, 34, 35, 37, 38, 39, 40, 41, 42, 45, 46, 47, 48, 53, 54, 57, 58, 59] },
    // Female, Normal, White
    RandomMiiData4 { gender: 1, age: 1, race: 1, values_count: 46, values: arr47![0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25, 27, 28, 29, 30, 32, 33, 34, 35, 37, 38, 39, 40, 41, 42, 45, 46, 47, 48, 53, 54, 57, 58, 59] },
    // Female, Old, White
    RandomMiiData4 { gender: 1, age: 2, race: 1, values_count: 46, values: arr47![0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 18, 19, 20, 21, 23, 24, 25, 26, 27, 28, 29, 30, 32, 33, 34, 35, 37, 38, 39, 40, 41, 42, 45, 46, 47, 48, 53, 54, 57, 58, 59] },
    // Female, Young, Asian
    RandomMiiData4 { gender: 1, age: 0, race: 2, values_count: 34, values: arr47![0, 1, 2, 4, 5, 7, 8, 9, 10, 11, 12, 13, 15, 16, 18, 19, 23, 24, 25, 27, 28, 29, 32, 33, 34, 35, 38, 39, 40, 41, 42, 45, 46, 47] },
    // Female, Normal, Asian
    RandomMiiData4 { gender: 1, age: 1, race: 2, values_count: 34, values: arr47![0, 1, 2, 4, 5, 7, 8, 9, 10, 11, 12, 13, 15, 16, 18, 19, 23, 24, 25, 27, 28, 29, 32, 33, 34, 35, 38, 39, 40, 41, 42, 45, 46, 47] },
    // Female, Old, Asian
    RandomMiiData4 { gender: 1, age: 2, race: 2, values_count: 35, values: arr47![0, 1, 2, 4, 5, 7, 8, 9, 10, 11, 12, 13, 15, 16, 18, 19, 23, 24, 25, 26, 27, 28, 29, 32, 33, 34, 35, 38, 39, 40, 41, 42, 45, 46, 47] },
];

// ---- RandomMiiEyeColor (3 entries) ----

pub const RANDOM_MII_EYE_COLOR: [RandomMiiData2; 3] = [
    RandomMiiData2 { arg_1: 0, values_count: 10, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 1] },
    RandomMiiData2 { arg_1: 1, values_count: 10, values: arr47![0, 1, 1, 2, 3, 3, 4, 4, 4, 5] },
    RandomMiiData2 { arg_1: 2, values_count: 10, values: arr47![0, 0, 0, 0, 0, 0, 0, 0, 0, 1] },
];

// ---- RandomMiiEyebrowType (18 entries) ----

pub const RANDOM_MII_EYEBROW_TYPE: [RandomMiiData4; 18] = [
    // Male, Young, Black
    RandomMiiData4 { gender: 0, age: 0, race: 0, values_count: 18, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18, 20] },
    // Male, Normal, Black
    RandomMiiData4 { gender: 0, age: 1, race: 0, values_count: 18, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18, 20] },
    // Male, Old, Black
    RandomMiiData4 { gender: 0, age: 2, race: 0, values_count: 18, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18, 20] },
    // Male, Young, White
    RandomMiiData4 { gender: 0, age: 0, race: 1, values_count: 23, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22] },
    // Male, Normal, White
    RandomMiiData4 { gender: 0, age: 1, race: 1, values_count: 23, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22] },
    // Male, Old, White
    RandomMiiData4 { gender: 0, age: 2, race: 1, values_count: 23, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22] },
    // Male, Young, Asian
    RandomMiiData4 { gender: 0, age: 0, race: 2, values_count: 21, values: arr47![0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22] },
    // Male, Normal, Asian
    RandomMiiData4 { gender: 0, age: 1, race: 2, values_count: 21, values: arr47![0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22] },
    // Male, Old, Asian
    RandomMiiData4 { gender: 0, age: 2, race: 2, values_count: 21, values: arr47![0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22] },
    // Female, Young, Black
    RandomMiiData4 { gender: 1, age: 0, race: 0, values_count: 9, values: arr47![0, 1, 3, 7, 8, 9, 10, 11, 13] },
    // Female, Normal, Black
    RandomMiiData4 { gender: 1, age: 1, race: 0, values_count: 9, values: arr47![0, 1, 3, 7, 8, 9, 10, 11, 13] },
    // Female, Old, Black
    RandomMiiData4 { gender: 1, age: 2, race: 0, values_count: 9, values: arr47![0, 1, 3, 7, 8, 9, 10, 11, 13] },
    // Female, Young, White
    RandomMiiData4 { gender: 1, age: 0, race: 1, values_count: 11, values: arr47![0, 1, 3, 7, 8, 9, 10, 11, 13, 15, 19] },
    // Female, Normal, White
    RandomMiiData4 { gender: 1, age: 1, race: 1, values_count: 11, values: arr47![0, 1, 3, 7, 8, 9, 10, 11, 13, 15, 19] },
    // Female, Old, White
    RandomMiiData4 { gender: 1, age: 2, race: 1, values_count: 11, values: arr47![0, 1, 3, 7, 8, 9, 10, 11, 13, 15, 19] },
    // Female, Young, Asian
    RandomMiiData4 { gender: 1, age: 0, race: 2, values_count: 9, values: arr47![0, 3, 7, 8, 9, 10, 11, 13, 15] },
    // Female, Normal, Asian
    RandomMiiData4 { gender: 1, age: 1, race: 2, values_count: 9, values: arr47![0, 3, 7, 8, 9, 10, 11, 13, 15] },
    // Female, Old, Asian
    RandomMiiData4 { gender: 1, age: 2, race: 2, values_count: 9, values: arr47![0, 3, 7, 8, 9, 10, 11, 13, 15] },
];

// ---- RandomMiiNoseType (18 entries) ----

pub const RANDOM_MII_NOSE_TYPE: [RandomMiiData4; 18] = [
    // Male, Young, Black
    RandomMiiData4 { gender: 0, age: 0, race: 0, values_count: 11, values: arr47![0, 1, 2, 3, 4, 5, 7, 8, 10, 13, 14] },
    // Male, Normal, Black
    RandomMiiData4 { gender: 0, age: 1, race: 0, values_count: 11, values: arr47![0, 1, 2, 3, 4, 5, 7, 8, 10, 13, 14] },
    // Male, Old, Black
    RandomMiiData4 { gender: 0, age: 2, race: 0, values_count: 11, values: arr47![0, 1, 2, 3, 4, 5, 7, 8, 10, 13, 14] },
    // Male, Young, White
    RandomMiiData4 { gender: 0, age: 0, race: 1, values_count: 18, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17] },
    // Male, Normal, White
    RandomMiiData4 { gender: 0, age: 1, race: 1, values_count: 18, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17] },
    // Male, Old, White
    RandomMiiData4 { gender: 0, age: 2, race: 1, values_count: 15, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 16] },
    // Male, Young, Asian
    RandomMiiData4 { gender: 0, age: 0, race: 2, values_count: 18, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17] },
    // Male, Normal, Asian
    RandomMiiData4 { gender: 0, age: 1, race: 2, values_count: 18, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17] },
    // Male, Old, Asian
    RandomMiiData4 { gender: 0, age: 2, race: 2, values_count: 15, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 16] },
    // Female, Young, Black
    RandomMiiData4 { gender: 1, age: 0, race: 0, values_count: 8, values: arr47![0, 1, 3, 4, 8, 10, 13, 14] },
    // Female, Normal, Black
    RandomMiiData4 { gender: 1, age: 1, race: 0, values_count: 8, values: arr47![0, 1, 3, 4, 8, 10, 13, 14] },
    // Female, Old, Black
    RandomMiiData4 { gender: 1, age: 2, race: 0, values_count: 8, values: arr47![0, 1, 3, 4, 8, 10, 13, 14] },
    // Female, Young, White
    RandomMiiData4 { gender: 1, age: 0, race: 1, values_count: 12, values: arr47![0, 1, 3, 4, 6, 8, 9, 10, 11, 13, 14, 15] },
    // Female, Normal, White
    RandomMiiData4 { gender: 1, age: 1, race: 1, values_count: 11, values: arr47![0, 1, 3, 4, 6, 8, 9, 10, 11, 13, 15] },
    // Female, Old, White
    RandomMiiData4 { gender: 1, age: 2, race: 1, values_count: 10, values: arr47![0, 1, 3, 4, 6, 8, 10, 11, 13, 14] },
    // Female, Young, Asian
    RandomMiiData4 { gender: 1, age: 0, race: 2, values_count: 12, values: arr47![0, 1, 3, 4, 6, 8, 9, 10, 11, 13, 14, 15] },
    // Female, Normal, Asian
    RandomMiiData4 { gender: 1, age: 1, race: 2, values_count: 11, values: arr47![0, 1, 3, 4, 6, 8, 9, 10, 11, 13, 15] },
    // Female, Old, Asian
    RandomMiiData4 { gender: 1, age: 2, race: 2, values_count: 10, values: arr47![0, 1, 3, 4, 6, 8, 10, 11, 13, 14] },
];

// ---- RandomMiiMouthType (18 entries) ----

pub const RANDOM_MII_MOUTH_TYPE: [RandomMiiData4; 18] = [
    // Male, Young, Black
    RandomMiiData4 { gender: 0, age: 0, race: 0, values_count: 25, values: arr47![0, 2, 3, 6, 7, 8, 9, 10, 12, 14, 15, 17, 18, 19, 21, 22, 23, 25, 26, 28, 30, 32, 33, 34, 35] },
    // Male, Normal, Black
    RandomMiiData4 { gender: 0, age: 1, race: 0, values_count: 27, values: arr47![0, 2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 21, 22, 23, 25, 26, 28, 30, 32, 33, 34, 35] },
    // Male, Old, Black
    RandomMiiData4 { gender: 0, age: 2, race: 0, values_count: 28, values: arr47![0, 2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 21, 22, 23, 25, 26, 28, 30, 31, 32, 33, 34, 35] },
    // Male, Young, White
    RandomMiiData4 { gender: 0, age: 0, race: 1, values_count: 24, values: arr47![0, 2, 3, 6, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 30, 31, 33, 34, 35] },
    // Male, Normal, White
    RandomMiiData4 { gender: 0, age: 1, race: 1, values_count: 26, values: arr47![0, 2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 30, 31, 33, 34, 35] },
    // Male, Old, White
    RandomMiiData4 { gender: 0, age: 2, race: 1, values_count: 26, values: arr47![0, 2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 30, 31, 33, 34, 35] },
    // Male, Young, Asian
    RandomMiiData4 { gender: 0, age: 0, race: 2, values_count: 24, values: arr47![0, 2, 3, 6, 7, 8, 9, 10, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 30, 31, 33, 34, 35] },
    // Male, Normal, Asian
    RandomMiiData4 { gender: 0, age: 1, race: 2, values_count: 26, values: arr47![0, 2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 30, 31, 33, 34, 35] },
    // Male, Old, Asian
    RandomMiiData4 { gender: 0, age: 2, race: 2, values_count: 26, values: arr47![0, 2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 30, 31, 33, 34, 35] },
    // Female, Young, Black
    RandomMiiData4 { gender: 1, age: 0, race: 0, values_count: 25, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 14, 15, 17, 18, 19, 21, 22, 23, 25, 26, 30, 33, 34, 35] },
    // Female, Normal, Black
    RandomMiiData4 { gender: 1, age: 1, race: 0, values_count: 26, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 13, 14, 15, 17, 18, 19, 21, 22, 23, 25, 26, 30, 33, 34, 35] },
    // Female, Old, Black
    RandomMiiData4 { gender: 1, age: 2, race: 0, values_count: 26, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 13, 14, 15, 17, 18, 19, 21, 22, 23, 25, 26, 30, 33, 34, 35] },
    // Female, Young, White
    RandomMiiData4 { gender: 1, age: 0, race: 1, values_count: 25, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 14, 15, 17, 18, 19, 21, 22, 23, 24, 26, 27, 29, 33, 35] },
    // Female, Normal, White
    RandomMiiData4 { gender: 1, age: 1, race: 1, values_count: 26, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 13, 14, 15, 17, 18, 19, 21, 22, 23, 24, 26, 27, 29, 33, 35] },
    // Female, Old, White
    RandomMiiData4 { gender: 1, age: 2, race: 1, values_count: 25, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 13, 14, 15, 17, 18, 19, 21, 22, 23, 24, 25, 29, 33, 35] },
    // Female, Young, Asian
    RandomMiiData4 { gender: 1, age: 0, race: 2, values_count: 24, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 29, 33] },
    // Female, Normal, Asian
    RandomMiiData4 { gender: 1, age: 1, race: 2, values_count: 25, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 29, 33] },
    // Female, Old, Asian
    RandomMiiData4 { gender: 1, age: 2, race: 2, values_count: 25, values: arr47![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 29, 33] },
];

// ---- RandomMiiGlassType (3 entries) ----

pub const RANDOM_MII_GLASS_TYPE: [RandomMiiData2; 3] = [
    RandomMiiData2 { arg_1: 0, values_count: 4, values: arr47![90, 94, 96, 100] },
    RandomMiiData2 { arg_1: 1, values_count: 8, values: arr47![83, 86, 90, 93, 94, 96, 98, 100] },
    RandomMiiData2 { arg_1: 2, values_count: 8, values: arr47![78, 83, 0, 93, 0, 0, 98, 100] },
];

// ---- Ver3 conversion functions ----

pub fn from_ver3_get_faceline_color(color: u8) -> u8 {
    FROM_VER3_FACELINE_COLOR_TABLE[color as usize]
}

pub fn from_ver3_get_hair_color(color: u8) -> u8 {
    FROM_VER3_HAIR_COLOR_TABLE[color as usize]
}

pub fn from_ver3_get_eye_color(color: u8) -> u8 {
    FROM_VER3_EYE_COLOR_TABLE[color as usize]
}

pub fn from_ver3_get_mouthline_color(color: u8) -> u8 {
    FROM_VER3_MOUTHLINE_COLOR_TABLE[color as usize]
}

pub fn from_ver3_get_glass_color(color: u8) -> u8 {
    FROM_VER3_GLASS_COLOR_TABLE[color as usize]
}

pub fn from_ver3_get_glass_type(glass_type: u8) -> u8 {
    FROM_VER3_GLASS_TYPE_TABLE[glass_type as usize]
}

pub fn get_faceline_color_from_ver3(color: u32) -> FacelineColor {
    // Safety: the table values are all valid FacelineColor variants (0..=9)
    unsafe { std::mem::transmute(VER3_FACELINE_COLOR_TABLE[color as usize]) }
}

pub fn get_hair_color_from_ver3(color: u32) -> u8 {
    VER3_HAIR_COLOR_TABLE[color as usize]
}

pub fn get_eye_color_from_ver3(color: u32) -> u8 {
    VER3_EYE_COLOR_TABLE[color as usize]
}

pub fn get_mouth_color_from_ver3(color: u32) -> u8 {
    VER3_MOUTH_COLOR_TABLE[color as usize]
}

pub fn get_glass_color_from_ver3(color: u32) -> u8 {
    VER3_GLASS_COLOR_TABLE[color as usize]
}

