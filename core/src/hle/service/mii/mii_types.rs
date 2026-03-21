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

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FacelineColor {
    Beige = 0,
    WarmBeige = 1,
    Natural = 2,
    Honey = 3,
    Chestnut = 4,
    Porcelain = 5,
    Ivory = 6,
    WarmIvory = 7,
    Almond = 8,
    Espresso = 9,
}

impl FacelineColor {
    pub const MAX: u8 = FacelineColor::Espresso as u8;
    pub const COUNT: usize = (Self::MAX as usize) + 1;
}

impl Default for FacelineColor {
    fn default() -> Self {
        FacelineColor::Beige
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommonColor {
    // For simplicity common colors aren't listed
    Max = 99,
}

impl CommonColor {
    pub const COUNT: usize = 100;
}

impl Default for CommonColor {
    fn default() -> Self {
        CommonColor::Max
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlassType {
    None = 0,
    Oval = 1,
    Wayfarer = 2,
    Rectangle = 3,
    TopRimless = 4,
    Rounded = 5,
    Oversized = 6,
    CatEye = 7,
    Square = 8,
    BottomRimless = 9,
    SemiOpaqueRounded = 10,
    SemiOpaqueCatEye = 11,
    SemiOpaqueOval = 12,
    SemiOpaqueRectangle = 13,
    SemiOpaqueAviator = 14,
    OpaqueRounded = 15,
    OpaqueCatEye = 16,
    OpaqueOval = 17,
    OpaqueRectangle = 18,
    OpaqueAviator = 19,
}

impl GlassType {
    pub const MAX: u8 = GlassType::OpaqueAviator as u8;
    pub const COUNT: usize = (Self::MAX as usize) + 1;
}

impl Default for GlassType {
    fn default() -> Self {
        GlassType::None
    }
}

/// Default Mii configuration data.
/// Maps to upstream `Service::Mii::DefaultMii` in mii_types.h.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DefaultMii {
    pub face_type: u32,
    pub face_color: u32,
    pub face_wrinkle: u32,
    pub face_makeup: u32,
    pub hair_type: u32,
    pub hair_color: u32,
    pub hair_flip: u32,
    pub eye_type: u32,
    pub eye_color: u32,
    pub eye_scale: u32,
    pub eye_aspect: u32,
    pub eye_rotate: u32,
    pub eye_x: u32,
    pub eye_y: u32,
    pub eyebrow_type: u32,
    pub eyebrow_color: u32,
    pub eyebrow_scale: u32,
    pub eyebrow_aspect: u32,
    pub eyebrow_rotate: u32,
    pub eyebrow_x: u32,
    pub eyebrow_y: u32,
    pub nose_type: u32,
    pub nose_scale: u32,
    pub nose_y: u32,
    pub mouth_type: u32,
    pub mouth_color: u32,
    pub mouth_scale: u32,
    pub mouth_aspect: u32,
    pub mouth_y: u32,
    pub mustache_type: u32,
    pub beard_type: u32,
    pub beard_color: u32,
    pub mustache_scale: u32,
    pub mustache_y: u32,
    pub glasses_type: u32,
    pub glasses_color: u32,
    pub glasses_scale: u32,
    pub glasses_y: u32,
    pub mole_type: u32,
    pub mole_scale: u32,
    pub mole_x: u32,
    pub mole_y: u32,
    pub height: u32,
    pub weight: u32,
    pub gender: u32,
    pub favorite_color: u32,
    pub region_move: u32,
    pub font_region: u32,
    pub r#type: u32,
    pub nickname: DefaultMiiNickname,
}

// Upstream Nickname in DefaultMii is std::array<char16_t, 10> = 20 bytes.
pub type DefaultMiiNickname = [u16; MAX_NAME_SIZE];

// static_assert equivalent: sizeof(DefaultMii) == 0xd8
const _: () = assert!(std::mem::size_of::<DefaultMii>() == 0xd8);

impl Default for DefaultMii {
    fn default() -> Self {
        // Safety: all-zeros is valid for this repr(C) struct of u32 + [u16; 10]
        unsafe { std::mem::zeroed() }
    }
}
