// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/types/core_data.h
//! Port of zuyu/src/core/hle/service/mii/types/core_data.cpp
//!
//! CoreData: the core Mii data structure used internally.
//! Size: 0x30 bytes in upstream (packed bit-fields + Nickname).

use crate::hle::service::mii::mii_types::{Age, Race};
use crate::hle::service::mii::mii_types::{
    BeardAndMustacheFlag, BeardType, CommonColor, EyeType, EyebrowType, FacelineColor,
    FacelineMake, FacelineType, FacelineWrinkle, FavoriteColor, FontRegion, Gender, GlassType,
    HairFlip, HairType, MoleType, MouthType, MustacheType, Nickname, NoseType, ValidationResult,
    MAX_BUILD, MAX_EYEBROW_ASPECT, MAX_EYEBROW_ROTATE, MAX_EYEBROW_SCALE, MAX_EYEBROW_X,
    MAX_EYEBROW_Y, MAX_EYE_ASPECT, MAX_EYE_ROTATE, MAX_EYE_SCALE, MAX_EYE_X, MAX_EYE_Y,
    MAX_GLASS_SCALE, MAX_GLASS_Y, MAX_HEIGHT, MAX_MOLE_SCALE, MAX_MOLE_X, MAX_MOLE_Y,
    MAX_MOUTH_ASPECT, MAX_MOUTH_SCALE, MAX_MOUTH_Y, MAX_MUSTACHE_SCALE, MAX_MUSTACHE_Y,
    MAX_NOSE_SCALE, MAX_NOSE_Y, MAX_REGION_MOVE, MAX_TYPE,
};
use crate::hle::service::mii::mii_util;
use crate::hle::service::mii::types::raw_data;

/// StoreDataBitFields: 7 packed u32 words containing all the Mii parameters.
/// Matches upstream StoreDataBitFields (0x1c bytes).
///
/// Bit layout per word matches upstream core_data.h exactly.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct StoreDataBitFields {
    words: [u32; 7],
}

const _: () = assert!(core::mem::size_of::<StoreDataBitFields>() == 0x1c);

/// Helper: extract bits [offset..offset+width) from a u32 word.
#[inline]
fn get_bits(word: u32, offset: u32, width: u32) -> u32 {
    (word >> offset) & ((1u32 << width) - 1)
}

/// Helper: set bits [offset..offset+width) of a u32 word to value.
#[inline]
fn set_bits(word: &mut u32, offset: u32, width: u32, value: u32) {
    let mask = ((1u32 << width) - 1) << offset;
    *word = (*word & !mask) | ((value & ((1u32 << width) - 1)) << offset);
}

impl StoreDataBitFields {
    // word_0 layout:
    //   [0:8]   hair_type
    //   [8:7]   height
    //   [15:1]  mole_type
    //   [16:7]  build
    //   [23:1]  hair_flip
    //   [24:7]  hair_color
    //   [31:1]  type

    pub fn hair_type(&self) -> u32 {
        get_bits(self.words[0], 0, 8)
    }
    pub fn set_hair_type(&mut self, v: u32) {
        set_bits(&mut self.words[0], 0, 8, v);
    }
    pub fn height(&self) -> u32 {
        get_bits(self.words[0], 8, 7)
    }
    pub fn set_height(&mut self, v: u32) {
        set_bits(&mut self.words[0], 8, 7, v);
    }
    pub fn mole_type(&self) -> u32 {
        get_bits(self.words[0], 15, 1)
    }
    pub fn set_mole_type(&mut self, v: u32) {
        set_bits(&mut self.words[0], 15, 1, v);
    }
    pub fn build(&self) -> u32 {
        get_bits(self.words[0], 16, 7)
    }
    pub fn set_build(&mut self, v: u32) {
        set_bits(&mut self.words[0], 16, 7, v);
    }
    pub fn hair_flip(&self) -> u32 {
        get_bits(self.words[0], 23, 1)
    }
    pub fn set_hair_flip(&mut self, v: u32) {
        set_bits(&mut self.words[0], 23, 1, v);
    }
    pub fn hair_color(&self) -> u32 {
        get_bits(self.words[0], 24, 7)
    }
    pub fn set_hair_color(&mut self, v: u32) {
        set_bits(&mut self.words[0], 24, 7, v);
    }
    pub fn type_val(&self) -> u32 {
        get_bits(self.words[0], 31, 1)
    }
    pub fn set_type_val(&mut self, v: u32) {
        set_bits(&mut self.words[0], 31, 1, v);
    }

    // word_1 layout:
    //   [0:7]   eye_color
    //   [7:1]   gender
    //   [8:7]   eyebrow_color
    //   [16:7]  mouth_color
    //   [24:7]  beard_color

    pub fn eye_color(&self) -> u32 {
        get_bits(self.words[1], 0, 7)
    }
    pub fn set_eye_color(&mut self, v: u32) {
        set_bits(&mut self.words[1], 0, 7, v);
    }
    pub fn gender(&self) -> u32 {
        get_bits(self.words[1], 7, 1)
    }
    pub fn set_gender(&mut self, v: u32) {
        set_bits(&mut self.words[1], 7, 1, v);
    }
    pub fn eyebrow_color(&self) -> u32 {
        get_bits(self.words[1], 8, 7)
    }
    pub fn set_eyebrow_color(&mut self, v: u32) {
        set_bits(&mut self.words[1], 8, 7, v);
    }
    pub fn mouth_color(&self) -> u32 {
        get_bits(self.words[1], 16, 7)
    }
    pub fn set_mouth_color(&mut self, v: u32) {
        set_bits(&mut self.words[1], 16, 7, v);
    }
    pub fn beard_color(&self) -> u32 {
        get_bits(self.words[1], 24, 7)
    }
    pub fn set_beard_color(&mut self, v: u32) {
        set_bits(&mut self.words[1], 24, 7, v);
    }

    // word_2 layout:
    //   [0:7]   glasses_color
    //   [8:6]   eye_type
    //   [14:2]  region_move
    //   [16:6]  mouth_type
    //   [22:2]  font_region
    //   [24:5]  eye_y
    //   [29:3]  glasses_scale

    pub fn glasses_color(&self) -> u32 {
        get_bits(self.words[2], 0, 7)
    }
    pub fn set_glasses_color(&mut self, v: u32) {
        set_bits(&mut self.words[2], 0, 7, v);
    }
    pub fn eye_type(&self) -> u32 {
        get_bits(self.words[2], 8, 6)
    }
    pub fn set_eye_type(&mut self, v: u32) {
        set_bits(&mut self.words[2], 8, 6, v);
    }
    pub fn region_move(&self) -> u32 {
        get_bits(self.words[2], 14, 2)
    }
    pub fn set_region_move(&mut self, v: u32) {
        set_bits(&mut self.words[2], 14, 2, v);
    }
    pub fn mouth_type(&self) -> u32 {
        get_bits(self.words[2], 16, 6)
    }
    pub fn set_mouth_type(&mut self, v: u32) {
        set_bits(&mut self.words[2], 16, 6, v);
    }
    pub fn font_region(&self) -> u32 {
        get_bits(self.words[2], 22, 2)
    }
    pub fn set_font_region(&mut self, v: u32) {
        set_bits(&mut self.words[2], 22, 2, v);
    }
    pub fn eye_y(&self) -> u32 {
        get_bits(self.words[2], 24, 5)
    }
    pub fn set_eye_y(&mut self, v: u32) {
        set_bits(&mut self.words[2], 24, 5, v);
    }
    pub fn glasses_scale(&self) -> u32 {
        get_bits(self.words[2], 29, 3)
    }
    pub fn set_glasses_scale(&mut self, v: u32) {
        set_bits(&mut self.words[2], 29, 3, v);
    }

    // word_3 layout:
    //   [0:5]   eyebrow_type
    //   [5:3]   mustache_type
    //   [8:5]   nose_type
    //   [13:3]  beard_type
    //   [16:5]  nose_y
    //   [21:3]  mouth_aspect
    //   [24:5]  mouth_y
    //   [29:3]  eyebrow_aspect

    pub fn eyebrow_type(&self) -> u32 {
        get_bits(self.words[3], 0, 5)
    }
    pub fn set_eyebrow_type(&mut self, v: u32) {
        set_bits(&mut self.words[3], 0, 5, v);
    }
    pub fn mustache_type(&self) -> u32 {
        get_bits(self.words[3], 5, 3)
    }
    pub fn set_mustache_type(&mut self, v: u32) {
        set_bits(&mut self.words[3], 5, 3, v);
    }
    pub fn nose_type(&self) -> u32 {
        get_bits(self.words[3], 8, 5)
    }
    pub fn set_nose_type(&mut self, v: u32) {
        set_bits(&mut self.words[3], 8, 5, v);
    }
    pub fn beard_type(&self) -> u32 {
        get_bits(self.words[3], 13, 3)
    }
    pub fn set_beard_type(&mut self, v: u32) {
        set_bits(&mut self.words[3], 13, 3, v);
    }
    pub fn nose_y(&self) -> u32 {
        get_bits(self.words[3], 16, 5)
    }
    pub fn set_nose_y(&mut self, v: u32) {
        set_bits(&mut self.words[3], 16, 5, v);
    }
    pub fn mouth_aspect(&self) -> u32 {
        get_bits(self.words[3], 21, 3)
    }
    pub fn set_mouth_aspect(&mut self, v: u32) {
        set_bits(&mut self.words[3], 21, 3, v);
    }
    pub fn mouth_y(&self) -> u32 {
        get_bits(self.words[3], 24, 5)
    }
    pub fn set_mouth_y(&mut self, v: u32) {
        set_bits(&mut self.words[3], 24, 5, v);
    }
    pub fn eyebrow_aspect(&self) -> u32 {
        get_bits(self.words[3], 29, 3)
    }
    pub fn set_eyebrow_aspect(&mut self, v: u32) {
        set_bits(&mut self.words[3], 29, 3, v);
    }

    // word_4 layout:
    //   [0:5]   mustache_y
    //   [5:3]   eye_rotate
    //   [8:5]   glasses_y
    //   [13:3]  eye_aspect
    //   [16:5]  mole_x
    //   [21:3]  eye_scale
    //   [24:5]  mole_y

    pub fn mustache_y(&self) -> u32 {
        get_bits(self.words[4], 0, 5)
    }
    pub fn set_mustache_y(&mut self, v: u32) {
        set_bits(&mut self.words[4], 0, 5, v);
    }
    pub fn eye_rotate(&self) -> u32 {
        get_bits(self.words[4], 5, 3)
    }
    pub fn set_eye_rotate(&mut self, v: u32) {
        set_bits(&mut self.words[4], 5, 3, v);
    }
    pub fn glasses_y(&self) -> u32 {
        get_bits(self.words[4], 8, 5)
    }
    pub fn set_glasses_y(&mut self, v: u32) {
        set_bits(&mut self.words[4], 8, 5, v);
    }
    pub fn eye_aspect(&self) -> u32 {
        get_bits(self.words[4], 13, 3)
    }
    pub fn set_eye_aspect(&mut self, v: u32) {
        set_bits(&mut self.words[4], 13, 3, v);
    }
    pub fn mole_x(&self) -> u32 {
        get_bits(self.words[4], 16, 5)
    }
    pub fn set_mole_x(&mut self, v: u32) {
        set_bits(&mut self.words[4], 16, 5, v);
    }
    pub fn eye_scale(&self) -> u32 {
        get_bits(self.words[4], 21, 3)
    }
    pub fn set_eye_scale(&mut self, v: u32) {
        set_bits(&mut self.words[4], 21, 3, v);
    }
    pub fn mole_y(&self) -> u32 {
        get_bits(self.words[4], 24, 5)
    }
    pub fn set_mole_y(&mut self, v: u32) {
        set_bits(&mut self.words[4], 24, 5, v);
    }

    // word_5 layout:
    //   [0:5]   glasses_type
    //   [8:4]   favorite_color
    //   [12:4]  faceline_type
    //   [16:4]  faceline_color
    //   [20:4]  faceline_wrinkle
    //   [24:4]  faceline_makeup
    //   [28:4]  eye_x

    pub fn glasses_type(&self) -> u32 {
        get_bits(self.words[5], 0, 5)
    }
    pub fn set_glasses_type(&mut self, v: u32) {
        set_bits(&mut self.words[5], 0, 5, v);
    }
    pub fn favorite_color(&self) -> u32 {
        get_bits(self.words[5], 8, 4)
    }
    pub fn set_favorite_color(&mut self, v: u32) {
        set_bits(&mut self.words[5], 8, 4, v);
    }
    pub fn faceline_type(&self) -> u32 {
        get_bits(self.words[5], 12, 4)
    }
    pub fn set_faceline_type(&mut self, v: u32) {
        set_bits(&mut self.words[5], 12, 4, v);
    }
    pub fn faceline_color(&self) -> u32 {
        get_bits(self.words[5], 16, 4)
    }
    pub fn set_faceline_color(&mut self, v: u32) {
        set_bits(&mut self.words[5], 16, 4, v);
    }
    pub fn faceline_wrinkle(&self) -> u32 {
        get_bits(self.words[5], 20, 4)
    }
    pub fn set_faceline_wrinkle(&mut self, v: u32) {
        set_bits(&mut self.words[5], 20, 4, v);
    }
    pub fn faceline_makeup(&self) -> u32 {
        get_bits(self.words[5], 24, 4)
    }
    pub fn set_faceline_makeup(&mut self, v: u32) {
        set_bits(&mut self.words[5], 24, 4, v);
    }
    pub fn eye_x(&self) -> u32 {
        get_bits(self.words[5], 28, 4)
    }
    pub fn set_eye_x(&mut self, v: u32) {
        set_bits(&mut self.words[5], 28, 4, v);
    }

    // word_6 layout:
    //   [0:4]   eyebrow_scale
    //   [4:4]   eyebrow_rotate
    //   [8:4]   eyebrow_x
    //   [12:4]  eyebrow_y
    //   [16:4]  nose_scale
    //   [20:4]  mouth_scale
    //   [24:4]  mustache_scale
    //   [28:4]  mole_scale

    pub fn eyebrow_scale(&self) -> u32 {
        get_bits(self.words[6], 0, 4)
    }
    pub fn set_eyebrow_scale(&mut self, v: u32) {
        set_bits(&mut self.words[6], 0, 4, v);
    }
    pub fn eyebrow_rotate(&self) -> u32 {
        get_bits(self.words[6], 4, 4)
    }
    pub fn set_eyebrow_rotate(&mut self, v: u32) {
        set_bits(&mut self.words[6], 4, 4, v);
    }
    pub fn eyebrow_x(&self) -> u32 {
        get_bits(self.words[6], 8, 4)
    }
    pub fn set_eyebrow_x(&mut self, v: u32) {
        set_bits(&mut self.words[6], 8, 4, v);
    }
    pub fn eyebrow_y(&self) -> u32 {
        get_bits(self.words[6], 12, 4)
    }
    pub fn set_eyebrow_y(&mut self, v: u32) {
        set_bits(&mut self.words[6], 12, 4, v);
    }
    pub fn nose_scale(&self) -> u32 {
        get_bits(self.words[6], 16, 4)
    }
    pub fn set_nose_scale(&mut self, v: u32) {
        set_bits(&mut self.words[6], 16, 4, v);
    }
    pub fn mouth_scale(&self) -> u32 {
        get_bits(self.words[6], 20, 4)
    }
    pub fn set_mouth_scale(&mut self, v: u32) {
        set_bits(&mut self.words[6], 20, 4, v);
    }
    pub fn mustache_scale(&self) -> u32 {
        get_bits(self.words[6], 24, 4)
    }
    pub fn set_mustache_scale(&mut self, v: u32) {
        set_bits(&mut self.words[6], 24, 4, v);
    }
    pub fn mole_scale(&self) -> u32 {
        get_bits(self.words[6], 28, 4)
    }
    pub fn set_mole_scale(&mut self, v: u32) {
        set_bits(&mut self.words[6], 28, 4, v);
    }
}

impl Default for StoreDataBitFields {
    fn default() -> Self {
        Self { words: [0u32; 7] }
    }
}

/// CoreData stores the essential Mii parameters as bit-packed fields.
///
/// Layout matches upstream:
///   StoreDataBitFields data (0x1c bytes)
///   Nickname name (0x14 bytes)
///   Total: 0x30 bytes
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CoreData {
    pub data: StoreDataBitFields,
    pub name: Nickname,
}

const _: () = assert!(core::mem::size_of::<CoreData>() == 0x30);

impl CoreData {
    pub fn new() -> Self {
        Self {
            data: StoreDataBitFields::default(),
            name: Nickname::default(),
        }
    }

    /// Matches upstream CoreData::SetDefault.
    pub fn set_default(&mut self) {
        self.data = StoreDataBitFields::default();
        self.name = self.get_default_nickname();
    }

    /// Matches upstream CoreData::BuildRandom.
    pub fn build_random(&mut self, mut age: Age, mut gender: Gender, mut race: Race) {
        if gender == Gender::All {
            gender = unsafe {
                core::mem::transmute(mii_util::get_random_value::<u64>(0, Gender::MAX as u64) as u8)
            };
        }

        if age == Age::All {
            let random = mii_util::get_random_value::<u64>(0, 9) as i32;
            if random >= 8 {
                age = Age::Old;
            } else if random >= 4 {
                age = Age::Normal;
            } else {
                age = Age::Young;
            }
        }

        if race == Race::All {
            let random = mii_util::get_random_value::<u64>(0, 9) as i32;
            if random >= 8 {
                race = Race::Black;
            } else if random >= 4 {
                race = Race::White;
            } else {
                race = Race::Asian;
            }
        }

        self.set_default();
        self.set_gender(gender);
        self.set_favorite_color(unsafe {
            core::mem::transmute(
                mii_util::get_random_value::<u64>(0, FavoriteColor::MAX as u64) as u8,
            )
        });
        self.set_region_move(0);
        self.set_font_region(FontRegion::Standard);
        self.set_type(0);
        self.set_height(64);
        self.set_build(64);

        let mut axis_y: u32 = 0;
        if gender == Gender::Female && age == Age::Young {
            axis_y = mii_util::get_random_value::<u64>(0, 2) as u32;
        }

        let index: usize = 3 * (age as usize) + 9 * (gender as usize) + (race as usize);

        let faceline_type_info = &raw_data::RANDOM_MII_FACELINE[index];
        let faceline_color_info =
            &raw_data::RANDOM_MII_FACELINE_COLOR[3 * (gender as usize) + (race as usize)];
        let faceline_wrinkle_info = &raw_data::RANDOM_MII_FACELINE_WRINKLE[index];
        let faceline_makeup_info = &raw_data::RANDOM_MII_FACELINE_MAKEUP[index];
        let hair_type_info = &raw_data::RANDOM_MII_HAIR_TYPE[index];
        let hair_color_info =
            &raw_data::RANDOM_MII_HAIR_COLOR[3 * (race as usize) + (age as usize)];
        let eye_type_info = &raw_data::RANDOM_MII_EYE_TYPE[index];
        let eye_color_info = &raw_data::RANDOM_MII_EYE_COLOR[race as usize];
        let eyebrow_type_info = &raw_data::RANDOM_MII_EYEBROW_TYPE[index];
        let nose_type_info = &raw_data::RANDOM_MII_NOSE_TYPE[index];
        let mouth_type_info = &raw_data::RANDOM_MII_MOUTH_TYPE[index];
        let glasses_type_info = &raw_data::RANDOM_MII_GLASS_TYPE[age as usize];

        self.data.set_faceline_type(
            faceline_type_info.values[mii_util::get_random_value::<u64>(
                0,
                faceline_type_info.values_count as u64 - 1,
            ) as usize],
        );
        self.data.set_faceline_color(
            faceline_color_info.values[mii_util::get_random_value::<u64>(
                0,
                faceline_color_info.values_count as u64 - 1,
            ) as usize],
        );
        self.data.set_faceline_wrinkle(
            faceline_wrinkle_info.values[mii_util::get_random_value::<u64>(
                0,
                faceline_wrinkle_info.values_count as u64 - 1,
            ) as usize],
        );
        self.data.set_faceline_makeup(
            faceline_makeup_info.values[mii_util::get_random_value::<u64>(
                0,
                faceline_makeup_info.values_count as u64 - 1,
            ) as usize],
        );

        self.data.set_hair_type(
            hair_type_info.values[mii_util::get_random_value::<u64>(
                0,
                hair_type_info.values_count as u64 - 1,
            ) as usize],
        );
        self.set_hair_color(raw_data::get_hair_color_from_ver3(
            hair_color_info.values[mii_util::get_random_value::<u64>(
                0,
                hair_color_info.values_count as u64 - 1,
            ) as usize],
        ) as u32);
        self.set_hair_flip(unsafe {
            core::mem::transmute(mii_util::get_random_value::<u64>(0, HairFlip::MAX as u64) as u8)
        });

        self.data.set_eye_type(
            eye_type_info.values[mii_util::get_random_value::<u64>(
                0,
                eye_type_info.values_count as u64 - 1,
            ) as usize],
        );

        let eye_rotate_1: usize = if gender != Gender::Male { 4 } else { 2 };
        let eye_rotate_2: i32 = if gender != Gender::Male { 3 } else { 4 };
        let eye_rotate_offset =
            32i32 - (raw_data::EYE_ROTATE_LOOKUP[eye_rotate_1] as i32) + eye_rotate_2;
        let eye_rotate =
            32i32 - (raw_data::EYE_ROTATE_LOOKUP[self.data.eye_type() as usize] as i32);

        self.set_eye_color(raw_data::get_eye_color_from_ver3(
            eye_color_info.values[mii_util::get_random_value::<u64>(
                0,
                eye_color_info.values_count as u64 - 1,
            ) as usize],
        ) as u32);
        self.set_eye_scale(4);
        self.set_eye_aspect(3);
        self.set_eye_rotate((eye_rotate_offset - eye_rotate) as u8);
        self.set_eye_x(2);
        self.set_eye_y((axis_y + 12) as u8);

        self.data.set_eyebrow_type(
            eyebrow_type_info.values[mii_util::get_random_value::<u64>(
                0,
                eyebrow_type_info.values_count as u64 - 1,
            ) as usize],
        );

        let eyebrow_rotate_1: usize = if race == Race::Asian { 6 } else { 0 };
        let eyebrow_y_val: u32 = if race == Race::Asian { 6 } else { 7 };
        let eyebrow_rotate_offset =
            32i32 - (raw_data::EYEBROW_ROTATE_LOOKUP[eyebrow_rotate_1] as i32) + 6;
        let eyebrow_rotate =
            32i32 - (raw_data::EYEBROW_ROTATE_LOOKUP[self.data.eyebrow_type() as usize] as i32);

        self.set_eyebrow_color_raw(self.get_hair_color_raw());
        self.set_eyebrow_scale(4);
        self.set_eyebrow_aspect(3);
        self.set_eyebrow_rotate((eyebrow_rotate_offset - eyebrow_rotate) as u8);
        self.set_eyebrow_x(2);
        self.set_eyebrow_y((axis_y + eyebrow_y_val) as u8);

        self.data.set_nose_type(
            nose_type_info.values[mii_util::get_random_value::<u64>(
                0,
                nose_type_info.values_count as u64 - 1,
            ) as usize],
        );
        self.set_nose_scale(if gender == Gender::Female { 3 } else { 4 });
        self.set_nose_y((axis_y + 9) as u8);

        let mouth_color_val: i32 = if gender == Gender::Female {
            mii_util::get_random_value::<u64>(0, 3) as i32
        } else {
            0
        };

        self.data.set_mouth_type(
            mouth_type_info.values[mii_util::get_random_value::<u64>(
                0,
                mouth_type_info.values_count as u64 - 1,
            ) as usize],
        );
        self.set_mouth_color(raw_data::get_mouth_color_from_ver3(mouth_color_val as u32) as u32);
        self.set_mouth_scale(4);
        self.set_mouth_aspect(3);
        self.set_mouth_y((axis_y + 13) as u8);

        self.set_beard_color_raw(self.get_hair_color_raw());
        self.set_mustache_scale(4);

        if gender == Gender::Male
            && age != Age::Young
            && (mii_util::get_random_value::<u64>(0, 9) as i32) < 2
        {
            let mustache_and_beard_flag = BeardAndMustacheFlag::from_bits_truncate(
                mii_util::get_random_value::<u64>(0, BeardAndMustacheFlag::ALL.bits() as u64)
                    as u32,
            );

            let mut beard_type = BeardType::None;
            let mut mustache_type = MustacheType::None;

            if mustache_and_beard_flag.contains(BeardAndMustacheFlag::BEARD) {
                beard_type = unsafe {
                    core::mem::transmute(mii_util::get_random_value::<u64>(
                        BeardType::MIN as u64,
                        BeardType::MAX as u64,
                    ) as u8)
                };
            }

            if mustache_and_beard_flag.contains(BeardAndMustacheFlag::MUSTACHE) {
                mustache_type = unsafe {
                    core::mem::transmute(mii_util::get_random_value::<u64>(
                        MustacheType::MIN as u64,
                        MustacheType::MAX as u64,
                    ) as u8)
                };
            }

            self.set_mustache_type_enum(mustache_type);
            self.set_beard_type_enum(beard_type);
            self.set_mustache_y(10);
        } else {
            self.set_mustache_type_enum(MustacheType::None);
            self.set_beard_type_enum(BeardType::None);
            self.set_mustache_y((axis_y + 10) as u8);
        }

        let glasses_type_start = mii_util::get_random_value::<u64>(0, 99) as usize;
        let mut glasses_type: u8 = 0;
        while glasses_type_start < glasses_type_info.values[glasses_type as usize] as usize {
            glasses_type += 1;
            if glasses_type as u32 >= glasses_type_info.values_count {
                glasses_type = 0;
                break;
            }
        }

        self.set_glass_type(unsafe { core::mem::transmute(glasses_type) });
        self.set_glass_color(raw_data::get_glass_color_from_ver3(0) as u32);
        self.set_glass_scale(4);
        self.set_glass_y((axis_y + 10) as u8);

        self.set_mole_type_enum(MoleType::None);
        self.set_mole_scale(4);
        self.set_mole_x(2);
        self.set_mole_y(20);
    }

    // --- Setters matching upstream CoreData setters ---

    pub fn set_font_region(&mut self, value: FontRegion) {
        self.data.set_font_region(value as u32);
    }

    pub fn set_favorite_color(&mut self, value: FavoriteColor) {
        self.data.set_favorite_color(value as u32);
    }

    pub fn set_gender(&mut self, value: Gender) {
        self.data.set_gender(value as u32);
    }

    pub fn set_height(&mut self, value: u8) {
        self.data.set_height(value as u32);
    }

    pub fn set_build(&mut self, value: u8) {
        self.data.set_build(value as u32);
    }

    pub fn set_type(&mut self, value: u8) {
        self.data.set_type_val(value as u32);
    }

    pub fn set_region_move(&mut self, value: u8) {
        self.data.set_region_move(value as u32);
    }

    pub fn set_faceline_type(&mut self, value: FacelineType) {
        self.data.set_faceline_type(value as u32);
    }

    pub fn set_faceline_color(&mut self, value: FacelineColor) {
        self.data.set_faceline_color(value as u32);
    }

    pub fn set_faceline_wrinkle(&mut self, value: FacelineWrinkle) {
        self.data.set_faceline_wrinkle(value as u32);
    }

    pub fn set_faceline_make(&mut self, value: FacelineMake) {
        self.data.set_faceline_makeup(value as u32);
    }

    pub fn set_hair_type(&mut self, value: HairType) {
        self.data.set_hair_type(value as u32);
    }

    pub fn set_hair_color(&mut self, value: u32) {
        self.data.set_hair_color(value);
    }

    pub fn set_hair_flip(&mut self, value: HairFlip) {
        self.data.set_hair_flip(value as u32);
    }

    pub fn set_eye_type(&mut self, value: EyeType) {
        self.data.set_eye_type(value as u32);
    }

    pub fn set_eye_color(&mut self, value: u32) {
        self.data.set_eye_color(value);
    }

    pub fn set_eye_scale(&mut self, value: u8) {
        self.data.set_eye_scale(value as u32);
    }

    pub fn set_eye_aspect(&mut self, value: u8) {
        self.data.set_eye_aspect(value as u32);
    }

    pub fn set_eye_rotate(&mut self, value: u8) {
        self.data.set_eye_rotate(value as u32);
    }

    pub fn set_eye_x(&mut self, value: u8) {
        self.data.set_eye_x(value as u32);
    }

    pub fn set_eye_y(&mut self, value: u8) {
        self.data.set_eye_y(value as u32);
    }

    pub fn set_eyebrow_type(&mut self, value: EyebrowType) {
        self.data.set_eyebrow_type(value as u32);
    }

    pub fn set_eyebrow_color_raw(&mut self, value: u32) {
        self.data.set_eyebrow_color(value);
    }

    pub fn set_eyebrow_color(&mut self, value: u32) {
        self.data.set_eyebrow_color(value);
    }

    pub fn set_eyebrow_scale(&mut self, value: u8) {
        self.data.set_eyebrow_scale(value as u32);
    }

    pub fn set_eyebrow_aspect(&mut self, value: u8) {
        self.data.set_eyebrow_aspect(value as u32);
    }

    pub fn set_eyebrow_rotate(&mut self, value: u8) {
        self.data.set_eyebrow_rotate(value as u32);
    }

    pub fn set_eyebrow_x(&mut self, value: u8) {
        self.data.set_eyebrow_x(value as u32);
    }

    pub fn set_eyebrow_y(&mut self, value: u8) {
        self.data.set_eyebrow_y(value as u32);
    }

    pub fn set_nose_type(&mut self, value: NoseType) {
        self.data.set_nose_type(value as u32);
    }

    pub fn set_nose_scale(&mut self, value: u8) {
        self.data.set_nose_scale(value as u32);
    }

    pub fn set_nose_y(&mut self, value: u8) {
        self.data.set_nose_y(value as u32);
    }

    pub fn set_mouth_type(&mut self, value: MouthType) {
        self.data.set_mouth_type(value as u32);
    }

    pub fn set_mouth_color(&mut self, value: u32) {
        self.data.set_mouth_color(value);
    }

    pub fn set_mouth_scale(&mut self, value: u8) {
        self.data.set_mouth_scale(value as u32);
    }

    pub fn set_mouth_aspect(&mut self, value: u8) {
        self.data.set_mouth_aspect(value as u32);
    }

    pub fn set_mouth_y(&mut self, value: u8) {
        self.data.set_mouth_y(value as u32);
    }

    pub fn set_beard_color_raw(&mut self, value: u32) {
        self.data.set_beard_color(value);
    }

    pub fn set_beard_color(&mut self, value: u32) {
        self.data.set_beard_color(value);
    }

    pub fn set_beard_type_enum(&mut self, value: BeardType) {
        self.data.set_beard_type(value as u32);
    }

    pub fn set_mustache_type_enum(&mut self, value: MustacheType) {
        self.data.set_mustache_type(value as u32);
    }

    pub fn set_mustache_scale(&mut self, value: u8) {
        self.data.set_mustache_scale(value as u32);
    }

    pub fn set_mustache_y(&mut self, value: u8) {
        self.data.set_mustache_y(value as u32);
    }

    pub fn set_glass_type(&mut self, value: GlassType) {
        self.data.set_glasses_type(value as u32);
    }

    pub fn set_glass_color(&mut self, value: u32) {
        self.data.set_glasses_color(value);
    }

    pub fn set_glass_scale(&mut self, value: u8) {
        self.data.set_glasses_scale(value as u32);
    }

    pub fn set_glass_y(&mut self, value: u8) {
        self.data.set_glasses_y(value as u32);
    }

    pub fn set_mole_type_enum(&mut self, value: MoleType) {
        self.data.set_mole_type(value as u32);
    }

    pub fn set_mole_scale(&mut self, value: u8) {
        self.data.set_mole_scale(value as u32);
    }

    pub fn set_mole_x(&mut self, value: u8) {
        self.data.set_mole_x(value as u32);
    }

    pub fn set_mole_y(&mut self, value: u8) {
        self.data.set_mole_y(value as u32);
    }

    pub fn set_nickname(&mut self, nickname: Nickname) {
        self.name = nickname;
    }

    // --- Getters matching upstream CoreData getters ---

    pub fn get_font_region(&self) -> FontRegion {
        unsafe { core::mem::transmute(self.data.font_region() as u8) }
    }

    pub fn get_favorite_color(&self) -> FavoriteColor {
        unsafe { core::mem::transmute(self.data.favorite_color() as u8) }
    }

    pub fn get_gender(&self) -> Gender {
        unsafe { core::mem::transmute(self.data.gender() as u8) }
    }

    pub fn get_height(&self) -> u8 {
        self.data.height() as u8
    }

    pub fn get_build(&self) -> u8 {
        self.data.build() as u8
    }

    pub fn get_type(&self) -> u8 {
        self.data.type_val() as u8
    }

    pub fn get_region_move(&self) -> u8 {
        self.data.region_move() as u8
    }

    pub fn get_faceline_type(&self) -> FacelineType {
        unsafe { core::mem::transmute(self.data.faceline_type() as u8) }
    }

    pub fn get_faceline_color(&self) -> FacelineColor {
        unsafe { core::mem::transmute(self.data.faceline_color() as u8) }
    }

    pub fn get_faceline_wrinkle(&self) -> FacelineWrinkle {
        unsafe { core::mem::transmute(self.data.faceline_wrinkle() as u8) }
    }

    pub fn get_faceline_make(&self) -> FacelineMake {
        unsafe { core::mem::transmute(self.data.faceline_makeup() as u8) }
    }

    pub fn get_hair_type(&self) -> HairType {
        unsafe { core::mem::transmute(self.data.hair_type() as u8) }
    }

    pub fn get_hair_color_raw(&self) -> u32 {
        self.data.hair_color()
    }

    pub fn get_hair_flip(&self) -> HairFlip {
        unsafe { core::mem::transmute(self.data.hair_flip() as u8) }
    }

    pub fn get_eye_type(&self) -> EyeType {
        unsafe { core::mem::transmute(self.data.eye_type() as u8) }
    }

    pub fn get_eye_color_raw(&self) -> u32 {
        self.data.eye_color()
    }

    pub fn get_eye_scale(&self) -> u8 {
        self.data.eye_scale() as u8
    }

    pub fn get_eye_aspect(&self) -> u8 {
        self.data.eye_aspect() as u8
    }

    pub fn get_eye_rotate(&self) -> u8 {
        self.data.eye_rotate() as u8
    }

    pub fn get_eye_x(&self) -> u8 {
        self.data.eye_x() as u8
    }

    pub fn get_eye_y(&self) -> u8 {
        self.data.eye_y() as u8
    }

    pub fn get_eyebrow_type(&self) -> EyebrowType {
        unsafe { core::mem::transmute(self.data.eyebrow_type() as u8) }
    }

    pub fn get_eyebrow_color_raw(&self) -> u32 {
        self.data.eyebrow_color()
    }

    pub fn get_eyebrow_scale(&self) -> u8 {
        self.data.eyebrow_scale() as u8
    }

    pub fn get_eyebrow_aspect(&self) -> u8 {
        self.data.eyebrow_aspect() as u8
    }

    pub fn get_eyebrow_rotate(&self) -> u8 {
        self.data.eyebrow_rotate() as u8
    }

    pub fn get_eyebrow_x(&self) -> u8 {
        self.data.eyebrow_x() as u8
    }

    pub fn get_eyebrow_y(&self) -> u8 {
        self.data.eyebrow_y() as u8
    }

    pub fn get_nose_type(&self) -> NoseType {
        unsafe { core::mem::transmute(self.data.nose_type() as u8) }
    }

    pub fn get_nose_scale(&self) -> u8 {
        self.data.nose_scale() as u8
    }

    pub fn get_nose_y(&self) -> u8 {
        self.data.nose_y() as u8
    }

    pub fn get_mouth_type(&self) -> MouthType {
        unsafe { core::mem::transmute(self.data.mouth_type() as u8) }
    }

    pub fn get_mouth_color_raw(&self) -> u32 {
        self.data.mouth_color()
    }

    pub fn get_mouth_scale(&self) -> u8 {
        self.data.mouth_scale() as u8
    }

    pub fn get_mouth_aspect(&self) -> u8 {
        self.data.mouth_aspect() as u8
    }

    pub fn get_mouth_y(&self) -> u8 {
        self.data.mouth_y() as u8
    }

    pub fn get_beard_color_raw(&self) -> u32 {
        self.data.beard_color()
    }

    pub fn get_beard_type(&self) -> BeardType {
        unsafe { core::mem::transmute(self.data.beard_type() as u8) }
    }

    pub fn get_mustache_type(&self) -> MustacheType {
        unsafe { core::mem::transmute(self.data.mustache_type() as u8) }
    }

    pub fn get_mustache_scale(&self) -> u8 {
        self.data.mustache_scale() as u8
    }

    pub fn get_mustache_y(&self) -> u8 {
        self.data.mustache_y() as u8
    }

    pub fn get_glass_type(&self) -> GlassType {
        unsafe { core::mem::transmute(self.data.glasses_type() as u8) }
    }

    pub fn get_glass_color_raw(&self) -> u32 {
        self.data.glasses_color()
    }

    pub fn get_glass_scale(&self) -> u8 {
        self.data.glasses_scale() as u8
    }

    pub fn get_glass_y(&self) -> u8 {
        self.data.glasses_y() as u8
    }

    pub fn get_mole_type(&self) -> MoleType {
        unsafe { core::mem::transmute(self.data.mole_type() as u8) }
    }

    pub fn get_mole_scale(&self) -> u8 {
        self.data.mole_scale() as u8
    }

    pub fn get_mole_x(&self) -> u8 {
        self.data.mole_x() as u8
    }

    pub fn get_mole_y(&self) -> u8 {
        self.data.mole_y() as u8
    }

    pub fn get_nickname(&self) -> Nickname {
        self.name
    }

    pub fn get_default_nickname(&self) -> Nickname {
        // 'n', 'o', ' ', 'n', 'a', 'm', 'e'
        let mut name = Nickname::default();
        name.data[0] = b'n' as u16;
        name.data[1] = b'o' as u16;
        name.data[2] = b' ' as u16;
        name.data[3] = b'n' as u16;
        name.data[4] = b'a' as u16;
        name.data[5] = b'm' as u16;
        name.data[6] = b'e' as u16;
        name
    }

    pub fn get_invalid_nickname(&self) -> Nickname {
        let mut name = Nickname::default();
        name.data[0] = b'?' as u16;
        name.data[1] = b'?' as u16;
        name.data[2] = b'?' as u16;
        name
    }

    /// Matches upstream CoreData::IsValid.
    pub fn is_valid(&self) -> ValidationResult {
        if !self.name.is_valid() {
            return ValidationResult::InvalidName;
        }
        if self.get_font_region() > FontRegion::MAX {
            return ValidationResult::InvalidFont;
        }
        if self.get_favorite_color() > FavoriteColor::MAX {
            return ValidationResult::InvalidColor;
        }
        if self.get_gender() > Gender::MAX {
            return ValidationResult::InvalidGender;
        }
        if self.get_height() > MAX_HEIGHT {
            return ValidationResult::InvalidHeight;
        }
        if self.get_build() > MAX_BUILD {
            return ValidationResult::InvalidBuild;
        }
        if self.get_type() > MAX_TYPE {
            return ValidationResult::InvalidType;
        }
        if self.get_region_move() > MAX_REGION_MOVE {
            return ValidationResult::InvalidRegionMove;
        }
        if self.get_faceline_type() > FacelineType::MAX {
            return ValidationResult::InvalidFacelineType;
        }
        if (self.data.faceline_color() as u8) > FacelineColor::MAX {
            return ValidationResult::InvalidFacelineColor;
        }
        if self.get_faceline_wrinkle() > FacelineWrinkle::MAX {
            return ValidationResult::InvalidFacelineWrinkle;
        }
        if self.get_faceline_make() > FacelineMake::MAX {
            return ValidationResult::InvalidFacelineMake;
        }
        if (self.data.hair_type() as u8) > HairType::MAX_U8 {
            return ValidationResult::InvalidHairType;
        }
        if (self.data.hair_color() as u8) > CommonColor::Max as u8 {
            return ValidationResult::InvalidHairColor;
        }
        if self.get_hair_flip() > HairFlip::MAX {
            return ValidationResult::InvalidHairFlip;
        }
        if self.get_eye_type() > EyeType::MAX {
            return ValidationResult::InvalidEyeType;
        }
        if (self.data.eye_color() as u8) > CommonColor::Max as u8 {
            return ValidationResult::InvalidEyeColor;
        }
        if self.get_eye_scale() > MAX_EYE_SCALE {
            return ValidationResult::InvalidEyeScale;
        }
        if self.get_eye_aspect() > MAX_EYE_ASPECT {
            return ValidationResult::InvalidEyeAspect;
        }
        if self.get_eye_rotate() > MAX_EYE_ROTATE {
            return ValidationResult::InvalidEyeRotate;
        }
        if self.get_eye_x() > MAX_EYE_X {
            return ValidationResult::InvalidEyeX;
        }
        if self.get_eye_y() > MAX_EYE_Y {
            return ValidationResult::InvalidEyeY;
        }
        if self.get_eyebrow_type() > EyebrowType::MAX {
            return ValidationResult::InvalidEyebrowType;
        }
        if (self.data.eyebrow_color() as u8) > CommonColor::Max as u8 {
            return ValidationResult::InvalidEyebrowColor;
        }
        if self.get_eyebrow_scale() > MAX_EYEBROW_SCALE {
            return ValidationResult::InvalidEyebrowScale;
        }
        if self.get_eyebrow_aspect() > MAX_EYEBROW_ASPECT {
            return ValidationResult::InvalidEyebrowAspect;
        }
        if self.get_eyebrow_rotate() > MAX_EYEBROW_ROTATE {
            return ValidationResult::InvalidEyebrowRotate;
        }
        if self.get_eyebrow_x() > MAX_EYEBROW_X {
            return ValidationResult::InvalidEyebrowX;
        }
        if self.get_eyebrow_y() > MAX_EYEBROW_Y {
            return ValidationResult::InvalidEyebrowY;
        }
        if self.get_nose_type() > NoseType::MAX {
            return ValidationResult::InvalidNoseType;
        }
        if self.get_nose_scale() > MAX_NOSE_SCALE {
            return ValidationResult::InvalidNoseScale;
        }
        if self.get_nose_y() > MAX_NOSE_Y {
            return ValidationResult::InvalidNoseY;
        }
        if self.get_mouth_type() > MouthType::MAX {
            return ValidationResult::InvalidMouthType;
        }
        if (self.data.mouth_color() as u8) > CommonColor::Max as u8 {
            return ValidationResult::InvalidMouthColor;
        }
        if self.get_mouth_scale() > MAX_MOUTH_SCALE {
            return ValidationResult::InvalidMouthScale;
        }
        if self.get_mouth_aspect() > MAX_MOUTH_ASPECT {
            return ValidationResult::InvalidMouthAspect;
        }
        if self.get_mouth_y() > MAX_MOUTH_Y {
            return ValidationResult::InvalidMouthY;
        }
        if (self.data.beard_color() as u8) > CommonColor::Max as u8 {
            return ValidationResult::InvalidBeardColor;
        }
        if self.get_beard_type() > BeardType::MAX {
            return ValidationResult::InvalidBeardType;
        }
        if self.get_mustache_type() > MustacheType::MAX {
            return ValidationResult::InvalidMustacheType;
        }
        if self.get_mustache_scale() > MAX_MUSTACHE_SCALE {
            return ValidationResult::InvalidMustacheScale;
        }
        if self.get_mustache_y() > MAX_MUSTACHE_Y {
            return ValidationResult::InvalidMustacheY;
        }
        if (self.data.glasses_type() as u8) > GlassType::MAX {
            return ValidationResult::InvalidGlassType;
        }
        if (self.data.glasses_color() as u8) > CommonColor::Max as u8 {
            return ValidationResult::InvalidGlassColor;
        }
        if self.get_glass_scale() > MAX_GLASS_SCALE {
            return ValidationResult::InvalidGlassScale;
        }
        if self.get_glass_y() > MAX_GLASS_Y {
            return ValidationResult::InvalidGlassY;
        }
        if self.get_mole_type() > MoleType::MAX {
            return ValidationResult::InvalidMoleType;
        }
        if self.get_mole_scale() > MAX_MOLE_SCALE {
            return ValidationResult::InvalidMoleScale;
        }
        if self.get_mole_x() > MAX_MOLE_X {
            return ValidationResult::InvalidMoleX;
        }
        if self.get_mole_y() > MAX_MOLE_Y {
            return ValidationResult::InvalidMoleY;
        }
        ValidationResult::NoErrors
    }
}

impl Default for CoreData {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_core_data_size() {
        assert_eq!(core::mem::size_of::<CoreData>(), 0x30);
    }

    #[test]
    fn test_bitfield_round_trip() {
        let mut cd = CoreData::new();
        cd.set_height(64);
        assert_eq!(cd.get_height(), 64);

        cd.set_build(100);
        assert_eq!(cd.get_build(), 100);

        cd.set_gender(Gender::Female);
        assert_eq!(cd.get_gender(), Gender::Female);

        cd.set_eye_scale(7);
        assert_eq!(cd.get_eye_scale(), 7);

        cd.set_faceline_type(FacelineType::Rounded);
        assert_eq!(cd.get_faceline_type(), FacelineType::Rounded);

        // Make sure fields don't interfere with each other
        assert_eq!(cd.get_height(), 64);
        assert_eq!(cd.get_build(), 100);
    }

    #[test]
    fn test_set_default() {
        let mut cd = CoreData::new();
        cd.set_default();
        assert_eq!(cd.name.data[0], b'n' as u16);
        assert_eq!(cd.name.data[6], b'e' as u16);
    }
}
