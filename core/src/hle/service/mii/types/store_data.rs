// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/types/store_data.h
//! Port of zuyu/src/core/hle/service/mii/types/store_data.cpp
//!
//! StoreData: the on-disk Mii storage format.
//! Size: 0x44 bytes in upstream.

use super::char_info::CharInfo;
use super::core_data::CoreData;
use super::raw_data;
use crate::hle::result::ResultCode;
use crate::hle::service::mii::mii_result::RESULT_NOT_UPDATED;
use crate::hle::service::mii::mii_types::Source;
use crate::hle::service::mii::mii_types::{
    Age, BeardType, CommonColor, EyeType, EyebrowType, FacelineColor, FacelineMake, FacelineType,
    FacelineWrinkle, FavoriteColor, FontRegion, Gender, GlassType, HairFlip, HairType, MoleType,
    MouthType, MustacheType, Nickname, NoseType, Race, ValidationResult,
};
use crate::hle::service::mii::mii_util;

/// StoreData is the serialized form of a Mii stored in the database.
///
/// Layout:
/// - core_data: CoreData (0x30 bytes)
/// - create_id: u128 (0x10 bytes)
/// - data_crc: u16
/// - device_crc: u16
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct StoreData {
    pub core_data: CoreData,
    /// Create ID stored as raw bytes to avoid u128 alignment padding.
    pub create_id: [u8; 16],
    pub data_crc: u16,
    pub device_crc: u16,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct StoreDataElement {
    pub store_data: StoreData,
    pub source: Source,
}

impl StoreData {
    pub fn new() -> Self {
        Self {
            core_data: CoreData::default(),
            create_id: [0u8; 16],
            data_crc: 0,
            device_crc: 0,
        }
    }

    pub fn create_id_as_u128(&self) -> u128 {
        u128::from_le_bytes(self.create_id)
    }

    pub fn set_create_id(&mut self, id: u128) {
        self.create_id = id.to_le_bytes();
    }

    /// Matches upstream StoreData::BuildDefault.
    pub fn build_default(&mut self, mii_index: u32) {
        let default_mii = &raw_data::DEFAULT_MII[mii_index as usize];
        self.core_data.set_default();

        self.core_data
            .set_faceline_type(unsafe { core::mem::transmute(default_mii.face_type as u8) });
        self.core_data
            .set_faceline_color(raw_data::get_faceline_color_from_ver3(
                default_mii.face_color,
            ));
        self.core_data
            .set_faceline_wrinkle(unsafe { core::mem::transmute(default_mii.face_wrinkle as u8) });
        self.core_data
            .set_faceline_make(unsafe { core::mem::transmute(default_mii.face_makeup as u8) });

        self.core_data
            .set_hair_type(unsafe { core::mem::transmute(default_mii.hair_type as u8) });
        self.core_data
            .set_hair_color(raw_data::get_hair_color_from_ver3(default_mii.hair_color) as u32);
        self.core_data
            .set_hair_flip(unsafe { core::mem::transmute(default_mii.hair_flip as u8) });
        self.core_data
            .set_eye_type(unsafe { core::mem::transmute(default_mii.eye_type as u8) });
        self.core_data
            .set_eye_color(raw_data::get_eye_color_from_ver3(default_mii.eye_color) as u32);
        self.core_data.set_eye_scale(default_mii.eye_scale as u8);
        self.core_data.set_eye_aspect(default_mii.eye_aspect as u8);
        self.core_data.set_eye_rotate(default_mii.eye_rotate as u8);
        self.core_data.set_eye_x(default_mii.eye_x as u8);
        self.core_data.set_eye_y(default_mii.eye_y as u8);

        self.core_data
            .set_eyebrow_type(unsafe { core::mem::transmute(default_mii.eyebrow_type as u8) });
        self.core_data
            .set_eyebrow_color(
                raw_data::get_hair_color_from_ver3(default_mii.eyebrow_color) as u32,
            );
        self.core_data
            .set_eyebrow_scale(default_mii.eyebrow_scale as u8);
        self.core_data
            .set_eyebrow_aspect(default_mii.eyebrow_aspect as u8);
        self.core_data
            .set_eyebrow_rotate(default_mii.eyebrow_rotate as u8);
        self.core_data.set_eyebrow_x(default_mii.eyebrow_x as u8);
        self.core_data
            .set_eyebrow_y((default_mii.eyebrow_y as i32 - 3) as u8);

        self.core_data
            .set_nose_type(unsafe { core::mem::transmute(default_mii.nose_type as u8) });
        self.core_data.set_nose_scale(default_mii.nose_scale as u8);
        self.core_data.set_nose_y(default_mii.nose_y as u8);

        self.core_data
            .set_mouth_type(unsafe { core::mem::transmute(default_mii.mouth_type as u8) });
        self.core_data
            .set_mouth_color(raw_data::get_mouth_color_from_ver3(default_mii.mouth_color) as u32);
        self.core_data
            .set_mouth_scale(default_mii.mouth_scale as u8);
        self.core_data
            .set_mouth_aspect(default_mii.mouth_aspect as u8);
        self.core_data.set_mouth_y(default_mii.mouth_y as u8);

        self.core_data.set_mustache_type_enum(unsafe {
            core::mem::transmute(default_mii.mustache_type as u8)
        });
        self.core_data
            .set_beard_type_enum(unsafe { core::mem::transmute(default_mii.beard_type as u8) });
        self.core_data
            .set_beard_color(raw_data::get_hair_color_from_ver3(default_mii.beard_color) as u32);
        self.core_data
            .set_mustache_scale(default_mii.mustache_scale as u8);
        self.core_data.set_mustache_y(default_mii.mustache_y as u8);

        self.core_data
            .set_glass_type(unsafe { core::mem::transmute(default_mii.glasses_type as u8) });
        self.core_data
            .set_glass_color(raw_data::get_glass_color_from_ver3(default_mii.glasses_color) as u32);
        self.core_data
            .set_glass_scale(default_mii.glasses_scale as u8);
        self.core_data.set_glass_y(default_mii.glasses_y as u8);

        self.core_data
            .set_mole_type_enum(unsafe { core::mem::transmute(default_mii.mole_type as u8) });
        self.core_data.set_mole_scale(default_mii.mole_scale as u8);
        self.core_data.set_mole_x(default_mii.mole_x as u8);
        self.core_data.set_mole_y(default_mii.mole_y as u8);

        self.core_data.set_height(default_mii.height as u8);
        self.core_data.set_build(default_mii.weight as u8);
        self.core_data
            .set_gender(unsafe { core::mem::transmute(default_mii.gender as u8) });
        self.core_data
            .set_favorite_color(unsafe { core::mem::transmute(default_mii.favorite_color as u8) });
        self.core_data
            .set_region_move(default_mii.region_move as u8);
        self.core_data
            .set_font_region(unsafe { core::mem::transmute(default_mii.font_region as u8) });
        self.core_data.set_type(default_mii.r#type as u8);
        self.core_data.set_nickname(default_mii.nickname);

        self.set_create_id(mii_util::make_create_id());
        self.set_checksum();
    }

    /// Matches upstream StoreData::BuildBase.
    pub fn build_base(&mut self, gender: Gender) {
        let default_mii = &raw_data::BASE_MII[if gender == Gender::Female { 1 } else { 0 }];
        self.core_data.set_default();

        self.core_data
            .set_faceline_type(unsafe { core::mem::transmute(default_mii.face_type as u8) });
        self.core_data
            .set_faceline_color(raw_data::get_faceline_color_from_ver3(
                default_mii.face_color,
            ));
        self.core_data
            .set_faceline_wrinkle(unsafe { core::mem::transmute(default_mii.face_wrinkle as u8) });
        self.core_data
            .set_faceline_make(unsafe { core::mem::transmute(default_mii.face_makeup as u8) });

        self.core_data
            .set_hair_type(unsafe { core::mem::transmute(default_mii.hair_type as u8) });
        self.core_data
            .set_hair_color(raw_data::get_hair_color_from_ver3(default_mii.hair_color) as u32);
        self.core_data
            .set_hair_flip(unsafe { core::mem::transmute(default_mii.hair_flip as u8) });
        self.core_data
            .set_eye_type(unsafe { core::mem::transmute(default_mii.eye_type as u8) });
        self.core_data
            .set_eye_color(raw_data::get_eye_color_from_ver3(default_mii.eye_color) as u32);
        self.core_data.set_eye_scale(default_mii.eye_scale as u8);
        self.core_data.set_eye_aspect(default_mii.eye_aspect as u8);
        self.core_data.set_eye_rotate(default_mii.eye_rotate as u8);
        self.core_data.set_eye_x(default_mii.eye_x as u8);
        self.core_data.set_eye_y(default_mii.eye_y as u8);

        self.core_data
            .set_eyebrow_type(unsafe { core::mem::transmute(default_mii.eyebrow_type as u8) });
        self.core_data
            .set_eyebrow_color(
                raw_data::get_hair_color_from_ver3(default_mii.eyebrow_color) as u32,
            );
        self.core_data
            .set_eyebrow_scale(default_mii.eyebrow_scale as u8);
        self.core_data
            .set_eyebrow_aspect(default_mii.eyebrow_aspect as u8);
        self.core_data
            .set_eyebrow_rotate(default_mii.eyebrow_rotate as u8);
        self.core_data.set_eyebrow_x(default_mii.eyebrow_x as u8);
        self.core_data
            .set_eyebrow_y((default_mii.eyebrow_y as i32 - 3) as u8);

        self.core_data
            .set_nose_type(unsafe { core::mem::transmute(default_mii.nose_type as u8) });
        self.core_data.set_nose_scale(default_mii.nose_scale as u8);
        self.core_data.set_nose_y(default_mii.nose_y as u8);

        self.core_data
            .set_mouth_type(unsafe { core::mem::transmute(default_mii.mouth_type as u8) });
        self.core_data
            .set_mouth_color(raw_data::get_mouth_color_from_ver3(default_mii.mouth_color) as u32);
        self.core_data
            .set_mouth_scale(default_mii.mouth_scale as u8);
        self.core_data
            .set_mouth_aspect(default_mii.mouth_aspect as u8);
        self.core_data.set_mouth_y(default_mii.mouth_y as u8);

        self.core_data.set_mustache_type_enum(unsafe {
            core::mem::transmute(default_mii.mustache_type as u8)
        });
        self.core_data
            .set_beard_type_enum(unsafe { core::mem::transmute(default_mii.beard_type as u8) });
        self.core_data
            .set_beard_color(raw_data::get_hair_color_from_ver3(default_mii.beard_color) as u32);
        self.core_data
            .set_mustache_scale(default_mii.mustache_scale as u8);
        self.core_data.set_mustache_y(default_mii.mustache_y as u8);

        self.core_data
            .set_glass_type(unsafe { core::mem::transmute(default_mii.glasses_type as u8) });
        self.core_data
            .set_glass_color(raw_data::get_glass_color_from_ver3(default_mii.glasses_color) as u32);
        self.core_data
            .set_glass_scale(default_mii.glasses_scale as u8);
        self.core_data.set_glass_y(default_mii.glasses_y as u8);

        self.core_data
            .set_mole_type_enum(unsafe { core::mem::transmute(default_mii.mole_type as u8) });
        self.core_data.set_mole_scale(default_mii.mole_scale as u8);
        self.core_data.set_mole_x(default_mii.mole_x as u8);
        self.core_data.set_mole_y(default_mii.mole_y as u8);

        self.core_data.set_height(default_mii.height as u8);
        self.core_data.set_build(default_mii.weight as u8);
        self.core_data
            .set_gender(unsafe { core::mem::transmute(default_mii.gender as u8) });
        self.core_data
            .set_favorite_color(unsafe { core::mem::transmute(default_mii.favorite_color as u8) });
        self.core_data
            .set_region_move(default_mii.region_move as u8);
        self.core_data
            .set_font_region(unsafe { core::mem::transmute(default_mii.font_region as u8) });
        self.core_data.set_type(default_mii.r#type as u8);
        self.core_data.set_nickname(default_mii.nickname);

        self.set_create_id(mii_util::make_create_id());
        self.set_checksum();
    }

    /// Matches upstream StoreData::BuildRandom.
    pub fn build_random(&mut self, age: Age, gender: Gender, race: Race) {
        self.core_data.build_random(age, gender, race);
        self.set_create_id(mii_util::make_create_id());
        self.set_checksum();
    }

    /// Matches upstream `StoreData::BuildWithCharInfo`.
    pub fn build_with_char_info(&mut self, char_info: &CharInfo) {
        self.core_data.build_from_char_info(char_info);
        self.set_create_id(mii_util::make_create_id());
        self.set_checksum();
    }

    /// Matches upstream `StoreData::BuildWithCoreData`.
    pub fn build_with_core_data(&mut self, in_core_data: CoreData) {
        self.core_data = in_core_data;
        self.set_create_id(mii_util::make_create_id());
        self.set_checksum();
    }

    /// Matches upstream `StoreData::Restore`.
    pub fn restore(&self) -> ResultCode {
        RESULT_NOT_UPDATED
    }

    /// Matches upstream StoreData::IsSpecial.
    pub fn is_special(&self) -> bool {
        self.core_data.get_type() == 1
    }

    pub fn set_font_region(&mut self, value: FontRegion) {
        self.core_data.set_font_region(value);
    }

    pub fn set_favorite_color(&mut self, value: FavoriteColor) {
        self.core_data.set_favorite_color(value);
    }

    pub fn set_gender(&mut self, value: Gender) {
        self.core_data.set_gender(value);
    }

    pub fn set_height(&mut self, value: u8) {
        self.core_data.set_height(value);
    }

    pub fn set_build(&mut self, value: u8) {
        self.core_data.set_build(value);
    }

    pub fn set_type(&mut self, value: u8) {
        self.core_data.set_type(value);
    }

    pub fn set_region_move(&mut self, value: u8) {
        self.core_data.set_region_move(value);
    }

    pub fn set_faceline_type(&mut self, value: FacelineType) {
        self.core_data.set_faceline_type(value);
    }

    pub fn set_faceline_color(&mut self, value: FacelineColor) {
        self.core_data.set_faceline_color(value);
    }

    pub fn set_faceline_wrinkle(&mut self, value: FacelineWrinkle) {
        self.core_data.set_faceline_wrinkle(value);
    }

    pub fn set_faceline_make(&mut self, value: FacelineMake) {
        self.core_data.set_faceline_make(value);
    }

    pub fn set_hair_type(&mut self, value: HairType) {
        self.core_data.set_hair_type(value);
    }

    pub fn set_hair_color(&mut self, value: CommonColor) {
        self.core_data.set_hair_color(value.0 as u32);
    }

    pub fn set_hair_flip(&mut self, value: HairFlip) {
        self.core_data.set_hair_flip(value);
    }

    pub fn set_eye_type(&mut self, value: EyeType) {
        self.core_data.set_eye_type(value);
    }

    pub fn set_eye_color(&mut self, value: CommonColor) {
        self.core_data.set_eye_color(value.0 as u32);
    }

    pub fn set_eye_scale(&mut self, value: u8) {
        self.core_data.set_eye_scale(value);
    }

    pub fn set_eye_aspect(&mut self, value: u8) {
        self.core_data.set_eye_aspect(value);
    }

    pub fn set_eye_rotate(&mut self, value: u8) {
        self.core_data.set_eye_rotate(value);
    }

    pub fn set_eye_x(&mut self, value: u8) {
        self.core_data.set_eye_x(value);
    }

    pub fn set_eye_y(&mut self, value: u8) {
        self.core_data.set_eye_y(value);
    }

    pub fn set_eyebrow_type(&mut self, value: EyebrowType) {
        self.core_data.set_eyebrow_type(value);
    }

    pub fn set_eyebrow_color(&mut self, value: CommonColor) {
        self.core_data.set_eyebrow_color(value.0 as u32);
    }

    pub fn set_eyebrow_scale(&mut self, value: u8) {
        self.core_data.set_eyebrow_scale(value);
    }

    pub fn set_eyebrow_aspect(&mut self, value: u8) {
        self.core_data.set_eyebrow_aspect(value);
    }

    pub fn set_eyebrow_rotate(&mut self, value: u8) {
        self.core_data.set_eyebrow_rotate(value);
    }

    pub fn set_eyebrow_x(&mut self, value: u8) {
        self.core_data.set_eyebrow_x(value);
    }

    pub fn set_eyebrow_y(&mut self, value: u8) {
        self.core_data.set_eyebrow_y(value);
    }

    pub fn set_nose_type(&mut self, value: NoseType) {
        self.core_data.set_nose_type(value);
    }

    pub fn set_nose_scale(&mut self, value: u8) {
        self.core_data.set_nose_scale(value);
    }

    pub fn set_nose_y(&mut self, value: u8) {
        self.core_data.set_nose_y(value);
    }

    pub fn set_mouth_type(&mut self, value: MouthType) {
        self.core_data.set_mouth_type(value);
    }

    pub fn set_mouth_color(&mut self, value: CommonColor) {
        self.core_data.set_mouth_color(value.0 as u32);
    }

    pub fn set_mouth_scale(&mut self, value: u8) {
        self.core_data.set_mouth_scale(value);
    }

    pub fn set_mouth_aspect(&mut self, value: u8) {
        self.core_data.set_mouth_aspect(value);
    }

    pub fn set_mouth_y(&mut self, value: u8) {
        self.core_data.set_mouth_y(value);
    }

    pub fn set_beard_color(&mut self, value: CommonColor) {
        self.core_data.set_beard_color(value.0 as u32);
    }

    pub fn set_beard_type(&mut self, value: BeardType) {
        self.core_data.set_beard_type_enum(value);
    }

    pub fn set_mustache_type(&mut self, value: MustacheType) {
        self.core_data.set_mustache_type_enum(value);
    }

    pub fn set_mustache_scale(&mut self, value: u8) {
        self.core_data.set_mustache_scale(value);
    }

    pub fn set_mustache_y(&mut self, value: u8) {
        self.core_data.set_mustache_y(value);
    }

    pub fn set_glass_type(&mut self, value: GlassType) {
        self.core_data.set_glass_type(value);
    }

    pub fn set_glass_color(&mut self, value: CommonColor) {
        self.core_data.set_glass_color(value.0 as u32);
    }

    pub fn set_glass_scale(&mut self, value: u8) {
        self.core_data.set_glass_scale(value);
    }

    pub fn set_glass_y(&mut self, value: u8) {
        self.core_data.set_glass_y(value);
    }

    pub fn set_mole_type(&mut self, value: MoleType) {
        self.core_data.set_mole_type_enum(value);
    }

    pub fn set_mole_scale(&mut self, value: u8) {
        self.core_data.set_mole_scale(value);
    }

    pub fn set_mole_x(&mut self, value: u8) {
        self.core_data.set_mole_x(value);
    }

    pub fn set_mole_y(&mut self, value: u8) {
        self.core_data.set_mole_y(value);
    }

    pub fn set_nickname(&mut self, nickname: Nickname) {
        self.core_data.set_nickname(nickname);
    }

    pub fn get_create_id(&self) -> [u8; 16] {
        self.create_id
    }

    /// Matches upstream StoreData::IsValid.
    pub fn is_valid(&self) -> ValidationResult {
        let result = self.core_data.is_valid();
        if result != ValidationResult::NoErrors {
            return result;
        }

        let data_slice = unsafe {
            core::slice::from_raw_parts(
                (&self.core_data as *const CoreData).cast::<u8>(),
                core::mem::size_of::<CoreData>() + core::mem::size_of::<[u8; 16]>(),
            )
        };
        if self.data_crc != mii_util::calculate_crc16(data_slice) {
            return ValidationResult::InvalidChecksum;
        }

        let device_id = mii_util::get_device_id().to_le_bytes();
        if self.device_crc
            != mii_util::calculate_device_crc16(&device_id, core::mem::size_of::<StoreData>())
        {
            return ValidationResult::InvalidChecksum;
        }

        ValidationResult::NoErrors
    }

    /// Matches upstream StoreData::SetChecksum.
    pub fn set_checksum(&mut self) {
        self.set_data_checksum();
        self.set_device_checksum();
    }

    /// Matches upstream StoreData::SetDataChecksum.
    fn set_data_checksum(&mut self) {
        // CRC over core_data + create_id (0x30 + 0x10 = 0x40 bytes)
        let data_ptr = &self.core_data as *const CoreData as *const u8;
        let data_len = core::mem::size_of::<CoreData>() + core::mem::size_of::<[u8; 16]>();
        let data_slice = unsafe { core::slice::from_raw_parts(data_ptr, data_len) };
        self.data_crc = mii_util::calculate_crc16(data_slice);
    }

    /// Matches upstream StoreData::SetDeviceChecksum.
    pub(crate) fn set_device_checksum(&mut self) {
        let device_id = mii_util::get_device_id();
        let device_id_bytes = device_id.to_le_bytes();
        self.device_crc =
            mii_util::calculate_device_crc16(&device_id_bytes, core::mem::size_of::<StoreData>());
    }

    /// Matches upstream StoreData::SetInvalidName.
    pub fn set_invalid_name(&mut self) {
        let invalid_name = self.core_data.get_invalid_nickname();
        self.core_data.set_nickname(invalid_name);
        self.set_checksum();
    }

    // --- Delegating getters to core_data ---

    pub fn get_font_region(&self) -> FontRegion {
        self.core_data.get_font_region()
    }
    pub fn get_favorite_color(&self) -> FavoriteColor {
        self.core_data.get_favorite_color()
    }
    pub fn get_gender(&self) -> Gender {
        self.core_data.get_gender()
    }
    pub fn get_height(&self) -> u8 {
        self.core_data.get_height()
    }
    pub fn get_build(&self) -> u8 {
        self.core_data.get_build()
    }
    pub fn get_type(&self) -> u8 {
        self.core_data.get_type()
    }
    pub fn get_region_move(&self) -> u8 {
        self.core_data.get_region_move()
    }
    pub fn get_faceline_type(&self) -> FacelineType {
        self.core_data.get_faceline_type()
    }
    pub fn get_faceline_color(&self) -> FacelineColor {
        self.core_data.get_faceline_color()
    }
    pub fn get_faceline_wrinkle(&self) -> FacelineWrinkle {
        self.core_data.get_faceline_wrinkle()
    }
    pub fn get_faceline_make(&self) -> FacelineMake {
        self.core_data.get_faceline_make()
    }
    pub fn get_hair_type(&self) -> HairType {
        self.core_data.get_hair_type()
    }
    pub fn get_hair_color_raw(&self) -> u32 {
        self.core_data.get_hair_color_raw()
    }
    pub fn get_hair_color(&self) -> CommonColor {
        CommonColor(self.core_data.get_hair_color_raw() as u8)
    }
    pub fn get_hair_flip(&self) -> HairFlip {
        self.core_data.get_hair_flip()
    }
    pub fn get_eye_type(&self) -> EyeType {
        self.core_data.get_eye_type()
    }
    pub fn get_eye_color_raw(&self) -> u32 {
        self.core_data.get_eye_color_raw()
    }
    pub fn get_eye_color(&self) -> CommonColor {
        CommonColor(self.core_data.get_eye_color_raw() as u8)
    }
    pub fn get_eye_scale(&self) -> u8 {
        self.core_data.get_eye_scale()
    }
    pub fn get_eye_aspect(&self) -> u8 {
        self.core_data.get_eye_aspect()
    }
    pub fn get_eye_rotate(&self) -> u8 {
        self.core_data.get_eye_rotate()
    }
    pub fn get_eye_x(&self) -> u8 {
        self.core_data.get_eye_x()
    }
    pub fn get_eye_y(&self) -> u8 {
        self.core_data.get_eye_y()
    }
    pub fn get_eyebrow_type(&self) -> EyebrowType {
        self.core_data.get_eyebrow_type()
    }
    pub fn get_eyebrow_color_raw(&self) -> u32 {
        self.core_data.get_eyebrow_color_raw()
    }
    pub fn get_eyebrow_color(&self) -> CommonColor {
        CommonColor(self.core_data.get_eyebrow_color_raw() as u8)
    }
    pub fn get_eyebrow_scale(&self) -> u8 {
        self.core_data.get_eyebrow_scale()
    }
    pub fn get_eyebrow_aspect(&self) -> u8 {
        self.core_data.get_eyebrow_aspect()
    }
    pub fn get_eyebrow_rotate(&self) -> u8 {
        self.core_data.get_eyebrow_rotate()
    }
    pub fn get_eyebrow_x(&self) -> u8 {
        self.core_data.get_eyebrow_x()
    }
    pub fn get_eyebrow_y(&self) -> u8 {
        self.core_data.get_eyebrow_y()
    }
    pub fn get_nose_type(&self) -> NoseType {
        self.core_data.get_nose_type()
    }
    pub fn get_nose_scale(&self) -> u8 {
        self.core_data.get_nose_scale()
    }
    pub fn get_nose_y(&self) -> u8 {
        self.core_data.get_nose_y()
    }
    pub fn get_mouth_type(&self) -> MouthType {
        self.core_data.get_mouth_type()
    }
    pub fn get_mouth_color_raw(&self) -> u32 {
        self.core_data.get_mouth_color_raw()
    }
    pub fn get_mouth_color(&self) -> CommonColor {
        CommonColor(self.core_data.get_mouth_color_raw() as u8)
    }
    pub fn get_mouth_scale(&self) -> u8 {
        self.core_data.get_mouth_scale()
    }
    pub fn get_mouth_aspect(&self) -> u8 {
        self.core_data.get_mouth_aspect()
    }
    pub fn get_mouth_y(&self) -> u8 {
        self.core_data.get_mouth_y()
    }
    pub fn get_beard_color_raw(&self) -> u32 {
        self.core_data.get_beard_color_raw()
    }
    pub fn get_beard_color(&self) -> CommonColor {
        CommonColor(self.core_data.get_beard_color_raw() as u8)
    }
    pub fn get_beard_type(&self) -> BeardType {
        self.core_data.get_beard_type()
    }
    pub fn get_mustache_type(&self) -> MustacheType {
        self.core_data.get_mustache_type()
    }
    pub fn get_mustache_scale(&self) -> u8 {
        self.core_data.get_mustache_scale()
    }
    pub fn get_mustache_y(&self) -> u8 {
        self.core_data.get_mustache_y()
    }
    pub fn get_glass_type(&self) -> GlassType {
        self.core_data.get_glass_type()
    }
    pub fn get_glass_color_raw(&self) -> u32 {
        self.core_data.get_glass_color_raw()
    }
    pub fn get_glass_color(&self) -> CommonColor {
        CommonColor(self.core_data.get_glass_color_raw() as u8)
    }
    pub fn get_glass_scale(&self) -> u8 {
        self.core_data.get_glass_scale()
    }
    pub fn get_glass_y(&self) -> u8 {
        self.core_data.get_glass_y()
    }
    pub fn get_mole_type(&self) -> MoleType {
        self.core_data.get_mole_type()
    }
    pub fn get_mole_scale(&self) -> u8 {
        self.core_data.get_mole_scale()
    }
    pub fn get_mole_x(&self) -> u8 {
        self.core_data.get_mole_x()
    }
    pub fn get_mole_y(&self) -> u8 {
        self.core_data.get_mole_y()
    }
    pub fn get_nickname(&self) -> Nickname {
        self.core_data.get_nickname()
    }

    pub fn equals(&self, data: &StoreData) -> bool {
        let mut is_identical = data.core_data.is_valid() == ValidationResult::NoErrors;
        is_identical &= self.get_nickname().data == data.get_nickname().data;
        is_identical &= self.get_create_id() == data.get_create_id();
        is_identical &= self.get_font_region() == data.get_font_region();
        is_identical &= self.get_favorite_color() == data.get_favorite_color();
        is_identical &= self.get_gender() == data.get_gender();
        is_identical &= self.get_height() == data.get_height();
        is_identical &= self.get_build() == data.get_build();
        is_identical &= self.get_type() == data.get_type();
        is_identical &= self.get_region_move() == data.get_region_move();
        is_identical &= self.get_faceline_type() == data.get_faceline_type();
        is_identical &= self.get_faceline_color() == data.get_faceline_color();
        is_identical &= self.get_faceline_wrinkle() == data.get_faceline_wrinkle();
        is_identical &= self.get_faceline_make() == data.get_faceline_make();
        is_identical &= self.get_hair_type() == data.get_hair_type();
        is_identical &= self.get_hair_color() == data.get_hair_color();
        is_identical &= self.get_hair_flip() == data.get_hair_flip();
        is_identical &= self.get_eye_type() == data.get_eye_type();
        is_identical &= self.get_eye_color() == data.get_eye_color();
        is_identical &= self.get_eye_scale() == data.get_eye_scale();
        is_identical &= self.get_eye_aspect() == data.get_eye_aspect();
        is_identical &= self.get_eye_rotate() == data.get_eye_rotate();
        is_identical &= self.get_eye_x() == data.get_eye_x();
        is_identical &= self.get_eye_y() == data.get_eye_y();
        is_identical &= self.get_eyebrow_type() == data.get_eyebrow_type();
        is_identical &= self.get_eyebrow_color() == data.get_eyebrow_color();
        is_identical &= self.get_eyebrow_scale() == data.get_eyebrow_scale();
        is_identical &= self.get_eyebrow_aspect() == data.get_eyebrow_aspect();
        is_identical &= self.get_eyebrow_rotate() == data.get_eyebrow_rotate();
        is_identical &= self.get_eyebrow_x() == data.get_eyebrow_x();
        is_identical &= self.get_eyebrow_y() == data.get_eyebrow_y();
        is_identical &= self.get_nose_type() == data.get_nose_type();
        is_identical &= self.get_nose_scale() == data.get_nose_scale();
        is_identical &= self.get_nose_y() == data.get_nose_y();
        is_identical &= self.get_mouth_type() == data.get_mouth_type();
        is_identical &= self.get_mouth_color() == data.get_mouth_color();
        is_identical &= self.get_mouth_scale() == data.get_mouth_scale();
        is_identical &= self.get_mouth_aspect() == data.get_mouth_aspect();
        is_identical &= self.get_mouth_y() == data.get_mouth_y();
        is_identical &= self.get_beard_color() == data.get_beard_color();
        is_identical &= self.get_beard_type() == data.get_beard_type();
        is_identical &= self.get_mustache_type() == data.get_mustache_type();
        is_identical &= self.get_mustache_scale() == data.get_mustache_scale();
        is_identical &= self.get_mustache_y() == data.get_mustache_y();
        is_identical &= self.get_glass_type() == data.get_glass_type();
        is_identical &= self.get_glass_color() == data.get_glass_color();
        is_identical &= self.get_glass_scale() == data.get_glass_scale();
        is_identical &= self.get_glass_y() == data.get_glass_y();
        is_identical &= self.get_mole_type() == data.get_mole_type();
        is_identical &= self.get_mole_scale() == data.get_mole_scale();
        is_identical &= self.get_mole_x() == data.get_mole_x();
        is_identical &= data.get_mole_y() == data.get_mole_y();
        is_identical
    }
}

impl Default for StoreData {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for StoreData {
    fn eq(&self, other: &Self) -> bool {
        self.equals(other)
    }
}

impl Eq for StoreData {}

impl PartialEq for StoreDataElement {
    fn eq(&self, other: &Self) -> bool {
        self.store_data == other.store_data && (self.source as u32) == (other.source as u32)
    }
}

impl Eq for StoreDataElement {}

const _: () = assert!(core::mem::size_of::<StoreData>() == 0x44);
const _: () = assert!(core::mem::size_of::<StoreDataElement>() == 0x48);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_with_char_info_copies_core_data_and_generates_new_create_id() {
        let mut source = StoreData::new();
        source.build_default(0);

        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&source);

        let mut rebuilt = StoreData::new();
        rebuilt.build_with_char_info(&char_info);

        assert_eq!(rebuilt.get_nickname().data, char_info.name.data);
        assert_eq!(rebuilt.get_type(), char_info.type_val);
        assert_ne!(rebuilt.get_create_id(), [0; 16]);
    }

    #[test]
    fn restore_matches_upstream_stub() {
        let mut store = StoreData::new();
        store.build_default(0);
        assert_eq!(store.restore(), RESULT_NOT_UPDATED);
    }

    #[test]
    fn is_valid_rejects_corrupted_data_crc() {
        let mut store = StoreData::new();
        store.build_default(0);
        assert_eq!(store.is_valid(), ValidationResult::NoErrors);

        store.data_crc ^= 1;
        assert_eq!(store.is_valid(), ValidationResult::InvalidChecksum);
    }

    #[test]
    fn equals_detects_field_difference() {
        let mut a = StoreData::new();
        a.build_default(0);

        let mut b = a;
        b.set_height(a.get_height().wrapping_add(1));
        b.set_checksum();

        assert!(!a.equals(&b));
        assert_ne!(a, b);
    }
}
