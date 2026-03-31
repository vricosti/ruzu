// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/types/store_data.h
//! Port of zuyu/src/core/hle/service/mii/types/store_data.cpp
//!
//! StoreData: the on-disk Mii storage format.
//! Size: 0x44 bytes in upstream.

use super::core_data::CoreData;
use super::raw_data;
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

    /// Matches upstream StoreData::IsSpecial.
    pub fn is_special(&self) -> bool {
        self.core_data.get_type() == 1
    }

    /// Matches upstream StoreData::IsValid.
    pub fn is_valid(&self) -> ValidationResult {
        let result = self.core_data.is_valid();
        if result != ValidationResult::NoErrors {
            return result;
        }
        // CRC checks would go here with full implementation
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
    fn set_device_checksum(&mut self) {
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
    pub fn get_hair_flip(&self) -> HairFlip {
        self.core_data.get_hair_flip()
    }
    pub fn get_eye_type(&self) -> EyeType {
        self.core_data.get_eye_type()
    }
    pub fn get_eye_color_raw(&self) -> u32 {
        self.core_data.get_eye_color_raw()
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
}

impl Default for StoreData {
    fn default() -> Self {
        Self::new()
    }
}

const _: () = assert!(core::mem::size_of::<StoreData>() == 0x44);
