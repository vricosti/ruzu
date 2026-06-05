// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/types/char_info.h
//! Port of zuyu/src/core/hle/service/mii/types/char_info.cpp
//!
//! CharInfo (nn::mii::detail::CharInfoRaw): the runtime Mii character info structure.
//! This is the 0x58-byte structure used in IPC.

use super::store_data::StoreData;
use crate::hle::service::mii::mii_types::{
    BeardType, CommonColor, EyeType, EyebrowType, FacelineColor, FacelineMake, FacelineType,
    FacelineWrinkle, FavoriteColor, FontRegion, Gender, GlassType, HairFlip, HairType, MoleType,
    MouthType, MustacheType, Nickname, NoseType, Source, ValidationResult, MAX_BUILD,
    MAX_EYEBROW_ASPECT, MAX_EYEBROW_ROTATE, MAX_EYEBROW_SCALE, MAX_EYEBROW_X, MAX_EYEBROW_Y,
    MAX_EYE_ASPECT, MAX_EYE_SCALE, MAX_EYE_X, MAX_EYE_Y, MAX_GLASS_SCALE, MAX_GLASS_Y, MAX_HEIGHT,
    MAX_MOLE_SCALE, MAX_MOLE_X, MAX_MOLE_Y, MAX_MOUTH_ASPECT, MAX_MOUTH_SCALE, MAX_MOUTH_Y,
    MAX_MUSTACHE_SCALE, MAX_MUSTACHE_Y, MAX_NOSE_SCALE, MAX_NOSE_Y, MAX_REGION_MOVE, MAX_TYPE,
};
use common::uuid::UUID;

/// CharInfo is the runtime representation of a Mii character.
/// Size: 0x58 bytes in upstream.
///
/// Layout matches upstream:
///   Common::UUID create_id (16 bytes)
///   Nickname name (20 bytes)
///   u16 null_terminator (2 bytes)
///   then individual u8 fields (50 bytes)
///   Total: 16 + 20 + 2 + 50 = 88 = 0x58.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct CharInfo {
    pub create_id: [u8; 16],
    pub name: Nickname,
    pub null_terminator: u16,
    pub font_region: u8,
    pub favorite_color: u8,
    pub gender: u8,
    pub height: u8,
    pub build: u8,
    pub type_val: u8,
    pub region_move: u8,
    pub faceline_type: u8,
    pub faceline_color: u8,
    pub faceline_wrinkle: u8,
    pub faceline_make: u8,
    pub hair_type: u8,
    pub hair_color: u8,
    pub hair_flip: u8,
    pub eye_type: u8,
    pub eye_color: u8,
    pub eye_scale: u8,
    pub eye_aspect: u8,
    pub eye_rotate: u8,
    pub eye_x: u8,
    pub eye_y: u8,
    pub eyebrow_type: u8,
    pub eyebrow_color: u8,
    pub eyebrow_scale: u8,
    pub eyebrow_aspect: u8,
    pub eyebrow_rotate: u8,
    pub eyebrow_x: u8,
    pub eyebrow_y: u8,
    pub nose_type: u8,
    pub nose_scale: u8,
    pub nose_y: u8,
    pub mouth_type: u8,
    pub mouth_color: u8,
    pub mouth_scale: u8,
    pub mouth_aspect: u8,
    pub mouth_y: u8,
    pub beard_color: u8,
    pub beard_type: u8,
    pub mustache_type: u8,
    pub mustache_scale: u8,
    pub mustache_y: u8,
    pub glass_type: u8,
    pub glass_color: u8,
    pub glass_scale: u8,
    pub glass_y: u8,
    pub mole_type: u8,
    pub mole_scale: u8,
    pub mole_x: u8,
    pub mole_y: u8,
    pub padding: u8,
}

const _: () = assert!(core::mem::size_of::<CharInfo>() == 0x58);

/// Port of upstream `Service::Mii::CharInfoElement`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct CharInfoElement {
    pub char_info: CharInfo,
    pub source: Source,
}

const _: () = assert!(core::mem::size_of::<CharInfoElement>() == 0x5c);

impl Default for CharInfo {
    fn default() -> Self {
        // SAFETY: All-zero is valid for CharInfo
        unsafe { core::mem::zeroed() }
    }
}

impl CharInfo {
    /// Matches upstream `CharInfo::SetFromStoreData`.
    pub fn set_from_store_data(&mut self, store_data: &StoreData) {
        self.name = store_data.get_nickname();
        self.null_terminator = 0;
        self.create_id = store_data.get_create_id();
        self.font_region = store_data.get_font_region() as u8;
        self.favorite_color = store_data.get_favorite_color() as u8;
        self.gender = store_data.get_gender() as u8;
        self.height = store_data.get_height();
        self.build = store_data.get_build();
        self.type_val = store_data.get_type();
        self.region_move = store_data.get_region_move();
        self.faceline_type = store_data.get_faceline_type() as u8;
        self.faceline_color = store_data.get_faceline_color() as u8;
        self.faceline_wrinkle = store_data.get_faceline_wrinkle() as u8;
        self.faceline_make = store_data.get_faceline_make() as u8;
        self.hair_type = store_data.get_hair_type() as u8;
        self.hair_color = store_data.get_hair_color().0;
        self.hair_flip = store_data.get_hair_flip() as u8;
        self.eye_type = store_data.get_eye_type() as u8;
        self.eye_color = store_data.get_eye_color().0;
        self.eye_scale = store_data.get_eye_scale();
        self.eye_aspect = store_data.get_eye_aspect();
        self.eye_rotate = store_data.get_eye_rotate();
        self.eye_x = store_data.get_eye_x();
        self.eye_y = store_data.get_eye_y();
        self.eyebrow_type = store_data.get_eyebrow_type() as u8;
        self.eyebrow_color = store_data.get_eyebrow_color().0;
        self.eyebrow_scale = store_data.get_eyebrow_scale();
        self.eyebrow_aspect = store_data.get_eyebrow_aspect();
        self.eyebrow_rotate = store_data.get_eyebrow_rotate();
        self.eyebrow_x = store_data.get_eyebrow_x();
        self.eyebrow_y = store_data.get_eyebrow_y().wrapping_add(3);
        self.nose_type = store_data.get_nose_type() as u8;
        self.nose_scale = store_data.get_nose_scale();
        self.nose_y = store_data.get_nose_y();
        self.mouth_type = store_data.get_mouth_type() as u8;
        self.mouth_color = store_data.get_mouth_color().0;
        self.mouth_scale = store_data.get_mouth_scale();
        self.mouth_aspect = store_data.get_mouth_aspect();
        self.mouth_y = store_data.get_mouth_y();
        self.beard_color = store_data.get_beard_color().0;
        self.beard_type = store_data.get_beard_type() as u8;
        self.mustache_type = store_data.get_mustache_type() as u8;
        self.mustache_scale = store_data.get_mustache_scale();
        self.mustache_y = store_data.get_mustache_y();
        self.glass_type = store_data.get_glass_type() as u8;
        self.glass_color = store_data.get_glass_color().0;
        self.glass_scale = store_data.get_glass_scale();
        self.glass_y = store_data.get_glass_y();
        self.mole_type = store_data.get_mole_type() as u8;
        self.mole_scale = store_data.get_mole_scale();
        self.mole_x = store_data.get_mole_x();
        self.mole_y = store_data.get_mole_y();
        self.padding = 0;
    }

    /// Matches upstream `CharInfo::Verify`.
    pub fn verify(&self) -> ValidationResult {
        if !UUID::from_bytes(self.create_id).is_valid() {
            return ValidationResult::InvalidCreateId;
        }
        if !self.name.is_valid() {
            return ValidationResult::InvalidName;
        }
        if self.font_region > FontRegion::MAX as u8 {
            return ValidationResult::InvalidFont;
        }
        if self.favorite_color > FavoriteColor::MAX as u8 {
            return ValidationResult::InvalidColor;
        }
        if self.gender > Gender::MAX as u8 {
            return ValidationResult::InvalidGender;
        }
        if self.height > MAX_HEIGHT {
            return ValidationResult::InvalidHeight;
        }
        if self.build > MAX_BUILD {
            return ValidationResult::InvalidBuild;
        }
        if self.type_val > MAX_TYPE {
            return ValidationResult::InvalidType;
        }
        if self.region_move > MAX_REGION_MOVE {
            return ValidationResult::InvalidRegionMove;
        }
        if self.faceline_type > FacelineType::MAX as u8 {
            return ValidationResult::InvalidFacelineType;
        }
        if self.faceline_color > FacelineColor::MAX as u8 {
            return ValidationResult::InvalidFacelineColor;
        }
        if self.faceline_wrinkle > FacelineWrinkle::MAX as u8 {
            return ValidationResult::InvalidFacelineWrinkle;
        }
        if self.faceline_make > FacelineMake::MAX as u8 {
            return ValidationResult::InvalidFacelineMake;
        }
        if self.hair_type > HairType::MAX_U8 {
            return ValidationResult::InvalidHairType;
        }
        if self.hair_color > CommonColor::Max.0 {
            return ValidationResult::InvalidHairColor;
        }
        if self.hair_flip > HairFlip::MAX as u8 {
            return ValidationResult::InvalidHairFlip;
        }
        if self.eye_type > EyeType::MAX as u8 {
            return ValidationResult::InvalidEyeType;
        }
        if self.eye_color > CommonColor::Max.0 {
            return ValidationResult::InvalidEyeColor;
        }
        if self.eye_scale > MAX_EYE_SCALE {
            return ValidationResult::InvalidEyeScale;
        }
        if self.eye_aspect > MAX_EYE_ASPECT {
            return ValidationResult::InvalidEyeAspect;
        }
        if self.eye_rotate > MAX_EYE_X {
            return ValidationResult::InvalidEyeRotate;
        }
        if self.eye_x > MAX_EYE_X {
            return ValidationResult::InvalidEyeX;
        }
        if self.eye_y > MAX_EYE_Y {
            return ValidationResult::InvalidEyeY;
        }
        if self.eyebrow_type > EyebrowType::MAX as u8 {
            return ValidationResult::InvalidEyebrowType;
        }
        if self.eyebrow_color > CommonColor::Max.0 {
            return ValidationResult::InvalidEyebrowColor;
        }
        if self.eyebrow_scale > MAX_EYEBROW_SCALE {
            return ValidationResult::InvalidEyebrowScale;
        }
        if self.eyebrow_aspect > MAX_EYEBROW_ASPECT {
            return ValidationResult::InvalidEyebrowAspect;
        }
        if self.eyebrow_rotate > MAX_EYEBROW_ROTATE {
            return ValidationResult::InvalidEyebrowRotate;
        }
        if self.eyebrow_x > MAX_EYEBROW_X {
            return ValidationResult::InvalidEyebrowX;
        }
        if self.eyebrow_y.wrapping_sub(3) > MAX_EYEBROW_Y {
            return ValidationResult::InvalidEyebrowY;
        }
        if self.nose_type > NoseType::MAX as u8 {
            return ValidationResult::InvalidNoseType;
        }
        if self.nose_scale > MAX_NOSE_SCALE {
            return ValidationResult::InvalidNoseScale;
        }
        if self.nose_y > MAX_NOSE_Y {
            return ValidationResult::InvalidNoseY;
        }
        if self.mouth_type > MouthType::MAX as u8 {
            return ValidationResult::InvalidMouthType;
        }
        if self.mouth_color > CommonColor::Max.0 {
            return ValidationResult::InvalidMouthColor;
        }
        if self.mouth_scale > MAX_MOUTH_SCALE {
            return ValidationResult::InvalidMouthScale;
        }
        if self.mouth_aspect > MAX_MOUTH_ASPECT {
            return ValidationResult::InvalidMouthAspect;
        }
        if self.mouth_y > MAX_MOUTH_Y {
            return ValidationResult::InvalidMoleY;
        }
        if self.beard_color > CommonColor::Max.0 {
            return ValidationResult::InvalidBeardColor;
        }
        if self.beard_type > BeardType::MAX as u8 {
            return ValidationResult::InvalidBeardType;
        }
        if self.mustache_type > MustacheType::MAX as u8 {
            return ValidationResult::InvalidMustacheType;
        }
        if self.mustache_scale > MAX_MUSTACHE_SCALE {
            return ValidationResult::InvalidMustacheScale;
        }
        if self.mustache_y > MAX_MUSTACHE_Y {
            return ValidationResult::InvalidMustacheY;
        }
        if self.glass_type > GlassType::MAX as u8 {
            return ValidationResult::InvalidGlassType;
        }
        if self.glass_color > CommonColor::Max.0 {
            return ValidationResult::InvalidGlassColor;
        }
        if self.glass_scale > MAX_GLASS_SCALE {
            return ValidationResult::InvalidGlassScale;
        }
        if self.glass_y > MAX_GLASS_Y {
            return ValidationResult::InvalidGlassY;
        }
        if self.mole_type > MoleType::MAX_U8 {
            return ValidationResult::InvalidMoleType;
        }
        if self.mole_scale > MAX_MOLE_SCALE {
            return ValidationResult::InvalidMoleScale;
        }
        if self.mole_x > MAX_MOLE_X {
            return ValidationResult::InvalidMoleX;
        }
        if self.mole_y > MAX_MOLE_Y {
            return ValidationResult::InvalidMoleY;
        }
        ValidationResult::NoErrors
    }

    pub fn get_create_id(&self) -> [u8; 16] {
        self.create_id
    }

    pub fn get_nickname(&self) -> Nickname {
        self.name
    }

    pub fn get_font_region(&self) -> FontRegion {
        unsafe { core::mem::transmute(self.font_region) }
    }

    pub fn get_favorite_color(&self) -> FavoriteColor {
        unsafe { core::mem::transmute(self.favorite_color) }
    }

    pub fn get_gender(&self) -> Gender {
        unsafe { core::mem::transmute(self.gender) }
    }

    pub fn get_height(&self) -> u8 {
        self.height
    }

    pub fn get_build(&self) -> u8 {
        self.build
    }

    pub fn get_type(&self) -> u8 {
        self.type_val
    }

    pub fn get_region_move(&self) -> u8 {
        self.region_move
    }

    pub fn get_faceline_type(&self) -> FacelineType {
        unsafe { core::mem::transmute(self.faceline_type) }
    }

    pub fn get_faceline_color(&self) -> FacelineColor {
        unsafe { core::mem::transmute(self.faceline_color) }
    }

    pub fn get_faceline_wrinkle(&self) -> FacelineWrinkle {
        unsafe { core::mem::transmute(self.faceline_wrinkle) }
    }

    pub fn get_faceline_make(&self) -> FacelineMake {
        unsafe { core::mem::transmute(self.faceline_make) }
    }

    pub fn get_hair_type(&self) -> HairType {
        unsafe { core::mem::transmute(self.hair_type) }
    }

    pub fn get_hair_color(&self) -> CommonColor {
        CommonColor(self.hair_color)
    }

    pub fn get_hair_flip(&self) -> HairFlip {
        unsafe { core::mem::transmute(self.hair_flip) }
    }

    pub fn get_eye_type(&self) -> EyeType {
        unsafe { core::mem::transmute(self.eye_type) }
    }

    pub fn get_eye_color(&self) -> CommonColor {
        CommonColor(self.eye_color)
    }

    pub fn get_eye_scale(&self) -> u8 {
        self.eye_scale
    }

    pub fn get_eye_aspect(&self) -> u8 {
        self.eye_aspect
    }

    pub fn get_eye_rotate(&self) -> u8 {
        self.eye_rotate
    }

    pub fn get_eye_x(&self) -> u8 {
        self.eye_x
    }

    pub fn get_eye_y(&self) -> u8 {
        self.eye_y
    }

    pub fn get_eyebrow_type(&self) -> EyebrowType {
        unsafe { core::mem::transmute(self.eyebrow_type) }
    }

    pub fn get_eyebrow_color(&self) -> CommonColor {
        CommonColor(self.eyebrow_color)
    }

    pub fn get_eyebrow_scale(&self) -> u8 {
        self.eyebrow_scale
    }

    pub fn get_eyebrow_aspect(&self) -> u8 {
        self.eyebrow_aspect
    }

    pub fn get_eyebrow_rotate(&self) -> u8 {
        self.eyebrow_rotate
    }

    pub fn get_eyebrow_x(&self) -> u8 {
        self.eyebrow_x
    }

    pub fn get_eyebrow_y(&self) -> u8 {
        self.eyebrow_y
    }

    pub fn get_nose_type(&self) -> NoseType {
        unsafe { core::mem::transmute(self.nose_type) }
    }

    pub fn get_nose_scale(&self) -> u8 {
        self.nose_scale
    }

    pub fn get_nose_y(&self) -> u8 {
        self.nose_y
    }

    pub fn get_mouth_type(&self) -> MouthType {
        unsafe { core::mem::transmute(self.mouth_type) }
    }

    pub fn get_mouth_color(&self) -> CommonColor {
        CommonColor(self.mouth_color)
    }

    pub fn get_mouth_scale(&self) -> u8 {
        self.mouth_scale
    }

    pub fn get_mouth_aspect(&self) -> u8 {
        self.mouth_aspect
    }

    pub fn get_mouth_y(&self) -> u8 {
        self.mouth_y
    }

    pub fn get_beard_color(&self) -> CommonColor {
        CommonColor(self.beard_color)
    }

    pub fn get_beard_type(&self) -> BeardType {
        unsafe { core::mem::transmute(self.beard_type) }
    }

    pub fn get_mustache_type(&self) -> MustacheType {
        unsafe { core::mem::transmute(self.mustache_type) }
    }

    pub fn get_mustache_scale(&self) -> u8 {
        self.mustache_scale
    }

    pub fn get_mustache_y(&self) -> u8 {
        self.mustache_y
    }

    pub fn get_glass_type(&self) -> GlassType {
        unsafe { core::mem::transmute(self.glass_type) }
    }

    pub fn get_glass_color(&self) -> CommonColor {
        CommonColor(self.glass_color)
    }

    pub fn get_glass_scale(&self) -> u8 {
        self.glass_scale
    }

    pub fn get_glass_y(&self) -> u8 {
        self.glass_y
    }

    pub fn get_mole_type(&self) -> MoleType {
        unsafe { core::mem::transmute(self.mole_type) }
    }

    pub fn get_mole_scale(&self) -> u8 {
        self.mole_scale
    }

    pub fn get_mole_x(&self) -> u8 {
        self.mole_x
    }

    pub fn get_mole_y(&self) -> u8 {
        self.mole_y
    }

    pub fn equals(&self, info: &CharInfo) -> bool {
        self == info
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_from_store_data_copies_fields_and_adjusts_eyebrow_y() {
        let mut store_data = StoreData::new();
        store_data.build_default(0);

        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&store_data);

        assert_eq!(char_info.create_id, store_data.get_create_id());
        assert_eq!(char_info.name.data, store_data.get_nickname().data);
        assert_eq!(char_info.null_terminator, 0);
        assert_eq!(
            char_info.eyebrow_y,
            store_data.get_eyebrow_y().wrapping_add(3)
        );
        assert_eq!(char_info.padding, 0);
    }

    #[test]
    fn getters_round_trip_values_from_store_data() {
        let mut store_data = StoreData::new();
        store_data.build_default(0);

        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&store_data);

        assert_eq!(char_info.get_create_id(), store_data.get_create_id());
        assert_eq!(char_info.get_nickname(), store_data.get_nickname());
        assert_eq!(
            char_info.get_font_region() as u8,
            store_data.get_font_region() as u8
        );
        assert_eq!(
            char_info.get_favorite_color() as u8,
            store_data.get_favorite_color() as u8
        );
        assert_eq!(char_info.get_gender() as u8, store_data.get_gender() as u8);
        assert_eq!(char_info.get_height(), store_data.get_height());
        assert_eq!(char_info.get_build(), store_data.get_build());
        assert_eq!(char_info.get_type(), store_data.get_type());
        assert_eq!(char_info.get_region_move(), store_data.get_region_move());
        assert_eq!(
            char_info.get_eyebrow_y(),
            store_data.get_eyebrow_y().wrapping_add(3)
        );
        assert!(char_info.equals(&char_info));
    }

    #[test]
    fn verify_rejects_zero_create_id() {
        let char_info = CharInfo::default();
        assert_eq!(char_info.verify(), ValidationResult::InvalidCreateId);
    }

    #[test]
    fn verify_accepts_default_char_info() {
        let mut store_data = StoreData::new();
        store_data.build_default(0);

        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&store_data);

        assert_eq!(char_info.verify(), ValidationResult::NoErrors);
    }

    #[test]
    fn verify_matches_upstream_mouth_y_error_variant() {
        let mut store_data = StoreData::new();
        store_data.build_default(0);

        let mut char_info = CharInfo::default();
        char_info.set_from_store_data(&store_data);
        char_info.mouth_y = MAX_MOUTH_Y.saturating_add(1);

        assert_eq!(char_info.verify(), ValidationResult::InvalidMoleY);
    }
}
