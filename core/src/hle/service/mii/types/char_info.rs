// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/types/char_info.h
//! Port of zuyu/src/core/hle/service/mii/types/char_info.cpp
//!
//! CharInfo (nn::mii::detail::CharInfoRaw): the runtime Mii character info structure.
//! This is the 0x58-byte structure used in IPC.

use crate::hle::service::mii::mii_types::Nickname;

/// CharInfo is the runtime representation of a Mii character.
/// Size: 0x58 bytes in upstream.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CharInfo {
    pub create_id: u128,
    pub name: Nickname,
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

impl Default for CharInfo {
    fn default() -> Self {
        // SAFETY: All-zero is valid for CharInfo
        unsafe { core::mem::zeroed() }
    }
}
