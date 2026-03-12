// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfp/nfp_types.h
//!
//! Types for the NFP (NFC/amiibo) service.

/// NFC device state.
///
/// Corresponds to `DeviceState` in upstream nfp_types.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeviceState {
    Initialized = 0,
    SearchingForTag = 1,
    TagFound = 2,
    TagRemoved = 3,
    TagMounted = 4,
    Unavailable = 5,
    Finalized = 6,
}

/// NFC tag mount target.
///
/// Corresponds to `MountTarget` in upstream nfp_types.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MountTarget {
    Rom = 1,
    Ram = 2,
    All = 3,
}

/// Model type.
///
/// Corresponds to `ModelType` in upstream nfp_types.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModelType {
    Amiibo = 0,
}

/// Tag type.
///
/// Corresponds to `NfpType` in upstream nfp_types.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NfpType {
    None = 0,
    Type1 = 1,
    Type2 = 2,
}

/// Amiibo tag info.
///
/// Corresponds to `TagInfo` in upstream nfp_types.h.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct TagInfo {
    pub uuid: [u8; 10],
    pub uuid_length: u8,
    pub reserved1: [u8; 0x15],
    pub protocol: u32,
    pub tag_type: u32,
    pub reserved2: [u8; 0x30],
}

impl Default for TagInfo {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// Amiibo common info.
///
/// Corresponds to `CommonInfo` in upstream nfp_types.h.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct CommonInfo {
    pub last_write_year: u16,
    pub last_write_month: u8,
    pub last_write_day: u8,
    pub write_counter: u16,
    pub version: u16,
    pub application_area_size: u32,
    pub reserved: [u8; 0x34],
}

impl Default for CommonInfo {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// Break type for debug operations.
///
/// Corresponds to `BreakType` in upstream nfp_types.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakType {
    Type0 = 0,
    Type1 = 1,
    Type2 = 2,
}

/// Write type for NTF operations.
///
/// Corresponds to `WriteType` in upstream nfp_types.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WriteType {
    Type0 = 0,
    Type1 = 1,
}

/// Amiibo register info.
///
/// Corresponds to `RegisterInfo` in upstream nfp_types.h.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct RegisterInfo {
    pub mii_store_data: [u8; 0x44],
    pub creation_year: u16,
    pub creation_month: u8,
    pub creation_day: u8,
    pub amiibo_name: [u8; 40 + 1],
    pub font_region: u8,
    pub reserved: [u8; 0x7A],
}
