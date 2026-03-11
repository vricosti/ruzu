// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/nfc_types.h

use bitflags::bitflags;

/// This is nn::nfc::BackendType
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum BackendType {
    None = 0,
    Nfc = 1,
    Nfp = 2,
    Mifare = 3,
}

/// This is nn::nfc::DeviceState
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u32)]
pub enum DeviceState {
    Initialized = 0,
    SearchingForTag = 1,
    TagFound = 2,
    TagRemoved = 3,
    TagMounted = 4,
    Unavailable = 5,
    Finalized = 6,
}

/// This is nn::nfc::State
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum State {
    NonInitialized = 0,
    Initialized = 1,
}

bitflags! {
    /// This is nn::nfc::TagType
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(transparent)]
    pub struct TagType: u32 {
        const NONE    = 0;
        const TYPE1   = 1 << 0;  // ISO14443A RW. Topaz
        const TYPE2   = 1 << 1;  // ISO14443A RW. Ultralight, NTAGX, ST25TN
        const TYPE3   = 1 << 2;  // ISO14443A RW/RO. Sony FeliCa
        const TYPE4A  = 1 << 3;  // ISO14443A RW/RO. DESFire
        const TYPE4B  = 1 << 4;  // ISO14443B RW/RO. DESFire
        const TYPE5   = 1 << 5;  // ISO15693 RW/RO. SLI, SLIX, ST25TV
        const MIFARE  = 1 << 6;  // Mifare classic. Skylanders
        const ALL     = 0xFFFFFFFF;
    }
}

bitflags! {
    /// PackedTagType (u8 version)
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(transparent)]
    pub struct PackedTagType: u8 {
        const NONE    = 0;
        const TYPE1   = 1 << 0;
        const TYPE2   = 1 << 1;
        const TYPE3   = 1 << 2;
        const TYPE4A  = 1 << 3;
        const TYPE4B  = 1 << 4;
        const TYPE5   = 1 << 5;
        const MIFARE  = 1 << 6;
        const ALL     = 0xFF;
    }
}

bitflags! {
    /// This is nn::nfc::NfcProtocol
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(transparent)]
    pub struct NfcProtocol: u32 {
        const NONE   = 0;
        const TYPE_A = 1 << 0; // ISO14443A
        const TYPE_B = 1 << 1; // ISO14443B
        const TYPE_F = 1 << 2; // Sony FeliCa
        const ALL    = 0xFFFFFFFF;
    }
}

/// This is nn::nfc::TestWaveType
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum TestWaveType {
    Unknown = 0,
}

pub type UniqueSerialNumber = [u8; 10];

/// This is nn::nfc::DeviceHandle
pub type DeviceHandle = u64;

/// This is nn::nfc::TagInfo
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct TagInfo {
    pub uuid: UniqueSerialNumber,
    pub uuid_length: u8,
    pub _padding1: [u8; 0x15],
    pub protocol: NfcProtocol,
    pub tag_type: TagType,
    pub _padding2: [u8; 0x30],
}

const _: () = assert!(core::mem::size_of::<TagInfo>() == 0x58);

impl Default for TagInfo {
    fn default() -> Self {
        // Zero-initialize the entire struct to match C++ zero-init behavior
        // SAFETY: TagInfo is repr(C) and all-zeros is valid
        unsafe { core::mem::zeroed() }
    }
}
