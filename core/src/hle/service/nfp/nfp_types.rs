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
    None = 0,
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

// ---- Amiibo name length constant ----
pub const AMIIBO_NAME_LENGTH: usize = 0xA;

// ---- Type aliases matching upstream ----
pub type UuidPart = [u8; 3];
pub type HashData = [u8; 0x20];
pub type ApplicationArea = [u8; 0xD8];

/// Corresponds to `AmiiboType` in upstream nfp_types.h.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AmiiboType {
    Figure = 0,
    Card = 1,
    Yarn = 2,
}

/// Corresponds to `AmiiboSeries` in upstream nfp_types.h.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AmiiboSeries {
    SuperSmashBros = 0,
    SuperMario = 1,
    ChibiRobo = 2,
    YoshiWoollyWorld = 3,
    Splatoon = 4,
    AnimalCrossing = 5,
    EightBitMario = 6,
    Skylanders = 7,
    Unknown8 = 8,
    TheLegendOfZelda = 9,
    ShovelKnight = 10,
    Unknown11 = 11,
    Kiby = 12,
    Pokemon = 13,
    MarioSportsSuperstars = 14,
    MonsterHunter = 15,
    BoxBoy = 16,
    Pikmin = 17,
    FireEmblem = 18,
    Metroid = 19,
    Others = 20,
    MegaMan = 21,
    Diablo = 22,
}

/// Corresponds to `AppAreaVersion` in upstream nfp_types.h.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AppAreaVersion {
    Nintendo3DS = 0,
    NintendoWiiU = 1,
    Nintendo3DSv2 = 2,
    NintendoSwitch = 3,
    NotSet = 0xFF,
}

/// Corresponds to `NtagTagUuid` in upstream nfp_types.h.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NtagTagUuid {
    pub part1: UuidPart,
    pub part2: UuidPart,
    pub nintendo_id: u8,
}
const _: () = assert!(core::mem::size_of::<NtagTagUuid>() == 7);

/// Corresponds to `TagUuid` in upstream nfp_types.h.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct TagUuid {
    pub part1: UuidPart,
    pub crc_check1: u8,
    pub part2: UuidPart,
    pub nintendo_id: u8,
}
const _: () = assert!(core::mem::size_of::<TagUuid>() == 8);

impl Default for TagUuid {
    fn default() -> Self {
        // SAFETY: TagUuid is repr(C) and all-zeros is valid
        unsafe { core::mem::zeroed() }
    }
}

/// Corresponds to `AmiiboDate` in upstream nfp_types.h.
/// Stored as big-endian u16.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct AmiiboDate {
    pub raw_date: u16,
}
const _: () = assert!(core::mem::size_of::<AmiiboDate>() == 2);

impl Default for AmiiboDate {
    fn default() -> Self {
        Self { raw_date: 0 }
    }
}

/// Corresponds to `Settings` in upstream nfp_types.h.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Settings {
    pub raw: u8,
}
const _: () = assert!(core::mem::size_of::<Settings>() == 1);

impl Default for Settings {
    fn default() -> Self {
        Self { raw: 0 }
    }
}

/// Corresponds to `AmiiboSettings` in upstream nfp_types.h.
/// Size: 0x20 bytes.
#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct AmiiboSettings {
    pub settings: Settings,
    pub country_code_id: u8,
    pub crc_counter: u16, // big-endian
    pub init_date: AmiiboDate,
    pub write_date: AmiiboDate,
    pub crc: u32,                               // big-endian
    pub amiibo_name: [u16; AMIIBO_NAME_LENGTH], // big-endian UTF-16
}
const _: () = assert!(core::mem::size_of::<AmiiboSettings>() == 0x20);

impl Default for AmiiboSettings {
    fn default() -> Self {
        // SAFETY: AmiiboSettings is repr(C, packed) and all-zeros is valid
        unsafe { core::mem::zeroed() }
    }
}

/// Corresponds to `AmiiboModelInfo` in upstream nfp_types.h.
/// Size: 0xC bytes.
#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct AmiiboModelInfo {
    pub character_id: u16,
    pub character_variant: u8,
    pub amiibo_type: u8,   // AmiiboType
    pub model_number: u16, // big-endian
    pub series: u8,        // AmiiboSeries
    pub tag_type: u8,      // PackedTagType
    pub unknown: [u8; 0x4],
}
const _: () = assert!(core::mem::size_of::<AmiiboModelInfo>() == 0xC);

impl Default for AmiiboModelInfo {
    fn default() -> Self {
        // SAFETY: AmiiboModelInfo is repr(C, packed) and all-zeros is valid
        unsafe { core::mem::zeroed() }
    }
}

/// Corresponds to `NTAG215Password` in upstream nfp_types.h.
/// Size: 0x8 bytes.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Ntag215Password {
    pub pwd: u32,
    pub pack: u16,
    pub rfui: u16,
}
const _: () = assert!(core::mem::size_of::<Ntag215Password>() == 0x8);

impl Default for Ntag215Password {
    fn default() -> Self {
        // SAFETY: Ntag215Password is repr(C) and all-zeros is valid
        unsafe { core::mem::zeroed() }
    }
}

use crate::hle::service::mii::types::ver3_store_data::{NfpStoreDataExtension, Ver3StoreData};

/// Corresponds to `EncryptedAmiiboFile` in upstream nfp_types.h.
/// This is the user_memory portion of the encrypted NTAG215 dump.
/// Size: 0x1F8 bytes. Uses pack(1).
#[derive(Clone, Copy)]
#[repr(C, packed)]
pub struct EncryptedAmiiboFile {
    pub constant_value: u8, // Must be 0xA5
    pub write_counter: u16, // big-endian
    pub amiibo_version: u8,
    pub settings: AmiiboSettings,       // 0x20
    pub hmac_tag: HashData,             // 0x20
    pub model_info: AmiiboModelInfo,    // 0xC
    pub keygen_salt: HashData,          // 0x20
    pub hmac_data: HashData,            // 0x20
    pub owner_mii: Ver3StoreData,       // 0x60
    pub application_id: u64,            // big-endian
    pub application_write_counter: u16, // big-endian
    pub application_area_id: u32,       // big-endian
    pub application_id_byte: u8,
    pub unknown: u8,
    pub mii_extension: NfpStoreDataExtension, // 0x8
    pub unknown2: [u32; 0x5],                 // 0x14
    pub register_info_crc: u32,               // big-endian
    pub application_area: ApplicationArea,    // 0xD8
}
const _: () = assert!(core::mem::size_of::<EncryptedAmiiboFile>() == 0x1F8);

impl Default for EncryptedAmiiboFile {
    fn default() -> Self {
        // SAFETY: EncryptedAmiiboFile is repr(C, packed) and all-zeros is valid
        unsafe { core::mem::zeroed() }
    }
}

/// Corresponds to `NTAG215File` in upstream nfp_types.h.
/// Decoded/encoded amiibo data layout. Size: 0x21C bytes. Uses pack(1).
///
/// Field offsets (verified against upstream constants):
/// - hmac_data at 0x08 (HMAC_DATA_START)
/// - write_counter at 0x29 (WRITE_COUNTER_START)
/// - settings at 0x2C (SETTINGS_START)
/// - hmac_tag at 0x1B4 (HMAC_TAG_START)
/// - uid at 0x1D4 (UUID_START)
/// - dynamic_lock at 0x208 (DYNAMIC_LOCK_START)
#[derive(Clone, Copy)]
#[repr(C, packed)]
pub struct Ntag215File {
    pub uid_crc_check2: u8,
    pub internal_number: u8,
    pub static_lock: u16,
    pub compatibility_container: u32,
    pub hmac_data: HashData, // 0x20 bytes
    pub constant_value: u8,  // Must be 0xA5
    pub write_counter: u16,  // big-endian
    pub amiibo_version: u8,
    pub settings: AmiiboSettings,       // 0x20
    pub owner_mii: Ver3StoreData,       // 0x60
    pub application_id: u64,            // big-endian
    pub application_write_counter: u16, // big-endian
    pub application_area_id: u32,       // big-endian
    pub application_id_byte: u8,
    pub unknown: u8,
    pub mii_extension: NfpStoreDataExtension, // 0x8
    pub unknown2: [u32; 0x5],                 // 0x14
    pub register_info_crc: u32,               // big-endian
    pub application_area: ApplicationArea,    // 0xD8
    pub hmac_tag: HashData,                   // 0x20
    pub uid: TagUuid,                         // 0x8
    pub model_info: AmiiboModelInfo,          // 0xC
    pub keygen_salt: HashData,                // 0x20
    pub dynamic_lock: u32,
    pub cfg0: u32,
    pub cfg1: u32,
    pub password: Ntag215Password, // 0x8
}
const _: () = assert!(core::mem::size_of::<Ntag215File>() == 0x21C);

impl Default for Ntag215File {
    fn default() -> Self {
        // SAFETY: Ntag215File is repr(C, packed) and all-zeros is valid
        unsafe { core::mem::zeroed() }
    }
}

/// Corresponds to `EncryptedNTAG215File` in upstream nfp_types.h.
/// Raw encrypted amiibo dump layout. Size: 0x21C bytes.
#[derive(Clone, Copy)]
#[repr(C, packed)]
pub struct EncryptedNtag215File {
    pub uuid: TagUuid, // 0x8
    pub uuid_crc_check2: u8,
    pub internal_number: u8,
    pub static_lock: u16,
    pub compatibility_container: u32,
    pub user_memory: EncryptedAmiiboFile, // 0x1F8
    pub dynamic_lock: u32,
    pub cfg0: u32,
    pub cfg1: u32,
    pub password: Ntag215Password, // 0x8
}
const _: () = assert!(core::mem::size_of::<EncryptedNtag215File>() == 0x21C);

impl Default for EncryptedNtag215File {
    fn default() -> Self {
        // SAFETY: EncryptedNtag215File is repr(C, packed) and all-zeros is valid
        unsafe { core::mem::zeroed() }
    }
}
