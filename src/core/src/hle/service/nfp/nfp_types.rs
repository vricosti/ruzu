// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nfp/nfp_types.h
//!
//! Types for the NFP (NFC/amiibo) service.

use crate::hle::service::mii::types::char_info::CharInfo;
use crate::hle::service::mii::types::store_data::StoreData;
use crate::hle::service::mii::types::ver3_store_data::{NfpStoreDataExtension, Ver3StoreData};
use crate::hle::service::nfc::nfc_types::PackedTagType;

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

/// Amiibo settings frontend mode.
///
/// Corresponds to upstream `CabinetMode`.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CabinetMode {
    #[default]
    StartNicknameAndOwnerSettings = 0,
    StartGameDataEraser = 1,
    StartRestorer = 2,
    StartFormatter = 3,
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

/// Upstream aliases NFP tag information directly to the NFC wire type.
pub type TagInfo = crate::hle::service::nfc::nfc_types::TagInfo;

/// Break type for debug operations.
///
/// Corresponds to `BreakType` in upstream nfp_types.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakType {
    Normal = 0,
    Unknown1 = 1,
    Unknown2 = 2,
}

/// Write type for NTF operations.
///
/// Corresponds to `WriteType` in upstream nfp_types.h.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WriteType {
    Unknown0 = 0,
    Unknown1 = 1,
}

// ---- Amiibo name length constant ----
pub const AMIIBO_NAME_LENGTH: usize = 0xA;
pub const APPLICATION_ID_VERSION_OFFSET: usize = 0x1C;
pub const COUNTER_LIMIT: u16 = 0xFFFF;

// ---- Type aliases matching upstream ----
pub type UuidPart = [u8; 3];
pub type HashData = [u8; 0x20];
pub type ApplicationArea = [u8; 0xD8];
pub type AmiiboName = [u8; (AMIIBO_NAME_LENGTH * 4) + 1];

/// Decoded date used by NFP IPC payloads.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[repr(C)]
pub struct WriteDate {
    pub year: u16,
    pub month: u8,
    pub day: u8,
}
const _: () = assert!(core::mem::size_of::<WriteDate>() == 0x4);

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

impl AmiiboDate {
    pub fn get_value(self) -> u16 {
        u16::from_be(self.raw_date)
    }

    pub fn get_year(self) -> u16 {
        ((self.get_value() & 0xFE00) >> 9) + 2000
    }

    pub fn get_month(self) -> u8 {
        ((self.get_value() & 0x01E0) >> 5) as u8
    }

    pub fn get_day(self) -> u8 {
        (self.get_value() & 0x001F) as u8
    }

    pub fn get_write_date(self) -> WriteDate {
        if !self.is_valid_date() {
            return WriteDate {
                year: 2000,
                month: 1,
                day: 1,
            };
        }
        WriteDate {
            year: self.get_year(),
            month: self.get_month(),
            day: self.get_day(),
        }
    }

    pub fn set_write_date(&mut self, write_date: WriteDate) {
        self.set_year(write_date.year);
        self.set_month(write_date.month);
        self.set_day(write_date.day);
    }

    pub fn set_year(&mut self, year: u16) {
        let year_converted = year.wrapping_sub(2000) << 9;
        self.raw_date = ((self.get_value() & !0xFE00) | year_converted).to_be();
    }

    pub fn set_month(&mut self, month: u8) {
        let month_converted = u16::from(month) << 5;
        self.raw_date = ((self.get_value() & !0x01E0) | month_converted).to_be();
    }

    pub fn set_day(&mut self, day: u8) {
        self.raw_date = ((self.get_value() & !0x001F) | u16::from(day)).to_be();
    }

    pub fn is_valid_date(self) -> bool {
        (1..32).contains(&self.get_day())
            && (1..13).contains(&self.get_month())
            && self.get_year() >= 2000
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

impl Settings {
    pub fn font_region(self) -> u8 {
        self.raw & 0x0F
    }

    pub fn set_font_region(&mut self, value: u8) {
        self.raw = (self.raw & !0x0F) | (value & 0x0F);
    }

    pub fn amiibo_initialized(self) -> bool {
        self.raw & (1 << 4) != 0
    }

    pub fn set_amiibo_initialized(&mut self, value: bool) {
        self.raw = (self.raw & !(1 << 4)) | (u8::from(value) << 4);
    }

    pub fn appdata_initialized(self) -> bool {
        self.raw & (1 << 5) != 0
    }

    pub fn set_appdata_initialized(&mut self, value: bool) {
        self.raw = (self.raw & !(1 << 5)) | (u8::from(value) << 5);
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
    pub amiibo_type: AmiiboType,
    pub model_number: u16, // big-endian
    pub series: AmiiboSeries,
    pub tag_type: PackedTagType,
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

/// Common amiibo information returned over NFP IPC.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CommonInfo {
    pub last_write_date: WriteDate,
    pub write_counter: u16,
    pub version: u8,
    pub _padding: u8,
    pub application_area_size: u32,
    pub _reserved: [u8; 0x34],
}
const _: () = assert!(core::mem::size_of::<CommonInfo>() == 0x40);

impl Default for CommonInfo {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// Amiibo model information returned over NFP IPC.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct ModelInfo {
    pub character_id: u16,
    pub character_variant: u8,
    pub amiibo_type: AmiiboType,
    pub model_number: u16,
    pub series: AmiiboSeries,
    pub _reserved: [u8; 0x39],
}
const _: () = assert!(core::mem::size_of::<ModelInfo>() == 0x40);

impl Default for ModelInfo {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// Public amiibo owner information returned over NFP IPC.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RegisterInfo {
    pub mii_char_info: CharInfo,
    pub creation_date: WriteDate,
    pub amiibo_name: AmiiboName,
    pub font_region: u8,
    pub _reserved: [u8; 0x7A],
}
const _: () = assert!(core::mem::size_of::<RegisterInfo>() == 0x100);

impl Default for RegisterInfo {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// Private amiibo owner information accepted by NFP and Cabinet.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RegisterInfoPrivate {
    pub mii_store_data: StoreData,
    pub creation_date: WriteDate,
    pub amiibo_name: AmiiboName,
    pub font_region: u8,
    pub _reserved: [u8; 0x8E],
}
const _: () = assert!(core::mem::size_of::<RegisterInfoPrivate>() == 0x100);

impl Default for RegisterInfoPrivate {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// Administrative amiibo information returned over NFP IPC.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct AdminInfo {
    pub application_id: u64,
    pub application_area_id: u32,
    pub crc_change_counter: u16,
    pub flags: u8,
    pub tag_type: PackedTagType,
    pub app_area_version: AppAreaVersion,
    pub _padding: [u8; 0x7],
    pub _reserved: [u8; 0x28],
}
const _: () = assert!(core::mem::size_of::<AdminInfo>() == 0x40);

impl Default for AdminInfo {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// Complete NFP transfer payload.
#[derive(Clone, Copy)]
#[repr(C, packed)]
pub struct NfpData {
    pub magic: u8,
    pub _padding_0: u8,
    pub write_counter: u8,
    pub _padding_1: u8,
    pub settings_crc: u32,
    pub _reserved_0: [u8; 0x38],
    pub common_info: CommonInfo,
    pub mii_char_info: Ver3StoreData,
    pub mii_store_data_extension: NfpStoreDataExtension,
    pub creation_date: WriteDate,
    pub amiibo_name: [u16; AMIIBO_NAME_LENGTH],
    pub amiibo_name_null_terminated: u16,
    pub settings: Settings,
    pub unknown1: u8,
    pub register_info_crc: u32,
    pub unknown2: [u32; 5],
    pub _reserved_1: [u8; 0x64],
    pub application_id: u64,
    pub access_id: u32,
    pub settings_crc_counter: u16,
    pub font_region: u8,
    pub tag_type: PackedTagType,
    pub console_type: AppAreaVersion,
    pub application_id_byte: u8,
    pub _reserved_2: [u8; 0x2E],
    pub application_area: ApplicationArea,
}
const _: () = assert!(core::mem::size_of::<NfpData>() == 0x298);

impl Default for NfpData {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

#[cfg(test)]
mod wire_layout_tests {
    use super::*;

    #[test]
    fn frontend_and_information_payloads_match_upstream_sizes() {
        assert_eq!(core::mem::size_of::<WriteDate>(), 0x4);
        assert_eq!(core::mem::size_of::<CommonInfo>(), 0x40);
        assert_eq!(core::mem::size_of::<ModelInfo>(), 0x40);
        assert_eq!(core::mem::size_of::<RegisterInfo>(), 0x100);
        assert_eq!(core::mem::size_of::<RegisterInfoPrivate>(), 0x100);
        assert_eq!(core::mem::size_of::<AdminInfo>(), 0x40);
        assert_eq!(core::mem::size_of::<NfpData>(), 0x298);
        assert_eq!(CabinetMode::StartFormatter as u8, 3);
    }

    #[test]
    fn amiibo_date_and_settings_match_upstream_bit_encoding() {
        let mut date = AmiiboDate::default();
        date.set_write_date(WriteDate {
            year: 2026,
            month: 8,
            day: 22,
        });
        assert_eq!(date.get_write_date().year, 2026);
        assert_eq!(date.get_write_date().month, 8);
        assert_eq!(date.get_write_date().day, 22);

        let mut settings = Settings::default();
        settings.set_font_region(7);
        settings.set_amiibo_initialized(true);
        settings.set_appdata_initialized(true);
        assert_eq!(settings.raw, 0x37);
        assert_eq!(settings.font_region(), 7);
        assert!(settings.amiibo_initialized());
        assert!(settings.appdata_initialized());
    }
}
