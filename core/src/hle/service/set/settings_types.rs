//! Port of zuyu/src/core/hle/service/set/settings_types.h
//!
//! All enums, structs, and constants used by the settings service.

/// nn::settings::system::AudioOutputMode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AudioOutputMode {
    Ch1 = 0,
    Ch2 = 1,
    Ch5_1 = 2,
    Ch7_1 = 3,
}

/// nn::settings::system::AudioOutputModeTarget
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AudioOutputModeTarget {
    None = 0,
    Hdmi = 1,
    Speaker = 2,
    Headphone = 3,
    Type3 = 4,
    Type4 = 5,
}

/// nn::settings::system::AudioVolumeTarget
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AudioVolumeTarget {
    Speaker = 0,
    Headphone = 1,
}

/// nn::settings::system::ColorSet
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ColorSet {
    BasicWhite = 0,
    BasicBlack = 1,
}

/// nn::settings::system::ConsoleSleepPlan
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ConsoleSleepPlan {
    Sleep1Hour = 0,
    Sleep2Hour = 1,
    Sleep3Hour = 2,
    Sleep6Hour = 3,
    Sleep12Hour = 4,
    Never = 5,
}

/// nn::settings::system::ErrorReportSharePermission
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ErrorReportSharePermission {
    NotConfirmed = 0,
    Granted = 1,
    Denied = 2,
}

/// nn::settings::factory::RegionCode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum FactoryRegionCode {
    Japan = 0,
    Usa = 1,
    Europe = 2,
    Australia = 3,
    China = 4,
    Korea = 5,
    Taiwan = 6,
}

/// nn::settings::system::FriendPresenceOverlayPermission
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum FriendPresenceOverlayPermission {
    NotConfirmed = 0,
    NoDisplay = 1,
    FavoriteFriends = 2,
    Friends = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GetFirmwareVersionType {
    Version1,
    Version2,
}

/// nn::settings::system::HandheldSleepPlan
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum HandheldSleepPlan {
    Sleep1Min = 0,
    Sleep3Min = 1,
    Sleep5Min = 2,
    Sleep10Min = 3,
    Sleep30Min = 4,
    Never = 5,
}

/// nn::settings::system::HdmiContentType
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum HdmiContentType {
    None = 0,
    Graphics = 1,
    Cinema = 2,
    Photo = 3,
    Game = 4,
}

/// nn::settings::system::CmuMode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum CmuMode {
    None = 0,
    ColorInvert = 1,
    HighContrast = 2,
    GrayScale = 3,
}

/// nn::settings::system::KeyboardLayout
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum KeyboardLayout {
    Japanese = 0,
    EnglishUs = 1,
    EnglishUsInternational = 2,
    EnglishUk = 3,
    French = 4,
    FrenchCa = 5,
    Spanish = 6,
    SpanishLatin = 7,
    German = 8,
    Italian = 9,
    Portuguese = 10,
    Russian = 11,
    Korean = 12,
    ChineseSimplified = 13,
    ChineseTraditional = 14,
}

/// nn::settings::Language
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Language {
    Japanese = 0,
    AmericanEnglish = 1,
    French = 2,
    German = 3,
    Italian = 4,
    Spanish = 5,
    Chinese = 6,
    Korean = 7,
    Dutch = 8,
    Portiguesue = 9,
    Russian = 10,
    Taiwanese = 11,
    BritishEnglish = 12,
    CanadianFrench = 13,
    LatinAmericanSpanish = 14,
    SimplifiedChinese = 15,
    TraditionalChinese = 16,
    BrazilianPortuguese = 17,
}

/// nn::settings::LanguageCode - NUL-terminated string stored in a u64.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
pub enum LanguageCode {
    Ja = 0x000000000000616A,
    EnUs = 0x00000053552D6E65,
    Fr = 0x0000000000007266,
    De = 0x0000000000006564,
    It = 0x0000000000007469,
    Es = 0x0000000000007365,
    ZhCn = 0x0000004E432D687A,
    Ko = 0x0000000000006F6B,
    Nl = 0x0000000000006C6E,
    Pt = 0x0000000000007470,
    Ru = 0x0000000000007572,
    ZhTw = 0x00000057542D687A,
    EnGb = 0x00000042472D6E65,
    FrCa = 0x00000041432D7266,
    Es419 = 0x00003931342D7365,
    ZhHans = 0x00736E61482D687A,
    ZhHant = 0x00746E61482D687A,
    PtBr = 0x00000052422D7470,
}

pub const AVAILABLE_LANGUAGE_CODES: [LanguageCode; 18] = [
    LanguageCode::Ja,
    LanguageCode::EnUs,
    LanguageCode::Fr,
    LanguageCode::De,
    LanguageCode::It,
    LanguageCode::Es,
    LanguageCode::ZhCn,
    LanguageCode::Ko,
    LanguageCode::Nl,
    LanguageCode::Pt,
    LanguageCode::Ru,
    LanguageCode::ZhTw,
    LanguageCode::EnGb,
    LanguageCode::FrCa,
    LanguageCode::Es419,
    LanguageCode::ZhHans,
    LanguageCode::ZhHant,
    LanguageCode::PtBr,
];

pub const LANGUAGE_TO_LAYOUT: [(LanguageCode, KeyboardLayout); 18] = [
    (LanguageCode::Ja, KeyboardLayout::Japanese),
    (LanguageCode::EnUs, KeyboardLayout::EnglishUs),
    (LanguageCode::Fr, KeyboardLayout::French),
    (LanguageCode::De, KeyboardLayout::German),
    (LanguageCode::It, KeyboardLayout::Italian),
    (LanguageCode::Es, KeyboardLayout::Spanish),
    (LanguageCode::ZhCn, KeyboardLayout::ChineseSimplified),
    (LanguageCode::Ko, KeyboardLayout::Korean),
    (LanguageCode::Nl, KeyboardLayout::EnglishUsInternational),
    (LanguageCode::Pt, KeyboardLayout::Portuguese),
    (LanguageCode::Ru, KeyboardLayout::Russian),
    (LanguageCode::ZhTw, KeyboardLayout::ChineseTraditional),
    (LanguageCode::EnGb, KeyboardLayout::EnglishUk),
    (LanguageCode::FrCa, KeyboardLayout::FrenchCa),
    (LanguageCode::Es419, KeyboardLayout::SpanishLatin),
    (LanguageCode::ZhHans, KeyboardLayout::ChineseSimplified),
    (LanguageCode::ZhHant, KeyboardLayout::ChineseTraditional),
    (LanguageCode::PtBr, KeyboardLayout::Portuguese),
];

/// nn::settings::system::NotificationVolume
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum NotificationVolume {
    Mute = 0,
    Low = 1,
    High = 2,
}

/// nn::settings::system::PrimaryAlbumStorage
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum PrimaryAlbumStorage {
    Nand = 0,
    SdCard = 1,
}

/// nn::settings::system::QuestFlag
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum QuestFlag {
    Retail = 0,
    Kiosk = 1,
}

/// nn::settings::system::RgbRange
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum RgbRange {
    Auto = 0,
    Full = 1,
    Limited = 2,
}

/// nn::settings::system::RegionCode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SystemRegionCode {
    Japan = 0,
    Usa = 1,
    Europe = 2,
    Australia = 3,
    HongKongTaiwanKorea = 4,
    China = 5,
}

/// nn::settings::system::TouchScreenMode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum TouchScreenMode {
    Stylus = 0,
    Standard = 1,
}

/// nn::settings::system::TvResolution
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum TvResolution {
    Auto = 0,
    Resolution1080p = 1,
    Resolution720p = 2,
    Resolution480p = 3,
}

/// nn::settings::system::PlatformRegion
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum PlatformRegion {
    Global = 1,
    Terra = 2,
}

/// nn::settings::system::ChineseTraditionalInputMethod
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ChineseTraditionalInputMethod {
    Unknown0 = 0,
    Unknown1 = 1,
    Unknown2 = 2,
}

/// nn::settings::system::AccountSettings
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct AccountSettings {
    pub flags: u32,
}

/// nn::settings::factory::BatteryLot
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct BatteryLot {
    pub lot_number: [u8; 0x18],
}

/// nn::settings::factory::SerialNumber
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SerialNumber {
    pub serial_number: [u8; 0x18],
}

/// nn::settings::system::FirmwareVersionFormat
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct FirmwareVersionFormat {
    pub major: u8,
    pub minor: u8,
    pub micro: u8,
    pub _padding0: u8,
    pub revision_major: u8,
    pub revision_minor: u8,
    pub _padding1: [u8; 2],
    pub platform: [u8; 0x20],
    pub version_hash: [u8; 0x40],
    pub display_version: [u8; 0x18],
    pub display_title: [u8; 0x80],
}

const _: () = assert!(std::mem::size_of::<FirmwareVersionFormat>() == 0x100);

/// nn::settings::system::HomeMenuScheme
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct HomeMenuScheme {
    pub main: u32,
    pub back: u32,
    pub sub: u32,
    pub bezel: u32,
    pub extra: u32,
}

const _: () = assert!(std::mem::size_of::<HomeMenuScheme>() == 0x14);

/// nn::settings::system::NotificationTime
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NotificationTime {
    pub hour: u32,
    pub minute: u32,
}

/// nn::settings::system::NotificationSettings
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NotificationSettings {
    pub flags: u32, // NotificationFlag
    pub volume: u32, // NotificationVolume
    pub start_time: NotificationTime,
    pub stop_time: NotificationTime,
}

const _: () = assert!(std::mem::size_of::<NotificationSettings>() == 0x18);

/// nn::settings::system::SleepSettings
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct SleepSettings {
    pub flags: u32, // SleepFlag
    pub handheld_sleep_plan: u32, // HandheldSleepPlan
    pub console_sleep_plan: u32, // ConsoleSleepPlan
}

const _: () = assert!(std::mem::size_of::<SleepSettings>() == 0xC);

/// nn::settings::system::TvSettings
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct TvSettings {
    pub flags: u32, // TvFlag
    pub tv_resolution: u32, // TvResolution
    pub hdmi_content_type: u32, // HdmiContentType
    pub rgb_range: u32, // RgbRange
    pub cmu_mode: u32, // CmuMode
    pub tv_underscan: u32,
    pub tv_gama: f32,
    pub contrast_ratio: f32,
}

const _: () = assert!(std::mem::size_of::<TvSettings>() == 0x20);
