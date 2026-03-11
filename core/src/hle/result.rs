// SPDX-FileCopyrightText: 2014 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/result.h
//! Status: COMPLET
//!
//! The Result type union, ErrorModule enum, ResultRange, and ResultCode.
//! All constants come from http://switchbrew.org/index.php?title=Error_codes

use common::bit_field;

/// Identifies the module which caused the error. Error codes can be propagated through a call
/// chain, meaning that this doesn't always correspond to the module where the API call made is
/// contained.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum ErrorModule {
    Common = 0,
    Kernel = 1,
    FS = 2,
    /// Used for Memory, Thread, Mutex, Nvidia
    OS = 3,
    HTCS = 4,
    NCM = 5,
    DD = 6,
    LR = 8,
    Loader = 9,
    CMIF = 10,
    HIPC = 11,
    TMA = 12,
    DMNT = 13,
    GDS = 14,
    PM = 15,
    NS = 16,
    BSDSockets = 17,
    HTC = 18,
    TSC = 19,
    NCMContent = 20,
    SM = 21,
    RO = 22,
    GC = 23,
    SDMMC = 24,
    OVLN = 25,
    SPL = 26,
    Socket = 27,
    HTCLOW = 29,
    DDSF = 30,
    HTCFS = 31,
    Async = 32,
    Util = 33,
    TIPC = 35,
    ANIF = 37,
    ETHC = 100,
    I2C = 101,
    GPIO = 102,
    UART = 103,
    CPAD = 104,
    Settings = 105,
    FTM = 106,
    WLAN = 107,
    XCD = 108,
    TMP451 = 109,
    NIFM = 110,
    HwOpus = 111,
    LSM6DS3 = 112,
    Bluetooth = 113,
    VI = 114,
    NFP = 115,
    Time = 116,
    FGM = 117,
    OE = 118,
    BH1730FVC = 119,
    PCIe = 120,
    Friends = 121,
    BCAT = 122,
    SSLSrv = 123,
    Account = 124,
    News = 125,
    Mii = 126,
    NFC = 127,
    AM = 128,
    PlayReport = 129,
    AHID = 130,
    Qlaunch = 132,
    PCV = 133,
    USBPD = 134,
    BPC = 135,
    PSM = 136,
    NIM = 137,
    PSC = 138,
    TC = 139,
    USB = 140,
    NSD = 141,
    PCTL = 142,
    BTM = 143,
    LA = 144,
    ETicket = 145,
    NGC = 146,
    ERPT = 147,
    APM = 148,
    CEC = 149,
    Profiler = 150,
    ErrorUpload = 151,
    LIDBE = 152,
    Audio = 153,
    NPNS = 154,
    NPNSHTTPSTREAM = 155,
    ARP = 157,
    SWKBD = 158,
    BOOT = 159,
    NetDiag = 160,
    NFCMifare = 161,
    UserlandAssert = 162,
    Fatal = 163,
    NIMShop = 164,
    SPSM = 165,
    BGTC = 167,
    UserlandCrash = 168,
    SASBUS = 169,
    PI = 170,
    AudioCtrl = 172,
    LBL = 173,
    JIT = 175,
    HDCP = 176,
    OMM = 177,
    PDM = 178,
    OLSC = 179,
    SREPO = 180,
    Dauth = 181,
    STDFU = 182,
    DBG = 183,
    DHCPS = 186,
    SPI = 187,
    AVM = 188,
    PWM = 189,
    RTC = 191,
    Regulator = 192,
    LED = 193,
    SIO = 195,
    PCM = 196,
    CLKRST = 197,
    POWCTL = 198,
    AudioOld = 201,
    HID = 202,
    LDN = 203,
    CS = 204,
    Irsensor = 205,
    Capture = 206,
    Manu = 208,
    ATK = 209,
    WEB = 210,
    LCS = 211,
    GRC = 212,
    Repair = 213,
    Album = 214,
    RID = 215,
    Migration = 216,
    MigrationLdcServ = 217,
    HIDBUS = 218,
    ENS = 219,
    WebSocket = 223,
    DCDMTP = 227,
    PGL = 228,
    Notification = 229,
    INS = 230,
    LP2P = 231,
    RCD = 232,
    LCM40607 = 233,
    PRC = 235,
    TMAHTC = 237,
    ECTX = 238,
    MNPP = 239,
    HSHL = 240,
    CAPMTP = 242,
    DP2HDMI = 244,
    Cradle = 245,
    SProfile = 246,
    NDRM = 250,
    TSPM = 499,
    DevMenu = 500,
    GeneralWebApplet = 800,
    WifiWebAuthApplet = 809,
    WhitelistedApplet = 810,
    ShopN = 811,
}

impl ErrorModule {
    /// Convert from a raw u32 value.
    /// Returns None if the value does not correspond to a known module.
    pub fn from_u32(value: u32) -> Option<Self> {
        // Safety: We validate the discriminant before transmuting.
        // For now, use a match to be safe.
        Some(match value {
            0 => Self::Common,
            1 => Self::Kernel,
            2 => Self::FS,
            3 => Self::OS,
            4 => Self::HTCS,
            5 => Self::NCM,
            6 => Self::DD,
            8 => Self::LR,
            9 => Self::Loader,
            10 => Self::CMIF,
            11 => Self::HIPC,
            12 => Self::TMA,
            13 => Self::DMNT,
            14 => Self::GDS,
            15 => Self::PM,
            16 => Self::NS,
            17 => Self::BSDSockets,
            18 => Self::HTC,
            19 => Self::TSC,
            20 => Self::NCMContent,
            21 => Self::SM,
            22 => Self::RO,
            23 => Self::GC,
            24 => Self::SDMMC,
            25 => Self::OVLN,
            26 => Self::SPL,
            27 => Self::Socket,
            29 => Self::HTCLOW,
            30 => Self::DDSF,
            31 => Self::HTCFS,
            32 => Self::Async,
            33 => Self::Util,
            35 => Self::TIPC,
            37 => Self::ANIF,
            100 => Self::ETHC,
            101 => Self::I2C,
            102 => Self::GPIO,
            103 => Self::UART,
            104 => Self::CPAD,
            105 => Self::Settings,
            106 => Self::FTM,
            107 => Self::WLAN,
            108 => Self::XCD,
            109 => Self::TMP451,
            110 => Self::NIFM,
            111 => Self::HwOpus,
            112 => Self::LSM6DS3,
            113 => Self::Bluetooth,
            114 => Self::VI,
            115 => Self::NFP,
            116 => Self::Time,
            117 => Self::FGM,
            118 => Self::OE,
            119 => Self::BH1730FVC,
            120 => Self::PCIe,
            121 => Self::Friends,
            122 => Self::BCAT,
            123 => Self::SSLSrv,
            124 => Self::Account,
            125 => Self::News,
            126 => Self::Mii,
            127 => Self::NFC,
            128 => Self::AM,
            129 => Self::PlayReport,
            130 => Self::AHID,
            132 => Self::Qlaunch,
            133 => Self::PCV,
            134 => Self::USBPD,
            135 => Self::BPC,
            136 => Self::PSM,
            137 => Self::NIM,
            138 => Self::PSC,
            139 => Self::TC,
            140 => Self::USB,
            141 => Self::NSD,
            142 => Self::PCTL,
            143 => Self::BTM,
            144 => Self::LA,
            145 => Self::ETicket,
            146 => Self::NGC,
            147 => Self::ERPT,
            148 => Self::APM,
            149 => Self::CEC,
            150 => Self::Profiler,
            151 => Self::ErrorUpload,
            152 => Self::LIDBE,
            153 => Self::Audio,
            154 => Self::NPNS,
            155 => Self::NPNSHTTPSTREAM,
            157 => Self::ARP,
            158 => Self::SWKBD,
            159 => Self::BOOT,
            160 => Self::NetDiag,
            161 => Self::NFCMifare,
            162 => Self::UserlandAssert,
            163 => Self::Fatal,
            164 => Self::NIMShop,
            165 => Self::SPSM,
            167 => Self::BGTC,
            168 => Self::UserlandCrash,
            169 => Self::SASBUS,
            170 => Self::PI,
            172 => Self::AudioCtrl,
            173 => Self::LBL,
            175 => Self::JIT,
            176 => Self::HDCP,
            177 => Self::OMM,
            178 => Self::PDM,
            179 => Self::OLSC,
            180 => Self::SREPO,
            181 => Self::Dauth,
            182 => Self::STDFU,
            183 => Self::DBG,
            186 => Self::DHCPS,
            187 => Self::SPI,
            188 => Self::AVM,
            189 => Self::PWM,
            191 => Self::RTC,
            192 => Self::Regulator,
            193 => Self::LED,
            195 => Self::SIO,
            196 => Self::PCM,
            197 => Self::CLKRST,
            198 => Self::POWCTL,
            201 => Self::AudioOld,
            202 => Self::HID,
            203 => Self::LDN,
            204 => Self::CS,
            205 => Self::Irsensor,
            206 => Self::Capture,
            208 => Self::Manu,
            209 => Self::ATK,
            210 => Self::WEB,
            211 => Self::LCS,
            212 => Self::GRC,
            213 => Self::Repair,
            214 => Self::Album,
            215 => Self::RID,
            216 => Self::Migration,
            217 => Self::MigrationLdcServ,
            218 => Self::HIDBUS,
            219 => Self::ENS,
            223 => Self::WebSocket,
            227 => Self::DCDMTP,
            228 => Self::PGL,
            229 => Self::Notification,
            230 => Self::INS,
            231 => Self::LP2P,
            232 => Self::RCD,
            233 => Self::LCM40607,
            235 => Self::PRC,
            237 => Self::TMAHTC,
            238 => Self::ECTX,
            239 => Self::MNPP,
            240 => Self::HSHL,
            242 => Self::CAPMTP,
            244 => Self::DP2HDMI,
            245 => Self::Cradle,
            246 => Self::SProfile,
            250 => Self::NDRM,
            499 => Self::TSPM,
            500 => Self::DevMenu,
            800 => Self::GeneralWebApplet,
            809 => Self::WifiWebAuthApplet,
            810 => Self::WhitelistedApplet,
            811 => Self::ShopN,
            _ => return None,
        })
    }
}

// Bitfield layout for Result:
//   Module:      bits [0, 9)   — 9 bits, ErrorModule
//   Description: bits [9, 22)  — 13 bits, u32
const MODULE_POSITION: usize = 0;
const MODULE_BITS: usize = 9;
const DESCRIPTION_POSITION: usize = 9;
const DESCRIPTION_BITS: usize = 13;

/// Encapsulates a Horizon OS error code, allowing it to be separated into its constituent fields.
///
/// This is a newtype over `u32` with the same bitfield layout as the C++ `Result` union:
/// - bits [0, 9):  ErrorModule
/// - bits [9, 22): description
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct ResultCode(pub u32);

impl ResultCode {
    /// Create a ResultCode from a raw u32 value.
    #[inline]
    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }

    /// Create a ResultCode from a module and description.
    #[inline]
    pub const fn from_module_description(module: ErrorModule, description: u32) -> Self {
        // Inline bitfield packing to keep this const.
        // format_value(v, pos, bits) = (v << pos) & mask(pos, bits)
        let module_mask = ((1u32 << MODULE_BITS) - 1) << MODULE_POSITION;
        let desc_mask = ((1u32 << DESCRIPTION_BITS) - 1) << DESCRIPTION_POSITION;
        let raw = ((module as u32) << MODULE_POSITION) & module_mask
            | ((description) << DESCRIPTION_POSITION) & desc_mask;
        Self(raw)
    }

    /// Returns true if the result represents success (raw value is 0).
    #[inline]
    pub const fn is_success(self) -> bool {
        self.0 == 0
    }

    /// Returns true if the result represents an error.
    #[inline]
    pub const fn is_error(self) -> bool {
        !self.is_success()
    }

    /// Returns true if the result represents a failure (alias for is_error).
    #[inline]
    pub const fn is_failure(self) -> bool {
        !self.is_success()
    }

    /// Get the raw u32 value.
    #[inline]
    pub const fn get_inner_value(self) -> u32 {
        self.0
    }

    /// Extract the ErrorModule from the result code.
    #[inline]
    pub fn get_module(self) -> ErrorModule {
        let raw_module = bit_field::extract_unsigned(self.0, MODULE_POSITION, MODULE_BITS);
        ErrorModule::from_u32(raw_module).unwrap_or(ErrorModule::Common)
    }

    /// Extract the raw module value from the result code (without enum conversion).
    #[inline]
    pub const fn get_module_raw(self) -> u32 {
        // Manual const extraction since bit_field::extract_unsigned is not const
        (self.0 >> MODULE_POSITION) & ((1 << MODULE_BITS) - 1)
    }

    /// Extract the description from the result code.
    #[inline]
    pub const fn get_description(self) -> u32 {
        // Manual const extraction since bit_field::extract_unsigned is not const
        (self.0 >> DESCRIPTION_POSITION) & ((1 << DESCRIPTION_BITS) - 1)
    }

    /// Check if this result includes another result (exact match).
    #[inline]
    pub const fn includes(self, result: ResultCode) -> bool {
        self.get_inner_value() == result.get_inner_value()
    }
}

impl core::fmt::Debug for ResultCode {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.is_success() {
            write!(f, "ResultCode(Success)")
        } else {
            write!(
                f,
                "ResultCode(module={}, description={})",
                self.get_module_raw(),
                self.get_description()
            )
        }
    }
}

impl core::fmt::Display for ResultCode {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.is_success() {
            write!(f, "Success")
        } else {
            write!(f, "Error(module={}, desc={})", self.get_module_raw(), self.get_description())
        }
    }
}

/// The default success `ResultCode`.
pub const RESULT_SUCCESS: ResultCode = ResultCode::new(0);

/// Placeholder result code used for unknown error codes.
///
/// This should only be used when a particular error code is not known yet.
pub const RESULT_UNKNOWN: ResultCode = ResultCode::new(u32::MAX);

/// A ResultRange defines an inclusive range of error descriptions within an error module.
/// This can be used to check whether the description of a given ResultCode falls within the range.
/// The conversion function returns a ResultCode with its description set to description_start.
///
/// # Example
/// ```
/// use core::hle::result::{ResultRange, ResultCode, ErrorModule, RESULT_SUCCESS};
///
/// const RESULT_COMMON_ERROR: ResultRange = ResultRange::new(ErrorModule::Common, 0, 9999);
///
/// fn example(value: i32) -> ResultCode {
///     let result = ResultCode::from_module_description(ErrorModule::Common, 42);
///     if RESULT_COMMON_ERROR.includes(result) {
///         return RESULT_COMMON_ERROR.as_result();
///     }
///     RESULT_SUCCESS
/// }
/// ```
pub struct ResultRange {
    code: ResultCode,
    description_end: u32,
}

impl ResultRange {
    /// Create a new ResultRange from module, start description, and end description (inclusive).
    #[inline]
    pub const fn new(module: ErrorModule, description_start: u32, description_end: u32) -> Self {
        Self {
            code: ResultCode::from_module_description(module, description_start),
            description_end,
        }
    }

    /// Convert this range to its base ResultCode (with description_start as the description).
    #[inline]
    pub const fn as_result(&self) -> ResultCode {
        self.code
    }

    /// Check if the given result falls within this range.
    /// Returns true if the module matches and the description is within [start, end] inclusive.
    #[inline]
    pub const fn includes(&self, other: ResultCode) -> bool {
        self.code.get_module_raw() == other.get_module_raw()
            && self.code.get_description() <= other.get_description()
            && other.get_description() <= self.description_end
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_result_success() {
        assert!(RESULT_SUCCESS.is_success());
        assert!(!RESULT_SUCCESS.is_error());
        assert!(!RESULT_SUCCESS.is_failure());
        assert_eq!(RESULT_SUCCESS.get_inner_value(), 0);
    }

    #[test]
    fn test_result_unknown() {
        assert!(RESULT_UNKNOWN.is_error());
        assert!(RESULT_UNKNOWN.is_failure());
        assert!(!RESULT_UNKNOWN.is_success());
        assert_eq!(RESULT_UNKNOWN.get_inner_value(), u32::MAX);
    }

    #[test]
    fn test_result_from_module_description() {
        let result = ResultCode::from_module_description(ErrorModule::Kernel, 1);
        assert!(result.is_error());
        assert_eq!(result.get_module(), ErrorModule::Kernel);
        assert_eq!(result.get_module_raw(), 1);
        assert_eq!(result.get_description(), 1);
    }

    #[test]
    fn test_result_bitfield_layout() {
        // Module is bits [0, 9), description is bits [9, 22)
        // Module = 2 (FS), description = 1
        let result = ResultCode::from_module_description(ErrorModule::FS, 1);
        // raw = (2 << 0) | (1 << 9) = 2 | 512 = 514
        assert_eq!(result.get_inner_value(), 2 | (1 << 9));
    }

    #[test]
    fn test_result_equality() {
        let a = ResultCode::from_module_description(ErrorModule::FS, 42);
        let b = ResultCode::from_module_description(ErrorModule::FS, 42);
        let c = ResultCode::from_module_description(ErrorModule::FS, 43);
        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_result_includes() {
        let a = ResultCode::from_module_description(ErrorModule::FS, 42);
        let b = ResultCode::from_module_description(ErrorModule::FS, 42);
        let c = ResultCode::from_module_description(ErrorModule::FS, 43);
        assert!(a.includes(b));
        assert!(!a.includes(c));
    }

    #[test]
    fn test_result_range_includes() {
        // Description field is 13 bits wide, max value 8191.
        let range = ResultRange::new(ErrorModule::Common, 0, 7999);
        let in_range = ResultCode::from_module_description(ErrorModule::Common, 500);
        let out_of_range = ResultCode::from_module_description(ErrorModule::Common, 8000);
        let wrong_module = ResultCode::from_module_description(ErrorModule::Kernel, 500);

        assert!(range.includes(in_range));
        assert!(!range.includes(out_of_range));
        assert!(!range.includes(wrong_module));
    }

    #[test]
    fn test_result_range_boundary() {
        let range = ResultRange::new(ErrorModule::FS, 10, 20);
        assert!(range.includes(ResultCode::from_module_description(ErrorModule::FS, 10)));
        assert!(range.includes(ResultCode::from_module_description(ErrorModule::FS, 15)));
        assert!(range.includes(ResultCode::from_module_description(ErrorModule::FS, 20)));
        assert!(!range.includes(ResultCode::from_module_description(ErrorModule::FS, 9)));
        assert!(!range.includes(ResultCode::from_module_description(ErrorModule::FS, 21)));
    }

    #[test]
    fn test_result_range_as_result() {
        let range = ResultRange::new(ErrorModule::FS, 10, 20);
        let result = range.as_result();
        assert_eq!(result.get_module(), ErrorModule::FS);
        assert_eq!(result.get_description(), 10);
    }

    #[test]
    fn test_error_module_from_u32() {
        assert_eq!(ErrorModule::from_u32(0), Some(ErrorModule::Common));
        assert_eq!(ErrorModule::from_u32(1), Some(ErrorModule::Kernel));
        assert_eq!(ErrorModule::from_u32(2), Some(ErrorModule::FS));
        assert_eq!(ErrorModule::from_u32(811), Some(ErrorModule::ShopN));
        assert_eq!(ErrorModule::from_u32(7), None); // gap in enum
        assert_eq!(ErrorModule::from_u32(9999), None);
    }
}
