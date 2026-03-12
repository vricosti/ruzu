// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/spl_types.h
//!
//! Types and enums for the SPL service.

pub const AES_128_KEY_SIZE: usize = 0x10;

/// SMC function IDs.
///
/// Corresponds to `Smc::FunctionId` in upstream spl_types.h.
pub mod smc {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(u32)]
    pub enum FunctionId {
        SetConfig = 0xC3000401,
        GetConfig = 0xC3000002,
        GetResult = 0xC3000003,
        GetResultData = 0xC3000404,
        ModularExponentiate = 0xC3000E05,
        GenerateRandomBytes = 0xC3000006,
        GenerateAesKek = 0xC3000007,
        LoadAesKey = 0xC3000008,
        ComputeAes = 0xC3000009,
        GenerateSpecificAesKey = 0xC300000A,
        ComputeCmac = 0xC300040B,
        ReencryptDeviceUniqueData = 0xC300D60C,
        DecryptDeviceUniqueData = 0xC300100D,
        ModularExponentiateWithStorageKey = 0xC300060F,
        PrepareEsDeviceUniqueKey = 0xC3000610,
        LoadPreparedAesKey = 0xC3000011,
        PrepareCommonEsTitleKey = 0xC3000012,
        // Deprecated functions.
        LoadEsDeviceKey = 0xC300100C,
        DecryptAndStoreGcKey = 0xC300100E,
        // Atmosphere functions.
        AtmosphereIramCopy = 0xF0000201,
        AtmosphereReadWriteRegister = 0xF0000002,
        AtmosphereGetEmummcConfig = 0xF0000404,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(u32)]
    pub enum CipherMode {
        CbcEncrypt = 0,
        CbcDecrypt = 1,
        Ctr = 2,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(u32)]
    pub enum DeviceUniqueDataMode {
        DecryptDeviceUniqueData = 0,
        DecryptAndStoreGcKey = 1,
        DecryptAndStoreEsDeviceKey = 2,
        DecryptAndStoreSslKey = 3,
        DecryptAndStoreDrmDeviceCertKey = 4,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(u32)]
    pub enum ModularExponentiateWithStorageKeyMode {
        Gc = 0,
        Ssl = 1,
        DrmDeviceCert = 2,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(u32)]
    pub enum EsCommonKeyType {
        TitleKey = 0,
        ArchiveKey = 1,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(C)]
    pub struct AsyncOperationKey {
        pub value: u64,
    }
}

/// Hardware type.
///
/// Corresponds to `HardwareType` in upstream spl_types.h.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum HardwareType {
    Icosa = 0,
    Copper = 1,
    Hoag = 2,
    Iowa = 3,
    Calcio = 4,
    Aula = 5,
}

/// SoC type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SocType {
    Erista = 0,
    Mariko = 1,
}

/// Hardware state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum HardwareState {
    Development = 0,
    Production = 1,
}

/// Memory arrangement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum MemoryArrangement {
    Standard = 0,
    StandardForAppletDev = 1,
    StandardForSystemDev = 2,
    Expanded = 3,
    ExpandedForAppletDev = 4,
    Dynamic = 5,
}

/// Boot reason.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum BootReason {
    Unknown = 0,
    AcOk = 1,
    OnKey = 2,
    RtcAlarm1 = 3,
    RtcAlarm2 = 4,
}

/// Boot reason value (bitfield union).
///
/// Corresponds to `BootReasonValue` in upstream spl_types.h.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct BootReasonValue {
    pub value: u32,
}
const _: () = assert!(std::mem::size_of::<BootReasonValue>() == 4);

impl BootReasonValue {
    pub fn power_intr(&self) -> u8 {
        (self.value & 0xFF) as u8
    }
    pub fn rtc_intr(&self) -> u8 {
        ((self.value >> 8) & 0xFF) as u8
    }
    pub fn nv_erc(&self) -> u8 {
        ((self.value >> 16) & 0xFF) as u8
    }
    pub fn boot_reason(&self) -> u8 {
        ((self.value >> 24) & 0xFF) as u8
    }
}

/// AES key (128-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct AesKey {
    pub data64: [u64; AES_128_KEY_SIZE / 8],
}
const _: () = assert!(std::mem::size_of::<AesKey>() == AES_128_KEY_SIZE);

impl AesKey {
    pub fn as_bytes(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(self.data64.as_ptr() as *const u8, AES_128_KEY_SIZE)
        }
    }
    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        unsafe {
            std::slice::from_raw_parts_mut(self.data64.as_mut_ptr() as *mut u8, AES_128_KEY_SIZE)
        }
    }
}

/// IV/Counter (128-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct IvCtr {
    pub data64: [u64; AES_128_KEY_SIZE / 8],
}
const _: () = assert!(std::mem::size_of::<IvCtr>() == AES_128_KEY_SIZE);

/// CMAC (128-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Cmac {
    pub data64: [u64; AES_128_KEY_SIZE / 8],
}
const _: () = assert!(std::mem::size_of::<Cmac>() == AES_128_KEY_SIZE);

/// Access key (128-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct AccessKey {
    pub data64: [u64; AES_128_KEY_SIZE / 8],
}
const _: () = assert!(std::mem::size_of::<AccessKey>() == AES_128_KEY_SIZE);

/// Key source (128-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct KeySource {
    pub data64: [u64; AES_128_KEY_SIZE / 8],
}
const _: () = assert!(std::mem::size_of::<KeySource>() == AES_128_KEY_SIZE);

/// Configuration items.
///
/// Corresponds to `ConfigItem` in upstream spl_types.h.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ConfigItem {
    // Standard config items.
    DisableProgramVerification = 1,
    DramId = 2,
    SecurityEngineInterruptNumber = 3,
    FuseVersion = 4,
    HardwareType = 5,
    HardwareState = 6,
    IsRecoveryBoot = 7,
    DeviceId = 8,
    BootReason = 9,
    MemoryMode = 10,
    IsDevelopmentFunctionEnabled = 11,
    KernelConfiguration = 12,
    IsChargerHiZModeEnabled = 13,
    QuestState = 14,
    RegulatorType = 15,
    DeviceUniqueKeyGeneration = 16,
    Package2Hash = 17,
    // Extension config items for exosphere.
    ExosphereApiVersion = 65000,
    ExosphereNeedsReboot = 65001,
    ExosphereNeedsShutdown = 65002,
    ExosphereGitCommitHash = 65003,
    ExosphereHasRcmBugPatch = 65004,
    ExosphereBlankProdInfo = 65005,
    ExosphereAllowCalWrites = 65006,
    ExosphereEmummcType = 65007,
    ExospherePayloadAddress = 65008,
    ExosphereLogConfiguration = 65009,
    ExosphereForceEnableUsb30 = 65010,
}

impl ConfigItem {
    pub fn from_u32(value: u32) -> Option<Self> {
        match value {
            1 => Some(ConfigItem::DisableProgramVerification),
            2 => Some(ConfigItem::DramId),
            3 => Some(ConfigItem::SecurityEngineInterruptNumber),
            4 => Some(ConfigItem::FuseVersion),
            5 => Some(ConfigItem::HardwareType),
            6 => Some(ConfigItem::HardwareState),
            7 => Some(ConfigItem::IsRecoveryBoot),
            8 => Some(ConfigItem::DeviceId),
            9 => Some(ConfigItem::BootReason),
            10 => Some(ConfigItem::MemoryMode),
            11 => Some(ConfigItem::IsDevelopmentFunctionEnabled),
            12 => Some(ConfigItem::KernelConfiguration),
            13 => Some(ConfigItem::IsChargerHiZModeEnabled),
            14 => Some(ConfigItem::QuestState),
            15 => Some(ConfigItem::RegulatorType),
            16 => Some(ConfigItem::DeviceUniqueKeyGeneration),
            17 => Some(ConfigItem::Package2Hash),
            65000 => Some(ConfigItem::ExosphereApiVersion),
            65001 => Some(ConfigItem::ExosphereNeedsReboot),
            65002 => Some(ConfigItem::ExosphereNeedsShutdown),
            65003 => Some(ConfigItem::ExosphereGitCommitHash),
            65004 => Some(ConfigItem::ExosphereHasRcmBugPatch),
            65005 => Some(ConfigItem::ExosphereBlankProdInfo),
            65006 => Some(ConfigItem::ExosphereAllowCalWrites),
            65007 => Some(ConfigItem::ExosphereEmummcType),
            65008 => Some(ConfigItem::ExospherePayloadAddress),
            65009 => Some(ConfigItem::ExosphereLogConfiguration),
            65010 => Some(ConfigItem::ExosphereForceEnableUsb30),
            _ => None,
        }
    }
}
