// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/platform_service_manager.cpp/.h
//!
//! Platform font management service (pl:s, pl:u).

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
pub enum FontArchives {
    Extension = 0x0100000000000810,
    Standard = 0x0100000000000811,
    Korean = 0x0100000000000812,
    ChineseTraditional = 0x0100000000000813,
    ChineseSimple = 0x0100000000000814,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SharedFontType {
    JapanUSEuropeStandard = 0,
    ChineseSimplified = 1,
    ExtendedChineseSimplified = 2,
    ChineseTraditional = 3,
    KoreanHangul = 4,
    NintendoExtended = 5,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum LoadState {
    Loading = 0,
    Loaded = 1,
}

pub const SHARED_FONTS: &[(FontArchives, &str)] = &[
    (FontArchives::Standard, "nintendo_udsg-r_std_003.bfttf"),
    (FontArchives::ChineseSimple, "nintendo_udsg-r_org_zh-cn_003.bfttf"),
    (FontArchives::ChineseSimple, "nintendo_udsg-r_ext_zh-cn_003.bfttf"),
    (FontArchives::ChineseTraditional, "nintendo_udjxh-db_zh-tw_003.bfttf"),
    (FontArchives::Korean, "nintendo_udsg-r_ko_003.bfttf"),
    (FontArchives::Extension, "nintendo_ext_003.bfttf"),
    (FontArchives::Extension, "nintendo_ext2_003.bfttf"),
];

/// Constants for shared font decryption
pub const EXPECTED_RESULT: u32 = 0x7f9a0218;
pub const EXPECTED_MAGIC: u32 = 0x36f81a1e;
pub const SHARED_FONT_MEM_SIZE: u64 = 0x1100000;

pub const IPLATFORM_SERVICE_MANAGER_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "RequestLoad"),
    (1, true, "GetLoadState"),
    (2, true, "GetSize"),
    (3, true, "GetSharedMemoryAddressOffset"),
    (4, true, "GetSharedMemoryNativeHandle"),
    (5, true, "GetSharedFontInOrderOfPriority"),
    (6, true, "GetSharedFontInOrderOfPriorityForSystem"),
    (100, false, "RequestApplicationFunctionAuthorization"),
    (101, false, "RequestApplicationFunctionAuthorizationByProcessId"),
    (102, false, "RequestApplicationFunctionAuthorizationByApplicationId"),
    (103, false, "RefreshApplicationFunctionBlackListDebugRecord"),
    (104, false, "RequestApplicationFunctionAuthorizationByProgramId"),
    (105, false, "GetFunctionBlackListSystemVersionToAuthorize"),
    (106, false, "GetFunctionBlackListVersion"),
    (1000, false, "LoadNgWordDataForPlatformRegionChina"),
    (1001, false, "GetNgWordDataSizeForPlatformRegionChina"),
];

#[derive(Debug, Clone, Copy, Default)]
pub struct FontRegion {
    pub offset: u32,
    pub size: u32,
}

pub const EMPTY_REGION: FontRegion = FontRegion { offset: 0, size: 0 };
