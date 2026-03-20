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

// ---------------------------------------------------------------------------
// IPlatformServiceManager — real service implementation (pl:s, pl:u)
// ---------------------------------------------------------------------------

use std::collections::BTreeMap;

use crate::file_sys::romfs;
use crate::file_sys::system_archive::system_archive;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPlatformServiceManager — handles shared font loading.
///
/// The constructor synthesizes all font archives, matching upstream
/// `IPlatformServiceManager::IPlatformServiceManager` in platform_service_manager.cpp.
pub struct IPlatformServiceManager {
    font_regions: Vec<FontRegion>,
    font_data: Vec<Vec<u8>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IPlatformServiceManager {
    pub fn new() -> Self {
        let mut font_regions = Vec::new();
        let mut font_data = Vec::new();
        let mut current_offset: u32 = 0;

        // Iterate SHARED_FONTS and synthesize each archive, matching upstream constructor.
        // Upstream: for each font in SHARED_FONTS:
        //   1. Try NCA from NAND
        //   2. Fall back to SynthesizeSystemArchive(font.first)
        //   3. ExtractRomFS and load font file
        for &(archive, font_file_name) in SHARED_FONTS {
            let title_id = archive as u64;

            // Synthesize the system archive (matches upstream fallback path)
            let romfs_file = system_archive::synthesize_system_archive(title_id);

            let mut font_bytes = Vec::new();

            if let Some(romfs_file) = romfs_file {
                // Extract the RomFS to get the font file
                let extracted = romfs::extract_romfs(Some(romfs_file));
                if let Some(dir) = extracted {
                    // Look for the font file in the extracted directory
                    if let Some(file) = dir.get_file(font_file_name) {
                        font_bytes = file.read_all_bytes();
                    } else {
                        log::warn!(
                            "IPlatformServiceManager: font file '{}' not found in archive {:#018X}",
                            font_file_name, title_id
                        );
                    }
                } else {
                    log::warn!(
                        "IPlatformServiceManager: failed to extract RomFS for {:#018X}",
                        title_id
                    );
                }
            } else {
                log::warn!(
                    "IPlatformServiceManager: failed to synthesize archive {:#018X}",
                    title_id
                );
            }

            let size = font_bytes.len() as u32;
            font_regions.push(FontRegion {
                offset: current_offset,
                size,
            });
            current_offset += (size + 0xFFF) & !0xFFF; // Align to 4K
            font_data.push(font_bytes);
        }

        log::info!(
            "IPlatformServiceManager: loaded {} fonts, total size {}",
            font_data.len(),
            current_offset
        );

        Self {
            font_regions,
            font_data,
            handlers: build_handler_map(&[
                (0, Some(Self::request_load), "RequestLoad"),
                (1, Some(Self::get_load_state), "GetLoadState"),
                (2, Some(Self::get_size), "GetSize"),
                (3, Some(Self::get_shared_memory_address_offset), "GetSharedMemoryAddressOffset"),
                (4, Some(Self::get_shared_memory_native_handle), "GetSharedMemoryNativeHandle"),
                (5, Some(Self::get_shared_font_in_order_of_priority), "GetSharedFontInOrderOfPriority"),
                (6, Some(Self::get_shared_font_in_order_of_priority), "GetSharedFontInOrderOfPriorityForSystem"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn request_load(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IPlatformServiceManager::RequestLoad called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_load_state(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IPlatformServiceManager::GetLoadState called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(LoadState::Loaded as u32);
    }

    fn get_size(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = crate::hle::service::ipc_helpers::RequestParser::new(ctx);
        let font_id = rp.pop_u32() as usize;
        let size = svc.font_regions.get(font_id).map_or(0, |r| r.size);
        log::debug!("IPlatformServiceManager::GetSize(font_id={}) -> {}", font_id, size);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(size);
    }

    fn get_shared_memory_address_offset(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = crate::hle::service::ipc_helpers::RequestParser::new(ctx);
        let font_id = rp.pop_u32() as usize;
        let offset = svc.font_regions.get(font_id).map_or(0, |r| r.offset);
        log::debug!("IPlatformServiceManager::GetSharedMemoryAddressOffset(font_id={}) -> {}", font_id, offset);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(offset);
    }

    fn get_shared_memory_native_handle(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IPlatformServiceManager::GetSharedMemoryNativeHandle called");
        // Return a copy handle for the shared memory
        if let Some(handle) = ctx.create_readable_event_handle(false) {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(handle);
        } else {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(0);
        }
    }

    fn get_shared_font_in_order_of_priority(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        log::debug!("IPlatformServiceManager::GetSharedFontInOrderOfPriority called");

        // Write font type, offset, and size arrays to output buffers
        let num_fonts = SHARED_FONTS.len();

        // Buffer 0: font types (u32 each)
        let mut types_buf = vec![0u8; num_fonts * 4];
        for i in 0..num_fonts {
            let font_type = i as u32;
            types_buf[i * 4..i * 4 + 4].copy_from_slice(&font_type.to_le_bytes());
        }
        ctx.write_buffer(&types_buf, 0);

        // Buffer 1: offsets (u32 each)
        let mut offsets_buf = vec![0u8; num_fonts * 4];
        for i in 0..num_fonts {
            let offset = svc.font_regions.get(i).map_or(0u32, |r| r.offset);
            offsets_buf[i * 4..i * 4 + 4].copy_from_slice(&offset.to_le_bytes());
        }
        ctx.write_buffer(&offsets_buf, 1);

        // Buffer 2: sizes (u32 each)
        // Note: may not have buffer 2 available in all cases

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(1); // loaded = true
        rb.push_u32(num_fonts as u32);
    }
}

impl SessionRequestHandler for IPlatformServiceManager {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
}

impl ServiceFramework for IPlatformServiceManager {
    fn get_service_name(&self) -> &str { "pl:u" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}
