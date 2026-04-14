// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/platform_service_manager.cpp/.h

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::core::SystemRef;
use crate::file_sys::system_archive::data::{
    font_chinese_simplified, font_chinese_traditional, font_extended_chinese_simplified,
    font_korean, font_nintendo_extended, font_standard,
};
use crate::hle::kernel::k_shared_memory::{KSharedMemory, MemoryPermission};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct FontRegion {
    pub offset: u32,
    pub size: u32,
}

pub const SHARED_FONT_MEM_SIZE: u64 = 0x1100000;
const EMPTY_REGION: FontRegion = FontRegion { offset: 0, size: 0 };
const MAX_ELEMENT_COUNT: usize = 6;

const SHARED_FONTS: [(FontArchives, &[u8]); 7] = [
    (FontArchives::Standard, &font_standard::FONT_STANDARD),
    (
        FontArchives::ChineseSimple,
        &font_chinese_simplified::FONT_CHINESE_SIMPLIFIED,
    ),
    (
        FontArchives::ChineseSimple,
        &font_extended_chinese_simplified::FONT_EXTENDED_CHINESE_SIMPLIFIED,
    ),
    (
        FontArchives::ChineseTraditional,
        &font_chinese_traditional::FONT_CHINESE_TRADITIONAL,
    ),
    (FontArchives::Korean, &font_korean::FONT_KOREAN),
    (
        FontArchives::Extension,
        &font_nintendo_extended::FONT_NINTENDO_EXTENDED,
    ),
    (
        FontArchives::Extension,
        &font_nintendo_extended::FONT_NINTENDO_EXTENDED,
    ),
];

pub const IPLATFORM_SERVICE_MANAGER_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "RequestLoad"),
    (1, true, "GetLoadState"),
    (2, true, "GetSize"),
    (3, true, "GetSharedMemoryAddressOffset"),
    (4, true, "GetSharedMemoryNativeHandle"),
    (5, true, "GetSharedFontInOrderOfPriority"),
    (6, true, "GetSharedFontInOrderOfPriorityForSystem"),
    (100, false, "RequestApplicationFunctionAuthorization"),
    (
        101,
        false,
        "RequestApplicationFunctionAuthorizationByProcessId",
    ),
    (
        102,
        false,
        "RequestApplicationFunctionAuthorizationByApplicationId",
    ),
    (103, false, "RefreshApplicationFunctionBlackListDebugRecord"),
    (
        104,
        false,
        "RequestApplicationFunctionAuthorizationByProgramId",
    ),
    (105, false, "GetFunctionBlackListSystemVersionToAuthorize"),
    (106, false, "GetFunctionBlackListVersion"),
    (1000, false, "LoadNgWordDataForPlatformRegionChina"),
    (1001, false, "GetNgWordDataSizeForPlatformRegionChina"),
];

fn align_up_8(value: usize) -> usize {
    (value + 7) & !7
}

fn build_shared_font_blob() -> (Vec<u8>, Vec<FontRegion>) {
    let mut blob = vec![0u8; SHARED_FONT_MEM_SIZE as usize];
    let mut regions = Vec::with_capacity(SHARED_FONTS.len());
    let mut offset = 0usize;

    for (_, font_bytes) in SHARED_FONTS {
        let size = font_bytes.len();
        if offset + size > blob.len() {
            break;
        }
        blob[offset..offset + size].copy_from_slice(font_bytes);
        regions.push(FontRegion {
            offset: offset as u32,
            size: size as u32,
        });
        offset = align_up_8(offset + size);
    }

    (blob, regions)
}

pub struct IPlatformServiceManager {
    system: SystemRef,
    service_name: &'static str,
    shared_font_bytes: Vec<u8>,
    shared_font_regions: Vec<FontRegion>,
    shared_memory: Mutex<Option<(u64, Arc<KSharedMemory>)>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IPlatformServiceManager {
    pub fn new(system: SystemRef, service_name: &'static str) -> Self {
        let (shared_font_bytes, shared_font_regions) = build_shared_font_blob();
        Self {
            system,
            service_name,
            shared_font_bytes,
            shared_font_regions,
            shared_memory: Mutex::new(None),
            handlers: build_handler_map(&[
                (0, Some(Self::request_load), "RequestLoad"),
                (1, Some(Self::get_load_state), "GetLoadState"),
                (2, Some(Self::get_size), "GetSize"),
                (
                    3,
                    Some(Self::get_shared_memory_address_offset),
                    "GetSharedMemoryAddressOffset",
                ),
                (
                    4,
                    Some(Self::get_shared_memory_native_handle),
                    "GetSharedMemoryNativeHandle",
                ),
                (
                    5,
                    Some(Self::get_shared_font_in_order_of_priority),
                    "GetSharedFontInOrderOfPriority",
                ),
                (
                    6,
                    Some(Self::get_shared_font_in_order_of_priority),
                    "GetSharedFontInOrderOfPriorityForSystem",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn get_shared_font_region(&self, index: usize) -> FontRegion {
        self.shared_font_regions
            .get(index)
            .copied()
            .unwrap_or(EMPTY_REGION)
    }

    fn create_shared_memory_object(
        &self,
        ctx: &HLERequestContext,
    ) -> Option<(u64, Arc<KSharedMemory>)> {
        let thread = ctx.get_thread()?;
        let parent = thread.lock().unwrap().parent.as_ref()?.upgrade()?;

        let system_ptr =
            self.system.get() as *const crate::core::System as *mut crate::core::System;
        let device_memory_ptr = unsafe { (*system_ptr).device_memory() as *const _ };
        let kernel = unsafe { (*system_ptr).kernel_mut()? };

        let mut shmem = KSharedMemory::new();
        if shmem
            .initialize(
                unsafe { &*device_memory_ptr },
                kernel.memory_manager_mut(),
                MemoryPermission::Read,
                MemoryPermission::Read,
                SHARED_FONT_MEM_SIZE as usize,
            )
            .is_error()
        {
            return None;
        }

        let dst = shmem.get_pointer_mut(0);
        if dst.is_null() {
            return None;
        }
        unsafe {
            std::ptr::copy_nonoverlapping(
                self.shared_font_bytes.as_ptr(),
                dst,
                self.shared_font_bytes.len(),
            );
        }

        let object_id = kernel.create_new_object_id() as u64;
        Some((object_id, Arc::new(shmem)))
    }

    fn request_load(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let _ = rp.pop_u32();

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_load_state(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let _ = rp.pop_u32();

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(LoadState::Loaded as u32);
    }

    fn get_size(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let font_id = rp.pop_u32() as usize;

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(service.get_shared_font_region(font_id).size);
    }

    fn get_shared_memory_address_offset(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let font_id = rp.pop_u32() as usize;

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(service.get_shared_font_region(font_id).offset);
    }

    fn get_shared_memory_native_handle(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let handle = (|| -> Option<u32> {
            let thread = ctx.get_thread()?;
            let parent = thread.lock().unwrap().parent.as_ref()?.upgrade()?;

            let (object_id, shared_memory) = {
                let mut cached = service.shared_memory.lock().unwrap();
                if cached.is_none() {
                    *cached = service.create_shared_memory_object(ctx);
                }
                cached.as_ref()?.clone()
            };

            let mut process = parent.lock().unwrap();
            process.register_shared_memory_object(object_id, shared_memory);
            process.handle_table.add(object_id).ok()
        })();

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle.unwrap_or(0));
    }

    fn get_shared_font_in_order_of_priority(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let _language_code = rp.pop_u64();

        let max_size = service.shared_font_regions.len().min(MAX_ELEMENT_COUNT);

        let mut font_codes = vec![0u8; max_size * 4];
        let mut font_offsets = vec![0u8; max_size * 4];
        let mut font_sizes = vec![0u8; max_size * 4];

        for i in 0..max_size {
            let region = service.get_shared_font_region(i);
            font_codes[i * 4..(i + 1) * 4].copy_from_slice(&(i as u32).to_le_bytes());
            font_offsets[i * 4..(i + 1) * 4].copy_from_slice(&region.offset.to_le_bytes());
            font_sizes[i * 4..(i + 1) * 4].copy_from_slice(&region.size.to_le_bytes());
        }

        ctx.write_buffer(&font_codes, 0);
        ctx.write_buffer(&font_offsets, 1);
        ctx.write_buffer(&font_sizes, 2);

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(true);
        rb.push_u32(max_size as u32);
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
    fn get_service_name(&self) -> &str {
        self.service_name
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shared_font_blob_builds_seven_regions() {
        let (_blob, regions) = build_shared_font_blob();
        assert_eq!(regions.len(), SHARED_FONTS.len());
        assert!(regions.iter().all(|region| region.size > 0));
    }

    #[test]
    fn shared_font_blob_offsets_are_monotonic() {
        let (_blob, regions) = build_shared_font_blob();
        for pair in regions.windows(2) {
            assert!(pair[0].offset < pair[1].offset);
        }
    }
}
