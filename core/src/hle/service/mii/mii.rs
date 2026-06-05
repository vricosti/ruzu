// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii.h
//! Port of zuyu/src/core/hle/service/mii/mii.cpp
//!
//! IStaticService and IDatabaseService for the Mii service.
//! Registers: mii:e and mii:u services.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::core::SystemRef;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::cmif_serialization::{write_out_array_bytes, CmifRequest, CmifResponse};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use crate::hle::service::set::system_settings_server::SystemSettingsService;
use crate::hle::service::sm::sm::ServiceManager;

use super::mii_manager::MiiManager;
use super::mii_result::{RESULT_INVALID_ARGUMENT, RESULT_PERMISSION_DENIED, RESULT_TEST_MODE_ONLY};
use super::mii_types::{Age, DatabaseSessionMetadata, Gender, Race, SourceFlag};
use super::types::char_info::{CharInfo, CharInfoElement};
use super::types::core_data::CoreData;
use super::types::store_data::{StoreData, StoreDataElement};
use super::types::ver3_store_data::Ver3StoreData;

/// Service names registered by the Mii module.
pub const SERVICE_NAME_E: &str = "mii:e";
pub const SERVICE_NAME_U: &str = "mii:u";

// ---------------------------------------------------------------------------
// IDatabaseService — port of upstream IDatabaseService (mii.cpp:22-293)
// ---------------------------------------------------------------------------

pub struct IDatabaseService {
    set_sys: Arc<SystemSettingsService>,
    manager: Arc<Mutex<MiiManager>>,
    is_system: bool,
    metadata: Mutex<DatabaseSessionMetadata>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDatabaseService {
    fn validate_build_random_args(
        age_raw: u8,
        gender_raw: u8,
        race_raw: u8,
    ) -> Result<(Age, Gender, Race), ResultCode> {
        let age = match age_raw {
            0 => Age::Young,
            1 => Age::Normal,
            2 => Age::Old,
            3 => Age::All,
            _ => return Err(RESULT_INVALID_ARGUMENT),
        };
        let gender = match gender_raw {
            0 => Gender::Male,
            1 => Gender::Female,
            2 => Gender::All,
            _ => return Err(RESULT_INVALID_ARGUMENT),
        };
        let race = match race_raw {
            0 => Race::Black,
            1 => Race::White,
            2 => Race::Asian,
            3 => Race::All,
            _ => return Err(RESULT_INVALID_ARGUMENT),
        };
        Ok((age, gender, race))
    }

    fn get_set_sys_service_from_handler(
        set_sys_handler: SessionRequestHandlerPtr,
    ) -> Arc<SystemSettingsService> {
        assert!(
            set_sys_handler.as_any().is::<SystemSettingsService>(),
            "set:sys is not an ISystemSettingsServer"
        );
        unsafe { Arc::from_raw(Arc::into_raw(set_sys_handler) as *const SystemSettingsService) }
    }

    fn get_set_sys_service(system: SystemRef) -> Arc<SystemSettingsService> {
        let Some(service_manager) = system.get().service_manager() else {
            // Local harness adaptation: upstream always constructs this owner
            // with a live service manager and typed `set:sys` dependency.
            return Arc::new(SystemSettingsService::new());
        };
        let set_sys_handler =
            ServiceManager::get_service_blocking(&service_manager, system, "set:sys");
        Self::get_set_sys_service_from_handler(set_sys_handler)
    }

    fn query_db_test_mode_enabled(set_sys: &SystemSettingsService) -> bool {
        let inner = set_sys.inner.lock().unwrap();
        inner
            .get_settings_item_value_bytes("mii", "is_db_test_mode_enabled")
            .is_some_and(|bytes| bytes.first().copied().unwrap_or(0) != 0)
    }

    pub fn new(system: SystemRef, manager: Arc<Mutex<MiiManager>>, is_system: bool) -> Self {
        let mut metadata = DatabaseSessionMetadata::default();
        let set_sys = Self::get_set_sys_service(system);
        let is_db_test_mode_enabled = Self::query_db_test_mode_enabled(&set_sys);
        {
            let mut manager_guard = manager.lock().unwrap();
            manager_guard.set_test_db(is_db_test_mode_enabled);
            let _ = manager_guard.initialize(&mut metadata);
        }
        Self {
            set_sys,
            manager,
            is_system,
            metadata: Mutex::new(metadata),
            handlers: build_handler_map(&[
                (0, Some(Self::is_updated_handler), "IsUpdated"),
                (1, Some(Self::is_full_database_handler), "IsFullDatabase"),
                (2, Some(Self::get_count_handler), "GetCount"),
                (3, Some(Self::get_handler), "Get"),
                (4, Some(Self::get1_handler), "Get1"),
                (5, Some(Self::update_latest_handler), "UpdateLatest"),
                (6, Some(Self::build_random_handler), "BuildRandom"),
                (7, Some(Self::build_default_handler), "BuildDefault"),
                (8, Some(Self::get2_handler), "Get2"),
                (9, Some(Self::get3_handler), "Get3"),
                (10, Some(Self::update_latest1_handler), "UpdateLatest1"),
                (
                    20,
                    Some(Self::is_broken_database_with_clear_flag_handler),
                    "IsBrokenDatabaseWithClearFlag",
                ),
                (
                    22,
                    Some(Self::set_interface_version_handler),
                    "SetInterfaceVersion",
                ),
                (11, Some(Self::find_index_handler), "FindIndex"),
                (12, Some(Self::move_handler), "Move"),
                (13, Some(Self::add_or_replace_handler), "AddOrReplace"),
                (14, Some(Self::delete_handler), "Delete"),
                (15, Some(Self::destroy_file_handler), "DestroyFile"),
                (16, Some(Self::delete_file_handler), "DeleteFile"),
                (17, Some(Self::format_handler), "Format"),
                (18, None, "Import"),
                (19, None, "Export"),
                (21, Some(Self::get_index_handler), "GetIndex"),
                (23, Some(Self::convert_handler), "Convert"),
                (
                    24,
                    Some(Self::convert_core_data_to_char_info_handler),
                    "ConvertCoreDataToCharInfo",
                ),
                (
                    25,
                    Some(Self::convert_char_info_to_core_data_handler),
                    "ConvertCharInfoToCoreData",
                ),
                (26, Some(Self::append_handler), "Append"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn has_database_source(source_flag: u32) -> bool {
        (source_flag & SourceFlag::Database as u32) != 0
    }

    fn trace_char_info(
        stage: u64,
        cmd: u32,
        result: ResultCode,
        aux0: u64,
        aux1: u64,
        char_info: Option<&CharInfo>,
    ) {
        if !common::trace::is_enabled(common::trace::cat::MII_SERVICE) {
            return;
        }
        let (create_lo, create_hi, name_0_3, name_4_7, traits0, traits1) =
            if let Some(char_info) = char_info {
                let create_lo =
                    u64::from_le_bytes(char_info.create_id[0..8].try_into().unwrap());
                let create_hi =
                    u64::from_le_bytes(char_info.create_id[8..16].try_into().unwrap());
                let pack_name = |base: usize| -> u64 {
                    let mut packed = 0u64;
                    for index in 0..4 {
                        packed |= (char_info.name.data[base + index] as u64) << (index * 16);
                    }
                    packed
                };
                let traits0 = (char_info.font_region as u64)
                    | ((char_info.favorite_color as u64) << 8)
                    | ((char_info.gender as u64) << 16)
                    | ((char_info.height as u64) << 24)
                    | ((char_info.build as u64) << 32)
                    | ((char_info.type_val as u64) << 40)
                    | ((char_info.region_move as u64) << 48);
                let traits1 = (char_info.faceline_type as u64)
                    | ((char_info.hair_type as u64) << 8)
                    | ((char_info.eye_type as u64) << 16)
                    | ((char_info.eyebrow_type as u64) << 24)
                    | ((char_info.nose_type as u64) << 32)
                    | ((char_info.mouth_type as u64) << 40)
                    | ((char_info.glass_type as u64) << 48)
                    | ((char_info.mole_type as u64) << 56);
                (create_lo, create_hi, pack_name(0), pack_name(4), traits0, traits1)
            } else {
                (0, 0, 0, 0, 0, 0)
            };
        common::trace::emit_raw(
            common::trace::cat::MII_SERVICE,
            &[
                stage,
                cmd as u64,
                result.get_inner_value() as u64,
                aux0,
                aux1,
                create_lo,
                create_hi,
                name_0_3,
                name_4_7,
                traits0,
                traits1,
            ],
        );
    }

    fn is_db_test_mode_enabled(&self) -> bool {
        Self::query_db_test_mode_enabled(&self.set_sys)
    }

    fn is_updated_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(_this);
        let mut request = CmifRequest::new(ctx);
        let source_flag_raw = request.raw::<u32>();
        log::debug!(
            "IDatabaseService::IsUpdated called source_flag={:#x}",
            source_flag_raw
        );
        let is_updated = {
            let mut metadata = service.metadata.lock().unwrap();
            service
                .manager
                .lock()
                .unwrap()
                .is_updated(&mut metadata, source_flag_raw)
        };
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_bool(is_updated);
    }

    fn is_full_database_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        log::debug!("IDatabaseService::IsFullDatabase called");
        let is_full = service.manager.lock().unwrap().is_full_database();
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_bool(is_full);
    }

    fn get_count_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let source_flag_raw = request.raw::<u32>();
        let count = {
            let metadata = service.metadata.lock().unwrap();
            service
                .manager
                .lock()
                .unwrap()
                .get_count(&metadata, source_flag_raw)
        };
        log::debug!(
            "IDatabaseService::GetCount called source_flag={:#x} count={}",
            source_flag_raw,
            count
        );
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_raw(&count);
    }

    /// Cmd 3: Get — returns upstream `CharInfoElement` array.
    fn get_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let source_flag_raw = request.raw::<u32>();
        let max_count = ctx.get_write_buffer_size(0) / core::mem::size_of::<CharInfoElement>();
        let result = {
            let metadata = service.metadata.lock().unwrap();
            service
                .manager
                .lock()
                .unwrap()
                .get_char_info_elements_limited(&metadata, max_count, source_flag_raw)
        };
        match result {
            Ok(elements) => {
                let count = elements.len() as u32;
                Self::trace_char_info(
                    1,
                    3,
                    RESULT_SUCCESS,
                    source_flag_raw as u64,
                    count as u64,
                    elements.first().map(|(char_info, _)| char_info),
                );

                if !elements.is_empty() {
                    let elements: Vec<CharInfoElement> = elements
                        .into_iter()
                        .map(|(char_info, source)| CharInfoElement { char_info, source })
                        .collect();
                    let bytes = unsafe {
                        core::slice::from_raw_parts(
                            elements.as_ptr().cast::<u8>(),
                            elements.len() * core::mem::size_of::<CharInfoElement>(),
                        )
                    };
                    write_out_array_bytes(ctx, 0, &bytes);
                }

                log::debug!(
                    "IDatabaseService::Get called source_flag={:#x} count={}",
                    source_flag_raw,
                    count
                );
                let mut response = CmifResponse::new(ctx, 3, 0, 0);
                response.push_result(RESULT_SUCCESS);
                response.push_raw(&count);
            }
            Err(result) => {
                Self::trace_char_info(1, 3, result, source_flag_raw as u64, 0, None);
                CmifResponse::result_only(ctx, result);
            }
        }
    }

    /// Cmd 4: Get1 — returns CharInfo array (CharInfo per element, no Source field).
    /// Same semantics as Get but the element is just CharInfo (0x58 bytes).
    fn get1_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let source_flag_raw = request.raw::<u32>();
        let max_count = ctx.get_write_buffer_size(0) / core::mem::size_of::<CharInfo>();
        let result = {
            let metadata = service.metadata.lock().unwrap();
            service.manager.lock().unwrap().get_char_infos_limited(
                &metadata,
                max_count,
                source_flag_raw,
            )
        };
        match result {
            Ok(elements) => {
                let count = elements.len() as u32;
                Self::trace_char_info(
                    2,
                    4,
                    RESULT_SUCCESS,
                    source_flag_raw as u64,
                    count as u64,
                    elements.first(),
                );

                if !elements.is_empty() {
                    let mut bytes =
                        Vec::with_capacity(elements.len() * core::mem::size_of::<CharInfo>());
                    for char_info in elements {
                        bytes.extend_from_slice(unsafe {
                            core::slice::from_raw_parts(
                                (&char_info as *const CharInfo).cast::<u8>(),
                                core::mem::size_of::<CharInfo>(),
                            )
                        });
                    }
                    write_out_array_bytes(ctx, 0, &bytes);
                }

                log::debug!(
                    "IDatabaseService::Get1 called source_flag={:#x} count={}",
                    source_flag_raw,
                    count
                );
                let mut response = CmifResponse::new(ctx, 3, 0, 0);
                response.push_result(RESULT_SUCCESS);
                response.push_raw(&count);
            }
            Err(result) => {
                Self::trace_char_info(2, 4, result, source_flag_raw as u64, 0, None);
                CmifResponse::result_only(ctx, result);
            }
        }
    }

    /// Cmd 5: UpdateLatest — takes a CharInfo + SourceFlag, returns the latest CharInfo.
    /// Port of upstream `IDatabaseService::UpdateLatest` (mii.cpp:114).
    fn update_latest_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let char_info = request.raw::<CharInfo>();
        let source_flag_raw = request.raw::<u32>();
        log::debug!(
            "IDatabaseService::UpdateLatest called source_flag={:#x}",
            source_flag_raw
        );
        let result = {
            let metadata = service.metadata.lock().unwrap();
            service
                .manager
                .lock()
                .unwrap()
                .update_latest(&metadata, &char_info, source_flag_raw)
        };

        match result {
            Ok(latest_char_info) => {
                Self::trace_char_info(
                    3,
                    5,
                    RESULT_SUCCESS,
                    source_flag_raw as u64,
                    0,
                    Some(&latest_char_info),
                );
                let words = (std::mem::size_of::<CharInfo>() + 3) / 4;
                let mut response = CmifResponse::new(ctx, 2 + words as u32, 0, 0);
                response.push_result(RESULT_SUCCESS);
                response.push_raw(&latest_char_info);
            }
            Err(result) => {
                Self::trace_char_info(3, 5, result, source_flag_raw as u64, 0, Some(&char_info));
                CmifResponse::result_only(ctx, result);
            }
        }
    }

    /// Cmd 6: BuildRandom — returns a random CharInfo.
    fn build_random_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let age_raw = request.raw::<u8>();
        let gender_raw = request.raw::<u8>();
        let race_raw = request.raw::<u8>();
        log::debug!(
            "IDatabaseService::BuildRandom called age={} gender={} race={}",
            age_raw,
            gender_raw,
            race_raw
        );

        let (age, gender, race) =
            match Self::validate_build_random_args(age_raw, gender_raw, race_raw) {
                Ok(args) => args,
                Err(result) => {
                    Self::trace_char_info(
                        4,
                        6,
                        result,
                        (age_raw as u64) | ((gender_raw as u64) << 8) | ((race_raw as u64) << 16),
                        0,
                        None,
                    );
                    CmifResponse::result_only(ctx, result);
                    return;
                }
            };

        let char_info = service
            .manager
            .lock()
            .unwrap()
            .build_random(age, gender, race);
        Self::trace_char_info(
            4,
            6,
            RESULT_SUCCESS,
            (age_raw as u64) | ((gender_raw as u64) << 8) | ((race_raw as u64) << 16),
            0,
            Some(&char_info),
        );
        let words = (std::mem::size_of::<CharInfo>() + 3) / 4;
        let mut response = CmifResponse::new(ctx, 2 + words as u32, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_raw(&char_info);
    }

    fn build_default_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let index = request.raw::<i32>();
        log::debug!("IDatabaseService::BuildDefault called, index={}", index);

        if index < 0 || index as usize >= super::mii_types::DEFAULT_MII_COUNT {
            Self::trace_char_info(5, 7, RESULT_INVALID_ARGUMENT, index as u64, 0, None);
            CmifResponse::result_only(ctx, RESULT_INVALID_ARGUMENT);
            return;
        }

        let char_info = service.manager.lock().unwrap().build_default(index as u32);
        Self::trace_char_info(
            5,
            7,
            RESULT_SUCCESS,
            index as u64,
            0,
            Some(&char_info),
        );
        let words = (std::mem::size_of::<CharInfo>() + 3) / 4;
        let mut response = CmifResponse::new(ctx, 2 + words as u32, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_raw(&char_info);
    }

    fn get2_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let source_flag_raw = request.raw::<u32>();
        let max_count = ctx.get_write_buffer_size(0) / core::mem::size_of::<StoreDataElement>();
        let result = {
            let metadata = service.metadata.lock().unwrap();
            service
                .manager
                .lock()
                .unwrap()
                .get_store_data_elements_limited(&metadata, max_count, source_flag_raw)
        };
        match result {
            Ok(elements) => {
                let count = elements.len() as u32;
                let mut first_char_info = None;
                if let Some(element) = elements.first() {
                    let mut char_info = CharInfo::default();
                    char_info.set_from_store_data(&element.store_data);
                    first_char_info = Some(char_info);
                }
                Self::trace_char_info(
                    6,
                    8,
                    RESULT_SUCCESS,
                    source_flag_raw as u64,
                    count as u64,
                    first_char_info.as_ref(),
                );

                if !elements.is_empty() {
                    let mut bytes = Vec::with_capacity(
                        elements.len() * core::mem::size_of::<StoreDataElement>(),
                    );
                    for element in elements {
                        bytes.extend_from_slice(unsafe {
                            core::slice::from_raw_parts(
                                (&element as *const StoreDataElement).cast::<u8>(),
                                core::mem::size_of::<StoreDataElement>(),
                            )
                        });
                    }
                    write_out_array_bytes(ctx, 0, &bytes);
                }

                log::debug!(
                    "IDatabaseService::Get2 called source_flag={:#x} count={}",
                    source_flag_raw,
                    count
                );
                let mut response = CmifResponse::new(ctx, 3, 0, 0);
                response.push_result(RESULT_SUCCESS);
                response.push_raw(&count);
            }
            Err(result) => {
                Self::trace_char_info(6, 8, result, source_flag_raw as u64, 0, None);
                CmifResponse::result_only(ctx, result);
            }
        }
    }

    fn get3_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let source_flag_raw = request.raw::<u32>();
        let max_count = ctx.get_write_buffer_size(0) / core::mem::size_of::<StoreData>();
        let result = {
            let metadata = service.metadata.lock().unwrap();
            service.manager.lock().unwrap().get_store_datas_limited(
                &metadata,
                max_count,
                source_flag_raw,
            )
        };
        match result {
            Ok(elements) => {
                let count = elements.len() as u32;
                let mut first_char_info = None;
                if let Some(store_data) = elements.first() {
                    let mut char_info = CharInfo::default();
                    char_info.set_from_store_data(store_data);
                    first_char_info = Some(char_info);
                }
                Self::trace_char_info(
                    7,
                    9,
                    RESULT_SUCCESS,
                    source_flag_raw as u64,
                    count as u64,
                    first_char_info.as_ref(),
                );

                if !elements.is_empty() {
                    let mut bytes =
                        Vec::with_capacity(elements.len() * core::mem::size_of::<StoreData>());
                    for store_data in elements {
                        bytes.extend_from_slice(unsafe {
                            core::slice::from_raw_parts(
                                (&store_data as *const StoreData).cast::<u8>(),
                                core::mem::size_of::<StoreData>(),
                            )
                        });
                    }
                    write_out_array_bytes(ctx, 0, &bytes);
                }

                log::debug!(
                    "IDatabaseService::Get3 called source_flag={:#x} count={}",
                    source_flag_raw,
                    count
                );
                let mut response = CmifResponse::new(ctx, 3, 0, 0);
                response.push_result(RESULT_SUCCESS);
                response.push_raw(&count);
            }
            Err(result) => {
                Self::trace_char_info(7, 9, result, source_flag_raw as u64, 0, None);
                CmifResponse::result_only(ctx, result);
            }
        }
    }

    fn update_latest1_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let store_data = request.raw::<StoreData>();
        let source_flag_raw = request.raw::<u32>();
        log::debug!(
            "IDatabaseService::UpdateLatest1 called source_flag={:#x}",
            source_flag_raw
        );

        let result = if !service.is_system {
            Err(RESULT_PERMISSION_DENIED)
        } else {
            let metadata = service.metadata.lock().unwrap();
            service.manager.lock().unwrap().update_latest_store_data(
                &metadata,
                &store_data,
                source_flag_raw,
            )
        };

        match result {
            Ok(latest) => {
                let mut latest_char_info = CharInfo::default();
                latest_char_info.set_from_store_data(&latest);
                Self::trace_char_info(
                    8,
                    10,
                    RESULT_SUCCESS,
                    source_flag_raw as u64,
                    0,
                    Some(&latest_char_info),
                );
                let words = (std::mem::size_of::<StoreData>() + 3) / 4;
                let mut response = CmifResponse::new(ctx, 2 + words as u32, 0, 0);
                response.push_result(RESULT_SUCCESS);
                response.push_raw(&latest);
            }
            Err(result) => {
                let mut input_char_info = CharInfo::default();
                input_char_info.set_from_store_data(&store_data);
                Self::trace_char_info(
                    8,
                    10,
                    result,
                    source_flag_raw as u64,
                    0,
                    Some(&input_char_info),
                );
                CmifResponse::result_only(ctx, result);
            }
        }
    }

    fn is_broken_database_with_clear_flag_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        log::debug!("IDatabaseService::IsBrokenDatabaseWithClearFlag called");
        if !service.is_system {
            CmifResponse::result_only(ctx, RESULT_PERMISSION_DENIED);
            return;
        }

        let is_broken = {
            let mut metadata = service.metadata.lock().unwrap();
            service
                .manager
                .lock()
                .unwrap()
                .is_broken_with_clear_flag(&mut metadata)
        };

        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_bool(is_broken);
    }

    fn set_interface_version_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let version = request.raw::<u32>();
        log::debug!(
            "IDatabaseService::SetInterfaceVersion called, version={}",
            version
        );
        let mut metadata = service.metadata.lock().unwrap();
        service
            .manager
            .lock()
            .unwrap()
            .set_interface_version(&mut metadata, version);

        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(RESULT_SUCCESS);
    }

    fn find_index_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let create_id = request.raw::<[u8; 16]>();
        let is_special = request.raw::<u8>() != 0;
        log::info!(
            "IDatabaseService::FindIndex called create_id={:02x?} is_special={}",
            create_id,
            is_special
        );

        let out_index = service
            .manager
            .lock()
            .unwrap()
            .find_index(create_id, is_special);

        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_raw(&out_index);
    }

    fn get_index_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let char_info = request.raw::<CharInfo>();
        log::info!("IDatabaseService::GetIndex called");

        let result = {
            let metadata = service.metadata.lock().unwrap();
            service
                .manager
                .lock()
                .unwrap()
                .get_index(&metadata, &char_info)
        };

        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        match result {
            Ok(index) => {
                response.push_result(RESULT_SUCCESS);
                response.push_raw(&index);
            }
            Err(result) => {
                response.push_result(result);
                response.push_raw(&(-1i32));
            }
        }
    }

    fn move_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let create_id = request.raw::<[u8; 16]>();
        let new_index = request.raw::<i32>();
        log::info!(
            "IDatabaseService::Move called create_id={:02x?} new_index={}",
            create_id,
            new_index
        );

        let result = if !service.is_system {
            RESULT_PERMISSION_DENIED
        } else {
            let mut metadata = service.metadata.lock().unwrap();
            let count = service
                .manager
                .lock()
                .unwrap()
                .get_count(&metadata, SourceFlag::Database as u32);

            if new_index < 0 || new_index >= count as i32 {
                RESULT_INVALID_ARGUMENT
            } else {
                service
                    .manager
                    .lock()
                    .unwrap()
                    .r#move(&mut metadata, new_index as u32, create_id)
            }
        };

        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn add_or_replace_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let store_data = request.raw::<StoreData>();
        log::info!("IDatabaseService::AddOrReplace called");

        let result = if !service.is_system {
            RESULT_PERMISSION_DENIED
        } else {
            let mut metadata = service.metadata.lock().unwrap();
            service
                .manager
                .lock()
                .unwrap()
                .add_or_replace(&mut metadata, store_data)
        };

        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn delete_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let create_id = request.raw::<[u8; 16]>();
        log::info!(
            "IDatabaseService::Delete called create_id={:02x?}",
            create_id
        );

        let result = if !service.is_system {
            RESULT_PERMISSION_DENIED
        } else {
            let mut metadata = service.metadata.lock().unwrap();
            service
                .manager
                .lock()
                .unwrap()
                .delete(&mut metadata, create_id)
        };

        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn append_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let char_info = request.raw::<CharInfo>();
        log::info!("IDatabaseService::Append called");

        let result = {
            let mut metadata = service.metadata.lock().unwrap();
            service
                .manager
                .lock()
                .unwrap()
                .append(&mut metadata, &char_info)
        };

        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn destroy_file_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let is_db_test_mode_enabled = service.is_db_test_mode_enabled();
        log::info!(
            "IDatabaseService::DestroyFile called is_db_test_mode_enabled={}",
            is_db_test_mode_enabled
        );

        let result = if !is_db_test_mode_enabled {
            RESULT_TEST_MODE_ONLY
        } else {
            let mut metadata = service.metadata.lock().unwrap();
            service.manager.lock().unwrap().destroy_file(&mut metadata)
        };

        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn delete_file_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let is_db_test_mode_enabled = service.is_db_test_mode_enabled();
        log::info!(
            "IDatabaseService::DeleteFile called is_db_test_mode_enabled={}",
            is_db_test_mode_enabled
        );

        let result = if !is_db_test_mode_enabled {
            RESULT_TEST_MODE_ONLY
        } else {
            service.manager.lock().unwrap().delete_file()
        };

        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn format_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let is_db_test_mode_enabled = service.is_db_test_mode_enabled();
        log::info!(
            "IDatabaseService::Format called is_db_test_mode_enabled={}",
            is_db_test_mode_enabled
        );

        let result = if !is_db_test_mode_enabled {
            RESULT_TEST_MODE_ONLY
        } else {
            let mut metadata = service.metadata.lock().unwrap();
            service.manager.lock().unwrap().format(&mut metadata)
        };

        let mut response = CmifResponse::new(ctx, 2, 0, 0);
        response.push_result(result);
    }

    fn convert_core_data_to_char_info_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let core_data = request.raw::<CoreData>();
        log::info!("IDatabaseService::ConvertCoreDataToCharInfo called");

        match service
            .manager
            .lock()
            .unwrap()
            .convert_core_data_to_char_info(&core_data)
        {
            Ok(char_info) => {
                Self::trace_char_info(9, 24, RESULT_SUCCESS, 0, 0, Some(&char_info));
                let words = (std::mem::size_of::<CharInfo>() + 3) / 4;
                let mut response = CmifResponse::new(ctx, 2 + words as u32, 0, 0);
                response.push_result(RESULT_SUCCESS);
                response.push_raw(&char_info);
            }
            Err(result) => {
                Self::trace_char_info(9, 24, result, 0, 0, None);
                CmifResponse::result_only(ctx, result);
            }
        }
    }

    fn convert_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let mii_v3 = request.raw::<Ver3StoreData>();
        log::info!("IDatabaseService::Convert called");

        match service
            .manager
            .lock()
            .unwrap()
            .convert_v3_to_char_info(&mii_v3)
        {
            Ok(char_info) => {
                Self::trace_char_info(10, 23, RESULT_SUCCESS, 0, 0, Some(&char_info));
                let words = (std::mem::size_of::<CharInfo>() + 3) / 4;
                let mut response = CmifResponse::new(ctx, 2 + words as u32, 0, 0);
                response.push_result(RESULT_SUCCESS);
                response.push_raw(&char_info);
            }
            Err(result) => {
                Self::trace_char_info(10, 23, result, 0, 0, None);
                CmifResponse::result_only(ctx, result);
            }
        }
    }

    fn convert_char_info_to_core_data_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let char_info = request.raw::<CharInfo>();
        log::info!("IDatabaseService::ConvertCharInfoToCoreData called");

        match service
            .manager
            .lock()
            .unwrap()
            .convert_char_info_to_core_data(&char_info)
        {
            Ok(core_data) => {
                Self::trace_char_info(11, 25, RESULT_SUCCESS, 0, 0, Some(&char_info));
                let words = (std::mem::size_of::<CoreData>() + 3) / 4;
                let mut response = CmifResponse::new(ctx, 2 + words as u32, 0, 0);
                response.push_result(RESULT_SUCCESS);
                response.push_raw(&core_data);
            }
            Err(result) => {
                Self::trace_char_info(11, 25, result, 0, 0, Some(&char_info));
                CmifResponse::result_only(ctx, result);
            }
        }
    }
}

impl ServiceFramework for IDatabaseService {
    fn get_service_name(&self) -> &str {
        "IDatabaseService"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
    fn invoke_request(&self, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        if let Some(info) = self.handlers.get(&cmd) {
            if let Some(handler) = info.handler_callback {
                handler(self, ctx);
            }
        } else {
            <Self as ServiceFramework>::report_unimplemented_function(self, ctx, None);
        }
    }
}

impl SessionRequestHandler for IDatabaseService {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
    fn service_name(&self) -> &str {
        "IDatabaseService"
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

// ---------------------------------------------------------------------------
// IStaticService — port of upstream IStaticService (mii.cpp:295-316)
// ---------------------------------------------------------------------------

pub struct IStaticService {
    system: SystemRef,
    pub is_system: bool,
    manager: Arc<Mutex<MiiManager>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IStaticService {
    pub fn new(system: SystemRef, manager: Arc<Mutex<MiiManager>>, is_system: bool) -> Self {
        Self {
            system,
            is_system,
            manager,
            handlers: build_handler_map(&[(
                0,
                Some(Self::get_database_service_handler),
                "GetDatabaseService",
            )]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn push_interface_response(
        ctx: &mut HLERequestContext,
        object: Arc<dyn SessionRequestHandler>,
    ) {
        let mut response = CmifResponse::new(ctx, 2, 0, 1);
        response.push_result(RESULT_SUCCESS);
        response.push_interface(object);
    }

    fn get_database_service_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IStaticService) };
        log::info!(
            "IStaticService::GetDatabaseService called, is_system={}",
            service.is_system
        );

        let db_service = Arc::new(IDatabaseService::new(
            service.system,
            Arc::clone(&service.manager),
            service.is_system,
        ));
        Self::push_interface_response(ctx, db_service);
    }
}

impl ServiceFramework for IStaticService {
    fn get_service_name(&self) -> &str {
        "IStaticService"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
    fn invoke_request(&self, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        if let Some(info) = self.handlers.get(&cmd) {
            if let Some(handler) = info.handler_callback {
                handler(self, ctx);
            }
        } else {
            <Self as ServiceFramework>::report_unimplemented_function(self, ctx, None);
        }
    }
}

impl SessionRequestHandler for IStaticService {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
    fn service_name(&self) -> &str {
        "IStaticService"
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

// ---------------------------------------------------------------------------
// IImageDatabaseService — port of upstream IImageDatabaseService (mii.cpp:322-359)
// ---------------------------------------------------------------------------

pub struct IImageDatabaseService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IImageDatabaseService {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, Some(Self::initialize_handler), "Initialize"),
                (10, None, "Reload"),
                (11, Some(Self::get_count_handler), "GetCount"),
                (12, None, "IsEmpty"),
                (13, None, "IsFull"),
                (14, None, "GetAttribute"),
                (15, None, "LoadImage"),
                (16, None, "AddOrUpdateImage"),
                (17, None, "DeleteImages"),
                (100, None, "DeleteFile"),
                (101, None, "DestroyFile"),
                (102, None, "ImportFile"),
                (103, None, "ExportFile"),
                (104, None, "ForceInitialize"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn initialize_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::info!("IImageDatabaseService::Initialize called");
        CmifResponse::result_only(ctx, RESULT_SUCCESS);
    }

    fn get_count_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IImageDatabaseService::GetCount called");
        let count = 0u32;
        let mut response = CmifResponse::new(ctx, 3, 0, 0);
        response.push_result(RESULT_SUCCESS);
        response.push_raw(&count);
    }
}

impl ServiceFramework for IImageDatabaseService {
    fn get_service_name(&self) -> &str {
        "miiimg"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
    fn invoke_request(&self, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        if let Some(info) = self.handlers.get(&cmd) {
            if let Some(handler) = info.handler_callback {
                handler(self, ctx);
            }
        } else {
            <Self as ServiceFramework>::report_unimplemented_function(self, ctx, None);
        }
    }
}

impl SessionRequestHandler for IImageDatabaseService {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
    fn service_name(&self) -> &str {
        "miiimg"
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validate_build_random_args_accepts_upstream_enum_maxima() {
        let (age, gender, race) = IDatabaseService::validate_build_random_args(3, 2, 3).unwrap();
        assert_eq!(age, Age::All);
        assert_eq!(gender, Gender::All);
        assert_eq!(race, Race::All);
    }

    #[test]
    fn validate_build_random_args_rejects_out_of_range_values() {
        assert_eq!(
            IDatabaseService::validate_build_random_args(4, 2, 3),
            Err(RESULT_INVALID_ARGUMENT)
        );
        assert_eq!(
            IDatabaseService::validate_build_random_args(3, 3, 3),
            Err(RESULT_INVALID_ARGUMENT)
        );
        assert_eq!(
            IDatabaseService::validate_build_random_args(3, 2, 4),
            Err(RESULT_INVALID_ARGUMENT)
        );
    }

    #[test]
    fn build_handler_map_keeps_import_export_as_registered_null_handlers() {
        let handlers = build_handler_map(&[(18, None, "Import"), (19, None, "Export")]);
        assert!(handlers.contains_key(&18));
        assert!(handlers.contains_key(&19));
        assert!(handlers.get(&18).unwrap().handler_callback.is_none());
        assert!(handlers.get(&19).unwrap().handler_callback.is_none());
    }

    #[test]
    fn get_set_sys_service_from_handler_downcasts_to_typed_owner() {
        let handler: SessionRequestHandlerPtr = Arc::new(SystemSettingsService::new());
        let set_sys = IDatabaseService::get_set_sys_service_from_handler(handler);
        assert!(set_sys
            .inner
            .lock()
            .unwrap()
            .get_settings_item_value_bytes("mii", "is_db_test_mode_enabled")
            .is_some());
    }
}

// ---------------------------------------------------------------------------
// LoopProcess — register mii:e, mii:u, and miiimg
// ---------------------------------------------------------------------------

/// Entry point for the Mii service module.
/// Port of upstream `Mii::LoopProcess` (mii.cpp:363-373).
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let mut server_manager = ServerManager::new(system);
    let manager = Arc::new(Mutex::new(MiiManager::new()));

    let mgr_e = Arc::clone(&manager);
    server_manager.register_named_service(
        "mii:e",
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(IStaticService::new(system, Arc::clone(&mgr_e), true))
        }),
        64,
    );

    let mgr_u = Arc::clone(&manager);
    server_manager.register_named_service(
        "mii:u",
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(IStaticService::new(system, Arc::clone(&mgr_u), false))
        }),
        64,
    );

    server_manager.register_named_service(
        "miiimg",
        Box::new(move || -> SessionRequestHandlerPtr { Arc::new(IImageDatabaseService::new()) }),
        64,
    );

    ServerManager::run_server(server_manager);
}
