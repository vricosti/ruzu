// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii.h
//! Port of zuyu/src/core/hle/service/mii/mii.cpp
//!
//! IStaticService and IDatabaseService for the Mii service.
//! Registers: mii:e and mii:u services.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::mii_manager::MiiManager;
use super::types::char_info::CharInfo;

/// Service names registered by the Mii module.
pub const SERVICE_NAME_E: &str = "mii:e";
pub const SERVICE_NAME_U: &str = "mii:u";

// ---------------------------------------------------------------------------
// IDatabaseService — port of upstream IDatabaseService (mii.cpp:22-293)
// ---------------------------------------------------------------------------

pub struct IDatabaseService {
    manager: Arc<Mutex<MiiManager>>,
    is_system: bool,
    interface_version: AtomicU32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDatabaseService {
    pub fn new(manager: Arc<Mutex<MiiManager>>, is_system: bool) -> Self {
        Self {
            manager,
            is_system,
            interface_version: AtomicU32::new(0),
            handlers: build_handler_map(&[
                (0, Some(Self::is_updated_handler), "IsUpdated"),
                (1, Some(Self::is_full_database_handler), "IsFullDatabase"),
                (2, Some(Self::get_count_handler), "GetCount"),
                (3, Some(Self::get_handler), "Get"),
                (4, Some(Self::get1_handler), "Get1"),
                (6, Some(Self::build_random_handler), "BuildRandom"),
                (7, Some(Self::build_default_handler), "BuildDefault"),
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
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn is_updated_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IDatabaseService::IsUpdated called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&(false as u8));
    }

    fn is_full_database_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IDatabaseService::IsFullDatabase called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&(false as u8));
    }

    fn get_count_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IDatabaseService::GetCount called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&0u32); // 0 Miis in the database
    }

    /// Cmd 3: Get — returns CharInfoElement array. We return count=0.
    fn get_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IDatabaseService::Get called");
        // Return 0 Miis — the buffer is untouched, just return count=0.
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&0u32);
    }

    /// Cmd 4: Get1 — returns CharInfo array. We return count=0.
    fn get1_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IDatabaseService::Get1 called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&0u32);
    }

    /// Cmd 6: BuildRandom — returns a random CharInfo.
    fn build_random_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IDatabaseService::BuildRandom called");
        let char_info = CharInfo::default();
        let words = (std::mem::size_of::<CharInfo>() + 3) / 4;
        let mut rb = ResponseBuilder::new(ctx, 2 + words as u32, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&char_info);
    }

    fn build_default_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let index = rp.pop_raw::<i32>();
        log::debug!("IDatabaseService::BuildDefault called, index={}", index);

        // Return a default (zeroed) CharInfo. Upstream builds from DefaultMii
        // data table, but the zeroed struct is valid and won't cause crashes.
        let char_info = CharInfo::default();
        let words = (std::mem::size_of::<CharInfo>() + 3) / 4;
        let mut rb = ResponseBuilder::new(ctx, 2 + words as u32, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&char_info);
    }

    fn is_broken_database_with_clear_flag_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        log::debug!("IDatabaseService::IsBrokenDatabaseWithClearFlag called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&(false as u8));
    }

    fn set_interface_version_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IDatabaseService) };
        let mut rp = RequestParser::new(ctx);
        let version = rp.pop_raw::<u32>();
        log::debug!(
            "IDatabaseService::SetInterfaceVersion called, version={}",
            version
        );
        service.interface_version.store(version, Ordering::Relaxed);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
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
    pub is_system: bool,
    manager: Arc<Mutex<MiiManager>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IStaticService {
    pub fn new(manager: Arc<Mutex<MiiManager>>, is_system: bool) -> Self {
        Self {
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
        let is_domain = ctx
            .get_manager()
            .map_or(false, |manager| manager.lock().unwrap().is_domain());
        let move_handle = if is_domain {
            0
        } else {
            ctx.create_session_for_service(object.clone()).unwrap_or(0)
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(object);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    fn get_database_service_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IStaticService) };
        log::info!(
            "IStaticService::GetDatabaseService called, is_system={}",
            service.is_system
        );

        let db_service = Arc::new(IDatabaseService::new(
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
// LoopProcess — register mii:e and mii:u
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
            Arc::new(IStaticService::new(Arc::clone(&mgr_e), true))
        }),
        64,
    );

    let mgr_u = Arc::clone(&manager);
    server_manager.register_named_service(
        "mii:u",
        Box::new(move || -> SessionRequestHandlerPtr {
            Arc::new(IStaticService::new(Arc::clone(&mgr_u), false))
        }),
        64,
    );

    ServerManager::run_server(server_manager);
}
