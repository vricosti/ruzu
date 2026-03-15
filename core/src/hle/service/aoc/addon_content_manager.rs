// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/aoc/addon_content_manager.h
//! Port of zuyu/src/core/hle/service/aoc/addon_content_manager.cpp
//!
//! IAddOnContentManager service ("aoc:u").

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerFactory,
    SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use crate::hle::service::sm::sm::ServiceManager;

/// IPC command IDs for IAddOnContentManager
pub mod commands {
    pub const COUNT_ADD_ON_CONTENT_BY_APPLICATION_ID: u32 = 0;
    pub const LIST_ADD_ON_CONTENT_BY_APPLICATION_ID: u32 = 1;
    pub const COUNT_ADD_ON_CONTENT: u32 = 2;
    pub const LIST_ADD_ON_CONTENT: u32 = 3;
    pub const GET_ADD_ON_CONTENT_BASE_ID_BY_APPLICATION_ID: u32 = 4;
    pub const GET_ADD_ON_CONTENT_BASE_ID: u32 = 5;
    pub const PREPARE_ADD_ON_CONTENT_BY_APPLICATION_ID: u32 = 6;
    pub const PREPARE_ADD_ON_CONTENT: u32 = 7;
    pub const GET_ADD_ON_CONTENT_LIST_CHANGED_EVENT: u32 = 8;
    pub const GET_ADD_ON_CONTENT_LOST_ERROR_CODE: u32 = 9;
    pub const GET_ADD_ON_CONTENT_LIST_CHANGED_EVENT_WITH_PROCESS_ID: u32 = 10;
    pub const NOTIFY_MOUNT_ADD_ON_CONTENT: u32 = 11;
    pub const NOTIFY_UNMOUNT_ADD_ON_CONTENT: u32 = 12;
    pub const IS_ADD_ON_CONTENT_MOUNTED_FOR_DEBUG: u32 = 13;
    pub const CHECK_ADD_ON_CONTENT_MOUNT_STATUS: u32 = 50;
    pub const CREATE_EC_PURCHASED_EVENT_MANAGER: u32 = 100;
    pub const CREATE_PERMANENT_EC_PURCHASED_EVENT_MANAGER: u32 = 101;
    pub const CREATE_CONTENTS_SERVICE_MANAGER: u32 = 110;
    pub const SET_REQUIRED_ADD_ON_CONTENTS_ON_CONTENTS_AVAILABILITY_TRANSITION: u32 = 200;
    pub const SETUP_HOST_ADD_ON_CONTENT: u32 = 300;
    pub const GET_REGISTERED_ADD_ON_CONTENT_PATH: u32 = 301;
    pub const UPDATE_CACHED_LIST: u32 = 302;
}

/// IAddOnContentManager service.
///
/// Corresponds to `IAddOnContentManager` in upstream `addon_content_manager.h`.
pub struct IAddOnContentManager {
    add_on_content: Vec<u64>,
    // TODO: service_context, aoc_change_event
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAddOnContentManager {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (
                commands::COUNT_ADD_ON_CONTENT,
                Some(Self::count_add_on_content_handler),
                "CountAddOnContent",
            ),
            (
                commands::GET_ADD_ON_CONTENT_BASE_ID,
                Some(Self::get_add_on_content_base_id_handler),
                "GetAddOnContentBaseId",
            ),
            (
                commands::PREPARE_ADD_ON_CONTENT,
                Some(Self::prepare_add_on_content_handler),
                "PrepareAddOnContent",
            ),
            (
                commands::CREATE_EC_PURCHASED_EVENT_MANAGER,
                Some(Self::create_ec_purchased_event_manager_handler),
                "CreateEcPurchasedEventManager",
            ),
            (
                commands::CREATE_PERMANENT_EC_PURCHASED_EVENT_MANAGER,
                Some(Self::create_permanent_ec_purchased_event_manager_handler),
                "CreatePermanentEcPurchasedEventManager",
            ),
        ]);
        Self {
            add_on_content: Vec::new(),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Stubbed: CountAddOnContent (cmd 2)
    pub fn count_add_on_content(&self, _process_id: u64) -> u32 {
        log::debug!("IAddOnContentManager::count_add_on_content called");
        // TODO: implement with actual content provider
        0
    }

    /// Stubbed: ListAddOnContent (cmd 3)
    pub fn list_add_on_content(
        &self,
        _offset: u32,
        _count: u32,
        _process_id: u64,
    ) -> (u32, Vec<u32>) {
        log::debug!("IAddOnContentManager::list_add_on_content called");
        // TODO: implement with actual content provider
        (0, Vec::new())
    }

    /// Stubbed: GetAddOnContentBaseId (cmd 5)
    pub fn get_add_on_content_base_id(&self, _process_id: u64) -> u64 {
        log::debug!("IAddOnContentManager::get_add_on_content_base_id called");
        // TODO: implement with actual title ID lookup
        0
    }

    /// Stubbed: PrepareAddOnContent (cmd 7)
    pub fn prepare_add_on_content(&self, addon_index: i32, process_id: u64) {
        log::warn!(
            "(STUBBED) IAddOnContentManager::prepare_add_on_content called, addon_index={}, process_id={}",
            addon_index,
            process_id
        );
    }

    /// Stubbed: GetAddOnContentListChangedEvent (cmd 8)
    pub fn get_add_on_content_list_changed_event(&self) {
        log::warn!("(STUBBED) IAddOnContentManager::get_add_on_content_list_changed_event called");
        // TODO: return event handle
    }

    /// Stubbed: GetAddOnContentListChangedEventWithProcessId (cmd 10)
    pub fn get_add_on_content_list_changed_event_with_process_id(&self, _process_id: u64) {
        log::warn!("(STUBBED) IAddOnContentManager::get_add_on_content_list_changed_event_with_process_id called");
        // TODO: return event handle
    }

    /// Stubbed: NotifyMountAddOnContent (cmd 11)
    pub fn notify_mount_add_on_content(&self) {
        log::warn!("(STUBBED) IAddOnContentManager::notify_mount_add_on_content called");
    }

    /// Stubbed: NotifyUnmountAddOnContent (cmd 12)
    pub fn notify_unmount_add_on_content(&self) {
        log::warn!("(STUBBED) IAddOnContentManager::notify_unmount_add_on_content called");
    }

    /// Stubbed: CheckAddOnContentMountStatus (cmd 50)
    pub fn check_add_on_content_mount_status(&self) {
        log::warn!("(STUBBED) IAddOnContentManager::check_add_on_content_mount_status called");
    }

    /// Stubbed: CreateEcPurchasedEventManager (cmd 100)
    pub fn create_ec_purchased_event_manager(&self) -> Arc<super::purchase_event_manager::IPurchaseEventManager> {
        log::warn!("(STUBBED) IAddOnContentManager::create_ec_purchased_event_manager called");
        Arc::new(super::purchase_event_manager::IPurchaseEventManager::new())
    }

    /// Stubbed: CreatePermanentEcPurchasedEventManager (cmd 101)
    pub fn create_permanent_ec_purchased_event_manager(&self) -> Arc<super::purchase_event_manager::IPurchaseEventManager> {
        log::warn!("(STUBBED) IAddOnContentManager::create_permanent_ec_purchased_event_manager called");
        Arc::new(super::purchase_event_manager::IPurchaseEventManager::new())
    }

    fn count_add_on_content_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAddOnContentManager) };
        let count = service.count_add_on_content(0);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn get_add_on_content_base_id_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAddOnContentManager) };
        let base_id = service.get_add_on_content_base_id(0);

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(base_id);
    }

    fn prepare_add_on_content_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAddOnContentManager) };
        let mut rp = RequestParser::new(ctx);
        let addon_index = rp.pop_i32();
        service.prepare_add_on_content(addon_index, 0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn create_ec_purchased_event_manager_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAddOnContentManager) };
        let manager = service.create_ec_purchased_event_manager();
        let handle = ctx.create_session_for_service(manager).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_move_objects(handle);
    }

    fn create_permanent_ec_purchased_event_manager_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAddOnContentManager) };
        let manager = service.create_permanent_ec_purchased_event_manager();
        let handle = ctx.create_session_for_service(manager).unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_move_objects(handle);
    }
}

/// Registers "aoc:u" service.
///
/// Corresponds to `LoopProcess` in upstream `addon_content_manager.cpp`.
pub fn loop_process(service_manager: &Arc<Mutex<ServiceManager>>) {
    let factory: SessionRequestHandlerFactory =
        Box::new(|| -> SessionRequestHandlerPtr { Arc::new(IAddOnContentManager::new()) });
    let result = service_manager
        .lock()
        .unwrap()
        .register_service("aoc:u".to_string(), 64, factory);
    if result.is_error() {
        log::warn!(
            "Failed to register service 'aoc:u': {:#x}",
            result.get_inner_value()
        );
    }
}

impl SessionRequestHandler for IAddOnContentManager {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IAddOnContentManager {
    fn get_service_name(&self) -> &str {
        "aoc:u"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
