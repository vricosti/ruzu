// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/library_applet_creator.h
//! Port of zuyu/src/core/hle/service/am/service/library_applet_creator.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::core::SystemRef;
use crate::hle::result::{ResultCode, RESULT_SUCCESS, RESULT_UNKNOWN};
use crate::hle::service::am::service::storage::IStorage;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for ILibraryAppletCreator:
/// - 0: CreateLibraryApplet
/// - 1: TerminateAllLibraryApplets
/// - 2: AreAnyLibraryAppletsLeft
/// - 10: CreateStorage
/// - 11: CreateTransferMemoryStorage
/// - 12: CreateHandleStorage
pub struct ILibraryAppletCreator {
    system: SystemRef,
    applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
    window_system: Arc<Mutex<crate::hle::service::am::window_system::WindowSystem>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ILibraryAppletCreator {
    pub fn new(
        system: SystemRef,
        applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
        window_system: Arc<Mutex<crate::hle::service::am::window_system::WindowSystem>>,
    ) -> Self {
        let handlers = build_handler_map(&[
            (0, None, "CreateLibraryApplet"),
            (1, None, "TerminateAllLibraryApplets"),
            (2, None, "AreAnyLibraryAppletsLeft"),
            (10, Some(Self::create_storage_handler), "CreateStorage"),
            (
                11,
                Some(Self::create_transfer_memory_storage_handler),
                "CreateTransferMemoryStorage",
            ),
            (12, None, "CreateHandleStorage"),
        ]);
        Self {
            system,
            applet,
            window_system,
            handlers,
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

    fn create_storage_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let creator =
            unsafe { &*(this as *const dyn ServiceFramework as *const ILibraryAppletCreator) };
        let mut rp = RequestParser::new(ctx);
        let size = rp.pop_i64();
        log::debug!("ILibraryAppletCreator::CreateStorage called, size={}", size);

        if size <= 0 {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_UNKNOWN);
            return;
        }

        let storage = Arc::new(IStorage::new_with_system(
            creator.system,
            vec![0u8; size as usize],
        ));
        Self::push_interface_response(ctx, storage);
    }

    fn create_transfer_memory_storage_handler(
        _this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let mut rp = RequestParser::new(ctx);
        let _is_writable = rp.pop_bool();
        let size = rp.pop_i64();

        log::warn!("(STUBBED) CreateTransferMemoryStorage called, size={}", size);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_UNKNOWN);
    }
}

impl SessionRequestHandler for ILibraryAppletCreator {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for ILibraryAppletCreator {
    fn get_service_name(&self) -> &str {
        "am::ILibraryAppletCreator"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
