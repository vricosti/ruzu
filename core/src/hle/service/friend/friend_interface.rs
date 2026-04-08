// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/friend/friend_interface.h
//! Port of zuyu/src/core/hle/service/friend/friend_interface.cpp
//!
//! Friend interface service ("friend:a", "friend:m", "friend:s", "friend:u", "friend:v").

use std::collections::BTreeMap;
use std::sync::Arc;

use super::friend::Module;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for Friend interface
pub mod commands {
    pub const CREATE_FRIEND_SERVICE: u32 = 0;
    pub const CREATE_NOTIFICATION_SERVICE: u32 = 1;
    pub const CREATE_DAEMON_SUSPEND_SESSION_SERVICE: u32 = 2;
}

/// Friend interface service.
///
/// Corresponds to `Friend` (derived from `Module::Interface`) in upstream `friend_interface.h`.
pub struct Friend {
    system: crate::core::SystemRef,
    module: Arc<Module>,
    name: String,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl Friend {
    pub fn new(system: crate::core::SystemRef, module: Arc<Module>, name: &str) -> Self {
        let handlers = build_handler_map(&[
            (
                commands::CREATE_FRIEND_SERVICE,
                Some(Self::create_friend_service_handler),
                "CreateFriendService",
            ),
            (
                commands::CREATE_NOTIFICATION_SERVICE,
                Some(Self::create_notification_service_handler),
                "CreateNotificationService",
            ),
            (
                commands::CREATE_DAEMON_SUSPEND_SESSION_SERVICE,
                None,
                "CreateDaemonSuspendSessionService",
            ),
        ]);

        Self {
            system,
            module,
            name: name.to_string(),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// CreateFriendService (cmd 0)
    pub fn create_friend_service(&self) -> super::friend::IFriendService {
        log::debug!("Friend({})::create_friend_service called", self.name);
        super::friend::IFriendService::new()
    }

    /// CreateNotificationService (cmd 1)
    pub fn create_notification_service(&self, uuid: u128) -> super::friend::INotificationService {
        log::debug!(
            "Friend({})::create_notification_service called, uuid={:#x}",
            self.name,
            uuid
        );
        super::friend::INotificationService::new(uuid)
    }

    fn push_interface_response(
        ctx: &mut HLERequestContext,
        object: Arc<dyn SessionRequestHandler>,
    ) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(crate::hle::result::RESULT_SUCCESS);
        rb.push_ipc_interface(object);
    }

    fn cast(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    fn create_friend_service_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = Self::cast(this);
        log::debug!("Friend({})::CreateFriendService called", this.name);
        let service: Arc<dyn SessionRequestHandler> = Arc::new(this.create_friend_service());
        Self::push_interface_response(ctx, service);
    }

    fn create_notification_service_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = Self::cast(this);
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_u64() as u128 | ((rp.pop_u64() as u128) << 64);
        let service: Arc<dyn SessionRequestHandler> =
            Arc::new(this.create_notification_service(uuid));
        Self::push_interface_response(ctx, service);
    }
}

impl SessionRequestHandler for Friend {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        &self.name
    }
}

impl ServiceFramework for Friend {
    fn get_service_name(&self) -> &str {
        &self.name
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "friend:a", "friend:m", "friend:s", "friend:u", "friend:v" services.
///
/// Corresponds to `LoopProcess` in upstream `friend.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let module = Arc::new(super::friend::Module);

    let mut server_manager = ServerManager::new(system);

    for &name in &["friend:a", "friend:m", "friend:s", "friend:u", "friend:v"] {
        let m = module.clone();
        server_manager.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(Friend::new(system, m.clone(), name))
            }),
            64,
        );
    }

    ServerManager::run_server(server_manager);
}
