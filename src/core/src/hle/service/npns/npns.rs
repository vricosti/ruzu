// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/npns/npns.cpp
//!
//! INpnsSystem ("npns:s") and INpnsUser ("npns:u") services.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for INpnsSystem ("npns:s").
///
/// Corresponds to the function table in `INpnsSystem` constructor (upstream npns.cpp).
pub mod system_commands {
    pub const LISTEN_ALL: u32 = 1;
    pub const LISTEN_TO: u32 = 2;
    pub const RECEIVE: u32 = 3;
    pub const RECEIVE_RAW: u32 = 4;
    pub const GET_RECEIVE_EVENT: u32 = 5;
    pub const LISTEN_UNDELIVERED: u32 = 6;
    pub const GET_STATE_CHANGE_EVENT: u32 = 7;
    pub const SUBSCRIBE_TOPIC: u32 = 11;
    pub const UNSUBSCRIBE_TOPIC: u32 = 12;
    pub const QUERY_IS_TOPIC_EXIST: u32 = 13;
    pub const CREATE_TOKEN: u32 = 21;
    pub const CREATE_TOKEN_WITH_APPLICATION_ID: u32 = 22;
    pub const DESTROY_TOKEN: u32 = 23;
    pub const DESTROY_TOKEN_WITH_APPLICATION_ID: u32 = 24;
    pub const QUERY_IS_TOKEN_VALID: u32 = 25;
    pub const LISTEN_TO_MY_APPLICATION_ID: u32 = 26;
    pub const DESTROY_TOKEN_ALL: u32 = 27;
    pub const UPLOAD_TOKEN_TO_BAAS: u32 = 31;
    pub const DESTROY_TOKEN_FOR_BAAS: u32 = 32;
    pub const CREATE_TOKEN_FOR_BAAS: u32 = 33;
    pub const SET_BAAS_DEVICE_ACCOUNT_ID_LIST: u32 = 34;
    pub const SUSPEND: u32 = 101;
    pub const RESUME: u32 = 102;
    pub const GET_STATE: u32 = 103;
    pub const GET_STATISTICS: u32 = 104;
    pub const GET_PLAY_REPORT_REQUEST_EVENT: u32 = 105;
    pub const GET_JID: u32 = 111;
    pub const CREATE_JID: u32 = 112;
    pub const DESTROY_JID: u32 = 113;
    pub const ATTACH_JID: u32 = 114;
    pub const DETACH_JID: u32 = 115;
    pub const CREATE_NOTIFICATION_RECEIVER: u32 = 120;
    pub const GET_STATE_WITH_HANDOVER: u32 = 151;
    pub const GET_STATE_CHANGE_EVENT_WITH_HANDOVER: u32 = 152;
    pub const GET_DROP_EVENT_WITH_HANDOVER: u32 = 153;
    pub const CREATE_TOKEN_ASYNC: u32 = 154;
    pub const CREATE_TOKEN_ASYNC_WITH_APPLICATION_ID: u32 = 155;
    pub const GET_REQUEST_CHANGE_STATE_CANCEL_EVENT: u32 = 161;
    pub const REQUEST_CHANGE_STATE_FORCE_TIMED_WITH_CANCEL_EVENT: u32 = 162;
    pub const REQUEST_CHANGE_STATE_FORCE_TIMED: u32 = 201;
    pub const REQUEST_CHANGE_STATE_FORCE_ASYNC: u32 = 202;
}

/// IPC command IDs for INpnsUser ("npns:u").
///
/// Corresponds to the function table in `INpnsUser` constructor (upstream npns.cpp).
pub mod user_commands {
    pub const LISTEN_ALL: u32 = 1;
    pub const LISTEN_TO: u32 = 2;
    pub const RECEIVE: u32 = 3;
    pub const RECEIVE_RAW: u32 = 4;
    pub const GET_RECEIVE_EVENT: u32 = 5;
    pub const GET_STATE_CHANGE_EVENT: u32 = 7;
    pub const CREATE_TOKEN: u32 = 21;
    pub const DESTROY_TOKEN: u32 = 23;
    pub const QUERY_IS_TOKEN_VALID: u32 = 25;
    pub const LISTEN_TO_MY_APPLICATION_ID: u32 = 26;
    pub const SUSPEND: u32 = 101;
    pub const RESUME: u32 = 102;
    pub const GET_STATE: u32 = 103;
    pub const GET_STATISTICS: u32 = 104;
    pub const GET_JID: u32 = 111;
    pub const CREATE_NOTIFICATION_RECEIVER: u32 = 120;
    pub const GET_STATE_WITH_HANDOVER: u32 = 151;
    pub const GET_STATE_CHANGE_EVENT_WITH_HANDOVER: u32 = 152;
    pub const GET_DROP_EVENT_WITH_HANDOVER: u32 = 153;
    pub const CREATE_TOKEN_ASYNC: u32 = 154;
}

/// INpnsSystem service ("npns:s").
///
/// Corresponds to `INpnsSystem` in upstream npns.cpp.
pub struct INpnsSystem {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    service_context: crate::hle::service::kernel_helpers::ServiceContext,
    /// Handle for the receive event. Returned by GetReceiveEvent.
    receive_event_handle: u32,
}

impl INpnsSystem {
    pub fn new() -> Self {
        let mut service_context =
            crate::hle::service::kernel_helpers::ServiceContext::new("npns:s".to_string());
        let receive_event_handle =
            service_context.create_event("npns:s:GetReceiveEvent".to_string());
        Self {
            handlers: build_handler_map(&[
                (system_commands::LISTEN_ALL, None, "ListenAll"),
                (
                    system_commands::LISTEN_TO,
                    Some(INpnsSystem::listen_to_handler),
                    "ListenTo",
                ),
                (system_commands::RECEIVE, None, "Receive"),
                (system_commands::RECEIVE_RAW, None, "ReceiveRaw"),
                (
                    system_commands::GET_RECEIVE_EVENT,
                    Some(INpnsSystem::get_receive_event_handler),
                    "GetReceiveEvent",
                ),
                (
                    system_commands::LISTEN_UNDELIVERED,
                    None,
                    "ListenUndelivered",
                ),
                (
                    system_commands::GET_STATE_CHANGE_EVENT,
                    None,
                    "GetStateChangeEVent",
                ),
                (system_commands::SUBSCRIBE_TOPIC, None, "SubscribeTopic"),
                (system_commands::UNSUBSCRIBE_TOPIC, None, "UnsubscribeTopic"),
                (
                    system_commands::QUERY_IS_TOPIC_EXIST,
                    None,
                    "QueryIsTopicExist",
                ),
                (system_commands::CREATE_TOKEN, None, "CreateToken"),
                (
                    system_commands::CREATE_TOKEN_WITH_APPLICATION_ID,
                    None,
                    "CreateTokenWithApplicationId",
                ),
                (system_commands::DESTROY_TOKEN, None, "DestroyToken"),
                (
                    system_commands::DESTROY_TOKEN_WITH_APPLICATION_ID,
                    None,
                    "DestroyTokenWithApplicationId",
                ),
                (
                    system_commands::QUERY_IS_TOKEN_VALID,
                    None,
                    "QueryIsTokenValid",
                ),
                (
                    system_commands::LISTEN_TO_MY_APPLICATION_ID,
                    None,
                    "ListenToMyApplicationId",
                ),
                (system_commands::DESTROY_TOKEN_ALL, None, "DestroyTokenAll"),
                (
                    system_commands::UPLOAD_TOKEN_TO_BAAS,
                    None,
                    "UploadTokenToBaaS",
                ),
                (
                    system_commands::DESTROY_TOKEN_FOR_BAAS,
                    None,
                    "DestroyTokenForBaaS",
                ),
                (
                    system_commands::CREATE_TOKEN_FOR_BAAS,
                    None,
                    "CreateTokenForBaaS",
                ),
                (
                    system_commands::SET_BAAS_DEVICE_ACCOUNT_ID_LIST,
                    None,
                    "SetBaaSDeviceAccountIdList",
                ),
                (system_commands::SUSPEND, None, "Suspend"),
                (system_commands::RESUME, None, "Resume"),
                (system_commands::GET_STATE, None, "GetState"),
                (system_commands::GET_STATISTICS, None, "GetStatistics"),
                (
                    system_commands::GET_PLAY_REPORT_REQUEST_EVENT,
                    None,
                    "GetPlayReportRequestEvent",
                ),
                (system_commands::GET_JID, None, "GetJid"),
                (system_commands::CREATE_JID, None, "CreateJid"),
                (system_commands::DESTROY_JID, None, "DestroyJid"),
                (system_commands::ATTACH_JID, None, "AttachJid"),
                (system_commands::DETACH_JID, None, "DetachJid"),
                (
                    system_commands::CREATE_NOTIFICATION_RECEIVER,
                    None,
                    "CreateNotificationReceiver",
                ),
                (
                    system_commands::GET_STATE_WITH_HANDOVER,
                    None,
                    "GetStateWithHandover",
                ),
                (
                    system_commands::GET_STATE_CHANGE_EVENT_WITH_HANDOVER,
                    None,
                    "GetStateChangeEventWithHandover",
                ),
                (
                    system_commands::GET_DROP_EVENT_WITH_HANDOVER,
                    None,
                    "GetDropEventWithHandover",
                ),
                (
                    system_commands::CREATE_TOKEN_ASYNC,
                    None,
                    "CreateTokenAsync",
                ),
                (
                    system_commands::CREATE_TOKEN_ASYNC_WITH_APPLICATION_ID,
                    None,
                    "CreateTokenAsyncWithApplicationId",
                ),
                (
                    system_commands::GET_REQUEST_CHANGE_STATE_CANCEL_EVENT,
                    None,
                    "GetRequestChangeStateCancelEvent",
                ),
                (
                    system_commands::REQUEST_CHANGE_STATE_FORCE_TIMED_WITH_CANCEL_EVENT,
                    None,
                    "RequestChangeStateForceTimedWithCancelEvent",
                ),
                (
                    system_commands::REQUEST_CHANGE_STATE_FORCE_TIMED,
                    None,
                    "RequestChangeStateForceTimed",
                ),
                (
                    system_commands::REQUEST_CHANGE_STATE_FORCE_ASYNC,
                    None,
                    "RequestChangeStateForceAsync",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
            service_context,
            receive_event_handle,
        }
    }

    /// ListenTo (cmd 2).
    ///
    /// Corresponds to `INpnsSystem::ListenTo` in upstream npns.cpp.
    pub fn listen_to(&self, program_id: u32) {
        log::warn!(
            "(STUBBED) INpnsSystem::listen_to called, program_id={}",
            program_id
        );
    }

    /// GetReceiveEvent (cmd 5).
    ///
    /// Corresponds to `INpnsSystem::GetReceiveEvent` in upstream npns.cpp.
    /// Returns the get_receive_event's readable event handle.
    pub fn get_receive_event(
        &self,
    ) -> Option<std::sync::Arc<crate::hle::service::os::event::Event>> {
        log::warn!("(STUBBED) INpnsSystem::get_receive_event called");
        self.service_context.get_event(self.receive_event_handle)
    }

    fn listen_to_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const INpnsSystem) };
        let mut rp = RequestParser::new(ctx);
        let program_id = rp.pop_u32();
        service.listen_to(program_id);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_receive_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const INpnsSystem) };
        let handle = service
            .get_receive_event()
            .and_then(|event| event.copy_handle(ctx))
            .unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }
}

impl SessionRequestHandler for INpnsSystem {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "npns:s"
    }
}

impl ServiceFramework for INpnsSystem {
    fn get_service_name(&self) -> &str {
        "npns:s"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// INpnsUser service ("npns:u").
///
/// Corresponds to `INpnsUser` in upstream npns.cpp.
/// All commands are nullptr (unimplemented) in upstream.
pub struct INpnsUser {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl INpnsUser {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (user_commands::LISTEN_ALL, None, "ListenAll"),
                (user_commands::LISTEN_TO, None, "ListenTo"),
                (user_commands::RECEIVE, None, "Receive"),
                (user_commands::RECEIVE_RAW, None, "ReceiveRaw"),
                (user_commands::GET_RECEIVE_EVENT, None, "GetReceiveEvent"),
                (
                    user_commands::GET_STATE_CHANGE_EVENT,
                    None,
                    "GetStateChangeEVent",
                ),
                (user_commands::CREATE_TOKEN, None, "CreateToken"),
                (user_commands::DESTROY_TOKEN, None, "DestroyToken"),
                (
                    user_commands::QUERY_IS_TOKEN_VALID,
                    None,
                    "QueryIsTokenValid",
                ),
                (
                    user_commands::LISTEN_TO_MY_APPLICATION_ID,
                    None,
                    "ListenToMyApplicationId",
                ),
                (user_commands::SUSPEND, None, "Suspend"),
                (user_commands::RESUME, None, "Resume"),
                (user_commands::GET_STATE, None, "GetState"),
                (user_commands::GET_STATISTICS, None, "GetStatistics"),
                (user_commands::GET_JID, None, "GetJid"),
                (
                    user_commands::CREATE_NOTIFICATION_RECEIVER,
                    None,
                    "CreateNotificationReceiver",
                ),
                (
                    user_commands::GET_STATE_WITH_HANDOVER,
                    None,
                    "GetStateWithHandover",
                ),
                (
                    user_commands::GET_STATE_CHANGE_EVENT_WITH_HANDOVER,
                    None,
                    "GetStateChangeEventWithHandover",
                ),
                (
                    user_commands::GET_DROP_EVENT_WITH_HANDOVER,
                    None,
                    "GetDropEventWithHandover",
                ),
                (user_commands::CREATE_TOKEN_ASYNC, None, "CreateTokenAsync"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for INpnsUser {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "npns:u"
    }
}

impl ServiceFramework for INpnsUser {
    fn get_service_name(&self) -> &str {
        "npns:u"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers "npns:s" and "npns:u" services.
///
/// Corresponds to `LoopProcess` in upstream npns.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "npns:s",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(INpnsSystem::new()) }),
            64,
        );
        server_manager.register_named_service(
            "npns:u",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(INpnsUser::new()) }),
            64,
        );
    }
    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn npns_tables_match_upstream_command_counts() {
        assert_eq!(INpnsSystem::new().handlers.len(), 41);
        assert_eq!(INpnsUser::new().handlers.len(), 20);
    }
}
