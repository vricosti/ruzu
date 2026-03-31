// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc_u1.h
//! Port of zuyu/src/core/hle/service/acc/acc_u1.cpp
//!
//! ACC_U1 service ("acc:u1").

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for ACC_U1
pub mod commands {
    pub const GET_USER_COUNT: u32 = 0;
    pub const GET_USER_EXISTENCE: u32 = 1;
    pub const LIST_ALL_USERS: u32 = 2;
    pub const LIST_OPEN_USERS: u32 = 3;
    pub const GET_LAST_OPENED_USER: u32 = 4;
    pub const GET_PROFILE: u32 = 5;
    pub const GET_PROFILE_DIGEST: u32 = 6;
    pub const IS_USER_REGISTRATION_REQUEST_PERMITTED: u32 = 50;
    pub const TRY_SELECT_USER_WITHOUT_INTERACTION: u32 = 51;
    pub const LIST_OPEN_CONTEXT_STORED_USERS: u32 = 60;
    pub const DEBUG_ACTIVATE_OPEN_CONTEXT_RETENTION: u32 = 99;
    pub const GET_USER_REGISTRATION_NOTIFIER: u32 = 100;
    pub const GET_USER_STATE_CHANGE_NOTIFIER: u32 = 101;
    pub const GET_BAAS_ACCOUNT_MANAGER_FOR_SYSTEM_SERVICE: u32 = 102;
    pub const GET_BAAS_USER_AVAILABILITY_CHANGE_NOTIFIER: u32 = 103;
    pub const GET_PROFILE_UPDATE_NOTIFIER: u32 = 104;
    pub const CHECK_NETWORK_SERVICE_AVAILABILITY_ASYNC: u32 = 105;
    pub const GET_PROFILE_SYNC_NOTIFIER: u32 = 106;
    pub const STORE_SAVE_DATA_THUMBNAIL: u32 = 110;
    pub const CLEAR_SAVE_DATA_THUMBNAIL: u32 = 111;
    pub const LOAD_SAVE_DATA_THUMBNAIL: u32 = 112;
    pub const GET_SAVE_DATA_THUMBNAIL_EXISTENCE: u32 = 113;
    pub const LIST_OPEN_USERS_IN_APPLICATION: u32 = 120;
    pub const ACTIVATE_OPEN_CONTEXT_RETENTION: u32 = 130;
    pub const LIST_QUALIFIED_USERS: u32 = 140;
    pub const AUTHENTICATE_APPLICATION_ASYNC: u32 = 150;
    pub const ENSURE_SIGNED_DEVICE_IDENTIFIER_CACHE: u32 = 151;
    pub const LOAD_SIGNED_DEVICE_IDENTIFIER_CACHE: u32 = 152;
    pub const GET_USER_LAST_OPENED_APPLICATION: u32 = 190;
    pub const ACTIVATE_OPEN_CONTEXT_HOLDER: u32 = 191;
    pub const DEBUG_INVALIDATE_TOKEN_CACHE: u32 = 997;
    pub const DEBUG_SET_USER_STATE_CLOSE: u32 = 998;
    pub const DEBUG_SET_USER_STATE_OPEN: u32 = 999;
}

/// ACC_U1 service.
///
/// Corresponds to `ACC_U1` in upstream `acc_u1.h`.
pub struct AccU1 {
    pub interface: super::acc::Interface,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl AccU1 {
    /// Matches upstream `AccU1(shared_ptr<Module>, shared_ptr<ProfileManager>, System&)`.
    pub fn new(
        module: std::sync::Arc<super::acc::Module>,
        profile_manager: std::sync::Arc<std::sync::Mutex<super::profile_manager::ProfileManager>>,
        system: crate::core::SystemRef,
    ) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(AccU1::get_user_count_handler), "GetUserCount"),
            (
                1,
                Some(AccU1::get_user_existence_handler),
                "GetUserExistence",
            ),
            (2, Some(AccU1::list_all_users_handler), "ListAllUsers"),
            (3, Some(AccU1::list_open_users_handler), "ListOpenUsers"),
            (
                4,
                Some(AccU1::get_last_opened_user_handler),
                "GetLastOpenedUser",
            ),
            (5, Some(AccU1::get_profile_handler), "GetProfile"),
            (6, None, "GetProfileDigest"),
            (
                50,
                Some(AccU1::is_user_registration_request_permitted_handler),
                "IsUserRegistrationRequestPermitted",
            ),
            (
                51,
                Some(AccU1::try_select_user_without_interaction_handler),
                "TrySelectUserWithoutInteraction",
            ),
            (
                60,
                Some(AccU1::list_open_context_stored_users_handler),
                "ListOpenContextStoredUsers",
            ),
            (99, None, "DebugActivateOpenContextRetention"),
            (100, None, "GetUserRegistrationNotifier"),
            (101, None, "GetUserStateChangeNotifier"),
            (102, None, "GetBaasAccountManagerForSystemService"),
            (103, None, "GetBaasUserAvailabilityChangeNotifier"),
            (104, None, "GetProfileUpdateNotifier"),
            (105, None, "CheckNetworkServiceAvailabilityAsync"),
            (106, None, "GetProfileSyncNotifier"),
            (110, None, "StoreSaveDataThumbnail"),
            (111, None, "ClearSaveDataThumbnail"),
            (112, None, "LoadSaveDataThumbnail"),
            (113, None, "GetSaveDataThumbnailExistence"),
            (120, None, "ListOpenUsersInApplication"),
            (130, None, "ActivateOpenContextRetention"),
            (
                140,
                Some(AccU1::list_qualified_users_handler),
                "ListQualifiedUsers",
            ),
            (150, None, "AuthenticateApplicationAsync"),
            (
                151,
                None,
                "EnsureSignedDeviceIdentifierCacheForNintendoAccountAsync",
            ),
            (
                152,
                None,
                "LoadSignedDeviceIdentifierCacheForNintendoAccount",
            ),
            (190, None, "GetUserLastOpenedApplication"),
            (191, None, "ActivateOpenContextHolder"),
            (997, None, "DebugInvalidateTokenCacheForUser"),
            (998, None, "DebugSetUserStateClose"),
            (999, None, "DebugSetUserStateOpen"),
        ]);

        Self {
            interface: super::acc::Interface::new(module, profile_manager, system, "acc:u1"),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_user_count_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU1) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, count) = svc.interface.get_user_count(&pm);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn get_user_existence_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU1) };
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<u128>();
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, exists) = svc.interface.get_user_existence(&pm, uuid);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(exists);
    }

    fn list_all_users_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU1) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, users) = svc.interface.list_all_users(&pm);
        let user_bytes = unsafe {
            std::slice::from_raw_parts(users.as_ptr() as *const u8, std::mem::size_of_val(&users))
        };
        ctx.write_buffer(user_bytes, 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn list_open_users_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU1) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, users) = svc.interface.list_open_users(&pm);
        let user_bytes = unsafe {
            std::slice::from_raw_parts(users.as_ptr() as *const u8, std::mem::size_of_val(&users))
        };
        ctx.write_buffer(user_bytes, 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_last_opened_user_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU1) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, uuid) = svc.interface.get_last_opened_user(&pm);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&uuid);
    }

    fn get_profile_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU1) };
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<u128>();
        let (_rc, iprofile) = svc.interface.get_profile(uuid);

        let is_domain = ctx
            .get_manager()
            .map_or(false, |m| m.lock().unwrap().is_domain());
        let move_handle = if is_domain {
            0
        } else {
            ctx.create_session_for_service(iprofile.clone())
                .unwrap_or(0)
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(iprofile);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    fn is_user_registration_request_permitted_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU1) };
        let (_rc, permitted) = svc.interface.is_user_registration_request_permitted();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(permitted);
    }

    fn try_select_user_without_interaction_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU1) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, uuid) = svc.interface.try_select_user_without_interaction(&pm);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&uuid);
    }

    fn list_open_context_stored_users_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU1) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, users) = svc.interface.list_open_context_stored_users(&pm);
        let user_bytes = unsafe {
            std::slice::from_raw_parts(users.as_ptr() as *const u8, std::mem::size_of_val(&users))
        };
        ctx.write_buffer(user_bytes, 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn list_qualified_users_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU1) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, users) = svc.interface.list_qualified_users(&pm);
        let user_bytes = unsafe {
            std::slice::from_raw_parts(users.as_ptr() as *const u8, std::mem::size_of_val(&users))
        };
        ctx.write_buffer(user_bytes, 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for AccU1 {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "acc:u1"
    }
}

impl ServiceFramework for AccU1 {
    fn get_service_name(&self) -> &str {
        "acc:u1"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
