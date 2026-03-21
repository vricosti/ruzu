// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc_u0.h
//! Port of zuyu/src/core/hle/service/acc/acc_u0.cpp
//!
//! ACC_U0 service ("acc:u0").

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for ACC_U0
pub mod commands {
    pub const GET_USER_COUNT: u32 = 0;
    pub const GET_USER_EXISTENCE: u32 = 1;
    pub const LIST_ALL_USERS: u32 = 2;
    pub const LIST_OPEN_USERS: u32 = 3;
    pub const GET_LAST_OPENED_USER: u32 = 4;
    pub const GET_PROFILE: u32 = 5;
    pub const GET_PROFILE_DIGEST: u32 = 6;        // 3.0.0+
    pub const IS_USER_REGISTRATION_REQUEST_PERMITTED: u32 = 50;
    pub const TRY_SELECT_USER_WITHOUT_INTERACTION: u32 = 51;
    pub const LIST_OPEN_CONTEXT_STORED_USERS: u32 = 60; // 5.0.0 - 5.1.0
    pub const DEBUG_ACTIVATE_OPEN_CONTEXT_RETENTION: u32 = 99; // 6.0.0+
    pub const INITIALIZE_APPLICATION_INFO: u32 = 100;
    pub const GET_BAAS_ACCOUNT_MANAGER_FOR_APPLICATION: u32 = 101;
    pub const AUTHENTICATE_APPLICATION_ASYNC: u32 = 102;
    pub const CHECK_NETWORK_SERVICE_AVAILABILITY_ASYNC: u32 = 103; // 4.0.0+
    pub const STORE_SAVE_DATA_THUMBNAIL: u32 = 110;
    pub const CLEAR_SAVE_DATA_THUMBNAIL: u32 = 111;
    pub const CREATE_GUEST_LOGIN_REQUEST: u32 = 120;
    pub const LOAD_OPEN_CONTEXT: u32 = 130;        // 5.0.0+
    pub const LIST_OPEN_CONTEXT_STORED_USERS_V2: u32 = 131; // 6.0.0+
    pub const INITIALIZE_APPLICATION_INFO_RESTRICTED: u32 = 140; // 6.0.0+
    pub const LIST_QUALIFIED_USERS: u32 = 141;     // 6.0.0+
    pub const IS_USER_ACCOUNT_SWITCH_LOCKED: u32 = 150; // 6.0.0+
    pub const INITIALIZE_APPLICATION_INFO_V2: u32 = 160;
}

/// ACC_U0 service.
///
/// Corresponds to `ACC_U0` in upstream `acc_u0.h`.
pub struct AccU0 {
    pub interface: super::acc::Interface,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl AccU0 {
    /// Matches upstream `ACC_U0(shared_ptr<Module>, shared_ptr<ProfileManager>, System&)`.
    pub fn new(
        module: std::sync::Arc<super::acc::Module>,
        profile_manager: std::sync::Arc<std::sync::Mutex<super::profile_manager::ProfileManager>>,
        system: crate::core::SystemRef,
    ) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(AccU0::get_user_count_handler), "GetUserCount"),
            (1, Some(AccU0::get_user_existence_handler), "GetUserExistence"),
            (2, Some(AccU0::list_all_users_handler), "ListAllUsers"),
            (3, Some(AccU0::list_open_users_handler), "ListOpenUsers"),
            (4, Some(AccU0::get_last_opened_user_handler), "GetLastOpenedUser"),
            (5, Some(AccU0::get_profile_handler), "GetProfile"),
            (6, None, "GetProfileDigest"),
            (50, Some(AccU0::is_user_registration_request_permitted_handler), "IsUserRegistrationRequestPermitted"),
            (51, Some(AccU0::try_select_user_without_interaction_handler), "TrySelectUserWithoutInteraction"),
            (60, Some(AccU0::list_open_context_stored_users_handler), "ListOpenContextStoredUsers"),
            (99, None, "DebugActivateOpenContextRetention"),
            (100, Some(AccU0::initialize_application_info_handler), "InitializeApplicationInfo"),
            (101, None, "GetBaasAccountManagerForApplication"),
            (102, None, "AuthenticateApplicationAsync"),
            (103, None, "CheckNetworkServiceAvailabilityAsync"),
            (110, None, "StoreSaveDataThumbnail"),
            (111, None, "ClearSaveDataThumbnail"),
            (120, None, "CreateGuestLoginRequest"),
            (130, None, "LoadOpenContext"),
            (131, Some(AccU0::list_open_context_stored_users_handler), "ListOpenContextStoredUsers"),
            (140, Some(AccU0::initialize_application_info_restricted_handler), "InitializeApplicationInfoRestricted"),
            (141, Some(AccU0::list_qualified_users_handler), "ListQualifiedUsers"),
            (150, Some(AccU0::is_user_account_switch_locked_handler), "IsUserAccountSwitchLocked"),
            (160, Some(AccU0::initialize_application_info_v2_handler), "InitializeApplicationInfoV2"),
        ]);

        Self {
            interface: super::acc::Interface::new(module, profile_manager, system, "acc:u0"),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_user_count_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU0) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, count) = svc.interface.get_user_count(&pm);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn get_user_existence_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU0) };
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<u128>();
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, exists) = svc.interface.get_user_existence(&pm, uuid);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(exists);
    }

    fn list_all_users_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU0) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, users) = svc.interface.list_all_users(&pm);
        let user_bytes = unsafe {
            std::slice::from_raw_parts(
                users.as_ptr() as *const u8,
                std::mem::size_of_val(&users),
            )
        };
        ctx.write_buffer(user_bytes, 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn list_open_users_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU0) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, users) = svc.interface.list_open_users(&pm);
        let user_bytes = unsafe {
            std::slice::from_raw_parts(
                users.as_ptr() as *const u8,
                std::mem::size_of_val(&users),
            )
        };
        ctx.write_buffer(user_bytes, 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_last_opened_user_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU0) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, uuid) = svc.interface.get_last_opened_user(&pm);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&uuid);
    }

    fn get_profile_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU0) };
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<u128>();
        let (_rc, iprofile) = svc.interface.get_profile(uuid);

        let is_domain = ctx.get_manager().map_or(false, |m| m.lock().unwrap().is_domain());
        let move_handle = if is_domain { 0 } else {
            ctx.create_session_for_service(iprofile.clone()).unwrap_or(0)
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(iprofile);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    fn is_user_registration_request_permitted_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU0) };
        let (_rc, permitted) = svc.interface.is_user_registration_request_permitted();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(permitted);
    }

    fn try_select_user_without_interaction_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU0) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, uuid) = svc.interface.try_select_user_without_interaction(&pm);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&uuid);
    }

    fn list_open_context_stored_users_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU0) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, users) = svc.interface.list_open_context_stored_users(&pm);
        let user_bytes = unsafe {
            std::slice::from_raw_parts(
                users.as_ptr() as *const u8,
                std::mem::size_of_val(&users),
            )
        };
        ctx.write_buffer(user_bytes, 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn initialize_application_info_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &mut *(std::ptr::addr_of!(*this).cast::<AccU0>().cast_mut()) };
        let rc = svc.interface.initialize_application_info();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    fn initialize_application_info_restricted_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &mut *(std::ptr::addr_of!(*this).cast::<AccU0>().cast_mut()) };
        let rc = svc.interface.initialize_application_info_restricted();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    fn list_qualified_users_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU0) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, users) = svc.interface.list_qualified_users(&pm);
        let user_bytes = unsafe {
            std::slice::from_raw_parts(
                users.as_ptr() as *const u8,
                std::mem::size_of_val(&users),
            )
        };
        ctx.write_buffer(user_bytes, 0);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn is_user_account_switch_locked_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccU0) };
        let (_rc, locked) = svc.interface.is_user_account_switch_locked();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(locked);
    }

    fn initialize_application_info_v2_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &mut *(std::ptr::addr_of!(*this).cast::<AccU0>().cast_mut()) };
        let rc = svc.interface.initialize_application_info_v2();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }
}

impl SessionRequestHandler for AccU0 {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "acc:u0"
    }
}

impl ServiceFramework for AccU0 {
    fn get_service_name(&self) -> &str {
        "acc:u0"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
