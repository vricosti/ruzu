// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc_su.h
//! Port of zuyu/src/core/hle/service/acc/acc_su.cpp
//!
//! ACC_SU service ("acc:su").

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for ACC_SU
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
    pub const BEGIN_USER_REGISTRATION: u32 = 200;
    pub const COMPLETE_USER_REGISTRATION: u32 = 201;
    pub const CANCEL_USER_REGISTRATION: u32 = 202;
    pub const DELETE_USER: u32 = 203;
    pub const SET_USER_POSITION: u32 = 204;
    pub const GET_PROFILE_EDITOR: u32 = 205;
    pub const COMPLETE_USER_REGISTRATION_FORCIBLY: u32 = 206;
    pub const CREATE_FLOATING_REGISTRATION_REQUEST: u32 = 210;
    pub const CREATE_PROCEDURE_TO_REGISTER_USER: u32 = 211;
    pub const RESUME_PROCEDURE_TO_REGISTER_USER: u32 = 212;
    pub const AUTHENTICATE_SERVICE_ASYNC: u32 = 230;
    pub const GET_BAAS_ACCOUNT_ADMINISTRATOR: u32 = 250;
    pub const PROXY_PROCEDURE_FOR_GUEST_LOGIN: u32 = 290;
    pub const PROXY_PROCEDURE_FOR_FLOATING_REGISTRATION: u32 = 291;
    pub const SUSPEND_BACKGROUND_DAEMON: u32 = 299;
    pub const SET_USER_UNQUALIFIED_FOR_DEBUG: u32 = 900;
    pub const UNSET_USER_UNQUALIFIED_FOR_DEBUG: u32 = 901;
    pub const LIST_USERS_UNQUALIFIED_FOR_DEBUG: u32 = 902;
    pub const REFRESH_FIRMWARE_SETTINGS_FOR_DEBUG: u32 = 910;
    pub const DEBUG_INVALIDATE_TOKEN_CACHE: u32 = 997;
    pub const DEBUG_SET_USER_STATE_CLOSE: u32 = 998;
    pub const DEBUG_SET_USER_STATE_OPEN: u32 = 999;
}

/// ACC_SU service.
///
/// Corresponds to `ACC_SU` in upstream `acc_su.h`.
pub struct AccSU {
    pub interface: super::acc::Interface,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl AccSU {
    /// Matches upstream `AccSU(shared_ptr<Module>, shared_ptr<ProfileManager>, System&)`.
    pub fn new(
        module: std::sync::Arc<super::acc::Module>,
        profile_manager: std::sync::Arc<std::sync::Mutex<super::profile_manager::ProfileManager>>,
        system: crate::core::SystemRef,
    ) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(AccSU::get_user_count_handler), "GetUserCount"),
            (1, Some(AccSU::get_user_existence_handler), "GetUserExistence"),
            (2, Some(AccSU::list_all_users_handler), "ListAllUsers"),
            (3, Some(AccSU::list_open_users_handler), "ListOpenUsers"),
            (4, Some(AccSU::get_last_opened_user_handler), "GetLastOpenedUser"),
            (5, Some(AccSU::get_profile_handler), "GetProfile"),
            (6, None, "GetProfileDigest"),
            (50, Some(AccSU::is_user_registration_request_permitted_handler), "IsUserRegistrationRequestPermitted"),
            (51, Some(AccSU::try_select_user_without_interaction_handler), "TrySelectUserWithoutInteraction"),
            (60, Some(AccSU::list_open_context_stored_users_handler), "ListOpenContextStoredUsers"),
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
            (140, Some(AccSU::list_qualified_users_handler), "ListQualifiedUsers"),
            (150, None, "AuthenticateApplicationAsync"),
            (151, None, "EnsureSignedDeviceIdentifierCacheForNintendoAccountAsync"),
            (152, None, "LoadSignedDeviceIdentifierCacheForNintendoAccount"),
            (190, None, "GetUserLastOpenedApplication"),
            (191, None, "ActivateOpenContextHolder"),
            (200, Some(AccSU::begin_user_registration_handler), "BeginUserRegistration"),
            (201, Some(AccSU::complete_user_registration_handler), "CompleteUserRegistration"),
            (202, None, "CancelUserRegistration"),
            (203, None, "DeleteUser"),
            (204, None, "SetUserPosition"),
            (205, Some(AccSU::get_profile_editor_handler), "GetProfileEditor"),
            (206, None, "CompleteUserRegistrationForcibly"),
            (210, None, "CreateFloatingRegistrationRequest"),
            (211, None, "CreateProcedureToRegisterUserWithNintendoAccount"),
            (212, None, "ResumeProcedureToRegisterUserWithNintendoAccount"),
            (230, None, "AuthenticateServiceAsync"),
            (250, None, "GetBaasAccountAdministrator"),
            (290, None, "ProxyProcedureForGuestLoginWithNintendoAccount"),
            (291, None, "ProxyProcedureForFloatingRegistrationWithNintendoAccount"),
            (299, None, "SuspendBackgroundDaemon"),
            (900, None, "SetUserUnqualifiedForDebug"),
            (901, None, "UnsetUserUnqualifiedForDebug"),
            (902, None, "ListUsersUnqualifiedForDebug"),
            (910, None, "RefreshFirmwareSettingsForDebug"),
            (997, None, "DebugInvalidateTokenCacheForUser"),
            (998, None, "DebugSetUserStateClose"),
            (999, None, "DebugSetUserStateOpen"),
        ]);

        Self {
            interface: super::acc::Interface::new(module, profile_manager, system, "acc:su"),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_user_count_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccSU) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, count) = svc.interface.get_user_count(&pm);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn get_user_existence_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccSU) };
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<u128>();
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, exists) = svc.interface.get_user_existence(&pm, uuid);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(exists);
    }

    fn list_all_users_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccSU) };
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
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccSU) };
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
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccSU) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, uuid) = svc.interface.get_last_opened_user(&pm);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&uuid);
    }

    fn get_profile_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccSU) };
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<u128>();
        let _rc = svc.interface.get_profile(uuid);
        // TODO: push IProfile interface object
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn is_user_registration_request_permitted_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccSU) };
        let (_rc, permitted) = svc.interface.is_user_registration_request_permitted();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(permitted);
    }

    fn try_select_user_without_interaction_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccSU) };
        let pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, uuid) = svc.interface.try_select_user_without_interaction(&pm);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&uuid);
    }

    fn list_open_context_stored_users_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccSU) };
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

    fn list_qualified_users_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccSU) };
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

    fn begin_user_registration_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &mut *(std::ptr::addr_of!(*this).cast::<AccSU>().cast_mut()) };
        let mut pm = svc.interface.profile_manager.lock().unwrap();
        let (_rc, uuid) = svc.interface.begin_user_registration(&mut pm);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&uuid);
    }

    fn complete_user_registration_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &mut *(std::ptr::addr_of!(*this).cast::<AccSU>().cast_mut()) };
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<u128>();
        let mut pm = svc.interface.profile_manager.lock().unwrap();
        let rc = svc.interface.complete_user_registration(&mut pm, uuid);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    fn get_profile_editor_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe { &*(this as *const dyn ServiceFramework as *const AccSU) };
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<u128>();
        let _rc = svc.interface.get_profile_editor(uuid);
        // TODO: push IProfileEditor interface object
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for AccSU {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "acc:su"
    }
}

impl ServiceFramework for AccSU {
    fn get_service_name(&self) -> &str {
        "acc:su"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
