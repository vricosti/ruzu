// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/friend/friend.h
//! Port of zuyu/src/core/hle/service/friend/friend.cpp
//!
//! Friend Module, IFriendService, and INotificationService.

use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for IFriendService (selected implemented commands)
pub mod friend_service_commands {
    pub const GET_COMPLETION_EVENT: u32 = 0;
    pub const CANCEL: u32 = 1;
    pub const GET_FRIEND_LIST: u32 = 10101;
    pub const CHECK_FRIEND_LIST_AVAILABILITY: u32 = 10120;
    pub const GET_BLOCKED_USER_LIST_IDS: u32 = 10400;
    pub const CHECK_BLOCKED_USER_LIST_AVAILABILITY: u32 = 10420;
    pub const DECLARE_CLOSE_ONLINE_PLAY_SESSION: u32 = 10601;
    pub const UPDATE_USER_PRESENCE: u32 = 10610;
    pub const GET_PLAY_HISTORY_REGISTRATION_KEY: u32 = 10700;
    pub const GET_FRIEND_COUNT: u32 = 20100;
    pub const GET_NEWLY_FRIEND_COUNT: u32 = 20101;
    pub const GET_RECEIVED_FRIEND_REQUEST_COUNT: u32 = 20200;
    pub const GET_PLAY_HISTORY_STATISTICS: u32 = 20701;
    pub const GET_RECEIVED_FRIEND_INVITATION_COUNT_CACHE: u32 = 22010;
}

/// IPC command IDs for INotificationService
pub mod notification_commands {
    pub const GET_EVENT: u32 = 0;
    pub const CLEAR: u32 = 1;
    pub const POP: u32 = 2;
}

/// PresenceFilter enum. Upstream: `PresenceFilter` in `friend.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum PresenceFilter {
    None = 0,
    Online = 1,
    OnlinePlay = 2,
    OnlineOrOnlinePlay = 3,
}

/// SizedFriendFilter. Upstream: `SizedFriendFilter` in `friend.cpp`.
#[repr(C)]
pub struct SizedFriendFilter {
    pub presence: PresenceFilter,
    pub is_favorite: u8,
    pub same_app: u8,
    pub same_app_played: u8,
    pub arbitrary_app_played: u8,
    pub group_id: u64,
}

/// NotificationTypes enum. Upstream: `NotificationTypes` in `friend.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum NotificationTypes {
    HasUpdatedFriendsList = 0x65,
    HasReceivedFriendRequest = 0x1,
}

/// SizedNotificationInfo. Upstream: `SizedNotificationInfo` in `friend.cpp`.
#[repr(C)]
pub struct SizedNotificationInfo {
    pub notification_type: NotificationTypes,
    pub _padding: u32,
    pub account_id: u64,
}

/// Module for Friend service.
///
/// Corresponds to `Module` in upstream `friend.h`.
pub struct Module;

impl Module {
    pub fn new() -> Self {
        Self
    }
}

/// IFriendService.
pub struct IFriendService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    service_context: crate::hle::service::kernel_helpers::ServiceContext,
    completion_event_handle: u32,
}

impl IFriendService {
    pub fn new() -> Self {
        let mut service_context =
            crate::hle::service::kernel_helpers::ServiceContext::new("IFriendService".to_string());
        let completion_event_handle =
            service_context.create_event("IFriendService:CompletionEvent".to_string());
        Self {
            handlers: build_handler_map(&[
                (
                    friend_service_commands::GET_COMPLETION_EVENT,
                    Some(Self::get_completion_event_handler),
                    "GetCompletionEvent",
                ),
                (
                    friend_service_commands::GET_FRIEND_LIST,
                    Some(Self::get_friend_list_handler),
                    "GetFriendList",
                ),
                (
                    friend_service_commands::CHECK_FRIEND_LIST_AVAILABILITY,
                    Some(Self::check_friend_list_availability_handler),
                    "CheckFriendListAvailability",
                ),
                (
                    friend_service_commands::GET_BLOCKED_USER_LIST_IDS,
                    Some(Self::get_blocked_user_list_ids_handler),
                    "GetBlockedUserListIds",
                ),
                (
                    friend_service_commands::CHECK_BLOCKED_USER_LIST_AVAILABILITY,
                    Some(Self::check_blocked_user_list_availability_handler),
                    "CheckBlockedUserListAvailability",
                ),
                (
                    friend_service_commands::DECLARE_CLOSE_ONLINE_PLAY_SESSION,
                    Some(Self::declare_close_online_play_session_handler),
                    "DeclareCloseOnlinePlaySession",
                ),
                (
                    friend_service_commands::UPDATE_USER_PRESENCE,
                    Some(Self::update_user_presence_handler),
                    "UpdateUserPresence",
                ),
                (
                    friend_service_commands::GET_PLAY_HISTORY_REGISTRATION_KEY,
                    Some(Self::get_play_history_registration_key_handler),
                    "GetPlayHistoryRegistrationKey",
                ),
                (
                    friend_service_commands::GET_FRIEND_COUNT,
                    Some(Self::get_friend_count_handler),
                    "GetFriendCount",
                ),
                (
                    friend_service_commands::GET_NEWLY_FRIEND_COUNT,
                    Some(Self::get_newly_friend_count_handler),
                    "GetNewlyFriendCount",
                ),
                (
                    friend_service_commands::GET_RECEIVED_FRIEND_REQUEST_COUNT,
                    Some(Self::get_received_friend_request_count_handler),
                    "GetReceivedFriendRequestCount",
                ),
                (
                    friend_service_commands::GET_PLAY_HISTORY_STATISTICS,
                    Some(Self::get_play_history_statistics_handler),
                    "GetPlayHistoryStatistics",
                ),
                (
                    friend_service_commands::GET_RECEIVED_FRIEND_INVITATION_COUNT_CACHE,
                    Some(Self::get_received_friend_invitation_count_cache_handler),
                    "GetReceivedFriendInvitationCountCache",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
            service_context,
            completion_event_handle,
        }
    }

    fn cast(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    pub fn get_completion_event(&self) -> u32 {
        log::debug!("IFriendService::get_completion_event called");
        self.completion_event_handle
    }

    pub fn get_friend_list(&self, _friend_offset: u32, _uuid: u128, _pid: u64) -> u32 {
        log::warn!("(STUBBED) IFriendService::get_friend_list called");
        0 // friend count
    }

    pub fn check_friend_list_availability(&self, _uuid: u128) -> bool {
        log::warn!("(STUBBED) IFriendService::check_friend_list_availability called");
        true
    }

    pub fn get_blocked_user_list_ids(&self) -> u32 {
        log::warn!("(STUBBED) IFriendService::get_blocked_user_list_ids called");
        0
    }

    pub fn check_blocked_user_list_availability(&self, _uuid: u128) -> bool {
        log::warn!("(STUBBED) IFriendService::check_blocked_user_list_availability called");
        true
    }

    pub fn declare_close_online_play_session(&self) {
        log::warn!("(STUBBED) IFriendService::declare_close_online_play_session called");
    }

    pub fn update_user_presence(&self) {
        log::warn!("(STUBBED) IFriendService::update_user_presence called");
    }

    pub fn get_play_history_registration_key(&self, _local_play: bool, _uuid: u128) {
        log::warn!("(STUBBED) IFriendService::get_play_history_registration_key called");
    }

    pub fn get_friend_count(&self) -> u32 {
        log::debug!("(STUBBED) IFriendService::get_friend_count called");
        0
    }

    pub fn get_newly_friend_count(&self) -> u32 {
        log::debug!("(STUBBED) IFriendService::get_newly_friend_count called");
        0
    }

    pub fn get_received_friend_request_count(&self) -> u32 {
        log::debug!("(STUBBED) IFriendService::get_received_friend_request_count called");
        0
    }

    pub fn get_play_history_statistics(&self) {
        log::error!("(STUBBED) IFriendService::get_play_history_statistics called");
    }

    pub fn get_received_friend_invitation_count_cache(&self) -> u32 {
        log::debug!("(STUBBED) IFriendService::get_received_friend_invitation_count_cache called");
        0
    }

    fn get_completion_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = Self::cast(this);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(this.get_completion_event());
    }

    fn get_friend_list_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = Self::cast(this);
        let mut rp = RequestParser::new(ctx);
        let friend_offset = rp.pop_u32();
        let uuid = rp.pop_u64() as u128 | ((rp.pop_u64() as u128) << 64);
        rp.skip((core::mem::size_of::<SizedFriendFilter>() + 3) / 4);
        let pid = rp.pop_u64();

        let count = this.get_friend_list(friend_offset, uuid, pid);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn check_friend_list_availability_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = Self::cast(this);
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_u64() as u128 | ((rp.pop_u64() as u128) << 64);
        let available = this.check_friend_list_availability(uuid);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(available);
    }

    fn get_blocked_user_list_ids_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = Self::cast(this);
        let count = this.get_blocked_user_list_ids();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn check_blocked_user_list_availability_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = Self::cast(this);
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_u64() as u128 | ((rp.pop_u64() as u128) << 64);
        let available = this.check_blocked_user_list_availability(uuid);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(available);
    }

    fn declare_close_online_play_session_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::cast(this).declare_close_online_play_session();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn update_user_presence_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::cast(this).update_user_presence();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_play_history_registration_key_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = Self::cast(this);
        let mut rp = RequestParser::new(ctx);
        let local_play = rp.pop_bool();
        let uuid = rp.pop_u64() as u128 | ((rp.pop_u64() as u128) << 64);
        this.get_play_history_registration_key(local_play, uuid);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_friend_count_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let count = Self::cast(this).get_friend_count();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn get_newly_friend_count_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let count = Self::cast(this).get_newly_friend_count();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn get_received_friend_request_count_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let count = Self::cast(this).get_received_friend_request_count();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn get_play_history_statistics_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::cast(this).get_play_history_statistics();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_received_friend_invitation_count_cache_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let count = Self::cast(this).get_received_friend_invitation_count_cache();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }
}

impl SessionRequestHandler for IFriendService {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "IFriendService"
    }
}

impl ServiceFramework for IFriendService {
    fn get_service_name(&self) -> &str {
        "IFriendService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// INotificationService.
pub struct INotificationService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    uuid: u128,
    notifications: Mutex<VecDeque<SizedNotificationInfo>>,
    states: Mutex<NotificationStates>,
    service_context: crate::hle::service::kernel_helpers::ServiceContext,
    notification_event_handle: u32,
}

struct NotificationStates {
    has_updated_friends: bool,
    has_received_friend_request: bool,
}

impl INotificationService {
    pub fn new(uuid: u128) -> Self {
        let mut service_context = crate::hle::service::kernel_helpers::ServiceContext::new(
            "INotificationService".to_string(),
        );
        let notification_event_handle =
            service_context.create_event("INotificationService:NotifyEvent".to_string());
        Self {
            handlers: build_handler_map(&[
                (
                    notification_commands::GET_EVENT,
                    Some(Self::get_event_handler),
                    "GetEvent",
                ),
                (notification_commands::CLEAR, Some(Self::clear_handler), "Clear"),
                (notification_commands::POP, Some(Self::pop_handler), "Pop"),
            ]),
            handlers_tipc: BTreeMap::new(),
            uuid,
            notifications: Mutex::new(VecDeque::new()),
            states: Mutex::new(NotificationStates {
                has_updated_friends: false,
                has_received_friend_request: false,
            }),
            service_context,
            notification_event_handle,
        }
    }

    pub fn get_event(&self) -> u32 {
        log::debug!("INotificationService::get_event called");
        self.notification_event_handle
    }

    pub fn clear(&self) {
        log::debug!("INotificationService::clear called");
        self.notifications.lock().unwrap().clear();
        let mut states = self.states.lock().unwrap();
        states.has_updated_friends = false;
        states.has_received_friend_request = false;
    }

    pub fn pop(&self) -> Option<SizedNotificationInfo> {
        log::debug!("INotificationService::pop called");
        let notification = self.notifications.lock().unwrap().pop_front()?;
        let mut states = self.states.lock().unwrap();

        match notification.notification_type {
            NotificationTypes::HasUpdatedFriendsList => {
                states.has_updated_friends = false;
            }
            NotificationTypes::HasReceivedFriendRequest => {
                states.has_received_friend_request = false;
            }
        }

        Some(notification)
    }

    fn get_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(this.get_event());
    }

    fn clear_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        this.clear();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn pop_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let notification = this.pop();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        if let Some(_info) = notification {
            // Buffer return not wired yet; keep success parity and state update.
        }
    }
}

impl SessionRequestHandler for INotificationService {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "INotificationService"
    }
}

impl ServiceFramework for INotificationService {
    fn get_service_name(&self) -> &str {
        "INotificationService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
