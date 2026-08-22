// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/friend/friend.h
//! Port of zuyu/src/core/hle/service/friend/friend.cpp
//!
//! Friend Module, IFriendService, and INotificationService.

use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use common::uuid::UUID;

use crate::core::SystemRef;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::acc::errors::RESULT_NO_NOTIFICATIONS;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::kernel_helpers::ServiceContext;
use crate::hle::service::os::event::Event;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for IFriendService handlers implemented in Rust.
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

const _: [(); 0x10] = [(); core::mem::size_of::<SizedFriendFilter>()];

/// FriendsUserSetting. Upstream: `IFriendService::FriendsUserSetting` in `friend.cpp`.
#[derive(Clone, Copy)]
#[repr(C)]
struct FriendsUserSetting {
    uuid: UUID,
    presence_permission: u32,
    play_log_permission: u32,
    friend_request_reception: u64,
    friend_code: [u8; 0x20],
    friend_code_next_issuable_time: u64,
    unk_x48: [u8; 0x7B8],
}

impl FriendsUserSetting {
    fn new(uuid: UUID) -> Self {
        let mut friend_code = [0; 0x20];
        let default_friend_code = b"0000-0000-0000";
        friend_code[..default_friend_code.len()].copy_from_slice(default_friend_code);

        Self {
            uuid,
            presence_permission: 2,
            play_log_permission: 5,
            friend_request_reception: 1,
            friend_code,
            friend_code_next_issuable_time: 99_999_999_999,
            unk_x48: [0; 0x7B8],
        }
    }

    fn as_bytes(&self) -> &[u8] {
        // Every byte is initialized above and this repr(C) payload has no padding.
        unsafe {
            core::slice::from_raw_parts(
                (self as *const Self).cast::<u8>(),
                core::mem::size_of::<Self>(),
            )
        }
    }
}

const _: [(); 0x800] = [(); core::mem::size_of::<FriendsUserSetting>()];

/// NotificationTypes enum. Upstream: `NotificationTypes` in `friend.cpp`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum NotificationTypes {
    HasUpdatedFriendsList = 0x65,
    HasReceivedFriendRequest = 0x1,
}

/// SizedNotificationInfo. Upstream: `SizedNotificationInfo` in `friend.cpp`.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct SizedNotificationInfo {
    pub notification_type: NotificationTypes,
    pub _padding: u32,
    pub account_id: u64,
}

const _: [(); 0x10] = [(); core::mem::size_of::<SizedNotificationInfo>()];

/// Module for Friend service.
///
/// Corresponds to `Module` in upstream `friend.h`.
pub struct Module;

impl Module {
    pub fn new() -> Self {
        Self
    }
}

// These methods are the Rust counterpart of `Module::Interface` in upstream
// `friend.cpp`. The concrete `Friend` constructor and command table remain in
// `friend_interface.rs`, matching `friend_interface.cpp`.
impl super::friend_interface::Friend {
    /// CreateFriendService (cmd 0).
    pub fn create_friend_service(&self) -> IFriendService {
        log::debug!("Friend({})::create_friend_service called", self.name);
        IFriendService::new(self.system)
    }

    /// CreateNotificationService (cmd 1).
    pub fn create_notification_service(&self, uuid: UUID) -> INotificationService {
        log::debug!(
            "Friend({})::create_notification_service called, uuid=0x{}",
            self.name,
            uuid.raw_string()
        );
        INotificationService::new(self.system, uuid)
    }

    fn push_interface_response(
        ctx: &mut HLERequestContext,
        object: Arc<dyn SessionRequestHandler>,
    ) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(object);
    }

    fn cast(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    pub(super) fn create_friend_service_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = Self::cast(this);
        log::debug!("Friend({})::CreateFriendService called", this.name);
        let service: Arc<dyn SessionRequestHandler> = Arc::new(this.create_friend_service());
        Self::push_interface_response(ctx, service);
    }

    pub(super) fn create_notification_service_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = Self::cast(this);
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<UUID>();
        let service: Arc<dyn SessionRequestHandler> =
            Arc::new(this.create_notification_service(uuid));
        Self::push_interface_response(ctx, service);
    }
}

/// IFriendService.
pub struct IFriendService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    // Flattened owner corresponding to Eden's `ServiceFramework::system` reference.
    #[allow(dead_code)]
    system: SystemRef,
    service_context: ServiceContext,
    completion_event_handle: u32,
    completion_event: Arc<Event>,
}

impl IFriendService {
    pub fn new(system: SystemRef) -> Self {
        let mut service_context = ServiceContext::new("IFriendService".to_string());
        let completion_event_handle =
            service_context.create_event("IFriendService:CompletionEvent".to_string());
        let completion_event = service_context
            .get_event(completion_event_handle)
            .expect("IFriendService completion event must exist");
        Self {
            handlers: build_handler_map(&[
                (
                    friend_service_commands::GET_COMPLETION_EVENT,
                    Some(Self::get_completion_event_handler),
                    "GetCompletionEvent",
                ),
                (
                    friend_service_commands::CANCEL,
                    Some(Self::cancel_handler),
                    "Cancel",
                ),
                (10100, None, "GetFriendListIds"),
                (
                    friend_service_commands::GET_FRIEND_LIST,
                    Some(Self::get_friend_list_handler),
                    "GetFriendList",
                ),
                (10102, None, "UpdateFriendInfo"),
                (10110, None, "GetFriendProfileImage"),
                (10111, None, "GetFriendProfileImageWithImageSize"),
                (
                    friend_service_commands::CHECK_FRIEND_LIST_AVAILABILITY,
                    Some(Self::check_friend_list_availability_handler),
                    "CheckFriendListAvailability",
                ),
                (10121, None, "EnsureFriendListAvailable"),
                (10200, None, "SendFriendRequestForApplication"),
                (10211, None, "AddFacedFriendRequestForApplication"),
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
                (10421, None, "EnsureBlockedUserListAvailable"),
                (10500, None, "GetProfileList"),
                (10501, None, "GetProfileListV2"),
                (10600, None, "DeclareOpenOnlinePlaySession"),
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
                    10701,
                    None,
                    "GetPlayHistoryRegistrationKeyWithNetworkServiceAccountId",
                ),
                (10702, None, "AddPlayHistory"),
                (11000, None, "GetProfileImageUrl"),
                (11001, None, "GetProfileImageUrlV2"),
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
                (20102, None, "GetFriendDetailedInfo"),
                (20103, None, "SyncFriendList"),
                (
                    20104,
                    Some(Self::request_sync_friend_list_handler),
                    "RequestSyncFriendList",
                ),
                (
                    20105,
                    Some(Self::get_friend_list_for_viewer_handler),
                    "GetFriendListForViewerV1",
                ),
                (20106, None, "UpdateFriendInfoForViewerV1"),
                (20107, None, "GetFriendDetailedInfoV2"),
                (
                    20108,
                    Some(Self::get_friend_list_for_viewer_handler),
                    "GetFriendListForViewerV2",
                ),
                (20109, None, "UpdateFriendInfoForViewerV2"),
                (20110, None, "LoadFriendSettingV1"),
                (20111, None, "LoadFriendSettingV2"),
                (
                    friend_service_commands::GET_RECEIVED_FRIEND_REQUEST_COUNT,
                    Some(Self::get_received_friend_request_count_handler),
                    "GetReceivedFriendRequestCount",
                ),
                (20201, None, "GetFriendRequestListV1"),
                (20202, None, "GetFriendRequestListV2"),
                (20203, None, "GetFriendRequestReceivedNotificationCount"),
                (20300, None, "GetFriendCandidateList"),
                (20301, None, "GetNintendoNetworkIdInfo"),
                (20302, None, "GetSnsAccountLinkage"),
                (20303, None, "GetSnsAccountProfile"),
                (20304, None, "GetSnsAccountFriendList"),
                (20400, None, "GetBlockedUserListV1"),
                (20401, None, "SyncBlockedUserList"),
                (20402, None, "GetBlockedUserListV2"),
                (20500, None, "GetProfileExtraListV1"),
                (20501, None, "GetRelationship"),
                (20502, None, "GetProfileExtraListV2"),
                (
                    20600,
                    Some(Self::get_user_presence_view_handler),
                    "GetUserPresenceViewV1",
                ),
                (
                    20601,
                    Some(Self::get_user_presence_view_handler),
                    "GetUserPresenceViewV2",
                ),
                (20700, None, "GetPlayHistoryListV1"),
                (
                    friend_service_commands::GET_PLAY_HISTORY_STATISTICS,
                    Some(Self::get_play_history_statistics_handler),
                    "GetPlayHistoryStatistics",
                ),
                (20702, None, "GetPlayHistoryListV2"),
                (
                    20800,
                    Some(Self::load_user_setting_handler),
                    "LoadUserSettingV1",
                ),
                (20801, None, "SyncUserSetting"),
                (
                    20802,
                    Some(Self::load_user_setting_handler),
                    "LoadUserSettingV2",
                ),
                (
                    20900,
                    Some(Self::request_list_summary_overlay_notification_handler),
                    "RequestListSummaryOverlayNotification",
                ),
                (21000, None, "GetExternalApplicationCatalog"),
                (22000, None, "GetReceivedFriendInvitationListV1"),
                (22001, None, "GetReceivedFriendInvitationDetailedInfoV1"),
                (22002, None, "GetReceivedFriendInvitationListV2"),
                (22003, None, "GetReceivedFriendInvitationDetailedInfoV2"),
                (
                    friend_service_commands::GET_RECEIVED_FRIEND_INVITATION_COUNT_CACHE,
                    Some(Self::get_received_friend_invitation_count_cache_handler),
                    "GetReceivedFriendInvitationCountCache",
                ),
                (30100, None, "DropFriendNewlyFlags"),
                (30101, None, "DeleteFriend"),
                (30110, None, "DropFriendNewlyFlag"),
                (30120, None, "ChangeFriendFavoriteFlag"),
                (30121, None, "ChangeFriendOnlineNotificationFlag"),
                (30130, None, "SetFriendNote"),
                (30131, None, "RequestUploadPendingNote"),
                (30190, None, "RequestSyncLocalUpdates"),
                (30200, None, "SendFriendRequest"),
                (30201, None, "SendFriendRequestWithApplicationInfoV1"),
                (30202, None, "CancelFriendRequest"),
                (30203, None, "AcceptFriendRequest"),
                (30204, None, "RejectFriendRequest"),
                (30205, None, "ReadFriendRequest"),
                (30210, None, "GetFacedFriendRequestRegistrationKey"),
                (30211, None, "AddFacedFriendRequest"),
                (30212, None, "CancelFacedFriendRequest"),
                (30213, None, "GetFacedFriendRequestProfileImage"),
                (30214, None, "GetFacedFriendRequestProfileImageFromPath"),
                (
                    30215,
                    None,
                    "SendFriendRequestWithExternalApplicationCatalogId",
                ),
                (30216, None, "ResendFacedFriendRequest"),
                (30217, None, "SendFriendRequestWithNintendoNetworkIdInfo"),
                (30218, None, "SendFriendRequestWithApplicationInfoV2"),
                (30300, None, "GetSnsAccountLinkPageUrl"),
                (30301, None, "UnlinkSnsAccount"),
                (30400, None, "BlockUser"),
                (30401, None, "BlockUserWithApplicationInfoV1"),
                (30402, None, "UnblockUser"),
                (30403, None, "BlockUserWithApplicationInfoV2"),
                (30500, None, "GetProfileExtraFromFriendCodeV1"),
                (30501, None, "GetProfileExtraFromFriendCodeV2"),
                (30700, None, "DeletePlayHistory"),
                (30701, None, "AddPlayHistoryWithApplication"),
                (30810, None, "ChangePresencePermission"),
                (30811, None, "ChangeFriendRequestReception"),
                (30812, None, "ChangePlayLogPermission"),
                (30820, None, "IssueFriendCode"),
                (30830, None, "ClearPlayLog"),
                (30900, None, "SendFriendInvitationV1"),
                (30901, None, "SendFriendInvitationV2"),
                (30910, None, "ReadFriendInvitation"),
                (30911, None, "ReadAllFriendInvitations"),
                (31000, None, "OpenUser"),
                (40100, None, "DeleteFriendListCache"),
                (40400, None, "DeleteBlockedUserListCache"),
                (49900, None, "DeleteNetworkServiceAccountCache"),
            ]),
            handlers_tipc: BTreeMap::new(),
            system,
            service_context,
            completion_event_handle,
            completion_event,
        }
    }

    fn cast(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    pub fn get_completion_event(&self) -> Arc<Event> {
        log::debug!("IFriendService::get_completion_event called");
        self.completion_event.signal();
        Arc::clone(&self.completion_event)
    }

    pub fn cancel(&self) {
        log::debug!("(STUBBED) IFriendService::cancel called");
    }

    pub fn get_friend_list(&self, _friend_offset: u32, _uuid: UUID, _pid: u64) -> u32 {
        log::warn!("(STUBBED) IFriendService::get_friend_list called");
        0 // friend count
    }

    pub fn check_friend_list_availability(&self, _uuid: UUID) -> bool {
        log::warn!("(STUBBED) IFriendService::check_friend_list_availability called");
        true
    }

    pub fn get_blocked_user_list_ids(&self) -> u32 {
        log::warn!("(STUBBED) IFriendService::get_blocked_user_list_ids called");
        0
    }

    pub fn check_blocked_user_list_availability(&self, _uuid: UUID) -> bool {
        log::warn!("(STUBBED) IFriendService::check_blocked_user_list_availability called");
        true
    }

    pub fn declare_close_online_play_session(&self) {
        log::warn!("(STUBBED) IFriendService::declare_close_online_play_session called");
    }

    pub fn update_user_presence(&self) {
        log::warn!("(STUBBED) IFriendService::update_user_presence called");
    }

    pub fn get_play_history_registration_key(&self, _local_play: bool, _uuid: UUID) {
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

    pub fn request_sync_friend_list(&self) {
        log::debug!("(STUBBED) IFriendService::request_sync_friend_list called");
    }

    pub fn get_friend_list_for_viewer(&self) -> u32 {
        log::debug!("(STUBBED) IFriendService::get_friend_list_for_viewer called");
        0
    }

    pub fn get_received_friend_request_count(&self, _uuid: UUID) -> (u32, u32) {
        log::debug!("(STUBBED) IFriendService::get_received_friend_request_count called");
        (0, 0)
    }

    pub fn get_user_presence_view(&self, _uuid: UUID) -> [u8; 0xE0] {
        log::debug!("(STUBBED) IFriendService::get_user_presence_view called");
        [0; 0xE0]
    }

    pub fn get_play_history_statistics(&self) {
        log::error!("(STUBBED) IFriendService::get_play_history_statistics called");
    }

    fn load_user_setting(&self, uuid: UUID) -> FriendsUserSetting {
        log::warn!("(STUBBED) IFriendService::load_user_setting called");
        FriendsUserSetting::new(uuid)
    }

    pub fn request_list_summary_overlay_notification(&self) {
        log::info!("(STUBBED) IFriendService::request_list_summary_overlay_notification called");
    }

    pub fn get_received_friend_invitation_count_cache(&self) -> u32 {
        log::debug!("(STUBBED) IFriendService::get_received_friend_invitation_count_cache called");
        0
    }

    fn get_completion_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = Self::cast(this);
        let object_id = this.get_completion_event().copy_object_id(ctx).unwrap_or(0);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_object_id(object_id);
    }

    fn cancel_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::cast(this).cancel();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_friend_list_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = Self::cast(this);
        let mut rp = RequestParser::new(ctx);
        let friend_offset = rp.pop_u32();
        let uuid = rp.pop_raw::<UUID>();
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
        let uuid = rp.pop_raw::<UUID>();
        let available = this.check_friend_list_availability(uuid);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(available);
    }

    fn get_blocked_user_list_ids_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
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
        let uuid = rp.pop_raw::<UUID>();
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
        let uuid = rp.pop_raw::<UUID>();
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

    fn get_newly_friend_count_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let count = Self::cast(this).get_newly_friend_count();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn request_sync_friend_list_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::cast(this).request_sync_friend_list();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_friend_list_for_viewer_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let count = Self::cast(this).get_friend_list_for_viewer();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
    }

    fn get_received_friend_request_count_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<UUID>();
        let (count, unknown) = Self::cast(this).get_received_friend_request_count(uuid);
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(count);
        rb.push_u32(unknown);
    }

    fn get_user_presence_view_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<UUID>();
        let presence = Self::cast(this).get_user_presence_view(uuid);
        ctx.write_buffer(&presence, 0);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_play_history_statistics_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::cast(this).get_play_history_statistics();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn load_user_setting_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let uuid = rp.pop_raw::<UUID>();
        let setting = Self::cast(this).load_user_setting(uuid);
        ctx.write_buffer(setting.as_bytes(), 0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn request_list_summary_overlay_notification_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        Self::cast(this).request_list_summary_overlay_notification();
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

impl Drop for IFriendService {
    fn drop(&mut self) {
        self.service_context
            .close_event(self.completion_event_handle);
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
    // Flattened owner corresponding to Eden's `ServiceFramework::system` reference.
    #[allow(dead_code)]
    system: SystemRef,
    // Eden retains the UUID but does not currently consume it after construction.
    #[allow(dead_code)]
    uuid: UUID,
    notifications: Mutex<VecDeque<SizedNotificationInfo>>,
    states: Mutex<NotificationStates>,
    service_context: ServiceContext,
    notification_event_handle: u32,
    notification_event: Arc<Event>,
}

struct NotificationStates {
    has_updated_friends: bool,
    has_received_friend_request: bool,
}

impl INotificationService {
    pub fn new(system: SystemRef, uuid: UUID) -> Self {
        let mut service_context = ServiceContext::new("INotificationService".to_string());
        let notification_event_handle =
            service_context.create_event("INotificationService:NotifyEvent".to_string());
        let notification_event = service_context
            .get_event(notification_event_handle)
            .expect("INotificationService notification event must exist");
        Self {
            handlers: build_handler_map(&[
                (
                    notification_commands::GET_EVENT,
                    Some(Self::get_event_handler),
                    "GetEvent",
                ),
                (
                    notification_commands::CLEAR,
                    Some(Self::clear_handler),
                    "Clear",
                ),
                (notification_commands::POP, Some(Self::pop_handler), "Pop"),
            ]),
            handlers_tipc: BTreeMap::new(),
            system,
            uuid,
            notifications: Mutex::new(VecDeque::new()),
            states: Mutex::new(NotificationStates {
                has_updated_friends: false,
                has_received_friend_request: false,
            }),
            service_context,
            notification_event_handle,
            notification_event,
        }
    }

    pub fn get_event(&self) -> Arc<Event> {
        log::debug!("INotificationService::get_event called");
        Arc::clone(&self.notification_event)
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
        let object_id = this.get_event().copy_object_id(ctx).unwrap_or(0);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_object_id(object_id);
    }

    fn clear_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        this.clear();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn pop_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        if let Some(notification) = this.pop() {
            let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_raw(&notification);
        } else {
            log::error!("No notifications in queue!");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_NO_NOTIFICATIONS);
        }
    }
}

impl Drop for INotificationService {
    fn drop(&mut self) {
        self.service_context
            .close_event(self.notification_event_handle);
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

/// `nd:app`, matching upstream `IServiceForApplication` in `friend.cpp`.
pub struct IServiceForApplication {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IServiceForApplication {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, None, "GetReceivableNeighborInfoCountMax"),
                (10, None, "IsNeighborDetectionEnabled"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IServiceForApplication {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "nd:app"
    }
}

impl ServiceFramework for IServiceForApplication {
    fn get_service_name(&self) -> &str {
        "nd:app"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// `nd:sys`, matching upstream `IServiceForSystem` in `friend.cpp`.
pub struct IServiceForSystem {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IServiceForSystem {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, None, "GetReceivableNeighborInfoCountMax"),
                (10, None, "IsNeighborDetectionEnabled"),
                (200, None, "SetSystemData"),
                (201, None, "ClearSystemData"),
                (203, None, "GetReceivableNeighborInfoCountForSystem"),
                (204, None, "ReceiveNeighborInfoForSystem"),
                (205, None, "SetSender"),
                (206, None, "GetSender"),
                (207, None, "CreateScannerForSystem"),
                (208, None, "CreateReceiveEventHolderForSystem"),
                (223, None, "EnableNeighborDetection"),
                (224, None, "DisableNeighborDetection"),
                (226, None, "EnablePowerSave"),
                (227, None, "DisablePowerSave"),
                (228, None, "IsPowerSaveEnabled"),
                (232, None, "ClearBlockedUsers"),
                (233, None, "GetBlockedUserCount"),
                (234, None, "BlockUserByLocalUserId"),
                (235, None, "BlockUserByNetworkUserId"),
                (236, None, "UnblockUserByLocalUserId"),
                (237, None, "UnblockUserByNetworkUserId"),
                (240, None, "DeleteApplication"),
                (250, None, "InitializeApplicationInfo"),
                (260, None, "CreateAccountSystemSaveDataAccessSuppressor"),
                (300, None, "AddReceivedNeighborInfoForSystemForDebug"),
                (301, None, "GetSendDataForDebug"),
                (302, None, "ClearReceiveCounterForDebug"),
                (303, None, "GetNextReceiveCounterForDebug"),
                (304, None, "ListBlockedUsersForDebug"),
                (305, None, "RefreshSendDataIdForDebug"),
                (306, None, "ReloadFwdbgSettingsForDebug"),
                (307, None, "EnableApplicationForDebug"),
                (308, None, "GetNextReceiveCountersForDebug"),
                (309, None, "ListApplicationInfoForDebug"),
                (310, None, "SetApplicationDataForDebug"),
                (400, None, "GetNetworkUserId"),
                (401, None, "DeleteNetworkUserId"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IServiceForSystem {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "nd:sys"
    }
}

impl ServiceFramework for IServiceForSystem {
    fn get_service_name(&self) -> &str {
        "nd:sys"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// Registers the friend and neighbor-detection services.
///
/// Corresponds to `LoopProcess` in upstream `friend.cpp` and deliberately
/// lives in this file rather than `friend_interface.rs`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;

    let module = Arc::new(Module);
    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "nd:app",
            Box::new(|| -> SessionRequestHandlerPtr { Arc::new(IServiceForApplication::new()) }),
            64,
        );
        server_manager.register_named_service(
            "nd:sys",
            Box::new(|| -> SessionRequestHandlerPtr { Arc::new(IServiceForSystem::new()) }),
            64,
        );

        for &name in &["friend:a", "friend:m", "friend:s", "friend:u", "friend:v"] {
            let module = Arc::clone(&module);
            server_manager.register_named_service(
                name,
                Box::new(move || -> SessionRequestHandlerPtr {
                    Arc::new(super::friend_interface::Friend::new(
                        system,
                        Arc::clone(&module),
                        name,
                    ))
                }),
                64,
            );
        }
    }

    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod a41_tests {
    use super::*;

    const IMPLEMENTED_FRIEND_COMMANDS: [u32; 22] = [
        0, 1, 10101, 10120, 10400, 10420, 10601, 10610, 10700, 20100, 20101, 20104, 20105, 20108,
        20200, 20600, 20601, 20701, 20800, 20802, 20900, 22010,
    ];

    #[test]
    fn friend_command_table_matches_upstream_partition() {
        let service = IFriendService::new(SystemRef::null());
        let implemented: Vec<u32> = service
            .handlers
            .iter()
            .filter_map(|(&id, info)| info.handler_callback.map(|_| id))
            .collect();

        assert_eq!(service.handlers.len(), 112);
        assert_eq!(implemented, IMPLEMENTED_FRIEND_COMMANDS);
        assert_eq!(service.handlers[&20105].name, "GetFriendListForViewerV1");
        assert_eq!(service.handlers[&20108].name, "GetFriendListForViewerV2");
        assert_eq!(service.handlers[&20800].name, "LoadUserSettingV1");
        assert_eq!(service.handlers[&20802].name, "LoadUserSettingV2");
    }

    #[test]
    fn completion_event_is_signaled_and_released_with_service() {
        let service = IFriendService::new(SystemRef::null());
        let event = Arc::clone(&service.completion_event);
        assert!(!event.is_signaled());

        let handed_out = service.get_completion_event();
        assert!(Arc::ptr_eq(&event, &handed_out));
        assert!(event.is_signaled());
        drop(handed_out);

        assert_eq!(Arc::strong_count(&event), 3);
        drop(service);
        assert_eq!(Arc::strong_count(&event), 1);
    }

    #[test]
    fn notification_service_retains_uuid_and_releases_event() {
        let uuid = UUID::from_bytes([0x5A; 16]);
        let service = INotificationService::new(SystemRef::null(), uuid);
        let event = Arc::clone(&service.notification_event);

        assert_eq!(service.uuid, uuid);
        assert_eq!(Arc::strong_count(&event), 3);
        drop(service);
        assert_eq!(Arc::strong_count(&event), 1);
    }

    #[test]
    fn user_setting_payload_matches_upstream_layout_and_defaults() {
        let uuid = UUID::from_bytes(core::array::from_fn(|index| index as u8));
        let setting = FriendsUserSetting::new(uuid);
        let bytes = setting.as_bytes();

        assert_eq!(bytes.len(), 0x800);
        assert_eq!(&bytes[0x00..0x10], &uuid.uuid);
        assert_eq!(u32::from_le_bytes(bytes[0x10..0x14].try_into().unwrap()), 2);
        assert_eq!(u32::from_le_bytes(bytes[0x14..0x18].try_into().unwrap()), 5);
        assert_eq!(u64::from_le_bytes(bytes[0x18..0x20].try_into().unwrap()), 1);
        assert_eq!(&bytes[0x20..0x2E], b"0000-0000-0000");
        assert_eq!(bytes[0x2E], 0);
        assert!(bytes[0x2F..0x40].iter().all(|&byte| byte == 0));
        assert_eq!(
            u64::from_le_bytes(bytes[0x40..0x48].try_into().unwrap()),
            99_999_999_999
        );
        assert!(bytes[0x48..].iter().all(|&byte| byte == 0));
    }

    #[test]
    fn friend_forwards_system_and_retains_module_owner() {
        let system = crate::core::System::new();
        let system_ref = SystemRef::from_ref(&system);
        let module = Arc::new(Module::new());
        let friend = super::super::friend_interface::Friend::new(
            system_ref,
            Arc::clone(&module),
            "friend:u",
        );

        assert_eq!(Arc::strong_count(&module), 2);
        assert!(!friend.create_friend_service().system.is_null());
        assert!(!friend
            .create_notification_service(UUID::new())
            .system
            .is_null());

        drop(friend);
        assert_eq!(Arc::strong_count(&module), 1);
    }

    #[test]
    fn neighbor_detection_tables_match_upstream() {
        assert_eq!(IServiceForApplication::new().handlers().len(), 2);
        assert_eq!(IServiceForSystem::new().handlers().len(), 37);
    }
}
