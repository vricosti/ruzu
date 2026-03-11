// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/friend/friend.h
//! Port of zuyu/src/core/hle/service/friend/friend.cpp
//!
//! Friend Module, IFriendService, and INotificationService.

use std::collections::VecDeque;

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
    // TODO: service_context, completion_event
}

impl IFriendService {
    pub fn new() -> Self {
        Self {}
    }

    pub fn get_completion_event(&self) {
        log::debug!("IFriendService::get_completion_event called");
        // TODO: return event handle
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
}

/// INotificationService.
pub struct INotificationService {
    uuid: u128,
    notifications: VecDeque<SizedNotificationInfo>,
    states: NotificationStates,
    // TODO: service_context, notification_event
}

struct NotificationStates {
    has_updated_friends: bool,
    has_received_friend_request: bool,
}

impl INotificationService {
    pub fn new(uuid: u128) -> Self {
        Self {
            uuid,
            notifications: VecDeque::new(),
            states: NotificationStates {
                has_updated_friends: false,
                has_received_friend_request: false,
            },
        }
    }

    pub fn get_event(&self) {
        log::debug!("INotificationService::get_event called");
        // TODO: return event handle
    }

    pub fn clear(&mut self) {
        log::debug!("INotificationService::clear called");
        self.notifications.clear();
        self.states.has_updated_friends = false;
        self.states.has_received_friend_request = false;
    }

    pub fn pop(&mut self) -> Option<SizedNotificationInfo> {
        log::debug!("INotificationService::pop called");
        let notification = self.notifications.pop_front()?;

        match notification.notification_type {
            NotificationTypes::HasUpdatedFriendsList => {
                self.states.has_updated_friends = false;
            }
            NotificationTypes::HasReceivedFriendRequest => {
                self.states.has_received_friend_request = false;
            }
        }

        Some(notification)
    }
}
