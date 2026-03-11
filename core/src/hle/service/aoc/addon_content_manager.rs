// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/aoc/addon_content_manager.h
//! Port of zuyu/src/core/hle/service/aoc/addon_content_manager.cpp
//!
//! IAddOnContentManager service ("aoc:u").

use std::sync::Arc;

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
}

impl IAddOnContentManager {
    pub fn new() -> Self {
        Self {
            add_on_content: Vec::new(),
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
}

/// Registers "aoc:u" service.
///
/// Corresponds to `LoopProcess` in upstream `addon_content_manager.cpp`.
pub fn loop_process() {
    // TODO: register "aoc:u" -> IAddOnContentManager with ServerManager
}
