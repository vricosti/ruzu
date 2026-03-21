// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/friend/friend_interface.h
//! Port of zuyu/src/core/hle/service/friend/friend_interface.cpp
//!
//! Friend interface service ("friend:a", "friend:m", "friend:s", "friend:u", "friend:v").

use std::sync::Arc;
use super::friend::Module;

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
}

impl Friend {
    pub fn new(system: crate::core::SystemRef, module: Arc<Module>, name: &str) -> Self {
        Self {
            system,
            module,
            name: name.to_string(),
        }
    }

    /// CreateFriendService (cmd 0)
    pub fn create_friend_service(&self) -> super::friend::IFriendService {
        log::debug!("Friend({})::create_friend_service called", self.name);
        super::friend::IFriendService::new()
    }

    /// CreateNotificationService (cmd 1)
    pub fn create_notification_service(
        &self,
        uuid: u128,
    ) -> super::friend::INotificationService {
        log::debug!(
            "Friend({})::create_notification_service called, uuid={:#x}",
            self.name,
            uuid
        );
        super::friend::INotificationService::new(uuid)
    }
}

/// Registers "friend:a", "friend:m", "friend:s", "friend:u", "friend:v" services.
///
/// Corresponds to `LoopProcess` in upstream `friend.cpp`.
pub fn loop_process() {
    // TODO: register services with ServerManager
}
