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
    pub(super) system: crate::core::SystemRef,
    pub(super) module: Arc<Module>,
    pub(super) name: String,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn concrete_friend_table_keeps_upstream_callback_partition() {
        let friend = Friend::new(
            crate::core::SystemRef::null(),
            Arc::new(Module::new()),
            "friend:u",
        );

        assert_eq!(friend.handlers.len(), 3);
        assert!(friend.handlers[&commands::CREATE_FRIEND_SERVICE]
            .handler_callback
            .is_some());
        assert!(friend.handlers[&commands::CREATE_NOTIFICATION_SERVICE]
            .handler_callback
            .is_some());
        assert!(friend.handlers[&commands::CREATE_DAEMON_SUSPEND_SESSION_SERVICE]
            .handler_callback
            .is_none());
    }
}
