// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/library_applet_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/library_applet_accessor.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for ILibraryAppletAccessor:
/// - 0: GetAppletStateChangedEvent
/// - 1: IsCompleted
/// - 10: Start
/// - 20: RequestExit
/// - 25: Terminate
/// - 30: GetResult
/// - 50: SetOutOfFocusApplicationSuspendingEnabled (unimplemented)
/// - 60: PresetLibraryAppletGpuTimeSliceZero
/// - 100: PushInData
/// - 101: PopOutData
/// - 102: PushExtraStorage (unimplemented)
/// - 103: PushInteractiveInData
/// - 104: PopInteractiveOutData
/// - 105: GetPopOutDataEvent
/// - 106: GetPopInteractiveOutDataEvent
/// - 110: NeedsToExitProcess (unimplemented)
/// - 120: GetLibraryAppletInfo (unimplemented)
/// - 150: RequestForAppletToGetForeground (unimplemented)
/// - 160: GetIndirectLayerConsumerHandle
pub struct ILibraryAppletAccessor {
    applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
    // TODO: AppletDataBroker reference
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ILibraryAppletAccessor {
    pub fn new(applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>) -> Self {
        let handlers = build_handler_map(&[
            (0, None, "GetAppletStateChangedEvent"),
            (1, None, "IsCompleted"),
            (10, None, "Start"),
            (20, None, "RequestExit"),
            (25, None, "Terminate"),
            (30, None, "GetResult"),
            (50, None, "SetOutOfFocusApplicationSuspendingEnabled"),
            (60, None, "PresetLibraryAppletGpuTimeSliceZero"),
            (100, None, "PushInData"),
            (101, None, "PopOutData"),
            (102, None, "PushExtraStorage"),
            (103, None, "PushInteractiveInData"),
            (104, None, "PopInteractiveOutData"),
            (105, None, "GetPopOutDataEvent"),
            (106, None, "GetPopInteractiveOutDataEvent"),
            (110, None, "NeedsToExitProcess"),
            (120, None, "GetLibraryAppletInfo"),
            (150, None, "RequestForAppletToGetForeground"),
            (160, None, "GetIndirectLayerConsumerHandle"),
        ]);
        Self {
            applet,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ILibraryAppletAccessor {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
}

impl ServiceFramework for ILibraryAppletAccessor {
    fn get_service_name(&self) -> &str {
        "am::ILibraryAppletAccessor"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
