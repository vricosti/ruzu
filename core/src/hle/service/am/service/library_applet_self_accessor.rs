// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/library_applet_self_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/library_applet_self_accessor.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::ResultCode;
use crate::hle::service::am::am_types::{AppletId, AppletIdentityInfo, LibraryAppletMode};
use crate::hle::service::am::applet_data_broker::AppletDataBroker;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Library applet info.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct LibraryAppletInfo {
    pub applet_id: AppletId,
    pub library_applet_mode: LibraryAppletMode,
}
const _: () = assert!(core::mem::size_of::<LibraryAppletInfo>() == 0x8);

/// Error code.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct ErrorCode {
    pub category: u32,
    pub number: u32,
}
const _: () = assert!(core::mem::size_of::<ErrorCode>() == 0x8);

/// Error context.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ErrorContext {
    pub error_type: u8,
    pub _padding: [u8; 0x7],
    pub data: [u8; 0x1f4],
    pub result: u32,
}
const _: () = assert!(core::mem::size_of::<ErrorContext>() == 0x200);

impl Default for ErrorContext {
    fn default() -> Self {
        Self {
            error_type: 0,
            _padding: [0u8; 0x7],
            data: [0u8; 0x1f4],
            result: 0,
        }
    }
}

/// ILibraryAppletSelfAccessor service.
pub struct ILibraryAppletSelfAccessor {
    applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
    /// Matches upstream `const std::shared_ptr<AppletDataBroker> m_broker`.
    /// Obtained from `m_applet->caller_applet_broker`.
    broker: Option<Arc<AppletDataBroker>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ILibraryAppletSelfAccessor {
    pub fn new(applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>) -> Self {
        // Upstream: m_broker{m_applet->caller_applet_broker}
        let broker = applet.lock().unwrap().caller_applet_broker.clone();
        let handlers = build_handler_map(&[
            (0, None, "PopInData"),
            (1, None, "PushOutData"),
            (2, None, "PopInteractiveInData"),
            (3, None, "PushInteractiveOutData"),
            (5, None, "GetPopInDataEvent"),
            (6, None, "GetPopInteractiveInDataEvent"),
            (10, None, "ExitProcessAndReturn"),
            (11, None, "GetLibraryAppletInfo"),
            (12, None, "GetMainAppletIdentityInfo"),
            (13, None, "CanUseApplicationCore"),
            (14, None, "GetCallerAppletIdentityInfo"),
            (15, None, "GetMainAppletApplicationControlProperty"),
            (16, None, "GetMainAppletStorageId"),
            (17, None, "GetCallerAppletIdentityInfoStack"),
            (18, None, "GetNextReturnDestinationAppletIdentityInfo"),
            (19, None, "GetDesirableKeyboardLayout"),
            (20, None, "PopExtraStorage"),
            (25, None, "GetPopExtraStorageEvent"),
            (30, None, "UnpopInData"),
            (31, None, "UnpopExtraStorage"),
            (40, None, "GetIndirectLayerProducerHandle"),
            (50, None, "ReportVisibleError"),
            (51, None, "ReportVisibleErrorWithErrorContext"),
            (60, None, "GetMainAppletApplicationDesiredLanguage"),
            (70, None, "GetCurrentApplicationId"),
            (80, None, "RequestExitToSelf"),
            (90, None, "CreateApplicationAndPushAndRequestToLaunch"),
            (100, None, "CreateGameMovieTrimmer"),
            (101, None, "ReserveResourceForMovieOperation"),
            (102, None, "UnreserveResourceForMovieOperation"),
            (110, None, "GetMainAppletAvailableUsers"),
            (120, None, "GetLaunchStorageInfoForDebug"),
            (130, None, "GetGpuErrorDetectedSystemEvent"),
            (140, None, "SetApplicationMemoryReservation"),
            (150, None, "ShouldSetGpuTimeSliceManually"),
            (160, None, "Cmd160"),
        ]);
        Self {
            applet,
            broker,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ILibraryAppletSelfAccessor {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "am::ILibraryAppletSelfAccessor"
    }
}

impl ServiceFramework for ILibraryAppletSelfAccessor {
    fn get_service_name(&self) -> &str {
        "am::ILibraryAppletSelfAccessor"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
