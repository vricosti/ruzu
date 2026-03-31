// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/window_controller.h
//! Port of zuyu/src/core/hle/service/am/service/window_controller.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::am::applet::Applet;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IWindowController:
/// - 0: CreateWindow (unimplemented)
/// - 1: GetAppletResourceUserId
/// - 2: GetAppletResourceUserIdOfCallerApplet
/// - 10: AcquireForegroundRights
/// - 11: ReleaseForegroundRights
/// - 12: RejectToChangeIntoBackground
/// - 20: SetAppletWindowVisibility
/// - 21: SetAppletGpuTimeSlice
pub struct IWindowController {
    /// Matches upstream `std::shared_ptr<Applet> m_applet`.
    applet: Arc<Mutex<Applet>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IWindowController {
    pub fn new(applet: Arc<Mutex<Applet>>) -> Self {
        let handlers = build_handler_map(&[
            (
                1,
                Some(Self::get_applet_resource_user_id_handler),
                "GetAppletResourceUserId",
            ),
            (
                2,
                Some(Self::get_applet_resource_user_id_of_caller_applet_handler),
                "GetAppletResourceUserIdOfCallerApplet",
            ),
            (
                10,
                Some(Self::acquire_foreground_rights_handler),
                "AcquireForegroundRights",
            ),
            (
                11,
                Some(Self::release_foreground_rights_handler),
                "ReleaseForegroundRights",
            ),
            (
                12,
                Some(Self::reject_to_change_into_background_handler),
                "RejectToChangeIntoBackground",
            ),
            (
                20,
                Some(Self::set_applet_window_visibility_handler),
                "SetAppletWindowVisibility",
            ),
            (
                21,
                Some(Self::set_applet_gpu_time_slice_handler),
                "SetAppletGpuTimeSlice",
            ),
        ]);
        Self {
            applet,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of IWindowController::GetAppletResourceUserId
    pub fn get_applet_resource_user_id(&self) -> u64 {
        log::info!("GetAppletResourceUserId called");
        self.applet.lock().unwrap().aruid.pid
    }

    /// Port of IWindowController::GetAppletResourceUserIdOfCallerApplet
    pub fn get_applet_resource_user_id_of_caller_applet(&self) -> u64 {
        log::info!("GetAppletResourceUserIdOfCallerApplet called");
        // Caller applet ownership is still missing in the Rust port.
        0
    }

    /// Port of IWindowController::AcquireForegroundRights
    pub fn acquire_foreground_rights(&self) {
        log::info!("AcquireForegroundRights called");
    }

    /// Port of IWindowController::ReleaseForegroundRights
    pub fn release_foreground_rights(&self) {
        log::info!("ReleaseForegroundRights called");
    }

    /// Port of IWindowController::RejectToChangeIntoBackground
    pub fn reject_to_change_into_background(&self) {
        log::info!("RejectToChangeIntoBackground called");
    }

    /// Port of IWindowController::SetAppletWindowVisibility
    pub fn set_applet_window_visibility(&self, visible: bool) {
        log::info!("SetAppletWindowVisibility called, visible={}", visible);
    }

    /// Port of IWindowController::SetAppletGpuTimeSlice
    pub fn set_applet_gpu_time_slice(&self, time_slice: i64) {
        log::warn!(
            "(STUBBED) SetAppletGpuTimeSlice called, time_slice={}",
            time_slice
        );
    }

    fn get_applet_resource_user_id_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IWindowController) };
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(service.get_applet_resource_user_id());
    }

    fn get_applet_resource_user_id_of_caller_applet_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IWindowController) };
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(service.get_applet_resource_user_id_of_caller_applet());
    }

    fn acquire_foreground_rights_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IWindowController) };
        service.acquire_foreground_rights();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn release_foreground_rights_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IWindowController) };
        service.release_foreground_rights();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn reject_to_change_into_background_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IWindowController) };
        service.reject_to_change_into_background();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_applet_window_visibility_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IWindowController) };
        let mut rp = RequestParser::new(ctx);
        service.set_applet_window_visibility(rp.pop_bool());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_applet_gpu_time_slice_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IWindowController) };
        let mut rp = RequestParser::new(ctx);
        service.set_applet_gpu_time_slice(rp.pop_i64());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IWindowController {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IWindowController {
    fn get_service_name(&self) -> &str {
        "am::IWindowController"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
