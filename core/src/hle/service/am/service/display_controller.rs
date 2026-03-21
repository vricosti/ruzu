// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/display_controller.h
//! Port of zuyu/src/core/hle/service/am/service/display_controller.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IDisplayController:
/// - 0: GetLastForegroundCaptureImage (unimplemented)
/// - 1: UpdateLastForegroundCaptureImage (unimplemented)
/// - 2: GetLastApplicationCaptureImage (unimplemented)
/// - 3: GetCallerAppletCaptureImage (unimplemented)
/// - 4: UpdateCallerAppletCaptureImage (unimplemented)
/// - 5: GetLastForegroundCaptureImageEx (unimplemented)
/// - 6: GetLastApplicationCaptureImageEx (unimplemented)
/// - 7: GetCallerAppletCaptureImageEx
/// - 8: TakeScreenShotOfOwnLayer
/// - 9: CopyBetweenCaptureBuffers (unimplemented)
/// - 10: AcquireLastApplicationCaptureBuffer (unimplemented)
/// - 11: ReleaseLastApplicationCaptureBuffer (unimplemented)
/// - 12: AcquireLastForegroundCaptureBuffer (unimplemented)
/// - 13: ReleaseLastForegroundCaptureBuffer (unimplemented)
/// - 14: AcquireCallerAppletCaptureBuffer (unimplemented)
/// - 15: ReleaseCallerAppletCaptureBuffer (unimplemented)
/// - 16: AcquireLastApplicationCaptureBufferEx (unimplemented)
/// - 17: AcquireLastForegroundCaptureBufferEx (unimplemented)
/// - 18: AcquireCallerAppletCaptureBufferEx (unimplemented)
/// - 20: ClearCaptureBuffer
/// - 21: ClearAppletTransitionBuffer (unimplemented)
/// - 22: AcquireLastApplicationCaptureSharedBuffer
/// - 23: ReleaseLastApplicationCaptureSharedBuffer
/// - 24: AcquireLastForegroundCaptureSharedBuffer
/// - 25: ReleaseLastForegroundCaptureSharedBuffer
/// - 26: AcquireCallerAppletCaptureSharedBuffer
/// - 27: ReleaseCallerAppletCaptureSharedBuffer
/// - 28: TakeScreenShotOfOwnLayerEx (unimplemented)
pub struct IDisplayController {
    applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDisplayController {
    pub fn new(applet: Arc<Mutex<crate::hle::service::am::applet::Applet>>) -> Self {
        let handlers = build_handler_map(&[
            (7, Some(Self::get_caller_applet_capture_image_ex_handler), "GetCallerAppletCaptureImageEx"),
            (8, Some(Self::take_screen_shot_of_own_layer_handler), "TakeScreenShotOfOwnLayer"),
            (20, Some(Self::clear_capture_buffer_handler), "ClearCaptureBuffer"),
            (25, Some(Self::release_last_foreground_capture_shared_buffer_handler), "ReleaseLastForegroundCaptureSharedBuffer"),
            (27, Some(Self::release_caller_applet_capture_shared_buffer_handler), "ReleaseCallerAppletCaptureSharedBuffer"),
            (23, Some(Self::release_last_application_capture_shared_buffer_handler), "ReleaseLastApplicationCaptureSharedBuffer"),
        ]);
        Self {
            applet,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of IDisplayController::GetCallerAppletCaptureImageEx
    pub fn get_caller_applet_capture_image_ex(&self) -> bool {
        log::warn!("(STUBBED) GetCallerAppletCaptureImageEx called");
        true // out_was_written
    }

    /// Port of IDisplayController::TakeScreenShotOfOwnLayer
    pub fn take_screen_shot_of_own_layer(&self, _unknown0: bool, _fbshare_layer_index: i32) {
        log::warn!("(STUBBED) TakeScreenShotOfOwnLayer called");
    }

    /// Port of IDisplayController::ClearCaptureBuffer
    pub fn clear_capture_buffer(&self, _unknown0: bool, _fbshare_layer_index: i32, _color: u32) {
        log::warn!("(STUBBED) ClearCaptureBuffer called");
    }

    /// Port of IDisplayController::ReleaseLastForegroundCaptureSharedBuffer
    pub fn release_last_foreground_capture_shared_buffer(&self) {
        log::warn!("(STUBBED) ReleaseLastForegroundCaptureSharedBuffer called");
    }

    /// Port of IDisplayController::ReleaseCallerAppletCaptureSharedBuffer
    pub fn release_caller_applet_capture_shared_buffer(&self) {
        log::warn!("(STUBBED) ReleaseCallerAppletCaptureSharedBuffer called");
    }

    /// Port of IDisplayController::ReleaseLastApplicationCaptureSharedBuffer
    pub fn release_last_application_capture_shared_buffer(&self) {
        log::warn!("(STUBBED) ReleaseLastApplicationCaptureSharedBuffer called");
    }

    fn get_caller_applet_capture_image_ex_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IDisplayController) };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(service.get_caller_applet_capture_image_ex());
    }

    fn take_screen_shot_of_own_layer_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IDisplayController) };
        let mut rp = RequestParser::new(ctx);
        service.take_screen_shot_of_own_layer(rp.pop_bool(), rp.pop_i32());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn clear_capture_buffer_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IDisplayController) };
        let mut rp = RequestParser::new(ctx);
        service.clear_capture_buffer(rp.pop_bool(), rp.pop_i32(), rp.pop_u32());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn release_last_foreground_capture_shared_buffer_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IDisplayController) };
        service.release_last_foreground_capture_shared_buffer();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn release_caller_applet_capture_shared_buffer_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IDisplayController) };
        service.release_caller_applet_capture_shared_buffer();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn release_last_application_capture_shared_buffer_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IDisplayController) };
        service.release_last_application_capture_shared_buffer();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IDisplayController {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IDisplayController {
    fn get_service_name(&self) -> &str {
        "am::IDisplayController"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
