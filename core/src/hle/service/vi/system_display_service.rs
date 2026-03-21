// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/system_display_service.cpp/.h

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::container::Container;
use super::vi_types::*;

pub struct ISystemDisplayService {
    container: Arc<Container>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISystemDisplayService {
    pub fn new(container: Arc<Container>) -> Self {
        Self {
            container,
            handlers: build_handler_map(&[
                (1200, None, "GetZOrderCountMin"),
                (1202, None, "GetZOrderCountMax"),
                (1203, None, "GetDisplayLogicalResolution"),
                (1204, None, "SetDisplayMagnification"),
                (2201, None, "SetLayerPosition"),
                (2203, None, "SetLayerSize"),
                (2204, None, "GetLayerZ"),
                (2205, Some(Self::set_layer_z), "SetLayerZ"),
                (2207, Some(Self::set_layer_visibility), "SetLayerVisibility"),
                (2209, None, "SetLayerAlpha"),
                (2210, None, "SetLayerPositionAndSize"),
                (2312, None, "CreateStrayLayer"),
                (2400, None, "OpenIndirectLayer"),
                (2401, None, "CloseIndirectLayer"),
                (2402, None, "FlipIndirectLayer"),
                (3000, Some(Self::list_display_modes), "ListDisplayModes"),
                (3001, None, "ListDisplayRgbRanges"),
                (3002, None, "ListDisplayContentTypes"),
                (3200, Some(Self::get_display_mode), "GetDisplayMode"),
                (3201, None, "SetDisplayMode"),
                (3202, None, "GetDisplayUnderscan"),
                (3203, None, "SetDisplayUnderscan"),
                (3204, None, "GetDisplayContentType"),
                (3205, None, "SetDisplayContentType"),
                (3206, None, "GetDisplayRgbRange"),
                (3207, None, "SetDisplayRgbRange"),
                (3208, None, "GetDisplayCmuMode"),
                (3209, None, "SetDisplayCmuMode"),
                (3210, None, "GetDisplayContrastRatio"),
                (3211, None, "SetDisplayContrastRatio"),
                (3214, None, "GetDisplayGamma"),
                (3215, None, "SetDisplayGamma"),
                (3216, None, "GetDisplayCmuLuma"),
                (3217, None, "SetDisplayCmuLuma"),
                (3218, None, "SetDisplayCrcMode"),
                (6013, None, "GetLayerPresentationSubmissionTimestamps"),
                (8225, None, "GetSharedBufferMemoryHandleId"),
                (8250, Some(Self::open_shared_layer), "OpenSharedLayer"),
                (8251, None, "CloseSharedLayer"),
                (8252, Some(Self::connect_shared_layer), "ConnectSharedLayer"),
                (8253, None, "DisconnectSharedLayer"),
                (8254, None, "AcquireSharedFrameBuffer"),
                (8255, None, "PresentSharedFrameBuffer"),
                (8256, None, "GetSharedFrameBufferAcquirableEvent"),
                (8257, None, "FillSharedFrameBufferColor"),
                (8258, None, "CancelSharedFrameBuffer"),
                (9000, None, "GetDp2hdmiController"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    /// cmd 2205: SetLayerZ
    fn set_layer_z(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let z_value = rp.pop_u32();
        let _padding = rp.pop_u32();
        let layer_id = rp.pop_u64();
        log::warn!("ISystemDisplayService::SetLayerZ (STUBBED) layer_id={}, z_value={}", layer_id, z_value);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 2207: SetLayerVisibility
    /// This function currently does nothing but return a success error code in
    /// the vi library itself, so do the same thing, but log out the passed in values.
    fn set_layer_visibility(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let visible = rp.pop_u32() != 0;
        let _padding = rp.pop_u32();
        let layer_id = rp.pop_u64();
        log::debug!("ISystemDisplayService::SetLayerVisibility layer_id={}, visible={}", layer_id, visible);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 3000: ListDisplayModes
    fn list_display_modes(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let display_id = rp.pop_u64();
        log::warn!("ISystemDisplayService::ListDisplayModes (STUBBED) display_id={}", display_id);

        let mode = DisplayMode {
            width: 1920,
            height: 1080,
            refresh_rate: 60.0,
            unknown: 0,
        };
        let bytes = unsafe {
            std::slice::from_raw_parts(
                &mode as *const DisplayMode as *const u8,
                std::mem::size_of::<DisplayMode>(),
            )
        };
        ctx.write_buffer(bytes, 0);

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(1); // count
    }

    /// cmd 3200: GetDisplayMode
    fn get_display_mode(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let display_id = rp.pop_u64();
        log::warn!("ISystemDisplayService::GetDisplayMode (STUBBED) display_id={}", display_id);

        let mode = if common::settings::is_docked_mode(&common::settings::values()) {
            DisplayMode {
                width: DisplayResolution::DockedWidth as u32,
                height: DisplayResolution::DockedHeight as u32,
                refresh_rate: 60.0,
                unknown: 0,
            }
        } else {
            DisplayMode {
                width: DisplayResolution::UndockedWidth as u32,
                height: DisplayResolution::UndockedHeight as u32,
                refresh_rate: 60.0,
                unknown: 0,
            }
        };

        let bytes = unsafe {
            std::slice::from_raw_parts(
                &mode as *const DisplayMode as *const u8,
                std::mem::size_of::<DisplayMode>(),
            )
        };
        ctx.write_buffer(bytes, 0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 8250: OpenSharedLayer
    fn open_shared_layer(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let layer_id = rp.pop_u64();
        log::info!("ISystemDisplayService::OpenSharedLayer (STUBBED) layer_id={}", layer_id);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 8252: ConnectSharedLayer
    fn connect_shared_layer(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let layer_id = rp.pop_u64();
        log::info!("ISystemDisplayService::ConnectSharedLayer (STUBBED) layer_id={}", layer_id);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for ISystemDisplayService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
}

impl ServiceFramework for ISystemDisplayService {
    fn get_service_name(&self) -> &str { "vi::ISystemDisplayService" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}
