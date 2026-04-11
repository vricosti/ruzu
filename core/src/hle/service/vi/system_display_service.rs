// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/system_display_service.cpp/.h

use std::collections::BTreeMap;
use std::sync::Arc;

use common::math_util::Rectangle;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::nvnflinger::ui::fence::Fence;
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
                (
                    8225,
                    Some(Self::get_shared_buffer_memory_handle_id),
                    "GetSharedBufferMemoryHandleId",
                ),
                (8250, Some(Self::open_shared_layer), "OpenSharedLayer"),
                (8251, None, "CloseSharedLayer"),
                (8252, Some(Self::connect_shared_layer), "ConnectSharedLayer"),
                (8253, None, "DisconnectSharedLayer"),
                (
                    8254,
                    Some(Self::acquire_shared_frame_buffer),
                    "AcquireSharedFrameBuffer",
                ),
                (
                    8255,
                    Some(Self::present_shared_frame_buffer),
                    "PresentSharedFrameBuffer",
                ),
                (
                    8256,
                    Some(Self::get_shared_frame_buffer_acquirable_event),
                    "GetSharedFrameBufferAcquirableEvent",
                ),
                (8257, None, "FillSharedFrameBufferColor"),
                (
                    8258,
                    Some(Self::cancel_shared_frame_buffer),
                    "CancelSharedFrameBuffer",
                ),
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
        log::warn!(
            "ISystemDisplayService::SetLayerZ (STUBBED) layer_id={}, z_value={}",
            layer_id,
            z_value
        );
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
        log::debug!(
            "ISystemDisplayService::SetLayerVisibility layer_id={}, visible={}",
            layer_id,
            visible
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 3000: ListDisplayModes
    fn list_display_modes(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let display_id = rp.pop_u64();
        log::warn!(
            "ISystemDisplayService::ListDisplayModes (STUBBED) display_id={}",
            display_id
        );

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
        log::warn!(
            "ISystemDisplayService::GetDisplayMode (STUBBED) display_id={}",
            display_id
        );

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

    fn get_shared_buffer_memory_handle_id(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let buffer_id = rp.pop_u64();
        let aruid = rp.pop_u64();

        match svc
            .container
            .get_shared_buffer_manager()
            .get_shared_buffer_memory_handle_id(buffer_id, aruid)
        {
            Ok((size, nvmap_handle, pool_layout)) => {
                let bytes = unsafe {
                    std::slice::from_raw_parts(
                        &pool_layout as *const _ as *const u8,
                        std::mem::size_of_val(&pool_layout),
                    )
                };
                ctx.write_buffer(bytes, 0);
                let mut rb = ResponseBuilder::new(ctx, 5, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_i32(nvmap_handle);
                rb.push_u64(size);
            }
            Err(err) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(err);
            }
        }
    }

    /// cmd 8250: OpenSharedLayer
    fn open_shared_layer(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let layer_id = rp.pop_u64();
        log::info!(
            "ISystemDisplayService::OpenSharedLayer (STUBBED) layer_id={}",
            layer_id
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 8252: ConnectSharedLayer
    fn connect_shared_layer(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let layer_id = rp.pop_u64();
        log::info!(
            "ISystemDisplayService::ConnectSharedLayer (STUBBED) layer_id={}",
            layer_id
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn acquire_shared_frame_buffer(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let layer_id = rp.pop_u64();

        match svc
            .container
            .get_shared_buffer_manager()
            .acquire_shared_frame_buffer(layer_id)
        {
            Ok((fence, slots, target_slot)) => {
                let mut rb = ResponseBuilder::new(ctx, 8, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_raw(&fence);
                for slot in slots {
                    rb.push_i32(slot);
                }
                rb.push_i64(target_slot);
            }
            Err(err) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(err);
            }
        }
    }

    fn present_shared_frame_buffer(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let fence: Fence = rp.pop_raw();
        let crop_region: Rectangle<i32> = rp.pop_raw();
        let window_transform = rp.pop_u32();
        let swap_interval = rp.pop_i32();
        let layer_id = rp.pop_u64();
        let surface_id = rp.pop_i64();

        let result = svc
            .container
            .get_shared_buffer_manager()
            .present_shared_frame_buffer(
                fence,
                crop_region,
                window_transform,
                swap_interval,
                layer_id,
                surface_id,
            );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result.err().unwrap_or(RESULT_SUCCESS));
    }

    fn get_shared_frame_buffer_acquirable_event(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let layer_id = rp.pop_u64();

        let handle = svc
            .container
            .get_shared_buffer_manager()
            .get_shared_frame_buffer_acquirable_event(layer_id)
            .ok()
            .and_then(|event| ctx.copy_handle_for_readable_event(event))
            .unwrap_or(0);

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle);
    }

    fn cancel_shared_frame_buffer(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let layer_id = rp.pop_u64();
        let slot = rp.pop_i64();

        let result = svc
            .container
            .get_shared_buffer_manager()
            .cancel_shared_frame_buffer(layer_id, slot);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result.err().unwrap_or(RESULT_SUCCESS));
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
    fn get_service_name(&self) -> &str {
        "vi::ISystemDisplayService"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
