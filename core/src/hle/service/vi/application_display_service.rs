// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/application_display_service.cpp/.h
//!
//! IApplicationDisplayService — the main display service returned by GetDisplayService.
//! Handles display/layer operations, vsync events, and sub-service creation.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::nvnflinger::parcel::OutputParcel;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::container::Container;
use super::vi_results;
use super::vi_types::*;

pub struct IApplicationDisplayService {
    container: Arc<Container>,
    open_layer_ids: Mutex<BTreeSet<u64>>,
    stray_layer_ids: Mutex<BTreeSet<u64>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IApplicationDisplayService {
    pub fn new(container: Arc<Container>) -> Self {
        Self {
            container,
            open_layer_ids: Mutex::new(BTreeSet::new()),
            stray_layer_ids: Mutex::new(BTreeSet::new()),
            handlers: build_handler_map(&[
                (100, Some(Self::get_relay_service), "GetRelayService"),
                (101, Some(Self::get_system_display_service), "GetSystemDisplayService"),
                (102, Some(Self::get_manager_display_service), "GetManagerDisplayService"),
                (103, Some(Self::get_relay_service), "GetIndirectDisplayTransactionService"),
                (1000, Some(Self::list_displays), "ListDisplays"),
                (1010, Some(Self::open_display), "OpenDisplay"),
                (1011, Some(Self::open_default_display), "OpenDefaultDisplay"),
                (1020, Some(Self::close_display), "CloseDisplay"),
                (1101, Some(Self::stub_ok), "SetDisplayEnabled"),
                (1102, Some(Self::get_display_resolution), "GetDisplayResolution"),
                (2020, Some(Self::open_layer), "OpenLayer"),
                (2021, Some(Self::close_layer), "CloseLayer"),
                (2030, Some(Self::create_stray_layer), "CreateStrayLayer"),
                (2031, Some(Self::destroy_stray_layer), "DestroyStrayLayer"),
                (2101, Some(Self::set_layer_scaling_mode), "SetLayerScalingMode"),
                (2102, Some(Self::convert_scaling_mode_handler), "ConvertScalingMode"),
                (2450, Some(Self::get_indirect_layer_image_map), "GetIndirectLayerImageMap"),
                (2451, None, "GetIndirectLayerImageCropMap"),
                (2460, Some(Self::get_indirect_layer_image_required_memory_info), "GetIndirectLayerImageRequiredMemoryInfo"),
                (5202, Some(Self::get_display_vsync_event), "GetDisplayVsyncEvent"),
                (5203, None, "GetDisplayVsyncEventForDebug"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    /// cmd 100 / 103: GetRelayService / GetIndirectDisplayTransactionService
    /// Returns the real IHOSBinderDriver from the Container.
    fn get_relay_service(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::info!("IApplicationDisplayService::GetRelayService called");
        let binder_driver = svc.container.get_binder_driver();
        super::super::am::service::application_proxy::IApplicationProxy::push_interface_response(ctx, binder_driver);
    }

    /// cmd 101: GetSystemDisplayService
    fn get_system_display_service(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("IApplicationDisplayService::GetSystemDisplayService (STUBBED)");
        let sub: Arc<dyn SessionRequestHandler> = Arc::new(
            super::vi::ViStubService::new("ISystemDisplayService"),
        );
        super::super::am::service::application_proxy::IApplicationProxy::push_interface_response(ctx, sub);
    }

    /// cmd 102: GetManagerDisplayService
    fn get_manager_display_service(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("IApplicationDisplayService::GetManagerDisplayService (STUBBED)");
        let sub: Arc<dyn SessionRequestHandler> = Arc::new(
            super::vi::ViStubService::new("IManagerDisplayService"),
        );
        super::super::am::service::application_proxy::IApplicationProxy::push_interface_response(ctx, sub);
    }

    /// cmd 1000: ListDisplays
    fn list_displays(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("IApplicationDisplayService::ListDisplays (STUBBED)");
        let info = DisplayInfo::default();
        let bytes = unsafe {
            std::slice::from_raw_parts(
                &info as *const DisplayInfo as *const u8,
                std::mem::size_of::<DisplayInfo>(),
            )
        };
        ctx.write_buffer(bytes, 0);
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(1); // count
    }

    /// cmd 1010: OpenDisplay
    fn open_display(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        // Display name is 0x40 bytes inline in request data
        let mut rp = RequestParser::new(ctx);
        let mut display_name: DisplayName = [0u8; 0x40];
        // Read 16 u32s = 64 bytes
        for i in 0..16 {
            let word = rp.pop_u32();
            let offset = i * 4;
            display_name[offset..offset + 4].copy_from_slice(&word.to_le_bytes());
        }
        display_name[0x3f] = 0;

        let name_str = std::str::from_utf8(&display_name)
            .unwrap_or("")
            .trim_end_matches('\0');
        log::info!("IApplicationDisplayService::OpenDisplay(\"{}\")", name_str);

        match svc.container.open_display(&display_name) {
            Ok(display_id) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(display_id);
            }
            Err(err) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(err);
            }
        }
    }

    /// cmd 1011: OpenDefaultDisplay
    fn open_default_display(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IApplicationDisplayService::OpenDefaultDisplay called");
        let mut name: DisplayName = [0u8; 0x40];
        name[..7].copy_from_slice(b"Default");
        match svc.container.open_display(&name) {
            Ok(display_id) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(display_id);
            }
            Err(err) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(err);
            }
        }
    }

    /// cmd 1020: CloseDisplay
    fn close_display(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let display_id = rp.pop_u64();
        log::debug!("IApplicationDisplayService::CloseDisplay({})", display_id);
        let _ = svc.container.close_display(display_id);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 1102: GetDisplayResolution
    fn get_display_resolution(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let display_id = rp.pop_u64();
        log::debug!("IApplicationDisplayService::GetDisplayResolution({})", display_id);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(DisplayResolution::UndockedWidth as u64);
        rb.push_u64(DisplayResolution::UndockedHeight as u64);
    }

    /// cmd 2020: OpenLayer
    fn open_layer(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);

        // Read DisplayName (0x40 = 16 u32s)
        let mut display_name: DisplayName = [0u8; 0x40];
        for i in 0..16 {
            let word = rp.pop_u32();
            let offset = i * 4;
            display_name[offset..offset + 4].copy_from_slice(&word.to_le_bytes());
        }
        display_name[0x3f] = 0;

        let layer_id = rp.pop_u64();
        let aruid = rp.pop_u64();

        log::info!("IApplicationDisplayService::OpenLayer(layer_id={}, aruid={:#x})", layer_id, aruid);

        // Open display first
        if let Err(err) = svc.container.open_display(&display_name) {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(err);
            return;
        }

        match svc.container.open_layer(layer_id, aruid) {
            Ok(producer_binder_id) => {
                svc.open_layer_ids.lock().unwrap().insert(layer_id);

                let native_window = NativeWindow::new(producer_binder_id);
                let mut parcel = OutputParcel::new();
                parcel.write_interface(&native_window);
                let buffer = parcel.serialize();
                ctx.write_buffer(&buffer, 0);

                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(buffer.len() as u64);
            }
            Err(err) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(err);
            }
        }
    }

    /// cmd 2021: CloseLayer
    fn close_layer(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let layer_id = rp.pop_u64();
        log::debug!("IApplicationDisplayService::CloseLayer({})", layer_id);

        {
            let mut ids = svc.open_layer_ids.lock().unwrap();
            if !ids.remove(&layer_id) {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(vi_results::RESULT_NOT_FOUND);
                return;
            }
        }
        let _ = svc.container.close_layer(layer_id);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 2030: CreateStrayLayer
    fn create_stray_layer(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let flags = rp.pop_u32();
        let _padding = rp.pop_u32(); // align to u64
        let display_id = rp.pop_u64();

        log::info!("IApplicationDisplayService::CreateStrayLayer(flags={}, display_id={})", flags, display_id);

        match svc.container.create_stray_layer(display_id) {
            Ok((producer_binder_id, layer_id)) => {
                svc.stray_layer_ids.lock().unwrap().insert(layer_id);

                let native_window = NativeWindow::new(producer_binder_id);
                let mut parcel = OutputParcel::new();
                parcel.write_interface(&native_window);
                let buffer = parcel.serialize();
                ctx.write_buffer(&buffer, 0);

                let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(layer_id);
                rb.push_u64(buffer.len() as u64);
            }
            Err(err) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(err);
            }
        }
    }

    /// cmd 2031: DestroyStrayLayer
    fn destroy_stray_layer(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let layer_id = rp.pop_u64();
        log::info!("IApplicationDisplayService::DestroyStrayLayer({})", layer_id);

        {
            let mut ids = svc.stray_layer_ids.lock().unwrap();
            if !ids.remove(&layer_id) {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(vi_results::RESULT_NOT_FOUND);
                return;
            }
        }
        let _ = svc.container.destroy_stray_layer(layer_id);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 2101: SetLayerScalingMode
    fn set_layer_scaling_mode(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("IApplicationDisplayService::SetLayerScalingMode called");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 2102: ConvertScalingMode
    fn convert_scaling_mode_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let mode_val = rp.pop_u32();
        log::debug!("IApplicationDisplayService::ConvertScalingMode({})", mode_val);

        let mode = unsafe { std::mem::transmute::<u32, NintendoScaleMode>(mode_val) };
        match convert_scaling_mode(mode) {
            Some(converted) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(converted as u64);
            }
            None => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(vi_results::RESULT_OPERATION_FAILED);
            }
        }
    }

    /// cmd 2450: GetIndirectLayerImageMap (STUBBED)
    fn get_indirect_layer_image_map(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("IApplicationDisplayService::GetIndirectLayerImageMap (STUBBED)");
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(0);
        rb.push_u64(0);
    }

    /// cmd 2460: GetIndirectLayerImageRequiredMemoryInfo
    fn get_indirect_layer_image_required_memory_info(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let width = rp.pop_i64();
        let height = rp.pop_i64();
        log::debug!("IApplicationDisplayService::GetIndirectLayerImageRequiredMemoryInfo(w={}, h={})", width, height);

        let (size, alignment) = get_indirect_layer_image_required_memory_info(width, height);
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(size as u64);
        rb.push_u64(alignment as u64);
    }

    /// cmd 5202: GetDisplayVsyncEvent
    fn get_display_vsync_event(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let display_id = rp.pop_u64();
        log::info!("IApplicationDisplayService::GetDisplayVsyncEvent(display_id={})", display_id);

        // Create a readable event handle that the game can wait on.
        // Signal it immediately so the game proceeds without blocking.
        if let Some(handle) = ctx.create_readable_event_handle(true) {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(handle);
        } else {
            log::warn!("GetDisplayVsyncEvent: failed to create event, returning dummy handle");
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(0);
        }
    }

    /// Generic stub that returns RESULT_SUCCESS with no extra data.
    fn stub_ok(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IApplicationDisplayService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
}

impl ServiceFramework for IApplicationDisplayService {
    fn get_service_name(&self) -> &str { "IApplicationDisplayService" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

// -- Pure logic helpers matching upstream --

pub fn convert_scaling_mode(mode: NintendoScaleMode) -> Option<ConvertedScaleMode> {
    match mode {
        NintendoScaleMode::None => Some(ConvertedScaleMode::None),
        NintendoScaleMode::Freeze => Some(ConvertedScaleMode::Freeze),
        NintendoScaleMode::ScaleToWindow => Some(ConvertedScaleMode::ScaleToWindow),
        NintendoScaleMode::ScaleAndCrop => Some(ConvertedScaleMode::ScaleAndCrop),
        NintendoScaleMode::PreserveAspectRatio => Some(ConvertedScaleMode::PreserveAspectRatio),
        _ => None,
    }
}

pub fn get_indirect_layer_image_required_memory_info(width: i64, height: i64) -> (i64, i64) {
    const BASE_SIZE: u64 = 0x20000;
    let texture_size = (width * height * 4) as u64;
    let alignment: i64 = 0x1000;
    let size = ((texture_size + BASE_SIZE - 1) / BASE_SIZE * BASE_SIZE) as i64;
    (size, alignment)
}
