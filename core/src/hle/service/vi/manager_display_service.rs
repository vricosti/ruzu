// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/manager_display_service.cpp/.h

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::kernel::k_process::KProcess;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::container::Container;

pub struct IManagerDisplayService {
    container: Arc<Container>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IManagerDisplayService {
    pub fn new(container: Arc<Container>) -> Self {
        Self {
            container,
            handlers: build_handler_map(&[
                (200, None, "AllocateProcessHeapBlock"),
                (201, None, "FreeProcessHeapBlock"),
                (1102, None, "GetDisplayResolution"),
                (
                    2010,
                    Some(Self::create_managed_layer_handler),
                    "CreateManagedLayer",
                ),
                (
                    2011,
                    Some(Self::destroy_managed_layer_handler),
                    "DestroyManagedLayer",
                ),
                (2012, None, "CreateStrayLayer"),
                (2050, None, "CreateIndirectLayer"),
                (2051, None, "DestroyIndirectLayer"),
                (2052, None, "CreateIndirectProducerEndPoint"),
                (2053, None, "DestroyIndirectProducerEndPoint"),
                (2054, None, "CreateIndirectConsumerEndPoint"),
                (2055, None, "DestroyIndirectConsumerEndPoint"),
                (2060, None, "CreateWatermarkCompositor"),
                (2062, None, "SetWatermarkText"),
                (2063, None, "SetWatermarkLayerStacks"),
                (2300, None, "AcquireLayerTexturePresentingEvent"),
                (2301, None, "ReleaseLayerTexturePresentingEvent"),
                (2302, None, "GetDisplayHotplugEvent"),
                (2303, None, "GetDisplayModeChangedEvent"),
                (2402, None, "GetDisplayHotplugState"),
                (2501, None, "GetCompositorErrorInfo"),
                (2601, None, "GetDisplayErrorEvent"),
                (2701, None, "GetDisplayFatalErrorEvent"),
                (4201, None, "SetDisplayAlpha"),
                (4203, None, "SetDisplayLayerStack"),
                (4205, None, "SetDisplayPowerState"),
                (4206, None, "SetDefaultDisplay"),
                (4207, None, "ResetDisplayPanel"),
                (4208, None, "SetDisplayFatalErrorEnabled"),
                (4209, None, "IsDisplayPanelOn"),
                (4300, None, "GetInternalPanelId"),
                (6000, Some(Self::add_to_layer_stack), "AddToLayerStack"),
                (6001, None, "RemoveFromLayerStack"),
                (
                    6002,
                    Some(Self::set_layer_visibility_handler),
                    "SetLayerVisibility",
                ),
                (6003, None, "SetLayerConfig"),
                (6004, None, "AttachLayerPresentationTracer"),
                (6005, None, "DetachLayerPresentationTracer"),
                (6006, None, "StartLayerPresentationRecording"),
                (6007, None, "StopLayerPresentationRecording"),
                (6008, None, "StartLayerPresentationFenceWait"),
                (6009, None, "StopLayerPresentationFenceWait"),
                (6010, None, "GetLayerPresentationAllFencesExpiredEvent"),
                (6011, None, "EnableLayerAutoClearTransitionBuffer"),
                (6012, None, "DisableLayerAutoClearTransitionBuffer"),
                (6013, None, "SetLayerOpacity"),
                (6014, None, "AttachLayerWatermarkCompositor"),
                (6015, None, "DetachLayerWatermarkCompositor"),
                (7000, None, "SetContentVisibility"),
                (8000, None, "SetConductorLayer"),
                (8001, None, "SetTimestampTracking"),
                (8100, None, "SetIndirectProducerFlipOffset"),
                (8200, None, "CreateSharedBufferStaticStorage"),
                (8201, None, "CreateSharedBufferTransferMemory"),
                (8202, None, "DestroySharedBuffer"),
                (8203, None, "BindSharedLowLevelLayerToManagedLayer"),
                (8204, None, "BindSharedLowLevelLayerToIndirectLayer"),
                (8207, None, "UnbindSharedLowLevelLayer"),
                (8208, None, "ConnectSharedLowLevelLayerToSharedBuffer"),
                (8209, None, "DisconnectSharedLowLevelLayerFromSharedBuffer"),
                (8210, None, "CreateSharedLayer"),
                (8211, None, "DestroySharedLayer"),
                (8216, None, "AttachSharedLayerToLowLevelLayer"),
                (8217, None, "ForceDetachSharedLayerFromLowLevelLayer"),
                (8218, None, "StartDetachSharedLayerFromLowLevelLayer"),
                (8219, None, "FinishDetachSharedLayerFromLowLevelLayer"),
                (8220, None, "GetSharedLayerDetachReadyEvent"),
                (8221, None, "GetSharedLowLevelLayerSynchronizedEvent"),
                (8222, None, "CheckSharedLowLevelLayerSynchronized"),
                (8223, None, "RegisterSharedBufferImporterAruid"),
                (8224, None, "UnregisterSharedBufferImporterAruid"),
                (8227, None, "CreateSharedBufferProcessHeap"),
                (8228, None, "GetSharedLayerLayerStacks"),
                (8229, None, "SetSharedLayerLayerStacks"),
                (
                    8291,
                    None,
                    "PresentDetachedSharedFrameBufferToLowLevelLayer",
                ),
                (8292, None, "FillDetachedSharedFrameBufferColor"),
                (8293, None, "GetDetachedSharedFrameBufferImage"),
                (8294, None, "SetDetachedSharedFrameBufferImage"),
                (8295, None, "CopyDetachedSharedFrameBufferImage"),
                (8296, None, "SetDetachedSharedFrameBufferSubImage"),
                (8297, None, "GetSharedFrameBufferContentParameter"),
                (8298, None, "ExpandStartupLogoOnSharedFrameBuffer"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    pub fn create_shared_layer_session(
        &self,
        owner_process: &Arc<std::sync::Mutex<KProcess>>,
        display_id: u64,
        enable_blending: bool,
    ) -> Result<(u64, u64), ResultCode> {
        self.container
            .get_shared_buffer_manager()
            .create_session(owner_process, display_id, enable_blending)
    }

    pub fn destroy_shared_layer_session(
        &self,
        owner_process: &Arc<std::sync::Mutex<KProcess>>,
    ) {
        self.container
            .get_shared_buffer_manager()
            .destroy_session(owner_process);
    }

    pub fn set_layer_blending(&self, enabled: bool, layer_id: u64) -> Result<(), ResultCode> {
        self.container.set_layer_blending(layer_id, enabled)
    }

    pub fn create_managed_layer(
        &self,
        flags: u32,
        display_id: u64,
        aruid: u64,
    ) -> Result<u64, ResultCode> {
        let _ = flags;
        self.container.create_managed_layer(display_id, aruid)
    }

    pub fn destroy_managed_layer(&self, layer_id: u64) -> Result<(), ResultCode> {
        self.container.destroy_managed_layer(layer_id)
    }

    pub fn set_layer_visibility(&self, visible: bool, layer_id: u64) -> Result<(), ResultCode> {
        self.container.set_layer_visibility(layer_id, visible)
    }

    /// cmd 2010: CreateManagedLayer
    fn create_managed_layer_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let flags = rp.pop_u32();
        let _padding = rp.pop_u32();
        let display_id = rp.pop_u64();
        let aruid = rp.pop_u64();
        log::debug!(
            "IManagerDisplayService::CreateManagedLayer flags={}, display={}, aruid={}",
            flags,
            display_id,
            aruid
        );

        match svc.create_managed_layer(flags, display_id, aruid) {
            Ok(layer_id) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_u64(layer_id);
            }
            Err(err) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(err);
            }
        }
    }

    /// cmd 2011: DestroyManagedLayer
    fn destroy_managed_layer_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let layer_id = rp.pop_u64();
        log::debug!(
            "IManagerDisplayService::DestroyManagedLayer layer_id={}",
            layer_id
        );

        match svc.destroy_managed_layer(layer_id) {
            Ok(()) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
            Err(err) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(err);
            }
        }
    }

    /// cmd 6000: AddToLayerStack
    fn add_to_layer_stack(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let stack_id = rp.pop_u32();
        let _padding = rp.pop_u32();
        let layer_id = rp.pop_u64();
        log::warn!(
            "IManagerDisplayService::AddToLayerStack (STUBBED) stack_id={}, layer_id={}",
            stack_id,
            layer_id
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 6002: SetLayerVisibility
    fn set_layer_visibility_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let visible = rp.pop_u32() != 0;
        let _padding = rp.pop_u32();
        let layer_id = rp.pop_u64();
        log::debug!(
            "IManagerDisplayService::SetLayerVisibility layer_id={}, visible={}",
            layer_id,
            visible
        );

        match svc.set_layer_visibility(visible, layer_id) {
            Ok(()) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
            Err(err) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(err);
            }
        }
    }
}

impl SessionRequestHandler for IManagerDisplayService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
}

impl ServiceFramework for IManagerDisplayService {
    fn get_service_name(&self) -> &str {
        "vi::IManagerDisplayService"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
