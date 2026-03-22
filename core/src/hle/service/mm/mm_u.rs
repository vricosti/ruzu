// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mm/mm_u.cpp
//!
//! MM_U service ("mm:u").

use std::collections::BTreeMap;
use std::sync::Mutex;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Interior mutable state for MM_U.
struct MmUState {
    min: u32,
    max: u32,
    current: u32,
    id: u32,
}

/// MM_U service ("mm:u").
///
/// Corresponds to `MM_U` class in upstream `mm_u.cpp`.
pub struct MmU {
    state: Mutex<MmUState>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl MmU {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::initialize_old_handler), "InitializeOld"),
            (1, Some(Self::finalize_old_handler), "FinalizeOld"),
            (2, Some(Self::set_and_wait_old_handler), "SetAndWaitOld"),
            (3, Some(Self::get_old_handler), "GetOld"),
            (4, Some(Self::initialize_handler), "Initialize"),
            (5, Some(Self::finalize_handler), "Finalize"),
            (6, Some(Self::set_and_wait_handler), "SetAndWait"),
            (7, Some(Self::get_handler), "Get"),
        ]);

        Self {
            state: Mutex::new(MmUState {
                min: 0,
                max: 0,
                current: 0,
                id: 1,
            }),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn initialize_old(&self) {
        log::warn!("(STUBBED) MmU::initialize_old called");
    }

    pub fn finalize_old(&self) {
        log::warn!("(STUBBED) MmU::finalize_old called");
    }

    pub fn set_and_wait_old(&self, min: u32, max: u32) {
        log::debug!("(STUBBED) MmU::set_and_wait_old called, min={:#x}, max={:#x}", min, max);
        let mut s = self.state.lock().unwrap();
        s.min = min;
        s.max = max;
        s.current = min;
    }

    pub fn get_old(&self) -> u32 {
        log::debug!("(STUBBED) MmU::get_old called");
        self.state.lock().unwrap().current
    }

    pub fn initialize(&self) -> u32 {
        log::warn!("(STUBBED) MmU::initialize called");
        self.state.lock().unwrap().id // Any non-zero value
    }

    pub fn finalize(&self) {
        log::warn!("(STUBBED) MmU::finalize called");
    }

    pub fn set_and_wait(&self, _input_id: u32, min: u32, max: u32) {
        log::debug!(
            "(STUBBED) MmU::set_and_wait called, min={:#x}, max={:#x}",
            min,
            max
        );
        let mut s = self.state.lock().unwrap();
        s.min = min;
        s.max = max;
        s.current = min;
    }

    pub fn get(&self) -> u32 {
        log::debug!("(STUBBED) MmU::get called");
        self.state.lock().unwrap().current
    }

    // --- Handler bridge functions ---

    fn initialize_old_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const MmU) };
        service.initialize_old();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn finalize_old_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const MmU) };
        service.finalize_old();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_and_wait_old_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const MmU) };
        let mut rp = RequestParser::new(ctx);
        let min = rp.pop_u32();
        let max = rp.pop_u32();
        service.set_and_wait_old(min, max);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_old_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const MmU) };
        let val = service.get_old();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(val);
    }

    fn initialize_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const MmU) };
        let id = service.initialize();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(id);
    }

    fn finalize_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const MmU) };
        service.finalize();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn set_and_wait_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const MmU) };
        let mut rp = RequestParser::new(ctx);
        let input_id = rp.pop_u32();
        let min = rp.pop_u32();
        let max = rp.pop_u32();
        service.set_and_wait(input_id, min, max);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const MmU) };
        let val = service.get();
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(val);
    }
}

impl SessionRequestHandler for MmU {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str { "mm:u" }
}

impl ServiceFramework for MmU {
    fn get_service_name(&self) -> &str { "mm:u" }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

/// Registers "mm:u" service.
///
/// Corresponds to `LoopProcess` in upstream `mm_u.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    let mut server_manager = ServerManager::new(system);
    server_manager.register_named_service(
        "mm:u",
        Box::new(|| -> SessionRequestHandlerPtr {
            std::sync::Arc::new(MmU::new())
        }),
        64,
    );
    ServerManager::run_server(server_manager);
}
