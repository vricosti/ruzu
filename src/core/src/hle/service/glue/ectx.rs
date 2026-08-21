// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/ectx.h
//! Port of zuyu/src/core/hle/service/glue/ectx.cpp

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for ECTX_AW
pub mod ectx_aw_commands {
    pub const CREATE_CONTEXT_REGISTRAR: u32 = 0;
    pub const COMMIT_CONTEXT: u32 = 1;
}

/// IPC command IDs for IContextRegistrar
pub mod context_registrar_commands {
    pub const COMPLETE: u32 = 0;
}

/// ECTX_AW service ("ectx:aw").
///
/// Corresponds to `ECTX_AW` in upstream `ectx.h`.
pub struct EctxAW {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl EctxAW {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    ectx_aw_commands::CREATE_CONTEXT_REGISTRAR,
                    Some(EctxAW::create_context_registrar_handler),
                    "CreateContextRegistrar",
                ),
                (ectx_aw_commands::COMMIT_CONTEXT, None, "CommitContext"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Creates and returns an IContextRegistrar instance.
    ///
    /// Upstream creates an IContextRegistrar via PushIpcInterface.
    pub fn create_context_registrar(&self) -> IContextRegistrar {
        IContextRegistrar::new()
    }

    fn create_context_registrar_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const EctxAW) };
        let registrar = service.create_context_registrar();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(Arc::new(registrar));
    }
}

impl SessionRequestHandler for EctxAW {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ectx:aw"
    }
}

impl ServiceFramework for EctxAW {
    fn get_service_name(&self) -> &str {
        "ectx:aw"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IContextRegistrar: nn::err::context::IContextRegistrar.
///
/// Defined in upstream `ectx.cpp`.
pub struct IContextRegistrar {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

#[derive(Clone, Copy, Default)]
#[repr(C)]
struct CompleteInputParameters {
    unk: u32,
}

impl IContextRegistrar {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(
                context_registrar_commands::COMPLETE,
                Some(IContextRegistrar::complete_handler),
                "Complete",
            )]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn complete(&self, _unk: u32, _value: &[u8]) -> (ResultCode, u32) {
        (RESULT_SUCCESS, 0)
    }

    fn complete_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IContextRegistrar) };
        let mut rp = RequestParser::new(ctx);
        let input = rp.pop_raw::<CompleteInputParameters>();
        let value = ctx.read_buffer(0);
        let (result, output) = service.complete(input.unk, &value);

        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(result);
        rb.push_u32(output);
    }
}

impl SessionRequestHandler for IContextRegistrar {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IContextRegistrar"
    }
}

impl ServiceFramework for IContextRegistrar {
    fn get_service_name(&self) -> &str {
        "IContextRegistrar"
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
    fn ectx_aw_registers_upstream_commands() {
        let service = EctxAW::new();
        assert_eq!(service.handlers.len(), 2);
        assert!(
            service.handlers[&ectx_aw_commands::CREATE_CONTEXT_REGISTRAR]
                .handler_callback
                .is_some()
        );
        assert!(service.handlers[&ectx_aw_commands::COMMIT_CONTEXT]
            .handler_callback
            .is_none());
    }

    #[test]
    fn context_registrar_complete_matches_upstream_stub_result() {
        let registrar = IContextRegistrar::new();
        assert_eq!(registrar.handlers.len(), 1);
        assert!(registrar.handlers[&context_registrar_commands::COMPLETE]
            .handler_callback
            .is_some());
        assert_eq!(
            registrar.complete(0xFFFF_FFFF, &[1, 2, 3]),
            (RESULT_SUCCESS, 0)
        );
    }
}
