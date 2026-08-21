//! Port of zuyu/src/core/hle/service/filesystem/fsp/fsp_pr.h and fsp_pr.cpp
//!
//! FSP_PR service ("fsp:pr").

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for FSP_PR ("fsp:pr"):
///
/// | Cmd | Name                            |
/// |-----|---------------------------------|
/// | 0   | RegisterProgram                 |
/// | 1   | UnregisterProgram               |
/// | 2   | SetCurrentProcess               |
/// | 256 | SetEnabledProgramVerification   |
pub struct FspPr {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl FspPr {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, Some(Self::stub_handler), "RegisterProgram"),
                (1, Some(Self::stub_handler), "UnregisterProgram"),
                (2, Some(Self::stub_handler), "SetCurrentProcess"),
                (
                    256,
                    Some(Self::stub_handler),
                    "SetEnabledProgramVerification",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn stub_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for FspPr {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "fsp:pr"
    }
}

impl ServiceFramework for FspPr {
    fn get_service_name(&self) -> &str {
        "fsp:pr"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }

    fn invoke_request(&self, ctx: &mut HLERequestContext)
    where
        Self: Sized,
    {
        let cmd = ctx.get_command();
        if let Some(fi) = self.handlers().get(&cmd) {
            if let Some(callback) = fi.handler_callback {
                callback(self, ctx);
                return;
            }
        }
        log::warn!(
            "fsp:pr: unimplemented command '{}' returned stub success",
            cmd
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}
