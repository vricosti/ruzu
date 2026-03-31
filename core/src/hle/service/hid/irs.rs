//! Port of zuyu/src/core/hle/service/hid/irs.h and irs.cpp
//!
//! IRS and IRS_SYS services ("irs", "irs:sys").

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IRS service - IR sensor interface.
pub struct Irs {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl Irs {
    fn stub_success_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::debug!("(STUBBED) irs command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (302, Some(Self::stub_success_handler), "ActivateIrsensor"),
            (303, Some(Self::stub_success_handler), "DeactivateIrsensor"),
            (
                304,
                Some(Self::stub_success_handler),
                "GetIrsensorSharedMemoryHandle",
            ),
            (305, Some(Self::stub_success_handler), "StopImageProcessor"),
            (306, Some(Self::stub_success_handler), "RunMomentProcessor"),
            (
                307,
                Some(Self::stub_success_handler),
                "RunClusteringProcessor",
            ),
            (
                308,
                Some(Self::stub_success_handler),
                "RunImageTransferProcessor",
            ),
            (
                309,
                Some(Self::stub_success_handler),
                "GetImageTransferProcessorState",
            ),
            (
                310,
                Some(Self::stub_success_handler),
                "RunTeraPluginProcessor",
            ),
            (
                311,
                Some(Self::stub_success_handler),
                "GetNpadIrCameraHandle",
            ),
            (
                312,
                Some(Self::stub_success_handler),
                "RunPointingProcessor",
            ),
            (
                313,
                Some(Self::stub_success_handler),
                "SuspendImageProcessor",
            ),
            (
                314,
                Some(Self::stub_success_handler),
                "CheckFirmwareVersion",
            ),
            (315, Some(Self::stub_success_handler), "SetFunctionLevel"),
            (
                316,
                Some(Self::stub_success_handler),
                "RunImageTransferExProcessor",
            ),
            (317, Some(Self::stub_success_handler), "RunIrLedProcessor"),
            (
                318,
                Some(Self::stub_success_handler),
                "StopImageProcessorAsync",
            ),
            (
                319,
                Some(Self::stub_success_handler),
                "ActivateIrsensorWithFunctionLevel",
            ),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for Irs {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "irs"
    }
}

impl ServiceFramework for Irs {
    fn get_service_name(&self) -> &str {
        "irs"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IRS_SYS service - system-level IR sensor interface.
pub struct IrsSys {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IrsSys {
    fn stub_success_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::debug!("(STUBBED) irs:sys command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (
                500,
                Some(Self::stub_success_handler),
                "SetAppletResourceUserId",
            ),
            (
                501,
                Some(Self::stub_success_handler),
                "RegisterAppletResourceUserId",
            ),
            (
                502,
                Some(Self::stub_success_handler),
                "UnregisterAppletResourceUserId",
            ),
            (
                503,
                Some(Self::stub_success_handler),
                "EnableAppletToGetInput",
            ),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IrsSys {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "irs:sys"
    }
}

impl ServiceFramework for IrsSys {
    fn get_service_name(&self) -> &str {
        "irs:sys"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
