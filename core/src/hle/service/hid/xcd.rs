//! Port of zuyu/src/core/hle/service/hid/xcd.h and xcd.cpp
//!
//! XCD_SYS service ("xcd:sys").

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// XCD_SYS service - external controller device system interface.
pub struct XcdSys {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl XcdSys {
    fn stub_success_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let cmd = ctx.get_command();
        log::debug!("(STUBBED) xcd:s command {}", cmd);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::stub_success_handler), "GetDataFormat"),
            (1, Some(Self::stub_success_handler), "SetDataFormat"),
            (2, Some(Self::stub_success_handler), "GetMcuState"),
            (3, Some(Self::stub_success_handler), "SetMcuState"),
            (4, Some(Self::stub_success_handler), "GetMcuVersionForNfc"),
            (5, Some(Self::stub_success_handler), "CheckNfcDevicePower"),
            (10, Some(Self::stub_success_handler), "SetNfcEvent"),
            (11, Some(Self::stub_success_handler), "GetNfcInfo"),
            (12, Some(Self::stub_success_handler), "StartNfcDiscovery"),
            (13, Some(Self::stub_success_handler), "StopNfcDiscovery"),
            (14, Some(Self::stub_success_handler), "StartNtagRead"),
            (15, Some(Self::stub_success_handler), "StartNtagWrite"),
            (16, Some(Self::stub_success_handler), "SendNfcRawData"),
            (17, Some(Self::stub_success_handler), "RegisterMifareKey"),
            (18, Some(Self::stub_success_handler), "ClearMifareKey"),
            (19, Some(Self::stub_success_handler), "StartMifareRead"),
            (20, Some(Self::stub_success_handler), "StartMifareWrite"),
            (101, Some(Self::stub_success_handler), "GetAwakeTriggerReasonForLeftRail"),
            (102, Some(Self::stub_success_handler), "GetAwakeTriggerReasonForRightRail"),
            (103, Some(Self::stub_success_handler), "GetAwakeTriggerBatteryLevelTransitionForLeftRail"),
            (104, Some(Self::stub_success_handler), "GetAwakeTriggerBatteryLevelTransitionForRightRail"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for XcdSys {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "xcd:s"
    }
}

impl ServiceFramework for XcdSys {
    fn get_service_name(&self) -> &str {
        "xcd:s"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
