//! Port of zuyu/src/core/hle/service/audio/final_output_recorder_manager_for_applet.h and .cpp
//!
//! IFinalOutputRecorderManagerForApplet service ("audrec:a").

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IFinalOutputRecorderManagerForApplet ("audrec:a"):
///
/// | Cmd | Name           |
/// |-----|----------------|
/// | 0   | RequestSuspend |
/// | 1   | RequestResume  |
pub struct IFinalOutputRecorderManagerForApplet {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IFinalOutputRecorderManagerForApplet {
    pub fn new() -> Self {
        let handlers =
            build_handler_map(&[(0, None, "RequestSuspend"), (1, None, "RequestResume")]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IFinalOutputRecorderManagerForApplet {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IFinalOutputRecorderManagerForApplet {
    fn get_service_name(&self) -> &str {
        "audrec:a"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
