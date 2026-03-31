//! Port of zuyu/src/core/hle/service/audio/final_output_recorder_manager.h and .cpp
//!
//! IFinalOutputRecorderManager service ("audrec:u").

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IFinalOutputRecorderManager ("audrec:u"):
///
/// | Cmd | Name                        |
/// |-----|-----------------------------|
/// | 0   | OpenFinalOutputRecorder     |
///
/// IFinalOutputRecorder (returned by OpenFinalOutputRecorder):
///
/// | Cmd | Name                                           |
/// |-----|------------------------------------------------|
/// | 0   | GetFinalOutputRecorderState                    |
/// | 1   | Start                                          |
/// | 2   | Stop                                           |
/// | 3   | AppendFinalOutputRecorderBuffer                |
/// | 4   | RegisterBufferEvent                            |
/// | 5   | GetReleasedFinalOutputRecorderBuffers          |
/// | 6   | ContainsFinalOutputRecorderBuffer              |
/// | 7   | GetFinalOutputRecorderBufferEndTime            |
/// | 8   | AppendFinalOutputRecorderBufferAuto            |
/// | 9   | GetReleasedFinalOutputRecorderBufferAuto       |
/// | 10  | FlushFinalOutputRecorderBuffers                |
/// | 11  | AttachWorkBuffer                               |
pub struct IFinalOutputRecorderManager {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IFinalOutputRecorderManager {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[(0, None, "OpenFinalOutputRecorder")]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for IFinalOutputRecorderManager {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IFinalOutputRecorderManager {
    fn get_service_name(&self) -> &str {
        "audrec:u"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
