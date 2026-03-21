//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_multi_commit_manager.h and .cpp
//!
//! IMultiCommitManager service.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IMultiCommitManager:
///
/// | Cmd | Name   |
/// |-----|--------|
/// | 1   | Add    |
/// | 2   | Commit |
pub struct IMultiCommitManager {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IMultiCommitManager {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (1, Some(Self::add_handler), "Add"),
                (2, Some(Self::commit_handler), "Commit"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of upstream IMultiCommitManager::Add (stubbed — returns success).
    fn add_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) IMultiCommitManager::Add");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Port of upstream IMultiCommitManager::Commit (stubbed — returns success).
    fn commit_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) IMultiCommitManager::Commit");
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IMultiCommitManager {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "fsp::IMultiCommitManager"
    }
}

impl ServiceFramework for IMultiCommitManager {
    fn get_service_name(&self) -> &str {
        "fsp::IMultiCommitManager"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
