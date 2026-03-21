//! Port of zuyu/src/core/hle/service/hid/applet_resource.h and applet_resource.cpp
//!
//! IAppletResource service.

use std::collections::BTreeMap;
use std::sync::Arc;

use hid_core::resource_manager::ResourceManager;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IAppletResource:
///
/// | Cmd | Name                  |
/// |-----|-----------------------|
/// | 0   | GetSharedMemoryHandle |
pub struct IAppletResource {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    resource_manager: Arc<parking_lot::Mutex<ResourceManager>>,
    aruid: u64,
}

impl IAppletResource {
    /// Upstream: IAppletResource::GetSharedMemoryHandle
    /// Returns the shared memory handle for the applet resource.
    /// TODO: Full implementation requires kernel shared memory support.
    fn get_shared_memory_handle(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("(STUBBED) IAppletResource::GetSharedMemoryHandle called");

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    pub fn new(resource_manager: Arc<parking_lot::Mutex<ResourceManager>>, aruid: u64) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::get_shared_memory_handle), "GetSharedMemoryHandle"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            resource_manager,
            aruid,
        }
    }
}

impl SessionRequestHandler for IAppletResource {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "hid::IAppletResource"
    }
}

impl ServiceFramework for IAppletResource {
    fn get_service_name(&self) -> &str {
        "hid::IAppletResource"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
