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
    ///
    /// Upstream calls resource_manager->GetSharedMemoryHandle(out_shared_memory_handle, aruid)
    /// which returns a KSharedMemory* via the applet_resource subsystem.
    ///
    /// Full implementation requires:
    /// 1. ResourceManager::get_shared_memory_handle(aruid) -> KSharedMemory
    /// 2. KSharedMemory to be returned as a copy handle in the IPC response
    ///
    /// Currently stubbed: KSharedMemory handle plumbing through IPC response
    /// copy handles is not yet wired. The response returns success without a
    /// handle, which allows the service to be instantiated without crashing.
    fn get_shared_memory_handle(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IAppletResource) };
        log::debug!(
            "(STUBBED) IAppletResource::GetSharedMemoryHandle called, aruid={}",
            service.aruid
        );

        // TODO: Wire KSharedMemory handle into IPC response copy handles.
        // Upstream: resource_manager->GetSharedMemoryHandle(&out_handle, aruid)
        // The copy handle should be added via ctx.add_copy_object() once
        // KSharedMemory IPC handle support is implemented.
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
