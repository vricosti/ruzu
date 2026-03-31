//! Port of zuyu/src/core/hle/service/hid/applet_resource.h and applet_resource.cpp
//!
//! IAppletResource service.

use std::collections::BTreeMap;
use std::sync::Arc;

use hid_core::resource_manager::ResourceManager;
use hid_core::resources::shared_memory_format::SharedMemoryFormat;

use crate::core::SystemRef;
use crate::hle::kernel::k_shared_memory::{KSharedMemory, MemoryPermission};
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
    system: SystemRef,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    resource_manager: Arc<parking_lot::Mutex<ResourceManager>>,
    aruid: u64,
    shared_memory: parking_lot::Mutex<Option<(u64, Arc<KSharedMemory>)>>,
}

impl IAppletResource {
    fn create_shared_memory_object(
        &self,
        ctx: &HLERequestContext,
    ) -> Option<(u64, Arc<KSharedMemory>)> {
        let snapshot = {
            let resource_manager = self.resource_manager.lock();
            resource_manager.get_shared_memory_handle(self.aruid).ok()?;
            let applet_resource = resource_manager.get_applet_resource()?;
            let applet_resource = applet_resource.lock();
            let shared_memory = applet_resource.get_shared_memory_format(self.aruid)?;

            let size = std::mem::size_of::<SharedMemoryFormat>();
            let mut snapshot = vec![0u8; size];
            unsafe {
                std::ptr::copy_nonoverlapping(
                    shared_memory as *const SharedMemoryFormat as *const u8,
                    snapshot.as_mut_ptr(),
                    size,
                );
            }
            snapshot
        };

        let system_ptr =
            self.system.get() as *const crate::core::System as *mut crate::core::System;
        let device_memory_ptr = unsafe { (*system_ptr).device_memory() as *const _ };
        let kernel = unsafe { (*system_ptr).kernel_mut()? };

        let mut shared_memory = KSharedMemory::new();
        if shared_memory
            .initialize(
                unsafe { &*device_memory_ptr },
                kernel.memory_manager_mut(),
                MemoryPermission::None,
                MemoryPermission::Read,
                std::mem::size_of::<SharedMemoryFormat>(),
            )
            .is_error()
        {
            return None;
        }

        let dst = shared_memory.get_pointer_mut(0);
        if dst.is_null() {
            return None;
        }

        unsafe {
            std::ptr::copy_nonoverlapping(snapshot.as_ptr(), dst, snapshot.len());
        }

        let object_id = kernel.create_new_object_id() as u64;
        Some((object_id, Arc::new(shared_memory)))
    }

    /// Upstream: IAppletResource::GetSharedMemoryHandle
    /// Returns the shared memory handle for the applet resource.
    fn get_shared_memory_handle(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IAppletResource) };
        let handle = (|| -> Option<u32> {
            let thread = ctx.get_thread()?;
            let parent = thread.lock().unwrap().parent.as_ref()?.upgrade()?;

            let (object_id, shared_memory) = {
                let mut cached = service.shared_memory.lock();
                if cached.is_none() {
                    *cached = service.create_shared_memory_object(ctx);
                }
                cached.as_ref()?.clone()
            };

            let mut process = parent.lock().unwrap();
            process.register_shared_memory_object(object_id, shared_memory);
            process.handle_table.add(object_id).ok()
        })();

        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_objects(handle.unwrap_or(0));
    }

    pub fn new(
        system: SystemRef,
        resource_manager: Arc<parking_lot::Mutex<ResourceManager>>,
        aruid: u64,
    ) -> Self {
        let handlers = build_handler_map(&[(
            0,
            Some(Self::get_shared_memory_handle),
            "GetSharedMemoryHandle",
        )]);

        Self {
            system,
            handlers,
            handlers_tipc: BTreeMap::new(),
            resource_manager,
            aruid,
            shared_memory: parking_lot::Mutex::new(None),
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

impl Drop for IAppletResource {
    fn drop(&mut self) {
        self.resource_manager
            .lock()
            .free_applet_resource_id(self.aruid);
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
