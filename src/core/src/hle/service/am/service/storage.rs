// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of Eden's `core/hle/service/am/service/storage.{h,cpp}`.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::core::SystemRef;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::am::am_results;
use crate::hle::service::am::library_applet_storage::{
    BufferLibraryAppletStorage, LibraryAppletStorage,
};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::storage_accessor::{IStorageAccessor, ITransferStorageAccessor};

/// IPC command table for IStorage:
/// - 0: Open
/// - 1: OpenTransferStorage
pub struct IStorage {
    /// Matches upstream `Core::System& system`.
    system: SystemRef,
    /// Backing storage implementation.
    /// Matches upstream `std::shared_ptr<LibraryAppletStorage> m_impl`.
    backing: Arc<Mutex<dyn LibraryAppletStorage>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IStorage {
    pub fn new(data: Vec<u8>) -> Self {
        Self::new_with_system(SystemRef::null(), data)
    }

    pub fn new_with_system(system: SystemRef, data: Vec<u8>) -> Self {
        let backing: Arc<Mutex<dyn LibraryAppletStorage>> =
            Arc::new(Mutex::new(BufferLibraryAppletStorage::new(data)));
        Self::new_with_backing(system, backing)
    }

    pub fn new_with_backing(
        system: SystemRef,
        backing: Arc<Mutex<dyn LibraryAppletStorage>>,
    ) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::open_handler), "Open"),
            (
                1,
                Some(Self::open_transfer_storage_handler),
                "OpenTransferStorage",
            ),
        ]);
        Self {
            system,
            backing,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn get_data(&self) -> Vec<u8> {
        self.backing.lock().unwrap().get_data()
    }

    /// Port of `IStorage::Open`.
    fn open(&self) -> Result<Arc<IStorageAccessor>, ResultCode> {
        if self
            .backing
            .lock()
            .unwrap()
            .get_handle_object_id()
            .is_some()
        {
            return Err(am_results::RESULT_INVALID_STORAGE_TYPE);
        }
        Ok(Arc::new(IStorageAccessor::new(
            self.system,
            Arc::clone(&self.backing),
        )))
    }

    /// Port of `IStorage::OpenTransferStorage`.
    fn open_transfer_storage(&self) -> Result<Arc<ITransferStorageAccessor>, ResultCode> {
        if self
            .backing
            .lock()
            .unwrap()
            .get_handle_object_id()
            .is_none()
        {
            return Err(am_results::RESULT_INVALID_STORAGE_TYPE);
        }
        Ok(Arc::new(ITransferStorageAccessor::new(
            self.system,
            Arc::clone(&self.backing),
        )))
    }

    fn push_interface_response(
        ctx: &mut HLERequestContext,
        object: Arc<dyn SessionRequestHandler>,
    ) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(object);
    }

    /// Port of IStorage::Open
    /// Creates an IStorageAccessor from this storage and returns it.
    /// Upstream: `R_UNLESS(m_impl->GetHandle() == nullptr, AM::ResultInvalidStorageType);`
    fn open_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let storage = unsafe { &*(this as *const dyn ServiceFramework as *const IStorage) };
        log::debug!("IStorage::Open called");

        match storage.open() {
            Ok(accessor) => Self::push_interface_response(ctx, accessor),
            Err(result) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(result);
            }
        }
    }

    /// Port of IStorage::OpenTransferStorage
    /// Creates an ITransferStorageAccessor and returns it.
    /// Upstream: `R_UNLESS(m_impl->GetHandle() != nullptr, AM::ResultInvalidStorageType);`
    fn open_transfer_storage_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let storage = unsafe { &*(this as *const dyn ServiceFramework as *const IStorage) };
        log::debug!("IStorage::OpenTransferStorage called");

        match storage.open_transfer_storage() {
            Ok(accessor) => Self::push_interface_response(ctx, accessor),
            Err(result) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(result);
            }
        }
    }
}

impl SessionRequestHandler for IStorage {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl ServiceFramework for IStorage {
    fn get_service_name(&self) -> &str {
        "am::IStorage"
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
    fn buffer_storage_opens_only_the_regular_accessor() {
        let storage = IStorage::new_with_system(SystemRef::null(), vec![1, 2, 3]);

        assert!(storage.open().is_ok());
        assert!(matches!(
            storage.open_transfer_storage(),
            Err(result) if result == am_results::RESULT_INVALID_STORAGE_TYPE
        ));
    }
}
