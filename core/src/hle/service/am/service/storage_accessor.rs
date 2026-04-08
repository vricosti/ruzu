// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/storage_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/storage_accessor.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::am::library_applet_storage::LibraryAppletStorage;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

fn read_storage_for_out_buffer(
    backing: &dyn LibraryAppletStorage,
    offset: i64,
    out_buffer_size: usize,
) -> Result<Vec<u8>, ResultCode> {
    let mut data = vec![0u8; out_buffer_size];
    backing.read(offset, &mut data)?;
    Ok(data)
}

/// IStorageAccessor service.
///
/// Matches upstream `IStorageAccessor` which holds a `shared_ptr<LibraryAppletStorage>`.
pub struct IStorageAccessor {
    /// Backing storage implementation.
    /// Matches upstream `std::shared_ptr<LibraryAppletStorage> m_impl`.
    backing: Arc<Mutex<dyn LibraryAppletStorage>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IStorageAccessor {
    pub fn new(backing: Arc<Mutex<dyn LibraryAppletStorage>>) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::get_size_handler), "GetSize"),
            (10, Some(Self::write_handler), "Write"),
            (11, Some(Self::read_handler), "Read"),
        ]);
        Self {
            backing,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of IStorageAccessor::GetSize
    fn get_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let accessor =
            unsafe { &*(this as *const dyn ServiceFramework as *const IStorageAccessor) };
        let size = accessor.backing.lock().unwrap().get_size();
        log::debug!("IStorageAccessor::GetSize called, size={}", size);

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(size as u64);
    }

    /// Port of IStorageAccessor::Write
    fn write_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let accessor =
            unsafe { &*(this as *const dyn ServiceFramework as *const IStorageAccessor) };
        let mut rp = RequestParser::new(ctx);
        let offset = rp.pop_i64();
        let buffer = ctx.read_buffer(0);
        log::debug!(
            "IStorageAccessor::Write called, offset={} size={}",
            offset,
            buffer.len()
        );

        let result = accessor.backing.lock().unwrap().write(offset, &buffer);
        let rc = match result {
            Ok(()) => RESULT_SUCCESS,
            Err(e) => e,
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }

    /// Port of IStorageAccessor::Read
    fn read_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let accessor =
            unsafe { &*(this as *const dyn ServiceFramework as *const IStorageAccessor) };
        let mut rp = RequestParser::new(ctx);
        let offset = rp.pop_i64();
        let out_buffer_size = ctx.get_write_buffer_size(0);
        log::debug!(
            "IStorageAccessor::Read called, offset={} size={}",
            offset,
            out_buffer_size
        );

        let backing = accessor.backing.lock().unwrap();
        let result = read_storage_for_out_buffer(&*backing, offset, out_buffer_size);
        drop(backing);

        let rc = match result {
            Ok(data) => {
                ctx.write_buffer(&data, 0);
                RESULT_SUCCESS
            }
            Err(e) => e,
        };

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(rc);
    }
}

impl SessionRequestHandler for IStorageAccessor {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
}

impl ServiceFramework for IStorageAccessor {
    fn get_service_name(&self) -> &str {
        "am::IStorageAccessor"
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
    use super::read_storage_for_out_buffer;
    use crate::hle::service::am::am_results;
    use crate::hle::service::am::library_applet_storage::BufferLibraryAppletStorage;

    #[test]
    fn storage_accessor_read_uses_exact_output_buffer_size() {
        let backing = BufferLibraryAppletStorage::new(vec![1, 2, 3, 4, 5, 6, 7, 8]);
        let data = read_storage_for_out_buffer(&backing, 2, 3).unwrap();
        assert_eq!(data, vec![3, 4, 5]);
    }

    #[test]
    fn storage_accessor_read_fails_when_output_buffer_exceeds_backing() {
        let backing = BufferLibraryAppletStorage::new(vec![1, 2, 3, 4]);
        let result = read_storage_for_out_buffer(&backing, 2, 3);
        assert_eq!(result.unwrap_err(), am_results::RESULT_INVALID_OFFSET);
    }
}

/// ITransferStorageAccessor service.
///
/// Matches upstream `ITransferStorageAccessor` which holds a `shared_ptr<LibraryAppletStorage>`.
pub struct ITransferStorageAccessor {
    /// Backing storage implementation.
    backing: Arc<Mutex<dyn LibraryAppletStorage>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ITransferStorageAccessor {
    pub fn new(backing: Arc<Mutex<dyn LibraryAppletStorage>>) -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::get_size_handler), "GetSize"),
            (1, None, "GetHandle"), // Needs KTransferMemory support
        ]);
        Self {
            backing,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of ITransferStorageAccessor::GetSize
    fn get_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let accessor =
            unsafe { &*(this as *const dyn ServiceFramework as *const ITransferStorageAccessor) };
        let size = accessor.backing.lock().unwrap().get_size();
        log::debug!("ITransferStorageAccessor::GetSize called, size={}", size);

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(size as u64);
    }
}

impl SessionRequestHandler for ITransferStorageAccessor {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
}

impl ServiceFramework for ITransferStorageAccessor {
    fn get_service_name(&self) -> &str {
        "am::ITransferStorageAccessor"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
