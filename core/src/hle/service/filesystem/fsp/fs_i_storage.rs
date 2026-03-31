//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_storage.h and fs_i_storage.cpp
//!
//! IStorage service.

use std::collections::BTreeMap;

use crate::file_sys::vfs::vfs_types::VirtualFile;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IStorage:
///
/// | Cmd | Name         |
/// |-----|--------------|
/// | 0   | Read         |
/// | 1   | Write        |
/// | 2   | Flush        |
/// | 3   | SetSize      |
/// | 4   | GetSize      |
/// | 5   | OperateRange |
pub struct IStorage {
    backend: VirtualFile,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IStorage {
    pub fn new(backend: VirtualFile) -> Self {
        Self {
            backend,
            handlers: build_handler_map(&[
                (0, Some(Self::read_handler), "Read"),
                (1, Some(Self::stub_success_handler), "Write"),
                (2, Some(Self::stub_success_handler), "Flush"),
                (3, Some(Self::stub_success_handler), "SetSize"),
                (4, Some(Self::get_size_handler), "GetSize"),
                (5, Some(Self::stub_success_handler), "OperateRange"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn read_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let storage = unsafe { &*(this as *const dyn ServiceFramework as *const IStorage) };
        let mut rp = RequestParser::new(ctx);
        let offset = rp.pop_i64();
        let length = rp.pop_i64();

        log::debug!(
            "IStorage::Read called, offset=0x{:X}, length={}",
            offset,
            length
        );

        if offset < 0 || length < 0 {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_SUCCESS);
            return;
        }

        let output_size = ctx.get_write_buffer_size(0);
        let read_len = usize::min(length as usize, output_size);
        let data = storage.backend.read_bytes(read_len, offset as usize);

        // Write read data to the output buffer.
        // Upstream: ctx.WriteBuffer(output);
        let mut padded = data;
        if read_len > padded.len() {
            padded.resize(read_len, 0);
        }
        ctx.write_buffer(&padded, 0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let storage = unsafe { &*(this as *const dyn ServiceFramework as *const IStorage) };
        let size = storage.backend.get_size() as u64;

        log::debug!("IStorage::GetSize called, size={}", size);

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(size);
    }

    fn stub_success_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IStorage {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "IStorage"
    }
}

impl ServiceFramework for IStorage {
    fn get_service_name(&self) -> &str {
        "IStorage"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }

    fn invoke_request(&self, ctx: &mut HLERequestContext)
    where
        Self: Sized,
    {
        let cmd = ctx.get_command();
        if let Some(fi) = self.handlers().get(&cmd) {
            if let Some(callback) = fi.handler_callback {
                log::trace!("Service::{}: {}", self.get_service_name(), fi.name);
                callback(self, ctx);
                return;
            }
        }

        log::warn!(
            "IStorage: unimplemented command '{}' returned stub success",
            cmd
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}
