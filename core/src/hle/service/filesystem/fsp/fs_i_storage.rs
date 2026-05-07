//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_storage.h and fs_i_storage.cpp
//!
//! IStorage service.
//!
//! Mirrors upstream's `D<&IStorage::Method>` dispatch + `Out<T>` / `OutBuffer<T>`
//! signatures via the CMIF helpers (`CmifRequest`, `CmifResponse`).

use std::collections::BTreeMap;

use crate::file_sys::errors;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::cmif_serialization::{CmifRequest, CmifResponse};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

#[inline]
fn to_ipc_result(r: common::ResultCode) -> ResultCode {
    ResultCode::new(r.raw())
}

/// Number of u32 words for `Result + raw<T>` CMIF response.
const fn response_words<T>() -> u32 {
    2 + ((core::mem::size_of::<T>() as u32 + 3) / 4)
}

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

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    // ---------------- Business-logic methods ----------------
    // Mirror upstream `Result IStorage::Read(OutBuffer, s64, s64)` and
    // `Result IStorage::GetSize(Out<u64>)`.

    fn read(&self, out_bytes: &mut [u8], offset: i64, length: i64) -> ResultCode {
        let backend_size = self.backend.get_size();
        log::debug!(
            "IStorage::Read called, offset=0x{:X}, length={}, backend_size={}",
            offset,
            length,
            backend_size
        );
        if length < 0 {
            return to_ipc_result(errors::RESULT_INVALID_SIZE);
        }
        if offset < 0 {
            return to_ipc_result(errors::RESULT_INVALID_OFFSET);
        }
        let bytes_read = self
            .backend
            .read(out_bytes, length as usize, offset as usize);
        if bytes_read != length as usize {
            log::warn!(
                "IStorage::Read short read, offset=0x{:X}, requested={}, read={}, backend_size={}",
                offset,
                length,
                bytes_read,
                backend_size
            );
        }
        // Investigation hook — mirrors zuyu's ZUYU_ISTORAGE_READ_DUMP.
        if std::env::var_os("RUZU_ISTORAGE_READ_DUMP").is_some() {
            use std::fmt::Write;
            let n = out_bytes.len().min(64);
            let mut hex = String::new();
            for b in &out_bytes[..n] {
                let _ = write!(hex, "{:02x}", b);
            }
            log::warn!(
                "IStorage::Read offset=0x{:X} length={} backend_size={} first{}B={}",
                offset,
                length,
                backend_size,
                n,
                hex
            );
        }
        RESULT_SUCCESS
    }

    fn get_size(&self, out_size: &mut u64) -> ResultCode {
        *out_size = self.backend.get_size() as u64;
        log::debug!("IStorage::GetSize called, size={}", *out_size);
        RESULT_SUCCESS
    }

    // ---------------- Per-cmd dispatch shims ----------------

    fn reply_result_only(ctx: &mut HLERequestContext, result: ResultCode) {
        let mut response = CmifResponse::result_only(ctx, result);
        let _ = &mut response;
    }

    fn read_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let storage = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let offset = request.raw::<i64>();
        let length = request.raw::<i64>();
        let buffer_size = ctx.get_write_buffer_size(0);
        let read_size = if length >= 0 {
            core::cmp::min(length as usize, buffer_size)
        } else {
            0
        };
        let mut buffer = vec![0u8; read_size];
        let result = storage.read(&mut buffer, offset, length);
        if result == RESULT_SUCCESS {
            ctx.write_buffer(&buffer, 0);
        }
        Self::reply_result_only(ctx, result);
    }

    fn get_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let storage = Self::as_self(this);
        let mut size: u64 = 0;
        let result = storage.get_size(&mut size);
        let mut response = CmifResponse::new(ctx, response_words::<u64>(), 0, 0);
        response.push_result(result);
        response.push_u64(size);
    }

    fn stub_success_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        Self::reply_result_only(ctx, RESULT_SUCCESS);
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
