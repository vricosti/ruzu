//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_file.h and fs_i_file.cpp
//!
//! IFile service.
//!
//! Mirrors upstream's `D<&IFile::Method>` dispatch + `Out<T>` / `InBuffer<T>` /
//! `OutBuffer<T>` signatures via the CMIF helpers (`CmifRequest`, `CmifResponse`).

use std::collections::BTreeMap;

use crate::file_sys::fs_file::{ReadOption, WriteOption};
use crate::file_sys::fsa::fs_i_file::IFile as FsaIFile;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::cmif_serialization::{CmifRequest, CmifResponse};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Number of u32 words for `Result + raw<T>` CMIF response.
const fn response_words<T>() -> u32 {
    2 + ((core::mem::size_of::<T>() as u32 + 3) / 4)
}

/// IPC command table for IFile:
///
/// | Cmd | Name                |
/// |-----|---------------------|
/// | 0   | Read                |
/// | 1   | Write               |
/// | 2   | Flush               |
/// | 3   | SetSize             |
/// | 4   | GetSize             |
/// | 5   | OperateRange        |
/// | 6   | OperateRangeWithBuffer |
pub struct IFile {
    backend: FsaIFile,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IFile {
    /// Construct an IFile from a VirtualFile, matching upstream
    /// `IFile::IFile(Core::System&, FileSys::VirtualFile)`.
    pub fn new(file: VirtualFile) -> Self {
        Self {
            backend: FsaIFile::new(file),
            handlers: build_handler_map(&[
                (0, Some(Self::read_handler), "Read"),
                (1, Some(Self::write_handler), "Write"),
                (2, Some(Self::flush_handler), "Flush"),
                (3, Some(Self::set_size_handler), "SetSize"),
                (4, Some(Self::get_size_handler), "GetSize"),
                (5, None, "OperateRange"),
                (6, None, "OperateRangeWithBuffer"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    // ---------------- Business-logic methods ----------------

    fn read(
        &self,
        option: ReadOption,
        out_size: &mut i64,
        offset: i64,
        out_buffer: &mut [u8],
        size: i64,
    ) -> ResultCode {
        log::debug!(
            "IFile::Read called, option={}, offset={:#x}, size={}",
            option.value,
            offset,
            size
        );
        let read_size = core::cmp::min(size as usize, out_buffer.len());
        match self.backend.read(offset, out_buffer, read_size, &option) {
            Ok(bytes_read) => {
                *out_size = bytes_read as i64;
                RESULT_SUCCESS
            }
            Err(rc) => ResultCode::new(rc.0),
        }
    }

    fn write(&self, buffer: &[u8], option: WriteOption, offset: i64, size: i64) -> ResultCode {
        log::debug!(
            "IFile::Write called, option={}, offset={:#x}, size={}",
            option.value,
            offset,
            size
        );
        let write_size = core::cmp::min(size as usize, buffer.len());
        match self
            .backend
            .write(offset, &buffer[..write_size], write_size, &option)
        {
            Ok(()) => RESULT_SUCCESS,
            Err(rc) => ResultCode::new(rc.0),
        }
    }

    fn flush(&self) -> ResultCode {
        log::debug!("IFile::Flush called");
        match self.backend.flush() {
            Ok(()) => RESULT_SUCCESS,
            Err(rc) => ResultCode::new(rc.0),
        }
    }

    fn set_size(&self, size: i64) -> ResultCode {
        log::debug!("IFile::SetSize called, size={}", size);
        match self.backend.set_size(size) {
            Ok(()) => RESULT_SUCCESS,
            Err(rc) => ResultCode::new(rc.0),
        }
    }

    fn get_size(&self, out_size: &mut i64) -> ResultCode {
        log::debug!("IFile::GetSize called");
        match self.backend.get_size() {
            Ok(size) => {
                *out_size = size;
                RESULT_SUCCESS
            }
            Err(rc) => ResultCode::new(rc.0),
        }
    }

    // ---------------- Per-cmd dispatch shims ----------------

    fn reply_result_only(ctx: &mut HLERequestContext, result: ResultCode) {
        let mut response = CmifResponse::result_only(ctx, result);
        let _ = &mut response;
    }

    fn read_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let option_raw = request.raw::<u32>();
        request.align_for::<i64>(); // align to next s64
        let offset = request.raw::<i64>();
        let size = request.raw::<i64>();
        let option = ReadOption { value: option_raw };
        log::trace!(
            "IFile::Read parsed option={} offset={:#x} size={}",
            option.value,
            offset,
            size
        );

        let buffer_size = ctx.get_write_buffer_size(0);
        let read_size = core::cmp::min(size as usize, buffer_size);
        let mut buffer = vec![0u8; read_size];
        let mut out_size: i64 = 0;
        let result = service.read(option, &mut out_size, offset, &mut buffer, size);
        if result == RESULT_SUCCESS {
            ctx.write_buffer(&buffer[..out_size as usize], 0);
            let mut response = CmifResponse::new(ctx, response_words::<i64>(), 0, 0);
            response.push_result(result);
            response.push_u64(out_size as u64);
        } else {
            Self::reply_result_only(ctx, result);
        }
    }

    fn write_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let option_raw = request.raw::<u32>();
        request.align_for::<i64>(); // align to next s64
        let offset = request.raw::<i64>();
        let size = request.raw::<i64>();
        let option = WriteOption { value: option_raw };

        let data = ctx.read_buffer(0);
        let result = service.write(&data, option, offset, size);
        Self::reply_result_only(ctx, result);
    }

    fn flush_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let result = service.flush();
        Self::reply_result_only(ctx, result);
    }

    fn set_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut request = CmifRequest::new(ctx);
        let size = request.raw::<i64>();
        let result = service.set_size(size);
        Self::reply_result_only(ctx, result);
    }

    fn get_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut out_size: i64 = 0;
        let result = service.get_size(&mut out_size);
        if result == RESULT_SUCCESS {
            let mut response = CmifResponse::new(ctx, response_words::<i64>(), 0, 0);
            response.push_result(result);
            response.push_u64(out_size as u64);
        } else {
            Self::reply_result_only(ctx, result);
        }
    }
}

impl SessionRequestHandler for IFile {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "fsp::IFile"
    }
}

impl ServiceFramework for IFile {
    fn get_service_name(&self) -> &str {
        "fsp::IFile"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
