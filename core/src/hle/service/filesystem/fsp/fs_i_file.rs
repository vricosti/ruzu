//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_file.h and fs_i_file.cpp
//!
//! IFile service.

use std::collections::BTreeMap;

use crate::file_sys::fs_file::{ReadOption, WriteOption};
use crate::file_sys::fsa::fs_i_file::IFile as FsaIFile;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

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

    /// Port of upstream IFile::Read.
    ///
    /// CMIF input params: ReadOption (u32), offset (s64), size (s64).
    /// CMIF output: out_size (s64), out_buffer (via buffer descriptor).
    fn read_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFile) };
        let mut rp = RequestParser::new(ctx);
        let option_raw = rp.pop_u32();
        rp.skip(1); // padding to align s64
        let offset = rp.pop_i64();
        let size = rp.pop_i64();

        let option = ReadOption { value: option_raw };
        log::debug!(
            "IFile::Read called, option={}, offset={:#x}, size={}",
            option.value,
            offset,
            size
        );

        let buffer_size = ctx.get_write_buffer_size(0);
        let read_size = std::cmp::min(size as usize, buffer_size);
        let mut output = vec![0u8; read_size];

        match service
            .backend
            .read(offset, &mut output, read_size, &option)
        {
            Ok(bytes_read) => {
                ctx.write_buffer(&output[..bytes_read], 0);
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_i64(bytes_read as i64);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(ResultCode::new(rc.0));
            }
        }
    }

    /// Port of upstream IFile::Write.
    ///
    /// CMIF input params: WriteOption (u32), offset (s64), size (s64).
    /// CMIF input buffer via buffer descriptor.
    fn write_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFile) };
        let mut rp = RequestParser::new(ctx);
        let option_raw = rp.pop_u32();
        rp.skip(1); // padding to align s64
        let offset = rp.pop_i64();
        let size = rp.pop_i64();

        let option = WriteOption { value: option_raw };
        log::debug!(
            "IFile::Write called, option={}, offset={:#x}, size={}",
            option.value,
            offset,
            size
        );

        let data = ctx.read_buffer(0);
        let write_size = std::cmp::min(size as usize, data.len());

        match service
            .backend
            .write(offset, &data[..write_size], write_size, &option)
        {
            Ok(()) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(ResultCode::new(rc.0));
            }
        }
    }

    /// Port of upstream IFile::Flush.
    fn flush_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFile) };
        log::debug!("IFile::Flush called");

        match service.backend.flush() {
            Ok(()) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(ResultCode::new(rc.0));
            }
        }
    }

    /// Port of upstream IFile::SetSize.
    fn set_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFile) };
        let mut rp = RequestParser::new(ctx);
        let size = rp.pop_i64();

        log::debug!("IFile::SetSize called, size={}", size);

        match service.backend.set_size(size) {
            Ok(()) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(ResultCode::new(rc.0));
            }
        }
    }

    /// Port of upstream IFile::GetSize.
    fn get_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IFile) };
        log::debug!("IFile::GetSize called");

        match service.backend.get_size() {
            Ok(size) => {
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_i64(size);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(ResultCode::new(rc.0));
            }
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
