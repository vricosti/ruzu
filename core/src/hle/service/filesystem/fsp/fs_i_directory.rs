//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_directory.h and fs_i_directory.cpp
//!
//! IDirectory service.

use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::file_sys::fsa::fs_i_directory::IDirectory as FsaIDirectory;
use crate::file_sys::fs_directory::DirectoryEntry;
use crate::file_sys::fs_filesystem::OpenDirectoryMode;
use crate::file_sys::vfs::vfs_types::VirtualDir;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IDirectory:
///
/// | Cmd | Name            |
/// |-----|-----------------|
/// | 0   | Read            |
/// | 1   | GetEntryCount   |
pub struct IDirectory {
    backend: Mutex<FsaIDirectory>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDirectory {
    /// Construct an IDirectory from a VirtualDir and open mode, matching upstream
    /// `IDirectory::IDirectory(Core::System&, FileSys::VirtualDir, FileSys::OpenDirectoryMode)`.
    pub fn new(directory: VirtualDir, mode: OpenDirectoryMode) -> Self {
        Self {
            backend: Mutex::new(FsaIDirectory::new(directory, mode)),
            handlers: build_handler_map(&[
                (0, Some(Self::read_handler), "Read"),
                (1, Some(Self::get_entry_count_handler), "GetEntryCount"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of upstream IDirectory::Read.
    ///
    /// Reads directory entries into the output buffer. Returns the number of entries read.
    fn read_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IDirectory) };
        log::debug!("IDirectory::Read called");

        let buffer_size = ctx.get_write_buffer_size(0);
        let entry_size = std::mem::size_of::<DirectoryEntry>();
        let max_entries = if entry_size > 0 {
            buffer_size / entry_size
        } else {
            0
        };

        let mut backend = service.backend.lock().unwrap();
        match backend.read(max_entries as i64) {
            Ok(entries) => {
                let count = entries.len();
                if count > 0 {
                    // Serialize entries as raw bytes into the output buffer.
                    let byte_len = count * entry_size;
                    let mut output = vec![0u8; byte_len];
                    unsafe {
                        std::ptr::copy_nonoverlapping(
                            entries.as_ptr() as *const u8,
                            output.as_mut_ptr(),
                            byte_len,
                        );
                    }
                    ctx.write_buffer(&output, 0);
                }
                let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                rb.push_i64(count as i64);
            }
            Err(rc) => {
                let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(ResultCode::new(rc.0));
            }
        }
    }

    /// Port of upstream IDirectory::GetEntryCount.
    fn get_entry_count_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const IDirectory) };
        log::debug!("IDirectory::GetEntryCount called");

        let backend = service.backend.lock().unwrap();
        let count = backend.get_entry_count();

        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_i64(count);
    }
}

impl SessionRequestHandler for IDirectory {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "fsp::IDirectory"
    }
}

impl ServiceFramework for IDirectory {
    fn get_service_name(&self) -> &str {
        "fsp::IDirectory"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
