//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_directory.h and fs_i_directory.cpp
//!
//! IDirectory service.
//!
//! Mirrors upstream's `D<&IDirectory::Method>` dispatch + `Out<T>` / `OutArray<T>`
//! signatures via the CMIF helpers (`CmifResponse`).

use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::file_sys::fs_directory::DirectoryEntry;
use crate::file_sys::fs_filesystem::OpenDirectoryMode;
use crate::file_sys::fsa::fs_i_directory::IDirectory as FsaIDirectory;
use crate::file_sys::vfs::vfs_types::VirtualDir;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::cmif_serialization::CmifResponse;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Number of u32 words for `Result + raw<T>` CMIF response.
const fn response_words<T>() -> u32 {
    2 + ((core::mem::size_of::<T>() as u32 + 3) / 4)
}

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

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    // ---------------- Business-logic methods ----------------
    // Mirrors upstream's
    //   Result Read(Out<s64> out_count, OutArray<DirectoryEntry, ...> out_entries)
    //   Result GetEntryCount(Out<s64> out_count)

    fn read(&self, out_count: &mut i64, out_entries: &mut [u8], max_entries: usize) -> ResultCode {
        log::debug!("IDirectory::Read called");
        let entry_size = core::mem::size_of::<DirectoryEntry>();
        let mut backend = self.backend.lock().unwrap();
        match backend.read(max_entries as i64) {
            Ok(entries) => {
                let count = entries.len();
                if count > 0 {
                    let byte_len = count * entry_size;
                    let dst = &mut out_entries[..byte_len];
                    unsafe {
                        core::ptr::copy_nonoverlapping(
                            entries.as_ptr() as *const u8,
                            dst.as_mut_ptr(),
                            byte_len,
                        );
                    }
                }
                *out_count = count as i64;
                RESULT_SUCCESS
            }
            Err(rc) => ResultCode::new(rc.0),
        }
    }

    fn get_entry_count(&self, out_count: &mut i64) -> ResultCode {
        log::debug!("IDirectory::GetEntryCount called");
        *out_count = self.backend.lock().unwrap().get_entry_count();
        RESULT_SUCCESS
    }

    // ---------------- Per-cmd dispatch shims ----------------

    fn read_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let buffer_size = ctx.get_write_buffer_size(0);
        let entry_size = core::mem::size_of::<DirectoryEntry>();
        let max_entries = if entry_size > 0 {
            buffer_size / entry_size
        } else {
            0
        };

        let mut out_count: i64 = 0;
        let mut buffer = vec![0u8; max_entries * entry_size];
        let result = service.read(&mut out_count, &mut buffer, max_entries);
        if result == RESULT_SUCCESS && out_count > 0 {
            let used = (out_count as usize) * entry_size;
            ctx.write_buffer(&buffer[..used], 0);
        }
        let mut response = CmifResponse::new(ctx, response_words::<i64>(), 0, 0);
        response.push_result(result);
        response.push_u64(out_count as u64);
    }

    fn get_entry_count_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = Self::as_self(this);
        let mut out_count: i64 = 0;
        let result = service.get_entry_count(&mut out_count);
        let mut response = CmifResponse::new(ctx, response_words::<i64>(), 0, 0);
        response.push_result(result);
        response.push_u64(out_count as u64);
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
