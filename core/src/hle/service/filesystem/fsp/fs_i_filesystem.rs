//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_filesystem.h and .cpp
//!
//! IFileSystem service.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IFileSystem:
///
/// | Cmd | Name                        |
/// |-----|-----------------------------|
/// | 0   | CreateFile                  |
/// | 1   | DeleteFile                  |
/// | 2   | CreateDirectory             |
/// | 3   | DeleteDirectory             |
/// | 4   | DeleteDirectoryRecursively  |
/// | 5   | RenameFile                  |
/// | 6   | RenameDirectory             |
/// | 7   | GetEntryType                |
/// | 8   | OpenFile                    |
/// | 9   | OpenDirectory               |
/// | 10  | Commit                      |
/// | 11  | GetFreeSpaceSize            |
/// | 12  | GetTotalSpaceSize           |
/// | 13  | CleanDirectoryRecursively   |
/// | 14  | GetFileTimeStampRaw         |
/// | 15  | QueryEntry                  |
/// | 16  | GetFileSystemAttribute      |
pub struct IFileSystem {
    // backend: FileSys::Fsa::IFileSystem,
    // size_getter: SizeGetter,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IFileSystem {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (10, Some(Self::commit_handler), "Commit"),
            (11, Some(Self::get_free_space_size_handler), "GetFreeSpaceSize"),
            (12, Some(Self::get_total_space_size_handler), "GetTotalSpaceSize"),
        ]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn commit_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_free_space_size_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(0);
    }

    fn get_total_space_size_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(0);
    }
}

impl SessionRequestHandler for IFileSystem {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "IFileSystem"
    }
}

impl ServiceFramework for IFileSystem {
    fn get_service_name(&self) -> &str {
        "IFileSystem"
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
            "IFileSystem: unimplemented command '{}' returned stub success",
            cmd
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}
