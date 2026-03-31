//! Port of zuyu/src/core/hle/service/filesystem/fsp/fs_i_save_data_info_reader.h and .cpp
//!
//! ISaveDataInfoReader service.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Port of ISaveDataInfoReader::SaveDataInfo
/// sizeof = 0x60
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SaveDataInfo {
    pub save_id_unknown: u64,
    pub space: u8,
    pub save_type: u8,
    pub _padding0: [u8; 6],
    pub user_id: [u8; 0x10],
    pub save_id: u64,
    pub title_id: u64,
    pub save_image_size: u64,
    pub index: u16,
    pub rank: u8,
    pub _padding1: [u8; 0x25],
}

const _: () = assert!(std::mem::size_of::<SaveDataInfo>() == 0x60);

impl Default for SaveDataInfo {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// IPC command table for ISaveDataInfoReader:
///
/// | Cmd | Name              |
/// |-----|-------------------|
/// | 0   | ReadSaveDataInfo  |
pub struct ISaveDataInfoReader {
    _info: Vec<SaveDataInfo>,
    _next_entry_index: u64,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISaveDataInfoReader {
    pub fn new() -> Self {
        Self {
            _info: Vec::new(),
            _next_entry_index: 0,
            handlers: build_handler_map(&[(
                0,
                Some(Self::read_save_data_info_handler),
                "ReadSaveDataInfo",
            )]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of upstream ISaveDataInfoReader::ReadSaveDataInfo.
    ///
    /// Returns save data entries to the caller. Currently stubbed to return 0 entries,
    /// which is safe for initial boot (no save data found).
    fn read_save_data_info_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::warn!("(STUBBED) ISaveDataInfoReader::ReadSaveDataInfo returning 0 entries");
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_i64(0); // out_count = 0
    }
}

impl SessionRequestHandler for ISaveDataInfoReader {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        "fsp::ISaveDataInfoReader"
    }
}

impl ServiceFramework for ISaveDataInfoReader {
    fn get_service_name(&self) -> &str {
        "fsp::ISaveDataInfoReader"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
