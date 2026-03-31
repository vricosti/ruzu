// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_directory_service.h
//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_directory_service.cpp

use std::collections::BTreeMap;

use super::bcat_result;
use super::bcat_types::*;
use crate::file_sys::vfs::vfs::{VfsDirectory, VfsFile};
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// The digest is only used to determine if a file is unique compared to others of the same name.
/// Since the algorithm isn't ever checked in game, MD5 is safe.
fn digest_file(file: &VirtualFile) -> BcatDigest {
    use md5::Digest;
    let mut out = BcatDigest::default();
    let bytes = file.read_all_bytes();
    // Upstream uses mbedtls_md5_ret. We use the md-5 crate equivalent.
    let result = md5::Md5::digest(&bytes);
    out.copy_from_slice(&result);
    out
}

/// IPC command IDs for IDeliveryCacheDirectoryService
pub mod commands {
    pub const OPEN: u32 = 0;
    pub const READ: u32 = 1;
    pub const GET_COUNT: u32 = 2;
}

/// IDeliveryCacheDirectoryService corresponds to upstream `IDeliveryCacheDirectoryService`.
pub struct IDeliveryCacheDirectoryService {
    root: VirtualDir,
    current_dir: Option<VirtualDir>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDeliveryCacheDirectoryService {
    pub fn new(root: VirtualDir) -> Self {
        let handlers = build_handler_map(&[
            (commands::OPEN, None, "Open"),
            (commands::READ, None, "Read"),
            (commands::GET_COUNT, None, "GetCount"),
        ]);

        Self {
            root,
            current_dir: None,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn open(&mut self, dir_name_raw: &DirectoryName) -> ResultCode {
        let dir_name = common::string_util::string_from_fixed_zero_terminated_buffer(
            dir_name_raw,
            dir_name_raw.len(),
        );

        log::debug!(
            "IDeliveryCacheDirectoryService::open called, dir_name={}",
            dir_name
        );

        let dir_verify = super::bcat_util::verify_name_valid_dir(dir_name_raw);
        if dir_verify.is_error() {
            return dir_verify;
        }
        if self.current_dir.is_some() {
            return bcat_result::RESULT_ENTITY_ALREADY_OPEN;
        }

        let dir = match self.root.get_subdirectory(&dir_name) {
            Some(d) => d,
            None => return bcat_result::RESULT_FAILED_OPEN_ENTITY,
        };

        self.current_dir = Some(dir);

        RESULT_SUCCESS
    }

    pub fn read(&self, out_buffer: &mut [DeliveryCacheDirectoryEntry]) -> (ResultCode, i32) {
        log::debug!(
            "IDeliveryCacheDirectoryService::read called, write_size={:016X}",
            out_buffer.len()
        );

        let current_dir = match &self.current_dir {
            Some(d) => d,
            None => return (bcat_result::RESULT_NO_OPEN_ENTRY, 0),
        };

        let files = current_dir.get_files();
        let count = std::cmp::min(files.len(), out_buffer.len()) as i32;

        for i in 0..count as usize {
            let file = &files[i];
            let mut name = FileName::default();
            let file_name = file.get_name();
            let file_name_bytes = file_name.as_bytes();
            let copy_len = std::cmp::min(file_name_bytes.len(), name.len());
            name[..copy_len].copy_from_slice(&file_name_bytes[..copy_len]);

            out_buffer[i] = DeliveryCacheDirectoryEntry {
                name,
                size: file.get_size() as u64,
                digest: digest_file(file),
            };
        }

        (RESULT_SUCCESS, count)
    }

    pub fn get_count(&self) -> (ResultCode, i32) {
        log::debug!("IDeliveryCacheDirectoryService::get_count called");

        match &self.current_dir {
            Some(d) => (RESULT_SUCCESS, d.get_files().len() as i32),
            None => (bcat_result::RESULT_NO_OPEN_ENTRY, 0),
        }
    }
}

impl SessionRequestHandler for IDeliveryCacheDirectoryService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IDeliveryCacheDirectoryService"
    }
}

impl ServiceFramework for IDeliveryCacheDirectoryService {
    fn get_service_name(&self) -> &str {
        "IDeliveryCacheDirectoryService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
