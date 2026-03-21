// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_file_service.h
//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_file_service.cpp

use std::collections::BTreeMap;

use crate::file_sys::vfs::vfs::{VfsDirectory, VfsFile};
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::bcat_result;
use super::bcat_types::*;

/// IPC command IDs for IDeliveryCacheFileService
pub mod commands {
    pub const OPEN: u32 = 0;
    pub const READ: u32 = 1;
    pub const GET_SIZE: u32 = 2;
    pub const GET_DIGEST: u32 = 3;
}

/// IDeliveryCacheFileService corresponds to upstream `IDeliveryCacheFileService`.
pub struct IDeliveryCacheFileService {
    root: VirtualDir,
    current_file: Option<VirtualFile>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IDeliveryCacheFileService {
    pub fn new(root: VirtualDir) -> Self {
        let handlers = build_handler_map(&[
            (commands::OPEN, None, "Open"),
            (commands::READ, None, "Read"),
            (commands::GET_SIZE, None, "GetSize"),
            (commands::GET_DIGEST, None, "GetDigest"),
        ]);

        Self {
            root,
            current_file: None,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn open(&mut self, dir_name_raw: &DirectoryName, file_name_raw: &FileName) -> ResultCode {
        let dir_name = common::string_util::string_from_fixed_zero_terminated_buffer(
            dir_name_raw,
            dir_name_raw.len(),
        );
        let file_name = common::string_util::string_from_fixed_zero_terminated_buffer(
            file_name_raw,
            file_name_raw.len(),
        );

        log::debug!(
            "IDeliveryCacheFileService::open called, dir_name={}, file_name={}",
            dir_name,
            file_name
        );

        let dir_verify = super::bcat_util::verify_name_valid_dir(dir_name_raw);
        if dir_verify.is_error() {
            return dir_verify;
        }
        // Note: upstream also calls VerifyNameValidDir on file_name_raw (not VerifyNameValidFile)
        let file_verify = super::bcat_util::verify_name_valid_dir(file_name_raw);
        if file_verify.is_error() {
            return file_verify;
        }
        if self.current_file.is_some() {
            return bcat_result::RESULT_ENTITY_ALREADY_OPEN;
        }

        let dir = match self.root.get_subdirectory(&dir_name) {
            Some(d) => d,
            None => return bcat_result::RESULT_FAILED_OPEN_ENTITY,
        };

        self.current_file = match dir.get_file(&file_name) {
            Some(f) => Some(f),
            None => return bcat_result::RESULT_FAILED_OPEN_ENTITY,
        };

        RESULT_SUCCESS
    }

    pub fn read(&self, offset: u64, out_buffer: &mut [u8]) -> (ResultCode, u64) {
        log::debug!(
            "IDeliveryCacheFileService::read called, offset={:016X}, size={:016X}",
            offset,
            out_buffer.len()
        );

        let current_file = match &self.current_file {
            Some(f) => f,
            None => return (bcat_result::RESULT_NO_OPEN_ENTRY, 0),
        };

        let file_size = current_file.get_size() as u64;
        let read_size = std::cmp::min(file_size.saturating_sub(offset), out_buffer.len() as u64);
        let buffer = current_file.read_bytes(read_size as usize, offset as usize);
        out_buffer[..buffer.len()].copy_from_slice(&buffer);

        (RESULT_SUCCESS, read_size)
    }

    pub fn get_size(&self) -> (ResultCode, u64) {
        log::debug!("IDeliveryCacheFileService::get_size called");

        match &self.current_file {
            Some(f) => (RESULT_SUCCESS, f.get_size() as u64),
            None => (bcat_result::RESULT_NO_OPEN_ENTRY, 0),
        }
    }

    pub fn get_digest(&self) -> (ResultCode, BcatDigest) {
        log::debug!("IDeliveryCacheFileService::get_digest called");

        if self.current_file.is_none() {
            return (bcat_result::RESULT_NO_OPEN_ENTRY, [0u8; 0x10]);
        }

        // Upstream has this commented out: //*out_digest = DigestFile(current_file);
        // So we return an empty digest matching upstream behavior.
        (RESULT_SUCCESS, [0u8; 0x10])
    }
}

impl SessionRequestHandler for IDeliveryCacheFileService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "IDeliveryCacheFileService"
    }
}

impl ServiceFramework for IDeliveryCacheFileService {
    fn get_service_name(&self) -> &str {
        "IDeliveryCacheFileService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
