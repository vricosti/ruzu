// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_file_service.h
//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_file_service.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
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
    // TODO: root: VirtualDir, current_file: Option<VirtualFile>
    pub has_open_file: bool,
    pub file_size: u64,
}

impl IDeliveryCacheFileService {
    pub fn new() -> Self {
        Self {
            has_open_file: false,
            file_size: 0,
        }
    }

    pub fn open(&mut self, dir_name_raw: &DirectoryName, file_name_raw: &FileName) -> ResultCode {
        log::debug!("IDeliveryCacheFileService::open called");

        let dir_verify = super::bcat_util::verify_name_valid_dir(dir_name_raw);
        if dir_verify.is_error() {
            return dir_verify;
        }
        // Note: upstream also calls VerifyNameValidDir on file_name_raw (not VerifyNameValidFile)
        let file_verify = super::bcat_util::verify_name_valid_dir(file_name_raw);
        if file_verify.is_error() {
            return file_verify;
        }
        if self.has_open_file {
            return bcat_result::RESULT_ENTITY_ALREADY_OPEN;
        }
        // TODO: look up directory and file in VFS
        bcat_result::RESULT_FAILED_OPEN_ENTITY
    }

    pub fn read(&self, _offset: u64, out_buffer: &mut [u8]) -> (ResultCode, u64) {
        log::debug!("IDeliveryCacheFileService::read called");
        if !self.has_open_file {
            return (bcat_result::RESULT_NO_OPEN_ENTRY, 0);
        }
        // TODO: read from current_file
        let _ = out_buffer;
        (RESULT_SUCCESS, 0)
    }

    pub fn get_size(&self) -> (ResultCode, u64) {
        log::debug!("IDeliveryCacheFileService::get_size called");
        if !self.has_open_file {
            return (bcat_result::RESULT_NO_OPEN_ENTRY, 0);
        }
        (RESULT_SUCCESS, self.file_size)
    }

    pub fn get_digest(&self) -> (ResultCode, BcatDigest) {
        log::debug!("IDeliveryCacheFileService::get_digest called");
        if !self.has_open_file {
            return (bcat_result::RESULT_NO_OPEN_ENTRY, [0u8; 0x10]);
        }
        // TODO: compute MD5 digest of current_file
        (RESULT_SUCCESS, [0u8; 0x10])
    }
}
