// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/bcat_types.h

use crate::hle::result::ResultCode;

pub type DirectoryName = [u8; 0x20];
pub type FileName = [u8; 0x20];
pub type BcatDigest = [u8; 0x10];
pub type Passphrase = [u8; 0x20];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SyncType {
    Normal = 0,
    Directory = 1,
    Count = 2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum DeliveryCacheProgressStatus {
    None = 0x0,
    Queued = 0x1,
    Connecting = 0x2,
    ProcessingDataList = 0x3,
    Downloading = 0x4,
    Committing = 0x5,
    Done = 0x9,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DeliveryCacheDirectoryEntry {
    pub name: FileName,
    pub size: u64,
    pub digest: BcatDigest,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct TitleIdVersion {
    pub title_id: u64,
    pub build_id: u64,
}

#[derive(Clone)]
#[repr(C)]
pub struct DeliveryCacheProgressImpl {
    pub status: DeliveryCacheProgressStatus,
    pub result: ResultCode,
    pub current_directory: DirectoryName,
    pub current_file: FileName,
    pub current_downloaded_bytes: i64,
    pub current_total_bytes: i64,
    pub total_downloaded_bytes: i64,
    pub total_bytes: i64,
    pub _reserved: [u8; 0x198],
}

const _: () = assert!(core::mem::size_of::<DeliveryCacheProgressImpl>() == 0x200);

impl Default for DeliveryCacheProgressImpl {
    fn default() -> Self {
        // SAFETY: All-zeros is valid for this repr(C) struct
        unsafe { core::mem::zeroed() }
    }
}
