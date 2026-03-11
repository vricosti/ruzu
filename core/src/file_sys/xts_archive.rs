// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/xts_archive.h / .cpp
// NAX (NCA Archive with XTS encryption) format.

use super::vfs::vfs_types::{VirtualDir, VirtualFile};

/// NAX header — 0x80 bytes.
#[derive(Clone)]
#[repr(C)]
pub struct NaxHeader {
    pub hmac: [u8; 0x20],
    pub magic: u64,
    pub key_area: [[u8; 0x10]; 2],
    pub file_size: u64,
    pub _padding: [u8; 0x30],
}
const _: () = assert!(std::mem::size_of::<NaxHeader>() == 0x80);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NaxContentType {
    Save = 0,
    Nca = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NaxStatus {
    Success,
    ErrorBadMagic,
    ErrorBadHmac,
    ErrorKeyDerivation,
    ErrorDecryption,
}

pub struct Nax {
    file: VirtualFile,
    status: NaxStatus,
    content_type: NaxContentType,
    dec_file: Option<VirtualFile>,
}

impl Nax {
    pub fn new(file: VirtualFile) -> Self {
        Self { file, status: NaxStatus::ErrorBadMagic, content_type: NaxContentType::Nca, dec_file: None }
    }

    pub fn with_nca_id(file: VirtualFile, _nca_id: [u8; 0x10]) -> Self {
        Self::new(file)
    }

    pub fn get_status(&self) -> NaxStatus { self.status }
    pub fn get_decrypted(&self) -> Option<VirtualFile> { self.dec_file.clone() }
    pub fn get_content_type(&self) -> NaxContentType { self.content_type }
    pub fn get_name(&self) -> String { self.file.get_name() }
    pub fn get_files(&self) -> Vec<VirtualFile> { self.dec_file.iter().cloned().collect() }
    pub fn get_subdirectories(&self) -> Vec<VirtualDir> { vec![] }
}
