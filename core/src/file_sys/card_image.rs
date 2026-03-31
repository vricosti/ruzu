// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/card_image.h and card_image.cpp
// XCI card image parsing.

use std::sync::Arc;

use super::content_archive::{NCAContentType, NCA};
use super::nca_metadata::CNMT;
use super::partition_filesystem::{PartitionFilesystem, ResultStatus};
use super::submission_package::NSP;
use super::vfs::vfs::{VfsDirectory, VfsFile};
use super::vfs::vfs_offset::OffsetVfsFile;
use super::vfs::vfs_types::{VirtualDir, VirtualFile};
use super::vfs::vfs_vector::VectorVfsDirectory;

// ============================================================================
// Constants
// ============================================================================

const GAMECARD_CERTIFICATE_OFFSET: u64 = 0x7000;
const PARTITION_NAMES: [&str; 4] = ["update", "normal", "secure", "logo"];

/// Helper to construct a four-character-code magic value from bytes.
const fn make_magic(a: u8, b: u8, c: u8, d: u8) -> u32 {
    (a as u32) | ((b as u32) << 8) | ((c as u32) << 16) | ((d as u32) << 24)
}

const HEAD_MAGIC: u32 = make_magic(b'H', b'E', b'A', b'D');

// ============================================================================
// Enums and structures
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum GamecardSize {
    S1GB = 0xFA,
    S2GB = 0xF8,
    S4GB = 0xF0,
    S8GB = 0xE0,
    S16GB = 0xE1,
    S32GB = 0xE2,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct GamecardInfo {
    pub firmware_version: u64,
    pub access_control_flags: u32,
    pub read_wait_time1: u32,
    pub read_wait_time2: u32,
    pub write_wait_time1: u32,
    pub write_wait_time2: u32,
    pub firmware_mode: u32,
    pub cup_version: u32,
    pub reserved1: [u8; 4],
    pub update_partition_hash: u64,
    pub cup_id: u64,
    pub reserved2: [u8; 0x38],
}

impl Default for GamecardInfo {
    fn default() -> Self {
        // Safety: all-zero is a valid representation for this #[repr(C)] struct.
        unsafe { std::mem::zeroed() }
    }
}

const _: () = assert!(std::mem::size_of::<GamecardInfo>() == 0x70);

#[derive(Clone, Copy)]
#[repr(C)]
pub struct GamecardHeader {
    pub signature: [u8; 0x100],
    pub magic: u32,
    pub secure_area_start: u32,
    pub backup_area_start: u32,
    pub kek_index: u8,
    pub size: u8,
    pub header_version: u8,
    pub flags: u8,
    pub package_id: u64,
    pub valid_data_end: u64,
    pub info_iv: [u8; 16],
    pub hfs_offset: u64,
    pub hfs_size: u64,
    pub hfs_header_hash: [u8; 0x20],
    pub initial_data_hash: [u8; 0x20],
    pub secure_mode_flag: u32,
    pub title_key_flag: u32,
    pub key_flag: u32,
    pub normal_area_end: u32,
    pub info: GamecardInfo,
}

const _: () = assert!(std::mem::size_of::<GamecardHeader>() == 0x200);

impl Default for GamecardHeader {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// XCI partition identifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum XCIPartition {
    Update = 0,
    Normal = 1,
    Secure = 2,
    Logo = 3,
}

// ============================================================================
// XCI
// ============================================================================

/// An implementation of VfsDirectory representing an XCI card image.
/// Corresponds to upstream `XCI`.
pub struct XCI {
    file: VirtualFile,
    header: GamecardHeader,

    status: ResultStatus,
    program_nca_status: ResultStatus,

    partitions: Vec<Option<VirtualDir>>,
    partitions_raw: Vec<Option<VirtualFile>>,
    secure_partition: Option<Arc<NSP>>,
    program: Option<Arc<NCA>>,
    ncas: Vec<Arc<NCA>>,

    update_normal_partition_end: u64,
}

impl XCI {
    pub fn new(file: VirtualFile, program_id: u64, program_index: usize) -> Self {
        let mut xci = Self {
            file: file.clone(),
            header: GamecardHeader::default(),
            status: ResultStatus::Success,
            program_nca_status: ResultStatus::ErrorXCIMissingProgramNCA,
            partitions: vec![None; PARTITION_NAMES.len()],
            partitions_raw: vec![None; PARTITION_NAMES.len()],
            secure_partition: None,
            program: None,
            ncas: Vec::new(),
            update_normal_partition_end: 0,
        };

        let header_status = xci.try_read_header();
        if header_status != ResultStatus::Success {
            xci.status = header_status;
            return xci;
        }

        let main_hfs = PartitionFilesystem::new(Arc::new(OffsetVfsFile::new(
            xci.file.clone(),
            xci.file.get_size() - xci.header.hfs_offset as usize,
            xci.header.hfs_offset as usize,
            String::new(),
        )));

        xci.update_normal_partition_end = main_hfs
            .get_file_offsets()
            .get("secure")
            .copied()
            .unwrap_or(0);

        if main_hfs.get_status() != ResultStatus::Success {
            xci.status = main_hfs.get_status();
            return xci;
        }

        for partition in [
            XCIPartition::Update,
            XCIPartition::Normal,
            XCIPartition::Secure,
            XCIPartition::Logo,
        ] {
            let idx = partition as usize;
            let raw = main_hfs.get_file(PARTITION_NAMES[idx]);
            xci.partitions_raw[idx] = raw;
        }

        // Parse secure partition as NSP
        if let Some(secure_raw) = &xci.partitions_raw[XCIPartition::Secure as usize] {
            let nsp = Arc::new(NSP::new(secure_raw.clone(), program_id, program_index));
            xci.ncas = nsp.get_ncas_collapsed();

            let prog_tid = nsp.get_program_title_id();
            xci.program = nsp.get_nca(
                prog_tid,
                super::nca_metadata::ContentRecordType::Program,
                super::nca_metadata::TitleType::Application,
            );
            xci.program_nca_status = nsp.get_program_status();
            if xci.program_nca_status == ResultStatus::ErrorNSPMissingProgramNCA {
                xci.program_nca_status = ResultStatus::ErrorXCIMissingProgramNCA;
            }
            xci.secure_partition = Some(nsp);
        }

        let result = xci.add_nca_from_partition(XCIPartition::Normal);
        if result != ResultStatus::Success {
            xci.status = result;
            return xci;
        }

        if xci.get_format_version() >= 0x2 {
            let result = xci.add_nca_from_partition(XCIPartition::Logo);
            if result != ResultStatus::Success {
                xci.status = result;
                return xci;
            }
        }

        xci.status = ResultStatus::Success;
        xci
    }

    pub fn get_status(&self) -> ResultStatus {
        self.status
    }

    pub fn get_program_nca_status(&self) -> ResultStatus {
        self.program_nca_status
    }

    pub fn get_format_version(&mut self) -> u8 {
        if self.get_logo_partition().is_none() {
            0x1
        } else {
            0x2
        }
    }

    pub fn get_partition(&mut self, partition: XCIPartition) -> Option<VirtualDir> {
        let id = partition as usize;
        if self.partitions[id].is_none() {
            if let Some(raw) = &self.partitions_raw[id] {
                self.partitions[id] = Some(Arc::new(PartitionFilesystem::new(raw.clone())));
            }
        }
        self.partitions[id].clone()
    }

    pub fn get_partitions(&mut self) -> Vec<VirtualDir> {
        let mut out = Vec::new();
        for id in [
            XCIPartition::Update,
            XCIPartition::Normal,
            XCIPartition::Secure,
            XCIPartition::Logo,
        ] {
            if let Some(part) = self.get_partition(id) {
                out.push(part);
            }
        }
        out
    }

    pub fn get_secure_partition_nsp(&self) -> Option<Arc<NSP>> {
        self.secure_partition.clone()
    }

    pub fn get_secure_partition(&mut self) -> Option<VirtualDir> {
        self.get_partition(XCIPartition::Secure)
    }

    pub fn get_normal_partition(&mut self) -> Option<VirtualDir> {
        self.get_partition(XCIPartition::Normal)
    }

    pub fn get_update_partition(&mut self) -> Option<VirtualDir> {
        self.get_partition(XCIPartition::Update)
    }

    pub fn get_logo_partition(&mut self) -> Option<VirtualDir> {
        self.get_partition(XCIPartition::Logo)
    }

    pub fn get_partition_raw(&self, partition: XCIPartition) -> Option<VirtualFile> {
        self.partitions_raw[partition as usize].clone()
    }

    pub fn get_secure_partition_raw(&self) -> Option<VirtualFile> {
        self.get_partition_raw(XCIPartition::Secure)
    }

    pub fn get_normal_partition_raw(&self) -> Option<VirtualFile> {
        self.get_partition_raw(XCIPartition::Normal)
    }

    pub fn get_update_partition_raw(&self) -> Option<VirtualFile> {
        self.get_partition_raw(XCIPartition::Update)
    }

    pub fn get_logo_partition_raw(&self) -> Option<VirtualFile> {
        self.get_partition_raw(XCIPartition::Logo)
    }

    pub fn get_storage_partition0(&self) -> VirtualFile {
        Arc::new(OffsetVfsFile::new(
            self.file.clone(),
            self.update_normal_partition_end as usize,
            0,
            "partition0".to_string(),
        ))
    }

    pub fn get_storage_partition1(&self) -> VirtualFile {
        Arc::new(OffsetVfsFile::new(
            self.file.clone(),
            self.file.get_size() - self.update_normal_partition_end as usize,
            self.update_normal_partition_end as usize,
            "partition1".to_string(),
        ))
    }

    pub fn get_program_title_id(&self) -> u64 {
        self.secure_partition
            .as_ref()
            .map(|sp| sp.get_program_title_id())
            .unwrap_or(0)
    }

    pub fn get_program_title_ids(&self) -> Vec<u64> {
        self.secure_partition
            .as_ref()
            .map(|sp| sp.get_program_title_ids())
            .unwrap_or_default()
    }

    pub fn get_system_update_title_id(&self) -> u64 {
        0x0100000000000816
    }

    pub fn has_program_nca(&self) -> bool {
        self.program.is_some()
    }

    pub fn get_program_nca_file(&self) -> Option<VirtualFile> {
        self.program.as_ref().map(|p| p.get_base_file())
    }

    pub fn get_ncas(&self) -> &[Arc<NCA>] {
        &self.ncas
    }

    pub fn get_nca_by_type(&self, nca_type: NCAContentType) -> Option<Arc<NCA>> {
        let program_id = self.get_program_title_id();
        self.ncas
            .iter()
            .find(|nca| nca.get_type() == nca_type && nca.get_title_id() == program_id)
            .cloned()
    }

    pub fn get_nca_file_by_type(&self, nca_type: NCAContentType) -> Option<VirtualFile> {
        self.get_nca_by_type(nca_type)
            .map(|nca| nca.get_base_file())
    }

    pub fn concatenated_pseudo_directory(&mut self) -> VirtualDir {
        let out = Arc::new(VectorVfsDirectory::new(vec![], vec![], String::new(), None));
        for part_id in [
            XCIPartition::Normal,
            XCIPartition::Logo,
            XCIPartition::Secure,
        ] {
            if let Some(part) = self.get_partition(part_id) {
                for part_file in part.get_files() {
                    out.add_file(part_file);
                }
            }
        }
        out
    }

    pub fn get_certificate(&self) -> [u8; 0x200] {
        let mut out = [0u8; 0x200];
        let len = out.len();
        self.file
            .read(&mut out, len, GAMECARD_CERTIFICATE_OFFSET as usize);
        out
    }

    /// Add NCAs from a non-secure partition (Normal, Logo).
    /// Corresponds to upstream `XCI::AddNCAFromPartition`.
    fn add_nca_from_partition(&mut self, part: XCIPartition) -> ResultStatus {
        let partition_index = part as usize;
        let partition = match self.get_partition(part) {
            Some(p) => p,
            None => return ResultStatus::ErrorXCIMissingPartition,
        };

        for partition_file in partition.get_files() {
            if partition_file.get_extension() != "nca" {
                continue;
            }

            let nca = Arc::new(NCA::new(partition_file, None));
            if nca.is_update() {
                continue;
            }
            if nca.get_type() == NCAContentType::Program {
                self.program_nca_status = nca.get_status();
            }
            if nca.get_status() == ResultStatus::Success {
                self.ncas.push(nca);
            } else {
                let error_id = nca.get_status() as u16;
                log::error!(
                    "Could not load NCA {}/{}, failed with error code {:04X} ({:?})",
                    PARTITION_NAMES[partition_index],
                    nca.get_name(),
                    error_id,
                    nca.get_status()
                );
            }
        }

        ResultStatus::Success
    }

    fn try_read_header(&mut self) -> ResultStatus {
        const CARD_INITIAL_DATA_REGION_SIZE: usize = 0x1000;

        let read_card_header = |file: &VirtualFile, header: &mut GamecardHeader| -> ResultStatus {
            let size = std::mem::size_of::<GamecardHeader>();
            let mut buf = vec![0u8; size];
            if file.read(&mut buf, size, 0) != size {
                return ResultStatus::ErrorBadXCIHeader;
            }
            unsafe {
                std::ptr::copy_nonoverlapping(
                    buf.as_ptr(),
                    header as *mut GamecardHeader as *mut u8,
                    size,
                );
            }
            if header.magic != HEAD_MAGIC {
                return ResultStatus::ErrorBadXCIHeader;
            }
            ResultStatus::Success
        };

        // Try to read the header directly
        if read_card_header(&self.file, &mut self.header) == ResultStatus::Success {
            return ResultStatus::Success;
        }

        // If we are large enough to have a key area, offset past it and retry
        let card_image_size = self.file.get_size();
        if card_image_size >= CARD_INITIAL_DATA_REGION_SIZE {
            self.file = Arc::new(OffsetVfsFile::new(
                self.file.clone(),
                card_image_size - CARD_INITIAL_DATA_REGION_SIZE,
                CARD_INITIAL_DATA_REGION_SIZE,
                String::new(),
            ));
            return read_card_header(&self.file, &mut self.header);
        }

        ResultStatus::ErrorBadXCIHeader
    }
}

impl VfsDirectory for XCI {
    fn get_files(&self) -> Vec<VirtualFile> {
        vec![]
    }

    fn get_subdirectories(&self) -> Vec<VirtualDir> {
        vec![]
    }

    fn get_name(&self) -> String {
        self.file.get_name()
    }

    fn get_parent_directory(&self) -> Option<VirtualDir> {
        self.file.get_containing_directory()
    }

    fn is_writable(&self) -> bool {
        false
    }

    fn is_readable(&self) -> bool {
        true
    }

    fn create_subdirectory(&self, _name: &str) -> Option<VirtualDir> {
        None
    }

    fn create_file(&self, _name: &str) -> Option<VirtualFile> {
        None
    }

    fn delete_subdirectory(&self, _name: &str) -> bool {
        false
    }

    fn delete_file(&self, _name: &str) -> bool {
        false
    }

    fn rename(&self, _name: &str) -> bool {
        false
    }
}
