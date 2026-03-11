// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/submission_package.h and submission_package.cpp
// NSP submission package parsing.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use super::content_archive::{is_directory_exefs, NCA, NCAContentType};
use super::nca_metadata::{CNMTHeader, ContentRecordType, TitleType, CNMT};
use super::partition_filesystem::{PartitionFilesystem, ResultStatus};
use super::vfs::vfs::{VfsDirectory, VfsFile};
use super::vfs::vfs_types::{VirtualDir, VirtualFile};

// ============================================================================
// NSP
// ============================================================================

/// An implementation of VfsDirectory that represents an NSP (Nintendo Submission Package).
/// Corresponds to upstream `NSP`.
pub struct NSP {
    file: VirtualFile,
    expected_program_id: u64,
    program_index: usize,

    extracted: bool,
    status: ResultStatus,
    program_status: BTreeMap<u64, ResultStatus>,

    pfs: Arc<PartitionFilesystem>,
    /// Map title_id -> {map (TitleType, ContentRecordType) -> NCA}
    ncas: BTreeMap<u64, BTreeMap<(u8, u8), Arc<NCA>>>,
    program_ids: BTreeSet<u64>,

    romfs: Option<VirtualFile>,
    exefs: Option<VirtualDir>,
}

impl NSP {
    pub fn new(file: VirtualFile, title_id: u64, program_index: usize) -> Self {
        let pfs = Arc::new(PartitionFilesystem::new(file.clone()));
        let mut nsp = Self {
            file,
            expected_program_id: title_id,
            program_index,
            extracted: false,
            status: ResultStatus::Success,
            program_status: BTreeMap::new(),
            pfs: pfs.clone(),
            ncas: BTreeMap::new(),
            program_ids: BTreeSet::new(),
            romfs: None,
            exefs: None,
        };

        if pfs.get_status() != ResultStatus::Success {
            nsp.status = pfs.get_status();
            return nsp;
        }

        let files = pfs.get_files();

        if is_directory_exefs(&(pfs.clone() as VirtualDir)) {
            nsp.extracted = true;
            nsp.initialize_exefs_and_romfs(&files);
            return nsp;
        }

        nsp.set_ticket_keys(&files);
        nsp.read_ncas(&files);

        nsp
    }

    pub fn get_status(&self) -> ResultStatus {
        self.status
    }

    pub fn get_program_status(&self) -> ResultStatus {
        if self.is_extracted_type() {
            if let Some(exefs) = self.get_exefs() {
                if is_directory_exefs(&exefs) {
                    return ResultStatus::Success;
                }
            }
        }

        let tid = self.get_program_title_id();
        self.program_status
            .get(&tid)
            .copied()
            .unwrap_or(ResultStatus::ErrorNSPMissingProgramNCA)
    }

    pub fn get_program_title_id(&self) -> u64 {
        if self.is_extracted_type() {
            return self.get_extracted_title_id() + self.program_index as u64;
        }

        let mut program_id = self.expected_program_id;
        if program_id == 0 {
            if let Some((&first_id, _)) = self.program_status.iter().next() {
                program_id = first_id;
            }
        }

        program_id += self.program_index as u64;
        if self.program_status.contains_key(&program_id) {
            return program_id;
        }

        let ids = self.get_program_title_ids();
        ids.into_iter()
            .find(|tid| (tid & 0x800) == 0)
            .unwrap_or(0)
    }

    pub fn get_extracted_title_id(&self) -> u64 {
        // TODO: Implement ProgramMetadata loading
        0
    }

    pub fn get_program_title_ids(&self) -> Vec<u64> {
        if self.is_extracted_type() {
            let tid = self.get_extracted_title_id();
            return if tid != 0 { vec![tid] } else { vec![] };
        }
        self.program_ids.iter().copied().collect()
    }

    pub fn is_extracted_type(&self) -> bool {
        self.extracted
    }

    pub fn get_romfs(&self) -> Option<VirtualFile> {
        self.romfs.clone()
    }

    pub fn get_exefs(&self) -> Option<VirtualDir> {
        self.exefs.clone()
    }

    /// Get all NCAs collapsed into a flat vector.
    pub fn get_ncas_collapsed(&self) -> Vec<NCA> {
        if self.extracted {
            log::warn!("get_ncas_collapsed called on an NSP that is of type extracted.");
        }
        // NOTE: This returns empty because NCA construction requires crypto.
        // The full implementation would iterate self.ncas and collect.
        Vec::new()
    }

    pub fn get_ncas(
        &self,
    ) -> &BTreeMap<u64, BTreeMap<(u8, u8), Arc<NCA>>> {
        &self.ncas
    }

    pub fn get_nca(
        &self,
        title_id: u64,
        record_type: ContentRecordType,
        title_type: TitleType,
    ) -> Option<Arc<NCA>> {
        if self.extracted {
            log::warn!("get_nca called on an NSP that is of type extracted.");
        }
        let title_map = self.ncas.get(&title_id)?;
        title_map
            .get(&(title_type as u8, record_type as u8))
            .cloned()
    }

    pub fn get_nca_file(
        &self,
        title_id: u64,
        record_type: ContentRecordType,
        title_type: TitleType,
    ) -> Option<VirtualFile> {
        if self.extracted {
            log::warn!("get_nca_file called on an NSP that is of type extracted.");
        }
        self.get_nca(title_id, record_type, title_type)
            .map(|nca| nca.get_base_file())
    }

    fn set_ticket_keys(&mut self, files: &[VirtualFile]) {
        for ticket_file in files {
            if ticket_file.get_extension() != "tik" {
                continue;
            }
            // TODO: Process ticket keys once crypto module is ported
        }
    }

    fn initialize_exefs_and_romfs(&mut self, files: &[VirtualFile]) {
        self.exefs = Some(self.pfs.clone() as VirtualDir);

        for file in files {
            if file.get_name().contains(".romfs") {
                self.romfs = Some(file.clone());
                return;
            }
        }
    }

    fn read_ncas(&mut self, files: &[VirtualFile]) {
        // TODO: Full NCA reading requires crypto subsystem.
        // This is a structural stub that matches the upstream API.
        for outer_file in files {
            let name = outer_file.get_name();
            if name.len() < 9 || !name.ends_with(".cnmt.nca") {
                continue;
            }

            // NCA parsing requires crypto; skip for now
            // In full implementation:
            // 1. Parse the .cnmt.nca as NCA
            // 2. Read CNMT from section0
            // 3. Resolve content records to NCAs in PFS
        }
    }
}

impl VfsDirectory for NSP {
    fn get_files(&self) -> Vec<VirtualFile> {
        self.pfs.get_files()
    }

    fn get_subdirectories(&self) -> Vec<VirtualDir> {
        self.pfs.get_subdirectories()
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
