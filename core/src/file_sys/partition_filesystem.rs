// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/partition_filesystem.h and partition_filesystem.cpp
// PFS0/HFS0 partition filesystem parsing.

use std::collections::BTreeMap;
use std::sync::Arc;

use super::vfs::vfs::{VfsDirectory, VfsFile};
use super::vfs::vfs_offset::OffsetVfsFile;
use super::vfs::vfs_types::{VirtualDir, VirtualFile};

/// Helper to construct a four-character-code magic value from bytes.
const fn make_magic(a: u8, b: u8, c: u8, d: u8) -> u32 {
    (a as u32) | ((b as u32) << 8) | ((c as u32) << 16) | ((d as u32) << 24)
}

const PFS0_MAGIC: u32 = make_magic(b'P', b'F', b'S', b'0');
const HFS0_MAGIC: u32 = make_magic(b'H', b'F', b'S', b'0');

/// Loader result status codes used across file_sys modules.
/// Corresponds to upstream `Loader::ResultStatus`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum ResultStatus {
    Success = 0,
    ErrorAlreadyLoaded = 1,
    ErrorNotImplemented = 2,
    ErrorNotInitialized = 3,
    ErrorBadNCAHeader = 4,
    ErrorBadPFSHeader = 5,
    ErrorIncorrectPFSFileSize = 6,
    ErrorBadNROHeader = 7,
    ErrorBadNSOHeader = 8,
    ErrorBadNSPHeader = 9,
    ErrorNullFile = 10,
    ErrorMissingNRO = 11,
    ErrorNoRomFS = 12,
    ErrorBadELFHeader = 13,
    ErrorInvalidFormat = 14,
    ErrorMissingKeyAreaKey = 15,
    ErrorMissingTitlekey = 16,
    ErrorMissingTitlekek = 17,
    ErrorMissingBKTRBaseRomFS = 18,
    ErrorBadXCIHeader = 19,
    ErrorXCIMissingProgramNCA = 20,
    ErrorXCIMissingPartition = 21,
    ErrorNSPMissingProgramNCA = 22,
    ErrorBadBKTRHeader = 23,
    ErrorBKTRSubsectionNotAfterRelocation = 24,
    ErrorBKTRSubsectionNotAtEnd = 25,
    ErrorBadRelocationBlock = 26,
    ErrorBadSubsectionBlock = 27,
    ErrorBadRelocationBuckets = 28,
    ErrorBadSubsectionBuckets = 29,
    ErrorMissingBKTRRomFS = 30,
    ErrorNoPackedUpdate = 31,
    ErrorBadKIPHeader = 32,
    ErrorBLZDecompressionFailed = 33,
    ErrorBadINIHeader = 34,
    ErrorINITooManyKIPs = 35,
    ErrorBadNPDMHeader = 36,
    ErrorBadACIDHeader = 37,
    ErrorBadACIHeader = 38,
    ErrorBadFileAccessControl = 39,
    ErrorBadFileAccessHeader = 40,
    ErrorBadKernelCapabilityDescriptors = 41,
    ErrorBadNAXHeader = 42,
    ErrorBadNAXFilePath = 43,
    ErrorIncorrectNAXFileSize = 44,
    ErrorNAXKeyHMACFailed = 45,
    ErrorNAXValidationHMACFailed = 46,
    ErrorNAXKeyDerivationFailed = 47,
    ErrorNAXInconvertibleToNCA = 48,
    ErrorBadNAXFilePath2 = 49,
    ErrorIncompleteDump = 50,
}

// ============================================================================
// Binary structures
// ============================================================================

/// PFS/HFS header — 0x10 bytes.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct Header {
    magic: u32,
    num_entries: u32,
    strtab_size: u32,
    _reserved: [u8; 4],
}

const _: () = assert!(std::mem::size_of::<Header>() == 0x10);

impl Header {
    fn has_valid_magic_value(&self) -> bool {
        self.magic == PFS0_MAGIC || self.magic == HFS0_MAGIC
    }
}

/// Common FS entry — 0x14 bytes (packed).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C, packed)]
struct FSEntry {
    offset: u64,
    size: u64,
    strtab_offset: u32,
}

const _: () = assert!(std::mem::size_of::<FSEntry>() == 0x14);

/// PFS entry — FSEntry + 4 bytes padding = 0x18.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C, packed)]
struct PFSEntry {
    fs_entry: FSEntry,
    _padding: [u8; 4],
}

const _: () = assert!(std::mem::size_of::<PFSEntry>() == 0x18);

/// HFS entry — FSEntry + hash region size + 8 bytes padding + 0x20 hash = 0x40.
#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
struct HFSEntry {
    fs_entry: FSEntry,
    hash_region_size: u32,
    _padding: [u8; 8],
    hash: [u8; 0x20],
}

const _: () = assert!(std::mem::size_of::<HFSEntry>() == 0x40);

// ============================================================================
// PartitionFilesystem
// ============================================================================

/// An implementation of VfsDirectory that parses PFS0/HFS0 partition filesystems.
///
/// Maps to upstream `PartitionFilesystem`.
pub struct PartitionFilesystem {
    status: ResultStatus,
    pfs_header: Header,
    is_hfs: bool,
    content_offset: usize,
    offsets: BTreeMap<String, u64>,
    sizes: BTreeMap<String, u64>,
    pfs_files: Vec<VirtualFile>,
}

impl PartitionFilesystem {
    pub fn new(file: VirtualFile) -> Self {
        let mut pf = Self {
            status: ResultStatus::Success,
            pfs_header: Header::default(),
            is_hfs: false,
            content_offset: 0,
            offsets: BTreeMap::new(),
            sizes: BTreeMap::new(),
            pfs_files: Vec::new(),
        };

        let header_size = std::mem::size_of::<Header>();

        // At least be as large as the header
        if file.get_size() < header_size {
            pf.status = ResultStatus::ErrorBadPFSHeader;
            return pf;
        }

        // Read header
        let mut header_bytes = vec![0u8; header_size];
        if file.read(&mut header_bytes, header_size, 0) != header_size {
            pf.status = ResultStatus::ErrorBadPFSHeader;
            return pf;
        }
        // SAFETY: Header is repr(C) with all-byte-safe fields.
        unsafe {
            std::ptr::copy_nonoverlapping(
                header_bytes.as_ptr(),
                &mut pf.pfs_header as *mut Header as *mut u8,
                header_size,
            );
        }

        if !pf.pfs_header.has_valid_magic_value() {
            pf.status = ResultStatus::ErrorBadPFSHeader;
            return pf;
        }

        pf.is_hfs = pf.pfs_header.magic == HFS0_MAGIC;

        let entry_size = if pf.is_hfs {
            std::mem::size_of::<HFSEntry>()
        } else {
            std::mem::size_of::<PFSEntry>()
        };

        let metadata_size = header_size
            + (pf.pfs_header.num_entries as usize * entry_size)
            + pf.pfs_header.strtab_size as usize;

        // Read all metadata
        let mut file_data = file.read_bytes(metadata_size, 0);
        let total_size = file_data.len();
        file_data.push(0); // Null terminator for string table

        if total_size != metadata_size {
            pf.status = ResultStatus::ErrorIncorrectPFSFileSize;
            return pf;
        }

        let entries_offset = header_size;
        let strtab_offset = entries_offset + (pf.pfs_header.num_entries as usize * entry_size);
        pf.content_offset = strtab_offset + pf.pfs_header.strtab_size as usize;

        for i in 0..pf.pfs_header.num_entries as usize {
            // Read FSEntry from file_data
            let entry_start = entries_offset + i * entry_size;
            let mut entry = FSEntry::default();
            let fs_entry_size = std::mem::size_of::<FSEntry>();
            if entry_start + fs_entry_size <= file_data.len() {
                unsafe {
                    std::ptr::copy_nonoverlapping(
                        file_data.as_ptr().add(entry_start),
                        &mut entry as *mut FSEntry as *mut u8,
                        fs_entry_size,
                    );
                }
            }

            // Read name from string table
            let name_start = strtab_offset + entry.strtab_offset as usize;
            let name = if name_start < file_data.len() {
                let end = file_data[name_start..]
                    .iter()
                    .position(|&b| b == 0)
                    .unwrap_or(file_data.len() - name_start);
                String::from_utf8_lossy(&file_data[name_start..name_start + end]).into_owned()
            } else {
                String::new()
            };

            let entry_off = { entry.offset };
            let entry_sz = { entry.size };
            let file_offset = pf.content_offset as u64 + entry_off;
            log::trace!(
                "PFS entry[{}]: name='{}', offset=0x{:X} (content_off=0x{:X} + entry_off=0x{:X}), size=0x{:X}",
                i, name, file_offset, pf.content_offset, entry_off, entry_sz
            );
            pf.offsets.insert(name.clone(), file_offset);
            pf.sizes.insert(name.clone(), entry.size);

            pf.pfs_files.push(Arc::new(OffsetVfsFile::new(
                file.clone(),
                entry.size as usize,
                file_offset as usize,
                name,
            )));
        }

        pf.status = ResultStatus::Success;
        pf
    }

    pub fn get_status(&self) -> ResultStatus {
        self.status
    }

    pub fn get_file_offsets(&self) -> BTreeMap<String, u64> {
        self.offsets.clone()
    }

    pub fn get_file_sizes(&self) -> BTreeMap<String, u64> {
        self.sizes.clone()
    }
}

impl VfsDirectory for PartitionFilesystem {
    fn get_files(&self) -> Vec<VirtualFile> {
        self.pfs_files.clone()
    }

    fn get_subdirectories(&self) -> Vec<VirtualDir> {
        vec![]
    }

    fn get_name(&self) -> String {
        if self.is_hfs {
            "HFS0".to_string()
        } else {
            "PFS0".to_string()
        }
    }

    fn get_parent_directory(&self) -> Option<VirtualDir> {
        // TODO(DarkLordZach): Add support for nested containers.
        None
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
