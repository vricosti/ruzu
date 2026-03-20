// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/kip.h and kip.cpp
//!
//! KIP (Kernel Initial Process) loader.

use crate::file_sys::kernel_executable::KIP;
use crate::file_sys::program_metadata::{ProgramAddressSpaceType, ProgramMetadata};
use crate::file_sys::vfs::vfs_types::VirtualFile;
use crate::hle::kernel::code_set::CodeSet;

use super::loader::{
    make_magic, AppLoader, FileType, FileTypeIdentifier, KProcess, LoadParameters, LoadResult,
    ResultStatus, System,
};
use super::nso::read_object;

// ============================================================================
// Constants
// ============================================================================

/// Page-align a size value.
///
/// Maps to upstream anonymous `PageAlignSize` in kip.cpp.
const YUZU_PAGEMASK: u32 = 0xFFF;

const fn page_align_size(size: u32) -> u32 {
    (size + YUZU_PAGEMASK) & !YUZU_PAGEMASK
}

// ============================================================================
// AppLoaderKip
// ============================================================================

/// Loads a KIP file.
///
/// Maps to upstream `Loader::AppLoader_KIP`.
pub struct AppLoaderKip {
    file: VirtualFile,
    is_loaded: bool,
    /// Parsed KIP content. Maps to upstream `std::unique_ptr<FileSys::KIP> kip`.
    kip: Option<KIP>,
}

impl FileTypeIdentifier for AppLoaderKip {
    /// Identifies whether or not the given file is a KIP file.
    ///
    /// Maps to upstream `AppLoader_KIP::IdentifyType`.
    fn identify_type(in_file: &VirtualFile) -> FileType {
        if in_file.get_size() < std::mem::size_of::<u32>() {
            return FileType::Error;
        }
        let magic: Option<u32> = read_object::<u32>(in_file.as_ref(), 0);
        match magic {
            Some(m) if m == make_magic(b'K', b'I', b'P', b'1') => FileType::KIP,
            _ => FileType::Error,
        }
    }
}

impl AppLoaderKip {
    /// Create a new KIP loader.
    ///
    /// Maps to upstream `AppLoader_KIP::AppLoader_KIP`.
    /// Upstream: `kip = std::make_unique<FileSys::KIP>(file)`.
    pub fn new(file: VirtualFile) -> Self {
        let kip = KIP::new(&file);
        let kip_opt = if kip.get_status()
            == crate::file_sys::partition_filesystem::ResultStatus::Success
        {
            Some(kip)
        } else {
            Some(kip) // Keep even on failure, upstream stores it and checks status in GetFileType/Load
        };
        Self {
            file,
            is_loaded: false,
            kip: kip_opt,
        }
    }
}

impl AppLoader for AppLoaderKip {
    /// Maps to upstream `AppLoader_KIP::GetFileType`.
    ///
    /// Upstream: `(kip != nullptr && kip->GetStatus() == ResultStatus::Success)
    ///     ? FileType::KIP : FileType::Error`
    fn get_file_type(&self) -> FileType {
        match &self.kip {
            Some(kip)
                if kip.get_status()
                    == crate::file_sys::partition_filesystem::ResultStatus::Success =>
            {
                FileType::KIP
            }
            _ => FileType::Error,
        }
    }

    /// Maps to upstream `AppLoader_KIP::Load`.
    fn load(&mut self, process: &mut KProcess, _system: &mut System) -> LoadResult {
        if self.is_loaded {
            return (ResultStatus::ErrorAlreadyLoaded, None);
        }

        let kip = match &self.kip {
            Some(k) => k,
            None => return (ResultStatus::ErrorNullFile, None),
        };

        if kip.get_status() != crate::file_sys::partition_filesystem::ResultStatus::Success {
            // Map KIP's internal ResultStatus to Loader::ResultStatus.
            // Upstream returns kip->GetStatus() directly since both use the same enum.
            let kip_status = kip.get_status();
            let loader_status = match kip_status {
                crate::file_sys::partition_filesystem::ResultStatus::ErrorBadKIPHeader => {
                    ResultStatus::ErrorBadKIPHeader
                }
                crate::file_sys::partition_filesystem::ResultStatus::ErrorBLZDecompressionFailed => {
                    ResultStatus::ErrorBLZDecompressionFailed
                }
                _ => ResultStatus::ErrorNotInitialized,
            };
            return (loader_status, None);
        }

        // Determine address space type from KIP flags.
        // Maps to upstream lambda `get_kip_address_space_type`.
        let address_space = if kip.is_64_bit() {
            if kip.is_39_bit_address_space() {
                ProgramAddressSpaceType::Is39Bit
            } else {
                ProgramAddressSpaceType::Is36Bit
            }
        } else {
            ProgramAddressSpaceType::Is32Bit
        };

        // Build ProgramMetadata manually from KIP properties.
        // Maps to upstream `metadata.LoadManual(...)`.
        let mut metadata = ProgramMetadata::new();
        metadata.load_manual(
            kip.is_64_bit(),
            address_space,
            kip.get_main_thread_priority(),
            kip.get_main_thread_cpu_core(),
            kip.get_main_thread_stack_size(),
            kip.get_title_id(),
            0xFFFFFFFFFFFFFFFF,
            0x1FE00000,
            kip.get_kernel_capabilities(),
        );

        // Build program image from decompressed sections.
        let mut code_set = CodeSet::new();
        let mut program_image: Vec<u8> = Vec::new();

        // Helper closure matching upstream `load_segment` lambda.
        let mut load_segment =
            |segment_index: usize, data: &[u8], offset: u32| {
                let offset = offset as usize;
                code_set.segments[segment_index].addr = offset as u64;
                code_set.segments[segment_index].offset = offset;
                code_set.segments[segment_index].size =
                    page_align_size(data.len() as u32);
                let needed = offset + data.len();
                if program_image.len() < needed {
                    program_image.resize(needed, 0);
                }
                program_image[offset..offset + data.len()].copy_from_slice(data);
            };

        // Text (index 0), ROData (index 1), Data (index 2)
        load_segment(0, kip.get_text_section(), kip.get_text_offset());
        load_segment(1, kip.get_rodata_section(), kip.get_rodata_offset());
        load_segment(2, kip.get_data_section(), kip.get_data_offset());

        // BSS
        let bss_total = page_align_size(kip.get_bss_offset()) + kip.get_bss_size();
        program_image.resize(bss_total as usize, 0);
        code_set.data_segment_mut().size += kip.get_bss_size();

        // Setup the process code layout.
        // Upstream: process.LoadFromMetadata(FileSys::ProgramMetadata::GetDefault(), program_image.size(), 0, false)
        let default_metadata = ProgramMetadata::get_default();
        let process_result = process.load_from_metadata(
            &default_metadata,
            program_image.len() as u64,
            0,     // fastmem_base
            false, // is_hbl
        );
        if process_result != crate::hle::result::RESULT_SUCCESS.get_inner_value() {
            return (ResultStatus::ErrorNotInitialized, None);
        }

        // Load codeset into process.
        code_set.memory = program_image;
        let base_address = process.get_entry_point().get();
        process.load_module(code_set, base_address);

        log::debug!(
            "loaded module {} @ {:#X}",
            kip.get_name(),
            base_address
        );

        self.is_loaded = true;

        (
            ResultStatus::Success,
            Some(LoadParameters {
                main_thread_priority: kip.get_main_thread_priority(),
                main_thread_stack_size: kip.get_main_thread_stack_size() as u64,
            }),
        )
    }
}
