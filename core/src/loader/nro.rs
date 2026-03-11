// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/nro.h and nro.cpp
//!
//! NRO (Nintendo Relocatable Object) loader.

use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::VirtualFile;

use super::loader::{
    make_magic, AppLoader, FileType, FileTypeIdentifier, KProcess, LoadParameters, LoadResult,
    Modules, ResultStatus, System, NACP,
};
use super::nso::{read_object, NsoArgumentHeader, NSO_ARGUMENT_DATA_ALLOCATION_SIZE};

// ============================================================================
// NroSegmentHeader
// ============================================================================

/// Segment header within an NRO file.
///
/// Maps to upstream `Loader::NroSegmentHeader` (in nro.cpp).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NroSegmentHeader {
    pub offset: u32,
    pub size: u32,
}

const _: () = assert!(std::mem::size_of::<NroSegmentHeader>() == 0x8);

// ============================================================================
// NroHeader
// ============================================================================

/// NRO file header.
///
/// Maps to upstream `Loader::NroHeader` (in nro.cpp).
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NroHeader {
    pub _padding0: [u8; 0x4],
    pub module_header_offset: u32,
    pub magic_ext1: u32,
    pub magic_ext2: u32,
    pub magic: u32,
    pub _padding1: [u8; 0x4],
    pub file_size: u32,
    pub _padding2: [u8; 0x4],
    /// Text, RoData, Data segments (in that order).
    pub segments: [NroSegmentHeader; 3],
    pub bss_size: u32,
    pub _padding3: [u8; 0x44],
}

const _: () = assert!(std::mem::size_of::<NroHeader>() == 0x80);

impl Default for NroHeader {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

// ============================================================================
// ModHeader (NRO variant)
// ============================================================================

/// MOD header found within NRO modules.
///
/// Maps to upstream `Loader::ModHeader` (in nro.cpp).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct ModHeader {
    magic: u32,
    dynamic_offset: u32,
    bss_start_offset: u32,
    bss_end_offset: u32,
    unwind_start_offset: u32,
    unwind_end_offset: u32,
    /// Offset to runtime-generated module object. Typically equal to .bss base.
    module_offset: u32,
}

const _: () = assert!(std::mem::size_of::<ModHeader>() == 0x1c);

// ============================================================================
// AssetSection / AssetHeader
// ============================================================================

/// Asset section descriptor.
///
/// Maps to upstream `Loader::AssetSection` (in nro.cpp).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct AssetSection {
    pub offset: u64,
    pub size: u64,
}

const _: () = assert!(std::mem::size_of::<AssetSection>() == 0x10);

/// NRO asset header, appended after the NRO proper.
///
/// Maps to upstream `Loader::AssetHeader` (in nro.cpp).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct AssetHeader {
    pub magic: u32,
    pub format_version: u32,
    pub icon: AssetSection,
    pub nacp: AssetSection,
    pub romfs: AssetSection,
}

const _: () = assert!(std::mem::size_of::<AssetHeader>() == 0x38);

// ============================================================================
// Helper
// ============================================================================

const YUZU_PAGEMASK: u32 = 0xFFF;

const fn page_align_size(size: u32) -> u32 {
    (size + YUZU_PAGEMASK) & !YUZU_PAGEMASK
}

// ============================================================================
// AppLoaderNro
// ============================================================================

/// Loads an NRO file.
///
/// Maps to upstream `Loader::AppLoader_NRO`.
pub struct AppLoaderNro {
    file: VirtualFile,
    is_loaded: bool,
    icon_data: Vec<u8>,
    // TODO: Use actual FileSys::NACP when ported.
    // nacp: Option<NACP>,
    romfs: Option<VirtualFile>,
}

impl FileTypeIdentifier for AppLoaderNro {
    /// Identifies whether or not the given file is an NRO file.
    ///
    /// Maps to upstream `AppLoader_NRO::IdentifyType`.
    fn identify_type(nro_file: &VirtualFile) -> FileType {
        let header: Option<NroHeader> = read_object(nro_file.as_ref(), 0);
        match header {
            Some(h) if h.magic == make_magic(b'N', b'R', b'O', b'0') => FileType::NRO,
            _ => FileType::Error,
        }
    }
}

impl AppLoaderNro {
    /// Create a new NRO loader.
    ///
    /// Maps to upstream `AppLoader_NRO::AppLoader_NRO`.
    pub fn new(file: VirtualFile) -> Self {
        let mut icon_data = Vec::new();
        let romfs = None;

        // Read NRO header to find asset section
        if let Some(nro_header) = read_object::<NroHeader>(file.as_ref(), 0) {
            let file_size = file.get_size();
            if file_size >= nro_header.file_size as usize + std::mem::size_of::<AssetHeader>() {
                let offset = nro_header.file_size as usize;
                if let Some(asset_header) =
                    read_object::<AssetHeader>(file.as_ref(), offset)
                {
                    if asset_header.format_version != 0 {
                        log::warn!(
                            "NRO Asset Header has format {}, currently supported format is 0. \
                             If strange glitches occur with metadata, check NRO assets.",
                            asset_header.format_version
                        );
                    }

                    if asset_header.magic == make_magic(b'A', b'S', b'E', b'T') {
                        // TODO: Parse NACP data when FileSys::NACP is ported.
                        // if asset_header.nacp.size > 0 { ... }

                        // TODO: Parse RomFS as OffsetVfsFile when fully integrated.
                        // if asset_header.romfs.size > 0 { ... }

                        // Read icon data
                        if asset_header.icon.size > 0 {
                            icon_data = file.read_bytes(
                                asset_header.icon.size as usize,
                                offset + asset_header.icon.offset as usize,
                            );
                        }
                    }
                }
            }
        }

        Self {
            file,
            is_loaded: false,
            icon_data,
            romfs,
        }
    }

    /// Check if this NRO is homebrew (has HOMEBREW magic).
    ///
    /// Maps to upstream `AppLoader_NRO::IsHomebrew`.
    pub fn is_homebrew(&self) -> bool {
        if let Some(header) = read_object::<NroHeader>(self.file.as_ref(), 0) {
            header.magic_ext1 == make_magic(b'H', b'O', b'M', b'E')
                && header.magic_ext2 == make_magic(b'B', b'R', b'E', b'W')
        } else {
            false
        }
    }

    /// Internal NRO loading implementation.
    ///
    /// Maps to upstream static `LoadNroImpl`.
    fn load_nro_impl(
        _system: &mut System,
        _process: &mut KProcess,
        data: &[u8],
    ) -> bool {
        if data.len() < std::mem::size_of::<NroHeader>() {
            return false;
        }

        // Read NRO header
        let mut nro_header = NroHeader::default();
        let header_bytes = unsafe {
            std::slice::from_raw_parts_mut(
                &mut nro_header as *mut NroHeader as *mut u8,
                std::mem::size_of::<NroHeader>(),
            )
        };
        header_bytes.copy_from_slice(&data[..std::mem::size_of::<NroHeader>()]);

        if nro_header.magic != make_magic(b'N', b'R', b'O', b'0') {
            return false;
        }

        // Build program image
        let aligned_size = page_align_size(nro_header.file_size) as usize;
        let mut program_image = vec![0u8; aligned_size];
        let copy_len = std::cmp::min(data.len(), program_image.len());
        program_image[..copy_len].copy_from_slice(&data[..copy_len]);

        if program_image.len() != aligned_size {
            return false;
        }

        // Default .bss to NRO header bss size if MOD0 section doesn't exist
        let mut bss_size = page_align_size(nro_header.bss_size);

        // Read MOD header
        let mod_offset = nro_header.module_header_offset as usize;
        if mod_offset + std::mem::size_of::<ModHeader>() <= program_image.len() {
            let mut mod_header = ModHeader::default();
            let mod_bytes = unsafe {
                std::slice::from_raw_parts_mut(
                    &mut mod_header as *mut ModHeader as *mut u8,
                    std::mem::size_of::<ModHeader>(),
                )
            };
            mod_bytes.copy_from_slice(
                &program_image[mod_offset..mod_offset + std::mem::size_of::<ModHeader>()],
            );

            if mod_header.magic == make_magic(b'M', b'O', b'D', b'0') {
                bss_size =
                    page_align_size(mod_header.bss_end_offset - mod_header.bss_start_offset);
            }
        }

        program_image.resize(program_image.len() + bss_size as usize, 0);

        // TODO: NCE patching, process setup, codeset loading.
        // The full implementation requires Kernel::CodeSet, KProcess::LoadFromMetadata,
        // KProcess::LoadModule, and NCE patcher integration.

        true
    }

    /// Load an NRO file into a process.
    ///
    /// Maps to upstream `AppLoader_NRO::LoadNro`.
    fn load_nro(
        &self,
        system: &mut System,
        process: &mut KProcess,
        nro_file: &dyn VfsFile,
    ) -> bool {
        let data = nro_file.read_all_bytes();
        Self::load_nro_impl(system, process, &data)
    }
}

impl AppLoader for AppLoaderNro {
    fn get_file_type(&self) -> FileType {
        Self::identify_type(&self.file)
    }

    /// Maps to upstream `AppLoader_NRO::Load`.
    fn load(&mut self, process: &mut KProcess, system: &mut System) -> LoadResult {
        if self.is_loaded {
            return (ResultStatus::ErrorAlreadyLoaded, None);
        }

        if !self.load_nro(system, process, self.file.as_ref()) {
            return (ResultStatus::ErrorLoadingNRO, None);
        }

        // TODO: ReadProgramId and register with FileSystemController.

        self.is_loaded = true;

        // TODO: Use actual Kernel::KThread::DefaultThreadPriority and Core::Memory::DEFAULT_STACK_SIZE
        const DEFAULT_THREAD_PRIORITY: i32 = 44;
        const DEFAULT_STACK_SIZE: u64 = 0x100000;

        (
            ResultStatus::Success,
            Some(LoadParameters {
                main_thread_priority: DEFAULT_THREAD_PRIORITY,
                main_thread_stack_size: DEFAULT_STACK_SIZE,
            }),
        )
    }

    fn read_icon(&self, buffer: &mut Vec<u8>) -> ResultStatus {
        if self.icon_data.is_empty() {
            return ResultStatus::ErrorNoIcon;
        }
        *buffer = self.icon_data.clone();
        ResultStatus::Success
    }

    fn read_program_id(&self, _out_program_id: &mut u64) -> ResultStatus {
        // TODO: Requires NACP to be ported.
        ResultStatus::ErrorNoControl
    }

    fn read_rom_fs(&self, out_file: &mut Option<VirtualFile>) -> ResultStatus {
        match &self.romfs {
            Some(f) => {
                *out_file = Some(f.clone());
                ResultStatus::Success
            }
            None => ResultStatus::ErrorNoRomFS,
        }
    }

    fn read_title(&self, _title: &mut String) -> ResultStatus {
        // TODO: Requires NACP to be ported.
        ResultStatus::ErrorNoControl
    }

    fn read_control_data(&self, _control: &mut NACP) -> ResultStatus {
        // TODO: Requires NACP to be ported.
        ResultStatus::ErrorNoControl
    }

    fn is_rom_fs_updatable(&self) -> bool {
        false
    }
}
