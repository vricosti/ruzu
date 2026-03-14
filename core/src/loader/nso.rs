// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/loader/nso.h and nso.cpp
//!
//! NSO (Nintendo Shared Object) loader.

use std::collections::BTreeMap;

use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::VirtualFile;
use crate::hle::kernel::code_set::CodeSet;

use super::loader::{
    make_magic, AppLoader, FileType, FileTypeIdentifier, KProcess, LoadParameters, LoadResult,
    Modules, ResultStatus, System, NACP,
};

// ============================================================================
// NSOSegmentHeader
// ============================================================================

/// Segment header within an NSO file.
///
/// Maps to upstream `Loader::NSOSegmentHeader`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NsoSegmentHeader {
    pub offset: u32,
    pub location: u32,
    pub size: u32,
    /// Union in C++: alignment or bss_size depending on context.
    pub alignment_or_bss_size: u32,
}

const _: () = assert!(std::mem::size_of::<NsoSegmentHeader>() == 0x10);

// ============================================================================
// NSOHeader
// ============================================================================

/// RoData-relative extent descriptor.
///
/// Maps to upstream `NSOHeader::RODataRelativeExtent`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct RoDataRelativeExtent {
    pub data_offset: u32,
    pub size: u32,
}

/// SHA-256 hash type.
pub type Sha256Hash = [u8; 0x20];

/// NSO file header.
///
/// Maps to upstream `Loader::NSOHeader`.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NsoHeader {
    pub magic: u32,
    pub version: u32,
    pub reserved: u32,
    pub flags: u32,
    /// Text, RoData, Data segments (in that order).
    pub segments: [NsoSegmentHeader; 3],
    pub build_id: [u8; 0x20],
    pub segments_compressed_size: [u32; 3],
    pub padding: [u8; 0x1C],
    pub api_info_extent: RoDataRelativeExtent,
    pub dynstr_extent: RoDataRelativeExtent,
    pub dynsyn_extent: RoDataRelativeExtent,
    pub segment_hashes: [Sha256Hash; 3],
}

const _: () = assert!(std::mem::size_of::<NsoHeader>() == 0x100);

impl Default for NsoHeader {
    fn default() -> Self {
        // Safety: NsoHeader is repr(C) and all-zeros is valid.
        unsafe { std::mem::zeroed() }
    }
}

impl NsoHeader {
    /// Check if a specific segment is compressed.
    ///
    /// Maps to upstream `NSOHeader::IsSegmentCompressed`.
    pub fn is_segment_compressed(&self, segment_num: usize) -> bool {
        assert!(segment_num < 3, "Invalid segment {}", segment_num);
        ((self.flags >> segment_num) & 1) != 0
    }
}

// ============================================================================
// NSO_ARGUMENT_DATA_ALLOCATION_SIZE
// ============================================================================

/// Maps to upstream `NSO_ARGUMENT_DATA_ALLOCATION_SIZE`.
pub const NSO_ARGUMENT_DATA_ALLOCATION_SIZE: u32 = 0x9000;

// ============================================================================
// NSOArgumentHeader
// ============================================================================

/// NSO argument header.
///
/// Maps to upstream `Loader::NSOArgumentHeader`.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NsoArgumentHeader {
    pub allocated_size: u32,
    pub actual_size: u32,
    pub _padding: [u8; 0x18],
}

const _: () = assert!(std::mem::size_of::<NsoArgumentHeader>() == 0x20);

impl Default for NsoArgumentHeader {
    fn default() -> Self {
        Self {
            allocated_size: 0,
            actual_size: 0,
            _padding: [0u8; 0x18],
        }
    }
}

// ============================================================================
// MODHeader (private to nso.cpp)
// ============================================================================

/// MOD header found within NSO modules.
///
/// Maps to upstream anonymous `MODHeader` struct in nso.cpp.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct ModHeader {
    magic: u32,
    dynamic_offset: u32,
    bss_start_offset: u32,
    bss_end_offset: u32,
    eh_frame_hdr_start_offset: u32,
    eh_frame_hdr_end_offset: u32,
    /// Offset to runtime-generated module object. Typically equal to .bss base.
    module_offset: u32,
}

const _: () = assert!(std::mem::size_of::<ModHeader>() == 0x1c);

// ============================================================================
// Helper functions
// ============================================================================

/// Page-align a size value.
///
/// Maps to upstream anonymous `PageAlignSize`.
const YUZU_PAGEMASK: u32 = 0xFFF;

const fn page_align_size(size: u32) -> u32 {
    (size + YUZU_PAGEMASK) & !YUZU_PAGEMASK
}

/// Decompress a segment using LZ4.
///
/// Maps to upstream `DecompressSegment`.
fn decompress_segment(compressed_data: &[u8], expected_size: u32) -> Vec<u8> {
    let mut result = vec![0u8; expected_size as usize];
    match lz4_flex::block::decompress_into(compressed_data, &mut result) {
        Ok(written) => {
            if written != expected_size as usize {
                log::warn!(
                    "NSO LZ4 decompression: expected {} bytes, got {}",
                    expected_size, written
                );
            }
        }
        Err(e) => {
            log::error!("NSO LZ4 decompression failed: {}", e);
            // Return zeroed buffer on failure
            result.fill(0);
        }
    }
    result
}

/// Read a plain-old-data struct from a VfsFile at the given offset.
///
/// Safety: T must be repr(C) and valid when zero-initialized.
pub fn read_object<T: Copy + Default>(file: &dyn VfsFile, offset: usize) -> Option<T> {
    let size = std::mem::size_of::<T>();
    let mut obj = T::default();
    let bytes =
        unsafe { std::slice::from_raw_parts_mut(&mut obj as *mut T as *mut u8, size) };
    let read = file.read(bytes, size, offset);
    if read == size {
        Some(obj)
    } else {
        None
    }
}

// ============================================================================
// AppLoaderNso
// ============================================================================

/// Loads an NSO file.
///
/// Maps to upstream `Loader::AppLoader_NSO`.
pub struct AppLoaderNso {
    file: VirtualFile,
    is_loaded: bool,
    modules: Modules,
}

impl FileTypeIdentifier for AppLoaderNso {
    /// Identifies whether or not the given file is a form of NSO file.
    ///
    /// Maps to upstream `AppLoader_NSO::IdentifyType`.
    fn identify_type(in_file: &VirtualFile) -> FileType {
        let magic: Option<u32> = read_object::<u32>(in_file.as_ref(), 0);
        match magic {
            Some(m) if m == make_magic(b'N', b'S', b'O', b'0') => FileType::NSO,
            _ => FileType::Error,
        }
    }
}

impl AppLoaderNso {
    pub fn new(file: VirtualFile) -> Self {
        Self {
            file,
            is_loaded: false,
            modules: BTreeMap::new(),
        }
    }

    /// Load an NSO module into a process at the specified base address.
    ///
    /// Maps to upstream `AppLoader_NSO::LoadModule`.
    ///
    /// TODO: This is a simplified version. The full implementation requires:
    /// - Kernel::CodeSet, Kernel::PhysicalMemory
    /// - FileSys::PatchManager
    /// - Core::NCE::Patcher (NCE backend)
    /// - Cheat system integration
    /// - Process memory mapping
    pub fn load_module(
        process: &mut KProcess,
        _system: &mut System,
        nso_file: &dyn VfsFile,
        load_base: u64,
        should_pass_arguments: bool,
        load_into_process: bool,
    ) -> Option<u64> {
        if nso_file.get_size() < std::mem::size_of::<NsoHeader>() {
            return None;
        }

        let nso_header: NsoHeader = read_object(nso_file, 0)?;

        if nso_header.magic != make_magic(b'N', b'S', b'O', b'0') {
            return None;
        }

        // Build program image and codeset metadata.
        let mut code_set = CodeSet::new();
        let mut program_image = Vec::new();

        for i in 0..3 {
            let mut data = nso_file.read_bytes(
                nso_header.segments_compressed_size[i] as usize,
                nso_header.segments[i].offset as usize,
            );
            if nso_header.is_segment_compressed(i) {
                data = decompress_segment(&data, nso_header.segments[i].size);
            }
            let needed_size =
                nso_header.segments[i].location as usize + data.len();
            if program_image.len() < needed_size {
                program_image.resize(needed_size, 0);
            }
            let loc = nso_header.segments[i].location as usize;
            program_image[loc..loc + data.len()].copy_from_slice(&data);

            code_set.segments[i].addr = nso_header.segments[i].location as u64;
            code_set.segments[i].offset = loc;
            code_set.segments[i].size = nso_header.segments[i].size;
        }

        // Add BSS
        let bss_size = nso_header.segments[2].alignment_or_bss_size;
        let image_size = page_align_size(program_image.len() as u32 + bss_size);
        program_image.resize(image_size as usize, 0);
        code_set.data_segment_mut().size += bss_size;

        for segment in &mut code_set.segments {
            segment.size = page_align_size(segment.size);
        }

        // Allocate argument data if requested AND there are actual arguments.
        // Upstream: only allocates when `!Settings::values.program_args.GetValue().empty()`.
        // Since we don't pass program_args yet, skip this to match upstream module layout.
        // TODO: Pass actual program_args through when Settings is ported.
        if should_pass_arguments && false /* no program_args yet */ {
            let arg_start = program_image.len();
            program_image.resize(arg_start + NSO_ARGUMENT_DATA_ALLOCATION_SIZE as usize, 0);
            // Write NsoArgumentHeader at arg_start
            let arg_header = NsoArgumentHeader {
                allocated_size: NSO_ARGUMENT_DATA_ALLOCATION_SIZE,
                actual_size: 0, // no arguments
                _padding: [0u8; 0x18],
            };
            let header_bytes = unsafe {
                std::slice::from_raw_parts(
                    &arg_header as *const NsoArgumentHeader as *const u8,
                    std::mem::size_of::<NsoArgumentHeader>(),
                )
            };
            program_image[arg_start..arg_start + header_bytes.len()]
                .copy_from_slice(header_bytes);
        }

        // TODO: Apply patches (PatchManager), NCE patching, cheats, and
        // actually load into process memory via codeset.

        if load_into_process {
            code_set.memory = program_image;
            process.load_module(code_set, load_base);
            log::info!(
                "NSO: loaded {} bytes at {:#X}",
                image_size,
                load_base
            );
        }

        Some(load_base + image_size as u64)
    }
}

impl AppLoader for AppLoaderNso {
    fn get_file_type(&self) -> FileType {
        Self::identify_type(&self.file)
    }

    /// Maps to upstream `AppLoader_NSO::Load`.
    fn load(&mut self, process: &mut KProcess, system: &mut System) -> LoadResult {
        if self.is_loaded {
            return (ResultStatus::ErrorAlreadyLoaded, None);
        }

        self.modules.clear();

        // TODO: Obtain base_address from process.GetEntryPoint()
        let base_address: u64 = 0;

        let result = Self::load_module(
            process,
            system,
            self.file.as_ref(),
            base_address,
            true,
            true,
        );

        if result.is_none() {
            return (ResultStatus::ErrorLoadingNSO, None);
        }

        self.modules
            .insert(base_address, self.file.get_name());
        log::debug!(
            "loaded module {} @ {:#X}",
            self.file.get_name(),
            base_address
        );

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

    fn read_nso_modules(&self, modules: &mut Modules) -> ResultStatus {
        *modules = self.modules.clone();
        ResultStatus::Success
    }
}
