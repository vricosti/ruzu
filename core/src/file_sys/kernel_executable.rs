// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/kernel_executable.h and kernel_executable.cpp
// KIP (Kernel Internal Process) and INI parsing.

use std::sync::Arc;

use super::partition_filesystem::ResultStatus;
use super::vfs::vfs::VfsFile;
use super::vfs::vfs_offset::OffsetVfsFile;
use super::vfs::vfs_types::VirtualFile;

// ============================================================================
// Constants
// ============================================================================

const fn make_magic(a: u8, b: u8, c: u8, d: u8) -> u32 {
    (a as u32) | ((b as u32) << 8) | ((c as u32) << 16) | ((d as u32) << 24)
}

const KIP1_MAGIC: u32 = make_magic(b'K', b'I', b'P', b'1');
const INI1_MAGIC: u32 = make_magic(b'I', b'N', b'I', b'1');
const INI_MAX_KIPS: u32 = 0x50;

// ============================================================================
// Binary structures
// ============================================================================

/// KIP section header — 0x10 bytes.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct KIPSectionHeader {
    pub offset: u32,
    pub decompressed_size: u32,
    pub compressed_size: u32,
    pub attribute: u32,
}

const _: () = assert!(std::mem::size_of::<KIPSectionHeader>() == 0x10);

/// KIP header — 0x100 bytes.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct KIPHeader {
    pub magic: u32,
    pub name: [u8; 0xC],
    pub title_id: u64,
    pub process_category: u32,
    pub main_thread_priority: u8,
    pub default_core: u8,
    pub _reserved: u8,
    pub flags: u8,
    pub sections: [KIPSectionHeader; 6],
    pub capabilities: [u32; 0x20],
}

const _: () = assert!(std::mem::size_of::<KIPHeader>() == 0x100);

impl Default for KIPHeader {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// INI header — 0x10 bytes.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct INIHeader {
    pub magic: u32,
    pub size: u32,
    pub kip_count: u32,
    pub _reserved: [u8; 4],
}

const _: () = assert!(std::mem::size_of::<INIHeader>() == 0x10);

// ============================================================================
// BLZ decompression
// ============================================================================

fn decompress_blz(data: &mut Vec<u8>) -> bool {
    if data.len() < 0xC {
        return false;
    }

    let data_size = data.len() - 0xC;

    let mut compressed_size = 0u32;
    let mut init_index = 0u32;
    let mut additional_size = 0u32;

    unsafe {
        std::ptr::copy_nonoverlapping(
            data.as_ptr().add(data_size),
            &mut compressed_size as *mut u32 as *mut u8,
            4,
        );
        std::ptr::copy_nonoverlapping(
            data.as_ptr().add(data_size + 4),
            &mut init_index as *mut u32 as *mut u8,
            4,
        );
        std::ptr::copy_nonoverlapping(
            data.as_ptr().add(data_size + 8),
            &mut additional_size as *mut u32 as *mut u8,
            4,
        );
    }

    let start_offset = data.len() - compressed_size as usize;
    data.resize(compressed_size as usize + additional_size as usize + start_offset, 0);

    let mut index = (compressed_size - init_index) as usize;
    let mut out_index = (compressed_size + additional_size) as usize;

    while out_index > 0 {
        if index == 0 {
            return false;
        }
        index -= 1;
        let control = data[index + start_offset];
        for i in 0..8u8 {
            if ((control << i) & 0x80) > 0 {
                if index < 2 {
                    return false;
                }
                index -= 2;
                let mut segment_offset =
                    data[index + start_offset] as usize | ((data[index + start_offset + 1] as usize) << 8);
                let mut segment_size = ((segment_offset >> 12) & 0xF) + 3;
                segment_offset &= 0xFFF;
                segment_offset += 3;

                if out_index < segment_size {
                    segment_size = out_index;
                }
                if out_index < segment_size {
                    return false;
                }

                out_index -= segment_size;

                for j in 0..segment_size {
                    if out_index + j + segment_offset + start_offset >= data.len() {
                        return false;
                    }
                    data[out_index + j + start_offset] =
                        data[out_index + j + segment_offset + start_offset];
                }
            } else {
                if out_index < 1 {
                    return false;
                }
                out_index -= 1;
                if index == 0 {
                    return false;
                }
                index -= 1;
                data[out_index + start_offset] = data[index + start_offset];
            }

            if out_index == 0 {
                break;
            }
        }
    }

    true
}

// ============================================================================
// KIP
// ============================================================================

/// Kernel Internal Process.
/// Corresponds to upstream `KIP`.
pub struct KIP {
    status: ResultStatus,
    header: KIPHeader,
    decompressed_sections: [Vec<u8>; 6],
}

/// Helper to read a repr(C) struct from a VfsFile at a given offset.
fn read_object<T: Copy>(file: &dyn VfsFile, offset: usize) -> Option<T> {
    let size = std::mem::size_of::<T>();
    let mut buf = vec![0u8; size];
    if file.read(&mut buf, size, offset) != size {
        return None;
    }
    unsafe { Some(std::ptr::read_unaligned(buf.as_ptr() as *const T)) }
}

impl KIP {
    pub fn new(file: &VirtualFile) -> Self {
        let empty_sections: [Vec<u8>; 6] = Default::default();
        let mut kip = Self {
            status: ResultStatus::Success,
            header: KIPHeader::default(),
            decompressed_sections: empty_sections,
        };

        let header_size = std::mem::size_of::<KIPHeader>();

        if file.get_size() < header_size {
            kip.status = ResultStatus::ErrorBadKIPHeader;
            return kip;
        }

        match read_object::<KIPHeader>(file.as_ref(), 0) {
            Some(hdr) => kip.header = hdr,
            None => {
                kip.status = ResultStatus::ErrorBadKIPHeader;
                return kip;
            }
        }

        if kip.header.magic != KIP1_MAGIC {
            kip.status = ResultStatus::ErrorBadKIPHeader;
            return kip;
        }

        let mut offset = header_size as u64;
        for i in 0..kip.header.sections.len() {
            let section = &kip.header.sections[i];
            let compressed = file.read_bytes(section.compressed_size as usize, offset as usize);
            offset += section.compressed_size as u64;

            if section.compressed_size == 0 && section.decompressed_size != 0 {
                kip.decompressed_sections[i] = vec![0u8; section.decompressed_size as usize];
            } else if section.compressed_size == section.decompressed_size {
                kip.decompressed_sections[i] = compressed;
            } else {
                kip.decompressed_sections[i] = compressed;
                if !decompress_blz(&mut kip.decompressed_sections[i]) {
                    kip.status = ResultStatus::ErrorBLZDecompressionFailed;
                    return kip;
                }
            }
        }

        kip
    }

    pub fn get_status(&self) -> ResultStatus {
        self.status
    }

    pub fn get_name(&self) -> String {
        let end = self.header.name.iter().position(|&b| b == 0).unwrap_or(self.header.name.len());
        String::from_utf8_lossy(&self.header.name[..end]).into_owned()
    }

    pub fn get_title_id(&self) -> u64 {
        self.header.title_id
    }

    pub fn get_section_decompressed(&self, index: u8) -> Vec<u8> {
        self.decompressed_sections[index as usize].clone()
    }

    pub fn is_64_bit(&self) -> bool {
        (self.header.flags & 0x8) != 0
    }

    pub fn is_39_bit_address_space(&self) -> bool {
        (self.header.flags & 0x10) != 0
    }

    pub fn is_service(&self) -> bool {
        (self.header.flags & 0x20) != 0
    }

    pub fn get_kernel_capabilities(&self) -> Vec<u32> {
        self.header.capabilities.to_vec()
    }

    pub fn get_main_thread_priority(&self) -> i32 {
        self.header.main_thread_priority as i32
    }

    pub fn get_main_thread_stack_size(&self) -> u32 {
        self.header.sections[1].attribute
    }

    pub fn get_main_thread_cpu_core(&self) -> u32 {
        self.header.default_core as u32
    }

    pub fn get_text_section(&self) -> &[u8] {
        &self.decompressed_sections[0]
    }

    pub fn get_rodata_section(&self) -> &[u8] {
        &self.decompressed_sections[1]
    }

    pub fn get_data_section(&self) -> &[u8] {
        &self.decompressed_sections[2]
    }

    pub fn get_text_offset(&self) -> u32 {
        self.header.sections[0].offset
    }

    pub fn get_rodata_offset(&self) -> u32 {
        self.header.sections[1].offset
    }

    pub fn get_data_offset(&self) -> u32 {
        self.header.sections[2].offset
    }

    pub fn get_bss_size(&self) -> u32 {
        self.header.sections[3].decompressed_size
    }

    pub fn get_bss_offset(&self) -> u32 {
        self.header.sections[3].offset
    }
}

// ============================================================================
// INI
// ============================================================================

/// Initial process binary (collection of KIPs).
/// Corresponds to upstream `INI`.
pub struct INI {
    status: ResultStatus,
    header: INIHeader,
    kips: Vec<KIP>,
}

impl INI {
    pub fn new(file: &VirtualFile) -> Self {
        let header_size = std::mem::size_of::<INIHeader>();
        let mut ini = Self {
            status: ResultStatus::Success,
            header: INIHeader::default(),
            kips: Vec::new(),
        };

        if file.get_size() < header_size {
            ini.status = ResultStatus::ErrorBadINIHeader;
            return ini;
        }

        match read_object::<INIHeader>(file.as_ref(), 0) {
            Some(hdr) => ini.header = hdr,
            None => {
                ini.status = ResultStatus::ErrorBadINIHeader;
                return ini;
            }
        }

        if ini.header.magic != INI1_MAGIC {
            ini.status = ResultStatus::ErrorBadINIHeader;
            return ini;
        }

        if ini.header.kip_count > INI_MAX_KIPS {
            ini.status = ResultStatus::ErrorINITooManyKIPs;
            return ini;
        }

        let mut offset = header_size as u64;
        for _i in 0..ini.header.kip_count {
            let kip_file: VirtualFile = Arc::new(OffsetVfsFile::new(
                file.clone(),
                file.get_size() - offset as usize,
                offset as usize,
                String::new(),
            ));
            let kip = KIP::new(&kip_file);
            if kip.get_status() == ResultStatus::Success {
                ini.kips.push(kip);
            }
        }

        ini
    }

    pub fn get_status(&self) -> ResultStatus {
        self.status
    }

    pub fn get_kips(&self) -> &[KIP] {
        &self.kips
    }
}
