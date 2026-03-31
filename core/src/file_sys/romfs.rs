// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/romfs.h and romfs.cpp
// RomFS filesystem extraction and creation.

use std::sync::Arc;

use super::fsmitm_romfsbuild::RomFSBuildContext;
use super::vfs::vfs::{VfsDirectory, VfsFile};
use super::vfs::vfs_concat::ConcatenatedVfsFile;
use super::vfs::vfs_offset::OffsetVfsFile;
use super::vfs::vfs_types::{VirtualDir, VirtualFile};
use super::vfs::vfs_vector::VectorVfsDirectory;

// ============================================================================
// Constants and structures
// ============================================================================

const ROMFS_ENTRY_EMPTY: u32 = 0xFFFFFFFF;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct TableLocation {
    offset: u64,
    size: u64,
}

const _: () = assert!(std::mem::size_of::<TableLocation>() == 0x10);

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct RomFSHeader {
    header_size: u64,
    directory_hash: TableLocation,
    directory_meta: TableLocation,
    file_hash: TableLocation,
    file_meta: TableLocation,
    data_offset: u64,
}

const _: () = assert!(std::mem::size_of::<RomFSHeader>() == 0x50);

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct DirectoryEntry {
    parent: u32,
    sibling: u32,
    child_dir: u32,
    child_file: u32,
    hash: u32,
    name_length: u32,
}

const _: () = assert!(std::mem::size_of::<DirectoryEntry>() == 0x18);

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
struct FileEntry {
    parent: u32,
    sibling: u32,
    offset: u64,
    size: u64,
    hash: u32,
    name_length: u32,
}

const _: () = assert!(std::mem::size_of::<FileEntry>() == 0x20);

struct RomFSTraversalContext {
    header: RomFSHeader,
    file: VirtualFile,
    directory_meta: Vec<u8>,
    file_meta: Vec<u8>,
}

// ============================================================================
// Helpers
// ============================================================================

fn read_object_from_buf<T: Copy + Default>(data: &[u8], offset: usize) -> Option<T> {
    let size = std::mem::size_of::<T>();
    if offset + size > data.len() {
        return None;
    }
    let mut obj = T::default();
    unsafe {
        std::ptr::copy_nonoverlapping(
            data.as_ptr().add(offset),
            &mut obj as *mut T as *mut u8,
            size,
        );
    }
    Some(obj)
}

fn get_directory_entry(
    ctx: &RomFSTraversalContext,
    offset: usize,
) -> Option<(DirectoryEntry, String)> {
    let entry: DirectoryEntry = read_object_from_buf(&ctx.directory_meta, offset)?;
    let entry_end = offset + std::mem::size_of::<DirectoryEntry>();
    let name_length =
        (entry.name_length as usize).min(ctx.directory_meta.len().saturating_sub(entry_end));
    let name = String::from_utf8_lossy(&ctx.directory_meta[entry_end..entry_end + name_length])
        .into_owned();
    Some((entry, name))
}

fn get_file_entry(ctx: &RomFSTraversalContext, offset: usize) -> Option<(FileEntry, String)> {
    let entry: FileEntry = read_object_from_buf(&ctx.file_meta, offset)?;
    let entry_end = offset + std::mem::size_of::<FileEntry>();
    let name_length =
        (entry.name_length as usize).min(ctx.file_meta.len().saturating_sub(entry_end));
    let name =
        String::from_utf8_lossy(&ctx.file_meta[entry_end..entry_end + name_length]).into_owned();
    Some((entry, name))
}

fn process_file(
    ctx: &RomFSTraversalContext,
    mut this_file_offset: u32,
    parent: &VectorVfsDirectory,
) {
    while this_file_offset != ROMFS_ENTRY_EMPTY {
        let (entry, name) = match get_file_entry(ctx, this_file_offset as usize) {
            Some(v) => v,
            None => return,
        };

        parent.add_file(Arc::new(OffsetVfsFile::new(
            ctx.file.clone(),
            entry.size as usize,
            (entry.offset + ctx.header.data_offset) as usize,
            name,
        )));

        this_file_offset = entry.sibling;
    }
}

fn process_directory(
    ctx: &RomFSTraversalContext,
    mut this_dir_offset: u32,
    parent: &VectorVfsDirectory,
) {
    while this_dir_offset != ROMFS_ENTRY_EMPTY {
        let (entry, name) = match get_directory_entry(ctx, this_dir_offset as usize) {
            Some(v) => v,
            None => return,
        };

        let current = Arc::new(VectorVfsDirectory::new(vec![], vec![], name, None));

        if entry.child_file != ROMFS_ENTRY_EMPTY {
            process_file(ctx, entry.child_file, &current);
        }

        if entry.child_dir != ROMFS_ENTRY_EMPTY {
            process_directory(ctx, entry.child_dir, &current);
        }

        parent.add_directory(current);
        this_dir_offset = entry.sibling;
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Converts a RomFS binary blob to a VFS directory tree.
/// Returns None on failure.
///
/// Corresponds to upstream `ExtractRomFS`.
pub fn extract_romfs(file: Option<VirtualFile>) -> Option<VirtualDir> {
    let root_container = Arc::new(VectorVfsDirectory::new(vec![], vec![], String::new(), None));
    let file = match file {
        Some(f) => f,
        None => return Some(root_container),
    };

    let header_size = std::mem::size_of::<RomFSHeader>();
    let mut header_buf = vec![0u8; header_size];
    if file.read(&mut header_buf, header_size, 0) != header_size {
        return None;
    }

    let header: RomFSHeader = read_object_from_buf(&header_buf, 0)?;
    if header.header_size != header_size as u64 {
        return None;
    }

    let ctx = RomFSTraversalContext {
        header,
        file: file.clone(),
        directory_meta: file.read_bytes(
            header.directory_meta.size as usize,
            header.directory_meta.offset as usize,
        ),
        file_meta: file.read_bytes(
            header.file_meta.size as usize,
            header.file_meta.offset as usize,
        ),
    };

    process_directory(&ctx, 0, &root_container);

    // The root entry has an empty name; look for it as a subdirectory
    if let Some(root) = root_container.get_subdirectory("") {
        return Some(root);
    }

    // Should not reach here for valid RomFS
    None
}

/// Converts a VFS directory tree into a RomFS binary.
/// Returns None on failure.
///
/// Corresponds to upstream `CreateRomFS`.
pub fn create_romfs(dir: Option<VirtualDir>, ext: Option<VirtualDir>) -> Option<VirtualFile> {
    let dir = dir?;
    let name = dir.get_name();
    let mut ctx = RomFSBuildContext::new(dir, ext);
    let parts = ctx.build();
    ConcatenatedVfsFile::make_concatenated_file_with_filler(0, name, parts)
}
