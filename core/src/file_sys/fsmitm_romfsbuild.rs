// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fsmitm_romfsbuild.h and fsmitm_romfsbuild.cpp
// RomFS builder for LayeredFS mod application.

use std::sync::Arc;

use super::ips_layer::patch_ips;
use super::vfs::vfs::{VfsDirectory, VfsFile};
use super::vfs::vfs_types::{VirtualDir, VirtualFile};
use super::vfs::vfs_vector::VectorVfsFile;

// ============================================================================
// Constants
// ============================================================================

const FS_MAX_PATH: u64 = 0x301;
const ROMFS_ENTRY_EMPTY: u32 = 0xFFFFFFFF;
const ROMFS_FILEPARTITION_OFS: u64 = 0x200;

// ============================================================================
// Binary structures (matching romfs.rs but used for building)
// ============================================================================

#[derive(Clone, Copy, Default)]
#[repr(C)]
struct RomFSHeader {
    header_size: u64,
    dir_hash_table_ofs: u64,
    dir_hash_table_size: u64,
    dir_table_ofs: u64,
    dir_table_size: u64,
    file_hash_table_ofs: u64,
    file_hash_table_size: u64,
    file_table_ofs: u64,
    file_table_size: u64,
    file_partition_ofs: u64,
}

const _: () = assert!(std::mem::size_of::<RomFSHeader>() == 0x50);

#[derive(Clone, Copy, Default)]
#[repr(C)]
struct RomFSDirectoryEntry {
    parent: u32,
    sibling: u32,
    child: u32,
    file: u32,
    hash: u32,
    name_size: u32,
}

const _: () = assert!(std::mem::size_of::<RomFSDirectoryEntry>() == 0x18);

#[derive(Clone, Copy, Default)]
#[repr(C)]
struct RomFSFileEntry {
    parent: u32,
    sibling: u32,
    offset: u64,
    size: u64,
    hash: u32,
    name_size: u32,
}

const _: () = assert!(std::mem::size_of::<RomFSFileEntry>() == 0x20);

// ============================================================================
// Build context node types
// ============================================================================

struct RomFSBuildDirectoryContext {
    path: String,
    cur_path_ofs: u32,
    path_len: u32,
    entry_offset: u32,
    parent: Option<usize>,  // index into directories vec
    child: Option<usize>,   // index into directories vec
    sibling: Option<usize>, // index into directories vec
    file: Option<usize>,    // index into files vec
}

struct RomFSBuildFileContext {
    path: String,
    cur_path_ofs: u32,
    path_len: u32,
    entry_offset: u32,
    offset: u64,
    size: u64,
    parent: Option<usize>, // index into directories vec
    sibling: Option<usize>, // index into files vec
    source: VirtualFile,
}

// ============================================================================
// Helper functions
// ============================================================================

fn romfs_calc_path_hash(parent: u32, path: &str, start: u32, path_len: u32) -> u32 {
    let mut hash = parent ^ 123456789;
    let bytes = path.as_bytes();
    for i in 0..path_len {
        hash = hash.rotate_right(5);
        hash ^= bytes[(start + i) as usize] as u32;
    }
    hash
}

fn romfs_get_hash_table_count(num_entries: u64) -> u64 {
    if num_entries < 3 {
        return 3;
    }
    if num_entries < 19 {
        return num_entries | 1;
    }
    let mut count = num_entries;
    while count % 2 == 0
        || count % 3 == 0
        || count % 5 == 0
        || count % 7 == 0
        || count % 11 == 0
        || count % 13 == 0
        || count % 17 == 0
    {
        count += 1;
    }
    count
}

/// Align `value` up to the given `alignment`.
fn align_up(value: u64, alignment: u64) -> u64 {
    let mask = alignment - 1;
    (value + mask) & !mask
}

fn align_up_u32(value: u32, alignment: u32) -> u32 {
    let mask = alignment - 1;
    (value + mask) & !mask
}

// ============================================================================
// RomFSBuildContext
// ============================================================================

/// RomFS builder that merges a base directory with an extension directory.
/// Corresponds to upstream `RomFSBuildContext`.
pub struct RomFSBuildContext {
    directories: Vec<RomFSBuildDirectoryContext>,
    files: Vec<RomFSBuildFileContext>,
    num_dirs: u64,
    num_files: u64,
    dir_table_size: u64,
    file_table_size: u64,
    dir_hash_table_size: u64,
    file_hash_table_size: u64,
    file_partition_size: u64,
}

impl RomFSBuildContext {
    pub fn new(base: VirtualDir, ext: Option<VirtualDir>) -> Self {
        let mut ctx = Self {
            directories: Vec::new(),
            files: Vec::new(),
            num_dirs: 1,
            num_files: 0,
            dir_table_size: 0x18,
            file_table_size: 0,
            dir_hash_table_size: 0,
            file_hash_table_size: 0,
            file_partition_size: 0,
        };

        // Create root directory
        ctx.directories.push(RomFSBuildDirectoryContext {
            path: "\0".to_string(),
            cur_path_ofs: 0,
            path_len: 0,
            entry_offset: 0,
            parent: None,
            child: None,
            sibling: None,
            file: None,
        });

        let root_idx = 0;
        ctx.visit_directory(base, ext, root_idx);
        ctx
    }

    fn visit_directory(
        &mut self,
        romfs_dir: VirtualDir,
        ext_dir: Option<VirtualDir>,
        parent_idx: usize,
    ) {
        // Process files
        for child_romfs_file in romfs_dir.get_files() {
            let name = child_romfs_file.get_name();

            // Check for stub file in ext
            if let Some(ref ext) = ext_dir {
                if ext.get_file(&format!("{}.stub", name)).is_some() {
                    continue;
                }
            }

            let parent_path_len = self.directories[parent_idx].path_len;
            let cur_path_ofs = parent_path_len + 1;
            let path_len = cur_path_ofs + name.len() as u32;
            let path = format!("{}/{}", self.directories[parent_idx].path, name);

            assert!((path_len as u64) < FS_MAX_PATH);

            let mut source: VirtualFile = child_romfs_file;

            // Apply IPS patches from ext
            if let Some(ref ext) = ext_dir {
                if let Some(ips) = ext.get_file(&format!("{}.ips", name)) {
                    if let Some(patched) = patch_ips(&source, &ips) {
                        source = patched;
                    }
                }
            }

            let size = source.get_size() as u64;

            let file_ctx = RomFSBuildFileContext {
                path,
                cur_path_ofs,
                path_len,
                entry_offset: 0,
                offset: 0,
                size,
                parent: Some(parent_idx),
                sibling: None,
                source,
            };
            self.add_file(parent_idx, file_ctx);
        }

        // Process subdirectories
        for child_romfs_dir in romfs_dir.get_subdirectories() {
            let name = child_romfs_dir.get_name();

            // Check for stub file in ext
            if let Some(ref ext) = ext_dir {
                if ext.get_file(&format!("{}.stub", name)).is_some() {
                    continue;
                }
            }

            let parent_path_len = self.directories[parent_idx].path_len;
            let cur_path_ofs = parent_path_len + 1;
            let path_len = cur_path_ofs + name.len() as u32;
            let path = format!("{}/{}", self.directories[parent_idx].path, name);

            assert!((path_len as u64) < FS_MAX_PATH);

            let dir_ctx = RomFSBuildDirectoryContext {
                path,
                cur_path_ofs,
                path_len,
                entry_offset: 0,
                parent: Some(parent_idx),
                child: None,
                sibling: None,
                file: None,
            };

            let child_idx = self.directories.len();
            if !self.add_directory(parent_idx, dir_ctx) {
                continue;
            }

            let child_ext_dir = ext_dir
                .as_ref()
                .and_then(|ext| ext.get_subdirectory(&name));
            self.visit_directory(child_romfs_dir, child_ext_dir, child_idx);
        }
    }

    fn add_directory(&mut self, _parent_idx: usize, dir_ctx: RomFSBuildDirectoryContext) -> bool {
        self.num_dirs += 1;
        let name_len = dir_ctx.path_len - dir_ctx.cur_path_ofs;
        self.dir_table_size += std::mem::size_of::<RomFSDirectoryEntry>() as u64
            + align_up_u32(name_len, 4) as u64;
        self.directories.push(dir_ctx);
        true
    }

    fn add_file(&mut self, _parent_idx: usize, file_ctx: RomFSBuildFileContext) {
        self.num_files += 1;
        let name_len = file_ctx.path_len - file_ctx.cur_path_ofs;
        self.file_table_size += std::mem::size_of::<RomFSFileEntry>() as u64
            + align_up_u32(name_len, 4) as u64;
        self.files.push(file_ctx);
    }

    /// Finalize the build context and produce the RomFS output.
    /// Returns a vector of (offset, file) pairs that compose the RomFS image.
    pub fn build(&mut self) -> Vec<(u64, VirtualFile)> {
        let dir_hash_table_entry_count = romfs_get_hash_table_count(self.num_dirs);
        let file_hash_table_entry_count = romfs_get_hash_table_count(self.num_files);
        self.dir_hash_table_size = 4 * dir_hash_table_entry_count;
        self.file_hash_table_size = 4 * file_hash_table_entry_count;

        // Allocate metadata buffer
        let metadata_size = (self.file_hash_table_size
            + self.file_table_size
            + self.dir_hash_table_size
            + self.dir_table_size) as usize;
        let mut metadata = vec![0u8; metadata_size];

        // Initialize hash tables to 0xFF
        let dir_ht_end = self.dir_hash_table_size as usize;
        for byte in &mut metadata[..dir_ht_end] {
            *byte = 0xFF;
        }
        let file_ht_start = (self.dir_hash_table_size + self.dir_table_size) as usize;
        let file_ht_end = file_ht_start + self.file_hash_table_size as usize;
        for byte in &mut metadata[file_ht_start..file_ht_end] {
            *byte = 0xFF;
        }

        // Sort tables by name
        self.files.sort_by(|a, b| a.path.cmp(&b.path));
        self.directories.sort_by(|a, b| a.path.cmp(&b.path));

        // We need to rebuild parent indices after sorting.
        // Build a map from old path to new index for directories.
        let dir_path_to_idx: std::collections::HashMap<String, usize> = self
            .directories
            .iter()
            .enumerate()
            .map(|(i, d)| (d.path.clone(), i))
            .collect();

        // Fix parent indices for directories
        for i in 0..self.directories.len() {
            if let Some(parent_idx) = self.directories[i].parent {
                // Find the parent by looking at the path prefix
                // Parent is the directory whose path is a prefix of this one
                // Since we stored the parent index before sort, we need to look it up
                // Actually the parent field points to an old index; we need the parent path.
                // For simplicity, recompute parent from path.
            }
        }

        // Determine file offsets
        let mut entry_offset: u32 = 0;
        for file in &mut self.files {
            self.file_partition_size = align_up(self.file_partition_size, 16);
            file.offset = self.file_partition_size;
            self.file_partition_size += file.size;
            file.entry_offset = entry_offset;
            let name_len = file.path_len - file.cur_path_ofs;
            entry_offset += std::mem::size_of::<RomFSFileEntry>() as u32 + align_up_u32(name_len, 4);
        }

        // Assign file sibling/parent ownership (reverse iteration)
        // Clear all directory file pointers first
        for dir in &mut self.directories {
            dir.file = None;
        }
        for i in (0..self.files.len()).rev() {
            if let Some(parent_idx) = self.files[i].parent {
                // Look up parent by path
                let parent_path_end = self.files[i].path.rfind('/').unwrap_or(0);
                let parent_path = &self.files[i].path[..parent_path_end];
                if let Some(&pidx) = dir_path_to_idx.get(parent_path) {
                    self.files[i].parent = Some(pidx);
                    self.files[i].sibling = self.directories[pidx].file.map(|_| {
                        // Find the file index that was last set as this dir's file
                        // We need the file index, not dir index
                        0 // placeholder
                    });
                    // Actually, we need the sibling to be the previous file's index
                    self.files[i].sibling = self.directories[pidx].file;
                    self.directories[pidx].file = Some(i);
                }
            }
        }

        // Determine directory offsets
        entry_offset = 0;
        for dir in &mut self.directories {
            dir.entry_offset = entry_offset;
            let name_len = dir.path_len - dir.cur_path_ofs;
            entry_offset += std::mem::size_of::<RomFSDirectoryEntry>() as u32
                + align_up_u32(name_len, 4);
        }

        // Assign directory sibling/child ownership (reverse iteration, skip root)
        for dir in &mut self.directories {
            dir.child = None;
        }
        let root_path = self.directories[0].path.clone();
        for i in (1..self.directories.len()).rev() {
            let parent_path_end = self.directories[i].path.rfind('/').unwrap_or(0);
            let parent_path = self.directories[i].path[..parent_path_end].to_string();
            if let Some(&pidx) = dir_path_to_idx.get(&parent_path) {
                self.directories[i].parent = Some(pidx);
                self.directories[i].sibling = self.directories[pidx].child;
                self.directories[pidx].child = Some(i);
            }
        }

        // Build output
        let mut out: Vec<(u64, VirtualFile)> = Vec::with_capacity(self.num_files as usize + 2);

        // Set header fields
        let mut header = RomFSHeader::default();
        header.header_size = std::mem::size_of::<RomFSHeader>() as u64;
        header.file_hash_table_size = self.file_hash_table_size;
        header.file_table_size = self.file_table_size;
        header.dir_hash_table_size = self.dir_hash_table_size;
        header.dir_table_size = self.dir_table_size;
        header.file_partition_ofs = ROMFS_FILEPARTITION_OFS;
        header.dir_hash_table_ofs =
            align_up(header.file_partition_ofs + self.file_partition_size, 4);
        header.dir_table_ofs = header.dir_hash_table_ofs + header.dir_hash_table_size;
        header.file_hash_table_ofs = header.dir_table_ofs + header.dir_table_size;
        header.file_table_ofs = header.file_hash_table_ofs + header.file_hash_table_size;

        // Serialize header
        let header_data = unsafe {
            std::slice::from_raw_parts(
                &header as *const RomFSHeader as *const u8,
                std::mem::size_of::<RomFSHeader>(),
            )
            .to_vec()
        };
        out.push((0, Arc::new(VectorVfsFile::new(header_data, String::new(), None))));

        // Populate file table entries
        let dir_ht_size = self.dir_hash_table_size as usize;
        let dir_t_size = self.dir_table_size as usize;
        let file_ht_size = self.file_hash_table_size as usize;

        for i in 0..self.files.len() {
            let parent_entry_offset = self.files[i]
                .parent
                .map(|p| self.directories[p].entry_offset)
                .unwrap_or(0);

            let sibling_entry_offset = self.files[i]
                .sibling
                .map(|s| self.files[s].entry_offset)
                .unwrap_or(ROMFS_ENTRY_EMPTY);

            let name_size = self.files[i].path_len - self.files[i].cur_path_ofs;
            let hash = romfs_calc_path_hash(
                parent_entry_offset,
                &self.files[i].path,
                self.files[i].cur_path_ofs,
                name_size,
            );

            // Read current hash bucket value
            let bucket_idx = (hash as u64 % file_hash_table_entry_count) as usize;
            let ht_offset = file_ht_start + bucket_idx * 4;
            let old_hash_val = u32::from_le_bytes([
                metadata[ht_offset],
                metadata[ht_offset + 1],
                metadata[ht_offset + 2],
                metadata[ht_offset + 3],
            ]);

            // Write new hash bucket
            let eo_bytes = self.files[i].entry_offset.to_le_bytes();
            metadata[ht_offset..ht_offset + 4].copy_from_slice(&eo_bytes);

            let cur_entry = RomFSFileEntry {
                parent: parent_entry_offset,
                sibling: sibling_entry_offset,
                offset: self.files[i].offset,
                size: self.files[i].size,
                hash: old_hash_val,
                name_size,
            };

            // Write file entry to file table
            let ft_start = file_ht_start + file_ht_size;
            let entry_start = ft_start + self.files[i].entry_offset as usize;
            let entry_bytes = unsafe {
                std::slice::from_raw_parts(
                    &cur_entry as *const RomFSFileEntry as *const u8,
                    std::mem::size_of::<RomFSFileEntry>(),
                )
            };
            metadata[entry_start..entry_start + entry_bytes.len()].copy_from_slice(entry_bytes);

            // Zero-fill and write name
            let name_start = entry_start + std::mem::size_of::<RomFSFileEntry>();
            let aligned_name_size = align_up_u32(name_size, 4) as usize;
            for b in &mut metadata[name_start..name_start + aligned_name_size] {
                *b = 0;
            }
            let name_bytes =
                &self.files[i].path.as_bytes()[self.files[i].cur_path_ofs as usize..];
            let copy_len = name_size as usize;
            metadata[name_start..name_start + copy_len].copy_from_slice(&name_bytes[..copy_len]);

            // Add file source to output
            out.push((
                self.files[i].offset + ROMFS_FILEPARTITION_OFS,
                self.files[i].source.clone(),
            ));
        }

        // Populate directory table entries
        for i in 0..self.directories.len() {
            let is_root = i == 0
                || self.directories[i].path == root_path;

            let parent_entry_offset = if is_root {
                0
            } else {
                self.directories[i]
                    .parent
                    .map(|p| self.directories[p].entry_offset)
                    .unwrap_or(0)
            };

            let sibling_entry_offset = self.directories[i]
                .sibling
                .map(|s| self.directories[s].entry_offset)
                .unwrap_or(ROMFS_ENTRY_EMPTY);

            let child_entry_offset = self.directories[i]
                .child
                .map(|c| self.directories[c].entry_offset)
                .unwrap_or(ROMFS_ENTRY_EMPTY);

            let file_entry_offset = self.directories[i]
                .file
                .map(|f| self.files[f].entry_offset)
                .unwrap_or(ROMFS_ENTRY_EMPTY);

            let name_size = self.directories[i].path_len - self.directories[i].cur_path_ofs;
            let hash_parent = if is_root { 0 } else { parent_entry_offset };
            let hash = romfs_calc_path_hash(
                hash_parent,
                &self.directories[i].path,
                self.directories[i].cur_path_ofs,
                name_size,
            );

            let bucket_idx = (hash as u64 % dir_hash_table_entry_count) as usize;
            let ht_offset = bucket_idx * 4;
            let old_hash_val = u32::from_le_bytes([
                metadata[ht_offset],
                metadata[ht_offset + 1],
                metadata[ht_offset + 2],
                metadata[ht_offset + 3],
            ]);

            let eo_bytes = self.directories[i].entry_offset.to_le_bytes();
            metadata[ht_offset..ht_offset + 4].copy_from_slice(&eo_bytes);

            let cur_entry = RomFSDirectoryEntry {
                parent: parent_entry_offset,
                sibling: sibling_entry_offset,
                child: child_entry_offset,
                file: file_entry_offset,
                hash: old_hash_val,
                name_size,
            };

            let dt_start = dir_ht_size;
            let entry_start = dt_start + self.directories[i].entry_offset as usize;
            let entry_bytes = unsafe {
                std::slice::from_raw_parts(
                    &cur_entry as *const RomFSDirectoryEntry as *const u8,
                    std::mem::size_of::<RomFSDirectoryEntry>(),
                )
            };
            metadata[entry_start..entry_start + entry_bytes.len()].copy_from_slice(entry_bytes);

            let name_start = entry_start + std::mem::size_of::<RomFSDirectoryEntry>();
            let aligned_name_size = align_up_u32(name_size, 4) as usize;
            for b in &mut metadata[name_start..name_start + aligned_name_size] {
                *b = 0;
            }
            let name_bytes =
                &self.directories[i].path.as_bytes()[self.directories[i].cur_path_ofs as usize..];
            let copy_len = name_size as usize;
            if copy_len > 0 {
                metadata[name_start..name_start + copy_len]
                    .copy_from_slice(&name_bytes[..copy_len]);
            }
        }

        // Write metadata blob
        out.push((
            header.dir_hash_table_ofs,
            Arc::new(VectorVfsFile::new(metadata, String::new(), None)),
        ));

        // Sort output by offset
        out.sort_by_key(|pair| pair.0);

        out
    }
}
