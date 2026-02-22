// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! PFS0 (Partition File System) and HFS0 (Hash File System) parser.
//!
//! NSP files are PFS0 containers. XCI partitions use HFS0. Both share the
//! same basic structure: a header with magic, entry count, string table size,
//! followed by entry descriptors and a string table.

use crate::vfs::{OffsetFile, VfsDirectory, VfsFile};
use byteorder::{LittleEndian, ReadBytesExt};
use std::io::Cursor;
use std::sync::Arc;
use thiserror::Error;

/// PFS0 magic: 'P', 'F', 'S', '0'.
const PFS0_MAGIC: u32 = u32::from_le_bytes([b'P', b'F', b'S', b'0']);

/// HFS0 magic: 'H', 'F', 'S', '0'.
const HFS0_MAGIC: u32 = u32::from_le_bytes([b'H', b'F', b'S', b'0']);

/// PFS0 header size (magic + num_entries + strtab_size + padding).
const PFS0_HEADER_SIZE: usize = 0x10;

/// Size of a PFS0 entry descriptor.
const PFS0_ENTRY_SIZE: usize = 0x18;

/// Size of an HFS0 entry descriptor (includes hash_offset and hash fields).
const HFS0_ENTRY_SIZE: usize = 0x40;

/// Errors from PFS parsing.
#[derive(Debug, Error)]
pub enum PfsError {
    #[error("data too small for PFS header: need {expected}, got {actual}")]
    DataTooSmall { expected: usize, actual: usize },

    #[error("invalid PFS magic: expected PFS0 or HFS0, got 0x{0:08X}")]
    InvalidMagic(u32),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("string table entry out of bounds")]
    StringOutOfBounds,
}

/// Partition type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PfsType {
    Pfs0,
    Hfs0,
}

/// A single file entry within the partition.
#[derive(Debug, Clone)]
pub struct PfsEntry {
    /// File name from string table.
    pub name: String,
    /// Offset of the file data relative to the data region start.
    pub data_offset: u64,
    /// Size of the file data.
    pub data_size: u64,
}

/// Parsed PFS0/HFS0 partition.
#[derive(Debug, Clone)]
pub struct Pfs {
    /// Partition type (PFS0 or HFS0).
    pub pfs_type: PfsType,
    /// File entries.
    pub entries: Vec<PfsEntry>,
    /// Offset from the start of the source where file data begins.
    pub data_offset: u64,
}

impl Pfs {
    /// Parse a PFS0/HFS0 from VfsFile data at a given base offset.
    ///
    /// `file`: the source file to read from.
    /// `base_offset`: offset within the file where the PFS0/HFS0 header starts.
    pub fn parse(file: &dyn VfsFile, base_offset: u64) -> Result<Self, PfsError> {
        // Read header (16 bytes)
        let header_data = read_from(file, base_offset, PFS0_HEADER_SIZE)?;
        let mut cur = Cursor::new(&header_data[..]);

        let magic = cur.read_u32::<LittleEndian>()?;
        let pfs_type = match magic {
            PFS0_MAGIC => PfsType::Pfs0,
            HFS0_MAGIC => PfsType::Hfs0,
            _ => return Err(PfsError::InvalidMagic(magic)),
        };

        let num_entries = cur.read_u32::<LittleEndian>()? as usize;
        let strtab_size = cur.read_u32::<LittleEndian>()? as usize;
        let _reserved = cur.read_u32::<LittleEndian>()?;

        let entry_size = match pfs_type {
            PfsType::Pfs0 => PFS0_ENTRY_SIZE,
            PfsType::Hfs0 => HFS0_ENTRY_SIZE,
        };

        // Read entry table + string table
        let entries_size = num_entries * entry_size;
        let table_data = read_from(
            file,
            base_offset + PFS0_HEADER_SIZE as u64,
            entries_size + strtab_size,
        )?;

        let entry_table = &table_data[..entries_size];
        let strtab = &table_data[entries_size..];

        // Parse entries
        let mut entries = Vec::with_capacity(num_entries);
        for i in 0..num_entries {
            let e_offset = i * entry_size;
            let mut ecur = Cursor::new(&entry_table[e_offset..e_offset + entry_size]);

            let data_offset = ecur.read_u64::<LittleEndian>()?;
            let data_size = ecur.read_u64::<LittleEndian>()?;
            let strtab_offset = ecur.read_u32::<LittleEndian>()? as usize;
            // Skip remaining fields (reserved for PFS0, hash fields for HFS0)

            // Read name from string table (null-terminated)
            let name = read_strtab_entry(strtab, strtab_offset)?;

            entries.push(PfsEntry {
                name,
                data_offset,
                data_size,
            });
        }

        // Data region starts after header + entry table + string table
        let data_offset = base_offset + PFS0_HEADER_SIZE as u64 + entries_size as u64
            + strtab_size as u64;

        Ok(Pfs {
            pfs_type,
            entries,
            data_offset,
        })
    }

    /// Create VfsFile entries for all files in this partition.
    pub fn open_files(&self, source: Arc<dyn VfsFile>) -> Vec<Arc<dyn VfsFile>> {
        self.entries
            .iter()
            .map(|entry| {
                Arc::new(OffsetFile::new(
                    source.clone(),
                    entry.name.clone(),
                    self.data_offset + entry.data_offset,
                    entry.data_size,
                )) as Arc<dyn VfsFile>
            })
            .collect()
    }

    /// Get a specific file by name (case-insensitive).
    pub fn open_file(&self, source: Arc<dyn VfsFile>, name: &str) -> Option<Arc<dyn VfsFile>> {
        let name_lower = name.to_lowercase();
        self.entries.iter().find(|e| e.name.to_lowercase() == name_lower).map(|entry| {
            Arc::new(OffsetFile::new(
                source.clone(),
                entry.name.clone(),
                self.data_offset + entry.data_offset,
                entry.data_size,
            )) as Arc<dyn VfsFile>
        })
    }

    /// Find entries matching an extension (case-insensitive).
    pub fn files_with_extension(&self, ext: &str) -> Vec<&PfsEntry> {
        let ext_lower = ext.to_lowercase();
        let dot_ext = if ext_lower.starts_with('.') {
            ext_lower
        } else {
            format!(".{}", ext_lower)
        };
        self.entries
            .iter()
            .filter(|e| e.name.to_lowercase().ends_with(&dot_ext))
            .collect()
    }
}

/// PFS0/HFS0 as a VfsDirectory.
pub struct PfsDirectory {
    pfs: Pfs,
    source: Arc<dyn VfsFile>,
}

impl PfsDirectory {
    pub fn new(pfs: Pfs, source: Arc<dyn VfsFile>) -> Self {
        Self { pfs, source }
    }
}

impl VfsDirectory for PfsDirectory {
    fn files(&self) -> Vec<Arc<dyn VfsFile>> {
        self.pfs.open_files(self.source.clone())
    }

    fn subdirs(&self) -> Vec<Arc<dyn VfsDirectory>> {
        vec![] // PFS0 is flat (no subdirectories)
    }

    fn get_file(&self, name: &str) -> Option<Arc<dyn VfsFile>> {
        self.pfs.open_file(self.source.clone(), name)
    }

    fn get_subdir(&self, _name: &str) -> Option<Arc<dyn VfsDirectory>> {
        None
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Read a null-terminated string from the string table at the given offset.
fn read_strtab_entry(strtab: &[u8], offset: usize) -> Result<String, PfsError> {
    if offset >= strtab.len() {
        return Err(PfsError::StringOutOfBounds);
    }
    let end = strtab[offset..]
        .iter()
        .position(|&b| b == 0)
        .unwrap_or(strtab.len() - offset);
    Ok(String::from_utf8_lossy(&strtab[offset..offset + end]).into_owned())
}

/// Read `size` bytes from a VfsFile at the given offset.
fn read_from(file: &dyn VfsFile, offset: u64, size: usize) -> Result<Vec<u8>, PfsError> {
    if size == 0 {
        return Ok(vec![]);
    }
    let available = file.size().saturating_sub(offset) as usize;
    if available < size {
        return Err(PfsError::DataTooSmall {
            expected: size,
            actual: available,
        });
    }
    let mut buf = vec![0u8; size];
    file.read(offset, &mut buf)?;
    Ok(buf)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::VecFile;

    /// Build a minimal PFS0 binary with the given file entries.
    fn build_pfs0(files: &[(&str, &[u8])]) -> Vec<u8> {
        let num_entries = files.len();

        // Build string table
        let mut strtab = Vec::new();
        let mut strtab_offsets = Vec::new();
        for (name, _) in files {
            strtab_offsets.push(strtab.len());
            strtab.extend_from_slice(name.as_bytes());
            strtab.push(0); // null terminator
        }
        // Pad to 4-byte alignment
        while strtab.len() % 4 != 0 {
            strtab.push(0);
        }

        let strtab_size = strtab.len();
        let entries_size = num_entries * PFS0_ENTRY_SIZE;

        // Header
        let mut data = Vec::new();
        data.extend_from_slice(&PFS0_MAGIC.to_le_bytes());
        data.extend_from_slice(&(num_entries as u32).to_le_bytes());
        data.extend_from_slice(&(strtab_size as u32).to_le_bytes());
        data.extend_from_slice(&0u32.to_le_bytes()); // reserved

        // Entry table
        let mut running_offset: u64 = 0;
        for (i, (_, content)) in files.iter().enumerate() {
            data.extend_from_slice(&running_offset.to_le_bytes()); // data_offset
            data.extend_from_slice(&(content.len() as u64).to_le_bytes()); // data_size
            data.extend_from_slice(&(strtab_offsets[i] as u32).to_le_bytes()); // strtab_offset
            data.extend_from_slice(&0u32.to_le_bytes()); // reserved
            running_offset += content.len() as u64;
        }

        // String table
        data.extend_from_slice(&strtab);

        // File data
        for (_, content) in files {
            data.extend_from_slice(content);
        }

        data
    }

    #[test]
    fn test_parse_pfs0_single_file() {
        let pfs_data = build_pfs0(&[("test.txt", b"Hello, PFS0!")]);
        let file = Arc::new(VecFile::new("test.pfs0".to_string(), pfs_data));

        let pfs = Pfs::parse(file.as_ref(), 0).unwrap();
        assert_eq!(pfs.pfs_type, PfsType::Pfs0);
        assert_eq!(pfs.entries.len(), 1);
        assert_eq!(pfs.entries[0].name, "test.txt");
        assert_eq!(pfs.entries[0].data_size, 12);

        let vfs_files = pfs.open_files(file);
        assert_eq!(vfs_files.len(), 1);
        let content = vfs_files[0].read_all().unwrap();
        assert_eq!(&content, b"Hello, PFS0!");
    }

    #[test]
    fn test_parse_pfs0_multiple_files() {
        let pfs_data = build_pfs0(&[
            ("main.nca", &[0xAA; 32]),
            ("meta.nca", &[0xBB; 16]),
            ("ticket.tik", &[0xCC; 8]),
        ]);
        let file = Arc::new(VecFile::new("game.nsp".to_string(), pfs_data));

        let pfs = Pfs::parse(file.as_ref(), 0).unwrap();
        assert_eq!(pfs.entries.len(), 3);
        assert_eq!(pfs.entries[0].name, "main.nca");
        assert_eq!(pfs.entries[1].name, "meta.nca");
        assert_eq!(pfs.entries[2].name, "ticket.tik");

        let files = pfs.open_files(file.clone());
        assert_eq!(files[0].read_all().unwrap(), vec![0xAA; 32]);
        assert_eq!(files[1].read_all().unwrap(), vec![0xBB; 16]);
        assert_eq!(files[2].read_all().unwrap(), vec![0xCC; 8]);
    }

    #[test]
    fn test_pfs_open_file_by_name() {
        let pfs_data = build_pfs0(&[
            ("a.nca", &[0x11; 4]),
            ("b.tik", &[0x22; 4]),
        ]);
        let file = Arc::new(VecFile::new("test.nsp".to_string(), pfs_data));

        let pfs = Pfs::parse(file.as_ref(), 0).unwrap();
        let tik = pfs.open_file(file.clone(), "b.tik").unwrap();
        assert_eq!(tik.read_all().unwrap(), vec![0x22; 4]);

        assert!(pfs.open_file(file.clone(), "nonexistent").is_none());
    }

    #[test]
    fn test_files_with_extension() {
        let pfs_data = build_pfs0(&[
            ("a.nca", &[0; 1]),
            ("b.nca", &[0; 1]),
            ("c.tik", &[0; 1]),
        ]);
        let file = Arc::new(VecFile::new("test.nsp".to_string(), pfs_data));

        let pfs = Pfs::parse(file.as_ref(), 0).unwrap();
        let ncas = pfs.files_with_extension("nca");
        assert_eq!(ncas.len(), 2);
        let tiks = pfs.files_with_extension(".tik");
        assert_eq!(tiks.len(), 1);
    }

    #[test]
    fn test_pfs_invalid_magic() {
        let data = vec![0xFF; 32];
        let file = VecFile::new("bad.pfs0".to_string(), data);
        let result = Pfs::parse(&file, 0);
        assert!(matches!(result, Err(PfsError::InvalidMagic(_))));
    }

    #[test]
    fn test_pfs_data_too_small() {
        let data = vec![0; 4]; // Way too small for header
        let file = VecFile::new("tiny.pfs0".to_string(), data);
        let result = Pfs::parse(&file, 0);
        assert!(result.is_err());
    }

    #[test]
    fn test_pfs_directory() {
        let pfs_data = build_pfs0(&[("file.bin", b"data")]);
        let source = Arc::new(VecFile::new("test.pfs0".to_string(), pfs_data));

        let pfs = Pfs::parse(source.as_ref(), 0).unwrap();
        let dir = PfsDirectory::new(pfs, source);

        let files = dir.files();
        assert_eq!(files.len(), 1);
        assert_eq!(files[0].name(), "file.bin");

        let found = dir.get_file("file.bin").unwrap();
        assert_eq!(found.read_all().unwrap(), b"data");

        assert!(dir.get_file("nope").is_none());
        assert!(dir.subdirs().is_empty());
    }
}
