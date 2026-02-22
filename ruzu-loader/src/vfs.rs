// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Virtual Filesystem abstraction for reading game containers.
//!
//! Provides traits and implementations for file-like access that works with
//! both real filesystem files and sub-regions within container formats (PFS0,
//! HFS0, NCA sections).

use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// A readable file within the virtual filesystem.
pub trait VfsFile: Send + Sync {
    /// Name of the file (basename only).
    fn name(&self) -> &str;

    /// Total size of the file in bytes.
    fn size(&self) -> u64;

    /// Read `buf.len()` bytes starting at `offset`.
    /// Returns the number of bytes actually read.
    fn read(&self, offset: u64, buf: &mut [u8]) -> io::Result<usize>;

    /// Read the entire file into a new Vec.
    fn read_all(&self) -> io::Result<Vec<u8>> {
        let size = self.size() as usize;
        let mut buf = vec![0u8; size];
        let n = self.read(0, &mut buf)?;
        buf.truncate(n);
        Ok(buf)
    }
}

/// A directory within the virtual filesystem.
pub trait VfsDirectory: Send + Sync {
    /// List all files in this directory.
    fn files(&self) -> Vec<Arc<dyn VfsFile>>;

    /// List all subdirectories.
    fn subdirs(&self) -> Vec<Arc<dyn VfsDirectory>>;

    /// Get a file by name (case-insensitive).
    fn get_file(&self, name: &str) -> Option<Arc<dyn VfsFile>>;

    /// Get a subdirectory by name (case-insensitive).
    fn get_subdir(&self, name: &str) -> Option<Arc<dyn VfsDirectory>>;
}

// ---------------------------------------------------------------------------
// RealFile — filesystem-backed VfsFile
// ---------------------------------------------------------------------------

/// A [`VfsFile`] backed by an actual file on the host filesystem.
pub struct RealFile {
    path: PathBuf,
    name: String,
    size: u64,
}

impl RealFile {
    /// Open a real file at the given path.
    pub fn open(path: &Path) -> io::Result<Self> {
        let metadata = std::fs::metadata(path)?;
        let name = path
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .into_owned();
        Ok(Self {
            path: path.to_path_buf(),
            name,
            size: metadata.len(),
        })
    }
}

impl VfsFile for RealFile {
    fn name(&self) -> &str {
        &self.name
    }

    fn size(&self) -> u64 {
        self.size
    }

    fn read(&self, offset: u64, buf: &mut [u8]) -> io::Result<usize> {
        use std::io::{Read, Seek, SeekFrom};
        let mut file = std::fs::File::open(&self.path)?;
        file.seek(SeekFrom::Start(offset))?;
        file.read(buf)
    }
}

// ---------------------------------------------------------------------------
// OffsetFile — a slice within another VfsFile
// ---------------------------------------------------------------------------

/// A [`VfsFile`] that represents a sub-region of a parent file.
///
/// Used by container parsers (PFS0, NCA) to expose individual entries.
pub struct OffsetFile {
    parent: Arc<dyn VfsFile>,
    name: String,
    offset: u64,
    size: u64,
}

impl OffsetFile {
    /// Create a new OffsetFile referencing a region of the parent.
    pub fn new(parent: Arc<dyn VfsFile>, name: String, offset: u64, size: u64) -> Self {
        Self {
            parent,
            name,
            offset,
            size,
        }
    }
}

impl VfsFile for OffsetFile {
    fn name(&self) -> &str {
        &self.name
    }

    fn size(&self) -> u64 {
        self.size
    }

    fn read(&self, offset: u64, buf: &mut [u8]) -> io::Result<usize> {
        if offset >= self.size {
            return Ok(0);
        }
        let available = (self.size - offset) as usize;
        let to_read = buf.len().min(available);
        self.parent.read(self.offset + offset, &mut buf[..to_read])
    }
}

// ---------------------------------------------------------------------------
// VecFile — in-memory VfsFile (useful for decrypted data)
// ---------------------------------------------------------------------------

/// A [`VfsFile`] backed by an in-memory byte buffer.
pub struct VecFile {
    name: String,
    data: Vec<u8>,
}

impl VecFile {
    /// Create a new in-memory file.
    pub fn new(name: String, data: Vec<u8>) -> Self {
        Self { name, data }
    }
}

impl VfsFile for VecFile {
    fn name(&self) -> &str {
        &self.name
    }

    fn size(&self) -> u64 {
        self.data.len() as u64
    }

    fn read(&self, offset: u64, buf: &mut [u8]) -> io::Result<usize> {
        let offset = offset as usize;
        if offset >= self.data.len() {
            return Ok(0);
        }
        let available = self.data.len() - offset;
        let to_read = buf.len().min(available);
        buf[..to_read].copy_from_slice(&self.data[offset..offset + to_read]);
        Ok(to_read)
    }
}

// ---------------------------------------------------------------------------
// Helper: read exact bytes from VfsFile
// ---------------------------------------------------------------------------

/// Read exactly `size` bytes from a VfsFile at the given offset.
pub fn read_exact(file: &dyn VfsFile, offset: u64, size: usize) -> io::Result<Vec<u8>> {
    let mut buf = vec![0u8; size];
    let n = file.read(offset, &mut buf)?;
    if n < size {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            format!(
                "read_exact: expected {} bytes at offset 0x{:X}, got {}",
                size, offset, n
            ),
        ));
    }
    Ok(buf)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_file() {
        let data = vec![0x01, 0x02, 0x03, 0x04, 0x05];
        let file = VecFile::new("test.bin".to_string(), data.clone());

        assert_eq!(file.name(), "test.bin");
        assert_eq!(file.size(), 5);

        let all = file.read_all().unwrap();
        assert_eq!(all, data);

        let mut buf = [0u8; 2];
        let n = file.read(2, &mut buf).unwrap();
        assert_eq!(n, 2);
        assert_eq!(buf, [0x03, 0x04]);
    }

    #[test]
    fn test_offset_file() {
        let parent = Arc::new(VecFile::new(
            "parent.bin".to_string(),
            vec![0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF],
        ));

        let child = OffsetFile::new(parent, "child.bin".to_string(), 2, 3);
        assert_eq!(child.name(), "child.bin");
        assert_eq!(child.size(), 3);

        let all = child.read_all().unwrap();
        assert_eq!(all, vec![0xCC, 0xDD, 0xEE]);

        let mut buf = [0u8; 1];
        let n = child.read(1, &mut buf).unwrap();
        assert_eq!(n, 1);
        assert_eq!(buf[0], 0xDD);
    }

    #[test]
    fn test_offset_file_read_past_end() {
        let parent = Arc::new(VecFile::new(
            "p.bin".to_string(),
            vec![0x01, 0x02, 0x03],
        ));
        let child = OffsetFile::new(parent, "c.bin".to_string(), 1, 2);

        let mut buf = [0u8; 10];
        let n = child.read(0, &mut buf).unwrap();
        assert_eq!(n, 2);
        assert_eq!(&buf[..2], &[0x02, 0x03]);
    }

    #[test]
    fn test_read_exact() {
        let file = VecFile::new("test.bin".to_string(), vec![0x10, 0x20, 0x30, 0x40]);
        let result = read_exact(&file, 1, 2).unwrap();
        assert_eq!(result, vec![0x20, 0x30]);
    }

    #[test]
    fn test_read_exact_eof() {
        let file = VecFile::new("test.bin".to_string(), vec![0x10, 0x20]);
        let result = read_exact(&file, 0, 10);
        assert!(result.is_err());
    }
}
