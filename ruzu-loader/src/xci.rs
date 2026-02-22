// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! XCI (Gamecard Image) loader.
//!
//! XCI is the gamecard dump format. Structure:
//! - 0x000..0x200: Gamecard header (contains HFS0 root partition offset)
//! - Root HFS0 partition contains sub-partitions: "update", "normal", "secure"
//! - The "secure" partition contains the actual NCAs, treated like an NSP

use crate::nsp::{self, NspError, NspLoadResult};
use crate::pfs::{Pfs, PfsError};
use crate::vfs::{OffsetFile, VfsFile};
use byteorder::{LittleEndian, ReadBytesExt};
use ruzu_crypto::key_manager::KeyManager;
use std::io::Cursor;
use std::sync::Arc;
use thiserror::Error;

/// XCI header magic: "HEAD".
const XCI_MAGIC: u32 = u32::from_le_bytes([b'H', b'E', b'A', b'D']);

/// Gamecard header size.
const XCI_HEADER_SIZE: usize = 0x200;

/// Offset of the magic in the header.
const MAGIC_OFFSET: usize = 0x100;

/// Offset of the root HFS0 partition offset (from beginning of XCI).
const ROOT_PARTITION_OFFSET: usize = 0x130;

/// Offset of the root HFS0 partition size.
const ROOT_PARTITION_SIZE_OFFSET: usize = 0x138;

/// Errors from XCI loading.
#[derive(Debug, Error)]
pub enum XciError {
    #[error("XCI too small: need at least {XCI_HEADER_SIZE} bytes, got {0}")]
    DataTooSmall(usize),

    #[error("invalid XCI magic: expected 0x{:08X}, got 0x{actual:08X}", XCI_MAGIC)]
    InvalidMagic { actual: u32 },

    #[error("secure partition not found in XCI")]
    NoSecurePartition,

    #[error("PFS error: {0}")]
    Pfs(#[from] PfsError),

    #[error("NSP error: {0}")]
    Nsp(#[from] NspError),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
}

/// Parsed XCI gamecard header.
#[derive(Debug)]
pub struct XciHeader {
    /// Offset of the root HFS0 partition.
    pub root_partition_offset: u64,
    /// Size of the root HFS0 header.
    pub root_partition_header_size: u64,
}

/// Parse the XCI header.
pub fn parse_xci_header(file: &dyn VfsFile) -> Result<XciHeader, XciError> {
    if file.size() < XCI_HEADER_SIZE as u64 {
        return Err(XciError::DataTooSmall(file.size() as usize));
    }

    let mut header_data = vec![0u8; XCI_HEADER_SIZE];
    file.read(0, &mut header_data).map_err(XciError::Io)?;

    // Check magic at offset 0x100
    let mut cur = Cursor::new(&header_data[MAGIC_OFFSET..MAGIC_OFFSET + 4]);
    let magic = cur.read_u32::<LittleEndian>()?;
    if magic != XCI_MAGIC {
        return Err(XciError::InvalidMagic { actual: magic });
    }

    // Root partition offset at 0x130
    let mut cur = Cursor::new(&header_data[ROOT_PARTITION_OFFSET..ROOT_PARTITION_OFFSET + 8]);
    let root_partition_offset = cur.read_u64::<LittleEndian>()?;

    // Root partition header size at 0x138
    let mut cur =
        Cursor::new(&header_data[ROOT_PARTITION_SIZE_OFFSET..ROOT_PARTITION_SIZE_OFFSET + 8]);
    let root_partition_header_size = cur.read_u64::<LittleEndian>()?;

    Ok(XciHeader {
        root_partition_offset,
        root_partition_header_size,
    })
}

/// Load an XCI: parse gamecard header → root HFS0 → secure partition → NCAs.
///
/// The secure partition is treated like an NSP (PFS0 containing NCAs).
pub fn load_xci(
    file: Arc<dyn VfsFile>,
    keys: &mut KeyManager,
) -> Result<NspLoadResult, XciError> {
    let header = parse_xci_header(file.as_ref())?;
    log::info!(
        "XCI root partition at offset 0x{:X}, header size 0x{:X}",
        header.root_partition_offset,
        header.root_partition_header_size
    );

    // Parse root HFS0
    let root_pfs = Pfs::parse(file.as_ref(), header.root_partition_offset)?;
    log::info!("XCI root HFS0 has {} partitions", root_pfs.entries.len());

    for entry in &root_pfs.entries {
        log::info!("  Partition: {} (size: {} bytes)", entry.name, entry.data_size);
    }

    // Find the "secure" partition
    let secure_entry = root_pfs
        .entries
        .iter()
        .find(|e| e.name.to_lowercase() == "secure")
        .ok_or(XciError::NoSecurePartition)?;

    // Create an OffsetFile for the secure partition
    let secure_offset = root_pfs.data_offset + secure_entry.data_offset;
    let secure_file = Arc::new(OffsetFile::new(
        file.clone(),
        "secure".to_string(),
        secure_offset,
        secure_entry.data_size,
    ));

    // Treat the secure partition as an NSP
    let result = nsp::load_nsp(secure_file, keys)?;

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::VecFile;

    #[test]
    fn test_xci_too_small() {
        let file = VecFile::new("test.xci".to_string(), vec![0; 16]);
        assert!(matches!(
            parse_xci_header(&file),
            Err(XciError::DataTooSmall(_))
        ));
    }

    #[test]
    fn test_xci_invalid_magic() {
        let mut data = vec![0u8; XCI_HEADER_SIZE];
        // Write wrong magic at offset 0x100
        data[0x100..0x104].copy_from_slice(&[0xFF, 0xFF, 0xFF, 0xFF]);
        let file = VecFile::new("test.xci".to_string(), data);
        assert!(matches!(
            parse_xci_header(&file),
            Err(XciError::InvalidMagic { .. })
        ));
    }

    #[test]
    fn test_xci_header_parse() {
        let mut data = vec![0u8; XCI_HEADER_SIZE];
        // Write HEAD magic
        data[0x100..0x104].copy_from_slice(&XCI_MAGIC.to_le_bytes());
        // Root partition offset = 0x1000
        data[0x130..0x138].copy_from_slice(&0x1000u64.to_le_bytes());
        // Root partition header size = 0x200
        data[0x138..0x140].copy_from_slice(&0x200u64.to_le_bytes());

        let file = VecFile::new("test.xci".to_string(), data);
        let header = parse_xci_header(&file).unwrap();
        assert_eq!(header.root_partition_offset, 0x1000);
        assert_eq!(header.root_partition_header_size, 0x200);
    }
}
