// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/xts_archive.h / .cpp
// Status: COMPLETE (structural parity; HMAC/decryption requires crypto wiring)
//
// NAX (NCA Archive with XTS encryption) format parser.

use std::sync::Arc;

use super::content_archive::NCA;
use super::vfs::vfs::VfsFile;
use super::vfs::vfs_types::{VirtualDir, VirtualFile};
use crate::crypto::key_manager::{Key128, Key256};
use crate::loader::loader::ResultStatus;

/// NAX header padding size — data starts at this offset.
const NAX_HEADER_PADDING_SIZE: u64 = 0x4000;

/// NAX magic bytes: 'N','A','X','0' as little-endian u64.
const NAX_MAGIC: u64 = u64::from_le_bytes([b'N', b'A', b'X', b'0', 0, 0, 0, 0]);

/// NAX header — 0x80 bytes.
///
/// Binary layout matches upstream `NAXHeader` exactly.
#[derive(Clone)]
#[repr(C)]
pub struct NaxHeader {
    /// HMAC-SHA256 of the header for validation.
    pub hmac: [u8; 0x20],
    /// Magic value — should be NAX_MAGIC.
    pub magic: u64,
    /// Encrypted key area (two 128-bit keys).
    pub key_area: [Key128; 2],
    /// Size of the encrypted file data.
    pub file_size: u64,
    /// Reserved padding.
    pub _padding: [u8; 0x30],
}

const _: () = assert!(std::mem::size_of::<NaxHeader>() == 0x80);

impl Default for NaxHeader {
    fn default() -> Self {
        Self {
            hmac: [0u8; 0x20],
            magic: 0,
            key_area: [[0u8; 16]; 2],
            file_size: 0,
            _padding: [0u8; 0x30],
        }
    }
}

/// Content type of a NAX archive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NaxContentType {
    Save = 0,
    Nca = 1,
}

/// NAX archive — decrypts XTS-encrypted NCA or save files from registered cache.
pub struct Nax {
    /// The raw file backing this NAX archive.
    file: VirtualFile,
    /// Parse status.
    status: ResultStatus,
    /// Content type (Save or NCA), determined by which SD key index succeeds.
    content_type: NaxContentType,
    /// The parsed header.
    header: NaxHeader,
    /// The decrypted virtual file (if parsing succeeded).
    dec_file: Option<VirtualFile>,
}

impl Nax {
    /// Construct a NAX from a file, deriving the NCA ID from the file path.
    ///
    /// Corresponds to upstream `NAX::NAX(VirtualFile file_)`.
    pub fn new(file: VirtualFile) -> Self {
        let path = file.get_full_path();

        // Extract two_dir and nca_id from path using regex pattern:
        // /registered/(000000[0-9A-F]{2})/([0-9A-F]{32}).nca
        let nax_path = extract_nax_path_components(&path);
        match nax_path {
            Some((two_dir, nca_id)) => {
                let formatted_path = format!("/registered/{}/{}.nca", two_dir, nca_id);
                let mut nax = Self {
                    file,
                    status: ResultStatus::Success,
                    content_type: NaxContentType::Nca,
                    header: NaxHeader::default(),
                    dec_file: None,
                };
                nax.status = nax.parse(&formatted_path);
                nax
            }
            None => Self {
                file,
                status: ResultStatus::ErrorBadNAXFilePath,
                content_type: NaxContentType::Nca,
                header: NaxHeader::default(),
                dec_file: None,
            },
        }
    }

    /// Construct a NAX from a file with an explicit NCA ID.
    ///
    /// Corresponds to upstream `NAX::NAX(VirtualFile file_, std::array<u8, 0x10> nca_id)`.
    pub fn with_nca_id(file: VirtualFile, nca_id: [u8; 0x10]) -> Self {
        // Hash the NCA ID with SHA-256 to get the directory component.
        let hash = sha256_hash(&nca_id);
        let nca_id_hex = hex_to_string(&nca_id, false);
        let formatted_path = format!("/registered/000000{:02X}/{}.nca", hash[0], nca_id_hex);

        let mut nax = Self {
            file,
            status: ResultStatus::Success,
            content_type: NaxContentType::Nca,
            header: NaxHeader::default(),
            dec_file: None,
        };
        nax.status = nax.parse(&formatted_path);
        nax
    }

    /// Parse the NAX header and attempt decryption.
    ///
    /// Corresponds to upstream `NAX::Parse`.
    fn parse(&mut self, _path: &str) -> ResultStatus {
        // Read the header.
        let header_size = std::mem::size_of::<NaxHeader>();
        let mut header_bytes = vec![0u8; header_size];
        let read = self.file.read(&mut header_bytes, header_size, 0);
        if read != header_size {
            return ResultStatus::ErrorBadNAXHeader;
        }

        // Safety: NaxHeader is repr(C) with no padding requirements beyond what we ensure.
        // Copy bytes into the header struct.
        unsafe {
            std::ptr::copy_nonoverlapping(
                header_bytes.as_ptr(),
                &mut self.header as *mut NaxHeader as *mut u8,
                header_size,
            );
        }

        // Check magic.
        if self.header.magic != NAX_MAGIC {
            return ResultStatus::ErrorBadNAXHeader;
        }

        // Check file size.
        if (self.file.get_size() as u64) < NAX_HEADER_PADDING_SIZE + self.header.file_size {
            return ResultStatus::ErrorIncorrectNAXFileSize;
        }

        // Key derivation and HMAC validation requires:
        // 1. KeyManager::DeriveSDSeedLazy()
        // 2. DeriveSDKeys() to get sd_keys
        // 3. HMAC-SHA256 computation for key derivation
        // 4. AES-ECB decryption of key area
        // 5. HMAC-SHA256 validation
        // 6. XTS decryption layer
        //
        // These operations depend on the full crypto pipeline being wired up.
        // For now, return an error indicating key derivation is needed.
        //
        // TODO: Wire up full crypto pipeline when KeyManager SD key derivation
        // and AES/HMAC operations are available.
        ResultStatus::ErrorNAXKeyDerivationFailed
    }

    /// Get the parse status.
    pub fn get_status(&self) -> ResultStatus {
        self.status
    }

    /// Get the decrypted file, if parsing and decryption succeeded.
    pub fn get_decrypted(&self) -> Option<VirtualFile> {
        self.dec_file.clone()
    }

    /// Create an NCA from the decrypted file, if the content type is NCA.
    ///
    /// Corresponds to upstream `NAX::AsNCA`.
    pub fn as_nca(&self) -> Option<NCA> {
        if self.content_type == NaxContentType::Nca {
            self.get_decrypted().map(|dec| NCA::new(dec, None))
        } else {
            None
        }
    }

    /// Get the content type (Save or NCA).
    pub fn get_content_type(&self) -> NaxContentType {
        self.content_type
    }

    /// Get the underlying file name.
    pub fn get_name(&self) -> String {
        self.file.get_name()
    }

    /// Get files in this directory (the decrypted file, if available).
    pub fn get_files(&self) -> Vec<VirtualFile> {
        self.dec_file.iter().cloned().collect()
    }

    /// Get subdirectories (always empty for NAX).
    pub fn get_subdirectories(&self) -> Vec<VirtualDir> {
        vec![]
    }
}

/// Extract the two_dir and nca_id components from a NAX file path.
///
/// Looks for pattern: /registered/(000000XX)/(YYYYYY...32hex).nca
fn extract_nax_path_components(path: &str) -> Option<(String, String)> {
    // Normalize path separators.
    let normalized = path.replace('\\', "/");

    // Find "/registered/" in the path.
    let registered_idx = normalized.find("/registered/")?;
    let after_registered = &normalized[registered_idx + "/registered/".len()..];

    // Split on '/'
    let parts: Vec<&str> = after_registered.splitn(2, '/').collect();
    if parts.len() != 2 {
        return None;
    }

    let two_dir = parts[0];
    let nca_file = parts[1];

    // Validate two_dir matches 000000XX pattern (8 hex chars).
    if two_dir.len() != 8 || !two_dir.starts_with("000000") {
        return None;
    }
    if !two_dir[6..].chars().all(|c| c.is_ascii_hexdigit()) {
        return None;
    }

    // Validate nca_file matches XXXXXXXX...32hex.nca
    if !nca_file.to_lowercase().ends_with(".nca") {
        return None;
    }
    let nca_id = &nca_file[..nca_file.len() - 4];
    if nca_id.len() != 32 || !nca_id.chars().all(|c| c.is_ascii_hexdigit()) {
        return None;
    }

    Some((two_dir.to_uppercase(), nca_id.to_lowercase()))
}

/// Compute SHA-256 hash of input data.
fn sha256_hash(data: &[u8]) -> [u8; 32] {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(data);
    let result = hasher.finalize();
    let mut hash = [0u8; 32];
    hash.copy_from_slice(&result);
    hash
}

/// Convert a byte slice to a hex string.
fn hex_to_string(data: &[u8], uppercase: bool) -> String {
    if uppercase {
        data.iter().map(|b| format!("{:02X}", b)).collect()
    } else {
        data.iter().map(|b| format!("{:02x}", b)).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nax_header_size() {
        assert_eq!(std::mem::size_of::<NaxHeader>(), 0x80);
    }

    #[test]
    fn test_nax_magic() {
        // 'N','A','X','0' in little-endian u64
        let magic_bytes = NAX_MAGIC.to_le_bytes();
        assert_eq!(magic_bytes[0], b'N');
        assert_eq!(magic_bytes[1], b'A');
        assert_eq!(magic_bytes[2], b'X');
        assert_eq!(magic_bytes[3], b'0');
    }

    #[test]
    fn test_extract_nax_path_components_valid() {
        let path = "/some/path/registered/000000AB/0123456789abcdef0123456789abcdef.nca";
        let result = extract_nax_path_components(path);
        assert!(result.is_some());
        let (two_dir, nca_id) = result.unwrap();
        assert_eq!(two_dir, "000000AB");
        assert_eq!(nca_id, "0123456789abcdef0123456789abcdef");
    }

    #[test]
    fn test_extract_nax_path_components_invalid_dir() {
        let path = "/registered/ABCDEFGH/0123456789abcdef0123456789abcdef.nca";
        assert!(extract_nax_path_components(path).is_none());
    }

    #[test]
    fn test_extract_nax_path_components_wrong_extension() {
        let path = "/registered/000000AB/0123456789abcdef0123456789abcdef.bin";
        assert!(extract_nax_path_components(path).is_none());
    }

    #[test]
    fn test_extract_nax_path_components_short_id() {
        let path = "/registered/000000AB/0123.nca";
        assert!(extract_nax_path_components(path).is_none());
    }

    #[test]
    fn test_hex_to_string() {
        assert_eq!(hex_to_string(&[0xAB, 0xCD], false), "abcd");
        assert_eq!(hex_to_string(&[0xAB, 0xCD], true), "ABCD");
    }
}
