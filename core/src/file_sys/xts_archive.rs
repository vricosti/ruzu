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
use super::vfs::vfs_offset::OffsetVfsFile;
use super::vfs::vfs_types::{VirtualDir, VirtualFile};
use crate::crypto::aes_util::{AesCipher, Mode, Op};
use crate::crypto::key_manager::{self, Key128, Key256, KeyManager};
use crate::crypto::xts_encryption_layer::XtsEncryptionLayer;
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
    fn parse(&mut self, path: &str) -> ResultStatus {
        // Read the header.
        let header_size = std::mem::size_of::<NaxHeader>();
        let mut header_bytes = vec![0u8; header_size];
        let read = self.file.read(&mut header_bytes, header_size, 0);
        if read != header_size {
            return ResultStatus::ErrorBadNAXHeader;
        }

        // Safety: NaxHeader is repr(C) with no padding requirements beyond what we ensure.
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

        // Derive SD keys via KeyManager, matching upstream.
        let keys_arc = KeyManager::instance();
        let mut keys = keys_arc.lock().unwrap();
        keys.derive_sd_seed_lazy();

        let mut sd_keys = [Key256::default(); 2];
        if let Err(_) = key_manager::derive_sd_keys(&mut sd_keys, &mut keys) {
            return ResultStatus::ErrorNAXKeyDerivationFailed;
        }

        // Save original encrypted key area for retry.
        let enc_keys = self.header.key_area;

        // Try both SD key indices (0 = Save, 1 = NCA).
        let mut found_index = None;
        for i in 0..2usize {
            // Derive NAX keys via HMAC-SHA256(sd_keys[i][..16], path).
            let nax_keys = match hmac_sha256_nax_keys(&sd_keys[i][..16], path.as_bytes()) {
                Some(k) => k,
                None => return ResultStatus::ErrorNAXKeyHMACFailed,
            };

            // Reset key_area to encrypted state before each attempt.
            self.header.key_area = enc_keys;

            // AES-ECB decrypt each key in the key area.
            for j in 0..2 {
                let mut cipher = AesCipher::new_128(nax_keys[j], Mode::ECB);
                let mut decrypted = [0u8; 16];
                cipher.transcode(&enc_keys[j], &mut decrypted, Op::Decrypt);
                self.header.key_area[j] = decrypted;
            }

            // HMAC-SHA256 validation: HMAC(sd_keys[i][16..32], header.magic..+0x60).
            // The upstream validates header bytes from &magic (offset 0x20) for 0x60 bytes.
            let header_validation_data = &header_bytes[0x20..0x80]; // magic through end = 0x60 bytes
            let validation = match hmac_sha256(&sd_keys[i][0x10..0x20], header_validation_data) {
                Some(v) => v,
                None => return ResultStatus::ErrorNAXValidationHMACFailed,
            };

            if self.header.hmac == validation {
                found_index = Some(i);
                break;
            }
        }

        let i = match found_index {
            Some(idx) => idx,
            None => return ResultStatus::ErrorNAXKeyDerivationFailed,
        };

        self.content_type = if i == 0 {
            NaxContentType::Save
        } else {
            NaxContentType::Nca
        };

        // Build the final XTS key from the decrypted key_area (two Key128 = one Key256).
        let mut final_key = Key256::default();
        final_key[..16].copy_from_slice(&self.header.key_area[0]);
        final_key[16..].copy_from_slice(&self.header.key_area[1]);

        // Create an offset view of the encrypted file (skip the NAX header).
        let enc_file: VirtualFile = Arc::new(OffsetVfsFile::new(
            self.file.clone(),
            self.header.file_size as usize,
            NAX_HEADER_PADDING_SIZE as usize,
            String::new(),
        ));

        // Wrap in XtsEncryptionLayer for transparent AES-XTS decryption.
        // Upstream: self.dec_file = make_shared<XTSEncryptionLayer>(enc_file, final_key);
        // Bridge the file_sys VirtualFile to crypto VirtualFile via FsVfsFileAdapter,
        // then wrap in XtsEncryptionLayer which implements file_sys::vfs::VfsFile.
        let crypto_file: crate::crypto::encryption_layer::VirtualFile = Arc::new(
            crate::crypto::encryption_layer::FsVfsFileAdapter::new(enc_file),
        );
        self.dec_file = Some(Arc::new(XtsEncryptionLayer::new(crypto_file, final_key))
            as std::sync::Arc<dyn crate::file_sys::vfs::vfs::VfsFile>);

        ResultStatus::Success
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

/// Compute HMAC-SHA256.
/// Implements RFC 2104 using SHA-256.
fn hmac_sha256(key: &[u8], data: &[u8]) -> Option<[u8; 32]> {
    use sha2::{Digest, Sha256};

    const BLOCK_SIZE: usize = 64;

    // If key is longer than block size, hash it first.
    let normalized_key = if key.len() > BLOCK_SIZE {
        let h = sha256_hash(key);
        h.to_vec()
    } else {
        key.to_vec()
    };

    // Pad key to block size.
    let mut k_ipad = [0x36u8; BLOCK_SIZE];
    let mut k_opad = [0x5cu8; BLOCK_SIZE];
    for (i, &b) in normalized_key.iter().enumerate() {
        k_ipad[i] ^= b;
        k_opad[i] ^= b;
    }

    // Inner hash: H(k_ipad || data)
    let mut inner = Sha256::new();
    inner.update(&k_ipad);
    inner.update(data);
    let inner_hash = inner.finalize();

    // Outer hash: H(k_opad || inner_hash)
    let mut outer = Sha256::new();
    outer.update(&k_opad);
    outer.update(&inner_hash);
    let result = outer.finalize();

    let mut out = [0u8; 32];
    out.copy_from_slice(&result);
    Some(out)
}

/// Compute HMAC-SHA256 and return the result as two Key128 NAX keys (32 bytes = 2 x 16).
/// Corresponds to upstream `CalculateHMAC256` used for NAX key derivation.
fn hmac_sha256_nax_keys(key: &[u8], data: &[u8]) -> Option<[Key128; 2]> {
    let hash = hmac_sha256(key, data)?;
    let mut keys = [[0u8; 16]; 2];
    keys[0].copy_from_slice(&hash[..16]);
    keys[1].copy_from_slice(&hash[16..]);
    Some(keys)
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
