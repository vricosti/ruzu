// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! NCA (Nintendo Content Archive) parser.
//!
//! NCA files contain the actual game data (code, assets, control info).
//! The first 0xC00 bytes are an AES-XTS encrypted header containing content
//! type, key area, and section info. Body sections use AES-CTR.

use crate::pfs::{Pfs, PfsError};
use crate::vfs::{VecFile, VfsFile};
use byteorder::{LittleEndian, ReadBytesExt};
use ruzu_crypto::aes_ctr;
use ruzu_crypto::aes_xts::{self, NCA_SECTOR_SIZE};
use ruzu_crypto::key_manager::{Key128, KeyManager};
use std::io::Cursor;
use std::sync::Arc;
use thiserror::Error;

/// NCA3 magic.
const NCA3_MAGIC: u32 = u32::from_le_bytes([b'N', b'C', b'A', b'3']);

/// NCA2 magic (older format, treated similarly).
const NCA2_MAGIC: u32 = u32::from_le_bytes([b'N', b'C', b'A', b'2']);

/// NCA header size (encrypted region).
pub const NCA_HEADER_SIZE: usize = 0xC00;

/// Size of the fixed header portion.
const NCA_FIXED_HEADER_SIZE: usize = 0x400;

/// Number of section entries in the NCA header.
const NCA_SECTION_COUNT: usize = 4;

/// Size of each section table entry.
const NCA_SECTION_ENTRY_SIZE: usize = 0x10;

/// Size of the key area.
const KEY_AREA_SIZE: usize = 0x40;

/// Key area entry size.
const KEY_AREA_ENTRY_SIZE: usize = 0x10;

/// Errors from NCA parsing.
#[derive(Debug, Error)]
pub enum NcaError {
    #[error("NCA data too small: need at least {NCA_HEADER_SIZE} bytes, got {0}")]
    DataTooSmall(usize),

    #[error("missing header key for NCA decryption")]
    MissingHeaderKey,

    #[error("invalid NCA magic after decryption: got 0x{0:08X}")]
    InvalidMagic(u32),

    #[error("missing key for NCA body decryption: {0}")]
    MissingBodyKey(String),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("PFS error in NCA section: {0}")]
    Pfs(#[from] PfsError),
}

/// NCA content type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NcaContentType {
    Program,
    Meta,
    Control,
    Manual,
    Data,
    PublicData,
    Unknown(u8),
}

impl From<u8> for NcaContentType {
    fn from(v: u8) -> Self {
        match v {
            0 => Self::Program,
            1 => Self::Meta,
            2 => Self::Control,
            3 => Self::Manual,
            4 => Self::Data,
            5 => Self::PublicData,
            x => Self::Unknown(x),
        }
    }
}

/// NCA section filesystem type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NcaSectionFsType {
    RomFs = 0,
    Pfs0 = 1,
}

/// NCA section encryption type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NcaCryptoType {
    None = 1,
    Xts = 2,
    Ctr = 3,
    BktrCtr = 4,
}

/// A section within the NCA.
#[derive(Debug, Clone)]
pub struct NcaSection {
    /// Start offset of this section within the NCA file.
    pub start_offset: u64,
    /// End offset of this section within the NCA file.
    pub end_offset: u64,
    /// Filesystem type (PFS0 or RomFS).
    pub fs_type: NcaSectionFsType,
    /// Encryption type.
    pub crypto_type: NcaCryptoType,
    /// Section CTR counter (upper 64 bits of the IV).
    pub ctr: u64,
}

/// Parsed NCA header.
#[derive(Debug, Clone)]
pub struct NcaHeader {
    /// NCA format version (3 or 2).
    pub version: u8,
    /// Content type.
    pub content_type: NcaContentType,
    /// Crypto type (key generation).
    pub crypto_type: u8,
    /// Key area encryption key index (0=Application, 1=Ocean, 2=System).
    pub key_area_type: u8,
    /// Title ID.
    pub title_id: u64,
    /// Rights ID (for titlekey encryption).
    pub rights_id: [u8; 16],
    /// Sections defined in this NCA.
    pub sections: Vec<NcaSection>,
    /// Raw key area (4 x 16-byte encrypted keys).
    pub key_area: [Key128; 4],
    /// Decrypted header bytes (for further parsing).
    pub decrypted_header: Vec<u8>,
}

impl NcaHeader {
    /// Check if this NCA uses rights-ID based encryption (title key).
    pub fn has_rights_id(&self) -> bool {
        self.rights_id.iter().any(|&b| b != 0)
    }

    /// Get the rights ID as a hex string.
    pub fn rights_id_hex(&self) -> String {
        hex::encode(self.rights_id)
    }
}

/// Parse an NCA header from the encrypted raw data.
///
/// The first 0xC00 bytes are decrypted with AES-128-XTS using the `header_key`.
pub fn parse_nca_header(data: &[u8], keys: &KeyManager) -> Result<NcaHeader, NcaError> {
    if data.len() < NCA_HEADER_SIZE {
        return Err(NcaError::DataTooSmall(data.len()));
    }

    let header_key = keys.header_key().ok_or(NcaError::MissingHeaderKey)?;

    // Decrypt the header with AES-XTS
    let mut header = data[..NCA_HEADER_SIZE].to_vec();
    aes_xts::decrypt_aes_xts(&header_key, 0, NCA_SECTOR_SIZE, &mut header);

    // Parse fixed header at offset 0x200 (first 0x200 is the signature)
    let hdr = &header[0x200..];
    let mut cur = Cursor::new(hdr);

    let magic = cur.read_u32::<LittleEndian>()?;
    if magic != NCA3_MAGIC && magic != NCA2_MAGIC {
        return Err(NcaError::InvalidMagic(magic));
    }

    let version = if magic == NCA3_MAGIC { 3 } else { 2 };

    // [0x04] distribution type (skip)
    let _dist_type = cur.read_u8()?;

    // [0x05] content type
    let content_type = NcaContentType::from(cur.read_u8()?);

    // [0x06] crypto type (key generation)
    let crypto_type = cur.read_u8()?;

    // [0x07] key area encryption key index
    let key_area_type = cur.read_u8()?;

    // [0x08] NCA size (8 bytes)
    let _nca_size = cur.read_u64::<LittleEndian>()?;

    // [0x10] title ID
    let title_id = cur.read_u64::<LittleEndian>()?;

    // [0x18] padding (4 bytes)
    let _padding = cur.read_u32::<LittleEndian>()?;

    // [0x1C] sdk addon version (4 bytes)
    let _sdk_version = cur.read_u32::<LittleEndian>()?;

    // [0x20] crypto type 2
    let _crypto_type2 = cur.read_u8()?;

    // Skip to rights ID at offset 0x30 in the fixed header (0x230 absolute)
    // Current position is 0x21, need to skip to 0x30
    let cur_pos = cur.position() as usize;
    let rights_id_offset = 0x30;
    if cur_pos < rights_id_offset {
        for _ in cur_pos..rights_id_offset {
            cur.read_u8()?;
        }
    }

    let mut rights_id = [0u8; 16];
    for b in &mut rights_id {
        *b = cur.read_u8()?;
    }

    // Parse section table at offset 0x40 in the fixed header
    let section_table_base = 0x40;
    let mut sections = Vec::new();

    for i in 0..NCA_SECTION_COUNT {
        let entry_offset = section_table_base + i * NCA_SECTION_ENTRY_SIZE;
        let entry = &hdr[entry_offset..entry_offset + NCA_SECTION_ENTRY_SIZE];
        let mut ecur = Cursor::new(entry);

        let start_offset = ecur.read_u32::<LittleEndian>()? as u64 * 0x200;
        let end_offset = ecur.read_u32::<LittleEndian>()? as u64 * 0x200;
        let _enabled = ecur.read_u32::<LittleEndian>()?;
        let _padding = ecur.read_u32::<LittleEndian>()?;

        if start_offset == 0 && end_offset == 0 {
            continue; // Empty section
        }

        // Parse section's FS header from the header data
        // FS headers are at offset 0x400 + section_index * 0x200 in the
        // decrypted header
        let _fs_header_offset = 0x200 + i * 0x200; // relative to header[0x200..]
        let fs_hdr_abs = 0x400 + i * 0x200; // absolute in decrypted header

        if fs_hdr_abs + 0x200 <= header.len() {
            let fs_hdr = &header[fs_hdr_abs..fs_hdr_abs + 0x200];
            let mut fcur = Cursor::new(fs_hdr);

            let _fs_version = fcur.read_u16::<LittleEndian>()?;
            let fs_type = fcur.read_u8()?;
            let crypto = fcur.read_u8()?;

            // Skip to CTR at offset 0x140 in FS header
            let ctr = if fs_hdr.len() >= 0x148 {
                let mut ccur = Cursor::new(&fs_hdr[0x140..0x148]);
                ccur.read_u64::<LittleEndian>().unwrap_or(0)
            } else {
                0
            };

            sections.push(NcaSection {
                start_offset,
                end_offset,
                fs_type: if fs_type == 1 {
                    NcaSectionFsType::Pfs0
                } else {
                    NcaSectionFsType::RomFs
                },
                crypto_type: match crypto {
                    1 => NcaCryptoType::None,
                    2 => NcaCryptoType::Xts,
                    3 => NcaCryptoType::Ctr,
                    4 => NcaCryptoType::BktrCtr,
                    _ => NcaCryptoType::None,
                },
                ctr,
            });
        }
    }

    // Parse key area at offset 0x100 in the fixed header
    let key_area_offset = 0x100;
    let mut key_area = [[0u8; 16]; 4];
    for (i, key) in key_area.iter_mut().enumerate() {
        let start = key_area_offset + i * KEY_AREA_ENTRY_SIZE;
        key.copy_from_slice(&hdr[start..start + KEY_AREA_ENTRY_SIZE]);
    }

    Ok(NcaHeader {
        version,
        content_type,
        crypto_type,
        key_area_type,
        title_id,
        rights_id,
        sections,
        key_area,
        decrypted_header: header,
    })
}

/// Decrypt an NCA section's data using the appropriate key.
///
/// Returns decrypted data for the section.
pub fn decrypt_nca_section(
    file: &dyn VfsFile,
    section: &NcaSection,
    header: &NcaHeader,
    keys: &KeyManager,
) -> Result<Vec<u8>, NcaError> {
    let section_size = (section.end_offset - section.start_offset) as usize;
    let mut data = vec![0u8; section_size];
    file.read(section.start_offset, &mut data)
        .map_err(NcaError::Io)?;

    match section.crypto_type {
        NcaCryptoType::None => {
            // No decryption needed
        }
        NcaCryptoType::Ctr | NcaCryptoType::BktrCtr => {
            let body_key = get_body_key(header, keys)?;
            let iv = aes_ctr::make_nca_ctr(section.ctr, section.start_offset);
            aes_ctr::decrypt_aes_ctr(&body_key, &iv, &mut data);
        }
        NcaCryptoType::Xts => {
            // XTS for body sections is rare but supported
            log::warn!("XTS body encryption not commonly used, section may not decrypt correctly");
        }
    }

    Ok(data)
}

/// Get the body decryption key for an NCA.
fn get_body_key(header: &NcaHeader, keys: &KeyManager) -> Result<Key128, NcaError> {
    if header.has_rights_id() {
        // Use title key from the key manager
        keys.title_key(&header.rights_id_hex())
            .ok_or_else(|| {
                NcaError::MissingBodyKey(format!(
                    "title key for rights_id={}",
                    header.rights_id_hex()
                ))
            })
    } else {
        // Decrypt key area entry with the key-area-key
        let kak = keys
            .key_area_key(header.key_area_type, header.crypto_type)
            .ok_or_else(|| {
                NcaError::MissingBodyKey(format!(
                    "key_area_key type={} revision={}",
                    header.key_area_type, header.crypto_type
                ))
            })?;

        // Decrypt the first key area entry with AES-128-ECB (= CTR with zero IV)
        let mut decrypted_key = header.key_area[0];
        let iv = [0u8; 16];
        aes_ctr::decrypt_aes_ctr(&kak, &iv, &mut decrypted_key);

        Ok(decrypted_key)
    }
}

/// Open the ExeFS (PFS0) from a Program NCA's section 0.
///
/// Returns the parsed PFS0 containing `main` (the NSO executable).
pub fn open_exefs(
    file: &dyn VfsFile,
    header: &NcaHeader,
    keys: &KeyManager,
) -> Result<Option<(Pfs, Vec<u8>)>, NcaError> {
    // ExeFS is typically in section 0 of a Program NCA
    let section = match header.sections.first() {
        Some(s) if s.fs_type == NcaSectionFsType::Pfs0 => s,
        _ => return Ok(None),
    };

    let decrypted = decrypt_nca_section(file, section, header, keys)?;
    let vec_file = Arc::new(VecFile::new("exefs".to_string(), decrypted.clone()));
    let pfs = Pfs::parse(vec_file.as_ref(), 0)?;

    Ok(Some((pfs, decrypted)))
}

/// Open the Control data (NACP + icon) from a Control NCA.
///
/// Returns raw decrypted section data (the RomFS or PFS0 containing control.nacp).
pub fn open_control(
    file: &dyn VfsFile,
    header: &NcaHeader,
    keys: &KeyManager,
) -> Result<Option<Vec<u8>>, NcaError> {
    if header.content_type != NcaContentType::Control {
        return Ok(None);
    }

    // Control NCA section 0 contains the control data (often as RomFS)
    let section = match header.sections.first() {
        Some(s) => s,
        None => return Ok(None),
    };

    let decrypted = decrypt_nca_section(file, section, header, keys)?;
    Ok(Some(decrypted))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nca_content_type_from() {
        assert_eq!(NcaContentType::from(0), NcaContentType::Program);
        assert_eq!(NcaContentType::from(1), NcaContentType::Meta);
        assert_eq!(NcaContentType::from(2), NcaContentType::Control);
        assert_eq!(NcaContentType::from(3), NcaContentType::Manual);
        assert_eq!(NcaContentType::from(4), NcaContentType::Data);
        assert_eq!(NcaContentType::from(99), NcaContentType::Unknown(99));
    }

    #[test]
    fn test_nca_header_rights_id() {
        let header = NcaHeader {
            version: 3,
            content_type: NcaContentType::Program,
            crypto_type: 0,
            key_area_type: 0,
            title_id: 0,
            rights_id: [0; 16],
            sections: vec![],
            key_area: [[0; 16]; 4],
            decrypted_header: vec![],
        };
        assert!(!header.has_rights_id());

        let header2 = NcaHeader {
            rights_id: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
            ..header
        };
        assert!(header2.has_rights_id());
    }

    #[test]
    fn test_nca_too_small() {
        let data = vec![0u8; 100];
        let keys = KeyManager::new();
        assert!(matches!(
            parse_nca_header(&data, &keys),
            Err(NcaError::DataTooSmall(_))
        ));
    }

    #[test]
    fn test_nca_missing_header_key() {
        let data = vec![0u8; NCA_HEADER_SIZE];
        let keys = KeyManager::new(); // Empty, no header_key
        assert!(matches!(
            parse_nca_header(&data, &keys),
            Err(NcaError::MissingHeaderKey)
        ));
    }
}
