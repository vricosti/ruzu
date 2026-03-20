// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/crypto/key_manager.h and key_manager.cpp
//! Manages cryptographic keys for Switch content decryption.
//!
//! This module preserves the upstream KeyIndex/S128KeyType/S256KeyType key classification
//! system and the full key derivation pipeline, matching the C++ file structure.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};

use common::fs::path_util::{get_ruzu_path, RuzuPath};
use common::hex_util::hex_string_to_array;

// sha_util::Sha256Hash type used indirectly via sha256() calls

/// A 128-bit (16 byte) key, used for AES-128.
pub type Key128 = [u8; 16];

/// A 256-bit (32 byte) key, used for AES-XTS header decryption.
pub type Key256 = [u8; 32];

/// Signature type for tickets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SignatureType {
    RSA4096SHA1 = 0x10000,
    RSA2048SHA1 = 0x10001,
    ECDSASHA1 = 0x10002,
    RSA4096SHA256 = 0x10003,
    RSA2048SHA256 = 0x10004,
    ECDSASHA256 = 0x10005,
}

pub fn get_signature_type_data_size(sig_type: SignatureType) -> u64 {
    match sig_type {
        SignatureType::RSA4096SHA1 | SignatureType::RSA4096SHA256 => 0x200,
        SignatureType::RSA2048SHA1 | SignatureType::RSA2048SHA256 => 0x100,
        SignatureType::ECDSASHA1 | SignatureType::ECDSASHA256 => 0x3C,
    }
}

pub fn get_signature_type_padding_size(sig_type: SignatureType) -> u64 {
    match sig_type {
        SignatureType::RSA4096SHA1
        | SignatureType::RSA4096SHA256
        | SignatureType::RSA2048SHA1
        | SignatureType::RSA2048SHA256 => 0x3C,
        SignatureType::ECDSASHA1 | SignatureType::ECDSASHA256 => 0x40,
    }
}

/// Title key type in ticket.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum TitleKeyType {
    Common = 0,
    Personalized = 1,
}

/// Ticket data structure (0x2C0 bytes in upstream).
#[derive(Debug, Clone)]
#[repr(C)]
pub struct TicketData {
    pub issuer: [u8; 0x40],
    pub title_key_block: [u8; 0x100],
    pub _padding1: [u8; 1],
    pub key_type: TitleKeyType,
    pub _padding2: [u8; 3],
    pub revision: u8,
    pub _padding3: [u8; 0xA],
    pub ticket_id: u64,
    pub device_id: u64,
    pub rights_id: [u8; 0x10],
    pub account_id: u32,
    pub _padding4: [u8; 0x14C],
}

/// Extract the common title key from the title_key_block (first 16 bytes).
impl TicketData {
    pub fn title_key_common(&self) -> Key128 {
        let mut key = [0u8; 16];
        key.copy_from_slice(&self.title_key_block[..16]);
        key
    }
}

impl Default for TicketData {
    fn default() -> Self {
        // Safety: all-zero is valid for this POD struct
        unsafe { std::mem::zeroed() }
    }
}

/// Ticket variant for different signature types.
#[derive(Debug, Clone)]
pub enum Ticket {
    Invalid,
    RSA4096 {
        sig_type: SignatureType,
        sig_data: [u8; 0x200],
        _padding: [u8; 0x3C],
        data: TicketData,
    },
    RSA2048 {
        sig_type: SignatureType,
        sig_data: [u8; 0x100],
        _padding: [u8; 0x3C],
        data: TicketData,
    },
    ECDSA {
        sig_type: SignatureType,
        sig_data: [u8; 0x3C],
        _padding: [u8; 0x40],
        data: TicketData,
    },
}

/// Read TicketData from raw bytes at the given offset by raw memcpy.
/// Upstream does `std::memcpy(&ticket, raw_data.data(), sizeof(ticket))` which
/// copies the entire struct. We parse just the TicketData at the data offset.
fn read_ticket_data(raw_data: &[u8], data_offset: usize) -> TicketData {
    let td_size = std::mem::size_of::<TicketData>();
    let mut ticket_data = TicketData::default();
    if raw_data.len() >= data_offset + td_size {
        let dst = unsafe {
            std::slice::from_raw_parts_mut(
                &mut ticket_data as *mut TicketData as *mut u8,
                td_size,
            )
        };
        dst.copy_from_slice(&raw_data[data_offset..data_offset + td_size]);
    } else if raw_data.len() > data_offset {
        // Partial copy - some tools provide short ticket data
        let available = raw_data.len() - data_offset;
        let dst = unsafe {
            std::slice::from_raw_parts_mut(
                &mut ticket_data as *mut TicketData as *mut u8,
                td_size,
            )
        };
        dst[..available].copy_from_slice(&raw_data[data_offset..]);
    }
    ticket_data
}

impl Ticket {
    pub fn is_valid(&self) -> bool {
        !matches!(self, Ticket::Invalid)
    }

    pub fn get_signature_type(&self) -> Option<SignatureType> {
        match self {
            Ticket::RSA4096 { sig_type, .. } => Some(*sig_type),
            Ticket::RSA2048 { sig_type, .. } => Some(*sig_type),
            Ticket::ECDSA { sig_type, .. } => Some(*sig_type),
            Ticket::Invalid => None,
        }
    }

    pub fn get_data(&self) -> Option<&TicketData> {
        match self {
            Ticket::RSA4096 { data, .. } => Some(data),
            Ticket::RSA2048 { data, .. } => Some(data),
            Ticket::ECDSA { data, .. } => Some(data),
            Ticket::Invalid => None,
        }
    }

    pub fn get_data_mut(&mut self) -> Option<&mut TicketData> {
        match self {
            Ticket::RSA4096 { data, .. } => Some(data),
            Ticket::RSA2048 { data, .. } => Some(data),
            Ticket::ECDSA { data, .. } => Some(data),
            Ticket::Invalid => None,
        }
    }

    /// Synthesize a common ticket given a title key and rights ID.
    pub fn synthesize_common(title_key: Key128, rights_id: &[u8; 0x10]) -> Self {
        let mut data = TicketData::default();
        data.rights_id = *rights_id;
        data.title_key_block[..16].copy_from_slice(&title_key);

        Ticket::RSA2048 {
            sig_type: SignatureType::RSA2048SHA256,
            sig_data: [0u8; 0x100],
            _padding: [0u8; 0x3C],
            data,
        }
    }

    /// Read a ticket from raw bytes.
    /// Corresponds to upstream `Ticket::Read(std::span<const u8>)`.
    pub fn read_from_bytes(raw_data: &[u8]) -> Self {
        if raw_data.len() < std::mem::size_of::<u32>() {
            log::warn!(
                "Attempted to parse ticket buffer with invalid size {}.",
                raw_data.len()
            );
            return Ticket::Invalid;
        }

        let sig_type_val = u32::from_le_bytes([
            raw_data[0],
            raw_data[1],
            raw_data[2],
            raw_data[3],
        ]);

        match sig_type_val {
            // RSA-4096 types
            0x10000 | 0x10003 => {
                let sig_type = if sig_type_val == 0x10000 {
                    SignatureType::RSA4096SHA1
                } else {
                    SignatureType::RSA4096SHA256
                };
                // 4 (sig_type) + 0x200 (sig) + 0x3C (pad) + sizeof(TicketData)
                let data_offset = 4 + 0x200 + 0x3C;
                let ticket_data = read_ticket_data(raw_data, data_offset);
                let mut sig_data = [0u8; 0x200];
                let sig_end = (4 + 0x200).min(raw_data.len());
                let copy_len = sig_end.saturating_sub(4);
                sig_data[..copy_len].copy_from_slice(&raw_data[4..4 + copy_len]);
                let mut padding = [0u8; 0x3C];
                let pad_start = 4 + 0x200;
                let pad_end = (pad_start + 0x3C).min(raw_data.len());
                let pad_len = pad_end.saturating_sub(pad_start);
                if pad_len > 0 {
                    padding[..pad_len].copy_from_slice(&raw_data[pad_start..pad_start + pad_len]);
                }
                Ticket::RSA4096 {
                    sig_type,
                    sig_data,
                    _padding: padding,
                    data: ticket_data,
                }
            }
            // RSA-2048 types
            0x10001 | 0x10004 => {
                let sig_type = if sig_type_val == 0x10001 {
                    SignatureType::RSA2048SHA1
                } else {
                    SignatureType::RSA2048SHA256
                };
                let data_offset = 4 + 0x100 + 0x3C;
                let ticket_data = read_ticket_data(raw_data, data_offset);
                let mut sig_data = [0u8; 0x100];
                let sig_end = (4 + 0x100).min(raw_data.len());
                let copy_len = sig_end.saturating_sub(4);
                sig_data[..copy_len].copy_from_slice(&raw_data[4..4 + copy_len]);
                let mut padding = [0u8; 0x3C];
                let pad_start = 4 + 0x100;
                let pad_end = (pad_start + 0x3C).min(raw_data.len());
                let pad_len = pad_end.saturating_sub(pad_start);
                if pad_len > 0 {
                    padding[..pad_len].copy_from_slice(&raw_data[pad_start..pad_start + pad_len]);
                }
                Ticket::RSA2048 {
                    sig_type,
                    sig_data,
                    _padding: padding,
                    data: ticket_data,
                }
            }
            // ECDSA types
            0x10002 | 0x10005 => {
                let sig_type = if sig_type_val == 0x10002 {
                    SignatureType::ECDSASHA1
                } else {
                    SignatureType::ECDSASHA256
                };
                let data_offset = 4 + 0x3C + 0x40;
                let ticket_data = read_ticket_data(raw_data, data_offset);
                let mut sig_data = [0u8; 0x3C];
                let sig_end = (4 + 0x3C).min(raw_data.len());
                let copy_len = sig_end.saturating_sub(4);
                sig_data[..copy_len].copy_from_slice(&raw_data[4..4 + copy_len]);
                let mut padding = [0u8; 0x40];
                let pad_start = 4 + 0x3C;
                let pad_end = (pad_start + 0x40).min(raw_data.len());
                let pad_len = pad_end.saturating_sub(pad_start);
                if pad_len > 0 {
                    padding[..pad_len].copy_from_slice(&raw_data[pad_start..pad_start + pad_len]);
                }
                Ticket::ECDSA {
                    sig_type,
                    sig_data,
                    _padding: padding,
                    data: ticket_data,
                }
            }
            _ => {
                log::warn!(
                    "Attempted to parse ticket buffer with invalid type {:X}.",
                    sig_type_val
                );
                Ticket::Invalid
            }
        }
    }

    /// Get the total size of the ticket in bytes.
    /// Corresponds to upstream `Ticket::GetSize`.
    pub fn get_size(&self) -> u64 {
        let sig_type = match self.get_signature_type() {
            Some(st) => st,
            None => return 0,
        };
        std::mem::size_of::<u32>() as u64
            + get_signature_type_data_size(sig_type)
            + get_signature_type_padding_size(sig_type)
            + std::mem::size_of::<TicketData>() as u64
    }
}

/// RSA key pair (generic over bit size).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RsaKeyPair<const BYTE_SIZE: usize> {
    pub encryption_key: Vec<u8>,
    pub decryption_key: Vec<u8>,
    pub modulus: Vec<u8>,
    pub exponent: [u8; 4],
}

impl<const BYTE_SIZE: usize> Default for RsaKeyPair<BYTE_SIZE> {
    fn default() -> Self {
        Self {
            encryption_key: vec![0u8; BYTE_SIZE],
            decryption_key: vec![0u8; BYTE_SIZE],
            modulus: vec![0u8; BYTE_SIZE],
            exponent: [0u8; 4],
        }
    }
}

/// Key category for file storage.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum KeyCategory {
    Standard,
    Title,
    Console,
}

/// 256-bit key type enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u64)]
pub enum S256KeyType {
    SDKey = 0,        // f1=SDKeyType
    Header = 1,       //
    SDKeySource = 2,  // f1=SDKeyType
    HeaderSource = 3, //
}

/// 128-bit key type enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u64)]
pub enum S128KeyType {
    Master = 0,        // f1=crypto revision
    Package1 = 1,      // f1=crypto revision
    Package2 = 2,      // f1=crypto revision
    Titlekek = 3,      // f1=crypto revision
    ETicketRSAKek = 4, //
    KeyArea = 5,       // f1=crypto revision f2=type {app, ocean, system}
    SDSeed = 6,        //
    Titlekey = 7,      // f1=rights id LSB f2=rights id MSB
    Source = 8,        // f1=source type, f2=sub id
    Keyblob = 9,       // f1=crypto revision
    KeyblobMAC = 10,   // f1=crypto revision
    TSEC = 11,         //
    SecureBoot = 12,   //
    BIS = 13,          // f1=partition (0-3), f2=type {crypt, tweak}
    HeaderKek = 14,    //
    SDKek = 15,        //
    RSAKek = 16,       //
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum KeyAreaKeyType {
    Application = 0,
    Ocean = 1,
    System = 2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum SourceKeyType {
    SDKek = 0,
    AESKekGeneration = 1,
    AESKeyGeneration = 2,
    RSAOaepKekGeneration = 3,
    Master = 4,
    Keyblob = 5,     // f2=crypto revision
    KeyAreaKey = 6,   // f2=KeyAreaKeyType
    Titlekek = 7,
    Package2 = 8,
    HeaderKek = 9,
    KeyblobMAC = 10,
    ETicketKek = 11,
    ETicketKekek = 12,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum SDKeyType {
    Save = 0,
    NCA = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum BISKeyType {
    Crypto = 0,
    Tweak = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum RSAKekType {
    Mask0 = 0,
    Seed3 = 1,
}

/// Key index for looking up keys in the key maps.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeyIndex<T: Ord> {
    pub key_type: T,
    pub field1: u64,
    pub field2: u64,
}

// ---------------------------------------------------------------------------
// Static key name → KeyIndex mappings (port of s128_file_id / s256_file_id / KEYS_VARIABLE_LENGTH)
// ---------------------------------------------------------------------------

/// Fixed 128-bit key names that map to a single KeyIndex (no trailing hex index).
/// Port of upstream `s128_file_id`.
const S128_FILE_ID: &[(&str, S128KeyType, u64, u64)] = &[
    ("eticket_rsa_kek", S128KeyType::ETicketRSAKek, 0, 0),
    ("eticket_rsa_kek_source", S128KeyType::Source, SourceKeyType::ETicketKek as u64, 0),
    ("eticket_rsa_kekek_source", S128KeyType::Source, SourceKeyType::ETicketKekek as u64, 0),
    ("rsa_kek_mask_0", S128KeyType::RSAKek, RSAKekType::Mask0 as u64, 0),
    ("rsa_kek_seed_3", S128KeyType::RSAKek, RSAKekType::Seed3 as u64, 0),
    ("rsa_oaep_kek_generation_source", S128KeyType::Source, SourceKeyType::RSAOaepKekGeneration as u64, 0),
    ("sd_card_kek_source", S128KeyType::Source, SourceKeyType::SDKek as u64, 0),
    ("aes_kek_generation_source", S128KeyType::Source, SourceKeyType::AESKekGeneration as u64, 0),
    ("aes_key_generation_source", S128KeyType::Source, SourceKeyType::AESKeyGeneration as u64, 0),
    ("package2_key_source", S128KeyType::Source, SourceKeyType::Package2 as u64, 0),
    ("master_key_source", S128KeyType::Source, SourceKeyType::Master as u64, 0),
    ("header_kek_source", S128KeyType::Source, SourceKeyType::HeaderKek as u64, 0),
    ("key_area_key_application_source", S128KeyType::Source, SourceKeyType::KeyAreaKey as u64, KeyAreaKeyType::Application as u64),
    ("key_area_key_ocean_source", S128KeyType::Source, SourceKeyType::KeyAreaKey as u64, KeyAreaKeyType::Ocean as u64),
    ("key_area_key_system_source", S128KeyType::Source, SourceKeyType::KeyAreaKey as u64, KeyAreaKeyType::System as u64),
    ("titlekek_source", S128KeyType::Source, SourceKeyType::Titlekek as u64, 0),
    ("keyblob_mac_key_source", S128KeyType::Source, SourceKeyType::KeyblobMAC as u64, 0),
    ("tsec_key", S128KeyType::TSEC, 0, 0),
    ("secure_boot_key", S128KeyType::SecureBoot, 0, 0),
    ("sd_seed", S128KeyType::SDSeed, 0, 0),
    ("bis_key_0_crypt", S128KeyType::BIS, 0, BISKeyType::Crypto as u64),
    ("bis_key_0_tweak", S128KeyType::BIS, 0, BISKeyType::Tweak as u64),
    ("bis_key_1_crypt", S128KeyType::BIS, 1, BISKeyType::Crypto as u64),
    ("bis_key_1_tweak", S128KeyType::BIS, 1, BISKeyType::Tweak as u64),
    ("bis_key_2_crypt", S128KeyType::BIS, 2, BISKeyType::Crypto as u64),
    ("bis_key_2_tweak", S128KeyType::BIS, 2, BISKeyType::Tweak as u64),
    ("bis_key_3_crypt", S128KeyType::BIS, 3, BISKeyType::Crypto as u64),
    ("bis_key_3_tweak", S128KeyType::BIS, 3, BISKeyType::Tweak as u64),
    ("header_kek", S128KeyType::HeaderKek, 0, 0),
    ("sd_card_kek", S128KeyType::SDKek, 0, 0),
];

/// Fixed 256-bit key names that map to a single KeyIndex.
/// Port of upstream `s256_file_id`.
const S256_FILE_ID: &[(&str, S256KeyType, u64, u64)] = &[
    ("header_key", S256KeyType::Header, 0, 0),
    ("sd_card_save_key_source", S256KeyType::SDKeySource, SDKeyType::Save as u64, 0),
    ("sd_card_nca_key_source", S256KeyType::SDKeySource, SDKeyType::NCA as u64, 0),
    ("header_key_source", S256KeyType::HeaderSource, 0, 0),
    ("sd_card_save_key", S256KeyType::SDKey, SDKeyType::Save as u64, 0),
    ("sd_card_nca_key", S256KeyType::SDKey, SDKeyType::NCA as u64, 0),
];

/// Variable-length 128-bit key names with a trailing hex revision index.
/// Port of upstream `KEYS_VARIABLE_LENGTH`.
/// Each entry is (key_type, sub_id, prefix_string).
/// If sub_id == 0, the parsed hex index goes into field1; otherwise it goes into field2.
const KEYS_VARIABLE_LENGTH: &[(S128KeyType, u64, &str)] = &[
    (S128KeyType::Master, 0, "master_key_"),
    (S128KeyType::Package1, 0, "package1_key_"),
    (S128KeyType::Package2, 0, "package2_key_"),
    (S128KeyType::Titlekek, 0, "titlekek_"),
    (S128KeyType::Source, SourceKeyType::Keyblob as u64, "keyblob_key_source_"),
    (S128KeyType::Keyblob, 0, "keyblob_key_"),
    (S128KeyType::KeyblobMAC, 0, "keyblob_mac_key_"),
];

/// Key area key prefixes with their KeyAreaKeyType index.
/// Port of upstream `kak_names` array.
const KAK_NAMES: &[(&str, u64)] = &[
    ("key_area_key_application_", KeyAreaKeyType::Application as u64),
    ("key_area_key_ocean_", KeyAreaKeyType::Ocean as u64),
    ("key_area_key_system_", KeyAreaKeyType::System as u64),
];

/// Check that a substring of `base` starting at `begin` with `length` chars consists entirely
/// of hex digits. Port of upstream `ValidCryptoRevisionString`.
fn valid_crypto_revision_string(base: &str, begin: usize, length: usize) -> bool {
    if base.len() < begin + length {
        return false;
    }
    base[begin..begin + length]
        .bytes()
        .all(|c| c.is_ascii_hexdigit())
}

/// Find a 128-bit key name in the static table.
fn find_128_by_name(name: &str) -> Option<(S128KeyType, u64, u64)> {
    for &(n, kt, f1, f2) in S128_FILE_ID {
        if n == name {
            return Some((kt, f1, f2));
        }
    }
    None
}

/// Find a 256-bit key name in the static table.
fn find_256_by_name(name: &str) -> Option<(S256KeyType, u64, u64)> {
    for &(n, kt, f1, f2) in S256_FILE_ID {
        if n == name {
            return Some((kt, f1, f2));
        }
    }
    None
}

/// Reverse lookup: find the static name for a 128-bit key index.
fn find_128_name_by_index(id: S128KeyType, field1: u64, field2: u64) -> Option<&'static str> {
    for &(name, kt, f1, f2) in S128_FILE_ID {
        if kt == id && f1 == field1 && f2 == field2 {
            return Some(name);
        }
    }
    None
}

/// Reverse lookup: find the static name for a 256-bit key index.
fn find_256_name_by_index(id: S256KeyType, field1: u64, field2: u64) -> Option<&'static str> {
    for &(name, kt, f1, f2) in S256_FILE_ID {
        if kt == id && f1 == field1 && f2 == field2 {
            return Some(name);
        }
    }
    None
}

/// Resolve the keys directory. Uses the common path manager (RuzuPath::KeysDir).
/// Also searches fallback locations used by yuzu/suyu for user convenience.
fn resolve_keys_dir() -> PathBuf {
    // Primary: use the common path manager
    let primary = get_ruzu_path(RuzuPath::KeysDir);
    if primary.exists() {
        return primary;
    }

    // Fallback locations (matching yuzu/suyu key file paths)
    let home = std::env::var("HOME").unwrap_or_default();
    let fallbacks = [
        format!("{}/.local/share/yuzu/keys", home),
        format!("{}/.config/yuzu/keys", home),
        format!("{}/.local/share/suyu/keys", home),
    ];

    for fb in &fallbacks {
        let p = PathBuf::from(fb);
        if p.exists() {
            return p;
        }
    }

    // Return primary even if it does not exist; caller will handle missing files.
    primary
}

/// Key manager singleton. Manages all cryptographic keys.
/// Port of Core::Crypto::KeyManager.
pub struct KeyManager {
    s128_keys: BTreeMap<KeyIndex<S128KeyType>, Key128>,
    s256_keys: BTreeMap<KeyIndex<S256KeyType>, Key256>,
    common_tickets: BTreeMap<u128, Ticket>,
    personal_tickets: BTreeMap<u128, Ticket>,
    ticket_databases_loaded: bool,
    encrypted_keyblobs: [[u8; 0xB0]; 0x20],
    keyblobs: [[u8; 0x90]; 0x20],
    eticket_extended_kek: [u8; 576],
    eticket_rsa_keypair: RsaKeyPair<256>, // 2048-bit RSA
    dev_mode: bool,
}

impl KeyManager {
    /// Returns the singleton KeyManager instance.
    ///
    /// Corresponds to upstream `KeyManager::Instance()`.
    /// Keys are loaded from disk once on first access.
    pub fn instance() -> Arc<Mutex<KeyManager>> {
        static INSTANCE: OnceLock<Arc<Mutex<KeyManager>>> = OnceLock::new();
        INSTANCE
            .get_or_init(|| Arc::new(Mutex::new(KeyManager::new())))
            .clone()
    }

    pub fn new() -> Self {
        let mut mgr = Self {
            s128_keys: BTreeMap::new(),
            s256_keys: BTreeMap::new(),
            common_tickets: BTreeMap::new(),
            personal_tickets: BTreeMap::new(),
            ticket_databases_loaded: false,
            encrypted_keyblobs: [[0u8; 0xB0]; 0x20],
            keyblobs: [[0u8; 0x90]; 0x20],
            eticket_extended_kek: [0u8; 576],
            eticket_rsa_keypair: RsaKeyPair::default(),
            dev_mode: false,
        };
        mgr.reload_keys();
        mgr
    }

    pub fn has_key_128(&self, id: S128KeyType, field1: u64, field2: u64) -> bool {
        self.s128_keys.contains_key(&KeyIndex {
            key_type: id,
            field1,
            field2,
        })
    }

    pub fn has_key_256(&self, id: S256KeyType, field1: u64, field2: u64) -> bool {
        self.s256_keys.contains_key(&KeyIndex {
            key_type: id,
            field1,
            field2,
        })
    }

    pub fn get_key_128(&self, id: S128KeyType, field1: u64, field2: u64) -> Key128 {
        self.s128_keys
            .get(&KeyIndex {
                key_type: id,
                field1,
                field2,
            })
            .copied()
            .unwrap_or([0u8; 16])
    }

    pub fn get_key_256(&self, id: S256KeyType, field1: u64, field2: u64) -> Key256 {
        self.s256_keys
            .get(&KeyIndex {
                key_type: id,
                field1,
                field2,
            })
            .copied()
            .unwrap_or([0u8; 32])
    }

    pub fn get_bis_key(&self, partition_id: u8) -> Key256 {
        let mut out = [0u8; 32];
        for bis_type in [BISKeyType::Crypto, BISKeyType::Tweak] {
            let idx = KeyIndex {
                key_type: S128KeyType::BIS,
                field1: partition_id as u64,
                field2: bis_type as u64,
            };
            if let Some(key) = self.s128_keys.get(&idx) {
                let offset = (bis_type as usize) * 16;
                out[offset..offset + 16].copy_from_slice(key);
            }
        }
        out
    }

    pub fn set_key_128(&mut self, id: S128KeyType, key: Key128, field1: u64, field2: u64) {
        if key == [0u8; 16] {
            return;
        }
        let idx = KeyIndex {
            key_type: id,
            field1,
            field2,
        };
        if self.s128_keys.contains_key(&idx) {
            return;
        }

        // WriteKeyToFile for title keys
        if id == S128KeyType::Titlekey {
            let mut rights_id = [0u8; 16];
            rights_id[..8].copy_from_slice(&field2.to_le_bytes());
            rights_id[8..].copy_from_slice(&field1.to_le_bytes());
            self.write_key_to_file(
                KeyCategory::Title,
                &common::hex_util::hex_to_string(&rights_id, false),
                &key,
            );
        }

        // Determine category
        let category = match id {
            S128KeyType::Keyblob
            | S128KeyType::KeyblobMAC
            | S128KeyType::TSEC
            | S128KeyType::SecureBoot
            | S128KeyType::SDSeed
            | S128KeyType::BIS => KeyCategory::Console,
            _ => KeyCategory::Standard,
        };

        // Try fixed name lookup
        if let Some(name) = find_128_name_by_index(id, field1, field2) {
            self.write_key_to_file(category, name, &key);
        }

        // Variable-length key names
        match id {
            S128KeyType::KeyArea => {
                let kak_names = [
                    "key_area_key_application_",
                    "key_area_key_ocean_",
                    "key_area_key_system_",
                ];
                if (field2 as usize) < kak_names.len() {
                    self.write_key_to_file(
                        category,
                        &format!("{}{:02X}", kak_names[field2 as usize], field1),
                        &key,
                    );
                }
            }
            S128KeyType::Master => {
                self.write_key_to_file(category, &format!("master_key_{:02X}", field1), &key);
            }
            S128KeyType::Package1 => {
                self.write_key_to_file(category, &format!("package1_key_{:02X}", field1), &key);
            }
            S128KeyType::Package2 => {
                self.write_key_to_file(category, &format!("package2_key_{:02X}", field1), &key);
            }
            S128KeyType::Titlekek => {
                self.write_key_to_file(category, &format!("titlekek_{:02X}", field1), &key);
            }
            S128KeyType::Keyblob => {
                self.write_key_to_file(category, &format!("keyblob_key_{:02X}", field1), &key);
            }
            S128KeyType::KeyblobMAC => {
                self.write_key_to_file(
                    category,
                    &format!("keyblob_mac_key_{:02X}", field1),
                    &key,
                );
            }
            S128KeyType::Source if field1 == SourceKeyType::Keyblob as u64 => {
                self.write_key_to_file(
                    category,
                    &format!("keyblob_key_source_{:02X}", field2),
                    &key,
                );
            }
            _ => {}
        }

        self.s128_keys.insert(idx, key);
    }

    pub fn set_key_256(&mut self, id: S256KeyType, key: Key256, field1: u64, field2: u64) {
        if key == [0u8; 32] {
            return;
        }
        let idx = KeyIndex {
            key_type: id,
            field1,
            field2,
        };
        if self.s256_keys.contains_key(&idx) {
            return;
        }

        // Try fixed name lookup
        if let Some(name) = find_256_name_by_index(id, field1, field2) {
            self.write_key_to_file(KeyCategory::Standard, name, &key);
        }

        self.s256_keys.insert(idx, key);
    }

    /// Check if the key file exists on disk.
    /// Port of upstream `KeyManager::KeyFileExists`.
    pub fn key_file_exists(title: bool) -> bool {
        let keys_dir = resolve_keys_dir();

        if title {
            return keys_dir.join("title.keys").exists();
        }

        let dev_mode = *common::settings::values().use_dev_keys.get_value();
        if dev_mode {
            keys_dir.join("dev.keys").exists()
        } else {
            keys_dir.join("prod.keys").exists()
        }
    }

    pub fn derive_sd_seed_lazy(&mut self) {
        if self.has_key_128(S128KeyType::SDSeed, 0, 0) {
            return;
        }
        if let Some(seed) = derive_sd_seed() {
            self.set_key_128(S128KeyType::SDSeed, seed, 0, 0);
        }
    }

    /// Check if key derivation is necessary.
    /// Corresponds to upstream `KeyManager::BaseDeriveNecessary`.
    pub fn base_derive_necessary(&self) -> bool {
        if !self.has_key_256(S256KeyType::Header, 0, 0) {
            return true;
        }

        for i in 0..CURRENT_CRYPTO_REVISION as u64 {
            if !self.has_key_128(S128KeyType::Master, i, 0)
                || !self.has_key_128(
                    S128KeyType::KeyArea,
                    i,
                    KeyAreaKeyType::Application as u64,
                )
                || !self.has_key_128(S128KeyType::KeyArea, i, KeyAreaKeyType::Ocean as u64)
                || !self.has_key_128(S128KeyType::KeyArea, i, KeyAreaKeyType::System as u64)
                || !self.has_key_128(S128KeyType::Titlekek, i, 0)
            {
                return true;
            }
        }

        false
    }

    /// Full key derivation pipeline.
    /// Corresponds to upstream `KeyManager::DeriveBase`.
    pub fn derive_base(&mut self) {
        if !self.base_derive_necessary() {
            return;
        }

        if !self.has_key_128(S128KeyType::SecureBoot, 0, 0)
            || !self.has_key_128(S128KeyType::TSEC, 0, 0)
        {
            return;
        }

        // Copy BIS keys between partitions 2 and 3 if one is missing
        let has_bis = |this: &Self, id: u64| {
            this.has_key_128(S128KeyType::BIS, id, BISKeyType::Crypto as u64)
                && this.has_key_128(S128KeyType::BIS, id, BISKeyType::Tweak as u64)
        };

        if has_bis(self, 2) && !has_bis(self, 3) {
            let crypto = self.get_key_128(S128KeyType::BIS, 2, BISKeyType::Crypto as u64);
            let tweak = self.get_key_128(S128KeyType::BIS, 2, BISKeyType::Tweak as u64);
            self.set_key_128(S128KeyType::BIS, crypto, 3, BISKeyType::Crypto as u64);
            self.set_key_128(S128KeyType::BIS, tweak, 3, BISKeyType::Tweak as u64);
        } else if has_bis(self, 3) && !has_bis(self, 2) {
            let crypto = self.get_key_128(S128KeyType::BIS, 3, BISKeyType::Crypto as u64);
            let tweak = self.get_key_128(S128KeyType::BIS, 3, BISKeyType::Tweak as u64);
            self.set_key_128(S128KeyType::BIS, crypto, 2, BISKeyType::Crypto as u64);
            self.set_key_128(S128KeyType::BIS, tweak, 2, BISKeyType::Tweak as u64);
        }

        // Find which revisions have keyblob sources and encrypted keyblobs
        let mut revisions = [false; 32];
        for i in 0..32 {
            revisions[i] = self.has_key_128(
                S128KeyType::Source,
                SourceKeyType::Keyblob as u64,
                i as u64,
            ) && self.encrypted_keyblobs[i] != [0u8; 0xB0];
        }

        if !revisions.iter().any(|&b| b) {
            return;
        }

        let sbk = self.get_key_128(S128KeyType::SecureBoot, 0, 0);
        let tsec = self.get_key_128(S128KeyType::TSEC, 0, 0);

        for i in 0..32 {
            if !revisions[i] {
                continue;
            }

            // Derive keyblob key
            let source = self.get_key_128(
                S128KeyType::Source,
                SourceKeyType::Keyblob as u64,
                i as u64,
            );
            let key = derive_keyblob_key(&sbk, &tsec, source);
            self.set_key_128(S128KeyType::Keyblob, key, i as u64, 0);

            // Derive keyblob MAC key
            if !self.has_key_128(S128KeyType::Source, SourceKeyType::KeyblobMAC as u64, 0) {
                continue;
            }
            let mac_source =
                self.get_key_128(S128KeyType::Source, SourceKeyType::KeyblobMAC as u64, 0);
            let mac_key = derive_keyblob_mac_key(&key, &mac_source);
            self.set_key_128(S128KeyType::KeyblobMAC, mac_key, i as u64, 0);

            // Verify CMAC
            let cmac = calculate_cmac(&self.encrypted_keyblobs[i][0x10..0x10 + 0xA0], &mac_key);
            if cmac != self.encrypted_keyblobs[i][..0x10] {
                continue;
            }

            // Decrypt keyblob
            if self.keyblobs[i] == [0u8; 0x90] {
                self.keyblobs[i] = decrypt_keyblob(&self.encrypted_keyblobs[i], &key);
                self.write_key_to_file(
                    KeyCategory::Console,
                    &format!("keyblob_{:02X}", i),
                    &self.keyblobs[i],
                );
            }

            // Extract package1 key (at offset 0x80 in keyblob)
            let mut package1 = [0u8; 16];
            package1.copy_from_slice(&self.keyblobs[i][0x80..0x90]);
            self.set_key_128(S128KeyType::Package1, package1, i as u64, 0);

            // Derive master key
            if self.has_key_128(S128KeyType::Source, SourceKeyType::Master as u64, 0) {
                let master_source =
                    self.get_key_128(S128KeyType::Source, SourceKeyType::Master as u64, 0);
                let master = derive_master_key(&self.keyblobs[i], &master_source);
                self.set_key_128(S128KeyType::Master, master, i as u64, 0);
            }
        }

        // Find which revisions have master keys
        let mut master_revisions = [false; 32];
        for i in 0..32 {
            master_revisions[i] = self.has_key_128(S128KeyType::Master, i as u64, 0);
        }

        if !master_revisions.iter().any(|&b| b) {
            return;
        }

        // Derive general purpose keys for each revision
        for i in 0..32 {
            if !master_revisions[i] {
                continue;
            }
            self.derive_general_purpose_keys(i);
        }

        // Derive header key
        if self.has_key_128(S128KeyType::Master, 0, 0)
            && self.has_key_128(
                S128KeyType::Source,
                SourceKeyType::AESKeyGeneration as u64,
                0,
            )
            && self.has_key_128(
                S128KeyType::Source,
                SourceKeyType::AESKekGeneration as u64,
                0,
            )
            && self.has_key_128(S128KeyType::Source, SourceKeyType::HeaderKek as u64, 0)
            && self.has_key_256(S256KeyType::HeaderSource, 0, 0)
        {
            let header_kek = generate_key_encryption_key(
                self.get_key_128(S128KeyType::Source, SourceKeyType::HeaderKek as u64, 0),
                self.get_key_128(S128KeyType::Master, 0, 0),
                self.get_key_128(
                    S128KeyType::Source,
                    SourceKeyType::AESKekGeneration as u64,
                    0,
                ),
                self.get_key_128(
                    S128KeyType::Source,
                    SourceKeyType::AESKeyGeneration as u64,
                    0,
                ),
            );
            self.set_key_128(S128KeyType::HeaderKek, header_kek, 0, 0);

            let mut header_cipher = super::aes_util::AesCipher::new_128(
                header_kek,
                super::aes_util::Mode::ECB,
            );
            let mut out = self.get_key_256(S256KeyType::HeaderSource, 0, 0);
            let src = out;
            header_cipher.transcode(&src, &mut out, super::aes_util::Op::Decrypt);
            self.set_key_256(S256KeyType::Header, out, 0, 0);
        }
    }

    /// Derive ETicket keys from partition data and content provider.
    /// Corresponds to upstream `KeyManager::DeriveETicket`.
    pub fn derive_e_ticket(
        &mut self,
        data: &mut super::partition_data_manager::PartitionDataManager,
        provider: &dyn crate::file_sys::registered_cache::ContentProvider,
    ) {
        // ETicket keys - get ES title (0x0100000000000033)
        let es = match provider.get_entry(
            0x0100000000000033,
            crate::file_sys::nca_metadata::ContentRecordType::Program,
        ) {
            Some(nca) => nca,
            None => return,
        };

        let exefs = match es.get_exefs() {
            Some(d) => d,
            None => return,
        };

        let main_file = match exefs.get_file("main") {
            Some(f) => f,
            None => return,
        };

        let bytes = main_file.read_all_bytes();

        // Search for eticket source keys
        let eticket_source_hashes: [[u8; 0x20]; 2] = [
            hex_string_to_array("B71DB271DC338DF380AA2C4335EF8873B1AFD408E80B3582D8719FC81C5E511C"),
            hex_string_to_array("E8965A187D30E57869F562D04383C996DE487BBA5761363D2D4D32391866A85C"),
        ];

        let eticket_kek =
            super::partition_data_manager::find_key_from_hex16(&bytes, eticket_source_hashes[0]);
        let eticket_kekek =
            super::partition_data_manager::find_key_from_hex16(&bytes, eticket_source_hashes[1]);

        let seed3 = data.get_rsa_kek_seed3();
        let mask0 = data.get_rsa_kek_mask0();

        if eticket_kek != [0u8; 16] {
            self.set_key_128(
                S128KeyType::Source,
                eticket_kek,
                SourceKeyType::ETicketKek as u64,
                0,
            );
        }
        if eticket_kekek != [0u8; 16] {
            self.set_key_128(
                S128KeyType::Source,
                eticket_kekek,
                SourceKeyType::ETicketKekek as u64,
                0,
            );
        }
        if seed3 != [0u8; 16] {
            self.set_key_128(
                S128KeyType::RSAKek,
                seed3,
                RSAKekType::Seed3 as u64,
                0,
            );
        }
        if mask0 != [0u8; 16] {
            self.set_key_128(
                S128KeyType::RSAKek,
                mask0,
                RSAKekType::Mask0 as u64,
                0,
            );
        }

        if eticket_kek == [0u8; 16]
            || eticket_kekek == [0u8; 16]
            || seed3 == [0u8; 16]
            || mask0 == [0u8; 16]
        {
            return;
        }

        // rsa_oaep_kek = seed3 ^ mask0
        let mut rsa_oaep_kek = [0u8; 16];
        for i in 0..16 {
            rsa_oaep_kek[i] = seed3[i] ^ mask0[i];
        }
        if rsa_oaep_kek == [0u8; 16] {
            return;
        }

        self.set_key_128(
            S128KeyType::Source,
            rsa_oaep_kek,
            SourceKeyType::RSAOaepKekGeneration as u64,
            0,
        );

        // Derive ETicket RSA Kek
        let master_00 = self.get_key_128(S128KeyType::Master, 0, 0);
        let mut temp_kek = [0u8; 16];
        let mut temp_kekek = [0u8; 16];
        let mut eticket_final = [0u8; 16];

        let mut es_master =
            super::aes_util::AesCipher::new_128(master_00, super::aes_util::Mode::ECB);
        es_master.transcode(
            &rsa_oaep_kek,
            &mut temp_kek,
            super::aes_util::Op::Decrypt,
        );
        let mut es_kekek =
            super::aes_util::AesCipher::new_128(temp_kek, super::aes_util::Mode::ECB);
        es_kekek.transcode(
            &eticket_kekek,
            &mut temp_kekek,
            super::aes_util::Op::Decrypt,
        );
        let mut es_kek =
            super::aes_util::AesCipher::new_128(temp_kekek, super::aes_util::Mode::ECB);
        es_kek.transcode(
            &eticket_kek,
            &mut eticket_final,
            super::aes_util::Op::Decrypt,
        );

        if eticket_final == [0u8; 16] {
            return;
        }

        self.set_key_128(S128KeyType::ETicketRSAKek, eticket_final, 0, 0);

        // Decrypt PRODINFO and get eticket extended kek
        data.decrypt_prodinfo(self.get_bis_key(0));

        self.eticket_extended_kek = data.get_eticket_extended_kek();
        self.write_key_to_file(
            KeyCategory::Console,
            "eticket_extended_kek",
            &self.eticket_extended_kek,
        );
        self.derive_e_ticket_rsa_key();
        self.populate_tickets();
    }

    /// Populate tickets from NAND save files.
    /// Corresponds to upstream `KeyManager::PopulateTickets`.
    pub fn populate_tickets(&mut self) {
        if self.ticket_databases_loaded {
            return;
        }
        self.ticket_databases_loaded = true;

        let mut tickets = Vec::new();

        let nand_dir = get_ruzu_path(RuzuPath::NANDDir);

        let system_save_e1_path = nand_dir.join("system/save/80000000000000e1");
        if system_save_e1_path.exists() {
            if let Ok(data) = std::fs::read(&system_save_e1_path) {
                let blob = get_ticketblob(&data);
                tickets.extend(blob);
            }
        }

        let system_save_e2_path = nand_dir.join("system/save/80000000000000e2");
        if system_save_e2_path.exists() {
            if let Ok(data) = std::fs::read(&system_save_e2_path) {
                let blob = get_ticketblob(&data);
                tickets.extend(blob);
            }
        }

        for ticket in &tickets {
            self.add_ticket(ticket);
        }
    }

    pub fn synthesize_tickets(&mut self) {
        for (idx, key) in &self.s128_keys {
            if idx.key_type != S128KeyType::Titlekey {
                continue;
            }
            let rights_id_u128 = ((idx.field1 as u128) << 64) | (idx.field2 as u128);
            let mut rights_id_bytes = [0u8; 16];
            rights_id_bytes.copy_from_slice(&rights_id_u128.to_le_bytes());
            let ticket = Ticket::synthesize_common(*key, &rights_id_bytes);
            self.common_tickets.insert(rights_id_u128, ticket);
        }
    }

    /// Populate keys from partition data.
    /// Corresponds to upstream `KeyManager::PopulateFromPartitionData`.
    pub fn populate_from_partition_data(
        &mut self,
        data: &mut super::partition_data_manager::PartitionDataManager,
    ) {
        if !self.base_derive_necessary() {
            return;
        }

        if !data.has_boot0() {
            return;
        }

        // Extract encrypted keyblobs from BOOT0
        for i in 0..self.encrypted_keyblobs.len() {
            if self.encrypted_keyblobs[i] != [0u8; 0xB0] {
                continue;
            }
            self.encrypted_keyblobs[i] = data.get_encrypted_keyblob(i);
            self.write_key_to_file(
                KeyCategory::Console,
                &format!("encrypted_keyblob_{:02X}", i),
                &self.encrypted_keyblobs[i],
            );
        }

        // Set key sources from partition data
        self.set_key_wrapped_128(
            S128KeyType::Source,
            data.get_package2_key_source(),
            SourceKeyType::Package2 as u64,
            0,
        );
        self.set_key_wrapped_128(
            S128KeyType::Source,
            data.get_aes_kek_generation_source(),
            SourceKeyType::AESKekGeneration as u64,
            0,
        );
        self.set_key_wrapped_128(
            S128KeyType::Source,
            data.get_titlekek_source(),
            SourceKeyType::Titlekek as u64,
            0,
        );
        self.set_key_wrapped_128(
            S128KeyType::Source,
            data.get_master_key_source(),
            SourceKeyType::Master as u64,
            0,
        );
        self.set_key_wrapped_128(
            S128KeyType::Source,
            data.get_keyblob_mac_key_source(),
            SourceKeyType::KeyblobMAC as u64,
            0,
        );

        let max_keyblob =
            super::partition_data_manager::PartitionDataManager::max_keyblob_source_hash();
        for i in 0..max_keyblob as usize {
            self.set_key_wrapped_128(
                S128KeyType::Source,
                data.get_keyblob_key_source(i),
                SourceKeyType::Keyblob as u64,
                i as u64,
            );
        }

        if data.has_fuses() {
            self.set_key_wrapped_128(
                S128KeyType::SecureBoot,
                data.get_secure_boot_key(),
                0,
                0,
            );
        }

        self.derive_base();

        // Find latest master key
        let mut latest_master = [0u8; 16];
        for i in (0..=0x1Fi8).rev() {
            let key = self.get_key_128(S128KeyType::Master, i as u64, 0);
            if key != [0u8; 16] {
                latest_master = key;
                break;
            }
        }

        // Get TZ master keys
        let masters = data.get_tz_master_keys(latest_master);
        for i in 0..masters.len() {
            if masters[i] != [0u8; 16]
                && !self.has_key_128(S128KeyType::Master, i as u64, 0)
            {
                self.set_key_128(S128KeyType::Master, masters[i], i as u64, 0);
            }
        }

        self.derive_base();

        if !data.has_package2_default() {
            return;
        }

        // Decrypt Package2
        let mut package2_keys = [[0u8; 16]; 0x20];
        for i in 0..package2_keys.len() {
            if self.has_key_128(S128KeyType::Package2, i as u64, 0) {
                package2_keys[i] =
                    self.get_key_128(S128KeyType::Package2, i as u64, 0);
            }
        }
        data.decrypt_package2(
            &package2_keys,
            super::partition_data_manager::Package2Type::NormalMain,
        );

        // Extract key sources from decrypted Package2
        self.set_key_wrapped_128(
            S128KeyType::Source,
            data.get_key_area_key_application_source_default(),
            SourceKeyType::KeyAreaKey as u64,
            KeyAreaKeyType::Application as u64,
        );
        self.set_key_wrapped_128(
            S128KeyType::Source,
            data.get_key_area_key_ocean_source_default(),
            SourceKeyType::KeyAreaKey as u64,
            KeyAreaKeyType::Ocean as u64,
        );
        self.set_key_wrapped_128(
            S128KeyType::Source,
            data.get_key_area_key_system_source_default(),
            SourceKeyType::KeyAreaKey as u64,
            KeyAreaKeyType::System as u64,
        );
        self.set_key_wrapped_128(
            S128KeyType::Source,
            data.get_sd_kek_source_default(),
            SourceKeyType::SDKek as u64,
            0,
        );
        self.set_key_wrapped_256(
            S256KeyType::SDKeySource,
            data.get_sd_save_key_source_default(),
            SDKeyType::Save as u64,
            0,
        );
        self.set_key_wrapped_256(
            S256KeyType::SDKeySource,
            data.get_sd_nca_key_source_default(),
            SDKeyType::NCA as u64,
            0,
        );
        self.set_key_wrapped_128(
            S128KeyType::Source,
            data.get_header_kek_source_default(),
            SourceKeyType::HeaderKek as u64,
            0,
        );
        self.set_key_wrapped_256(
            S256KeyType::HeaderSource,
            data.get_header_key_source_default(),
            0,
            0,
        );
        self.set_key_wrapped_128(
            S128KeyType::Source,
            data.get_aes_key_generation_source_default(),
            SourceKeyType::AESKeyGeneration as u64,
            0,
        );

        self.derive_base();
    }

    pub fn get_common_tickets(&self) -> &BTreeMap<u128, Ticket> {
        &self.common_tickets
    }

    pub fn get_personalized_tickets(&self) -> &BTreeMap<u128, Ticket> {
        &self.personal_tickets
    }

    /// Add a ticket to the key manager.
    /// Corresponds to upstream `KeyManager::AddTicket`.
    pub fn add_ticket(&mut self, ticket: &Ticket) -> bool {
        if !ticket.is_valid() {
            log::warn!("Attempted to add invalid ticket.");
            return false;
        }

        let data = ticket.get_data().unwrap();
        let rid = &data.rights_id;

        // Convert rights_id bytes to u128 (two u64s, matching upstream memcpy)
        let rights_id_lo = u64::from_le_bytes([
            rid[0], rid[1], rid[2], rid[3], rid[4], rid[5], rid[6], rid[7],
        ]);
        let rights_id_hi = u64::from_le_bytes([
            rid[8], rid[9], rid[10], rid[11], rid[12], rid[13], rid[14], rid[15],
        ]);
        let rights_id_u128 = ((rights_id_hi as u128) << 64) | (rights_id_lo as u128);

        if data.key_type == TitleKeyType::Common {
            self.common_tickets
                .insert(rights_id_u128, ticket.clone());
        } else {
            self.personal_tickets
                .insert(rights_id_u128, ticket.clone());
        }

        if self.has_key_128(S128KeyType::Titlekey, rights_id_hi, rights_id_lo) {
            log::debug!(
                "Skipping parsing title key from ticket for known rights ID {:016X}{:016X}.",
                rights_id_hi,
                rights_id_lo
            );
            return true;
        }

        let key = match self.parse_ticket_title_key(ticket) {
            Some(k) => k,
            None => return false,
        };
        self.set_key_128(S128KeyType::Titlekey, key, rights_id_hi, rights_id_lo);
        true
    }

    /// Reload all keys from disk.
    /// Port of upstream `KeyManager::ReloadKeys`.
    ///
    /// Loads keys from the keys directory in this order (matching upstream):
    /// 1. prod.keys_autogenerated / dev.keys_autogenerated
    /// 2. prod.keys / dev.keys
    /// 3. title.keys_autogenerated
    /// 4. title.keys
    /// 5. console.keys_autogenerated
    /// 6. console.keys
    pub fn reload_keys(&mut self) {
        let keys_dir = resolve_keys_dir();

        if !keys_dir.exists() {
            // Attempt to create the directory, matching upstream behavior.
            if let Err(e) = std::fs::create_dir_all(&keys_dir) {
                log::error!("Failed to create the keys directory: {}", e);
            }
        }

        self.dev_mode = *common::settings::values().use_dev_keys.get_value();

        if self.dev_mode {
            self.load_from_file(&keys_dir.join("dev.keys_autogenerated"), false);
            self.load_from_file(&keys_dir.join("dev.keys"), false);
        } else {
            self.load_from_file(&keys_dir.join("prod.keys_autogenerated"), false);
            self.load_from_file(&keys_dir.join("prod.keys"), false);
        }

        self.load_from_file(&keys_dir.join("title.keys_autogenerated"), true);
        self.load_from_file(&keys_dir.join("title.keys"), true);
        self.load_from_file(&keys_dir.join("console.keys_autogenerated"), false);
        self.load_from_file(&keys_dir.join("console.keys"), false);
    }

    /// Load keys from a single key file.
    /// Port of upstream `KeyManager::LoadFromFile`.
    ///
    /// If `is_title_keys` is true, each line is treated as `rights_id_hex = title_key_hex`,
    /// stored as S128KeyType::Titlekey. Otherwise, the key name is matched against the
    /// static tables (s128_file_id, s256_file_id, KEYS_VARIABLE_LENGTH, kak_names, etc.).
    fn load_from_file(&mut self, file_path: &Path, is_title_keys: bool) {
        if !file_path.exists() {
            return;
        }

        let content = match std::fs::read_to_string(file_path) {
            Ok(c) => c,
            Err(e) => {
                log::error!("Failed to read key file {}: {}", file_path.display(), e);
                return;
            }
        };

        log::info!("Loading keys from {}", file_path.display());

        for line in content.lines() {
            // Split on '=' into exactly two parts
            let mut parts = line.splitn(2, '=');
            let name_part = match parts.next() {
                Some(s) => s,
                None => continue,
            };
            let value_part = match parts.next() {
                Some(s) => s,
                None => continue,
            };

            // Strip all spaces (upstream does erase+remove of ' ')
            let name: String = name_part.chars().filter(|c| *c != ' ').collect();
            let value: String = value_part.chars().filter(|c| *c != ' ').collect();

            if name.is_empty() || value.is_empty() {
                continue;
            }

            // Skip comments
            if name.starts_with('#') {
                continue;
            }

            if is_title_keys {
                // Parse rights_id (32 hex chars = 16 bytes) and title key (32 hex chars = 16 bytes)
                if name.len() < 32 || value.len() < 32 {
                    continue;
                }
                let rights_id_raw: [u8; 16] = hex_string_to_array(&name);
                // Interpret as u128 (two u64s in little-endian byte order, matching upstream memcpy)
                let rights_id_lo = u64::from_le_bytes([
                    rights_id_raw[0], rights_id_raw[1], rights_id_raw[2], rights_id_raw[3],
                    rights_id_raw[4], rights_id_raw[5], rights_id_raw[6], rights_id_raw[7],
                ]);
                let rights_id_hi = u64::from_le_bytes([
                    rights_id_raw[8], rights_id_raw[9], rights_id_raw[10], rights_id_raw[11],
                    rights_id_raw[12], rights_id_raw[13], rights_id_raw[14], rights_id_raw[15],
                ]);
                let key: Key128 = hex_string_to_array(&value);
                // Upstream stores: s128_keys[{Titlekey, rights_id[1], rights_id[0]}] = key
                // rights_id[0] is the first u64 (lo), rights_id[1] is the second u64 (hi)
                self.s128_keys.insert(
                    KeyIndex {
                        key_type: S128KeyType::Titlekey,
                        field1: rights_id_hi,
                        field2: rights_id_lo,
                    },
                    key,
                );
            } else {
                let lower_name = name.to_lowercase();

                // Try fixed 128-bit key names
                if let Some((kt, f1, f2)) = find_128_by_name(&lower_name) {
                    if value.len() >= 32 {
                        let key: Key128 = hex_string_to_array(&value);
                        self.s128_keys.insert(
                            KeyIndex { key_type: kt, field1: f1, field2: f2 },
                            key,
                        );
                    }
                    continue;
                }

                // Try fixed 256-bit key names
                if let Some((kt, f1, f2)) = find_256_by_name(&lower_name) {
                    if value.len() >= 64 {
                        let key: Key256 = hex_string_to_array(&value);
                        self.s256_keys.insert(
                            KeyIndex { key_type: kt, field1: f1, field2: f2 },
                            key,
                        );
                    }
                    continue;
                }

                // Try keyblob_XX (not keyblob_key_* or keyblob_mac_key_*)
                if lower_name.starts_with("keyblob_") && !lower_name.starts_with("keyblob_k") {
                    if valid_crypto_revision_string(&lower_name, 8, 2) && value.len() >= 0x90 * 2
                    {
                        let index =
                            u64::from_str_radix(&lower_name[8..10], 16).unwrap_or(0) as usize;
                        if index < 0x20 {
                            self.keyblobs[index] = hex_string_to_array(&value);
                        }
                    }
                    continue;
                }

                // Try encrypted_keyblob_XX
                if lower_name.starts_with("encrypted_keyblob_") {
                    if valid_crypto_revision_string(&lower_name, 18, 2)
                        && value.len() >= 0xB0 * 2
                    {
                        let index =
                            u64::from_str_radix(&lower_name[18..20], 16).unwrap_or(0) as usize;
                        if index < 0x20 {
                            self.encrypted_keyblobs[index] = hex_string_to_array(&value);
                        }
                    }
                    continue;
                }

                // Try eticket_extended_kek
                if lower_name.starts_with("eticket_extended_kek") {
                    if value.len() >= 576 * 2 {
                        self.eticket_extended_kek = hex_string_to_array(&value);
                    }
                    continue;
                }

                // Try eticket_rsa_keypair
                if lower_name.starts_with("eticket_rsa_keypair") {
                    if value.len() >= 528 * 2 {
                        let key_data: [u8; 528] = hex_string_to_array(&value);
                        let decryption_key = &key_data[..0x100];
                        let modulus = &key_data[0x100..0x200];
                        let exponent = &key_data[0x200..0x204];
                        self.eticket_rsa_keypair.decryption_key[..0x100]
                            .copy_from_slice(decryption_key);
                        self.eticket_rsa_keypair.modulus[..0x100].copy_from_slice(modulus);
                        self.eticket_rsa_keypair.exponent.copy_from_slice(exponent);
                    }
                    continue;
                }

                // Try variable-length key names (master_key_XX, titlekek_XX, etc.)
                let mut matched_variable = false;
                for &(key_type, sub_id, prefix) in KEYS_VARIABLE_LENGTH {
                    if !valid_crypto_revision_string(&lower_name, prefix.len(), 2) {
                        continue;
                    }
                    if lower_name.starts_with(prefix) {
                        let index = u64::from_str_radix(
                            &lower_name[prefix.len()..prefix.len() + 2],
                            16,
                        )
                        .unwrap_or(0);
                        if value.len() >= 32 {
                            let key: Key128 = hex_string_to_array(&value);
                            if sub_id == 0 {
                                self.s128_keys.insert(
                                    KeyIndex { key_type, field1: index, field2: 0 },
                                    key,
                                );
                            } else {
                                self.s128_keys.insert(
                                    KeyIndex { key_type, field1: sub_id, field2: index },
                                    key,
                                );
                            }
                        }
                        matched_variable = true;
                        break;
                    }
                }
                if matched_variable {
                    // Upstream does NOT continue here; it falls through to kak_names.
                    // But note: upstream puts kak_names in an else-branch of the
                    // KEYS_VARIABLE_LENGTH loop, so it always runs for non-variable keys.
                    // Actually re-reading upstream: the kak_names loop is OUTSIDE the else-if
                    // chain, in the final else block, so it only runs when no other match
                    // was found. But it is NOT inside the for-loop break. Let me re-read...
                    // Actually in upstream, both the KEYS_VARIABLE_LENGTH loop and the
                    // kak_names loop are in the final `else` block sequentially.
                    // After the KEYS_VARIABLE_LENGTH loop breaks, execution continues to
                    // the kak_names loop. So we should NOT skip the kak_names check.
                }

                // Try key_area_key_application_XX, key_area_key_ocean_XX, key_area_key_system_XX
                for &(prefix, kak_type) in KAK_NAMES {
                    if lower_name.starts_with(prefix) {
                        let suffix_start = prefix.len();
                        if valid_crypto_revision_string(&lower_name, suffix_start, 2)
                            && value.len() >= 32
                        {
                            let index = u64::from_str_radix(
                                &lower_name[suffix_start..suffix_start + 2],
                                16,
                            )
                            .unwrap_or(0);
                            let key: Key128 = hex_string_to_array(&value);
                            self.s128_keys.insert(
                                KeyIndex {
                                    key_type: S128KeyType::KeyArea,
                                    field1: index,
                                    field2: kak_type,
                                },
                                key,
                            );
                        }
                    }
                }
            }
        }
    }

    /// Parse the title key from a ticket.
    /// Corresponds to upstream `KeyManager::ParseTicketTitleKey`.
    pub fn parse_ticket_title_key(&self, ticket: &Ticket) -> Option<Key128> {
        if !ticket.is_valid() {
            log::warn!("Attempted to parse title key of invalid ticket.");
            return None;
        }

        let data = ticket.get_data().unwrap();

        if data.rights_id == [0u8; 16] {
            log::warn!("Attempted to parse title key of ticket with no rights ID.");
            return None;
        }

        if is_all_zero(&data.issuer) {
            log::warn!("Attempted to parse title key of ticket with invalid issuer.");
            return None;
        }

        if data.issuer[0] != b'R'
            || data.issuer[1] != b'o'
            || data.issuer[2] != b'o'
            || data.issuer[3] != b't'
        {
            log::warn!("Parsing ticket with non-standard certificate authority.");
        }

        if data.key_type == TitleKeyType::Common {
            return Some(data.title_key_common());
        }

        // Personalized ticket - requires RSA OAEP decryption
        if self.eticket_rsa_keypair == RsaKeyPair::default() {
            log::warn!(
                "Skipping personalized ticket title key parsing due to missing ETicket RSA key-pair."
            );
            return None;
        }

        // RSA modular exponentiation: M = S^D mod N
        // Using manual big integer arithmetic since we can't add crate deps.
        let d_bytes = &self.eticket_rsa_keypair.decryption_key;
        let n_bytes = &self.eticket_rsa_keypair.modulus;
        let s_bytes = &data.title_key_block;

        let d = bytes_to_bignum(d_bytes);
        let n = bytes_to_bignum(n_bytes);
        let s = bytes_to_bignum(s_bytes);

        let m = mod_exp(&s, &d, &n);
        let rsa_step = bignum_to_bytes_256(&m);

        let m_0 = rsa_step[0];
        let mut m_1 = [0u8; 0x20];
        m_1.copy_from_slice(&rsa_step[0x01..0x21]);
        let mut m_2 = [0u8; 0xDF];
        m_2.copy_from_slice(&rsa_step[0x21..0x100]);

        if m_0 != 0 {
            return None;
        }

        // MGF1 operations for OAEP unpadding
        let mgf1_m2 = mgf1::<0x20, 0xDF>(&m_2);
        for i in 0..0x20 {
            m_1[i] ^= mgf1_m2[i];
        }
        let mgf1_m1 = mgf1::<0xDF, 0x20>(&m_1);
        for i in 0..0xDF {
            m_2[i] ^= mgf1_m1[i];
        }

        let offset = find_ticket_offset(&m_2)?;
        assert!(offset > 0);

        let mut key_temp = [0u8; 16];
        key_temp.copy_from_slice(&m_2[offset as usize..offset as usize + 16]);
        Some(key_temp)
    }

    pub fn are_keys_loaded(&self) -> bool {
        !self.s128_keys.is_empty() && !self.s256_keys.is_empty()
    }

    fn set_key_wrapped_128(&mut self, id: S128KeyType, key: Key128, field1: u64, field2: u64) {
        if key == [0u8; 16] {
            return;
        }
        self.set_key_128(id, key, field1, field2);
    }

    fn set_key_wrapped_256(&mut self, id: S256KeyType, key: Key256, field1: u64, field2: u64) {
        if key == [0u8; 32] {
            return;
        }
        self.set_key_256(id, key, field1, field2);
    }

    /// Derive general purpose keys for a given crypto revision.
    /// Corresponds to upstream `KeyManager::DeriveGeneralPurposeKeys`.
    fn derive_general_purpose_keys(&mut self, crypto_revision: usize) {
        let kek_generation_source =
            self.get_key_128(S128KeyType::Source, SourceKeyType::AESKekGeneration as u64, 0);
        let key_generation_source =
            self.get_key_128(S128KeyType::Source, SourceKeyType::AESKeyGeneration as u64, 0);

        if self.has_key_128(S128KeyType::Master, crypto_revision as u64, 0) {
            for kak_type in [
                KeyAreaKeyType::Application,
                KeyAreaKeyType::Ocean,
                KeyAreaKeyType::System,
            ] {
                if self.has_key_128(
                    S128KeyType::Source,
                    SourceKeyType::KeyAreaKey as u64,
                    kak_type as u64,
                ) {
                    let source = self.get_key_128(
                        S128KeyType::Source,
                        SourceKeyType::KeyAreaKey as u64,
                        kak_type as u64,
                    );
                    let kek = generate_key_encryption_key(
                        source,
                        self.get_key_128(S128KeyType::Master, crypto_revision as u64, 0),
                        kek_generation_source,
                        key_generation_source,
                    );
                    self.set_key_128(
                        S128KeyType::KeyArea,
                        kek,
                        crypto_revision as u64,
                        kak_type as u64,
                    );
                }
            }

            let master_key =
                self.get_key_128(S128KeyType::Master, crypto_revision as u64, 0);
            let mut master_cipher =
                super::aes_util::AesCipher::new_128(master_key, super::aes_util::Mode::ECB);

            for key_type in [SourceKeyType::Titlekek, SourceKeyType::Package2] {
                if self.has_key_128(S128KeyType::Source, key_type as u64, 0) {
                    let source =
                        self.get_key_128(S128KeyType::Source, key_type as u64, 0);
                    let mut key = [0u8; 16];
                    master_cipher.transcode(&source, &mut key, super::aes_util::Op::Decrypt);
                    let target_type = if key_type == SourceKeyType::Titlekek {
                        S128KeyType::Titlekek
                    } else {
                        S128KeyType::Package2
                    };
                    self.set_key_128(target_type, key, crypto_revision as u64, 0);
                }
            }
        }
    }

    /// Derive the ETicket RSA key from the extended KEK.
    /// Corresponds to upstream `KeyManager::DeriveETicketRSAKey`.
    fn derive_e_ticket_rsa_key(&mut self) {
        if is_all_zero(&self.eticket_extended_kek)
            || !self.has_key_128(S128KeyType::ETicketRSAKek, 0, 0)
        {
            return;
        }

        let eticket_final = self.get_key_128(S128KeyType::ETicketRSAKek, 0, 0);

        let extended_iv = &self.eticket_extended_kek[..0x10];
        let mut extended_dec = [0u8; 0x230];
        let mut rsa_cipher =
            super::aes_util::AesCipher::new_128(eticket_final, super::aes_util::Mode::CTR);
        rsa_cipher.set_iv(extended_iv);
        rsa_cipher.transcode(
            &self.eticket_extended_kek[0x10..0x10 + 0x230],
            &mut extended_dec,
            super::aes_util::Op::Decrypt,
        );

        self.eticket_rsa_keypair.decryption_key[..0x100]
            .copy_from_slice(&extended_dec[..0x100]);
        self.eticket_rsa_keypair.modulus[..0x100]
            .copy_from_slice(&extended_dec[0x100..0x200]);
        self.eticket_rsa_keypair.exponent
            .copy_from_slice(&extended_dec[0x200..0x204]);
    }

    /// Write a key to the appropriate auto-generated key file.
    /// Corresponds to upstream `KeyManager::WriteKeyToFile`.
    fn write_key_to_file(&self, category: KeyCategory, keyname: &str, key: &[u8]) {
        let keys_dir = get_ruzu_path(RuzuPath::KeysDir);

        let filename = match category {
            KeyCategory::Standard => {
                if self.dev_mode {
                    "dev.keys_autogenerated"
                } else {
                    "prod.keys_autogenerated"
                }
            }
            KeyCategory::Title => "title.keys_autogenerated",
            KeyCategory::Console => "console.keys_autogenerated",
        };

        let path = keys_dir.join(filename);
        let add_info_text = !path.exists();

        let mut file = match std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&path)
        {
            Ok(f) => f,
            Err(_) => return,
        };

        use std::io::Write;
        if add_info_text {
            let _ = write!(
                file,
                "# This file is autogenerated by Ruzu\n\
                 # It serves to store keys that were automatically generated from the normal keys\n\
                 # If you are experiencing issues involving keys, it may help to delete this file\n"
            );
        }

        let hex = common::hex_util::hex_to_string(key, false);
        let _ = write!(file, "\n{} = {}", keyname, hex);
    }
}

impl Default for KeyManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---- Free functions matching upstream ----

/// Current crypto revision constant.
const CURRENT_CRYPTO_REVISION: u64 = 0x5;

/// Check if all bytes in a slice are zero.
fn is_all_zero(data: &[u8]) -> bool {
    data.iter().all(|&b| b == 0)
}

/// Generate a key encryption key. Port of upstream GenerateKeyEncryptionKey.
/// Three-step AES-ECB decrypt chain.
pub fn generate_key_encryption_key(
    source: Key128,
    master: Key128,
    kek_seed: Key128,
    key_seed: Key128,
) -> Key128 {
    use super::aes_util::{AesCipher, Mode, Op};

    let mut out = [0u8; 16];

    let mut cipher1 = AesCipher::new_128(master, Mode::ECB);
    cipher1.transcode(&kek_seed, &mut out, Op::Decrypt);

    let mut cipher2 = AesCipher::new_128(out, Mode::ECB);
    cipher2.transcode(&source, &mut out, Op::Decrypt);

    if key_seed != [0u8; 16] {
        let mut cipher3 = AesCipher::new_128(out, Mode::ECB);
        cipher3.transcode(&key_seed, &mut out, Op::Decrypt);
    }

    out
}

/// Derive a keyblob key. Port of upstream DeriveKeyblobKey.
/// Two AES-ECB decrypts: tsec decrypts source in-place, then sbk decrypts source in-place.
pub fn derive_keyblob_key(sbk: &Key128, tsec: &Key128, mut source: Key128) -> Key128 {
    use super::aes_util::{AesCipher, Mode, Op};

    let mut tsec_cipher = AesCipher::new_128(*tsec, Mode::ECB);
    let src = source;
    tsec_cipher.transcode(&src, &mut source, Op::Decrypt);

    let mut sbk_cipher = AesCipher::new_128(*sbk, Mode::ECB);
    let src = source;
    sbk_cipher.transcode(&src, &mut source, Op::Decrypt);

    source
}

/// Derive a keyblob MAC key. Port of upstream DeriveKeyblobMACKey.
/// Single AES-ECB decrypt.
pub fn derive_keyblob_mac_key(keyblob_key: &Key128, mac_source: &Key128) -> Key128 {
    use super::aes_util::{AesCipher, Mode, Op};

    let mut mac_cipher = AesCipher::new_128(*keyblob_key, Mode::ECB);
    let mut mac_key = [0u8; 16];
    mac_cipher.transcode(mac_source, &mut mac_key, Op::Decrypt);
    mac_key
}

/// Derive a master key from a keyblob. Port of upstream DeriveMasterKey.
/// Copy first 16 bytes from keyblob as master_root, AES-ECB decrypt master_source.
pub fn derive_master_key(keyblob: &[u8; 0x90], master_source: &Key128) -> Key128 {
    use super::aes_util::{AesCipher, Mode, Op};

    let mut master_root = [0u8; 16];
    master_root.copy_from_slice(&keyblob[..16]);

    let mut master_cipher = AesCipher::new_128(master_root, Mode::ECB);
    let mut master = [0u8; 16];
    master_cipher.transcode(master_source, &mut master, Op::Decrypt);
    master
}

/// Decrypt a keyblob. Port of upstream DecryptKeyblob.
/// AES-CTR decrypt: IV is encrypted_keyblob[0x10..0x20], decrypt encrypted_keyblob[0x20..0x20+0x90].
pub fn decrypt_keyblob(encrypted_keyblob: &[u8; 0xB0], key: &Key128) -> [u8; 0x90] {
    use super::aes_util::{AesCipher, Mode, Op};

    let mut keyblob = [0u8; 0x90];
    let mut cipher = AesCipher::new_128(*key, Mode::CTR);
    cipher.set_iv(&encrypted_keyblob[0x10..0x20]);
    cipher.transcode(&encrypted_keyblob[0x20..0x20 + 0x90], &mut keyblob, Op::Decrypt);
    keyblob
}

/// Calculate AES-128-CMAC. Port of upstream CalculateCMAC.
/// Manual implementation using AES-ECB (RFC 4493).
fn calculate_cmac(source: &[u8], key: &Key128) -> [u8; 16] {
    use super::aes_util::{AesCipher, Mode, Op};

    // Step 1: Generate subkeys
    let mut cipher = AesCipher::new_128(*key, Mode::ECB);
    let zero_block = [0u8; 16];
    let mut l = [0u8; 16];
    cipher.transcode(&zero_block, &mut l, Op::Encrypt);

    // Generate K1
    let mut k1 = [0u8; 16];
    let msb = l[0] & 0x80;
    for i in 0..15 {
        k1[i] = (l[i] << 1) | (l[i + 1] >> 7);
    }
    k1[15] = l[15] << 1;
    if msb != 0 {
        k1[15] ^= 0x87;
    }

    // Generate K2
    let mut k2 = [0u8; 16];
    let msb = k1[0] & 0x80;
    for i in 0..15 {
        k2[i] = (k1[i] << 1) | (k1[i + 1] >> 7);
    }
    k2[15] = k1[15] << 1;
    if msb != 0 {
        k2[15] ^= 0x87;
    }

    // Step 2: Process message
    let n = if source.is_empty() {
        1
    } else {
        (source.len() + 15) / 16
    };
    let flag = !source.is_empty() && (source.len() % 16 == 0);

    let mut m_last = [0u8; 16];
    if flag {
        // Complete block - XOR with K1
        let last_start = (n - 1) * 16;
        for i in 0..16 {
            m_last[i] = source[last_start + i] ^ k1[i];
        }
    } else {
        // Incomplete block - pad and XOR with K2
        let last_start = (n - 1) * 16;
        let remaining = source.len() - last_start;
        let mut padded = [0u8; 16];
        padded[..remaining].copy_from_slice(&source[last_start..]);
        padded[remaining] = 0x80;
        for i in 0..16 {
            m_last[i] = padded[i] ^ k2[i];
        }
    }

    // Step 3: CBC-MAC
    let mut x = [0u8; 16];
    let mut y = [0u8; 16];

    for i in 0..n - 1 {
        let block_start = i * 16;
        for j in 0..16 {
            y[j] = x[j] ^ source[block_start + j];
        }
        let src = y;
        cipher.transcode(&src, &mut x, Op::Encrypt);
    }

    // Last block
    for j in 0..16 {
        y[j] = x[j] ^ m_last[j];
    }
    let mut t = [0u8; 16];
    cipher.transcode(&y, &mut t, Op::Encrypt);

    t
}

/// Attempt to derive the SD seed. Port of upstream DeriveSDSeed.
/// Reads system save 8000000000000043 and Nintendo/Contents/private from NAND/SDMC.
pub fn derive_sd_seed() -> Option<Key128> {
    let system_save_43_path =
        get_ruzu_path(RuzuPath::NANDDir).join("system/save/8000000000000043");

    let save_43_data = std::fs::read(&system_save_43_path).ok()?;

    let sd_private_path =
        get_ruzu_path(RuzuPath::SDMCDir).join("Nintendo/Contents/private");

    let sd_private_data = std::fs::read(&sd_private_path).ok()?;

    if sd_private_data.len() < 0x10 {
        return None;
    }

    let mut private_seed = [0u8; 0x10];
    private_seed.copy_from_slice(&sd_private_data[..0x10]);

    // Search for private_seed in save_43
    let mut offset = None;
    for i in 0..save_43_data.len().saturating_sub(0x10) {
        if save_43_data[i..i + 0x10] == private_seed {
            offset = Some(i);
            break;
        }
    }

    let offset = offset?;
    let seed_offset = offset + 0x10;
    if seed_offset + 0x10 > save_43_data.len() {
        return None;
    }

    let mut seed = [0u8; 16];
    seed.copy_from_slice(&save_43_data[seed_offset..seed_offset + 0x10]);
    Some(seed)
}

/// Derive SD keys. Port of upstream DeriveSDKeys.
pub fn derive_sd_keys(
    sd_keys: &mut [Key256; 2],
    keys: &mut KeyManager,
) -> Result<(), &'static str> {
    if !keys.has_key_128(S128KeyType::Source, SourceKeyType::SDKek as u64, 0) {
        return Err("ErrorMissingSDKEKSource");
    }
    if !keys.has_key_128(S128KeyType::Source, SourceKeyType::AESKekGeneration as u64, 0) {
        return Err("ErrorMissingAESKEKGenerationSource");
    }
    if !keys.has_key_128(S128KeyType::Source, SourceKeyType::AESKeyGeneration as u64, 0) {
        return Err("ErrorMissingAESKeyGenerationSource");
    }

    let sd_kek_source =
        keys.get_key_128(S128KeyType::Source, SourceKeyType::SDKek as u64, 0);
    let aes_kek_gen =
        keys.get_key_128(S128KeyType::Source, SourceKeyType::AESKekGeneration as u64, 0);
    let aes_key_gen =
        keys.get_key_128(S128KeyType::Source, SourceKeyType::AESKeyGeneration as u64, 0);
    let master_00 = keys.get_key_128(S128KeyType::Master, 0, 0);
    let sd_kek =
        generate_key_encryption_key(sd_kek_source, master_00, aes_kek_gen, aes_key_gen);
    keys.set_key_128(S128KeyType::SDKek, sd_kek, 0, 0);

    if !keys.has_key_128(S128KeyType::SDSeed, 0, 0) {
        return Err("ErrorMissingSDSeed");
    }
    let sd_seed = keys.get_key_128(S128KeyType::SDSeed, 0, 0);

    if !keys.has_key_256(S256KeyType::SDKeySource, SDKeyType::Save as u64, 0) {
        return Err("ErrorMissingSDSaveKeySource");
    }
    if !keys.has_key_256(S256KeyType::SDKeySource, SDKeyType::NCA as u64, 0) {
        return Err("ErrorMissingSDNCAKeySource");
    }

    let mut sd_key_sources = [
        keys.get_key_256(S256KeyType::SDKeySource, SDKeyType::Save as u64, 0),
        keys.get_key_256(S256KeyType::SDKeySource, SDKeyType::NCA as u64, 0),
    ];

    // XOR sources with seed
    for source in sd_key_sources.iter_mut() {
        for i in 0..source.len() {
            source[i] ^= sd_seed[i & 0xF];
        }
    }

    // Decrypt with sd_kek
    let mut cipher = super::aes_util::AesCipher::new_128(sd_kek, super::aes_util::Mode::ECB);
    for (i, source) in sd_key_sources.iter().enumerate() {
        cipher.transcode(source, &mut sd_keys[i], super::aes_util::Op::Decrypt);
    }

    keys.set_key_256(
        S256KeyType::SDKey,
        sd_keys[0],
        SDKeyType::Save as u64,
        0,
    );
    keys.set_key_256(
        S256KeyType::SDKey,
        sd_keys[1],
        SDKeyType::NCA as u64,
        0,
    );

    Ok(())
}

/// Extract tickets from a raw ticket save file.
/// Corresponds to upstream `GetTicketblob`.
fn get_ticketblob(buffer: &[u8]) -> Vec<Ticket> {
    let mut out = Vec::new();
    let mut offset = 0;
    while offset + 4 < buffer.len() {
        // Look for RSA-2048 SHA256 signature type: 0x00010004 (little-endian: 04 00 01 00)
        if buffer[offset] == 0x04
            && buffer[offset + 1] == 0x00
            && buffer[offset + 2] == 0x01
            && buffer[offset + 3] == 0x00
        {
            // RSA2048 ticket size: 4 + 0x100 + 0x3C + sizeof(TicketData)
            let ticket_size = 4 + 0x100 + 0x3C + std::mem::size_of::<TicketData>();
            if offset + ticket_size <= buffer.len() {
                let ticket = Ticket::read_from_bytes(&buffer[offset..offset + ticket_size]);
                offset += ticket_size;
                if ticket.is_valid() {
                    out.push(ticket);
                }
            } else {
                break;
            }
        } else {
            offset += 1;
        }
    }
    out
}

/// MGF1 (Mask Generation Function 1) using SHA-256.
/// Corresponds to upstream `MGF1<target_size, in_size>`.
fn mgf1<const TARGET_SIZE: usize, const IN_SIZE: usize>(seed: &[u8; IN_SIZE]) -> [u8; TARGET_SIZE] {
    let mut seed_exp = vec![0u8; IN_SIZE + 4];
    seed_exp[..IN_SIZE].copy_from_slice(seed);

    let mut out_vec = Vec::new();
    let mut i = 0u32;
    while out_vec.len() < TARGET_SIZE {
        seed_exp[IN_SIZE + 3] = i as u8;
        let hash = super::sha_util::sha256(&seed_exp);
        out_vec.extend_from_slice(&hash);
        i += 1;
    }

    let mut target = [0u8; TARGET_SIZE];
    target.copy_from_slice(&out_vec[..TARGET_SIZE]);
    target
}

/// Find the ticket offset in OAEP-decoded data.
/// Corresponds to upstream `FindTicketOffset`.
fn find_ticket_offset(data: &[u8]) -> Option<u64> {
    for i in 0x20..data.len().saturating_sub(0x10) {
        if data[i] == 0x01 {
            return Some((i + 1) as u64);
        } else if data[i] != 0x00 {
            return None;
        }
    }
    None
}

// ---- Simple big integer operations for RSA ----
// These are needed for personalized ticket parsing.
// Using a minimal manual implementation to avoid adding crate dependencies.

/// A simple big integer represented as a vector of u32 limbs (little-endian).
type BigNum = Vec<u32>;

/// Convert big-endian bytes to BigNum (little-endian u32 limbs).
fn bytes_to_bignum(bytes: &[u8]) -> BigNum {
    // Pad to multiple of 4
    let padded_len = (bytes.len() + 3) / 4 * 4;
    let mut padded = vec![0u8; padded_len];
    padded[padded_len - bytes.len()..].copy_from_slice(bytes);

    let num_limbs = padded_len / 4;
    let mut result = Vec::with_capacity(num_limbs);
    for i in (0..num_limbs).rev() {
        let offset = i * 4;
        let limb = u32::from_be_bytes([
            padded[offset],
            padded[offset + 1],
            padded[offset + 2],
            padded[offset + 3],
        ]);
        result.push(limb);
    }

    // Remove leading zeros
    while result.len() > 1 && *result.last().unwrap() == 0 {
        result.pop();
    }
    result
}

/// Convert BigNum to 256-byte big-endian output.
fn bignum_to_bytes_256(n: &BigNum) -> [u8; 0x100] {
    let mut result = [0u8; 0x100];
    for (i, &limb) in n.iter().enumerate() {
        let bytes = limb.to_be_bytes();
        let offset = 0x100 - (i + 1) * 4;
        if offset < 0x100 {
            let end = (offset + 4).min(0x100);
            let start_byte = 4 - (end - offset);
            result[offset..end].copy_from_slice(&bytes[start_byte..]);
        }
    }
    result
}

/// Compare two BigNums.
fn bignum_cmp(a: &BigNum, b: &BigNum) -> std::cmp::Ordering {
    if a.len() != b.len() {
        return a.len().cmp(&b.len());
    }
    for i in (0..a.len()).rev() {
        if a[i] != b[i] {
            return a[i].cmp(&b[i]);
        }
    }
    std::cmp::Ordering::Equal
}

/// Subtract b from a (a >= b assumed). Returns a - b.
fn bignum_sub(a: &BigNum, b: &BigNum) -> BigNum {
    let mut result = Vec::with_capacity(a.len());
    let mut borrow: u64 = 0;
    for i in 0..a.len() {
        let ai = a[i] as u64;
        let bi = if i < b.len() { b[i] as u64 } else { 0 };
        let diff = ai.wrapping_sub(bi).wrapping_sub(borrow);
        result.push(diff as u32);
        borrow = if ai < bi + borrow { 1 } else { 0 };
    }
    while result.len() > 1 && *result.last().unwrap() == 0 {
        result.pop();
    }
    result
}

/// Multiply BigNum by a single u32 and add.
#[allow(dead_code)]
fn bignum_mul_add(a: &BigNum, b: u32, shift: usize) -> BigNum {
    let mut result = vec![0u32; shift];
    let mut carry: u64 = 0;
    for &limb in a.iter() {
        let prod = limb as u64 * b as u64 + carry;
        result.push(prod as u32);
        carry = prod >> 32;
    }
    if carry > 0 {
        result.push(carry as u32);
    }
    while result.len() > 1 && *result.last().unwrap() == 0 {
        result.pop();
    }
    result
}

/// BigNum addition.
#[allow(dead_code)]
fn bignum_add(a: &BigNum, b: &BigNum) -> BigNum {
    let len = a.len().max(b.len());
    let mut result = Vec::with_capacity(len + 1);
    let mut carry: u64 = 0;
    for i in 0..len {
        let ai = if i < a.len() { a[i] as u64 } else { 0 };
        let bi = if i < b.len() { b[i] as u64 } else { 0 };
        let sum = ai + bi + carry;
        result.push(sum as u32);
        carry = sum >> 32;
    }
    if carry > 0 {
        result.push(carry as u32);
    }
    result
}

/// BigNum modular reduction: a mod m.
fn bignum_mod(a: &BigNum, m: &BigNum) -> BigNum {
    if bignum_cmp(a, m) == std::cmp::Ordering::Less {
        return a.clone();
    }

    // Simple schoolbook division
    let mut remainder = vec![0u32; 1];
    let a_bits = a.len() * 32;

    for bit_idx in (0..a_bits).rev() {
        // Shift remainder left by 1 bit
        let mut carry = 0u32;
        for limb in remainder.iter_mut() {
            let new_carry = *limb >> 31;
            *limb = (*limb << 1) | carry;
            carry = new_carry;
        }
        if carry > 0 {
            remainder.push(carry);
        }

        // Set LSB from a
        let limb_idx = bit_idx / 32;
        let bit_pos = bit_idx % 32;
        if limb_idx < a.len() && (a[limb_idx] >> bit_pos) & 1 == 1 {
            remainder[0] |= 1;
        }

        // If remainder >= m, subtract
        if bignum_cmp(&remainder, m) != std::cmp::Ordering::Less {
            remainder = bignum_sub(&remainder, m);
        }
    }

    while remainder.len() > 1 && *remainder.last().unwrap() == 0 {
        remainder.pop();
    }
    remainder
}

/// Modular exponentiation: base^exp mod modulus.
/// Uses square-and-multiply algorithm.
fn mod_exp(base: &BigNum, exp: &BigNum, modulus: &BigNum) -> BigNum {
    if modulus.len() == 1 && modulus[0] == 1 {
        return vec![0];
    }

    let mut result = vec![1u32];
    let mut base = bignum_mod(base, modulus);

    let exp_bits = exp.len() * 32;

    for bit_idx in 0..exp_bits {
        let limb_idx = bit_idx / 32;
        let bit_pos = bit_idx % 32;

        if limb_idx < exp.len() && (exp[limb_idx] >> bit_pos) & 1 == 1 {
            // result = (result * base) mod modulus
            result = bignum_mul_mod(&result, &base, modulus);
        }
        // base = (base * base) mod modulus
        base = bignum_mul_mod(&base, &base, modulus);
    }

    result
}

/// Multiply two BigNums and reduce mod m.
fn bignum_mul_mod(a: &BigNum, b: &BigNum, m: &BigNum) -> BigNum {
    // Simple schoolbook multiplication
    let mut result = vec![0u32; a.len() + b.len() + 1];
    for i in 0..a.len() {
        let mut carry: u64 = 0;
        for j in 0..b.len() {
            let prod = a[i] as u64 * b[j] as u64 + result[i + j] as u64 + carry;
            result[i + j] = prod as u32;
            carry = prod >> 32;
        }
        result[i + b.len()] += carry as u32;
    }
    while result.len() > 1 && *result.last().unwrap() == 0 {
        result.pop();
    }
    bignum_mod(&result, m)
}
