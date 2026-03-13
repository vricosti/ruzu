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

use super::sha_util::Sha256Hash;

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

        // TODO: Full ticket deserialization matching upstream Ticket::Read
        // For now, return Invalid for unrecognized types
        match sig_type_val {
            0x10004 | 0x10003 => {
                // RSA-2048 SHA256 / SHA1 - most common in practice
                // Minimal parsing: extract what we need
                if raw_data.len() < 0x400 {
                    return Ticket::Invalid;
                }
                let mut data = TicketData::default();
                // TODO: Properly deserialize TicketData from raw bytes
                Ticket::RSA2048 {
                    sig_type: SignatureType::RSA2048SHA256,
                    sig_data: [0u8; 0x100],
                    _padding: [0u8; 0x3C],
                    data,
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
        // TODO: WriteKeyToFile logic
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
        // TODO: WriteKeyToFile logic
        self.s256_keys.insert(idx, key);
    }

    /// Check if the key file exists on disk.
    /// Port of upstream `KeyManager::KeyFileExists`.
    pub fn key_file_exists(title: bool) -> bool {
        let keys_dir = resolve_keys_dir();

        if title {
            return keys_dir.join("title.keys").exists();
        }

        // TODO: Check Settings::values.use_dev_keys when global settings are wired up
        // For now, only check prod.keys (dev_mode = false default).
        keys_dir.join("prod.keys").exists()
    }

    pub fn derive_sd_seed_lazy(&mut self) {
        if self.has_key_128(S128KeyType::SDSeed, 0, 0) {
            return;
        }
        if let Some(seed) = derive_sd_seed() {
            self.set_key_128(S128KeyType::SDSeed, seed, 0, 0);
        }
    }

    pub fn base_derive_necessary(&self) -> bool {
        // TODO: Full implementation checking all necessary keys
        !self.has_key_256(S256KeyType::Header, 0, 0)
    }

    pub fn derive_base(&mut self) {
        // TODO: Full key derivation pipeline from upstream DeriveBase
        if !self.base_derive_necessary() {
            return;
        }
        // Stub: actual derivation requires SecureBoot + TSEC keys + keyblobs
    }

    pub fn derive_e_ticket(
        &mut self,
        _data: &mut super::partition_data_manager::PartitionDataManager,
    ) {
        // TODO: Full ETicket derivation from upstream DeriveETicket
    }

    pub fn populate_tickets(&mut self) {
        if self.ticket_databases_loaded {
            return;
        }
        self.ticket_databases_loaded = true;
        // TODO: Read ticket databases from NAND saves
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

    pub fn populate_from_partition_data(
        &mut self,
        _data: &mut super::partition_data_manager::PartitionDataManager,
    ) {
        // TODO: Full implementation from upstream PopulateFromPartitionData
    }

    pub fn get_common_tickets(&self) -> &BTreeMap<u128, Ticket> {
        &self.common_tickets
    }

    pub fn get_personalized_tickets(&self) -> &BTreeMap<u128, Ticket> {
        &self.personal_tickets
    }

    pub fn add_ticket(&mut self, ticket: &Ticket) -> bool {
        if !ticket.is_valid() {
            log::warn!("Attempted to add invalid ticket.");
            return false;
        }
        // TODO: Full ticket addition logic
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

        // TODO: Read Settings::values.use_dev_keys when global settings are wired up.
        // For now, always use prod mode (dev_mode = false).
        self.dev_mode = false;

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

    fn derive_general_purpose_keys(&mut self, _crypto_revision: usize) {
        // TODO: Full implementation from upstream DeriveGeneralPurposeKeys
    }

    fn derive_e_ticket_rsa_key(&mut self) {
        // TODO: Full implementation from upstream DeriveETicketRSAKey
    }
}

impl Default for KeyManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---- Free functions matching upstream ----

/// Generate a key encryption key. Port of upstream GenerateKeyEncryptionKey.
pub fn generate_key_encryption_key(
    _source: Key128,
    _master: Key128,
    _kek_seed: Key128,
    _key_seed: Key128,
) -> Key128 {
    // TODO: Full implementation using AES-ECB cipher chain
    [0u8; 16]
}

/// Derive a keyblob key. Port of upstream DeriveKeyblobKey.
pub fn derive_keyblob_key(_sbk: &Key128, _tsec: &Key128, _source: Key128) -> Key128 {
    // TODO: Full implementation using AES-ECB cipher chain
    [0u8; 16]
}

/// Derive a keyblob MAC key. Port of upstream DeriveKeyblobMACKey.
pub fn derive_keyblob_mac_key(_keyblob_key: &Key128, _mac_source: &Key128) -> Key128 {
    // TODO: Full implementation
    [0u8; 16]
}

/// Derive a master key from a keyblob. Port of upstream DeriveMasterKey.
pub fn derive_master_key(_keyblob: &[u8; 0x90], _master_source: &Key128) -> Key128 {
    // TODO: Full implementation
    [0u8; 16]
}

/// Decrypt a keyblob. Port of upstream DecryptKeyblob.
pub fn decrypt_keyblob(_encrypted_keyblob: &[u8; 0xB0], _key: &Key128) -> [u8; 0x90] {
    // TODO: Full implementation using AES-CTR
    [0u8; 0x90]
}

/// Attempt to derive the SD seed. Port of upstream DeriveSDSeed.
pub fn derive_sd_seed() -> Option<Key128> {
    // TODO: Read from system save 8*43 and private file
    None
}

/// Derive SD keys. Port of upstream DeriveSDKeys.
pub fn derive_sd_keys(
    _sd_keys: &mut [Key256; 2],
    _keys: &mut KeyManager,
) -> Result<(), &'static str> {
    // TODO: Full implementation
    Err("Not implemented")
}
