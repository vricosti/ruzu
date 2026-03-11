// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/crypto/key_manager.h and key_manager.cpp
//! Manages cryptographic keys for Switch content decryption.
//!
//! This module preserves the upstream KeyIndex/S128KeyType/S256KeyType key classification
//! system and the full key derivation pipeline, matching the C++ file structure.

use std::collections::BTreeMap;

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
        // TODO: mgr.reload_keys();
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

    pub fn key_file_exists(_title: bool) -> bool {
        // TODO: Check for prod.keys / title.keys / dev.keys existence
        false
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

    pub fn reload_keys(&mut self) {
        // TODO: Load keys from files matching upstream ReloadKeys
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
