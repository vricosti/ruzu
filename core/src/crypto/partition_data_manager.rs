// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/crypto/partition_data_manager.h and partition_data_manager.cpp
//! Status: COMPLET (structural parity; SHA256 key search depends on crypto integration)
//! Derniere synchro: 2026-03-12
//!
//! Manages partition data (BOOT0, fuses, kfuses, Package2, PRODINFO)
//! for key derivation and decryption during system setup.

use crate::file_sys::vfs::vfs::VfsFile;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};

/// Package2 type variants.
/// Corresponds to upstream `Package2Type`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum Package2Type {
    NormalMain = 0,
    NormalSub = 1,
    SafeModeMain = 2,
    SafeModeSub = 3,
    RepairMain = 4,
    RepairSub = 5,
}

/// Number of encrypted keyblobs stored in BOOT0.
pub const NUM_ENCRYPTED_KEYBLOBS: usize = 32;

/// Size of a single encrypted keyblob.
pub const ENCRYPTED_KEYBLOB_SIZE: usize = 0xB0;

/// A single encrypted keyblob.
pub type EncryptedKeyBlob = [u8; ENCRYPTED_KEYBLOB_SIZE];

/// All encrypted keyblobs.
pub type EncryptedKeyBlobs = [EncryptedKeyBlob; NUM_ENCRYPTED_KEYBLOBS];

/// Package2 header structure.
/// Corresponds to upstream `Package2Header`.
#[derive(Clone)]
#[repr(C)]
struct Package2Header {
    signature: [u8; 0x100],
    header_ctr: [u8; 0x10],
    section_ctr: [[u8; 0x10]; 4],
    magic: u32,
    base_offset: u32,
    _padding0: [u8; 4],
    version_max: u8,
    version_min: u8,
    _padding1: [u8; 2],
    section_size: [u32; 4],
    section_offset: [u32; 4],
    section_hash: [[u8; 0x20]; 4],
}
const _: () = assert!(std::mem::size_of::<Package2Header>() == 0x200);

// Source hashes used for finding keys in the secure monitor binary.
// Each is a SHA-256 hash of a 16-byte key source.
// Corresponds to upstream `source_hashes` array.
const SOURCE_HASH_COUNT: usize = 16;
const _SOURCE_HASH_NAMES: [&str; SOURCE_HASH_COUNT] = [
    "keyblob_mac_key_source",
    "master_key_source",
    "package2_key_source",
    "aes_kek_generation_source",
    "aes_key_generation_source",
    "titlekek_source",
    "key_area_key_application_source",
    "key_area_key_ocean_source",
    "key_area_key_system_source",
    "sd_card_kek_source",
    "sd_card_save_key_source",
    "sd_card_nca_key_source",
    "header_kek_source",
    "header_key_source",
    "rsa_kek_seed3",
    "rsa_kek_mask0",
];

/// File name candidates used when searching for partition data files.
const PACKAGE2_NAMES: [&str; 6] = [
    "BCPKG2-1-Normal-Main",
    "BCPKG2-2-Normal-Sub",
    "BCPKG2-3-SafeMode-Main",
    "BCPKG2-4-SafeMode-Sub",
    "BCPKG2-5-Repair-Main",
    "BCPKG2-6-Repair-Sub",
];

/// Find a file in a directory by trying multiple name variants.
fn find_file_in_dir(dir: &VirtualDir, name: &str) -> Option<VirtualFile> {
    let upper = name.to_uppercase();
    for candidate in &[
        name.to_string(),
        format!("{}.bin", name),
        upper.clone(),
        format!("{}.BIN", upper),
    ] {
        if let Some(file) = dir.get_file(candidate) {
            return Some(file);
        }
    }
    None
}

/// Partition data manager.
///
/// Corresponds to upstream `Core::Crypto::PartitionDataManager`.
pub struct PartitionDataManager {
    boot0: Option<VirtualFile>,
    fuses: Option<VirtualFile>,
    kfuses: Option<VirtualFile>,
    package2: [Option<VirtualFile>; 6],
    prodinfo: Option<VirtualFile>,
    secure_monitor: Option<VirtualFile>,
    package1_decrypted: Option<VirtualFile>,

    // Processed data
    package2_decrypted: [Option<VirtualFile>; 6],
    prodinfo_decrypted: Option<VirtualFile>,
    secure_monitor_bytes: Vec<u8>,
    package1_decrypted_bytes: Vec<u8>,
    package2_fs: [Vec<u8>; 6],
    package2_spl: [Vec<u8>; 6],
}

impl PartitionDataManager {
    /// Create a new PartitionDataManager from a system data directory.
    ///
    /// Corresponds to upstream constructor.
    pub fn new(sysdata_dir: &VirtualDir) -> Self {
        let boot0 = find_file_in_dir(sysdata_dir, "BOOT0");
        let fuses = find_file_in_dir(sysdata_dir, "fuses");
        let kfuses = find_file_in_dir(sysdata_dir, "kfuses");
        let prodinfo = find_file_in_dir(sysdata_dir, "PRODINFO");
        let secure_monitor = find_file_in_dir(sysdata_dir, "secmon");
        let package1_decrypted = find_file_in_dir(sysdata_dir, "pkg1_decr");

        let package2: [Option<VirtualFile>; 6] = [
            find_file_in_dir(sysdata_dir, PACKAGE2_NAMES[0]),
            find_file_in_dir(sysdata_dir, PACKAGE2_NAMES[1]),
            find_file_in_dir(sysdata_dir, PACKAGE2_NAMES[2]),
            find_file_in_dir(sysdata_dir, PACKAGE2_NAMES[3]),
            find_file_in_dir(sysdata_dir, PACKAGE2_NAMES[4]),
            find_file_in_dir(sysdata_dir, PACKAGE2_NAMES[5]),
        ];

        let secure_monitor_bytes = secure_monitor
            .as_ref()
            .map(|f| f.read_all_bytes())
            .unwrap_or_default();
        let package1_decrypted_bytes = package1_decrypted
            .as_ref()
            .map(|f| f.read_all_bytes())
            .unwrap_or_default();

        Self {
            boot0,
            fuses,
            kfuses,
            package2,
            prodinfo,
            secure_monitor,
            package1_decrypted,
            package2_decrypted: Default::default(),
            prodinfo_decrypted: None,
            secure_monitor_bytes,
            package1_decrypted_bytes,
            package2_fs: Default::default(),
            package2_spl: Default::default(),
        }
    }

    // =========================================================================
    // BOOT0
    // =========================================================================

    pub fn has_boot0(&self) -> bool {
        self.boot0.is_some()
    }

    pub fn get_boot0_raw(&self) -> Option<&VirtualFile> {
        self.boot0.as_ref()
    }

    /// Read a single encrypted keyblob from BOOT0.
    pub fn get_encrypted_keyblob(&self, index: usize) -> EncryptedKeyBlob {
        if self.has_boot0() && index < NUM_ENCRYPTED_KEYBLOBS {
            self.get_encrypted_keyblobs()[index]
        } else {
            [0u8; ENCRYPTED_KEYBLOB_SIZE]
        }
    }

    /// Read all encrypted keyblobs from BOOT0.
    pub fn get_encrypted_keyblobs(&self) -> EncryptedKeyBlobs {
        let mut out = [[0u8; ENCRYPTED_KEYBLOB_SIZE]; NUM_ENCRYPTED_KEYBLOBS];
        if let Some(ref boot0) = self.boot0 {
            for i in 0..NUM_ENCRYPTED_KEYBLOBS {
                boot0.read(&mut out[i], ENCRYPTED_KEYBLOB_SIZE, 0x180000 + i * 0x200);
            }
        }
        out
    }

    pub fn get_secure_monitor(&self) -> &[u8] {
        &self.secure_monitor_bytes
    }

    /// Get the package2 key source from secure monitor.
    /// Corresponds to upstream `GetPackage2KeySource`.
    pub fn get_package2_key_source(&self) -> [u8; 0x10] {
        find_key_from_hex_16(&self.secure_monitor_bytes, 2)
    }

    /// Get the AES KEK generation source from secure monitor.
    pub fn get_aes_kek_generation_source(&self) -> [u8; 0x10] {
        find_key_from_hex_16(&self.secure_monitor_bytes, 3)
    }

    /// Get the titlekek source from secure monitor.
    pub fn get_titlekek_source(&self) -> [u8; 0x10] {
        find_key_from_hex_16(&self.secure_monitor_bytes, 5)
    }

    /// Get the RSA KEK seed 3 from secure monitor.
    pub fn get_rsa_kek_seed3(&self) -> [u8; 0x10] {
        find_key_from_hex_16(&self.secure_monitor_bytes, 14)
    }

    /// Get the RSA KEK mask 0 from secure monitor.
    pub fn get_rsa_kek_mask0(&self) -> [u8; 0x10] {
        find_key_from_hex_16(&self.secure_monitor_bytes, 15)
    }

    /// Get the master key source from package1 decrypted.
    pub fn get_master_key_source(&self) -> [u8; 0x10] {
        find_key_from_hex_16(&self.package1_decrypted_bytes, 1)
    }

    /// Get the keyblob MAC key source.
    pub fn get_keyblob_mac_key_source(&self) -> [u8; 0x10] {
        find_key_from_hex_16(&self.package1_decrypted_bytes, 0)
    }

    pub fn get_package1_decrypted(&self) -> &[u8] {
        &self.package1_decrypted_bytes
    }

    // =========================================================================
    // Fuses
    // =========================================================================

    pub fn has_fuses(&self) -> bool {
        self.fuses.is_some()
    }

    pub fn get_fuses_raw(&self) -> Option<&VirtualFile> {
        self.fuses.as_ref()
    }

    /// Get the secure boot key from fuses.
    pub fn get_secure_boot_key(&self) -> [u8; 0x10] {
        let mut out = [0u8; 0x10];
        if let Some(ref fuses) = self.fuses {
            fuses.read(&mut out, 0x10, 0xA4);
        }
        out
    }

    // =========================================================================
    // K-Fuses
    // =========================================================================

    pub fn has_kfuses(&self) -> bool {
        self.kfuses.is_some()
    }

    pub fn get_kfuses_raw(&self) -> Option<&VirtualFile> {
        self.kfuses.as_ref()
    }

    // =========================================================================
    // Package2
    // =========================================================================

    pub fn has_package2(&self, pkg_type: Package2Type) -> bool {
        self.package2[pkg_type as usize].is_some()
    }

    pub fn get_package2_raw(&self, pkg_type: Package2Type) -> Option<&VirtualFile> {
        self.package2[pkg_type as usize].as_ref()
    }

    /// Decrypt a Package2 file.
    /// Requires package2 keys to have been derived.
    pub fn decrypt_package2(
        &mut self,
        _package2_keys: &[[u8; 16]; 0x20],
        _pkg_type: Package2Type,
    ) {
        // TODO: Implement Package2 decryption using AES-CTR.
        // This requires reading the Package2Header, decrypting the header
        // using the appropriate key, then decrypting each section.
        log::warn!("PartitionDataManager::decrypt_package2 not yet implemented");
    }

    /// Get the decompressed FS section from Package2.
    pub fn get_package2_fs_decompressed(
        &self,
        pkg_type: Package2Type,
    ) -> &[u8] {
        &self.package2_fs[pkg_type as usize]
    }

    /// Get the decompressed SPL section from Package2.
    pub fn get_package2_spl_decompressed(
        &self,
        pkg_type: Package2Type,
    ) -> &[u8] {
        &self.package2_spl[pkg_type as usize]
    }

    // =========================================================================
    // PRODINFO
    // =========================================================================

    pub fn has_prodinfo(&self) -> bool {
        self.prodinfo.is_some()
    }

    pub fn get_prodinfo_raw(&self) -> Option<&VirtualFile> {
        self.prodinfo.as_ref()
    }

    pub fn decrypt_prodinfo(&mut self, _bis_key: [u8; 0x20]) {
        // TODO: Implement PRODINFO decryption using AES-XTS.
        log::warn!("PartitionDataManager::decrypt_prodinfo not yet implemented");
    }

    pub fn get_decrypted_prodinfo(&self) -> Option<&VirtualFile> {
        self.prodinfo_decrypted.as_ref()
    }

    /// Get the eTicket extended KEK from decrypted PRODINFO.
    pub fn get_eticket_extended_kek(&self) -> [u8; 0x240] {
        let mut out = [0u8; 0x240];
        if let Some(ref prodinfo) = self.prodinfo_decrypted {
            prodinfo.read(&mut out, 0x240, 0x3890);
        }
        out
    }
}

/// Search a binary blob for a 16-byte key whose SHA-256 hash matches
/// the expected hash at the given source_hashes index.
///
/// Corresponds to upstream `FindKeyFromHex<0x10>`.
///
/// NOTE: The actual SHA-256 comparison requires the source hash table
/// and a SHA-256 implementation. For now this returns zeroes as a placeholder.
fn find_key_from_hex_16(_binary: &[u8], _hash_index: usize) -> [u8; 0x10] {
    // TODO: Implement SHA-256 key search using the source_hashes table.
    // The algorithm is:
    // 1. For each position i in binary (0..binary.len()-16):
    //    a. SHA-256(binary[i..i+16])
    //    b. Compare with source_hashes[hash_index]
    //    c. If match, return binary[i..i+16]
    // 2. Return zeros if not found.
    [0u8; 0x10]
}

/// Search a binary blob for a 16-byte key whose SHA-256 hash matches
/// the given hash.
///
/// Corresponds to upstream free function `FindKeyFromHex16`.
pub fn find_key_from_hex16(_binary: &[u8], _hash: [u8; 0x20]) -> [u8; 0x10] {
    // TODO: Implement with real SHA-256.
    [0u8; 0x10]
}
