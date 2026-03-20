// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/crypto/partition_data_manager.h and partition_data_manager.cpp
//! Manages partition data (BOOT0, fuses, kfuses, Package2, PRODINFO)
//! for key derivation and decryption during system setup.

use std::sync::Arc;

use crate::crypto::aes_util::{AesCipher, Mode, Op};
use crate::crypto::encryption_layer::FsVfsFileAdapter;
use crate::crypto::sha_util::sha256;
use crate::crypto::xts_encryption_layer::XtsEncryptionLayer;
use crate::file_sys::kernel_executable::INI;
use crate::file_sys::partition_filesystem::ResultStatus;
use crate::file_sys::vfs::vfs_offset::OffsetVfsFile;
use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use crate::file_sys::vfs::vfs_vector::VectorVfsFile;
use common::hex_util::hex_string_to_array;

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
#[derive(Clone, Copy)]
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

impl Default for Package2Header {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

/// PK21 magic value.
const PACKAGE2_MAGIC: u32 = u32::from_le_bytes([b'P', b'K', b'2', b'1']);

// Source hashes used for finding keys in the secure monitor binary.
// Each is a SHA-256 hash of a 16-byte key source.
// Corresponds to upstream `source_hashes` array.
const SOURCE_HASHES: [[u8; 0x20]; 16] = [
    hex_string_to_array("B24BD293259DBC7AC5D63F88E60C59792498E6FC5443402C7FFE87EE8B61A3F0"), // keyblob_mac_key_source
    hex_string_to_array("7944862A3A5C31C6720595EFD302245ABD1B54CCDCF33000557681E65C5664A4"), // master_key_source
    hex_string_to_array("21E2DF100FC9E094DB51B47B9B1D6E94ED379DB8B547955BEF8FE08D8DD35603"), // package2_key_source
    hex_string_to_array("FC02B9D37B42D7A1452E71444F1F700311D1132E301A83B16062E72A78175085"), // aes_kek_generation_source
    hex_string_to_array("FBD10056999EDC7ACDB96098E47E2C3606230270D23281E671F0F389FC5BC585"), // aes_key_generation_source
    hex_string_to_array("C48B619827986C7F4E3081D59DB2B460C84312650E9A8E6B458E53E8CBCA4E87"), // titlekek_source
    hex_string_to_array("04AD66143C726B2A139FB6B21128B46F56C553B2B3887110304298D8D0092D9E"), // key_area_key_application_source
    hex_string_to_array("FD434000C8FF2B26F8E9A9D2D2C12F6BE5773CBB9DC86300E1BD99F8EA33A417"), // key_area_key_ocean_source
    hex_string_to_array("1F17B1FD51AD1C2379B58F152CA4912EC2106441E51722F38700D5937A1162F7"), // key_area_key_system_source
    hex_string_to_array("6B2ED877C2C52334AC51E59ABFA7EC457F4A7D01E46291E9F2EAA45F011D24B7"), // sd_card_kek_source
    hex_string_to_array("D482743563D3EA5DCDC3B74E97C9AC8A342164FA041A1DC80F17F6D31E4BC01C"), // sd_card_save_key_source
    hex_string_to_array("2E751CECF7D93A2B957BD5FFCB082FD038CC2853219DD3092C6DAB9838F5A7CC"), // sd_card_nca_key_source
    hex_string_to_array("1888CAED5551B3EDE01499E87CE0D86827F80820EFB275921055AA4E2ABDFFC2"), // header_kek_source
    hex_string_to_array("8F783E46852DF6BE0BA4E19273C4ADBAEE16380043E1B8C418C4089A8BD64AA6"), // header_key_source
    hex_string_to_array("D1757E52F1AE55FA882EC690BC6F954AC46A83DC22F277F8806BD55577C6EED7"), // rsa_kek_seed3
    hex_string_to_array("FC02B9D37B42D7A1452E71444F1F700311D1132E301A83B16062E72A78175085"), // rsa_kek_mask0
];

// Keyblob source hashes for each revision. Corresponds to upstream `keyblob_source_hashes`.
const KEYBLOB_SOURCE_HASHES: [[u8; 0x20]; 32] = [
    hex_string_to_array("8A06FE274AC491436791FDB388BCDD3AB9943BD4DEF8094418CDAC150FD73786"), // 00
    hex_string_to_array("2D5CAEB2521FEF70B47E17D6D0F11F8CE2C1E442A979AD8035832C4E9FBCCC4B"), // 01
    hex_string_to_array("61C5005E713BAE780641683AF43E5F5C0E03671117F702F401282847D2FC6064"), // 02
    hex_string_to_array("8E9795928E1C4428E1B78F0BE724D7294D6934689C11B190943923B9D5B85903"), // 03
    hex_string_to_array("95FA33AF95AFF9D9B61D164655B32710ED8D615D46C7D6CC3CC70481B686B402"), // 04
    hex_string_to_array("3F5BE7B3C8B1ABD8C10B4B703D44766BA08730562C172A4FE0D6B866B3E2DB3E"), // 05
    [0u8; 0x20], // 06
    [0u8; 0x20], // 07
    [0u8; 0x20], // 08
    [0u8; 0x20], // 09
    [0u8; 0x20], // 0A
    [0u8; 0x20], // 0B
    [0u8; 0x20], // 0C
    [0u8; 0x20], // 0D
    [0u8; 0x20], // 0E
    [0u8; 0x20], // 0F
    [0u8; 0x20], // 10
    [0u8; 0x20], // 11
    [0u8; 0x20], // 12
    [0u8; 0x20], // 13
    [0u8; 0x20], // 14
    [0u8; 0x20], // 15
    [0u8; 0x20], // 16
    [0u8; 0x20], // 17
    [0u8; 0x20], // 18
    [0u8; 0x20], // 19
    [0u8; 0x20], // 1A
    [0u8; 0x20], // 1B
    [0u8; 0x20], // 1C
    [0u8; 0x20], // 1D
    [0u8; 0x20], // 1E
    [0u8; 0x20], // 1F
];

// Master key hashes. Corresponds to upstream `master_key_hashes`.
const MASTER_KEY_HASHES: [[u8; 0x20]; 32] = [
    hex_string_to_array("0EE359BE3C864BB0782E1D70A718A0342C551EED28C369754F9C4F691BECF7CA"), // 00
    hex_string_to_array("4FE707B7E4ABDAF727C894AAF13B1351BFE2AC90D875F73B2E20FA94B9CC661E"), // 01
    hex_string_to_array("79277C0237A2252EC3DFAC1F7C359C2B3D121E9DB15BB9AB4C2B4408D2F3AE09"), // 02
    hex_string_to_array("4F36C565D13325F65EE134073C6A578FFCB0008E02D69400836844EAB7432754"), // 03
    hex_string_to_array("75FF1D95D26113550EE6FCC20ACB58E97EDEB3A2FF52543ED5AEC63BDCC3DA50"), // 04
    hex_string_to_array("EBE2BCD6704673EC0F88A187BB2AD9F1CC82B718C389425941BDC194DC46B0DD"), // 05
    hex_string_to_array("9497E6779F5D840F2BBA1DE4E95BA1D6F21EFC94717D5AE5CA37D7EC5BD37A19"), // 06
    hex_string_to_array("4EC96B8CB01B8DCE382149443430B2B6EBCB2983348AFA04A25E53609DABEDF6"), // 07
    hex_string_to_array("2998E2E23609BC2675FF062A2D64AF5B1B78DFF463B24119D64A1B64F01B2D51"), // 08
    hex_string_to_array("9D486A98067C44B37CF173D3BF577891EB6081FF6B4A166347D9DBBF7025076B"), // 09
    hex_string_to_array("4EC5A237A75A083A9C5F6CF615601522A7F822D06BD4BA32612C9CEBBB29BD45"), // 0A
    [0u8; 0x20], // 0B
    [0u8; 0x20], // 0C
    [0u8; 0x20], // 0D
    [0u8; 0x20], // 0E
    [0u8; 0x20], // 0F
    [0u8; 0x20], // 10
    [0u8; 0x20], // 11
    [0u8; 0x20], // 12
    [0u8; 0x20], // 13
    [0u8; 0x20], // 14
    [0u8; 0x20], // 15
    [0u8; 0x20], // 16
    [0u8; 0x20], // 17
    [0u8; 0x20], // 18
    [0u8; 0x20], // 19
    [0u8; 0x20], // 1A
    [0u8; 0x20], // 1B
    [0u8; 0x20], // 1C
    [0u8; 0x20], // 1D
    [0u8; 0x20], // 1E
    [0u8; 0x20], // 1F
];

/// Calculate MAX_KEYBLOB_SOURCE_HASH: the highest non-zero keyblob source hash index + 1.
/// Corresponds to upstream `CalculateMaxKeyblobSourceHash`.
fn calculate_max_keyblob_source_hash() -> u8 {
    for i in (0..=0x1Fu8).rev() {
        if KEYBLOB_SOURCE_HASHES[i as usize] != [0u8; 0x20] {
            return i + 1;
        }
    }
    0
}

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

/// Try to decrypt a Package2 header with a given key.
/// Returns true if decryption succeeds (magic matches PK21).
/// Corresponds to upstream `AttemptDecrypt`.
fn attempt_decrypt(key: &[u8; 16], header: &mut Package2Header) -> bool {
    let mut temp = *header;
    let mut cipher = AesCipher::new_128(*key, Mode::CTR);
    cipher.set_iv(&header.header_ctr);

    // Decrypt everything after the signature (starting from header_ctr)
    let sig_size = std::mem::size_of::<[u8; 0x100]>();
    let header_size = std::mem::size_of::<Package2Header>();
    let decrypt_size = header_size - sig_size;

    // Get the bytes after signature
    let temp_bytes = unsafe {
        std::slice::from_raw_parts_mut(
            (&mut temp as *mut Package2Header as *mut u8).add(sig_size),
            decrypt_size,
        )
    };
    let src_bytes = unsafe {
        std::slice::from_raw_parts(
            (header as *const Package2Header as *const u8).add(sig_size),
            decrypt_size,
        )
    };
    cipher.transcode(src_bytes, temp_bytes, Op::Decrypt);

    if temp.magic == PACKAGE2_MAGIC {
        *header = temp;
        return true;
    }
    false
}

/// Search a binary for encrypted master keys using AES-ECB decryption and SHA-256 hash matching.
/// Corresponds to upstream `FindEncryptedMasterKeyFromHex`.
fn find_encrypted_master_key_from_hex(binary: &[u8], key: &[u8; 0x10]) -> [[u8; 0x10]; 0x20] {
    if binary.len() < 0x10 {
        return [[0u8; 0x10]; 0x20];
    }

    let mut out = [[0u8; 0x10]; 0x20];
    let cipher = AesCipher::new_128(*key, Mode::ECB);

    for i in 0..binary.len() - 0x10 {
        let mut dec_temp = [0u8; 0x10];
        // Use a new cipher each time since ECB is stateless per block
        let mut c = AesCipher::new_128(*key, Mode::ECB);
        c.transcode(&binary[i..i + 0x10], &mut dec_temp, Op::Decrypt);
        let temp = sha256(&dec_temp);

        for k in 0..out.len() {
            if temp == MASTER_KEY_HASHES[k] {
                out[k] = dec_temp;
                break;
            }
        }
    }

    let _ = cipher; // suppress warning
    out
}

/// Partition data manager.
///
/// Corresponds to upstream `Core::Crypto::PartitionDataManager`.
#[allow(dead_code)]
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
    /// The maximum non-zero keyblob source hash revision.
    /// Corresponds to upstream `MAX_KEYBLOB_SOURCE_HASH`.
    pub fn max_keyblob_source_hash() -> u8 {
        calculate_max_keyblob_source_hash()
    }

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

    /// Get TZ master keys by decrypting candidates from secure monitor bytes.
    /// Corresponds to upstream `GetTZMasterKeys`.
    pub fn get_tz_master_keys(&self, master_key: [u8; 0x10]) -> [[u8; 0x10]; 0x20] {
        find_encrypted_master_key_from_hex(&self.secure_monitor_bytes, &master_key)
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

    /// Get the keyblob key source for a given revision.
    /// Corresponds to upstream `GetKeyblobKeySource`.
    pub fn get_keyblob_key_source(&self, revision: usize) -> [u8; 0x10] {
        if KEYBLOB_SOURCE_HASHES[revision] == [0u8; 0x20] {
            log::warn!(
                "No keyblob source hash for crypto revision {:02X}! Cannot derive keys...",
                revision
            );
        }
        find_key_from_hex(&self.package1_decrypted_bytes, &KEYBLOB_SOURCE_HASHES[revision])
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

    /// Has any Package2 file (default NormalMain).
    pub fn has_package2_default(&self) -> bool {
        self.has_package2(Package2Type::NormalMain)
    }

    pub fn get_package2_raw(&self, pkg_type: Package2Type) -> Option<&VirtualFile> {
        self.package2[pkg_type as usize].as_ref()
    }

    /// Decrypt a Package2 file.
    /// Requires package2 keys to have been derived.
    /// Corresponds to upstream `DecryptPackage2`.
    pub fn decrypt_package2(
        &mut self,
        package2_keys: &[[u8; 16]; 0x20],
        pkg_type: Package2Type,
    ) {
        let pkg_file = match self.package2[pkg_type as usize].as_ref() {
            Some(f) => f.clone(),
            None => return,
        };

        let file_size = pkg_file.get_size();
        if file_size <= 0x4000 {
            return;
        }

        // Create an offset view starting at 0x4000
        let file: VirtualFile = Arc::new(OffsetVfsFile::new(
            pkg_file,
            file_size - 0x4000,
            0x4000,
            String::new(),
        ));

        // Read the header
        let header_size = std::mem::size_of::<Package2Header>();
        let raw_bytes = file.read_bytes(header_size, 0);
        if raw_bytes.len() < header_size {
            return;
        }
        let mut header = Package2Header::default();
        unsafe {
            std::ptr::copy_nonoverlapping(
                raw_bytes.as_ptr(),
                &mut header as *mut Package2Header as *mut u8,
                header_size,
            );
        }

        // Try to decrypt header if it's not already decrypted
        let mut revision = 0xFFusize;
        if header.magic != PACKAGE2_MAGIC {
            for i in 0..package2_keys.len() {
                if attempt_decrypt(&package2_keys[i], &mut header) {
                    revision = i;
                }
            }
        }

        if header.magic != PACKAGE2_MAGIC {
            return;
        }

        // Read and decrypt section 1 (INI section)
        let section1_offset = header.section_size[0] as usize + header_size;
        let section1_size = header.section_size[1] as usize;
        if section1_size == 0 {
            return;
        }

        let section1_raw = file.read_bytes(section1_size, section1_offset);
        if section1_raw.len() < section1_size {
            return;
        }

        let mut section1_dec = vec![0u8; section1_size];
        let mut cipher = AesCipher::new_128(package2_keys[revision], Mode::CTR);
        cipher.set_iv(&header.section_ctr[1]);
        cipher.transcode(&section1_raw, &mut section1_dec, Op::Decrypt);

        // Parse INI from decrypted section
        let ini_file: VirtualFile = Arc::new(VectorVfsFile::new(
            section1_dec,
            "ini".to_string(),
            None,
        ));
        let ini = INI::new(&ini_file);
        if ini.get_status() != ResultStatus::Success {
            return;
        }

        for kip in ini.get_kips() {
            if kip.get_status() != ResultStatus::Success {
                return;
            }

            let name = kip.get_name();
            if name != "FS" && name != "spl" {
                continue;
            }

            let text = kip.get_text_section();
            let rodata = kip.get_rodata_section();
            let data = kip.get_data_section();

            let mut out = Vec::with_capacity(text.len() + rodata.len() + data.len());
            out.extend_from_slice(text);
            out.extend_from_slice(rodata);
            out.extend_from_slice(data);

            if name == "FS" {
                self.package2_fs[pkg_type as usize] = out;
            } else if name == "spl" {
                self.package2_spl[pkg_type as usize] = out;
            }
        }
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

    /// Get key area key application source from FS section.
    pub fn get_key_area_key_application_source(&self, pkg_type: Package2Type) -> [u8; 0x10] {
        find_key_from_hex(&self.package2_fs[pkg_type as usize], &SOURCE_HASHES[6])
    }

    /// Convenience: default NormalMain.
    pub fn get_key_area_key_application_source_default(&self) -> [u8; 0x10] {
        self.get_key_area_key_application_source(Package2Type::NormalMain)
    }

    /// Get key area key ocean source from FS section.
    pub fn get_key_area_key_ocean_source(&self, pkg_type: Package2Type) -> [u8; 0x10] {
        find_key_from_hex(&self.package2_fs[pkg_type as usize], &SOURCE_HASHES[7])
    }

    pub fn get_key_area_key_ocean_source_default(&self) -> [u8; 0x10] {
        self.get_key_area_key_ocean_source(Package2Type::NormalMain)
    }

    /// Get key area key system source from FS section.
    pub fn get_key_area_key_system_source(&self, pkg_type: Package2Type) -> [u8; 0x10] {
        find_key_from_hex(&self.package2_fs[pkg_type as usize], &SOURCE_HASHES[8])
    }

    pub fn get_key_area_key_system_source_default(&self) -> [u8; 0x10] {
        self.get_key_area_key_system_source(Package2Type::NormalMain)
    }

    /// Get SD KEK source from FS section.
    pub fn get_sd_kek_source(&self, pkg_type: Package2Type) -> [u8; 0x10] {
        find_key_from_hex(&self.package2_fs[pkg_type as usize], &SOURCE_HASHES[9])
    }

    pub fn get_sd_kek_source_default(&self) -> [u8; 0x10] {
        self.get_sd_kek_source(Package2Type::NormalMain)
    }

    /// Get SD save key source from FS section (32 bytes).
    pub fn get_sd_save_key_source(&self, pkg_type: Package2Type) -> [u8; 0x20] {
        find_key_from_hex_32(&self.package2_fs[pkg_type as usize], &SOURCE_HASHES[10])
    }

    pub fn get_sd_save_key_source_default(&self) -> [u8; 0x20] {
        self.get_sd_save_key_source(Package2Type::NormalMain)
    }

    /// Get SD NCA key source from FS section (32 bytes).
    pub fn get_sd_nca_key_source(&self, pkg_type: Package2Type) -> [u8; 0x20] {
        find_key_from_hex_32(&self.package2_fs[pkg_type as usize], &SOURCE_HASHES[11])
    }

    pub fn get_sd_nca_key_source_default(&self) -> [u8; 0x20] {
        self.get_sd_nca_key_source(Package2Type::NormalMain)
    }

    /// Get header KEK source from FS section.
    pub fn get_header_kek_source(&self, pkg_type: Package2Type) -> [u8; 0x10] {
        find_key_from_hex(&self.package2_fs[pkg_type as usize], &SOURCE_HASHES[12])
    }

    pub fn get_header_kek_source_default(&self) -> [u8; 0x10] {
        self.get_header_kek_source(Package2Type::NormalMain)
    }

    /// Get header key source from FS section (32 bytes).
    pub fn get_header_key_source(&self, pkg_type: Package2Type) -> [u8; 0x20] {
        find_key_from_hex_32(&self.package2_fs[pkg_type as usize], &SOURCE_HASHES[13])
    }

    pub fn get_header_key_source_default(&self) -> [u8; 0x20] {
        self.get_header_key_source(Package2Type::NormalMain)
    }

    /// Get AES key generation source from SPL section.
    pub fn get_aes_key_generation_source(&self, pkg_type: Package2Type) -> [u8; 0x10] {
        find_key_from_hex(&self.package2_spl[pkg_type as usize], &SOURCE_HASHES[4])
    }

    pub fn get_aes_key_generation_source_default(&self) -> [u8; 0x10] {
        self.get_aes_key_generation_source(Package2Type::NormalMain)
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

    /// Decrypt PRODINFO using AES-XTS with the provided BIS key.
    /// Corresponds to upstream `DecryptProdInfo`.
    pub fn decrypt_prodinfo(&mut self, bis_key: [u8; 0x20]) {
        let prodinfo = match self.prodinfo.as_ref() {
            Some(f) => f.clone(),
            None => return,
        };

        // Wrap the file_sys VirtualFile as an encryption_layer VirtualFile
        let adapted: super::encryption_layer::VirtualFile =
            Arc::new(FsVfsFileAdapter::new(prodinfo));
        let xts = XtsEncryptionLayer::new(adapted, bis_key);
        // The XtsEncryptionLayer implements file_sys::vfs::vfs::VfsFile, so wrap as VirtualFile
        self.prodinfo_decrypted = Some(Arc::new(xts));
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
/// Corresponds to upstream `FindKeyFromHex<0x10>(binary, source_hashes[hash_index])`.
fn find_key_from_hex_16(binary: &[u8], hash_index: usize) -> [u8; 0x10] {
    if hash_index >= SOURCE_HASHES.len() {
        return [0u8; 0x10];
    }
    find_key_from_hex(binary, &SOURCE_HASHES[hash_index])
}

/// Generic search for a 16-byte key by SHA-256 hash.
/// Corresponds to upstream `FindKeyFromHex<0x10>`.
fn find_key_from_hex(binary: &[u8], hash: &[u8; 0x20]) -> [u8; 0x10] {
    if binary.len() < 0x10 {
        return [0u8; 0x10];
    }

    for i in 0..binary.len() - 0x10 {
        let temp = sha256(&binary[i..i + 0x10]);
        if temp == *hash {
            let mut out = [0u8; 0x10];
            out.copy_from_slice(&binary[i..i + 0x10]);
            return out;
        }
    }

    [0u8; 0x10]
}

/// Search for a 32-byte key by SHA-256 hash.
/// Corresponds to upstream `FindKeyFromHex<0x20>`.
fn find_key_from_hex_32(binary: &[u8], hash: &[u8; 0x20]) -> [u8; 0x20] {
    if binary.len() < 0x20 {
        return [0u8; 0x20];
    }

    for i in 0..binary.len() - 0x20 {
        let temp = sha256(&binary[i..i + 0x20]);
        if temp == *hash {
            let mut out = [0u8; 0x20];
            out.copy_from_slice(&binary[i..i + 0x20]);
            return out;
        }
    }

    [0u8; 0x20]
}

/// Search a binary blob for a 16-byte key whose SHA-256 hash matches
/// the given hash.
///
/// Corresponds to upstream free function `FindKeyFromHex16`.
pub fn find_key_from_hex16(binary: &[u8], hash: [u8; 0x20]) -> [u8; 0x10] {
    find_key_from_hex(binary, &hash)
}
