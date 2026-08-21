// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

// SPDX-FileCopyrightText: Copyright 2017 socram8888/amiitool
// SPDX-License-Identifier: MIT

//! Port of zuyu/src/core/hle/service/nfc/common/amiibo_crypto.h
//! Port of zuyu/src/core/hle/service/nfc/common/amiibo_crypto.cpp
//!
//! Amiibo encryption/decryption utilities.

use crate::hle::service::nfc::nfc_types::PackedTagType;
use crate::hle::service::nfp::nfp_types::{
    EncryptedAmiiboFile, EncryptedNtag215File, HashData, Ntag215File, TagUuid,
};

use aes::Aes128;
use cipher::{KeyIvInit, StreamCipher};
use hmac::{Hmac, Mac};
use sha2::Sha256;

type HmacSha256 = Hmac<Sha256>;
type Aes128Ctr = ctr::Ctr128BE<Aes128>;

// Byte locations in NTAG215File (matching upstream constants)
const HMAC_DATA_START: usize = 0x8;
const SETTINGS_START: usize = 0x2C;
const WRITE_COUNTER_START: usize = 0x29;
const HMAC_TAG_START: usize = 0x1B4;
const UUID_START: usize = 0x1D4;
const DYNAMIC_LOCK_START: usize = 0x208;

pub type HmacKey = [u8; 0x10];
pub type DrgbOutput = [u8; 0x20];

/// Corresponds to `HashSeed` in upstream amiibo_crypto.h.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct HashSeed {
    pub magic: u16, // big-endian write_counter
    pub padding: [u8; 0xE],
    pub uid_1: TagUuid,
    pub uid_2: TagUuid,
    pub keygen_salt: [u8; 0x20],
}
const _: () = assert!(core::mem::size_of::<HashSeed>() == 0x40);

impl Default for HashSeed {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// Corresponds to `InternalKey` in upstream amiibo_crypto.h.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct InternalKey {
    pub hmac_key: HmacKey,
    pub type_string: [u8; 0xE],
    pub reserved: u8,
    pub magic_length: u8,
    pub magic_bytes: [u8; 0x10],
    pub xor_pad: [u8; 0x20],
}
const _: () = assert!(core::mem::size_of::<InternalKey>() == 0x50);

impl Default for InternalKey {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// Corresponds to `DerivedKeys` in upstream amiibo_crypto.h.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct DerivedKeys {
    pub aes_key: [u8; 0x10],
    pub aes_iv: [u8; 0x10],
    pub hmac_key: [u8; 0x10],
}
const _: () = assert!(core::mem::size_of::<DerivedKeys>() == 0x30);

impl Default for DerivedKeys {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// Corresponds to `CryptoCtx` in upstream amiibo_crypto.h.
struct CryptoCtx {
    buffer: Vec<u8>,
    used: bool,
    buffer_size: usize,
    counter: i16,
}

/// Validates that the encrypted amiibo file is not corrupted.
///
/// Corresponds to `IsAmiiboValid(const EncryptedNTAG215File&)` in upstream.
pub fn is_amiibo_valid_encrypted(ntag_file: &EncryptedNtag215File) -> bool {
    // Read packed fields safely
    let uuid = unsafe { core::ptr::read_unaligned(core::ptr::addr_of!(ntag_file.uuid)) };
    let uuid_crc_check2 = ntag_file.uuid_crc_check2;
    let static_lock =
        unsafe { core::ptr::read_unaligned(core::ptr::addr_of!(ntag_file.static_lock)) };
    let compatibility_container = unsafe {
        core::ptr::read_unaligned(core::ptr::addr_of!(ntag_file.compatibility_container))
    };
    let dynamic_lock =
        unsafe { core::ptr::read_unaligned(core::ptr::addr_of!(ntag_file.dynamic_lock)) };
    let cfg0 = unsafe { core::ptr::read_unaligned(core::ptr::addr_of!(ntag_file.cfg0)) };
    let cfg1 = unsafe { core::ptr::read_unaligned(core::ptr::addr_of!(ntag_file.cfg1)) };

    let user_memory =
        unsafe { core::ptr::read_unaligned(core::ptr::addr_of!(ntag_file.user_memory)) };
    let write_counter =
        unsafe { core::ptr::read_unaligned(core::ptr::addr_of!(user_memory.write_counter)) };
    let model_info =
        unsafe { core::ptr::read_unaligned(core::ptr::addr_of!(user_memory.model_info)) };

    log::debug!("uuid_lock=0x{:x}", static_lock);
    log::debug!("compatibility_container=0x{:x}", compatibility_container);
    log::debug!("write_count={}", write_counter);
    let character_id = { model_info.character_id };
    let character_variant = { model_info.character_variant };
    let amiibo_type = { model_info.amiibo_type };
    let model_number = { model_info.model_number };
    let series = { model_info.series };
    let tag_type = { model_info.tag_type };
    log::debug!("character_id=0x{:x}", character_id);
    log::debug!("character_variant={}", character_variant);
    log::debug!("amiibo_type={}", amiibo_type);
    log::debug!("model_number=0x{:x}", model_number);
    log::debug!("series={}", series);
    log::debug!("tag_type=0x{:x}", tag_type);
    log::debug!("tag_dynamic_lock=0x{:x}", dynamic_lock);
    log::debug!("tag_CFG0=0x{:x}", cfg0);
    log::debug!("tag_CFG1=0x{:x}", cfg1);

    // Validate UUID CRC
    const CT: u8 = 0x88; // As defined in ISO/IEC 14443-3
    if (CT ^ uuid.part1[0] ^ uuid.part1[1] ^ uuid.part1[2]) != uuid.crc_check1 {
        return false;
    }
    if (uuid.part2[0] ^ uuid.part2[1] ^ uuid.part2[2] ^ uuid.nintendo_id) != uuid_crc_check2 {
        return false;
    }

    // Check against all known constants on an amiibo binary
    if static_lock != 0xE00F {
        return false;
    }
    if compatibility_container != 0xEEFF10F1 {
        return false;
    }
    if tag_type != PackedTagType::TYPE2.bits() {
        return false;
    }
    if (dynamic_lock & 0xFFFFFF) != 0x0F0001 {
        return false;
    }
    if cfg0 != 0x04000000 {
        return false;
    }
    if cfg1 != 0x5F {
        return false;
    }
    true
}

/// Validates that the decoded amiibo file is not corrupted.
///
/// Corresponds to `IsAmiiboValid(const NTAG215File&)` in upstream.
pub fn is_amiibo_valid(ntag_file: &Ntag215File) -> bool {
    is_amiibo_valid_encrypted(&encoded_data_to_nfc_data(ntag_file))
}

/// Converts from encrypted file format to encoded file format.
///
/// Corresponds to `NfcDataToEncodedData` in upstream.
pub fn nfc_data_to_encoded_data(nfc_data: &EncryptedNtag215File) -> Ntag215File {
    let mut encoded_data = Ntag215File::default();

    // Read all packed fields safely via read_unaligned
    unsafe {
        encoded_data.uid = core::ptr::read_unaligned(core::ptr::addr_of!(nfc_data.uuid));
        encoded_data.uid_crc_check2 = nfc_data.uuid_crc_check2;
        encoded_data.internal_number = nfc_data.internal_number;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.static_lock),
            core::ptr::read_unaligned(core::ptr::addr_of!(nfc_data.static_lock)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.compatibility_container),
            core::ptr::read_unaligned(core::ptr::addr_of!(nfc_data.compatibility_container)),
        );

        let user_memory = core::ptr::read_unaligned(core::ptr::addr_of!(nfc_data.user_memory));
        encoded_data.hmac_data = user_memory.hmac_data;
        encoded_data.constant_value = user_memory.constant_value;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.write_counter),
            core::ptr::read_unaligned(core::ptr::addr_of!(user_memory.write_counter)),
        );
        encoded_data.amiibo_version = user_memory.amiibo_version;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.settings),
            core::ptr::read_unaligned(core::ptr::addr_of!(user_memory.settings)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.owner_mii),
            core::ptr::read_unaligned(core::ptr::addr_of!(user_memory.owner_mii)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.application_id),
            core::ptr::read_unaligned(core::ptr::addr_of!(user_memory.application_id)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.application_write_counter),
            core::ptr::read_unaligned(core::ptr::addr_of!(user_memory.application_write_counter)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.application_area_id),
            core::ptr::read_unaligned(core::ptr::addr_of!(user_memory.application_area_id)),
        );
        encoded_data.application_id_byte = user_memory.application_id_byte;
        encoded_data.unknown = user_memory.unknown;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.mii_extension),
            core::ptr::read_unaligned(core::ptr::addr_of!(user_memory.mii_extension)),
        );
        encoded_data.unknown2 = user_memory.unknown2;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.register_info_crc),
            core::ptr::read_unaligned(core::ptr::addr_of!(user_memory.register_info_crc)),
        );
        encoded_data.application_area = user_memory.application_area;
        encoded_data.hmac_tag = user_memory.hmac_tag;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.model_info),
            core::ptr::read_unaligned(core::ptr::addr_of!(user_memory.model_info)),
        );
        encoded_data.keygen_salt = user_memory.keygen_salt;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.dynamic_lock),
            core::ptr::read_unaligned(core::ptr::addr_of!(nfc_data.dynamic_lock)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.cfg0),
            core::ptr::read_unaligned(core::ptr::addr_of!(nfc_data.cfg0)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.cfg1),
            core::ptr::read_unaligned(core::ptr::addr_of!(nfc_data.cfg1)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(encoded_data.password),
            core::ptr::read_unaligned(core::ptr::addr_of!(nfc_data.password)),
        );
    }

    encoded_data
}

/// Converts from encoded file format to encrypted file format.
///
/// Corresponds to `EncodedDataToNfcData` in upstream.
pub fn encoded_data_to_nfc_data(encoded_data: &Ntag215File) -> EncryptedNtag215File {
    let mut nfc_data = EncryptedNtag215File::default();

    unsafe {
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(nfc_data.uuid),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.uid)),
        );
        nfc_data.uuid_crc_check2 = encoded_data.uid_crc_check2;
        nfc_data.internal_number = encoded_data.internal_number;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(nfc_data.static_lock),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.static_lock)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(nfc_data.compatibility_container),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.compatibility_container)),
        );

        // Build user_memory
        let mut user_memory = EncryptedAmiiboFile::default();
        user_memory.hmac_data = encoded_data.hmac_data;
        user_memory.constant_value = encoded_data.constant_value;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(user_memory.write_counter),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.write_counter)),
        );
        user_memory.amiibo_version = encoded_data.amiibo_version;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(user_memory.settings),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.settings)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(user_memory.owner_mii),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.owner_mii)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(user_memory.application_id),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.application_id)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(user_memory.application_write_counter),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.application_write_counter)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(user_memory.application_area_id),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.application_area_id)),
        );
        user_memory.application_id_byte = encoded_data.application_id_byte;
        user_memory.unknown = encoded_data.unknown;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(user_memory.mii_extension),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.mii_extension)),
        );
        user_memory.unknown2 = encoded_data.unknown2;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(user_memory.register_info_crc),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.register_info_crc)),
        );
        user_memory.application_area = encoded_data.application_area;
        user_memory.hmac_tag = encoded_data.hmac_tag;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(user_memory.model_info),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.model_info)),
        );
        user_memory.keygen_salt = encoded_data.keygen_salt;

        core::ptr::write_unaligned(core::ptr::addr_of_mut!(nfc_data.user_memory), user_memory);
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(nfc_data.dynamic_lock),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.dynamic_lock)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(nfc_data.cfg0),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.cfg0)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(nfc_data.cfg1),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.cfg1)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(nfc_data.password),
            core::ptr::read_unaligned(core::ptr::addr_of!(encoded_data.password)),
        );
    }

    nfc_data
}

/// Generates seed needed for key derivation.
///
/// Corresponds to `GetSeed` in upstream.
fn get_seed(data: &Ntag215File) -> HashSeed {
    let write_counter =
        unsafe { core::ptr::read_unaligned(core::ptr::addr_of!(data.write_counter)) };
    let uid = unsafe { core::ptr::read_unaligned(core::ptr::addr_of!(data.uid)) };
    HashSeed {
        magic: write_counter,
        padding: [0u8; 0xE],
        uid_1: uid,
        uid_2: uid,
        keygen_salt: data.keygen_salt,
    }
}

/// Middle step on the generation of derived keys.
///
/// Corresponds to `GenerateInternalKey` in upstream.
fn generate_internal_key(key: &InternalKey, seed: &HashSeed) -> Vec<u8> {
    let seed_part1_len = key.magic_bytes.len() - key.magic_length as usize;
    let string_size = key.type_string.len(); // 0xE

    let mut output = Vec::with_capacity(string_size + seed_part1_len);

    // Copy whole type string (up to first null byte, like memccpy with '\0')
    let null_pos = key
        .type_string
        .iter()
        .position(|&b| b == 0)
        .unwrap_or(string_size);
    output.extend_from_slice(&key.type_string[..null_pos]);
    // If null was found, include it; memccpy stops after copying the delimiter
    if null_pos < string_size {
        output.push(0);
    }
    // Pad remaining bytes from type_string that memccpy would have skipped
    // Actually, memccpy copies up to and including the delimiter or count bytes.
    // The upstream code passes the full string_size as count.
    // memccpy returns a pointer past the delimiter if found, or NULL if not.
    // But the output vector was sized to string_size + seedPart1Len, and the
    // data after the memccpy'd region is overwritten by the next memcpy.
    // Let me re-read the upstream more carefully.
    //
    // output.data() is of size string_size + seedPart1Len
    // memccpy(output.data(), key.type_string.data(), '\0', string_size)
    //   -> copies up to string_size bytes, stopping after '\0'
    //   -> returns pointer past the copied '\0', or NULL
    // memcpy(output.data() + string_size, &seed, seedPart1Len)
    //   -> copies seedPart1Len bytes of seed starting at output + string_size
    //
    // So the bytes between the end of the type_string copy and string_size
    // are left uninitialized (from the vector constructor, they'd be zero).
    // Then seedPart1Len bytes of seed are placed at offset string_size.
    //
    // The vector was created with size string_size + seedPart1Len, so it has
    // exactly that many bytes already allocated and "valid".

    // Reset and redo: create a vector of the right initial size
    output.clear();
    output.resize(string_size + seed_part1_len, 0u8);

    // Copy type_string (up to and including first null, or full string_size)
    {
        let copy_len = null_pos.min(string_size);
        output[..copy_len].copy_from_slice(&key.type_string[..copy_len]);
        if null_pos < string_size {
            output[copy_len] = 0; // the null delimiter
        }
    }

    // Append seedPart1Len bytes from the seed at offset string_size
    let seed_bytes: &[u8] = unsafe {
        core::slice::from_raw_parts(
            seed as *const HashSeed as *const u8,
            core::mem::size_of::<HashSeed>(),
        )
    };
    output[string_size..string_size + seed_part1_len]
        .copy_from_slice(&seed_bytes[..seed_part1_len]);

    // Append magic_length bytes from magic_bytes
    output.extend_from_slice(&key.magic_bytes[..key.magic_length as usize]);

    // Append uid_1 as bytes
    let uid1_bytes: &[u8] = unsafe {
        core::slice::from_raw_parts(
            &seed.uid_1 as *const TagUuid as *const u8,
            core::mem::size_of::<TagUuid>(),
        )
    };
    output.extend_from_slice(uid1_bytes);

    // Append uid_2 as bytes
    let uid2_bytes: &[u8] = unsafe {
        core::slice::from_raw_parts(
            &seed.uid_2 as *const TagUuid as *const u8,
            core::mem::size_of::<TagUuid>(),
        )
    };
    output.extend_from_slice(uid2_bytes);

    // XOR keygen_salt with xor_pad
    for i in 0..seed.keygen_salt.len() {
        output.push(seed.keygen_salt[i] ^ key.xor_pad[i]);
    }

    output
}

/// Initializes the HMAC-DRBG-like context.
///
/// Corresponds to `CryptoInit` in upstream.
fn crypto_init(_hmac_key: &HmacKey, seed: &[u8]) -> CryptoCtx {
    let buffer_size = 2 + seed.len(); // sizeof(u16) + seed.size()
    let mut buffer = vec![0u8; buffer_size];
    // Place seed after the 2-byte counter prefix
    buffer[2..].copy_from_slice(seed);

    CryptoCtx {
        buffer,
        used: false,
        buffer_size,
        counter: 0,
    }
}

/// Feeds data to HMAC context to generate one block of derived output.
///
/// Corresponds to `CryptoStep` in upstream.
fn crypto_step(ctx: &mut CryptoCtx, hmac_key: &HmacKey) -> DrgbOutput {
    // Store counter in big endian, and increment
    ctx.buffer[0] = (ctx.counter >> 8) as u8;
    ctx.buffer[1] = (ctx.counter >> 0) as u8;
    ctx.counter += 1;
    ctx.used = true;

    // HMAC-SHA256
    let mut mac = HmacSha256::new_from_slice(hmac_key).expect("HMAC key length is always valid");
    mac.update(&ctx.buffer[..ctx.buffer_size]);
    let result = mac.finalize().into_bytes();

    let mut output = [0u8; 0x20];
    output.copy_from_slice(&result);
    output
}

/// Generates the derived key from amiibo data.
///
/// Corresponds to `GenerateKey` in upstream.
fn generate_key(key: &InternalKey, data: &Ntag215File) -> DerivedKeys {
    let seed = get_seed(data);

    // Generate internal seed
    let internal_key = generate_internal_key(key, &seed);

    // Initialize context
    let mut ctx = crypto_init(&key.hmac_key, &internal_key);

    // Generate derived keys: two HMAC steps produce 64 bytes, we take first 48
    let temp0 = crypto_step(&mut ctx, &key.hmac_key);
    let temp1 = crypto_step(&mut ctx, &key.hmac_key);

    let mut derived_keys = DerivedKeys::default();
    // Copy 48 bytes (0x30) from the two 32-byte outputs
    let combined: [u8; 64] = {
        let mut buf = [0u8; 64];
        buf[..32].copy_from_slice(&temp0);
        buf[32..].copy_from_slice(&temp1);
        buf
    };
    let dk_bytes: &mut [u8] = unsafe {
        core::slice::from_raw_parts_mut(
            &mut derived_keys as *mut DerivedKeys as *mut u8,
            core::mem::size_of::<DerivedKeys>(),
        )
    };
    dk_bytes.copy_from_slice(&combined[..0x30]);

    derived_keys
}

/// Encodes or decodes amiibo data using AES-128-CTR.
///
/// Encrypts/decrypts the range [SETTINGS_START, HMAC_TAG_START) = [0x2C, 0x1B4).
/// Copies remaining fields directly.
///
/// Corresponds to `Cipher` in upstream.
fn cipher(keys: &DerivedKeys, in_data: &Ntag215File, out_data: &mut Ntag215File) {
    const ENCRYPTED_DATA_SIZE: usize = HMAC_TAG_START - SETTINGS_START;

    // Get the encrypted region as byte slices via the raw NTAG215File bytes
    let in_bytes: &[u8] = unsafe {
        core::slice::from_raw_parts(
            in_data as *const Ntag215File as *const u8,
            core::mem::size_of::<Ntag215File>(),
        )
    };
    let out_bytes: &mut [u8] = unsafe {
        core::slice::from_raw_parts_mut(
            out_data as *mut Ntag215File as *mut u8,
            core::mem::size_of::<Ntag215File>(),
        )
    };

    // AES-128-CTR encrypt/decrypt the settings region
    let mut ctr_cipher = Aes128Ctr::new(keys.aes_key.as_ref().into(), keys.aes_iv.as_ref().into());

    // Copy input encrypted region to output, then apply CTR in-place
    out_bytes[SETTINGS_START..SETTINGS_START + ENCRYPTED_DATA_SIZE]
        .copy_from_slice(&in_bytes[SETTINGS_START..SETTINGS_START + ENCRYPTED_DATA_SIZE]);
    ctr_cipher
        .apply_keystream(&mut out_bytes[SETTINGS_START..SETTINGS_START + ENCRYPTED_DATA_SIZE]);

    // Copy the rest of the data directly (non-encrypted fields)
    unsafe {
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(out_data.uid),
            core::ptr::read_unaligned(core::ptr::addr_of!(in_data.uid)),
        );
        out_data.uid_crc_check2 = in_data.uid_crc_check2;
        out_data.internal_number = in_data.internal_number;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(out_data.static_lock),
            core::ptr::read_unaligned(core::ptr::addr_of!(in_data.static_lock)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(out_data.compatibility_container),
            core::ptr::read_unaligned(core::ptr::addr_of!(in_data.compatibility_container)),
        );

        out_data.constant_value = in_data.constant_value;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(out_data.write_counter),
            core::ptr::read_unaligned(core::ptr::addr_of!(in_data.write_counter)),
        );

        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(out_data.model_info),
            core::ptr::read_unaligned(core::ptr::addr_of!(in_data.model_info)),
        );
        out_data.keygen_salt = in_data.keygen_salt;
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(out_data.dynamic_lock),
            core::ptr::read_unaligned(core::ptr::addr_of!(in_data.dynamic_lock)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(out_data.cfg0),
            core::ptr::read_unaligned(core::ptr::addr_of!(in_data.cfg0)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(out_data.cfg1),
            core::ptr::read_unaligned(core::ptr::addr_of!(in_data.cfg1)),
        );
        core::ptr::write_unaligned(
            core::ptr::addr_of_mut!(out_data.password),
            core::ptr::read_unaligned(core::ptr::addr_of!(in_data.password)),
        );
    }
}

/// Loads both amiibo keys from key_retail.bin.
///
/// Returns (locked_secret, unfixed_info) on success.
///
/// Corresponds to `LoadKeys` in upstream.
fn load_keys() -> Option<(InternalKey, InternalKey)> {
    let keys_dir = common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::KeysDir);
    let key_path = keys_dir.join("key_retail.bin");

    let data = match std::fs::read(&key_path) {
        Ok(d) => d,
        Err(_) => {
            log::error!("Failed to open key file");
            return None;
        }
    };

    const KEY_SIZE: usize = core::mem::size_of::<InternalKey>(); // 0x50
    if data.len() < KEY_SIZE * 2 {
        log::error!(
            "key_retail.bin too small: {} bytes, need {}",
            data.len(),
            KEY_SIZE * 2
        );
        return None;
    }

    let mut unfixed_info = InternalKey::default();
    let mut locked_secret = InternalKey::default();

    unsafe {
        let dst = &mut unfixed_info as *mut InternalKey as *mut u8;
        core::ptr::copy_nonoverlapping(data.as_ptr(), dst, KEY_SIZE);

        let dst = &mut locked_secret as *mut InternalKey as *mut u8;
        core::ptr::copy_nonoverlapping(data.as_ptr().add(KEY_SIZE), dst, KEY_SIZE);
    }

    Some((locked_secret, unfixed_info))
}

/// Returns true if key_retail.bin exists.
///
/// Corresponds to `IsKeyAvailable` in upstream.
pub fn is_amiibo_crypto_available() -> bool {
    let keys_dir = common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::KeysDir);
    common::fs::fs::exists(&keys_dir.join("key_retail.bin"))
}

/// Decodes encrypted amiibo data. Returns true if output is valid.
///
/// Corresponds to `DecodeAmiibo` in upstream.
pub fn decode_amiibo(
    encrypted_tag_data: &EncryptedNtag215File,
    tag_data: &mut Ntag215File,
) -> bool {
    let (locked_secret, unfixed_info) = match load_keys() {
        Some(keys) => keys,
        None => return false,
    };

    // Generate keys
    let encoded_data = nfc_data_to_encoded_data(encrypted_tag_data);
    let data_keys = generate_key(&unfixed_info, &encoded_data);
    let tag_keys = generate_key(&locked_secret, &encoded_data);

    // Decrypt
    cipher(&data_keys, &encoded_data, tag_data);

    // Regenerate tag HMAC. Note: order matters, data HMAC depends on tag HMAC!
    let tag_data_bytes: &[u8] = unsafe {
        core::slice::from_raw_parts(
            tag_data as *const Ntag215File as *const u8,
            core::mem::size_of::<Ntag215File>(),
        )
    };

    let input_length = DYNAMIC_LOCK_START - UUID_START;
    let tag_hmac = {
        let mut mac = HmacSha256::new_from_slice(&tag_keys.hmac_key)
            .expect("HMAC key length is always valid");
        mac.update(&tag_data_bytes[UUID_START..UUID_START + input_length]);
        let result = mac.finalize().into_bytes();
        let mut hmac: HashData = [0u8; 0x20];
        hmac.copy_from_slice(&result);
        hmac
    };
    tag_data.hmac_tag = tag_hmac;

    // Regenerate data HMAC
    let input_length2 = DYNAMIC_LOCK_START - WRITE_COUNTER_START;
    // Need to re-get tag_data_bytes after modifying hmac_tag
    let tag_data_bytes: &[u8] = unsafe {
        core::slice::from_raw_parts(
            tag_data as *const Ntag215File as *const u8,
            core::mem::size_of::<Ntag215File>(),
        )
    };
    let data_hmac = {
        let mut mac = HmacSha256::new_from_slice(&data_keys.hmac_key)
            .expect("HMAC key length is always valid");
        mac.update(&tag_data_bytes[WRITE_COUNTER_START..WRITE_COUNTER_START + input_length2]);
        let result = mac.finalize().into_bytes();
        let mut hmac: HashData = [0u8; 0x20];
        hmac.copy_from_slice(&result);
        hmac
    };
    tag_data.hmac_data = data_hmac;

    // Validate HMACs
    let encrypted_user_memory =
        unsafe { core::ptr::read_unaligned(core::ptr::addr_of!(encrypted_tag_data.user_memory)) };

    if tag_data.hmac_data != encrypted_user_memory.hmac_data {
        log::error!("hmac_data doesn't match");
        return false;
    }

    if tag_data.hmac_tag != encrypted_user_memory.hmac_tag {
        log::error!("hmac_tag doesn't match");
        return false;
    }

    true
}

/// Encodes plain amiibo data. Returns true if output is valid.
///
/// Corresponds to `EncodeAmiibo` in upstream.
pub fn encode_amiibo(
    tag_data: &Ntag215File,
    encrypted_tag_data: &mut EncryptedNtag215File,
) -> bool {
    let (locked_secret, unfixed_info) = match load_keys() {
        Some(keys) => keys,
        None => return false,
    };

    // Generate keys
    let data_keys = generate_key(&unfixed_info, tag_data);
    let tag_keys = generate_key(&locked_secret, tag_data);

    let mut encoded_tag_data = Ntag215File::default();

    // Generate tag HMAC
    let tag_data_bytes: &[u8] = unsafe {
        core::slice::from_raw_parts(
            tag_data as *const Ntag215File as *const u8,
            core::mem::size_of::<Ntag215File>(),
        )
    };

    let input_length = DYNAMIC_LOCK_START - UUID_START;
    let input_length2 = HMAC_TAG_START - WRITE_COUNTER_START;

    // Tag HMAC
    let tag_hmac = {
        let mut mac = HmacSha256::new_from_slice(&tag_keys.hmac_key)
            .expect("HMAC key length is always valid");
        mac.update(&tag_data_bytes[UUID_START..UUID_START + input_length]);
        let result = mac.finalize().into_bytes();
        let mut hmac: HashData = [0u8; 0x20];
        hmac.copy_from_slice(&result);
        hmac
    };
    encoded_tag_data.hmac_tag = tag_hmac;

    // Generate data HMAC (requires tag HMAC as input)
    let data_hmac = {
        let mut mac = HmacSha256::new_from_slice(&data_keys.hmac_key)
            .expect("HMAC key length is always valid");
        // Data from write_counter through HMAC_TAG_START
        mac.update(&tag_data_bytes[WRITE_COUNTER_START..WRITE_COUNTER_START + input_length2]);
        // Tag HMAC
        mac.update(&encoded_tag_data.hmac_tag);
        // UUID region
        mac.update(&tag_data_bytes[UUID_START..UUID_START + input_length]);
        let result = mac.finalize().into_bytes();
        let mut hmac: HashData = [0u8; 0x20];
        hmac.copy_from_slice(&result);
        hmac
    };
    encoded_tag_data.hmac_data = data_hmac;

    // Encrypt
    cipher(&data_keys, tag_data, &mut encoded_tag_data);

    // Convert back to hardware format
    *encrypted_tag_data = encoded_data_to_nfc_data(&encoded_tag_data);

    true
}
