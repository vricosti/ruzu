// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! AES-128-XTS decryption for NCA headers.
//!
//! XTS mode uses two AES-128 keys (total 256 bits). The NCA `header_key` is
//! split: first 16 bytes = data key, last 16 bytes = tweak key.
//!
//! Each 0x200-byte sector is decrypted independently with a sector-number tweak.

use aes::cipher::{BlockDecrypt, BlockEncrypt, KeyInit};
use aes::Aes128;
use cipher::generic_array::GenericArray;

use crate::key_manager::Key256;

/// AES block size in bytes.
const AES_BLOCK_SIZE: usize = 16;

/// NCA header sector size for XTS.
pub const NCA_SECTOR_SIZE: usize = 0x200;

/// Decrypt data using AES-128-XTS with the given 256-bit key.
///
/// `key`: 256-bit key (first 16 bytes = data key, last 16 bytes = tweak key).
/// `sector`: Starting sector number for the tweak.
/// `sector_size`: Size of each XTS sector (typically 0x200 for NCA headers).
/// `data`: Buffer to decrypt in-place. Must be a multiple of `AES_BLOCK_SIZE`.
pub fn decrypt_aes_xts(key: &Key256, sector: u64, sector_size: usize, data: &mut [u8]) {
    let data_key = Aes128::new(GenericArray::from_slice(&key[..16]));
    let tweak_key = Aes128::new(GenericArray::from_slice(&key[16..32]));

    let num_sectors = (data.len() + sector_size - 1) / sector_size;

    for s in 0..num_sectors {
        let sec_start = s * sector_size;
        let sec_end = std::cmp::min(sec_start + sector_size, data.len());
        let sector_data = &mut data[sec_start..sec_end];

        decrypt_xts_sector(&data_key, &tweak_key, sector + s as u64, sector_data);
    }
}

/// Decrypt a single XTS sector.
fn decrypt_xts_sector(data_key: &Aes128, tweak_key: &Aes128, sector_num: u64, data: &mut [u8]) {
    let num_blocks = data.len() / AES_BLOCK_SIZE;
    if num_blocks == 0 {
        return;
    }

    // Compute initial tweak: encrypt the sector number with the tweak key
    let mut tweak = [0u8; 16];
    tweak[..8].copy_from_slice(&sector_num.to_le_bytes());
    let mut tweak_block = GenericArray::clone_from_slice(&tweak);
    tweak_key.encrypt_block(&mut tweak_block);
    tweak.copy_from_slice(&tweak_block);

    for i in 0..num_blocks {
        let block_start = i * AES_BLOCK_SIZE;
        let block = &mut data[block_start..block_start + AES_BLOCK_SIZE];

        // XOR with tweak
        xor_block(block, &tweak);

        // Decrypt with data key
        let mut cipher_block = GenericArray::clone_from_slice(block);
        data_key.decrypt_block(&mut cipher_block);
        block.copy_from_slice(&cipher_block);

        // XOR with tweak again
        xor_block(block, &tweak);

        // Multiply tweak by alpha (x in GF(2^128))
        gf128_mul_alpha(&mut tweak);
    }
}

/// XOR a 16-byte block with a tweak value.
#[inline]
fn xor_block(block: &mut [u8], tweak: &[u8; 16]) {
    for (b, t) in block.iter_mut().zip(tweak.iter()) {
        *b ^= *t;
    }
}

/// Multiply a value in GF(2^128) by alpha (x), used for XTS tweak advancement.
///
/// This is a left-shift with conditional XOR of the reduction polynomial
/// (x^128 + x^7 + x^2 + x + 1 → feedback byte 0x87).
fn gf128_mul_alpha(tweak: &mut [u8; 16]) {
    let mut carry = 0u8;
    for byte in tweak.iter_mut() {
        let new_carry = *byte >> 7;
        *byte = (*byte << 1) | carry;
        carry = new_carry;
    }
    // If there was a carry out of the MSB, XOR in the reduction polynomial
    if carry != 0 {
        tweak[0] ^= 0x87;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gf128_mul_alpha() {
        let mut tweak = [0u8; 16];
        tweak[0] = 0x01;
        gf128_mul_alpha(&mut tweak);
        assert_eq!(tweak[0], 0x02);

        // Test carry propagation
        let mut tweak2 = [0u8; 16];
        tweak2[0] = 0x80;
        gf128_mul_alpha(&mut tweak2);
        assert_eq!(tweak2[0], 0x00);
        assert_eq!(tweak2[1], 0x01);
    }

    #[test]
    fn test_gf128_mul_alpha_reduction() {
        // Carry out of byte[15] MSB → XOR with 0x87
        let mut tweak = [0u8; 16];
        tweak[15] = 0x80;
        gf128_mul_alpha(&mut tweak);
        assert_eq!(tweak[0], 0x87);
        assert_eq!(tweak[15], 0x00);
    }

    #[test]
    fn test_xts_round_trip() {
        // Create a test key and encrypt then decrypt
        let key: Key256 = [
            0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
            0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b,
            0x1c, 0x1d, 0x1e, 0x1f,
        ];

        let plaintext = vec![0x42u8; NCA_SECTOR_SIZE];
        let mut data = plaintext.clone();

        // Encrypt (XTS encrypt)
        encrypt_aes_xts(&key, 0, NCA_SECTOR_SIZE, &mut data);
        assert_ne!(data, plaintext);

        // Decrypt
        decrypt_aes_xts(&key, 0, NCA_SECTOR_SIZE, &mut data);
        assert_eq!(data, plaintext);
    }

    #[test]
    fn test_xts_multi_sector() {
        let key: Key256 = [0xAA; 32];
        let plaintext = vec![0x55u8; NCA_SECTOR_SIZE * 3];
        let mut data = plaintext.clone();

        encrypt_aes_xts(&key, 0, NCA_SECTOR_SIZE, &mut data);
        assert_ne!(data, plaintext);

        decrypt_aes_xts(&key, 0, NCA_SECTOR_SIZE, &mut data);
        assert_eq!(data, plaintext);
    }

    /// Helper: XTS encrypt (for testing round-trip only).
    fn encrypt_aes_xts(key: &Key256, sector: u64, sector_size: usize, data: &mut [u8]) {
        let data_key = Aes128::new(GenericArray::from_slice(&key[..16]));
        let tweak_key = Aes128::new(GenericArray::from_slice(&key[16..32]));

        let num_sectors = (data.len() + sector_size - 1) / sector_size;
        for s in 0..num_sectors {
            let sec_start = s * sector_size;
            let sec_end = std::cmp::min(sec_start + sector_size, data.len());
            let sector_data = &mut data[sec_start..sec_end];
            encrypt_xts_sector(&data_key, &tweak_key, sector + s as u64, sector_data);
        }
    }

    fn encrypt_xts_sector(
        data_key: &Aes128,
        tweak_key: &Aes128,
        sector_num: u64,
        data: &mut [u8],
    ) {
        let num_blocks = data.len() / AES_BLOCK_SIZE;
        if num_blocks == 0 {
            return;
        }

        let mut tweak = [0u8; 16];
        tweak[..8].copy_from_slice(&sector_num.to_le_bytes());
        let mut tweak_block = GenericArray::clone_from_slice(&tweak);
        tweak_key.encrypt_block(&mut tweak_block);
        tweak.copy_from_slice(&tweak_block);

        for i in 0..num_blocks {
            let block_start = i * AES_BLOCK_SIZE;
            let block = &mut data[block_start..block_start + AES_BLOCK_SIZE];

            xor_block(block, &tweak);

            let mut cipher_block = GenericArray::clone_from_slice(block);
            data_key.encrypt_block(&mut cipher_block);
            block.copy_from_slice(&cipher_block);

            xor_block(block, &tweak);
            gf128_mul_alpha(&mut tweak);
        }
    }
}
