// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/crypto/aes_util.h and aes_util.cpp
//! AES encryption utilities (ECB, CBC, CTR, XTS cipher contexts).
//!
//! Uses the `aes` crate ecosystem for Rust crypto operations instead of mbedtls.

use super::key_manager::{Key128, Key256};

/// Cipher mode. Values match upstream mbedtls cipher type IDs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Mode {
    CTR = 11,
    ECB = 2,
    XTS = 70,
}

/// Encryption/decryption operation direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Encrypt,
    Decrypt,
}

/// Nintendo XTS tweak calculation: big-endian sector ID stored in 16 bytes.
fn calculate_nintendo_tweak(sector_id: usize) -> [u8; 16] {
    let mut out = [0u8; 16];
    let mut id = sector_id;
    // Fill from the end (big-endian)
    for i in (0..=0xF).rev() {
        out[i] = (id & 0xFF) as u8;
        id >>= 8;
    }
    out
}

/// Multiply tweak by x in GF(2^128) with reducing polynomial x^128 + x^7 + x^2 + x + 1.
/// This is the standard XTS tweak update step (IEEE P1619).
fn xts_multiply_tweak(tweak: &mut [u8; 16]) {
    let mut carry = 0u8;
    for byte in tweak.iter_mut() {
        let new_carry = *byte >> 7;
        *byte = (*byte << 1) | carry;
        carry = new_carry;
    }
    if carry != 0 {
        tweak[0] ^= 0x87; // Reduction polynomial
    }
}

/// Internal cipher state. We store the raw key and mode, and perform operations
/// on demand. This mirrors upstream's CipherContext which wraps mbedtls contexts.
struct CipherState {
    mode: Mode,
    key: Vec<u8>,
    iv: Vec<u8>,
}

impl CipherState {
    fn new(key: &[u8], mode: Mode) -> Self {
        Self {
            mode,
            key: key.to_vec(),
            iv: vec![0u8; 16],
        }
    }

    fn set_iv(&mut self, iv: &[u8]) {
        self.iv = iv.to_vec();
        if self.iv.len() < 16 {
            self.iv.resize(16, 0);
        }
    }

    fn transcode(&mut self, src: &[u8], dest: &mut [u8], op: Op) {
        match self.mode {
            Mode::ECB => self.transcode_ecb(src, dest, op),
            Mode::CTR => self.transcode_ctr(src, dest, op),
            Mode::XTS => self.transcode_xts(src, dest, op),
        }
    }

    fn transcode_ecb(&self, src: &[u8], dest: &mut [u8], op: Op) {
        use aes::cipher::generic_array::GenericArray;
        use aes::cipher::{BlockDecrypt, BlockEncrypt, KeyInit};

        let block_size = 16;
        let len = src.len().min(dest.len());

        match self.key.len() {
            16 => {
                let key = GenericArray::from_slice(&self.key);
                match op {
                    Op::Encrypt => {
                        let cipher = aes::Aes128::new(key);
                        for offset in (0..len).step_by(block_size) {
                            let end = (offset + block_size).min(len);
                            if end - offset < block_size {
                                // Partial block: pad, encrypt, copy back
                                let mut block = [0u8; 16];
                                block[..end - offset].copy_from_slice(&src[offset..end]);
                                let block_ga = GenericArray::from_mut_slice(&mut block);
                                cipher.encrypt_block(block_ga);
                                dest[offset..end].copy_from_slice(&block[..end - offset]);
                            } else {
                                let mut block = [0u8; 16];
                                block.copy_from_slice(&src[offset..offset + 16]);
                                let block_ga = GenericArray::from_mut_slice(&mut block);
                                cipher.encrypt_block(block_ga);
                                dest[offset..offset + 16].copy_from_slice(&block);
                            }
                        }
                    }
                    Op::Decrypt => {
                        let cipher = aes::Aes128::new(key);
                        for offset in (0..len).step_by(block_size) {
                            let end = (offset + block_size).min(len);
                            if end - offset < block_size {
                                let mut block = [0u8; 16];
                                block[..end - offset].copy_from_slice(&src[offset..end]);
                                let block_ga = GenericArray::from_mut_slice(&mut block);
                                cipher.decrypt_block(block_ga);
                                dest[offset..end].copy_from_slice(&block[..end - offset]);
                            } else {
                                let mut block = [0u8; 16];
                                block.copy_from_slice(&src[offset..offset + 16]);
                                let block_ga = GenericArray::from_mut_slice(&mut block);
                                cipher.decrypt_block(block_ga);
                                dest[offset..offset + 16].copy_from_slice(&block);
                            }
                        }
                    }
                }
            }
            32 => {
                let key = GenericArray::from_slice(&self.key);
                match op {
                    Op::Encrypt => {
                        let cipher = aes::Aes256::new(key);
                        for offset in (0..len).step_by(block_size) {
                            let end = (offset + block_size).min(len);
                            let mut block = [0u8; 16];
                            let copy_len = end - offset;
                            block[..copy_len].copy_from_slice(&src[offset..end]);
                            let block_ga = GenericArray::from_mut_slice(&mut block);
                            cipher.encrypt_block(block_ga);
                            dest[offset..end].copy_from_slice(&block[..copy_len]);
                        }
                    }
                    Op::Decrypt => {
                        let cipher = aes::Aes256::new(key);
                        for offset in (0..len).step_by(block_size) {
                            let end = (offset + block_size).min(len);
                            let mut block = [0u8; 16];
                            let copy_len = end - offset;
                            block[..copy_len].copy_from_slice(&src[offset..end]);
                            let block_ga = GenericArray::from_mut_slice(&mut block);
                            cipher.decrypt_block(block_ga);
                            dest[offset..end].copy_from_slice(&block[..copy_len]);
                        }
                    }
                }
            }
            _ => {
                log::error!("Unsupported key size for ECB: {}", self.key.len());
            }
        }
    }

    fn transcode_ctr(&mut self, src: &[u8], dest: &mut [u8], _op: Op) {
        // CTR mode is the same for encrypt and decrypt
        use aes::cipher::generic_array::GenericArray;
        use aes::cipher::{KeyIvInit, StreamCipher};

        let len = src.len().min(dest.len());
        dest[..len].copy_from_slice(&src[..len]);

        match self.key.len() {
            16 => {
                let key = GenericArray::from_slice(&self.key);
                let iv = GenericArray::from_slice(&self.iv[..16]);
                let mut cipher = ctr::Ctr128BE::<aes::Aes128>::new(key, iv);
                cipher.apply_keystream(&mut dest[..len]);
            }
            32 => {
                let key = GenericArray::from_slice(&self.key);
                let iv = GenericArray::from_slice(&self.iv[..16]);
                let mut cipher = ctr::Ctr128BE::<aes::Aes256>::new(key, iv);
                cipher.apply_keystream(&mut dest[..len]);
            }
            _ => {
                log::error!("Unsupported key size for CTR: {}", self.key.len());
            }
        }
    }

    fn transcode_xts(&mut self, src: &[u8], dest: &mut [u8], op: Op) {
        // XTS mode uses a 256-bit key split into two 128-bit keys.
        // The IV/tweak is set before calling.
        // Manual XTS-AES-128 implementation (avoids xts_mode crate dependency).
        use aes::cipher::generic_array::GenericArray;
        use aes::cipher::{BlockDecrypt, BlockEncrypt, KeyInit};

        let len = src.len().min(dest.len());
        dest[..len].copy_from_slice(&src[..len]);

        match self.key.len() {
            32 => {
                // Two 128-bit keys: key1 for data, key2 for tweak
                let key1 = GenericArray::from_slice(&self.key[..16]);
                let key2 = GenericArray::from_slice(&self.key[16..32]);
                let cipher1 = aes::Aes128::new(key1);
                let cipher2 = aes::Aes128::new(key2);

                // Encrypt tweak with key2
                let mut tweak = [0u8; 16];
                tweak.copy_from_slice(&self.iv[..16]);
                let tweak_block = GenericArray::from_mut_slice(&mut tweak);
                cipher2.encrypt_block(tweak_block);

                // Process each 16-byte block
                for chunk in dest[..len].chunks_mut(16) {
                    if chunk.len() < 16 {
                        break; // Partial block at end; XTS requires full blocks
                    }

                    // XOR with tweak
                    for i in 0..16 {
                        chunk[i] ^= tweak[i];
                    }

                    // Encrypt or decrypt
                    let block = GenericArray::from_mut_slice(chunk);
                    match op {
                        Op::Encrypt => cipher1.encrypt_block(block),
                        Op::Decrypt => cipher1.decrypt_block(block),
                    }

                    // XOR with tweak again
                    for i in 0..16 {
                        chunk[i] ^= tweak[i];
                    }

                    // Multiply tweak by x in GF(2^128) with primitive polynomial
                    xts_multiply_tweak(&mut tweak);
                }
            }
            _ => {
                log::error!("Unsupported key size for XTS: {}", self.key.len());
            }
        }
    }
}

/// AES cipher context supporting ECB, CTR, and XTS modes.
/// Port of Crypto::AESCipher<Key, KeySize>.
pub struct AesCipher {
    state: CipherState,
}

impl AesCipher {
    /// Creates a new AES cipher with a 128-bit key.
    pub fn new_128(key: Key128, mode: Mode) -> Self {
        Self {
            state: CipherState::new(&key, mode),
        }
    }

    /// Creates a new AES cipher with a 256-bit key.
    pub fn new_256(key: Key256, mode: Mode) -> Self {
        Self {
            state: CipherState::new(&key, mode),
        }
    }

    /// Sets the initialization vector / nonce.
    pub fn set_iv(&mut self, data: &[u8]) {
        self.state.set_iv(data);
    }

    /// Transcodes (encrypts or decrypts) data.
    pub fn transcode(&mut self, src: &[u8], dest: &mut [u8], op: Op) {
        self.state.transcode(src, dest, op);
    }

    /// Transcodes data in-place.
    pub fn transcode_inplace(&mut self, data: &mut [u8], op: Op) {
        // Copy to temporary buffer for in-place operation
        let src = data.to_vec();
        self.state.transcode(&src, data, op);
    }

    /// XTS transcode with sector-based tweak calculation (Nintendo format).
    pub fn xts_transcode(
        &mut self,
        src: &[u8],
        dest: &mut [u8],
        sector_id: usize,
        sector_size: usize,
        op: Op,
    ) {
        assert!(
            src.len() % sector_size == 0,
            "XTS decryption size must be a multiple of sector size."
        );
        let size = src.len().min(dest.len());
        let mut current_sector = sector_id;
        let mut offset = 0;

        while offset < size {
            let tweak = calculate_nintendo_tweak(current_sector);
            self.set_iv(&tweak);
            let end = (offset + sector_size).min(size);
            self.transcode(&src[offset..end], &mut dest[offset..end], op);
            current_sector += 1;
            offset += sector_size;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nintendo_tweak() {
        let tweak = calculate_nintendo_tweak(0);
        assert_eq!(tweak, [0u8; 16]);

        let tweak = calculate_nintendo_tweak(1);
        let mut expected = [0u8; 16];
        expected[15] = 1;
        assert_eq!(tweak, expected);
    }
}
