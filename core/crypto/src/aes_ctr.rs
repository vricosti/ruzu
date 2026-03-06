// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! AES-128-CTR stream decryption for NCA body sections.

use crate::key_manager::Key128;
use cipher::{KeyIvInit, StreamCipher};

type Aes128Ctr = ctr::Ctr128BE<aes::Aes128>;

/// Decrypt data in-place using AES-128-CTR.
///
/// `key`: 128-bit AES key.
/// `iv`: 128-bit initialization vector / nonce (big-endian counter).
/// `data`: buffer to decrypt in place.
pub fn decrypt_aes_ctr(key: &Key128, iv: &Key128, data: &mut [u8]) {
    let mut cipher = Aes128Ctr::new(key.into(), iv.into());
    cipher.apply_keystream(data);
}

/// Build an NCA section CTR IV from the section offset and counter.
///
/// The NCA CTR format is:
/// - Bytes [0..4]: upper 32 bits of section generation counter (big-endian)
/// - Bytes [4..8]: lower 32 bits of section generation counter (big-endian)
/// - Bytes [8..12]: upper 32 bits of (offset / 0x10) (big-endian)
/// - Bytes [12..16]: lower 32 bits of (offset / 0x10) (big-endian)
pub fn make_nca_ctr(section_ctr: u64, offset: u64) -> Key128 {
    let mut iv = [0u8; 16];
    let ctr_bytes = section_ctr.to_be_bytes();
    iv[..8].copy_from_slice(&ctr_bytes);

    let block_offset = (offset >> 4).to_be_bytes();
    iv[8..16].copy_from_slice(&block_offset);

    iv
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_aes_ctr_round_trip() {
        let key: Key128 = [
            0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf,
            0x4f, 0x3c,
        ];
        let iv: Key128 = [
            0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd,
            0xfe, 0xff,
        ];
        let plaintext = b"Hello, NCA world!";

        // Encrypt
        let mut buf = plaintext.to_vec();
        decrypt_aes_ctr(&key, &iv, &mut buf);
        assert_ne!(&buf, plaintext);

        // Decrypt (CTR is symmetric)
        decrypt_aes_ctr(&key, &iv, &mut buf);
        assert_eq!(&buf, plaintext);
    }

    #[test]
    fn test_make_nca_ctr() {
        let ctr = make_nca_ctr(0x0102030405060708, 0x100);
        assert_eq!(&ctr[..8], &[0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]);
        // 0x100 >> 4 = 0x10
        assert_eq!(
            &ctr[8..16],
            &[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10]
        );
    }

    #[test]
    fn test_aes_ctr_empty() {
        let key = [0u8; 16];
        let iv = [0u8; 16];
        let mut data = vec![];
        decrypt_aes_ctr(&key, &iv, &mut data);
        assert!(data.is_empty());
    }
}
