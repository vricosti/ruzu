// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! AES-128-ECB decryption for single-block operations (title keys, key area keys).

use aes::cipher::{BlockDecrypt, KeyInit};
use aes::Aes128;
use cipher::generic_array::GenericArray;

use crate::key_manager::Key128;

/// Decrypt a single 16-byte block using AES-128-ECB.
///
/// Used for title key decryption (titlekek → title key) and
/// key area key decryption (key_area_key → section key).
pub fn decrypt_aes_128_ecb(key: &Key128, data: &mut Key128) {
    let cipher = Aes128::new(GenericArray::from_slice(key));
    let mut block = GenericArray::clone_from_slice(data);
    cipher.decrypt_block(&mut block);
    data.copy_from_slice(&block);
}

#[cfg(test)]
mod tests {
    use super::*;
    use aes::cipher::{BlockEncrypt, KeyInit};
    use aes::Aes128;
    use cipher::generic_array::GenericArray;

    #[test]
    fn test_ecb_round_trip() {
        let key: Key128 = [
            0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf,
            0x4f, 0x3c,
        ];
        let plaintext: Key128 = [
            0x6b, 0xc1, 0xbe, 0xe2, 0x2e, 0x40, 0x9f, 0x96, 0xe9, 0x3d, 0x7e, 0x11, 0x73, 0x93,
            0x17, 0x2a,
        ];

        // Encrypt
        let cipher = Aes128::new(GenericArray::from_slice(&key));
        let mut block = GenericArray::clone_from_slice(&plaintext);
        cipher.encrypt_block(&mut block);
        let mut ciphertext: Key128 = [0; 16];
        ciphertext.copy_from_slice(&block);
        assert_ne!(ciphertext, plaintext);

        // Decrypt with our function
        decrypt_aes_128_ecb(&key, &mut ciphertext);
        assert_eq!(ciphertext, plaintext);
    }

    #[test]
    fn test_ecb_is_not_ctr() {
        // Prove that AES-ECB decrypt != AES-CTR with zero IV
        let key: Key128 = [0x42; 16];
        let data: Key128 = [0xAB; 16];

        // ECB decrypt
        let mut ecb_result = data;
        decrypt_aes_128_ecb(&key, &mut ecb_result);

        // CTR with zero IV
        let mut ctr_result = data;
        crate::aes_ctr::decrypt_aes_ctr(&key, &[0u8; 16], &mut ctr_result);

        // They should produce DIFFERENT results
        assert_ne!(
            ecb_result, ctr_result,
            "ECB decrypt and CTR-zero-IV should differ"
        );
    }
}
