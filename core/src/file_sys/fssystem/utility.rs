// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/file_sys/fssystem/fssystem_utility.h and .cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-12
//!
//! Utility functions for the fssystem layer.

/// Add a value to a big-endian counter stored in a byte buffer.
///
/// Corresponds to upstream `FileSys::AddCounter`.
///
/// The counter is stored in big-endian byte order. This function adds
/// `value` to the counter, propagating carries from the least significant
/// byte (last element) to the most significant byte (first element).
pub fn add_counter(counter: &mut [u8], value: u64) {
    let counter_size = counter.len();
    let mut remaining = value;
    let mut carry: u8 = 0;

    for i in 0..counter_size {
        let idx = counter_size - 1 - i;
        let sum = counter[idx] as u64 + (remaining & 0xFF) + carry as u64;
        carry = (sum >> 8) as u8;
        counter[idx] = (sum & 0xFF) as u8;
        remaining >>= 8;
        if carry == 0 && remaining == 0 {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_counter_simple() {
        let mut counter = [0u8; 16];
        add_counter(&mut counter, 1);
        assert_eq!(counter[15], 1);
    }

    #[test]
    fn test_add_counter_carry() {
        let mut counter = [0u8; 16];
        counter[15] = 0xFF;
        add_counter(&mut counter, 1);
        assert_eq!(counter[15], 0);
        assert_eq!(counter[14], 1);
    }

    #[test]
    fn test_add_counter_large() {
        let mut counter = [0u8; 16];
        add_counter(&mut counter, 0x1234);
        assert_eq!(counter[15], 0x34);
        assert_eq!(counter[14], 0x12);
    }

    #[test]
    fn test_add_counter_overflow() {
        let mut counter = [0xFF; 4];
        add_counter(&mut counter, 1);
        assert_eq!(counter, [0x00, 0x00, 0x00, 0x00]);
    }
}
