// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_utility.h / .cpp

/// Add a value to a big-endian counter stored in a byte buffer.
/// Corresponds to upstream `FileSys::AddCounter`.
pub fn add_counter(counter: &mut [u8], value: u64) {
    let mut carry = value;
    for byte in counter.iter_mut().rev() {
        let sum = (*byte as u64) + (carry & 0xFF);
        *byte = sum as u8;
        carry = (carry >> 8) + (sum >> 8);
        if carry == 0 {
            break;
        }
    }
}
