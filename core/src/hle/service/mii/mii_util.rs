// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/mii_util.h
//!
//! MiiUtil: CRC-16 calculations and random value generation for Mii data.

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::time::SystemTime;

/// Simple pseudo-random state seeded from system time.
fn simple_random_bytes(buf: &mut [u8]) {
    let seed = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let mut h = DefaultHasher::new();
    seed.hash(&mut h);
    let mut state = h.finish();
    for byte in buf.iter_mut() {
        state = state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        *byte = (state >> 33) as u8;
    }
}

fn simple_random_u64() -> u64 {
    let mut buf = [0u8; 8];
    simple_random_bytes(&mut buf);
    u64::from_le_bytes(buf)
}

/// Calculate CRC-16/CCITT over the given data.
/// Matches upstream MiiUtil::CalculateCrc16.
pub fn calculate_crc16(data: &[u8]) -> u16 {
    let mut crc: i32 = 0;
    for &byte in data {
        crc ^= (byte as i32) << 8;
        for _ in 0..8 {
            crc <<= 1;
            if (crc & 0x10000) != 0 {
                crc = (crc ^ 0x1021) & 0xFFFF;
            }
        }
    }
    (crc as u16).swap_bytes()
}

/// Calculate device CRC-16 using a UUID and data size.
/// Matches upstream MiiUtil::CalculateDeviceCrc16.
pub fn calculate_device_crc16(uuid: &[u8; 16], data_size: usize) -> u16 {
    const MAGIC: i32 = 0x1021;
    let mut crc: i32 = 0;

    for &byte in uuid.iter() {
        for _ in 0..8 {
            crc <<= 1;
            if (crc & 0x10000) != 0 {
                crc ^= MAGIC;
            }
        }
        crc ^= byte as i32;
    }

    // As much as this looks wrong this is what N's does
    for _ in 0..(data_size * 8) {
        crc <<= 1;
        if (crc & 0x10000) != 0 {
            crc ^= MAGIC;
        }
    }

    (crc as u16).swap_bytes()
}

/// Generate a random RFC 4122 v4 UUID for Mii CreateId.
/// Matches upstream MiiUtil::MakeCreateId.
pub fn make_create_id() -> u128 {
    let mut uuid_bytes = [0u8; 16];
    simple_random_bytes(&mut uuid_bytes);

    // Set version 4
    uuid_bytes[6] = (uuid_bytes[6] & 0x0F) | 0x40;
    // Set variant 1
    uuid_bytes[8] = (uuid_bytes[8] & 0x3F) | 0x80;

    u128::from_le_bytes(uuid_bytes)
}

/// Get a device ID. Should be nn::settings::detail::GetMiiAuthorId().
/// Matches upstream MiiUtil::GetDeviceId (returns default UUID).
pub fn get_device_id() -> u128 {
    0
}

/// Generate a random value in [min, max].
pub fn get_random_value<T: Into<u64> + TryFrom<u64> + Copy>(min: T, max: T) -> T {
    let min_val = min.into();
    let max_val = max.into();
    if min_val >= max_val {
        return min;
    }
    let range = max_val - min_val + 1;
    let val = min_val + (simple_random_u64() % range);
    T::try_from(val).unwrap_or(min)
}

/// Check if a font region is valid for the given text.
/// Upstream always returns true (TODO: check against font tables).
pub fn is_font_region_valid(_font: super::mii_types::FontRegion, _text: &[u16]) -> bool {
    // Upstream TODO: This function needs to check against the font tables
    true
}
