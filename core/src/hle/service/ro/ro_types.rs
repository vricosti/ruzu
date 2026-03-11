// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ro/ro_types.h
//!
//! Types for the RO service.

/// NRR header structure.
///
/// Corresponds to `NRRHeader` in upstream ro_types.h.
#[repr(C)]
pub struct NrrHeader {
    pub magic: u32,
    pub key_generation: u32,
    pub reserved_08: [u8; 8],
    pub modulus: [u8; 0x100],
    pub fixed_key_signature: [u8; 0x100],
    pub nrr_signature: [u8; 0x100],
    pub program_id: u64,
    pub nrr_size: u32,
    pub nrr_kind: u8,
    pub reserved_339: [u8; 3],
    pub hash_offset: u32,
    pub hash_count: u32,
    pub reserved_344: [u8; 8],
}

/// NRR certificate header.
///
/// Corresponds to `NRRCertification` in upstream ro_types.h.
#[repr(C)]
pub struct NrrCertification {
    pub magic: u64,
    pub key_generation: u32,
    pub reserved_0c: [u8; 4],
    pub public_key: [u8; 0x100],
    pub nrr_certification_signature: [u8; 0x100],
}
