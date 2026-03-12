// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ro/ro_types.h
//!
//! Types for the RO service.

/// NRR kind — user or JIT plugin.
///
/// Corresponds to `NrrKind` in upstream ro_types.h.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NrrKind {
    User = 0,
    JitPlugin = 1,
}

/// Module ID — 0x20 bytes identifying an NRO module.
///
/// Corresponds to `ModuleId` in upstream ro_types.h.
pub const MODULE_ID_SIZE: usize = 0x20;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ModuleId {
    pub data: [u8; MODULE_ID_SIZE],
}
const _: () = assert!(std::mem::size_of::<ModuleId>() == MODULE_ID_SIZE);

impl Default for ModuleId {
    fn default() -> Self {
        Self {
            data: [0u8; MODULE_ID_SIZE],
        }
    }
}

/// NRR certification header.
///
/// Corresponds to `NrrCertification` in upstream ro_types.h.
pub const NRR_CERT_RSA_KEY_SIZE: usize = 0x100;
pub const NRR_CERT_SIGNED_SIZE: usize = 0x120;

#[repr(C)]
pub struct NrrCertification {
    pub program_id_mask: u64,
    pub program_id_pattern: u64,
    pub reserved_10: [u8; 0x10],
    pub modulus: [u8; NRR_CERT_RSA_KEY_SIZE],
    pub signature: [u8; NRR_CERT_RSA_KEY_SIZE],
}
const _: () = assert!(
    std::mem::size_of::<NrrCertification>() == NRR_CERT_RSA_KEY_SIZE + NRR_CERT_SIGNED_SIZE
);

/// NRR header structure.
///
/// Corresponds to `NrrHeader` in upstream ro_types.h.
pub const NRR_MAGIC: u32 = u32::from_le_bytes([b'N', b'R', b'R', b'0']);

#[repr(C)]
pub struct NrrHeader {
    pub magic: u32,
    pub key_generation: u32,
    pub _reserved_08: [u8; 8],
    pub certification: NrrCertification,
    pub signature: [u8; 0x100],
    pub program_id: u64,
    pub size: u32,
    pub nrr_kind: u8,
    pub _reserved_339: [u8; 3],
    pub hashes_offset: u32,
    pub num_hashes: u32,
    pub _reserved_344: [u8; 8],
}
const _: () = assert!(std::mem::size_of::<NrrHeader>() == 0x350);

impl NrrHeader {
    pub fn is_magic_valid(&self) -> bool {
        self.magic == NRR_MAGIC
    }

    pub fn is_program_id_valid(&self) -> bool {
        (self.program_id & self.certification.program_id_mask)
            == self.certification.program_id_pattern
    }

    pub fn get_nrr_kind(&self) -> NrrKind {
        match self.nrr_kind {
            0 => NrrKind::User,
            1 => NrrKind::JitPlugin,
            _ => panic!("Invalid NrrKind: {}", self.nrr_kind),
        }
    }

    pub fn get_program_id(&self) -> u64 {
        self.program_id
    }

    pub fn get_size(&self) -> u32 {
        self.size
    }

    pub fn get_num_hashes(&self) -> u32 {
        self.num_hashes
    }

    pub fn get_hashes_offset(&self) -> usize {
        self.hashes_offset as usize
    }

    pub fn get_key_generation(&self) -> u32 {
        self.key_generation
    }

    pub fn get_certification_signature(&self) -> &[u8; NRR_CERT_RSA_KEY_SIZE] {
        &self.certification.signature
    }

    pub fn get_certification_modulus(&self) -> &[u8; NRR_CERT_RSA_KEY_SIZE] {
        &self.certification.modulus
    }

    pub fn get_signature(&self) -> &[u8; 0x100] {
        &self.signature
    }
}

/// NRO header structure.
///
/// Corresponds to `NroHeader` in upstream ro_types.h.
pub const NRO_MAGIC: u32 = u32::from_le_bytes([b'N', b'R', b'O', b'0']);

#[repr(C)]
pub struct NroHeader {
    pub entrypoint_insn: u32,
    pub mod_offset: u32,
    pub _reserved_08: [u8; 0x8],
    pub magic: u32,
    pub _reserved_14: [u8; 0x4],
    pub size: u32,
    pub _reserved_1c: [u8; 0x4],
    pub text_offset: u32,
    pub text_size: u32,
    pub ro_offset: u32,
    pub ro_size: u32,
    pub rw_offset: u32,
    pub rw_size: u32,
    pub bss_size: u32,
    pub _reserved_3c: [u8; 0x4],
    pub module_id: ModuleId,
    pub _reserved_60: [u8; 0x20],
}
const _: () = assert!(std::mem::size_of::<NroHeader>() == 0x80);

impl NroHeader {
    pub fn is_magic_valid(&self) -> bool {
        self.magic == NRO_MAGIC
    }

    pub fn get_size(&self) -> u32 {
        self.size
    }

    pub fn get_text_offset(&self) -> u32 {
        self.text_offset
    }

    pub fn get_text_size(&self) -> u32 {
        self.text_size
    }

    pub fn get_ro_offset(&self) -> u32 {
        self.ro_offset
    }

    pub fn get_ro_size(&self) -> u32 {
        self.ro_size
    }

    pub fn get_rw_offset(&self) -> u32 {
        self.rw_offset
    }

    pub fn get_rw_size(&self) -> u32 {
        self.rw_size
    }

    pub fn get_bss_size(&self) -> u32 {
        self.bss_size
    }

    pub fn get_module_id(&self) -> &ModuleId {
        &self.module_id
    }
}
