// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nfc/mifare_types.h

/// Mifare command types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum MifareCmd {
    None = 0x00,
    Read = 0x30,
    AuthA = 0x60,
    AuthB = 0x61,
    Write = 0xA0,
    Transfer = 0xB0,
    Decrement = 0xC0,
    Increment = 0xC1,
    Store = 0xC2,
}

pub type DataBlock = [u8; 0x10];
pub type KeyData = [u8; 0x6];

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SectorKey {
    pub command: MifareCmd,
    pub unknown: u8, // Usually 1
    pub _padding1: [u8; 0x6],
    pub sector_key: KeyData,
    pub _padding2: [u8; 0x2],
}

const _: () = assert!(core::mem::size_of::<SectorKey>() == 0x10);

/// This is nn::nfc::MifareReadBlockParameter
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MifareReadBlockParameter {
    pub sector_number: u8,
    pub _padding: [u8; 0x7],
    pub sector_key: SectorKey,
}

const _: () = assert!(core::mem::size_of::<MifareReadBlockParameter>() == 0x18);

/// This is nn::nfc::MifareReadBlockData
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MifareReadBlockData {
    pub data: DataBlock,
    pub sector_number: u8,
    pub _padding: [u8; 0x7],
}

const _: () = assert!(core::mem::size_of::<MifareReadBlockData>() == 0x18);

/// This is nn::nfc::MifareWriteBlockParameter
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MifareWriteBlockParameter {
    pub data: DataBlock,
    pub sector_number: u8,
    pub _padding: [u8; 0x7],
    pub sector_key: SectorKey,
}

const _: () = assert!(core::mem::size_of::<MifareWriteBlockParameter>() == 0x28);

impl Default for SectorKey {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

impl Default for MifareReadBlockParameter {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

impl Default for MifareReadBlockData {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

impl Default for MifareWriteBlockParameter {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}
