// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/memory/dmnt_cheat_types.h
//! Cheat VM opcode types and definitions used by the dmnt cheat system.

/// Describes the extents of a memory region.
#[derive(Debug, Clone, Default)]
pub struct MemoryRegionExtents {
    pub base: u64,
    pub size: u64,
}

/// Metadata about the cheat process, including memory region extents and build ID.
#[derive(Debug, Clone, Default)]
pub struct CheatProcessMetadata {
    pub process_id: u64,
    pub title_id: u64,
    pub main_nso_extents: MemoryRegionExtents,
    pub heap_extents: MemoryRegionExtents,
    pub alias_extents: MemoryRegionExtents,
    pub aslr_extents: MemoryRegionExtents,
    pub main_nso_build_id: [u8; 0x20],
}

/// A single cheat definition containing a name and opcode program.
#[derive(Debug, Clone)]
pub struct CheatDefinition {
    pub readable_name: [u8; 0x40],
    pub num_opcodes: u32,
    pub opcodes: [u32; 0x100],
}

impl Default for CheatDefinition {
    fn default() -> Self {
        Self {
            readable_name: [0u8; 0x40],
            num_opcodes: 0,
            opcodes: [0u32; 0x100],
        }
    }
}

/// A cheat entry with enabled state, ID, and its definition.
#[derive(Debug, Clone, Default)]
pub struct CheatEntry {
    pub enabled: bool,
    pub cheat_id: u32,
    pub definition: CheatDefinition,
}
