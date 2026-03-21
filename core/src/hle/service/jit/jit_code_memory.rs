// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/jit/jit_code_memory.h
//! Port of zuyu/src/core/hle/service/jit/jit_code_memory.cpp
//!
//! CodeMemory — manages JIT code memory regions.

/// CodeMemory manages mapped code memory regions for JIT.
///
/// Corresponds to `CodeMemory` in upstream jit_code_memory.h / jit_code_memory.cpp.
///
/// Upstream fields:
///   m_code_memory: Kernel::KCodeMemory* — reference to the kernel code memory object.
///   m_size: size_t — size of the mapped region.
///   m_address: u64 — virtual address where the code memory is mapped.
///   m_perm: Kernel::Svc::MemoryPermission — permission of the mapped region.
///
/// Upstream Initialize() maps code memory into the process address space at a random address
/// within the alias code region, retrying until successful. Finalize() unmaps and closes.
///
/// Blocked on KCodeMemory and KProcess page table integration. KCodeMemory exists at
/// kernel/k_code_memory.rs but MapToOwner/UnmapFromOwner are not yet wired.
pub struct CodeMemory {
    size: usize,
    address: u64,
}

impl CodeMemory {
    pub fn new() -> Self {
        Self {
            size: 0,
            address: 0,
        }
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn get_address(&self) -> u64 {
        self.address
    }
}
