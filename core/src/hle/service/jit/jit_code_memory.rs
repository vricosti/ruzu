// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/jit/jit_code_memory.h
//! Port of zuyu/src/core/hle/service/jit/jit_code_memory.cpp
//!
//! CodeMemory — manages JIT code memory regions.

/// CodeMemory manages mapped code memory regions for JIT.
///
/// Corresponds to `CodeMemory` in upstream jit_code_memory.h / jit_code_memory.cpp.
pub struct CodeMemory {
    // TODO: mapped memory state
}

impl CodeMemory {
    pub fn new() -> Self {
        Self {}
    }
}
