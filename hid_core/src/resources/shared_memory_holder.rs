// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/shared_memory_holder.h and shared_memory_holder.cpp

use common::ResultCode;

/// This is nn::hid::detail::SharedMemoryHolder
pub struct SharedMemoryHolder {
    is_owner: bool,
    is_created: bool,
    is_mapped: bool,
    // TODO: shared_memory handle and address
}

impl SharedMemoryHolder {
    pub fn new() -> Self {
        Self {
            is_owner: false,
            is_created: false,
            is_mapped: false,
        }
    }

    pub fn initialize(&mut self) -> ResultCode {
        todo!()
    }

    pub fn finalize(&mut self) {
        todo!()
    }

    pub fn is_mapped(&self) -> bool {
        self.is_mapped
    }
}

impl Default for SharedMemoryHolder {
    fn default() -> Self {
        Self::new()
    }
}
