// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/shared_memory_holder.h and shared_memory_holder.cpp

use common::ResultCode;

/// This is nn::hid::detail::SharedMemoryHolder
///
/// In the C++ upstream, this manages a KSharedMemory allocation for the HID shared memory format.
/// In Rust, the kernel shared memory primitives are not yet available, so this provides the
/// structural interface with stub implementations for the kernel-dependent operations.
pub struct SharedMemoryHolder {
    is_created: bool,
    is_mapped: bool,
    // TODO: shared_memory: Option<KSharedMemory>,
    // TODO: address: Option<*mut SharedMemoryFormat>,
}

impl SharedMemoryHolder {
    pub fn new() -> Self {
        Self {
            is_created: false,
            is_mapped: false,
        }
    }

    /// Initialize the shared memory allocation.
    /// In the C++ upstream, this creates a KSharedMemory, registers it with the kernel,
    /// and constructs a SharedMemoryFormat at the mapped address.
    pub fn initialize(&mut self) -> ResultCode {
        // TODO: Create KSharedMemory and map it
        // For now, mark as created/mapped to allow the applet resource flow to proceed
        self.is_created = true;
        self.is_mapped = true;
        ResultCode::SUCCESS
    }

    /// Release the shared memory allocation.
    pub fn finalize(&mut self) {
        // TODO: Close KSharedMemory
        self.is_created = false;
        self.is_mapped = false;
    }

    pub fn is_mapped(&self) -> bool {
        self.is_mapped
    }

    pub fn is_created(&self) -> bool {
        self.is_created
    }
}

impl Default for SharedMemoryHolder {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for SharedMemoryHolder {
    fn drop(&mut self) {
        self.finalize();
    }
}
