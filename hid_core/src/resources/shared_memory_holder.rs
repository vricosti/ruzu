// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/shared_memory_holder.h and shared_memory_holder.cpp

use common::ResultCode;

/// This is nn::hid::detail::SharedMemoryHolder
///
/// Upstream (shared_memory_holder.h) holds a Kernel::KSharedMemory* and a SharedMemoryFormat*
/// pointer. Initialize() calls KSharedMemory::Create + Initialize + Register, then
/// placement-constructs a SharedMemoryFormat at the mapped address. Finalize() calls
/// shared_memory->Close(). GetAddress() returns the SharedMemoryFormat*, GetHandle()
/// returns the KSharedMemory*.
///
/// The Rust port allocates SharedMemoryFormat on the heap via Box instead of going through
/// kernel shared memory, since the HID service runs in-process. The kernel KSharedMemory
/// handle is stored separately for use by SVC MapSharedMemory when guest processes map it.
pub struct SharedMemoryHolder {
    is_created: bool,
    is_mapped: bool,
    // Upstream: Kernel::KSharedMemory* shared_memory (from core/hle/kernel/k_shared_memory.h)
    // Upstream: SharedMemoryFormat* address (from hid_core/resources/shared_memory_format.h)
    //
    // Full integration requires passing Core::System into Initialize() to call
    // Kernel::KSharedMemory::Create(system.Kernel()) and mapping via system.DeviceMemory().
    // The SharedMemoryFormat is defined in hid_core/src/resources/shared_memory_format.rs.
    address: Option<Box<super::shared_memory_format::SharedMemoryFormat>>,
}

impl SharedMemoryHolder {
    pub fn new() -> Self {
        Self {
            is_created: false,
            is_mapped: false,
            address: None,
        }
    }

    /// Initialize the shared memory allocation.
    ///
    /// Upstream (shared_memory_holder.cpp Initialize) takes Core::System& and calls:
    ///   shared_memory = Kernel::KSharedMemory::Create(system.Kernel());
    ///   shared_memory->Initialize(system.DeviceMemory(), nullptr,
    ///       Svc::MemoryPermission::None, Svc::MemoryPermission::Read,
    ///       sizeof(SharedMemoryFormat));
    ///   Kernel::KSharedMemory::Register(system.Kernel(), shared_memory);
    ///   address = std::construct_at(reinterpret_cast<SharedMemoryFormat*>(
    ///       shared_memory->GetPointer()));
    ///
    /// This Rust implementation allocates SharedMemoryFormat on the heap. Full kernel
    /// shared memory integration (for guest SVC MapSharedMemory) requires passing
    /// Core::System and using core::hle::kernel::k_shared_memory::KSharedMemory.
    pub fn initialize(&mut self) -> ResultCode {
        // SAFETY: SharedMemoryFormat is repr(C) and zero-initialized is valid (matches upstream
        // behavior where shared memory pages are zero-filled by the kernel).
        let format: Box<super::shared_memory_format::SharedMemoryFormat> = unsafe {
            let layout = std::alloc::Layout::new::<super::shared_memory_format::SharedMemoryFormat>();
            let ptr = std::alloc::alloc_zeroed(layout)
                as *mut super::shared_memory_format::SharedMemoryFormat;
            if ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }
            Box::from_raw(ptr)
        };
        self.address = Some(format);
        self.is_created = true;
        self.is_mapped = true;
        ResultCode::SUCCESS
    }

    /// Release the shared memory allocation.
    ///
    /// Upstream (shared_memory_holder.cpp Finalize) calls shared_memory->Close()
    /// if address is non-null. The Rust version drops the Box<SharedMemoryFormat>.
    pub fn finalize(&mut self) {
        self.address = None;
        self.is_created = false;
        self.is_mapped = false;
    }

    /// Get a reference to the shared memory format.
    /// Upstream: SharedMemoryFormat* GetAddress().
    pub fn get_address(&self) -> Option<&super::shared_memory_format::SharedMemoryFormat> {
        self.address.as_deref()
    }

    /// Get a mutable reference to the shared memory format.
    /// Upstream: SharedMemoryFormat* GetAddress().
    pub fn get_address_mut(&mut self) -> Option<&mut super::shared_memory_format::SharedMemoryFormat> {
        self.address.as_deref_mut()
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
