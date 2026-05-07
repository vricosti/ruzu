// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/shared_memory_holder.h and shared_memory_holder.cpp

use std::any::Any;
use std::sync::Arc;

use common::ResultCode;

use super::super::hid_result;

/// Factory trait providing kernel-backed shared memory for the HID
/// SharedMemoryHolder.
///
/// Upstream zuyu's `SharedMemoryHolder::Initialize(Core::System&)` directly
/// constructs a `Kernel::KSharedMemory` from the passed `Core::System`. ruzu's
/// `hid_core` crate cannot depend on the `core` crate (that would form a
/// dependency cycle, since `core` already depends on `hid_core`), so the
/// equivalent path is expressed as a trait that the `core` layer implements.
///
/// Semantics match upstream's call to
/// `KSharedMemory::Create + Initialize + Register`: the returned pointer is
/// the host-side mapped address of a freshly-allocated kernel shared memory
/// region of `size` bytes, zero-initialized. The returned `Arc<dyn Any>` is
/// the keepalive for the underlying `KSharedMemory` and must outlive any use
/// of the pointer; the `core` side downcasts it to recover the
/// `Arc<KSharedMemory>` handle to hand out via IPC.
pub trait KSharedMemoryBacking: Send + Sync {
    /// Allocate a kernel shared memory region of `size` bytes.
    /// Returns `(host_ptr, keepalive)` on success, `None` on allocation
    /// failure.
    fn create(&self, size: usize) -> Option<(*mut u8, Arc<dyn Any + Send + Sync>)>;
}

/// This is nn::hid::detail::SharedMemoryHolder
///
/// Upstream (shared_memory_holder.h) holds a Kernel::KSharedMemory* and a
/// SharedMemoryFormat* pointer. Initialize() calls KSharedMemory::Create +
/// Initialize + Register, then placement-constructs a SharedMemoryFormat at
/// the kernel-mapped address. Finalize() calls shared_memory->Close().
/// GetAddress() returns the SharedMemoryFormat*, GetHandle() returns the
/// Kernel::KSharedMemory*.
///
/// The Rust port stores the SharedMemoryFormat as a raw pointer into the
/// kernel-shared-memory backing storage (provided by `core` via the
/// `KSharedMemoryBacking` trait). This way, daemon writes through
/// `address_mut()` and guest reads of the mapped page reference the **same**
/// physical memory — matching upstream's pointer-share semantics. (Earlier
/// the port allocated a Box<SharedMemoryFormat> that was snapshot-copied into
/// a fresh KSharedMemory at GetSharedMemoryHandle time, which left the guest
/// with a stale zero-snapshot — see project memory entry
/// `project_mk8d_hid_shared_mem_root_cause_2026_05_02`.)
pub struct SharedMemoryHolder {
    /// Upstream: `bool is_owner`.
    is_owner: bool,
    /// Upstream: `bool is_created`.
    is_created: bool,
    /// Upstream: `bool is_mapped`.
    is_mapped: bool,
    /// Upstream: `Kernel::KSharedMemory* shared_memory`. We store the
    /// keepalive as `Arc<dyn Any>` so the `core`-side `KSharedMemory` can be
    /// kept alive without `hid_core` referencing the kernel type.
    shared_memory: Option<Arc<dyn Any + Send + Sync>>,
    /// Upstream: `SharedMemoryFormat* address`. Points into the kernel
    /// shared memory mapped region created above.
    address: *mut super::shared_memory_format::SharedMemoryFormat,
}

// SAFETY: the holder logically owns a region of memory that is guarded by the
// daemon's update mutex (resource_manager.shared_mutex). The raw pointer is
// only mutated through the (already-synchronized) `get_address_mut` access
// path, mirroring upstream's `SharedMemoryFormat*` pointer.
unsafe impl Send for SharedMemoryHolder {}
unsafe impl Sync for SharedMemoryHolder {}

impl SharedMemoryHolder {
    pub fn new() -> Self {
        Self {
            is_owner: false,
            is_created: false,
            is_mapped: false,
            shared_memory: None,
            address: std::ptr::null_mut(),
        }
    }

    /// Initialize the shared memory allocation.
    ///
    /// Upstream (shared_memory_holder.cpp Initialize) takes Core::System& and
    /// calls `KSharedMemory::Create + Initialize + Register` then
    /// `std::construct_at` a SharedMemoryFormat at the mapped pointer. The
    /// Rust port routes the kernel allocation through the `KSharedMemoryBacking`
    /// trait (implemented in the `core` crate) to avoid a circular dependency.
    pub fn initialize(&mut self, backing: &dyn KSharedMemoryBacking) -> ResultCode {
        if self.is_mapped {
            // Upstream returns success without re-allocation in this case via
            // the `if (!shared_memory.IsMapped())` gate at the call site.
            return ResultCode::SUCCESS;
        }

        let size = std::mem::size_of::<super::shared_memory_format::SharedMemoryFormat>();
        let Some((ptr, keepalive)) = backing.create(size) else {
            // Upstream: ResultSharedMemoryNotInitialized on Initialize failure.
            return hid_result::RESULT_SHARED_MEMORY_NOT_INITIALIZED;
        };
        if ptr.is_null() {
            return hid_result::RESULT_SHARED_MEMORY_NOT_INITIALIZED;
        }

        // Upstream calls `std::construct_at` on the mapped pointer to invoke
        // `SharedMemoryFormat`'s default constructor. The Rust port does not
        // implement `Default` for `SharedMemoryFormat` (it is a `repr(C)`
        // POD with many sub-structs), and the `KSharedMemory` page is
        // already zero-filled at allocation time (see
        // `core::hle::kernel::k_shared_memory::KSharedMemory::initialize`).
        // Zero-initialization matches upstream's effective post-construction
        // state for this struct.

        self.shared_memory = Some(keepalive);
        self.address = ptr as *mut super::shared_memory_format::SharedMemoryFormat;
        self.is_owner = true;
        self.is_created = true;
        self.is_mapped = true;
        ResultCode::SUCCESS
    }

    /// Release the shared memory allocation.
    ///
    /// Upstream (shared_memory_holder.cpp Finalize) calls shared_memory->Close()
    /// if address is non-null, then clears the flags. The Rust version drops
    /// the `Arc<dyn Any>` keepalive — when the last reference is released,
    /// the underlying `KSharedMemory` is closed and the page freed.
    pub fn finalize(&mut self) {
        if !self.address.is_null() {
            // The Drop impl of KSharedMemory (or whatever backing type the
            // core crate registered) will close the kernel object when the
            // last Arc reference is dropped — this is the equivalent of
            // upstream's `shared_memory->Close()`.
            self.shared_memory = None;
        }
        self.address = std::ptr::null_mut();
        self.is_created = false;
        self.is_mapped = false;
        self.is_owner = false;
    }

    /// Get a reference to the shared memory format.
    /// Upstream: `SharedMemoryFormat* GetAddress()`.
    pub fn get_address(&self) -> Option<&super::shared_memory_format::SharedMemoryFormat> {
        // SAFETY: `self.address` is non-null only when `initialize` succeeded
        // and `finalize` has not run; the pointed-to region lives as long as
        // `self.shared_memory` (the Arc keepalive). The borrow lifetime is
        // tied to `&self`.
        unsafe { self.address.as_ref() }
    }

    /// Get a mutable reference to the shared memory format.
    /// Upstream: `SharedMemoryFormat* GetAddress()`.
    pub fn get_address_mut(
        &mut self,
    ) -> Option<&mut super::shared_memory_format::SharedMemoryFormat> {
        // SAFETY: same as `get_address`. The `&mut self` borrow ensures
        // exclusive access through this path, and the daemon serializes
        // writes via `resource_manager.shared_mutex`.
        unsafe { self.address.as_mut() }
    }

    /// Returns the opaque keepalive for the kernel shared memory backing.
    /// Upstream: `Kernel::KSharedMemory* GetHandle()`.
    ///
    /// The caller (in the `core` crate) is expected to downcast this
    /// `Arc<dyn Any>` to `Arc<KSharedMemory>` to register it in the process
    /// handle table.
    pub fn get_handle(&self) -> Option<Arc<dyn Any + Send + Sync>> {
        self.shared_memory.clone()
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
