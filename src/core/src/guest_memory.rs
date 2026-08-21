//! Port of eden/src/core/guest_memory.h
//! Status: COMPLET
//! Derniere synchro: 2026-08-17
//!
//! GuestMemory and GuestMemoryScoped types for safe access to guest (emulated)
//! memory. The C++ version uses templates parameterized on a memory interface M,
//! element type T, and GuestMemoryFlags. In Rust we use generics with a trait
//! for the memory interface.

use bitflags::bitflags;
use common::scratch_buffer::ScratchBuffer;
use std::marker::PhantomData;

bitflags! {
    /// Flags controlling guest memory access behavior.
    /// Matches upstream `GuestMemoryFlags` enum.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct GuestMemoryFlags: u32 {
        const READ   = 1 << 0;
        const WRITE  = 1 << 1;
        const SAFE   = 1 << 2;
        const CACHED = 1 << 3;

        const SAFE_READ              = Self::READ.bits() | Self::SAFE.bits();
        const SAFE_WRITE             = Self::WRITE.bits() | Self::SAFE.bits();
        const SAFE_READ_WRITE        = Self::SAFE_READ.bits() | Self::SAFE_WRITE.bits();
        const SAFE_READ_CACHED_WRITE = Self::SAFE_READ_WRITE.bits() | Self::CACHED.bits();

        const UNSAFE_READ              = Self::READ.bits();
        const UNSAFE_WRITE             = Self::WRITE.bits();
        const UNSAFE_READ_WRITE        = Self::UNSAFE_READ.bits() | Self::UNSAFE_WRITE.bits();
        const UNSAFE_READ_CACHED_WRITE = Self::UNSAFE_READ_WRITE.bits() | Self::CACHED.bits();
    }
}

/// Trait abstracting the memory interface that GuestMemory operates on.
/// This corresponds to the template parameter M in the C++ code, which requires
/// methods like GetSpan, ReadBlock, WriteBlock, etc.
pub trait GuestMemoryInterface {
    /// Whether this memory interface has flush/invalidation support.
    /// Corresponds to `M::HAS_FLUSH_INVALIDATION` in C++.
    const HAS_FLUSH_INVALIDATION: bool;

    /// Try to get a direct pointer to the given device address range.
    /// Returns None if the range is not directly accessible.
    fn get_span(&self, addr: u64, size: usize) -> Option<*mut u8>;

    /// Read a block of memory (safe path, may trigger page tracking).
    fn read_block(&self, addr: u64, dest: *mut u8, size: usize);

    /// Read a block of memory (unsafe path, no page tracking).
    fn read_block_unsafe(&self, addr: u64, dest: *mut u8, size: usize);

    /// Write a block of memory (safe path).
    fn write_block(&self, addr: u64, src: *const u8, size: usize);

    /// Write a block of memory (unsafe path).
    fn write_block_unsafe(&self, addr: u64, src: *const u8, size: usize);

    /// Write a block of memory through the cache.
    fn write_block_cached(&self, addr: u64, src: *const u8, size: usize);

    /// Flush a region of memory.
    fn flush_region(&self, addr: u64, size: usize);

    /// Invalidate a region of memory (notify GPU of CPU writes).
    fn invalidate_region(&self, addr: u64, size: usize);
}

/// Guest memory accessor providing read/write access to emulated memory.
///
/// Corresponds to the C++ `GuestMemory<M, T, FLAGS>` template.
/// This type attempts to get a direct span to the memory region; if that fails,
/// it falls back to copying data through ReadBlock/WriteBlock.
///
/// The FLAGS parameter is a runtime value here (since Rust doesn't have const
/// generics for bitflags). The caller is responsible for passing consistent flags.
pub struct GuestMemory<'memory, 'backup, M: GuestMemoryInterface, T: Copy + Default + Clone> {
    memory: &'memory M,
    addr: u64,
    size: usize,
    flags: GuestMemoryFlags,
    /// When we have a direct span, this points into the memory backend.
    /// When we have a copy, this points into data_copy.
    data_ptr: *mut T,
    data_copy: Vec<T>,
    span_valid: bool,
    is_data_copy: bool,
    addr_changed: bool,
    _backup: PhantomData<&'backup mut ScratchBuffer<T>>,
}

impl<'memory, M: GuestMemoryInterface, T: Copy + Default + Clone>
    GuestMemory<'memory, 'static, M, T>
{
    /// Create a new GuestMemory accessor.
    ///
    /// If FLAGS includes READ, the data is immediately read from guest memory.
    /// If FLAGS is write-only, a buffer is allocated for the caller to fill.
    pub fn new(memory: &'memory M, addr: u64, size: usize, flags: GuestMemoryFlags) -> Self {
        Self::new_impl(memory, addr, size, flags, None)
    }
}

impl<'memory, 'backup, M: GuestMemoryInterface, T: Copy + Default + Clone>
    GuestMemory<'memory, 'backup, M, T>
{
    /// Create an accessor using the caller-owned upstream scratch buffer when
    /// the guest range cannot be exposed as one contiguous host span.
    pub fn new_with_backup(
        memory: &'memory M,
        addr: u64,
        size: usize,
        flags: GuestMemoryFlags,
        backup: &'backup mut ScratchBuffer<T>,
    ) -> Self {
        Self::new_impl(memory, addr, size, flags, Some(backup))
    }

    fn new_impl(
        memory: &'memory M,
        addr: u64,
        size: usize,
        flags: GuestMemoryFlags,
        backup: Option<&'backup mut ScratchBuffer<T>>,
    ) -> Self {
        assert!(
            flags.contains(GuestMemoryFlags::READ) || flags.contains(GuestMemoryFlags::WRITE),
            "GuestMemory must have at least READ or WRITE flag"
        );

        let mut gm = GuestMemory {
            memory,
            addr,
            size,
            flags,
            data_ptr: core::ptr::null_mut(),
            data_copy: Vec::new(),
            span_valid: false,
            is_data_copy: false,
            addr_changed: false,
            _backup: PhantomData,
        };

        if !flags.contains(GuestMemoryFlags::READ) {
            // Write-only path: allocate a buffer
            if !gm.try_set_span() {
                gm.set_copy_storage(size, backup);
                gm.span_valid = true;
                gm.is_data_copy = true;
            }
        } else {
            // Read path
            gm.read_impl(addr, size, backup);
        }

        gm
    }

    pub fn data(&self) -> *const T {
        self.data_ptr as *const T
    }

    pub fn data_mut(&mut self) -> *mut T {
        self.data_ptr
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn size_bytes(&self) -> usize {
        self.size * core::mem::size_of::<T>()
    }

    /// Get a slice view of the data.
    ///
    /// # Safety
    /// The data pointer must be valid and the span must have been initialized.
    pub unsafe fn as_slice(&self) -> &[T] {
        if self.size == 0 {
            return &[];
        }
        core::slice::from_raw_parts(self.data_ptr as *const T, self.size)
    }

    /// Get a mutable slice view of the data.
    ///
    /// # Safety
    /// The data pointer must be valid and the span must have been initialized.
    pub unsafe fn as_mut_slice(&mut self) -> &mut [T] {
        if self.size == 0 {
            return &mut [];
        }
        core::slice::from_raw_parts_mut(self.data_ptr, self.size)
    }

    /// Update the address and size for a subsequent read/write.
    pub fn set_address_and_size(&mut self, addr: u64, size: usize) {
        self.addr = addr;
        self.size = size;
        self.addr_changed = true;
    }

    /// Read data from the given guest address into this accessor.
    pub fn read(&mut self, addr: u64, size: usize) {
        self.read_impl(addr, size, None);
    }

    /// Read using a caller-owned scratch buffer for the non-contiguous fallback.
    pub fn read_with_backup(
        &mut self,
        addr: u64,
        size: usize,
        backup: &'backup mut ScratchBuffer<T>,
    ) {
        self.read_impl(addr, size, Some(backup));
    }

    fn read_impl(&mut self, addr: u64, size: usize, backup: Option<&'backup mut ScratchBuffer<T>>) {
        self.addr = addr;
        self.size = size;

        if self.size == 0 {
            self.is_data_copy = true;
            return;
        }

        if self.try_set_span() {
            if self.flags.contains(GuestMemoryFlags::SAFE) && M::HAS_FLUSH_INVALIDATION {
                self.memory.flush_region(self.addr, self.size_bytes());
            }
        } else {
            self.set_copy_storage(self.size, backup);
            self.is_data_copy = true;
            self.span_valid = true;

            if self.flags.contains(GuestMemoryFlags::SAFE) {
                self.memory
                    .read_block(self.addr, self.data_ptr as *mut u8, self.size_bytes());
            } else {
                self.memory.read_block_unsafe(
                    self.addr,
                    self.data_ptr as *mut u8,
                    self.size_bytes(),
                );
            }
        }
    }

    fn set_copy_storage(&mut self, size: usize, backup: Option<&'backup mut ScratchBuffer<T>>) {
        if let Some(backup) = backup {
            backup.resize_destructive(size);
            self.data_ptr = backup.data_mut().as_mut_ptr();
        } else {
            self.data_copy.resize(size, T::default());
            self.data_ptr = self.data_copy.as_mut_ptr();
        }
    }

    /// Write data to guest memory from the given slice.
    pub fn write(&self, write_data: &[T]) {
        let src = write_data.as_ptr() as *const u8;
        if self.flags.contains(GuestMemoryFlags::CACHED) {
            self.memory
                .write_block_cached(self.addr, src, self.size_bytes());
        } else if self.flags.contains(GuestMemoryFlags::SAFE) {
            self.memory.write_block(self.addr, src, self.size_bytes());
        } else {
            self.memory
                .write_block_unsafe(self.addr, src, self.size_bytes());
        }
    }

    /// Try to get a direct span pointer to guest memory.
    fn try_set_span(&mut self) -> bool {
        if let Some(ptr) = self.memory.get_span(self.addr, self.size_bytes()) {
            self.data_ptr = ptr as *mut T;
            self.span_valid = true;
            self.is_data_copy = false;
            true
        } else {
            false
        }
    }

    /// Whether we're working with a copy of the data (vs direct span).
    pub fn is_data_copy(&self) -> bool {
        self.is_data_copy
    }

    /// Whether the address was changed after construction.
    pub fn address_changed(&self) -> bool {
        self.addr_changed
    }
}

/// Scoped guest memory accessor that automatically writes back on drop.
///
/// Corresponds to the C++ `GuestMemoryScoped<M, T, FLAGS>` template.
/// When the FLAGS include WRITE, the destructor writes the data back to guest
/// memory if the data was copied or the address was changed.
pub struct GuestMemoryScoped<'memory, 'backup, M: GuestMemoryInterface, T: Copy + Default + Clone> {
    inner: GuestMemory<'memory, 'backup, M, T>,
}

impl<'memory, M: GuestMemoryInterface, T: Copy + Default + Clone>
    GuestMemoryScoped<'memory, 'static, M, T>
{
    /// Create a new scoped guest memory accessor.
    pub fn new(memory: &'memory M, addr: u64, size: usize, flags: GuestMemoryFlags) -> Self {
        Self {
            inner: GuestMemory::new(memory, addr, size, flags),
        }
    }
}

impl<'memory, 'backup, M: GuestMemoryInterface, T: Copy + Default + Clone>
    GuestMemoryScoped<'memory, 'backup, M, T>
{
    pub fn new_with_backup(
        memory: &'memory M,
        addr: u64,
        size: usize,
        flags: GuestMemoryFlags,
        backup: &'backup mut ScratchBuffer<T>,
    ) -> Self {
        Self {
            inner: GuestMemory::new_with_backup(memory, addr, size, flags, backup),
        }
    }

    pub fn data(&self) -> *const T {
        self.inner.data()
    }

    pub fn data_mut(&mut self) -> *mut T {
        self.inner.data_mut()
    }

    pub fn size(&self) -> usize {
        self.inner.size()
    }

    pub fn size_bytes(&self) -> usize {
        self.inner.size_bytes()
    }

    /// # Safety
    /// The data pointer must be valid.
    pub unsafe fn as_slice(&self) -> &[T] {
        self.inner.as_slice()
    }

    /// # Safety
    /// The data pointer must be valid.
    pub unsafe fn as_mut_slice(&mut self) -> &mut [T] {
        self.inner.as_mut_slice()
    }
}

impl<'memory, 'backup, M: GuestMemoryInterface, T: Copy + Default + Clone> Drop
    for GuestMemoryScoped<'memory, 'backup, M, T>
{
    fn drop(&mut self) {
        if !self.inner.flags.contains(GuestMemoryFlags::WRITE) {
            return;
        }

        if self.inner.size == 0 {
            return;
        }

        if self.inner.addr_changed || self.inner.is_data_copy {
            assert!(self.inner.span_valid);
            let src = self.inner.data_ptr as *const u8;
            let size = self.inner.size_bytes();
            if self.inner.flags.contains(GuestMemoryFlags::CACHED) {
                self.inner
                    .memory
                    .write_block_cached(self.inner.addr, src, size);
            } else if self.inner.flags.contains(GuestMemoryFlags::SAFE) {
                self.inner.memory.write_block(self.inner.addr, src, size);
            } else {
                self.inner
                    .memory
                    .write_block_unsafe(self.inner.addr, src, size);
            }
        } else if self.inner.flags.contains(GuestMemoryFlags::SAFE)
            || self.inner.flags.contains(GuestMemoryFlags::CACHED)
        {
            self.inner
                .memory
                .invalidate_region(self.inner.addr, self.inner.size_bytes());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::{Cell, UnsafeCell};

    struct TestMemory {
        bytes: UnsafeCell<Vec<u8>>,
        direct: bool,
        read_calls: Cell<usize>,
        flush_calls: Cell<usize>,
    }

    impl TestMemory {
        fn new(words: &[u32], direct: bool) -> Self {
            let bytes = words.iter().flat_map(|word| word.to_ne_bytes()).collect();
            Self {
                bytes: UnsafeCell::new(bytes),
                direct,
                read_calls: Cell::new(0),
                flush_calls: Cell::new(0),
            }
        }
    }

    impl GuestMemoryInterface for TestMemory {
        const HAS_FLUSH_INVALIDATION: bool = true;

        fn get_span(&self, addr: u64, size: usize) -> Option<*mut u8> {
            if !self.direct {
                return None;
            }
            let bytes = unsafe { &mut *self.bytes.get() };
            (addr as usize + size <= bytes.len())
                .then(|| unsafe { bytes.as_mut_ptr().add(addr as usize) })
        }

        fn read_block(&self, addr: u64, dest: *mut u8, size: usize) {
            self.read_calls.set(self.read_calls.get() + 1);
            let bytes = unsafe { &*self.bytes.get() };
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr().add(addr as usize), dest, size);
            }
        }

        fn read_block_unsafe(&self, addr: u64, dest: *mut u8, size: usize) {
            self.read_block(addr, dest, size);
        }

        fn write_block(&self, _addr: u64, _src: *const u8, _size: usize) {}

        fn write_block_unsafe(&self, _addr: u64, _src: *const u8, _size: usize) {}

        fn write_block_cached(&self, _addr: u64, _src: *const u8, _size: usize) {}

        fn flush_region(&self, _addr: u64, _size: usize) {
            self.flush_calls.set(self.flush_calls.get() + 1);
        }

        fn invalidate_region(&self, _addr: u64, _size: usize) {}
    }

    #[test]
    fn contiguous_safe_read_uses_span_and_flushes_without_touching_backup() {
        let memory = TestMemory::new(&[0x1234_5678, 0x90ab_cdef], true);
        let mut backup = ScratchBuffer::<u32>::new();
        let guest =
            GuestMemory::new_with_backup(&memory, 0, 2, GuestMemoryFlags::SAFE_READ, &mut backup);

        assert!(!guest.is_data_copy());
        assert_eq!(unsafe { guest.as_slice() }, &[0x1234_5678, 0x90ab_cdef]);
        assert_eq!(memory.read_calls.get(), 0);
        assert_eq!(memory.flush_calls.get(), 1);
        drop(guest);
        assert_eq!(backup.size(), 0);
    }

    #[test]
    fn non_contiguous_read_uses_and_reuses_caller_backup() {
        let memory = TestMemory::new(&[0x1234_5678, 0x90ab_cdef], false);
        let mut backup = ScratchBuffer::<u32>::with_capacity(4);
        let capacity = backup.capacity();
        let guest =
            GuestMemory::new_with_backup(&memory, 0, 2, GuestMemoryFlags::UNSAFE_READ, &mut backup);

        assert!(guest.is_data_copy());
        assert_eq!(unsafe { guest.as_slice() }, &[0x1234_5678, 0x90ab_cdef]);
        assert_eq!(memory.read_calls.get(), 1);
        drop(guest);
        assert_eq!(backup.size(), 2);
        assert_eq!(backup.capacity(), capacity);
    }
}
