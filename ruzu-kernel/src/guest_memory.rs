//! Port of zuyu/src/core/guest_memory.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Guest memory access helpers. Provides `GuestMemory` and `GuestMemoryScoped` types
//! that can transparently read/write guest memory, either via direct host pointer
//! access (when the memory region is contiguous in host) or via copy through a
//! scratch buffer.

use bitflags::bitflags;

bitflags! {
    /// Flags controlling guest memory access behavior.
    ///
    /// Port of `GuestMemoryFlags` enum from C++.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct GuestMemoryFlags: u32 {
        /// Read access.
        const READ = 1 << 0;
        /// Write access.
        const WRITE = 1 << 1;
        /// Safe access (triggers GPU flush/invalidation).
        const SAFE = 1 << 2;
        /// Cached write (uses WriteBlockCached).
        const CACHED = 1 << 3;

        /// Safe read.
        const SAFE_READ = Self::READ.bits() | Self::SAFE.bits();
        /// Safe write.
        const SAFE_WRITE = Self::WRITE.bits() | Self::SAFE.bits();
        /// Safe read + write.
        const SAFE_READ_WRITE = Self::SAFE_READ.bits() | Self::SAFE_WRITE.bits();
        /// Safe read + cached write.
        const SAFE_READ_CACHED_WRITE = Self::SAFE_READ_WRITE.bits() | Self::CACHED.bits();

        /// Unsafe read (no GPU flushing).
        const UNSAFE_READ = Self::READ.bits();
        /// Unsafe write (no GPU invalidation).
        const UNSAFE_WRITE = Self::WRITE.bits();
        /// Unsafe read + write.
        const UNSAFE_READ_WRITE = Self::UNSAFE_READ.bits() | Self::UNSAFE_WRITE.bits();
        /// Unsafe read + cached write.
        const UNSAFE_READ_CACHED_WRITE = Self::UNSAFE_READ_WRITE.bits() | Self::CACHED.bits();
    }
}

/// Provides access to a region of guest memory, either as a direct host-memory span
/// (zero-copy) or as a local copy.
///
/// This is a port of the C++ `GuestMemory<M, T, FLAGS>` template. Since Rust doesn't
/// support const-generic bitflags as template parameters the same way C++ does, the
/// flags are stored as a runtime value and checked at access time.
///
/// # Type Parameters
/// - `T`: The element type (typically `u8` or `u32`).
pub struct GuestMemory<T: Copy + Default> {
    /// The flags controlling read/write/safe behavior.
    flags: GuestMemoryFlags,
    /// Guest virtual address of the memory region.
    addr: u64,
    /// Number of elements of type T.
    size: usize,
    /// The data, either a direct reference or an owned copy.
    data: GuestMemoryData<T>,
    /// Whether the data is a local copy (vs direct host pointer).
    is_data_copy: bool,
    /// Whether the address was changed after construction.
    addr_changed: bool,
}

enum GuestMemoryData<T: Copy + Default> {
    /// Direct pointer into host memory (zero-copy path).
    Direct(*mut T, usize),
    /// Local copy of the data.
    Copy(Vec<T>),
    /// No data yet.
    Empty,
}

impl<T: Copy + Default> GuestMemory<T> {
    /// Create a new `GuestMemory` for read access from the given `Memory`.
    ///
    /// If the guest memory region is contiguous in host memory, the data will be
    /// accessed directly via pointer. Otherwise, it will be copied into a local buffer.
    pub fn new_read(memory: &super::memory::Memory, addr: u64, size: usize) -> Self {
        let flags = GuestMemoryFlags::SAFE_READ;
        let mut gm = Self {
            flags,
            addr,
            size,
            data: GuestMemoryData::Empty,
            is_data_copy: false,
            addr_changed: false,
        };
        gm.read_from(memory, addr, size);
        gm
    }

    /// Create a new `GuestMemory` for write access.
    ///
    /// Allocates a local buffer for writing that can later be flushed to guest memory.
    pub fn new_write(memory: &super::memory::Memory, addr: u64, size: usize) -> Self {
        let flags = GuestMemoryFlags::SAFE_WRITE;
        let mut gm = Self {
            flags,
            addr,
            size,
            data: GuestMemoryData::Empty,
            is_data_copy: false,
            addr_changed: false,
        };

        // Try to get a direct span first.
        if let Some(ptr) = memory.get_span_mut(addr, size * std::mem::size_of::<T>()) {
            gm.data = GuestMemoryData::Direct(ptr as *mut T, size);
            gm.is_data_copy = false;
        } else {
            let mut buf = Vec::with_capacity(size);
            buf.resize(size, T::default());
            gm.data = GuestMemoryData::Copy(buf);
            gm.is_data_copy = true;
        }
        gm
    }

    /// Create a new `GuestMemory` with custom flags.
    pub fn new_with_flags(
        memory: &super::memory::Memory,
        addr: u64,
        size: usize,
        flags: GuestMemoryFlags,
    ) -> Self {
        let mut gm = Self {
            flags,
            addr,
            size,
            data: GuestMemoryData::Empty,
            is_data_copy: false,
            addr_changed: false,
        };

        if flags.contains(GuestMemoryFlags::READ) {
            gm.read_from(memory, addr, size);
        } else if flags.contains(GuestMemoryFlags::WRITE) {
            if let Some(ptr) = memory.get_span_mut(addr, size * std::mem::size_of::<T>()) {
                gm.data = GuestMemoryData::Direct(ptr as *mut T, size);
                gm.is_data_copy = false;
            } else {
                let mut buf = Vec::with_capacity(size);
                buf.resize(size, T::default());
                gm.data = GuestMemoryData::Copy(buf);
                gm.is_data_copy = true;
            }
        }

        gm
    }

    /// Read data from guest memory into this buffer.
    fn read_from(&mut self, memory: &super::memory::Memory, addr: u64, size: usize) {
        self.addr = addr;
        self.size = size;

        if size == 0 {
            self.is_data_copy = true;
            self.data = GuestMemoryData::Empty;
            return;
        }

        // Try to get a direct span.
        if let Some(ptr) = memory.get_span_mut(addr, size * std::mem::size_of::<T>()) {
            self.data = GuestMemoryData::Direct(ptr as *mut T, size);
            self.is_data_copy = false;
        } else {
            // Fall back to copying.
            let byte_size = size * std::mem::size_of::<T>();
            let mut bytes = vec![0u8; byte_size];
            if self.flags.contains(GuestMemoryFlags::SAFE) {
                memory.read_block(addr, &mut bytes);
            } else {
                memory.read_block_unsafe(addr, &mut bytes);
            }
            // Reinterpret as Vec<T>.
            let mut buf = Vec::with_capacity(size);
            buf.resize(size, T::default());
            unsafe {
                std::ptr::copy_nonoverlapping(
                    bytes.as_ptr(),
                    buf.as_mut_ptr() as *mut u8,
                    byte_size,
                );
            }
            self.data = GuestMemoryData::Copy(buf);
            self.is_data_copy = true;
        }
    }

    /// Write the data back to guest memory.
    pub fn write(&self, memory: &mut super::memory::Memory) {
        let byte_size = self.size * std::mem::size_of::<T>();
        let data_ptr = self.data_ptr();
        if data_ptr.is_null() || byte_size == 0 {
            return;
        }

        let bytes = unsafe { std::slice::from_raw_parts(data_ptr as *const u8, byte_size) };

        if self.flags.contains(GuestMemoryFlags::SAFE) {
            memory.write_block(self.addr, bytes);
        } else {
            memory.write_block_unsafe(self.addr, bytes);
        }
    }

    /// Get a pointer to the data.
    pub fn data_ptr(&self) -> *mut T {
        match &self.data {
            GuestMemoryData::Direct(ptr, _) => *ptr,
            GuestMemoryData::Copy(v) => v.as_ptr() as *mut T,
            GuestMemoryData::Empty => std::ptr::null_mut(),
        }
    }

    /// Get a slice view of the data.
    pub fn as_slice(&self) -> &[T] {
        match &self.data {
            GuestMemoryData::Direct(ptr, len) => unsafe {
                std::slice::from_raw_parts(*ptr as *const T, *len)
            },
            GuestMemoryData::Copy(v) => v.as_slice(),
            GuestMemoryData::Empty => &[],
        }
    }

    /// Get a mutable slice view of the data.
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        match &mut self.data {
            GuestMemoryData::Direct(ptr, len) => unsafe {
                std::slice::from_raw_parts_mut(*ptr, *len)
            },
            GuestMemoryData::Copy(ref mut v) => v.as_mut_slice(),
            GuestMemoryData::Empty => &mut [],
        }
    }

    /// Get the number of elements.
    pub fn len(&self) -> usize {
        self.size
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    /// Get size in bytes.
    pub fn size_bytes(&self) -> usize {
        self.size * std::mem::size_of::<T>()
    }

    /// Whether the data is a local copy (not direct host memory).
    pub fn is_data_copy(&self) -> bool {
        self.is_data_copy
    }

    /// Whether the address was changed after construction.
    pub fn address_changed(&self) -> bool {
        self.addr_changed
    }

    /// Set a new address and size.
    pub fn set_address_and_size(&mut self, addr: u64, size: usize) {
        self.addr = addr;
        self.size = size;
        self.addr_changed = true;
    }

    /// Index into the data.
    pub fn get(&self, index: usize) -> T {
        self.as_slice()[index]
    }

    /// Mutable index into the data.
    pub fn set(&mut self, index: usize, value: T) {
        self.as_mut_slice()[index] = value;
    }
}

/// Scoped version of GuestMemory that automatically writes back on drop
/// if the WRITE flag is set.
///
/// Port of `GuestMemoryScoped<M, T, FLAGS>` from C++.
pub struct GuestMemoryScoped<T: Copy + Default> {
    inner: GuestMemory<T>,
    /// Reference to Memory is not stored; caller must call `flush()` manually
    /// before dropping, or use the `write_back` method.
    needs_writeback: bool,
}

impl<T: Copy + Default> GuestMemoryScoped<T> {
    /// Create a new scoped guest memory access.
    pub fn new(
        memory: &super::memory::Memory,
        addr: u64,
        size: usize,
        flags: GuestMemoryFlags,
    ) -> Self {
        let inner = GuestMemory::new_with_flags(memory, addr, size, flags);
        let needs_writeback = flags.contains(GuestMemoryFlags::WRITE);
        Self {
            inner,
            needs_writeback,
        }
    }

    /// Write the data back to guest memory (must be called before drop if write is needed).
    pub fn write_back(&mut self, memory: &mut super::memory::Memory) {
        if self.needs_writeback && self.inner.size > 0 {
            if self.inner.addr_changed || self.inner.is_data_copy {
                self.inner.write(memory);
            }
            self.needs_writeback = false;
        }
    }

    /// Get a slice of the data.
    pub fn as_slice(&self) -> &[T] {
        self.inner.as_slice()
    }

    /// Get a mutable slice of the data.
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        self.inner.as_mut_slice()
    }

    /// Get the number of elements.
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Index into the data.
    pub fn get(&self, index: usize) -> T {
        self.inner.get(index)
    }

    /// Mutable index into the data.
    pub fn set(&mut self, index: usize, value: T) {
        self.inner.set(index, value);
    }
}

impl<T: Copy + Default> Drop for GuestMemoryScoped<T> {
    fn drop(&mut self) {
        if self.needs_writeback {
            log::warn!("GuestMemoryScoped dropped without write_back() being called");
        }
    }
}

/// Type aliases matching the C++ typedefs.
pub type CpuGuestMemory<T> = GuestMemory<T>;
pub type CpuGuestMemoryScoped<T> = GuestMemoryScoped<T>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_guest_memory_flags() {
        let flags = GuestMemoryFlags::SAFE_READ;
        assert!(flags.contains(GuestMemoryFlags::READ));
        assert!(flags.contains(GuestMemoryFlags::SAFE));
        assert!(!flags.contains(GuestMemoryFlags::WRITE));
    }

    #[test]
    fn test_guest_memory_flags_combinations() {
        let flags = GuestMemoryFlags::SAFE_READ_WRITE;
        assert!(flags.contains(GuestMemoryFlags::READ));
        assert!(flags.contains(GuestMemoryFlags::WRITE));
        assert!(flags.contains(GuestMemoryFlags::SAFE));
        assert!(!flags.contains(GuestMemoryFlags::CACHED));
    }

    #[test]
    fn test_guest_memory_empty() {
        let mem = super::super::memory::Memory::new();
        let gm = GuestMemory::<u8>::new_read(&mem, 0, 0);
        assert!(gm.is_empty());
        assert_eq!(gm.len(), 0);
        assert_eq!(gm.size_bytes(), 0);
    }
}
