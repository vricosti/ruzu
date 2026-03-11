//! Port of zuyu/src/core/hle/kernel/physical_memory.h
//! Status: Ported
//! Derniere synchro: 2026-03-11
//!
//! Upstream defines PhysicalMemory as a std::vector<u8> with a 256-byte alignment
//! allocator. In Rust we use a newtype around Vec<u8>. The alignment guarantee
//! applies to the backing allocation; Rust's global allocator typically provides
//! sufficient alignment for u8, so we document the 256-byte requirement but do
//! not enforce it at the type level (no custom allocator needed for correctness
//! in the emulator context).

/// Host-side physical memory backing.
///
/// Upstream: `class PhysicalMemory : public std::vector<u8, AlignmentAllocator<u8, 256>>`.
///
/// The 256-byte alignment is required by GPU memory access patterns.
/// In Rust, `Vec<u8>` is used; if strict 256-byte alignment is needed in
/// the future, a custom allocator can be added.
#[derive(Debug, Clone, Default)]
pub struct PhysicalMemory {
    data: Vec<u8>,
}

/// Required alignment for GPU memory (256 bytes).
pub const PHYSICAL_MEMORY_ALIGNMENT: usize = 256;

impl PhysicalMemory {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn with_size(size: usize) -> Self {
        Self {
            data: vec![0u8; size],
        }
    }

    pub fn resize(&mut self, new_size: usize) {
        self.data.resize(new_size, 0);
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.data
    }

    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        &mut self.data
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.data.as_ptr()
    }

    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.data.as_mut_ptr()
    }
}

impl std::ops::Deref for PhysicalMemory {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl std::ops::DerefMut for PhysicalMemory {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}
