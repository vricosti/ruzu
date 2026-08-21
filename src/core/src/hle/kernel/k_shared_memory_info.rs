//! Port of zuyu/src/core/hle/kernel/k_shared_memory_info.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! KSharedMemoryInfo: reference-counted tracking node for shared memory
//! attached to a process.

/// Tracks a reference to a KSharedMemory attached to a process.
///
/// Upstream this is a slab-allocated intrusive list node. Here we use a
/// plain struct; the intrusive list uses Vec instead of slab-allocated nodes.
pub struct KSharedMemoryInfo {
    /// Index or identifier of the associated shared memory (placeholder for pointer).
    m_shared_memory: usize,
    m_reference_count: usize,
}

impl KSharedMemoryInfo {
    pub fn new() -> Self {
        Self {
            m_shared_memory: 0,
            m_reference_count: 0,
        }
    }

    pub fn initialize(&mut self, shared_memory: usize) {
        self.m_shared_memory = shared_memory;
        self.m_reference_count = 0;
    }

    pub fn get_shared_memory(&self) -> usize {
        self.m_shared_memory
    }

    pub fn open(&mut self) {
        self.m_reference_count += 1;
        assert!(self.m_reference_count > 0);
    }

    /// Returns true if the reference count reached zero.
    pub fn close(&mut self) -> bool {
        assert!(self.m_reference_count > 0);
        self.m_reference_count -= 1;
        self.m_reference_count == 0
    }
}

impl Default for KSharedMemoryInfo {
    fn default() -> Self {
        Self::new()
    }
}
