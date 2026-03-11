//! Port of zuyu/src/core/hle/kernel/k_dynamic_resource_manager.h
//! Status: Stubbed
//! Derniere synchro: 2026-03-11
//!
//! Template-based dynamic resource managers for slab-allocated kernel objects.
//! Upstream uses C++ templates; here we use concrete types.

/// Port of Kernel::KBlockInfoManager (KDynamicResourceManager<KBlockInfo>).
///
/// Stubbed: depends on KDynamicSlabHeap and KDynamicPageManager.
pub struct KBlockInfoManager {
    used: usize,
    peak: usize,
    count: usize,
}

impl KBlockInfoManager {
    pub fn new() -> Self {
        Self {
            used: 0,
            peak: 0,
            count: 0,
        }
    }

    pub fn get_size(&self) -> usize {
        0
    }
    pub fn get_used(&self) -> usize {
        self.used
    }
    pub fn get_peak(&self) -> usize {
        self.peak
    }
    pub fn get_count(&self) -> usize {
        self.count
    }
}

impl Default for KBlockInfoManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Port of Kernel::KMemoryBlockSlabManager (KDynamicResourceManager<KMemoryBlock>).
///
/// Stubbed.
pub struct KMemoryBlockSlabManager {
    used: usize,
    peak: usize,
    count: usize,
}

impl KMemoryBlockSlabManager {
    pub fn new() -> Self {
        Self {
            used: 0,
            peak: 0,
            count: 0,
        }
    }

    pub fn get_size(&self) -> usize {
        0
    }
    pub fn get_used(&self) -> usize {
        self.used
    }
    pub fn get_peak(&self) -> usize {
        self.peak
    }
    pub fn get_count(&self) -> usize {
        self.count
    }
}

impl Default for KMemoryBlockSlabManager {
    fn default() -> Self {
        Self::new()
    }
}
