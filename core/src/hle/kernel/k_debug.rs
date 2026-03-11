//! Port of zuyu/src/core/hle/kernel/k_debug.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! KDebug: minimal kernel debug object. Upstream inherits from
//! KAutoObjectWithSlabHeapAndContainer<KDebug, KAutoObjectWithList>.

/// KDebug kernel object.
///
/// Currently a minimal stub matching the upstream structure, which has
/// no additional state beyond its base class.
pub struct KDebug {
    // Upstream: inherits KAutoObjectWithSlabHeapAndContainer
    // No additional fields.
}

impl KDebug {
    pub fn new() -> Self {
        Self {}
    }

    pub fn post_destroy(_arg: usize) {
        // No-op, matching upstream.
    }
}

impl Default for KDebug {
    fn default() -> Self {
        Self::new()
    }
}
