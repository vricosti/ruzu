//! Port of zuyu/src/core/hle/kernel/k_affinity_mask.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! KAffinityMask — bitmask representing CPU core affinity for threads.
//! Mirrors upstream `Kernel::KAffinityMask`.

use crate::hardware_properties::NUM_CPU_CORES;

/// Bitmask covering all allowed cores.
const ALLOWED_AFFINITY_MASK: u64 = (1u64 << NUM_CPU_CORES) - 1;

/// KAffinityMask — stores a bitmask of allowed CPU cores.
///
/// Mirrors upstream `Kernel::KAffinityMask`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct KAffinityMask {
    m_mask: u64,
}

impl KAffinityMask {
    pub const fn new() -> Self {
        Self { m_mask: 0 }
    }

    /// Get the raw affinity mask.
    pub const fn get_affinity_mask(&self) -> u64 {
        self.m_mask
    }

    /// Set the raw affinity mask. Asserts that only valid core bits are set.
    pub fn set_affinity_mask(&mut self, new_mask: u64) {
        debug_assert!((new_mask & !ALLOWED_AFFINITY_MASK) == 0);
        self.m_mask = new_mask;
    }

    /// Check if a specific core is included in the affinity mask.
    pub const fn get_affinity(&self, core: i32) -> bool {
        (self.m_mask & Self::get_core_bit(core)) != 0
    }

    /// Set or clear affinity for a specific core.
    pub fn set_affinity(&mut self, core: i32, set: bool) {
        if set {
            self.m_mask |= Self::get_core_bit(core);
        } else {
            self.m_mask &= !Self::get_core_bit(core);
        }
    }

    /// Set all allowed cores.
    pub fn set_all(&mut self) {
        self.m_mask = ALLOWED_AFFINITY_MASK;
    }

    /// Get the bit for a given core index.
    const fn get_core_bit(core: i32) -> u64 {
        debug_assert!(core >= 0 && (core as u32) < NUM_CPU_CORES);
        1u64 << core
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_mask_is_zero() {
        let mask = KAffinityMask::new();
        assert_eq!(mask.get_affinity_mask(), 0);
    }

    #[test]
    fn test_set_affinity() {
        let mut mask = KAffinityMask::new();
        mask.set_affinity(0, true);
        assert!(mask.get_affinity(0));
        assert!(!mask.get_affinity(1));
        assert_eq!(mask.get_affinity_mask(), 1);

        mask.set_affinity(2, true);
        assert_eq!(mask.get_affinity_mask(), 0b101);

        mask.set_affinity(0, false);
        assert!(!mask.get_affinity(0));
        assert_eq!(mask.get_affinity_mask(), 0b100);
    }

    #[test]
    fn test_set_all() {
        let mut mask = KAffinityMask::new();
        mask.set_all();
        assert_eq!(mask.get_affinity_mask(), ALLOWED_AFFINITY_MASK);
        for core in 0..NUM_CPU_CORES as i32 {
            assert!(mask.get_affinity(core));
        }
    }

    #[test]
    fn test_set_affinity_mask() {
        let mut mask = KAffinityMask::new();
        mask.set_affinity_mask(0b1010);
        assert!(!mask.get_affinity(0));
        assert!(mask.get_affinity(1));
        assert!(!mask.get_affinity(2));
        assert!(mask.get_affinity(3));
    }
}
