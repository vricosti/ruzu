//! Port of zuyu/src/core/gpu_dirty_memory_manager.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! GPU dirty memory tracking. Collects memory write notifications and allows
//! gathering them in batches for GPU page table invalidation.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

use crate::device_memory_manager::DEVICE_PAGEBITS;

/// Tracks dirty (modified) memory regions for GPU page table synchronization.
///
/// Corresponds to the C++ `GPUDirtyMemoryManager` class.
///
/// The manager uses a lock-free approach for the common case (single page being
/// dirtied repeatedly), with a fallback to a mutex-protected buffer when pages
/// change. Uses double-buffering to allow concurrent collection and gathering.
pub struct GpuDirtyMemoryManager {
    /// Current transform address, stored as a packed u64 (address:u32 | mask:u32).
    /// Accessed atomically for lock-free collection.
    current: AtomicU64,

    /// Mutex protecting the back buffer.
    guard: Mutex<()>,

    /// Back buffer for accumulated transforms (producer side).
    back_buffer: Mutex<Vec<TransformAddress>>,

    /// Front buffer for gathered transforms (consumer side).
    front_buffer: Vec<TransformAddress>,
}

/// Packed address + dirty mask pair.
/// The address identifies a half-page (page_bits = DEVICE_PAGEBITS - 1),
/// and the mask tracks 64-byte aligned dirty sub-regions within that half-page.
#[derive(Debug, Clone, Copy)]
#[repr(C, align(8))]
struct TransformAddress {
    address: u32,
    mask: u32,
}

impl TransformAddress {
    /// Pack into a u64 for atomic operations.
    fn to_u64(self) -> u64 {
        (self.address as u64) | ((self.mask as u64) << 32)
    }

    /// Unpack from a u64.
    fn from_u64(val: u64) -> Self {
        Self {
            address: val as u32,
            mask: (val >> 32) as u32,
        }
    }
}

// Constants matching upstream
const PAGE_BITS: usize = DEVICE_PAGEBITS - 1;
const PAGE_SIZE: usize = 1 << PAGE_BITS;
const PAGE_MASK: usize = PAGE_SIZE - 1;

const ALIGN_BITS: usize = 6;
const ALIGN_SIZE: usize = 1 << ALIGN_BITS;
const ALIGN_MASK: usize = ALIGN_SIZE - 1;

const DEFAULT_TRANSFORM: TransformAddress = TransformAddress {
    address: !0u32,
    mask: 0u32,
};

impl GpuDirtyMemoryManager {
    /// Create a new dirty memory manager.
    pub fn new() -> Self {
        Self {
            current: AtomicU64::new(DEFAULT_TRANSFORM.to_u64()),
            guard: Mutex::new(()),
            back_buffer: Mutex::new(Vec::with_capacity(256)),
            front_buffer: Vec::with_capacity(256),
        }
    }

    /// Collect a dirty memory notification for the given physical address and size.
    ///
    /// This is the hot path, called from GPU memory write handlers. Uses lock-free
    /// atomic operations for the common case where the same page is being dirtied
    /// repeatedly.
    pub fn collect(&self, address: u64, size: usize) {
        let t = self.build_transform(address, size);
        let mut tmp;
        let mut original;

        loop {
            tmp = TransformAddress::from_u64(self.current.load(Ordering::Acquire));
            original = tmp;

            if tmp.address != t.address {
                if Self::is_valid(tmp.address as u64) {
                    let _lk = self.guard.lock().unwrap();
                    self.back_buffer.lock().unwrap().push(tmp);
                    self.current.store(t.to_u64(), Ordering::Relaxed);
                    return;
                }
                tmp.address = t.address;
                tmp.mask = 0;
            }

            if (tmp.mask | t.mask) == tmp.mask {
                return;
            }
            tmp.mask |= t.mask;

            match self.current.compare_exchange_weak(
                original.to_u64(),
                tmp.to_u64(),
                Ordering::Release,
                Ordering::Relaxed,
            ) {
                Ok(_) => return,
                Err(_) => continue,
            }
        }
    }

    /// Gather all accumulated dirty regions and invoke the callback for each
    /// contiguous dirty range.
    ///
    /// The callback receives (physical_address, size) pairs.
    pub fn gather(&mut self, callback: &mut dyn FnMut(u64, usize)) {
        {
            let _lk = self.guard.lock().unwrap();
            let t = TransformAddress::from_u64(
                self.current
                    .swap(DEFAULT_TRANSFORM.to_u64(), Ordering::Relaxed),
            );
            let mut back = self.back_buffer.lock().unwrap();
            core::mem::swap(&mut self.front_buffer, &mut *back);
            if Self::is_valid(t.address as u64) {
                self.front_buffer.push(t);
            }
        }

        for transform in &self.front_buffer {
            let mut offset: usize = 0;
            let mut mask = transform.mask as u64;

            while mask != 0 {
                let empty_bits = mask.trailing_zeros() as usize;
                offset += empty_bits << ALIGN_BITS;
                mask >>= empty_bits;

                let continuous_bits = (!mask).trailing_zeros() as usize;
                let phys_addr = ((transform.address as u64) << PAGE_BITS) + offset as u64;
                callback(phys_addr, continuous_bits << ALIGN_BITS);

                if continuous_bits < ALIGN_SIZE {
                    mask >>= continuous_bits;
                } else {
                    mask = 0;
                }
                offset += continuous_bits << ALIGN_BITS;
            }
        }

        self.front_buffer.clear();
    }

    // --- Private helpers ---

    fn is_valid(address: u64) -> bool {
        address < (1u64 << 39)
    }

    fn create_mask(top_bit: usize, minor_bit: usize) -> u32 {
        if top_bit == 0 {
            return 0;
        }
        let mut mask: u32 = !0u32;
        mask <<= 32 - top_bit;
        mask >>= 32 - top_bit;
        mask >>= minor_bit;
        mask <<= minor_bit;
        mask
    }

    fn build_transform(&self, address: u64, size: usize) -> TransformAddress {
        let minor_address = (address as usize) & PAGE_MASK;
        let minor_bit = minor_address >> ALIGN_BITS;
        let top_bit = (minor_address + size + ALIGN_MASK) >> ALIGN_BITS;
        TransformAddress {
            address: (address >> PAGE_BITS) as u32,
            mask: Self::create_mask(top_bit, minor_bit),
        }
    }
}

impl Default for GpuDirtyMemoryManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_construction() {
        let mgr = GpuDirtyMemoryManager::new();
        let current = TransformAddress::from_u64(mgr.current.load(Ordering::Relaxed));
        assert_eq!(current.address, !0u32);
        assert_eq!(current.mask, 0);
    }

    #[test]
    fn test_create_mask() {
        // Full mask
        let m = GpuDirtyMemoryManager::create_mask(32, 0);
        assert_eq!(m, !0u32);

        // Single bit
        let m = GpuDirtyMemoryManager::create_mask(1, 0);
        assert_eq!(m, 1);

        // Empty
        let m = GpuDirtyMemoryManager::create_mask(0, 0);
        assert_eq!(m, 0);
    }

    #[test]
    fn test_transform_address_round_trip() {
        let t = TransformAddress {
            address: 0x12345678,
            mask: 0xABCDEF01,
        };
        let packed = t.to_u64();
        let unpacked = TransformAddress::from_u64(packed);
        assert_eq!(unpacked.address, t.address);
        assert_eq!(unpacked.mask, t.mask);
    }

    #[test]
    fn test_collect_and_gather() {
        let mut mgr = GpuDirtyMemoryManager::new();

        // Collect a single dirty region
        mgr.collect(0x1000, 64);

        let mut results = Vec::new();
        mgr.gather(&mut |addr, size| {
            results.push((addr, size));
        });

        assert!(!results.is_empty());
    }
}
