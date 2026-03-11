//! Port of zuyu/src/core/hle/kernel/k_thread_local_page.h / k_thread_local_page.cpp
//! Status: Partial (structural port)
//! Derniere synchro: 2026-03-11
//!
//! KThreadLocalPage: manages thread-local storage regions within a single page.

use super::k_typed_address::KProcessAddress;

/// Page size used for TLP (4 KiB).
pub const PAGE_SIZE: usize = 4096;

/// Size of a thread-local region (from Svc::ThreadLocalRegionSize).
pub const THREAD_LOCAL_REGION_SIZE: usize = 0x200;

/// Number of TLS regions that fit in one page.
pub const REGIONS_PER_PAGE: usize = PAGE_SIZE / THREAD_LOCAL_REGION_SIZE;

/// KThreadLocalPage manages free/used TLS regions within a page.
/// Matches upstream `KThreadLocalPage` (k_thread_local_page.h).
pub struct KThreadLocalPage {
    /// Virtual address of the page.
    pub virt_addr: KProcessAddress,
    /// Per-region free flags.
    pub is_region_free: [bool; REGIONS_PER_PAGE],
}

impl KThreadLocalPage {
    /// Create a new thread-local page at the given address.
    pub fn new(addr: KProcessAddress) -> Self {
        Self {
            virt_addr: addr,
            is_region_free: [true; REGIONS_PER_PAGE],
        }
    }

    /// Get the base address of this page.
    pub fn get_address(&self) -> KProcessAddress {
        self.virt_addr
    }

    /// Initialize the page (allocate kernel memory, map into process).
    /// TODO: Port from k_thread_local_page.cpp.
    pub fn initialize(&mut self) -> u32 {
        // TODO: Full implementation
        0
    }

    /// Finalize and release the page.
    /// TODO: Port from k_thread_local_page.cpp.
    pub fn finalize(&mut self) -> u32 {
        // TODO: Full implementation
        0
    }

    /// Reserve a free region and return its address.
    /// Matches upstream `KThreadLocalPage::Reserve()`.
    pub fn reserve(&mut self) -> Option<KProcessAddress> {
        for i in 0..REGIONS_PER_PAGE {
            if self.is_region_free[i] {
                self.is_region_free[i] = false;
                return Some(self.get_region_address(i));
            }
        }
        None
    }

    /// Release a previously reserved region.
    /// Matches upstream `KThreadLocalPage::Release()`.
    pub fn release(&mut self, addr: KProcessAddress) {
        let index = self.get_region_index(addr);
        assert!(!self.is_region_free[index]);
        self.is_region_free[index] = true;
    }

    /// Are all regions in use?
    pub fn is_all_used(&self) -> bool {
        self.is_region_free.iter().all(|&free| !free)
    }

    /// Are all regions free?
    pub fn is_all_free(&self) -> bool {
        self.is_region_free.iter().all(|&free| free)
    }

    /// Is any region in use?
    pub fn is_any_used(&self) -> bool {
        !self.is_all_free()
    }

    /// Is any region free?
    pub fn is_any_free(&self) -> bool {
        !self.is_all_used()
    }

    /// Get the address of region `i`.
    fn get_region_address(&self, i: usize) -> KProcessAddress {
        KProcessAddress::new(
            self.virt_addr.get() + (i * THREAD_LOCAL_REGION_SIZE) as u64,
        )
    }

    /// Check if `addr` falls within this page.
    fn contains(&self, addr: KProcessAddress) -> bool {
        self.virt_addr.get() <= addr.get()
            && addr.get() < self.virt_addr.get() + PAGE_SIZE as u64
    }

    /// Get the region index for a given address.
    fn get_region_index(&self, addr: KProcessAddress) -> usize {
        assert!(self.contains(addr));
        ((addr.get() - self.virt_addr.get()) as usize) / THREAD_LOCAL_REGION_SIZE
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regions_per_page() {
        assert_eq!(REGIONS_PER_PAGE, 8);
    }

    #[test]
    fn test_reserve_and_release() {
        let mut page = KThreadLocalPage::new(KProcessAddress::new(0x1000));
        assert!(page.is_all_free());

        let addr = page.reserve().unwrap();
        assert_eq!(addr.get(), 0x1000);
        assert!(page.is_any_used());

        page.release(addr);
        assert!(page.is_all_free());
    }
}
