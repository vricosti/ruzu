//! Port of zuyu/src/core/hle/kernel/memory_types.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! Basic memory constants and types used by the kernel.

/// Number of bits in a page address (12 = 4 KiB pages).
pub const PAGE_BITS: usize = 12;

/// Size of a single page in bytes (4096).
pub const PAGE_SIZE: usize = 1 << PAGE_BITS;

/// A single page of memory (4096 bytes).
pub type Page = [u8; PAGE_SIZE];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_page_constants() {
        assert_eq!(PAGE_BITS, 12);
        assert_eq!(PAGE_SIZE, 4096);
        assert_eq!(std::mem::size_of::<Page>(), PAGE_SIZE);
    }
}
