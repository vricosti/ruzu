// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

/// Virtual address in guest memory (64-bit).
pub type VAddr = u64;

/// Physical address in guest memory (64-bit).
pub type PAddr = u64;

/// Kernel object handle.
pub type Handle = u32;

/// Process ID.
pub type ProcessId = u64;

/// Thread ID.
pub type ThreadId = u64;

/// Page size (4 KiB, matching ARM64 / Switch).
pub const PAGE_SIZE: usize = 0x1000;

/// Page size as u64 for address math.
pub const PAGE_SIZE_U64: u64 = PAGE_SIZE as u64;

/// Page shift (log2 of PAGE_SIZE).
pub const PAGE_SHIFT: u32 = 12;

/// Page mask for alignment checks.
pub const PAGE_MASK: u64 = PAGE_SIZE_U64 - 1;

/// Invalid handle sentinel.
pub const INVALID_HANDLE: Handle = 0;

/// TLS (Thread Local Storage) region size per thread.
pub const TLS_ENTRY_SIZE: usize = 0x200;

/// Number of TLS slots per page.
pub const TLS_SLOTS_PER_PAGE: usize = PAGE_SIZE / TLS_ENTRY_SIZE;

/// Fixed base address for NRO loading (no ASLR in Phase 1).
pub const NRO_BASE_ADDRESS: VAddr = 0x0800_0000;

/// Heap region base address.
pub const HEAP_REGION_BASE: VAddr = 0x0_8000_0000;

/// Stack region base address.
pub const STACK_REGION_BASE: VAddr = 0x0_7FF0_0000;

/// Default stack size (1 MiB).
pub const DEFAULT_STACK_SIZE: usize = 0x10_0000;

/// Guest address space size: 39-bit (512 GiB).
pub const ADDRESS_SPACE_SIZE: u64 = 1 << 39;

/// Number of pages in the address space.
pub const ADDRESS_SPACE_PAGES: usize = (ADDRESS_SPACE_SIZE / PAGE_SIZE_U64) as usize;

/// Switch system tick frequency (19.2 MHz).
pub const CNTFRQ_HZ: u64 = 19_200_000;

/// Align a value up to the given alignment.
#[inline]
pub const fn align_up(value: u64, alignment: u64) -> u64 {
    let mask = alignment - 1;
    (value + mask) & !mask
}

/// Align a value down to the given alignment.
#[inline]
pub const fn align_down(value: u64, alignment: u64) -> u64 {
    value & !(alignment - 1)
}

/// Check if a value is page-aligned.
#[inline]
pub const fn is_page_aligned(value: u64) -> bool {
    value & PAGE_MASK == 0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_align_up() {
        assert_eq!(align_up(0, PAGE_SIZE_U64), 0);
        assert_eq!(align_up(1, PAGE_SIZE_U64), PAGE_SIZE_U64);
        assert_eq!(align_up(PAGE_SIZE_U64, PAGE_SIZE_U64), PAGE_SIZE_U64);
        assert_eq!(
            align_up(PAGE_SIZE_U64 + 1, PAGE_SIZE_U64),
            PAGE_SIZE_U64 * 2
        );
    }

    #[test]
    fn test_align_down() {
        assert_eq!(align_down(0, PAGE_SIZE_U64), 0);
        assert_eq!(align_down(1, PAGE_SIZE_U64), 0);
        assert_eq!(align_down(PAGE_SIZE_U64, PAGE_SIZE_U64), PAGE_SIZE_U64);
        assert_eq!(
            align_down(PAGE_SIZE_U64 + 1, PAGE_SIZE_U64),
            PAGE_SIZE_U64
        );
    }

    #[test]
    fn test_is_page_aligned() {
        assert!(is_page_aligned(0));
        assert!(is_page_aligned(PAGE_SIZE_U64));
        assert!(is_page_aligned(PAGE_SIZE_U64 * 100));
        assert!(!is_page_aligned(1));
        assert!(!is_page_aligned(PAGE_SIZE_U64 + 1));
    }
}
