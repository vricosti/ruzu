// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `video_core/buffer_cache/word_manager.h`
//!
//! Word-level dirty tracking for buffer cache pages.
//! Tracks CPU, GPU, cached CPU, untracked, and preflushable states
//! using bitmask words where each bit represents one device page.

use common::types::VAddr;

// ---------------------------------------------------------------------------
// Constants (from word_manager.h top-level)
// ---------------------------------------------------------------------------

/// Number of device pages tracked per 64-bit word.
pub const PAGES_PER_WORD: u64 = 64;

/// Bytes per device page (matches `Core::DEVICE_PAGESIZE`).
pub const BYTES_PER_PAGE: u64 = 4096;

/// Bytes covered by a single tracking word.
pub const BYTES_PER_WORD: u64 = PAGES_PER_WORD * BYTES_PER_PAGE;

// ---------------------------------------------------------------------------
// Type — tracking channel enum
// ---------------------------------------------------------------------------

/// Which tracking channel a query / mutation targets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Cpu,
    Gpu,
    CachedCpu,
    Untracked,
    Preflushable,
}

// ---------------------------------------------------------------------------
// WordsArray<N> — small-vector-optimized bitmask storage
// ---------------------------------------------------------------------------

/// Vector tracking modified pages, tightly packed with small vector optimization.
///
/// When the buffer is small enough (`num_words <= STACK_WORDS`), the bits live
/// on the stack. Otherwise they are heap-allocated.
pub struct WordsArray<const STACK_WORDS: usize> {
    /// Small-buffer stack storage.
    pub stack: [u64; STACK_WORDS],
    /// Heap pointer used when the buffer does not fit on the stack.
    /// When using stack storage this is left dangling / uninitialized.
    pub heap: *mut u64,
}

// Manual impls because raw pointer prevents auto-derive.
unsafe impl<const N: usize> Send for WordsArray<N> {}
unsafe impl<const N: usize> Sync for WordsArray<N> {}

impl<const N: usize> Default for WordsArray<N> {
    fn default() -> Self {
        Self {
            stack: [0u64; N],
            heap: std::ptr::null_mut(),
        }
    }
}

impl<const N: usize> WordsArray<N> {
    /// Returns a pointer to the words state.
    #[inline]
    pub fn pointer(&self, is_short: bool) -> *const u64 {
        if is_short {
            self.stack.as_ptr()
        } else {
            self.heap as *const u64
        }
    }

    /// Returns a mutable pointer to the words state.
    #[inline]
    pub fn pointer_mut(&mut self, is_short: bool) -> *mut u64 {
        if is_short {
            self.stack.as_mut_ptr()
        } else {
            self.heap
        }
    }

    /// Returns a slice view into the active storage.
    #[inline]
    pub fn as_slice(&self, is_short: bool, num_words: usize) -> &[u64] {
        unsafe { std::slice::from_raw_parts(self.pointer(is_short), num_words) }
    }

    /// Returns a mutable slice view into the active storage.
    #[inline]
    pub fn as_mut_slice(&mut self, is_short: bool, num_words: usize) -> &mut [u64] {
        unsafe { std::slice::from_raw_parts_mut(self.pointer_mut(is_short), num_words) }
    }
}

// ---------------------------------------------------------------------------
// Words<N> — the five tracking channels for one region
// ---------------------------------------------------------------------------

/// Tracks five channels of page-level dirty bits for a memory region.
///
/// Corresponds to the C++ `Words<stack_words>` template.
pub struct Words<const STACK_WORDS: usize> {
    pub size_bytes: u64,
    pub num_words: usize,
    pub cpu: WordsArray<STACK_WORDS>,
    pub gpu: WordsArray<STACK_WORDS>,
    pub cached_cpu: WordsArray<STACK_WORDS>,
    pub untracked: WordsArray<STACK_WORDS>,
    pub preflushable: WordsArray<STACK_WORDS>,
}

impl<const STACK_WORDS: usize> Default for Words<STACK_WORDS> {
    fn default() -> Self {
        Self {
            size_bytes: 0,
            num_words: 0,
            cpu: WordsArray::default(),
            gpu: WordsArray::default(),
            cached_cpu: WordsArray::default(),
            untracked: WordsArray::default(),
            preflushable: WordsArray::default(),
        }
    }
}

impl<const STACK_WORDS: usize> Words<STACK_WORDS> {
    /// Create a new `Words` tracking `size_bytes` of memory.
    pub fn new(size_bytes: u64) -> Self {
        let num_words = common::div_ceil::div_ceil(size_bytes, BYTES_PER_WORD) as usize;
        let mut words = Self {
            size_bytes,
            num_words,
            cpu: WordsArray::default(),
            gpu: WordsArray::default(),
            cached_cpu: WordsArray::default(),
            untracked: WordsArray::default(),
            preflushable: WordsArray::default(),
        };

        if words.is_short() {
            words.cpu.stack.fill(!0u64);
            words.gpu.stack.fill(0);
            words.cached_cpu.stack.fill(0);
            words.untracked.stack.fill(!0u64);
            words.preflushable.stack.fill(0);
        } else {
            // Share allocation between all five channels
            let layout = std::alloc::Layout::array::<u64>(num_words * 5).unwrap();
            let alloc = unsafe { std::alloc::alloc_zeroed(layout) as *mut u64 };
            assert!(!alloc.is_null(), "allocation failed");

            words.cpu.heap = alloc;
            words.gpu.heap = unsafe { alloc.add(num_words) };
            words.cached_cpu.heap = unsafe { alloc.add(num_words * 2) };
            words.untracked.heap = unsafe { alloc.add(num_words * 3) };
            words.preflushable.heap = unsafe { alloc.add(num_words * 4) };

            // cpu: all 1s, gpu: 0, cached_cpu: 0, untracked: all 1s, preflushable: 0
            unsafe {
                std::ptr::write_bytes(words.cpu.heap, 0xFF, num_words);
                // gpu, cached_cpu already zeroed
                std::ptr::write_bytes(words.untracked.heap, 0xFF, num_words);
                // preflushable already zeroed
            }
        }

        // Clean up trailing bits in the last word
        let last_word_size = size_bytes % BYTES_PER_WORD;
        let last_local_page = common::div_ceil::div_ceil(last_word_size, BYTES_PER_PAGE);
        let shift = (PAGES_PER_WORD - last_local_page) % PAGES_PER_WORD;
        let last_word = (!0u64 << shift) >> shift;

        let is_short = words.is_short();
        let nw = words.num_words;
        if nw > 0 {
            words.cpu.as_mut_slice(is_short, nw)[nw - 1] = last_word;
            words.untracked.as_mut_slice(is_short, nw)[nw - 1] = last_word;
        }

        words
    }

    /// Returns true when the buffer fits in the small vector optimization.
    #[inline]
    pub fn is_short(&self) -> bool {
        self.num_words <= STACK_WORDS
    }

    /// Returns the number of words.
    #[inline]
    pub fn num_words(&self) -> usize {
        self.num_words
    }

    /// Get a slice for the given tracking type.
    pub fn span(&self, ty: Type) -> &[u64] {
        let is_short = self.is_short();
        let n = self.num_words;
        match ty {
            Type::Cpu => self.cpu.as_slice(is_short, n),
            Type::Gpu => self.gpu.as_slice(is_short, n),
            Type::CachedCpu => self.cached_cpu.as_slice(is_short, n),
            Type::Untracked => self.untracked.as_slice(is_short, n),
            Type::Preflushable => self.preflushable.as_slice(is_short, n),
        }
    }

    /// Get a mutable slice for the given tracking type.
    pub fn span_mut(&mut self, ty: Type) -> &mut [u64] {
        let is_short = self.is_short();
        let n = self.num_words;
        match ty {
            Type::Cpu => self.cpu.as_mut_slice(is_short, n),
            Type::Gpu => self.gpu.as_mut_slice(is_short, n),
            Type::CachedCpu => self.cached_cpu.as_mut_slice(is_short, n),
            Type::Untracked => self.untracked.as_mut_slice(is_short, n),
            Type::Preflushable => self.preflushable.as_mut_slice(is_short, n),
        }
    }
}

impl<const STACK_WORDS: usize> Drop for Words<STACK_WORDS> {
    fn drop(&mut self) {
        if !self.is_short() && !self.cpu.heap.is_null() {
            // CPU heap is the base for the single shared allocation
            let layout = std::alloc::Layout::array::<u64>(self.num_words * 5).unwrap();
            unsafe {
                std::alloc::dealloc(self.cpu.heap as *mut u8, layout);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// DeviceTracker trait — abstraction for the rasterizer notification callback
// ---------------------------------------------------------------------------

/// Trait that the rasterizer must implement so that the word manager can
/// notify it about page-tracking changes.
///
/// Corresponds to the `DeviceTracker` template parameter in C++.
pub trait DeviceTracker {
    /// Adjust the cached-page reference count for a range.
    ///
    /// `delta` is +1 when the tracker should start watching or -1 when it
    /// should stop.
    fn update_pages_cached_count(&self, addr: VAddr, size: u64, delta: i32);
}

// ---------------------------------------------------------------------------
// WordManager<DT, STACK_WORDS>
// ---------------------------------------------------------------------------

/// Per-region word-level dirty tracker.
///
/// Corresponds to the C++ `WordManager<DeviceTracker, stack_words>` template.
pub struct WordManager<DT: DeviceTracker, const STACK_WORDS: usize = 1> {
    cpu_addr: VAddr,
    tracker: *const DT,
    words: Words<STACK_WORDS>,
}

unsafe impl<DT: DeviceTracker, const N: usize> Send for WordManager<DT, N> {}
unsafe impl<DT: DeviceTracker, const N: usize> Sync for WordManager<DT, N> {}

impl<DT: DeviceTracker, const STACK_WORDS: usize> WordManager<DT, STACK_WORDS> {
    /// Create a new word manager for a region starting at `cpu_addr` of `size_bytes`.
    pub fn new(cpu_addr: VAddr, tracker: &DT, size_bytes: u64) -> Self {
        Self {
            cpu_addr,
            tracker: tracker as *const DT,
            words: Words::new(size_bytes),
        }
    }

    /// Create a default (empty) word manager.
    pub fn empty() -> Self {
        Self {
            cpu_addr: 0,
            tracker: std::ptr::null(),
            words: Words::default(),
        }
    }

    pub fn set_cpu_address(&mut self, new_cpu_addr: VAddr) {
        self.cpu_addr = new_cpu_addr;
    }

    pub fn get_cpu_addr(&self) -> VAddr {
        self.cpu_addr
    }

    /// Extract bits from a word between `page_start` and `page_end`.
    #[inline]
    pub fn extract_bits(word: u64, page_start: usize, page_end: usize) -> u64 {
        let number_bits: usize = 64;
        let limit_page_end = number_bits - page_end.min(number_bits);
        let bits = (word >> page_start) << page_start;
        (bits << limit_page_end) >> limit_page_end
    }

    /// Get the word index and page-within-word for an address.
    #[inline]
    pub fn get_word_page(address: VAddr) -> (usize, usize) {
        let addr = address as usize;
        let word_number = addr / BYTES_PER_WORD as usize;
        let amount_pages = addr % BYTES_PER_WORD as usize;
        (word_number, amount_pages / BYTES_PER_PAGE as usize)
    }

    /// Iterate over words that overlap `[offset, offset+size)`, calling `func`
    /// with `(word_index, mask)`.
    ///
    /// If `func` returns `Some(true)`, iteration stops early (bool-break pattern).
    pub fn iterate_words<F>(&self, offset: u64, size: u64, mut func: F)
    where
        F: FnMut(usize, u64) -> Option<bool>,
    {
        let start = (offset as i64).max(0) as usize;
        let end = ((offset + size) as i64).max(0) as usize;
        if start >= self.size_bytes() as usize || end <= start {
            return;
        }
        let (mut start_word, start_page) = Self::get_word_page(start as u64);
        let (mut end_word, mut end_page) = Self::get_word_page((end as u64) + BYTES_PER_PAGE - 1);
        let num_words = self.num_words();
        start_word = start_word.min(num_words);
        end_word = end_word.min(num_words);
        let diff = end_word - start_word;
        end_word += (end_page + PAGES_PER_WORD as usize - 1) / PAGES_PER_WORD as usize;
        end_word = end_word.min(num_words);
        let mut current_end_page = end_page + diff * PAGES_PER_WORD as usize;
        let mut current_start_page = start_page;
        let base_mask: u64 = !0u64;

        for word_index in start_word..end_word {
            let mask = Self::extract_bits(base_mask, current_start_page, current_end_page);
            current_start_page = 0;
            current_end_page = current_end_page.saturating_sub(PAGES_PER_WORD as usize);
            if let Some(true) = func(word_index, mask) {
                return;
            }
        }
    }

    /// Iterate over contiguous runs of set pages within a word mask.
    #[inline]
    pub fn iterate_pages<F>(mask: u64, mut func: F)
    where
        F: FnMut(usize, usize),
    {
        let mut m = mask;
        let mut offset: usize = 0;
        while m != 0 {
            let empty_bits = m.trailing_zeros() as usize;
            offset += empty_bits;
            m >>= empty_bits;

            let continuous_bits = m.trailing_ones() as usize;
            func(offset, continuous_bits);
            m = if continuous_bits < PAGES_PER_WORD as usize {
                m >> continuous_bits
            } else {
                0
            };
            offset += continuous_bits;
        }
    }

    /// Change the state of a range of pages.
    ///
    /// `enable` = true sets the bits, false clears them.
    pub fn change_region_state(&mut self, ty: Type, enable: bool, dirty_addr: u64, size: u64) {
        // We need to work with raw pointers to allow simultaneous mutable access
        // to different word arrays, matching the C++ approach.
        let is_short = self.words.is_short();
        let num_words = self.words.num_words;

        let state_ptr = self.words.span_mut(ty).as_mut_ptr();
        let untracked_ptr = self.words.untracked.pointer_mut(is_short);
        let cached_ptr = self.words.cached_cpu.pointer_mut(is_short);

        let cpu_addr = self.cpu_addr;
        let tracker = self.tracker;

        self.iterate_words(dirty_addr - cpu_addr, size, |index, mask| {
            unsafe {
                let state_word = state_ptr.add(index);
                let untracked_word = untracked_ptr.add(index);
                let cached_word = cached_ptr.add(index);

                match ty {
                    Type::Cpu | Type::CachedCpu => {
                        // NotifyRasterizer<!enable>
                        if !tracker.is_null() {
                            Self::notify_rasterizer_raw(
                                tracker,
                                cpu_addr,
                                !enable,
                                index,
                                *untracked_word,
                                mask,
                            );
                        }
                    }
                    _ => {}
                }

                if enable {
                    *state_word |= mask;
                    if matches!(ty, Type::Cpu | Type::CachedCpu) {
                        *untracked_word |= mask;
                    }
                    if matches!(ty, Type::Cpu) {
                        *cached_word &= !mask;
                    }
                } else {
                    if matches!(ty, Type::Cpu) {
                        let word = *state_word & mask;
                        *cached_word &= !word;
                    }
                    *state_word &= !mask;
                    if matches!(ty, Type::Cpu | Type::CachedCpu) {
                        *untracked_word &= !mask;
                    }
                }
            }
            None
        });
    }

    /// Call `func` for each modified range and optionally clear the modified bits.
    pub fn for_each_modified_range<F>(
        &mut self,
        ty: Type,
        clear: bool,
        query_cpu_range: VAddr,
        size: u64,
        func: &mut F,
    ) where
        F: FnMut(VAddr, u64),
    {
        assert_ne!(ty, Type::Untracked);

        let is_short = self.words.is_short();
        let state_ptr = self.words.span_mut(ty).as_mut_ptr();
        let untracked_ptr = self.words.untracked.pointer_mut(is_short);
        let cached_ptr = self.words.cached_cpu.pointer_mut(is_short);

        let offset = query_cpu_range - self.cpu_addr;
        let cpu_addr = self.cpu_addr;
        let tracker = self.tracker;

        let mut pending = false;
        let mut pending_offset: usize = 0;
        let mut pending_pointer: usize = 0;

        self.iterate_words(offset, size, |index, mut mask| {
            unsafe {
                if matches!(ty, Type::Gpu) {
                    mask &= !(*untracked_ptr.add(index));
                }
                let word = (*state_ptr.add(index)) & mask;

                if clear {
                    match ty {
                        Type::Cpu | Type::CachedCpu => {
                            if !tracker.is_null() {
                                Self::notify_rasterizer_raw(
                                    tracker,
                                    cpu_addr,
                                    true,
                                    index,
                                    *untracked_ptr.add(index),
                                    mask,
                                );
                            }
                        }
                        _ => {}
                    }
                    *state_ptr.add(index) &= !mask;
                    if matches!(ty, Type::Cpu | Type::CachedCpu) {
                        *untracked_ptr.add(index) &= !mask;
                    }
                    if matches!(ty, Type::Cpu) {
                        *cached_ptr.add(index) &= !word;
                    }
                }

                let base_offset = index * PAGES_PER_WORD as usize;
                Self::iterate_pages(word, |pages_offset, pages_size| {
                    if !pending {
                        pending_offset = base_offset + pages_offset;
                        pending_pointer = base_offset + pages_offset + pages_size;
                        pending = true;
                        return;
                    }
                    if pending_pointer == base_offset + pages_offset {
                        pending_pointer += pages_size;
                        return;
                    }
                    // Release the pending range
                    func(
                        cpu_addr + (pending_offset as u64) * BYTES_PER_PAGE,
                        (pending_pointer - pending_offset) as u64 * BYTES_PER_PAGE,
                    );
                    pending_offset = base_offset + pages_offset;
                    pending_pointer = base_offset + pages_offset + pages_size;
                });
            }
            None
        });
        if pending {
            func(
                cpu_addr + (pending_offset as u64) * BYTES_PER_PAGE,
                (pending_pointer - pending_offset) as u64 * BYTES_PER_PAGE,
            );
        }
    }

    /// Returns true when a region has been modified for the given type.
    pub fn is_region_modified(&self, ty: Type, offset: u64, size: u64) -> bool {
        assert_ne!(ty, Type::Untracked);

        let state_words = self.words.span(ty);
        let untracked_words = self.words.span(Type::Untracked);
        let mut result = false;

        self.iterate_words(offset, size, |index, mut mask| {
            if matches!(ty, Type::Gpu) {
                mask &= !untracked_words[index];
            }
            let word = state_words[index] & mask;
            if word != 0 {
                result = true;
                return Some(true);
            }
            Some(false)
        });
        result
    }

    /// Returns the inclusive modified region as a `(begin, end)` pair in bytes.
    pub fn modified_region(&self, ty: Type, offset: u64, size: u64) -> (u64, u64) {
        assert_ne!(ty, Type::Untracked);

        let state_words = self.words.span(ty);
        let untracked_words = self.words.span(Type::Untracked);
        let mut begin: u64 = u64::MAX;
        let mut end: u64 = 0;

        self.iterate_words(offset, size, |index, mut mask| {
            if matches!(ty, Type::Gpu) {
                mask &= !untracked_words[index];
            }
            let word = state_words[index] & mask;
            if word == 0 {
                return None;
            }
            let local_page_begin = word.trailing_zeros() as u64;
            let local_page_end = PAGES_PER_WORD - word.leading_zeros() as u64;
            let page_index = index as u64 * PAGES_PER_WORD;
            begin = begin.min(page_index + local_page_begin);
            end = page_index + local_page_end;
            None
        });

        if begin < end {
            (begin * BYTES_PER_PAGE, end * BYTES_PER_PAGE)
        } else {
            (0, 0)
        }
    }

    /// Returns the number of words of the manager.
    #[inline]
    pub fn num_words(&self) -> usize {
        self.words.num_words()
    }

    /// Returns the size in bytes of the manager.
    #[inline]
    pub fn size_bytes(&self) -> u64 {
        self.words.size_bytes
    }

    /// Returns true when the buffer fits in the small vector optimization.
    #[inline]
    pub fn is_short(&self) -> bool {
        self.words.is_short()
    }

    /// Flush cached CPU writes: move cached bits into the CPU channel.
    pub fn flush_cached_writes(&mut self) {
        let num_words = self.num_words();
        let is_short = self.words.is_short();
        let cached_ptr = self.words.cached_cpu.pointer_mut(is_short);
        let untracked_ptr = self.words.untracked.pointer_mut(is_short);
        let cpu_ptr = self.words.cpu.pointer_mut(is_short);
        let tracker = self.tracker;
        let cpu_addr = self.cpu_addr;

        for word_index in 0..num_words {
            unsafe {
                let cached_bits = *cached_ptr.add(word_index);
                if !tracker.is_null() {
                    Self::notify_rasterizer_raw(
                        tracker,
                        cpu_addr,
                        false,
                        word_index,
                        *untracked_ptr.add(word_index),
                        cached_bits,
                    );
                }
                *untracked_ptr.add(word_index) |= cached_bits;
                *cpu_ptr.add(word_index) |= cached_bits;
                *cached_ptr.add(word_index) = 0;
            }
        }
    }

    // -----------------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------------

    /// Low-level rasterizer notification using raw pointer to tracker.
    ///
    /// Safety: `tracker` must be a valid pointer or null (null is handled by caller).
    #[inline]
    unsafe fn notify_rasterizer_raw(
        tracker: *const DT,
        cpu_addr: VAddr,
        add_to_tracker: bool,
        word_index: usize,
        current_bits: u64,
        new_bits: u64,
    ) {
        let changed_bits = if add_to_tracker {
            current_bits & new_bits
        } else {
            !current_bits & new_bits
        };
        let addr = cpu_addr + word_index as u64 * BYTES_PER_WORD;
        Self::iterate_pages(changed_bits, |page_offset, page_size| {
            let delta = if add_to_tracker { 1 } else { -1 };
            (*tracker).update_pages_cached_count(
                addr + page_offset as u64 * BYTES_PER_PAGE,
                page_size as u64 * BYTES_PER_PAGE,
                delta,
            );
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_bits() {
        // Full word
        assert_eq!(
            WordManager::<DummyTracker, 1>::extract_bits(!0u64, 0, 64),
            !0u64
        );
        // First bit only
        assert_eq!(WordManager::<DummyTracker, 1>::extract_bits(!0u64, 0, 1), 1);
        // Bits 2..5
        assert_eq!(
            WordManager::<DummyTracker, 1>::extract_bits(!0u64, 2, 5),
            0b11100
        );
    }

    #[test]
    fn test_get_word_page() {
        let (w, p) = WordManager::<DummyTracker, 1>::get_word_page(0);
        assert_eq!(w, 0);
        assert_eq!(p, 0);

        let (w, p) = WordManager::<DummyTracker, 1>::get_word_page(BYTES_PER_WORD);
        assert_eq!(w, 1);
        assert_eq!(p, 0);
    }

    #[test]
    fn test_iterate_pages() {
        let mut ranges = Vec::new();
        WordManager::<DummyTracker, 1>::iterate_pages(0b1110_0011, |off, sz| {
            ranges.push((off, sz));
        });
        assert_eq!(ranges, vec![(0, 2), (5, 3)]);
    }

    struct DummyTracker;
    impl DeviceTracker for DummyTracker {
        fn update_pages_cached_count(&self, _addr: VAddr, _size: u64, _delta: i32) {}
    }
}
