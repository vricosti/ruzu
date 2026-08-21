// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/invalidation_accumulator.h
//!
//! Accumulates cache invalidation ranges, merging adjacent regions.

/// GPU virtual address type.
pub type GPUVAddr = u64;

/// Virtual address type.
pub type VAddr = u64;

const ATOMICITY_BITS: usize = 5;
const ATOMICITY_SIZE: usize = 1 << ATOMICITY_BITS;
const ATOMICITY_SIZE_MASK: usize = ATOMICITY_SIZE - 1;
const ATOMICITY_MASK: u64 = !(ATOMICITY_SIZE_MASK as u64);

/// Accumulates invalidation ranges, merging adjacent entries aligned to 32-byte boundaries.
pub struct InvalidationAccumulator {
    start_address: GPUVAddr,
    last_collection: GPUVAddr,
    accumulated_size: usize,
    has_collected: bool,
    buffer: Vec<(VAddr, usize)>,
}

impl InvalidationAccumulator {
    pub fn new() -> Self {
        Self {
            start_address: 0,
            last_collection: 0,
            accumulated_size: 0,
            has_collected: false,
            buffer: Vec::new(),
        }
    }

    /// Add an invalidation range. Merges with the current range if adjacent.
    pub fn add(&mut self, mut address: GPUVAddr, mut size: usize) {
        // Fast path: if the range is already covered
        if address >= self.start_address && address + size as u64 <= self.last_collection {
            return;
        }

        // Align size and address to atomicity boundary
        size = ((address as usize + size + ATOMICITY_SIZE_MASK) & !ATOMICITY_SIZE_MASK)
            - address as usize;
        address &= ATOMICITY_MASK;

        let reset_values = |this: &mut Self| {
            if this.has_collected {
                this.buffer
                    .push((this.start_address, this.accumulated_size));
            }
            this.start_address = address;
            this.accumulated_size = size;
            this.last_collection = address + size as u64;
        };

        if !self.has_collected {
            reset_values(self);
            self.has_collected = true;
            return;
        }

        if address != self.last_collection {
            reset_values(self);
            return;
        }

        self.accumulated_size += size;
        self.last_collection += size as u64;
    }

    /// Clear all accumulated ranges.
    pub fn clear(&mut self) {
        self.buffer.clear();
        self.start_address = 0;
        self.last_collection = 0;
        self.has_collected = false;
    }

    /// Returns true if any invalidation has been accumulated.
    pub fn any_accumulated(&self) -> bool {
        self.has_collected
    }

    /// Invoke a callback for each accumulated range.
    pub fn callback<F: FnMut(VAddr, usize)>(&mut self, mut func: F) {
        if !self.has_collected {
            return;
        }
        self.buffer
            .push((self.start_address, self.accumulated_size));
        for &(address, size) in &self.buffer {
            func(address, size);
        }
    }
}

impl Default for InvalidationAccumulator {
    fn default() -> Self {
        Self::new()
    }
}
