// SPDX-FileCopyrightText: 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/range_sets.h and zuyu/src/common/range_sets.inc
//!
//! Provides `RangeSet` and `OverlapRangeSet` for managing sets of address
//! ranges. The C++ implementation uses boost::icl interval sets. In Rust we
//! use a `BTreeMap` to store non-overlapping intervals, merging on add and
//! splitting on subtract.

use std::collections::BTreeMap;

// ---------------------------------------------------------------------------
// RangeSet
// ---------------------------------------------------------------------------

/// A set of non-overlapping address ranges that automatically merges adjacent
/// or overlapping intervals when ranges are added, and splits/trims when
/// ranges are subtracted.
///
/// Maps to the C++ `RangeSet<AddressType>`.
pub struct RangeSet {
    /// Maps interval start -> interval end (exclusive). Intervals are always
    /// non-overlapping and non-adjacent (merged on insertion).
    ranges: BTreeMap<u64, u64>,
}

impl RangeSet {
    pub fn new() -> Self {
        Self {
            ranges: BTreeMap::new(),
        }
    }

    /// Add the range `[base_address, base_address + size)` to the set,
    /// merging with any overlapping or adjacent existing ranges.
    pub fn add(&mut self, base_address: u64, size: usize) {
        if size == 0 {
            return;
        }
        let mut start = base_address;
        let mut end = base_address + size as u64;

        // Collect and remove all overlapping/adjacent ranges.
        // An existing range [s, e) overlaps or is adjacent if s <= end && e >= start.
        let keys_to_remove: Vec<u64> = self
            .ranges
            .range(..=end)
            .filter(|(&s, &e)| s <= end && e >= start)
            .map(|(&s, _)| s)
            .collect();

        for k in &keys_to_remove {
            let e = self.ranges.remove(k).unwrap();
            if *k < start {
                start = *k;
            }
            if e > end {
                end = e;
            }
        }

        self.ranges.insert(start, end);
    }

    /// Subtract the range `[base_address, base_address + size)` from the set.
    pub fn subtract(&mut self, base_address: u64, size: usize) {
        if size == 0 {
            return;
        }
        let sub_start = base_address;
        let sub_end = base_address + size as u64;

        // Find all ranges that overlap [sub_start, sub_end).
        let keys_to_process: Vec<u64> = self
            .ranges
            .range(..sub_end)
            .filter(|(&s, &e)| s < sub_end && e > sub_start)
            .map(|(&s, _)| s)
            .collect();

        for k in keys_to_process {
            let e = self.ranges.remove(&k).unwrap();
            // The existing range is [k, e). We subtract [sub_start, sub_end).
            // Left remainder: [k, sub_start) if k < sub_start
            if k < sub_start {
                self.ranges.insert(k, sub_start);
            }
            // Right remainder: [sub_end, e) if e > sub_end
            if e > sub_end {
                self.ranges.insert(sub_end, e);
            }
        }
    }

    /// Remove all ranges.
    pub fn clear(&mut self) {
        self.ranges.clear();
    }

    /// Returns true if the set contains no ranges.
    pub fn empty(&self) -> bool {
        self.ranges.is_empty()
    }

    /// Iterate over all ranges as `(start, end)` pairs where the range is
    /// `[start, end)`. Matches upstream `ForEach(func)` where func receives
    /// `(lower, upper)`.
    pub fn for_each<F: FnMut(u64, u64)>(&self, mut func: F) {
        for (&start, &end) in &self.ranges {
            func(start, end);
        }
    }

    /// Iterate over ranges that intersect `[device_addr, device_addr + size)`.
    /// The callback receives `(clamped_start, clamped_end)` where the range is
    /// clamped to the query window.
    pub fn for_each_in_range<F: FnMut(u64, u64)>(
        &self,
        device_addr: u64,
        size: usize,
        mut func: F,
    ) {
        if self.ranges.is_empty() {
            return;
        }
        let start_address = device_addr;
        let end_address = device_addr + size as u64;

        for (&s, &e) in self.ranges.range(..end_address) {
            if e <= start_address {
                continue;
            }
            let clamped_start = s.max(start_address);
            let clamped_end = e.min(end_address);
            func(clamped_start, clamped_end);
        }
        // Also check if there's a range starting exactly at or before start_address
        // that might extend into our window -- already covered by the loop above
        // since we iterate all ranges with start < end_address.
    }
}

impl Default for RangeSet {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// OverlapRangeSet
// ---------------------------------------------------------------------------

/// A set of address ranges that tracks overlap counts. Adding the same range
/// twice increments the count; subtracting decrements it. Ranges with a count
/// of zero or less are removed.
///
/// Maps to the C++ `OverlapRangeSet<AddressType>`.
///
/// Internally uses a sorted vector of `(start, end, count)` entries that are
/// split on boundaries as needed (similar to boost::icl::split_interval_map).
pub struct OverlapRangeSet {
    /// Sorted by start address. Ranges are non-overlapping.
    /// Each entry is (start_inclusive, end_exclusive, overlap_count).
    entries: Vec<(u64, u64, i32)>,
}

impl OverlapRangeSet {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Add the range `[base_address, base_address + size)`, incrementing
    /// overlap counts for any existing sub-ranges.
    pub fn add(&mut self, base_address: u64, size: usize) {
        if size == 0 {
            return;
        }
        let add_start = base_address;
        let add_end = base_address + size as u64;
        self.apply_delta(add_start, add_end, 1);
    }

    /// Subtract one overlap from `[base_address, base_address + size)`.
    /// Ranges whose count drops to zero are removed.
    pub fn subtract(&mut self, base_address: u64, size: usize) {
        self.subtract_with_callback(base_address, size, 1, |_, _| {});
    }

    /// Subtract one overlap and call `on_delete(start, end)` for each
    /// sub-range whose count drops to exactly zero before removal.
    pub fn subtract_with_on_delete<F: FnMut(u64, u64)>(
        &mut self,
        base_address: u64,
        size: usize,
        on_delete: F,
    ) {
        self.subtract_with_callback(base_address, size, 1, on_delete);
    }

    /// Delete all overlaps in `[base_address, base_address + size)`.
    pub fn delete_all(&mut self, base_address: u64, size: usize) {
        self.subtract_with_callback(base_address, size, i32::MAX, |_, _| {});
    }

    /// Remove all entries.
    pub fn clear(&mut self) {
        self.entries.clear();
    }

    /// Returns true if no ranges are tracked.
    pub fn empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Iterate over all ranges as `(start, end, count)`.
    pub fn for_each<F: FnMut(u64, u64, i32)>(&self, mut func: F) {
        for &(s, e, c) in &self.entries {
            func(s, e, c);
        }
    }

    /// Iterate over ranges intersecting `[device_addr, device_addr + size)`,
    /// clamping to the query window.
    pub fn for_each_in_range<F: FnMut(u64, u64, i32)>(
        &self,
        device_addr: u64,
        size: usize,
        mut func: F,
    ) {
        if self.entries.is_empty() {
            return;
        }
        let start_address = device_addr;
        let end_address = device_addr + size as u64;

        for &(s, e, c) in &self.entries {
            if e <= start_address {
                continue;
            }
            if s >= end_address {
                break;
            }
            let clamped_start = s.max(start_address);
            let clamped_end = e.min(end_address);
            func(clamped_start, clamped_end, c);
        }
    }

    // -- internal helpers --

    /// Apply a delta to all sub-ranges within `[add_start, add_end)`.
    /// Splits existing entries at the boundaries as needed and inserts new
    /// entries for uncovered portions.
    fn apply_delta(&mut self, add_start: u64, add_end: u64, delta: i32) {
        if add_start >= add_end {
            return;
        }

        let mut new_entries: Vec<(u64, u64, i32)> = Vec::new();
        let mut pos = add_start;
        let mut i = 0;

        // Copy entries before the affected range.
        while i < self.entries.len() && self.entries[i].1 <= add_start {
            new_entries.push(self.entries[i]);
            i += 1;
        }

        // Process entries that overlap [add_start, add_end).
        while i < self.entries.len() && self.entries[i].0 < add_end {
            let (es, ee, ec) = self.entries[i];

            // Fill gap before this entry with the delta.
            if pos < es.min(add_end) {
                let gap_end = es.min(add_end);
                new_entries.push((pos, gap_end, delta));
                pos = gap_end;
            }

            if es < add_start {
                // Entry starts before our range -- split off the prefix.
                new_entries.push((es, add_start, ec));
                // The overlapping part:
                let overlap_end = ee.min(add_end);
                new_entries.push((add_start, overlap_end, ec + delta));
                pos = overlap_end;
                if ee > add_end {
                    // Entry extends past our range -- split off the suffix.
                    new_entries.push((add_end, ee, ec));
                    pos = add_end;
                }
            } else {
                // Entry starts within our range.
                let overlap_start = es.max(pos);
                // Fill any gap.
                if pos < overlap_start {
                    new_entries.push((pos, overlap_start, delta));
                }
                let overlap_end = ee.min(add_end);
                new_entries.push((overlap_start, overlap_end, ec + delta));
                pos = overlap_end;
                if ee > add_end {
                    new_entries.push((add_end, ee, ec));
                    pos = add_end;
                }
            }
            i += 1;
        }

        // Fill remaining gap.
        if pos < add_end {
            new_entries.push((pos, add_end, delta));
        }

        // Copy entries after the affected range.
        while i < self.entries.len() {
            new_entries.push(self.entries[i]);
            i += 1;
        }

        self.entries = new_entries;
    }

    /// Subtract `amount` from overlap counts in `[base_address, base_address + size)`.
    /// Calls `on_delete` for sub-ranges whose count drops to exactly zero.
    /// Removes sub-ranges whose count drops to zero or below.
    fn subtract_with_callback<F: FnMut(u64, u64)>(
        &mut self,
        base_address: u64,
        size: usize,
        amount: i32,
        mut on_delete: F,
    ) {
        if size == 0 || self.entries.is_empty() {
            return;
        }
        let sub_start = base_address;
        let sub_end = base_address + size as u64;

        // First, apply the negative delta.
        self.apply_delta(sub_start, sub_end, -amount);

        // Now remove entries with count <= 0, calling on_delete for count == 0.
        let mut cleaned = Vec::with_capacity(self.entries.len());
        for &(s, e, c) in &self.entries {
            if c <= 0 {
                if c == 0 {
                    on_delete(s, e);
                }
                // Drop this entry (count <= 0).
            } else {
                cleaned.push((s, e, c));
            }
        }
        self.entries = cleaned;
    }
}

impl Default for OverlapRangeSet {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_range_set_add_merge() {
        let mut rs = RangeSet::new();
        rs.add(10, 10); // [10, 20)
        rs.add(20, 10); // [20, 30) -- should merge into [10, 30)

        let mut ranges = Vec::new();
        rs.for_each(|s, e| ranges.push((s, e)));
        assert_eq!(ranges, vec![(10, 30)]);
    }

    #[test]
    fn test_range_set_add_overlap() {
        let mut rs = RangeSet::new();
        rs.add(10, 20); // [10, 30)
        rs.add(15, 10); // [15, 25) -- already covered

        let mut ranges = Vec::new();
        rs.for_each(|s, e| ranges.push((s, e)));
        assert_eq!(ranges, vec![(10, 30)]);
    }

    #[test]
    fn test_range_set_subtract_split() {
        let mut rs = RangeSet::new();
        rs.add(10, 30); // [10, 40)
        rs.subtract(20, 5); // remove [20, 25) -> [10,20) and [25,40)

        let mut ranges = Vec::new();
        rs.for_each(|s, e| ranges.push((s, e)));
        assert_eq!(ranges, vec![(10, 20), (25, 40)]);
    }

    #[test]
    fn test_range_set_subtract_trim_start() {
        let mut rs = RangeSet::new();
        rs.add(10, 20); // [10, 30)
        rs.subtract(10, 5); // [10, 15) removed -> [15, 30)

        let mut ranges = Vec::new();
        rs.for_each(|s, e| ranges.push((s, e)));
        assert_eq!(ranges, vec![(15, 30)]);
    }

    #[test]
    fn test_range_set_for_each_in_range() {
        let mut rs = RangeSet::new();
        rs.add(0, 100);
        rs.add(200, 100);

        let mut ranges = Vec::new();
        rs.for_each_in_range(50, 200, |s, e| ranges.push((s, e)));
        assert_eq!(ranges, vec![(50, 100), (200, 250)]);
    }

    #[test]
    fn test_range_set_empty_and_clear() {
        let mut rs = RangeSet::new();
        assert!(rs.empty());
        rs.add(0, 10);
        assert!(!rs.empty());
        rs.clear();
        assert!(rs.empty());
    }

    #[test]
    fn test_overlap_range_set_basic() {
        let mut ors = OverlapRangeSet::new();
        ors.add(10, 10); // [10, 20) count=1
        ors.add(10, 10); // [10, 20) count=2

        let mut ranges = Vec::new();
        ors.for_each(|s, e, c| ranges.push((s, e, c)));
        assert_eq!(ranges, vec![(10, 20, 2)]);

        ors.subtract(10, 10);
        let mut ranges = Vec::new();
        ors.for_each(|s, e, c| ranges.push((s, e, c)));
        assert_eq!(ranges, vec![(10, 20, 1)]);

        ors.subtract(10, 10);
        assert!(ors.empty());
    }

    #[test]
    fn test_overlap_range_set_partial_overlap() {
        let mut ors = OverlapRangeSet::new();
        ors.add(10, 20); // [10, 30) count=1
        ors.add(20, 20); // [20, 40) count=1 on [20,30) becomes 2, [30,40) becomes 1

        let mut ranges = Vec::new();
        ors.for_each(|s, e, c| ranges.push((s, e, c)));
        assert_eq!(ranges, vec![(10, 20, 1), (20, 30, 2), (30, 40, 1)]);
    }

    #[test]
    fn test_overlap_range_set_delete_all() {
        let mut ors = OverlapRangeSet::new();
        ors.add(10, 10);
        ors.add(10, 10);
        ors.add(10, 10);
        ors.delete_all(10, 10);
        assert!(ors.empty());
    }

    #[test]
    fn test_overlap_range_set_on_delete_callback() {
        let mut ors = OverlapRangeSet::new();
        ors.add(10, 10);
        let mut deleted = Vec::new();
        ors.subtract_with_on_delete(10, 10, |s, e| deleted.push((s, e)));
        assert_eq!(deleted, vec![(10, 20)]);
        assert!(ors.empty());
    }
}
