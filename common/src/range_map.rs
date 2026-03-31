//! Port of zuyu/src/common/range_map.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::collections::BTreeMap;

/// A map keyed by ranges, for tracking memory regions.
///
/// Mirrors the C++ `Common::RangeMap<KeyTBase, ValueT>`. Internally uses a
/// `BTreeMap<i64, V>` where each entry marks the start of a region with the
/// given value. Unmapped regions have the `null_value`.
///
/// The key type `K` is converted to `i64` internally (matching the C++ signed
/// key conversion).
pub struct RangeMap<V: Copy + PartialEq> {
    null_value: V,
    container: BTreeMap<i64, V>,
}

impl<V: Copy + PartialEq> RangeMap<V> {
    /// Create a new range map with the given null (unmapped) value.
    pub fn new(null_value: V) -> Self {
        let mut container = BTreeMap::new();
        container.insert(i64::MIN, null_value);
        Self {
            null_value,
            container,
        }
    }

    /// Map the range `[address, address_end)` to `value`.
    pub fn map(&mut self, address: u64, address_end: u64, value: V) {
        let new_address = (address as i64).max(0);
        let new_address_end = (address_end as i64).max(0);
        self.internal_map(new_address, new_address_end, value);
    }

    /// Unmap the range `[address, address_end)` (set to null value).
    pub fn unmap(&mut self, address: u64, address_end: u64) {
        let nv = self.null_value;
        self.map(address, address_end, nv);
    }

    /// Get the continuous size from `address` until the region changes value.
    /// Returns 0 if the address is unmapped.
    pub fn get_continuous_size_from(&self, address: u64) -> usize {
        let new_address = address as i64;
        if new_address < 0 {
            return 0;
        }
        self.continuous_size_internal(new_address)
    }

    /// Get the value at the given address.
    pub fn get_value_at(&self, address: u64) -> V {
        let new_address = address as i64;
        if new_address < 0 {
            return self.null_value;
        }
        self.get_value_internal(new_address)
    }

    // -- private helpers --

    fn continuous_size_internal(&self, address: i64) -> usize {
        let it = self.get_first_element_before_or_on(address);
        match it {
            None => 0,
            Some((&key, &val)) => {
                if val == self.null_value {
                    return 0;
                }
                // Find the next entry after key
                let next = self
                    .container
                    .range((std::ops::Bound::Excluded(key), std::ops::Bound::Unbounded))
                    .next();
                match next {
                    None => (i64::MAX - address) as usize,
                    Some((&next_key, _)) => (next_key - address) as usize,
                }
            }
        }
    }

    fn get_value_internal(&self, address: i64) -> V {
        match self.get_first_element_before_or_on(address) {
            None => self.null_value,
            Some((_, &val)) => val,
        }
    }

    /// Find the entry with the largest key <= address.
    fn get_first_element_before_or_on(&self, address: i64) -> Option<(&i64, &V)> {
        // range ..=address gives all keys <= address, we want the last one
        self.container.range(..=address).next_back()
    }

    fn get_first_value_within(&self, address: i64) -> V {
        // lower_bound equivalent: first key >= address
        let at_or_after = self.container.range(address..).next();
        if let Some((&key, _)) = at_or_after {
            if key == address {
                // Check predecessor
                if let Some((_, &val)) = self.container.range(..address).next_back() {
                    return val;
                }
                return self
                    .container
                    .range(address..)
                    .next()
                    .map(|(_, &v)| v)
                    .unwrap_or(self.null_value);
            }
        }
        // Get the entry just before address
        match self.container.range(..address).next_back() {
            Some((_, &val)) => val,
            None => {
                // If begin, return begin's value
                self.container
                    .values()
                    .next()
                    .copied()
                    .unwrap_or(self.null_value)
            }
        }
    }

    fn get_last_value_within(&self, address: i64) -> V {
        // upper_bound equivalent: first key > address
        // Then go back one
        match self.container.range(..=address).next_back() {
            Some((_, &val)) => val,
            None => {
                // Would be a bug per C++ comment
                self.container
                    .values()
                    .next()
                    .copied()
                    .unwrap_or(self.null_value)
            }
        }
    }

    fn internal_map(&mut self, address: i64, address_end: i64, value: V) {
        let must_add_start = self.get_first_value_within(address) != value;
        let last_value = self.get_last_value_within(address_end);
        let must_add_end = last_value != value;

        // Remove all entries in [address, address_end]
        let keys_to_remove: Vec<i64> = self
            .container
            .range(address..=address_end)
            .map(|(&k, _)| k)
            .collect();
        // But we need to match C++ behavior: lower_bound(address)..upper_bound(address_end)
        // upper_bound(address_end) means keys strictly > address_end
        // The range address..=address_end captures keys in [address, address_end]
        // But C++ uses upper_bound which would be strictly > address_end.
        // Actually the keys_to_remove with ..address_end (exclusive) would be wrong.
        // Let me re-check: C++ does lower_bound(address) to upper_bound(address_end).
        // upper_bound(x) = first element with key > x.
        // So it erases [lower_bound(address), upper_bound(address_end)) which means
        // all keys k where address <= k <= address_end.
        // Our range(address..=address_end) is correct.

        for k in keys_to_remove {
            self.container.remove(&k);
        }

        if must_add_start {
            self.container.insert(address, value);
        }
        if must_add_end {
            self.container.insert(address_end, last_value);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_and_get() {
        let mut rm = RangeMap::new(0u32);
        rm.map(100, 200, 1);
        assert_eq!(rm.get_value_at(50), 0);
        assert_eq!(rm.get_value_at(100), 1);
        assert_eq!(rm.get_value_at(150), 1);
        assert_eq!(rm.get_value_at(200), 0);
    }

    #[test]
    fn test_unmap() {
        let mut rm = RangeMap::new(0u32);
        rm.map(100, 200, 1);
        rm.unmap(120, 150);
        assert_eq!(rm.get_value_at(110), 1);
        assert_eq!(rm.get_value_at(130), 0);
        assert_eq!(rm.get_value_at(160), 1);
    }

    #[test]
    fn test_continuous_size() {
        let mut rm = RangeMap::new(0u32);
        rm.map(100, 200, 1);
        assert_eq!(rm.get_continuous_size_from(100), 100);
        assert_eq!(rm.get_continuous_size_from(150), 50);
        assert_eq!(rm.get_continuous_size_from(50), 0); // unmapped
    }
}
