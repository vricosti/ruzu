//! Port of zuyu/src/common/slot_vector.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::hash::{Hash, Hasher};

/// Identifier for a slot within a [`SlotVector`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SlotId {
    pub index: u32,
}

impl SlotId {
    pub const INVALID_INDEX: u32 = u32::MAX;

    pub const fn invalid() -> Self {
        Self {
            index: Self::INVALID_INDEX,
        }
    }

    pub const fn is_valid(self) -> bool {
        self.index != Self::INVALID_INDEX
    }
}

impl Default for SlotId {
    fn default() -> Self {
        Self::invalid()
    }
}

impl Hash for SlotId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

/// A vector that reuses freed slots. Used heavily by GPU cache.
///
/// Mirrors the C++ `Common::SlotVector<T>` from zuyu. Items are stored in a flat
/// array with a bitset tracking which slots are occupied. Freed indices are
/// pushed onto a free list and reused on the next insert.
pub struct SlotVector<T> {
    values: Vec<Option<T>>,
    stored_bitset: Vec<u64>,
    free_list: Vec<u32>,
}

impl<T> Default for SlotVector<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> SlotVector<T> {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            stored_bitset: Vec::new(),
            free_list: Vec::new(),
        }
    }

    /// Access an element by its slot id.
    ///
    /// # Panics
    /// Panics if the id is invalid or the slot is not occupied.
    pub fn get(&self, id: SlotId) -> &T {
        self.validate_index(id);
        self.values[id.index as usize].as_ref().unwrap()
    }

    /// Access an element mutably by its slot id.
    ///
    /// # Panics
    /// Panics if the id is invalid or the slot is not occupied.
    pub fn get_mut(&mut self, id: SlotId) -> &mut T {
        self.validate_index(id);
        self.values[id.index as usize].as_mut().unwrap()
    }

    /// Insert a value and return its [`SlotId`].
    pub fn insert(&mut self, value: T) -> SlotId {
        let index = self.free_value_index();
        self.values[index as usize] = Some(value);
        self.set_storage_bit(index);
        SlotId { index }
    }

    /// Remove the value at `id`, freeing the slot for reuse.
    pub fn erase(&mut self, id: SlotId) {
        self.values[id.index as usize] = None;
        self.free_list.push(id.index);
        self.reset_storage_bit(id.index);
    }

    /// Returns the number of occupied slots.
    pub fn size(&self) -> usize {
        self.values.len() - self.free_list.len()
    }

    /// Returns an iterator over `(SlotId, &T)` pairs for all occupied slots.
    pub fn iter(&self) -> SlotVectorIter<'_, T> {
        let first = self.find_first_set();
        SlotVectorIter {
            slot_vector: self,
            current: first,
        }
    }

    /// Returns a mutable iterator over `(SlotId, &mut T)` pairs for all occupied slots.
    pub fn iter_mut(&mut self) -> SlotVectorIterMut<'_, T> {
        let first = self.find_first_set_from_bitset(&self.stored_bitset);
        SlotVectorIterMut {
            current: first,
            values: &mut self.values,
            stored_bitset: &self.stored_bitset,
        }
    }

    // -- private helpers --

    fn set_storage_bit(&mut self, index: u32) {
        let word = (index / 64) as usize;
        self.stored_bitset[word] |= 1u64 << (index % 64);
    }

    fn reset_storage_bit(&mut self, index: u32) {
        let word = (index / 64) as usize;
        self.stored_bitset[word] &= !(1u64 << (index % 64));
    }

    fn read_storage_bit(&self, index: u32) -> bool {
        let word = (index / 64) as usize;
        ((self.stored_bitset[word] >> (index % 64)) & 1) != 0
    }

    fn validate_index(&self, id: SlotId) {
        debug_assert!(id.is_valid());
        debug_assert!((id.index as usize / 64) < self.stored_bitset.len());
        debug_assert!(self.read_storage_bit(id.index));
    }

    fn free_value_index(&mut self) -> u32 {
        if self.free_list.is_empty() {
            let new_cap = if self.values.is_empty() {
                1
            } else {
                self.values.len() * 2
            };
            self.reserve(new_cap);
        }
        self.free_list.pop().unwrap()
    }

    fn reserve(&mut self, new_capacity: usize) {
        let old_capacity = self.values.len();

        // Grow storage
        self.values.resize_with(new_capacity, || None);
        self.stored_bitset.resize((new_capacity + 63) / 64, 0);

        // Add newly available indices to free list
        for i in old_capacity..new_capacity {
            self.free_list.push(i as u32);
        }
    }

    fn find_first_set(&self) -> u32 {
        self.find_first_set_from_bitset(&self.stored_bitset)
    }

    fn find_first_set_from_bitset(&self, bitset: &[u64]) -> u32 {
        for (word_idx, &word) in bitset.iter().enumerate() {
            if word != 0 {
                return (word_idx as u32) * 64 + word.trailing_zeros();
            }
        }
        SlotId::INVALID_INDEX
    }

    fn find_next_set(&self, current: u32) -> u32 {
        let size = self.stored_bitset.len() as u32 * 64;
        let mut idx = current + 1;
        while idx < size {
            if self.read_storage_bit(idx) {
                return idx;
            }
            idx += 1;
        }
        SlotId::INVALID_INDEX
    }
}

impl<T> std::ops::Index<SlotId> for SlotVector<T> {
    type Output = T;

    fn index(&self, id: SlotId) -> &T {
        self.get(id)
    }
}

impl<T> std::ops::IndexMut<SlotId> for SlotVector<T> {
    fn index_mut(&mut self, id: SlotId) -> &mut T {
        self.get_mut(id)
    }
}

/// Immutable iterator over a [`SlotVector`].
pub struct SlotVectorIter<'a, T> {
    slot_vector: &'a SlotVector<T>,
    current: u32,
}

impl<'a, T> Iterator for SlotVectorIter<'a, T> {
    type Item = (SlotId, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current == SlotId::INVALID_INDEX {
            return None;
        }
        let id = SlotId {
            index: self.current,
        };
        let value = self.slot_vector.values[self.current as usize].as_ref().unwrap();
        self.current = self.slot_vector.find_next_set(self.current);
        Some((id, value))
    }
}

/// Mutable iterator over a [`SlotVector`].
pub struct SlotVectorIterMut<'a, T> {
    current: u32,
    values: &'a mut Vec<Option<T>>,
    stored_bitset: &'a Vec<u64>,
}

impl<'a, T> SlotVectorIterMut<'a, T> {
    fn find_next_set(&self, current: u32) -> u32 {
        let size = self.stored_bitset.len() as u32 * 64;
        let mut idx = current + 1;
        while idx < size {
            let word = (idx / 64) as usize;
            if ((self.stored_bitset[word] >> (idx % 64)) & 1) != 0 {
                return idx;
            }
            idx += 1;
        }
        SlotId::INVALID_INDEX
    }
}

impl<'a, T> Iterator for SlotVectorIterMut<'a, T> {
    type Item = (SlotId, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current == SlotId::INVALID_INDEX {
            return None;
        }
        let id = SlotId {
            index: self.current,
        };
        let next = self.find_next_set(self.current);
        self.current = next;
        // SAFETY: Each iteration yields a unique index, so we never alias.
        let value = unsafe {
            let ptr = self.values[id.index as usize].as_mut().unwrap() as *mut T;
            &mut *ptr
        };
        Some((id, value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_and_get() {
        let mut sv = SlotVector::new();
        let id1 = sv.insert(10);
        let id2 = sv.insert(20);
        assert_eq!(sv[id1], 10);
        assert_eq!(sv[id2], 20);
        assert_eq!(sv.size(), 2);
    }

    #[test]
    fn test_erase_and_reuse() {
        let mut sv = SlotVector::new();
        let id1 = sv.insert(10);
        let id2 = sv.insert(20);
        sv.erase(id1);
        assert_eq!(sv.size(), 1);
        let id3 = sv.insert(30);
        // id3 should reuse id1's slot
        assert_eq!(id3.index, id1.index);
        assert_eq!(sv[id3], 30);
        assert_eq!(sv[id2], 20);
    }

    #[test]
    fn test_iter() {
        let mut sv = SlotVector::new();
        let _id1 = sv.insert(1);
        let id2 = sv.insert(2);
        let _id3 = sv.insert(3);
        sv.erase(id2);

        let collected: Vec<i32> = sv.iter().map(|(_, v)| *v).collect();
        assert_eq!(collected, vec![1, 3]);
    }

    #[test]
    fn test_slot_id_default() {
        let id = SlotId::default();
        assert!(!id.is_valid());
    }
}
