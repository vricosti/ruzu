//! Port of zuyu/src/common/lru_cache.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::collections::VecDeque;

/// An item in the LRU cache, stored in a pool with intrusive linked-list
/// pointers (indices).
struct Item<O, K> {
    obj: O,
    tick: K,
    next: Option<usize>,
    prev: Option<usize>,
}

/// LRU (Least Recently Used) cache.
///
/// Mirrors the C++ `Common::LeastRecentlyUsedCache<Traits>`. Items are stored
/// in a pool (`VecDeque`). A doubly-linked list (using indices into the pool)
/// orders items by tick, with the least-recently-used at the front.
///
/// - `O` is the object type (C++ `Traits::ObjectType`).
/// - `K` is the tick/key type (C++ `Traits::TickType`), which must be `Ord + Copy`.
pub struct LeastRecentlyUsedCache<O, K> {
    item_pool: VecDeque<Item<O, K>>,
    free_items: VecDeque<usize>,
    first: Option<usize>,
    last: Option<usize>,
}

impl<O: Copy, K: Ord + Copy + Default + Into<i64>> Default for LeastRecentlyUsedCache<O, K> {
    fn default() -> Self {
        Self::new()
    }
}

impl<O: Copy, K: Ord + Copy + Default + Into<i64>> LeastRecentlyUsedCache<O, K> {
    pub fn new() -> Self {
        Self {
            item_pool: VecDeque::new(),
            free_items: VecDeque::new(),
            first: None,
            last: None,
        }
    }

    /// Insert an object with the given tick. Returns the pool id of the item.
    pub fn insert(&mut self, obj: O, tick: K) -> usize {
        let new_id = self.build(obj, tick);
        self.attach(new_id);
        new_id
    }

    /// Touch (move to back) an item if the new tick is greater than its current tick.
    pub fn touch(&mut self, id: usize, tick: K) {
        if self.item_pool[id].tick >= tick {
            return;
        }
        self.item_pool[id].tick = tick;
        if self.last == Some(id) {
            return;
        }
        self.detach(id);
        self.attach(id);
    }

    /// Free an item, returning its slot to the pool for reuse.
    pub fn free(&mut self, id: usize) {
        self.detach(id);
        self.item_pool[id].prev = None;
        self.item_pool[id].next = None;
        self.free_items.push_back(id);
    }

    /// Iterate over items from front (least recently used) whose tick is <= `tick`.
    ///
    /// The callback returns `true` to stop iteration early, or `false` to continue.
    pub fn for_each_item_below<F>(&mut self, tick: K, mut func: F)
    where
        F: FnMut(O) -> bool,
    {
        let mut current = self.first;
        while let Some(id) = current {
            let item_tick: i64 = self.item_pool[id].tick.into();
            let target_tick: i64 = tick.into();
            if target_tick - item_tick < 0 {
                return;
            }
            let next = self.item_pool[id].next;
            let obj = self.item_pool[id].obj;
            if func(obj) {
                return;
            }
            current = next;
        }
    }

    /// Same as `for_each_item_below` but the callback does not return a bool.
    pub fn for_each_item_below_void<F>(&mut self, tick: K, mut func: F)
    where
        F: FnMut(O),
    {
        self.for_each_item_below(tick, |obj| {
            func(obj);
            false
        });
    }

    // -- private helpers --

    fn build(&mut self, obj: O, tick: K) -> usize {
        if let Some(id) = self.free_items.pop_front() {
            self.item_pool[id] = Item {
                obj,
                tick,
                next: None,
                prev: None,
            };
            id
        } else {
            let id = self.item_pool.len();
            self.item_pool.push_back(Item {
                obj,
                tick,
                next: None,
                prev: None,
            });
            id
        }
    }

    fn attach(&mut self, id: usize) {
        if self.first.is_none() {
            self.first = Some(id);
        }
        match self.last {
            None => {
                self.last = Some(id);
            }
            Some(last_id) => {
                self.item_pool[id].prev = Some(last_id);
                self.item_pool[last_id].next = Some(id);
                self.item_pool[id].next = None;
                self.last = Some(id);
            }
        }
    }

    fn detach(&mut self, id: usize) {
        let prev = self.item_pool[id].prev;
        let next = self.item_pool[id].next;

        if let Some(prev_id) = prev {
            self.item_pool[prev_id].next = next;
        }
        if let Some(next_id) = next {
            self.item_pool[next_id].prev = prev;
        }
        if self.first == Some(id) {
            self.first = next;
            if let Some(first_id) = self.first {
                self.item_pool[first_id].prev = None;
            }
        }
        if self.last == Some(id) {
            self.last = prev;
            if let Some(last_id) = self.last {
                self.item_pool[last_id].next = None;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_and_iterate() {
        let mut cache = LeastRecentlyUsedCache::<u32, i64>::new();
        cache.insert(10, 1);
        cache.insert(20, 2);
        cache.insert(30, 3);

        let mut items = Vec::new();
        cache.for_each_item_below_void(5, |obj| items.push(obj));
        assert_eq!(items, vec![10, 20, 30]);
    }

    #[test]
    fn test_touch_reorders() {
        let mut cache = LeastRecentlyUsedCache::<u32, i64>::new();
        let id1 = cache.insert(10, 1);
        let _id2 = cache.insert(20, 2);
        cache.touch(id1, 5);

        let mut items = Vec::new();
        cache.for_each_item_below_void(10, |obj| items.push(obj));
        // 20 (tick=2) should be first, then 10 (tick=5)
        assert_eq!(items, vec![20, 10]);
    }

    #[test]
    fn test_free() {
        let mut cache = LeastRecentlyUsedCache::<u32, i64>::new();
        let id1 = cache.insert(10, 1);
        let _id2 = cache.insert(20, 2);
        cache.free(id1);

        let mut items = Vec::new();
        cache.for_each_item_below_void(10, |obj| items.push(obj));
        assert_eq!(items, vec![20]);
    }
}
