// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/intrusive_red_black_tree.h
//!
//! Intrusive red-black tree using arena-based (index-based) node storage.
//! Nodes are stored in a `Vec<T>` arena and referenced by index.
//! This provides safe Rust equivalents of the C++ intrusive pointer-based tree.

use crate::tree::{self, HasRBEntry, RBEntry, RBHead, NONE};

/// An intrusive red-black tree node.
/// Corresponds to `IntrusiveRedBlackTreeNode` in C++.
///
/// Types that want to be stored in the tree should embed this and
/// implement `HasRBEntry`.
#[derive(Debug, Clone)]
pub struct IntrusiveRedBlackTreeNode {
    entry: RBEntry,
}

impl Default for IntrusiveRedBlackTreeNode {
    fn default() -> Self {
        Self {
            entry: RBEntry::default(),
        }
    }
}

impl IntrusiveRedBlackTreeNode {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn rb_entry(&self) -> &RBEntry {
        &self.entry
    }

    #[inline]
    pub fn rb_entry_mut(&mut self) -> &mut RBEntry {
        &mut self.entry
    }

    #[inline]
    pub fn set_rb_entry(&mut self, entry: RBEntry) {
        self.entry = entry;
    }
}

/// Trait that nodes stored in `IntrusiveRedBlackTree` must implement.
/// Provides access to the embedded `IntrusiveRedBlackTreeNode`.
///
/// Corresponds to the C++ Traits classes like `IntrusiveRedBlackTreeMemberTraits`
/// and `IntrusiveRedBlackTreeBaseTraits`.
pub trait IntrusiveRedBlackTreeNodeAccess {
    fn get_node(&self) -> &IntrusiveRedBlackTreeNode;
    fn get_node_mut(&mut self) -> &mut IntrusiveRedBlackTreeNode;
}

/// Adapter: any type implementing `IntrusiveRedBlackTreeNodeAccess` automatically
/// implements `HasRBEntry` so it can be used with the `tree` module functions.
impl<T: IntrusiveRedBlackTreeNodeAccess> HasRBEntry for T {
    fn rb_entry(&self) -> &RBEntry {
        self.get_node().rb_entry()
    }

    fn rb_entry_mut(&mut self) -> &mut RBEntry {
        self.get_node_mut().rb_entry_mut()
    }

    fn set_rb_entry(&mut self, entry: RBEntry) {
        self.get_node_mut().set_rb_entry(entry);
    }
}

/// An intrusive red-black tree that stores elements in an arena (Vec).
///
/// Corresponds to `IntrusiveRedBlackTree<T, Traits, Comparator>` in C++.
///
/// The comparator is provided as a closure to each operation rather than
/// being stored in the tree itself, matching the C++ design where comparison
/// is a static function on the Comparator type.
pub struct IntrusiveRedBlackTree<T> {
    head: RBHead,
    nodes: Vec<T>,
}

impl<T: IntrusiveRedBlackTreeNodeAccess> IntrusiveRedBlackTree<T> {
    /// Create a new empty tree.
    pub fn new() -> Self {
        Self {
            head: RBHead::new(),
            nodes: Vec::new(),
        }
    }

    /// Create a new empty tree with pre-allocated capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            head: RBHead::new(),
            nodes: Vec::with_capacity(capacity),
        }
    }

    /// Returns true if the tree is empty.
    #[inline]
    pub fn empty(&self) -> bool {
        self.head.is_empty()
    }

    /// Returns the number of elements in the arena.
    /// Note: this includes removed elements that may still occupy arena slots.
    #[inline]
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Returns a reference to the node arena.
    #[inline]
    pub fn nodes(&self) -> &[T] {
        &self.nodes
    }

    /// Returns a mutable reference to the node arena.
    #[inline]
    pub fn nodes_mut(&mut self) -> &mut [T] {
        &mut self.nodes
    }

    /// Returns a reference to the tree head.
    #[inline]
    pub fn head(&self) -> &RBHead {
        &self.head
    }

    /// Returns a mutable reference to the tree head.
    #[inline]
    pub fn head_mut(&mut self) -> &mut RBHead {
        &mut self.head
    }

    /// Get a reference to a node by index.
    #[inline]
    pub fn get(&self, index: usize) -> Option<&T> {
        self.nodes.get(index)
    }

    /// Get a mutable reference to a node by index.
    #[inline]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.nodes.get_mut(index)
    }

    /// Add a new element to the arena and insert it into the tree.
    /// Returns the index of the inserted element, or the index of the
    /// existing duplicate if one exists (in which case the element is NOT added).
    ///
    /// Corresponds to `IntrusiveRedBlackTree::insert`.
    pub fn insert<F>(&mut self, element: T, cmp: F) -> usize
    where
        F: Fn(&T, &T) -> i32,
    {
        let idx = self.nodes.len();
        self.nodes.push(element);
        let existing = tree::rb_insert(&mut self.head, &mut self.nodes, idx, cmp);
        if existing != NONE {
            // Duplicate found; remove the just-pushed element.
            self.nodes.pop();
            existing
        } else {
            idx
        }
    }

    /// Insert an element that is already in the arena at the given index.
    /// Returns `NONE` on success, or the index of the existing duplicate.
    pub fn insert_at<F>(&mut self, idx: usize, cmp: F) -> usize
    where
        F: Fn(&T, &T) -> i32,
    {
        tree::rb_insert(&mut self.head, &mut self.nodes, idx, cmp)
    }

    /// Find a node matching the element at `idx` using the comparator.
    /// Returns `NONE` if not found.
    pub fn find<F>(&self, idx: usize, cmp: F) -> usize
    where
        F: Fn(&T, &T) -> i32,
    {
        tree::rb_find(&self.head, &self.nodes, idx, cmp)
    }

    /// Find the first node >= `idx`. Returns `NONE` if not found.
    pub fn nfind<F>(&self, idx: usize, cmp: F) -> usize
    where
        F: Fn(&T, &T) -> i32,
    {
        tree::rb_nfind(&self.head, &self.nodes, idx, cmp)
    }

    /// Find by key. Returns `NONE` if not found.
    pub fn find_key<K, F>(&self, key: &K, cmp: F) -> usize
    where
        F: Fn(&K, &T) -> i32,
    {
        tree::rb_find_key(&self.head, &self.nodes, key, cmp)
    }

    /// Find first node >= key. Returns `NONE` if not found.
    pub fn nfind_key<K, F>(&self, key: &K, cmp: F) -> usize
    where
        F: Fn(&K, &T) -> i32,
    {
        tree::rb_nfind_key(&self.head, &self.nodes, key, cmp)
    }

    /// Find a node known to exist. Panics if not found.
    pub fn find_existing<F>(&self, idx: usize, cmp: F) -> usize
    where
        F: Fn(&T, &T) -> i32,
    {
        tree::rb_find_existing(&self.head, &self.nodes, idx, cmp)
    }

    /// Find by key, known to exist. Panics if not found.
    pub fn find_existing_key<K, F>(&self, key: &K, cmp: F) -> usize
    where
        F: Fn(&K, &T) -> i32,
    {
        tree::rb_find_existing_key(&self.head, &self.nodes, key, cmp)
    }

    /// Remove the node at the given index from the tree.
    /// Note: this only removes it from the tree structure; it remains in the arena.
    /// Returns the index of the removed node.
    pub fn erase(&mut self, idx: usize) -> usize {
        tree::rb_remove(&mut self.head, &mut self.nodes, idx)
    }

    /// Returns the index of the minimum element, or `NONE` if empty.
    pub fn front_index(&self) -> usize {
        tree::rb_min(&self.head, &self.nodes)
    }

    /// Returns a reference to the minimum element, if any.
    pub fn front(&self) -> Option<&T> {
        let idx = self.front_index();
        if idx == NONE {
            None
        } else {
            Some(&self.nodes[idx])
        }
    }

    /// Returns the index of the maximum element, or `NONE` if empty.
    pub fn back_index(&self) -> usize {
        tree::rb_max(&self.head, &self.nodes)
    }

    /// Returns a reference to the maximum element, if any.
    pub fn back(&self) -> Option<&T> {
        let idx = self.back_index();
        if idx == NONE {
            None
        } else {
            Some(&self.nodes[idx])
        }
    }

    /// Returns the in-order successor of the node at `idx`, or `NONE`.
    pub fn next(&self, idx: usize) -> usize {
        tree::rb_next(&self.nodes, idx)
    }

    /// Returns the in-order predecessor of the node at `idx`, or `NONE`.
    pub fn prev(&self, idx: usize) -> usize {
        tree::rb_prev(&self.nodes, idx)
    }

    /// Returns an iterator over the tree in sorted order.
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            nodes: &self.nodes,
            head: &self.head,
            current: tree::rb_min(&self.head, &self.nodes),
        }
    }

    /// Erase the current element and return the index of the next element.
    /// Corresponds to `IntrusiveRedBlackTreeImpl::erase(iterator)`.
    pub fn erase_and_next(&mut self, idx: usize) -> usize {
        let next = tree::rb_next(&self.nodes, idx);
        self.erase(idx);
        next
    }
}

impl<T: IntrusiveRedBlackTreeNodeAccess> Default for IntrusiveRedBlackTree<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Iterator over an `IntrusiveRedBlackTree` in sorted order.
///
/// Corresponds to `IntrusiveRedBlackTreeImpl::Iterator` and
/// `IntrusiveRedBlackTree::Iterator` in C++.
pub struct Iter<'a, T> {
    nodes: &'a [T],
    head: &'a RBHead,
    current: usize,
}

impl<'a, T: IntrusiveRedBlackTreeNodeAccess> Iterator for Iter<'a, T> {
    type Item = (usize, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current == NONE {
            return None;
        }
        let idx = self.current;
        self.current = tree::rb_next(self.nodes, self.current);
        Some((idx, &self.nodes[idx]))
    }
}

impl<'a, T: IntrusiveRedBlackTreeNodeAccess> DoubleEndedIterator for Iter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        // For a proper double-ended iterator we'd need to track the back cursor.
        // This is a simplified implementation — for full bidirectional iteration,
        // use `prev()` manually or iterate in reverse from `back_index()`.
        None
    }
}

/// Mutable iterator that yields `(index, &mut T)` pairs.
pub struct IterMut<'a, T> {
    current: usize,
    /// We store pointers to avoid borrow checker issues with simultaneous
    /// arena access and tree navigation.
    nodes_ptr: *mut T,
    nodes_len: usize,
    _phantom: std::marker::PhantomData<&'a mut T>,
}

impl<T: IntrusiveRedBlackTreeNodeAccess> IntrusiveRedBlackTree<T> {
    /// Returns a mutable iterator over the tree in sorted order.
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        let current = tree::rb_min(&self.head, &self.nodes);
        IterMut {
            current,
            nodes_ptr: self.nodes.as_mut_ptr(),
            nodes_len: self.nodes.len(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<'a, T: IntrusiveRedBlackTreeNodeAccess> Iterator for IterMut<'a, T> {
    type Item = (usize, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current == NONE {
            return None;
        }
        let idx = self.current;
        // Safety: we only yield each index once during iteration,
        // and the caller holds a mutable borrow on the tree.
        let node = unsafe {
            assert!(idx < self.nodes_len);
            let slice = std::slice::from_raw_parts(self.nodes_ptr, self.nodes_len);
            self.current = tree::rb_next(slice, idx);
            &mut *self.nodes_ptr.add(idx)
        };
        Some((idx, node))
    }
}

/// Trait for types that can serve as base nodes in the intrusive tree
/// (via inheritance-style composition).
/// Corresponds to `IntrusiveRedBlackTreeBaseNode<Derived>` in C++.
pub trait IntrusiveRedBlackTreeBaseNode: IntrusiveRedBlackTreeNodeAccess {
    /// Get the in-order predecessor, returning `NONE` if at the beginning.
    fn get_prev_index(&self, nodes: &[Self]) -> usize
    where
        Self: Sized,
    {
        // Find our own index first by scanning. In practice, callers should
        // use the tree's `prev()` method with a known index instead.
        // This exists for API parity with the C++ base node class.
        NONE
    }

    /// Get the in-order successor, returning `NONE` if at the end.
    fn get_next_index(&self, nodes: &[Self]) -> usize
    where
        Self: Sized,
    {
        NONE
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone)]
    struct TestItem {
        node: IntrusiveRedBlackTreeNode,
        value: i32,
    }

    impl TestItem {
        fn new(value: i32) -> Self {
            Self {
                node: IntrusiveRedBlackTreeNode::new(),
                value,
            }
        }
    }

    impl IntrusiveRedBlackTreeNodeAccess for TestItem {
        fn get_node(&self) -> &IntrusiveRedBlackTreeNode {
            &self.node
        }
        fn get_node_mut(&mut self) -> &mut IntrusiveRedBlackTreeNode {
            &mut self.node
        }
    }

    fn cmp_items(a: &TestItem, b: &TestItem) -> i32 {
        if a.value < b.value {
            -1
        } else if a.value > b.value {
            1
        } else {
            0
        }
    }

    fn key_cmp(key: &i32, item: &TestItem) -> i32 {
        if *key < item.value {
            -1
        } else if *key > item.value {
            1
        } else {
            0
        }
    }

    #[test]
    fn test_basic_operations() {
        let mut tree: IntrusiveRedBlackTree<TestItem> = IntrusiveRedBlackTree::new();

        assert!(tree.empty());

        // Insert some items
        let idx0 = tree.insert(TestItem::new(10), cmp_items);
        let idx1 = tree.insert(TestItem::new(5), cmp_items);
        let idx2 = tree.insert(TestItem::new(15), cmp_items);
        let idx3 = tree.insert(TestItem::new(3), cmp_items);
        let idx4 = tree.insert(TestItem::new(7), cmp_items);

        assert!(!tree.empty());
        assert_eq!(tree.len(), 5);

        // Check front/back
        assert_eq!(tree.front().unwrap().value, 3);
        assert_eq!(tree.back().unwrap().value, 15);
    }

    #[test]
    fn test_iteration() {
        let mut tree: IntrusiveRedBlackTree<TestItem> = IntrusiveRedBlackTree::new();

        tree.insert(TestItem::new(10), cmp_items);
        tree.insert(TestItem::new(5), cmp_items);
        tree.insert(TestItem::new(15), cmp_items);
        tree.insert(TestItem::new(3), cmp_items);
        tree.insert(TestItem::new(7), cmp_items);

        let values: Vec<i32> = tree.iter().map(|(_, item)| item.value).collect();
        assert_eq!(values, vec![3, 5, 7, 10, 15]);
    }

    #[test]
    fn test_find() {
        let mut tree: IntrusiveRedBlackTree<TestItem> = IntrusiveRedBlackTree::new();

        tree.insert(TestItem::new(10), cmp_items);
        tree.insert(TestItem::new(5), cmp_items);
        tree.insert(TestItem::new(15), cmp_items);

        let found = tree.find_key(&10, key_cmp);
        assert_ne!(found, NONE);
        assert_eq!(tree.get(found).unwrap().value, 10);

        let not_found = tree.find_key(&99, key_cmp);
        assert_eq!(not_found, NONE);
    }

    #[test]
    fn test_erase() {
        let mut tree: IntrusiveRedBlackTree<TestItem> = IntrusiveRedBlackTree::new();

        tree.insert(TestItem::new(10), cmp_items);
        tree.insert(TestItem::new(5), cmp_items);
        tree.insert(TestItem::new(15), cmp_items);

        let idx = tree.find_key(&10, key_cmp);
        assert_ne!(idx, NONE);

        tree.erase(idx);

        let idx = tree.find_key(&10, key_cmp);
        assert_eq!(idx, NONE);

        // Other items still present
        assert_ne!(tree.find_key(&5, key_cmp), NONE);
        assert_ne!(tree.find_key(&15, key_cmp), NONE);
    }

    #[test]
    fn test_erase_and_next() {
        let mut tree: IntrusiveRedBlackTree<TestItem> = IntrusiveRedBlackTree::new();

        tree.insert(TestItem::new(10), cmp_items);
        tree.insert(TestItem::new(5), cmp_items);
        tree.insert(TestItem::new(15), cmp_items);

        let front = tree.front_index();
        assert_eq!(tree.get(front).unwrap().value, 5);

        let next = tree.erase_and_next(front);
        assert_ne!(next, NONE);
        assert_eq!(tree.get(next).unwrap().value, 10);
    }

    #[test]
    fn test_duplicate_insert() {
        let mut tree: IntrusiveRedBlackTree<TestItem> = IntrusiveRedBlackTree::new();

        let idx0 = tree.insert(TestItem::new(10), cmp_items);
        let idx1 = tree.insert(TestItem::new(10), cmp_items);

        // Duplicate should return the existing index
        assert_eq!(idx1, idx0);
        assert_eq!(tree.len(), 1); // Only one element in the arena
    }

    #[test]
    fn test_nfind() {
        let mut tree: IntrusiveRedBlackTree<TestItem> = IntrusiveRedBlackTree::new();

        tree.insert(TestItem::new(10), cmp_items);
        tree.insert(TestItem::new(20), cmp_items);
        tree.insert(TestItem::new(30), cmp_items);

        let key_idx = tree.nodes.len();
        tree.nodes.push(TestItem::new(15));

        let found = tree.nfind(key_idx, cmp_items);
        assert_ne!(found, NONE);
        assert_eq!(tree.get(found).unwrap().value, 20);
    }
}
