// SPDX-FileCopyrightText: 2002 Niels Provos <provos@citi.umich.edu>
// SPDX-License-Identifier: BSD-2-Clause

//! Port of zuyu/src/common/tree.h
//!
//! Red-black tree implementation based on the FreeBSD tree.h.
//! Uses index-based references instead of raw pointers for safe Rust.
//!
//! A red-black tree is a binary search tree with the node color as an
//! extra attribute. It fulfills a set of conditions:
//! - every search path from the root to a leaf consists of the
//!   same number of black nodes,
//! - each red node (except for the root) has a black parent,
//! - each leaf node is black.
//!
//! Every operation on a red-black tree is bounded as O(lg n).
//! The maximum height of a red-black tree is 2lg (n+1).

/// Sentinel value representing a null/absent node index.
pub const NONE: usize = usize::MAX;

/// Node color in a red-black tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum RBColor {
    Black = 0,
    Red = 1,
}

/// Red-black tree entry (link fields) for an index-based tree.
/// Corresponds to `freebsd::RBEntry<T>` in C++.
#[derive(Debug, Clone, Copy)]
pub struct RBEntry {
    left: usize,
    right: usize,
    parent: usize,
    color: RBColor,
}

impl Default for RBEntry {
    fn default() -> Self {
        Self {
            left: NONE,
            right: NONE,
            parent: NONE,
            color: RBColor::Black,
        }
    }
}

impl RBEntry {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn left(&self) -> usize {
        self.left
    }

    #[inline]
    pub fn set_left(&mut self, e: usize) {
        self.left = e;
    }

    #[inline]
    pub fn right(&self) -> usize {
        self.right
    }

    #[inline]
    pub fn set_right(&mut self, e: usize) {
        self.right = e;
    }

    #[inline]
    pub fn parent(&self) -> usize {
        self.parent
    }

    #[inline]
    pub fn set_parent(&mut self, e: usize) {
        self.parent = e;
    }

    #[inline]
    pub fn is_black(&self) -> bool {
        self.color == RBColor::Black
    }

    #[inline]
    pub fn is_red(&self) -> bool {
        self.color == RBColor::Red
    }

    #[inline]
    pub fn color(&self) -> RBColor {
        self.color
    }

    #[inline]
    pub fn set_color(&mut self, c: RBColor) {
        self.color = c;
    }
}

/// Trait for types that contain an `RBEntry` link field.
/// Corresponds to the C++ `HasRBEntry` concept.
pub trait HasRBEntry {
    fn rb_entry(&self) -> &RBEntry;
    fn rb_entry_mut(&mut self) -> &mut RBEntry;
    fn set_rb_entry(&mut self, entry: RBEntry);
}

/// Root of a red-black tree, storing an optional root index.
/// Corresponds to `freebsd::RBHead<T>` in C++.
#[derive(Debug, Clone)]
pub struct RBHead {
    root: usize,
}

impl Default for RBHead {
    fn default() -> Self {
        Self { root: NONE }
    }
}

impl RBHead {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn root(&self) -> usize {
        self.root
    }

    #[inline]
    pub fn set_root(&mut self, root: usize) {
        self.root = root;
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.root == NONE
    }
}

// ── Helper accessors ──
// These mirror the free-function accessors in the C++ `tree.h`.

#[inline]
fn rb_left<T: HasRBEntry>(nodes: &[T], idx: usize) -> usize {
    nodes[idx].rb_entry().left()
}

#[inline]
fn rb_right<T: HasRBEntry>(nodes: &[T], idx: usize) -> usize {
    nodes[idx].rb_entry().right()
}

#[inline]
fn rb_parent<T: HasRBEntry>(nodes: &[T], idx: usize) -> usize {
    nodes[idx].rb_entry().parent()
}

#[inline]
fn rb_is_black<T: HasRBEntry>(nodes: &[T], idx: usize) -> bool {
    nodes[idx].rb_entry().is_black()
}

#[inline]
fn rb_is_red<T: HasRBEntry>(nodes: &[T], idx: usize) -> bool {
    nodes[idx].rb_entry().is_red()
}

#[inline]
fn rb_color<T: HasRBEntry>(nodes: &[T], idx: usize) -> RBColor {
    nodes[idx].rb_entry().color()
}

#[inline]
fn rb_set_color<T: HasRBEntry>(nodes: &mut [T], idx: usize, c: RBColor) {
    nodes[idx].rb_entry_mut().set_color(c);
}

#[inline]
fn rb_set_left<T: HasRBEntry>(nodes: &mut [T], idx: usize, e: usize) {
    nodes[idx].rb_entry_mut().set_left(e);
}

#[inline]
fn rb_set_right<T: HasRBEntry>(nodes: &mut [T], idx: usize, e: usize) {
    nodes[idx].rb_entry_mut().set_right(e);
}

#[inline]
fn rb_set_parent<T: HasRBEntry>(nodes: &mut [T], idx: usize, e: usize) {
    nodes[idx].rb_entry_mut().set_parent(e);
}

/// Initialize a node's RBEntry fields for insertion.
/// Corresponds to `RB_SET`.
fn rb_set<T: HasRBEntry>(nodes: &mut [T], elm: usize, parent: usize) {
    let entry = nodes[elm].rb_entry_mut();
    entry.set_parent(parent);
    entry.set_left(NONE);
    entry.set_right(NONE);
    entry.set_color(RBColor::Red);
}

/// Set black on first, red on second.
/// Corresponds to `RB_SET_BLACKRED`.
fn rb_set_blackred<T: HasRBEntry>(nodes: &mut [T], black: usize, red: usize) {
    rb_set_color(nodes, black, RBColor::Black);
    rb_set_color(nodes, red, RBColor::Red);
}

/// Left rotation. Corresponds to `RB_ROTATE_LEFT`.
fn rb_rotate_left<T: HasRBEntry>(head: &mut RBHead, nodes: &mut [T], elm: usize) -> usize {
    let tmp = rb_right(nodes, elm);
    let tmp_left = rb_left(nodes, tmp);
    rb_set_right(nodes, elm, tmp_left);
    if rb_right(nodes, elm) != NONE {
        rb_set_parent(nodes, tmp_left, elm);
    }

    let elm_parent = rb_parent(nodes, elm);
    rb_set_parent(nodes, tmp, elm_parent);
    if rb_parent(nodes, tmp) != NONE {
        if elm == rb_left(nodes, elm_parent) {
            rb_set_left(nodes, elm_parent, tmp);
        } else {
            rb_set_right(nodes, elm_parent, tmp);
        }
    } else {
        head.set_root(tmp);
    }

    rb_set_left(nodes, tmp, elm);
    rb_set_parent(nodes, elm, tmp);
    tmp
}

/// Right rotation. Corresponds to `RB_ROTATE_RIGHT`.
fn rb_rotate_right<T: HasRBEntry>(head: &mut RBHead, nodes: &mut [T], elm: usize) -> usize {
    let tmp = rb_left(nodes, elm);
    let tmp_right = rb_right(nodes, tmp);
    rb_set_left(nodes, elm, tmp_right);
    if rb_left(nodes, elm) != NONE {
        rb_set_parent(nodes, tmp_right, elm);
    }

    let elm_parent = rb_parent(nodes, elm);
    rb_set_parent(nodes, tmp, elm_parent);
    if rb_parent(nodes, tmp) != NONE {
        if elm == rb_left(nodes, elm_parent) {
            rb_set_left(nodes, elm_parent, tmp);
        } else {
            rb_set_right(nodes, elm_parent, tmp);
        }
    } else {
        head.set_root(tmp);
    }

    rb_set_right(nodes, tmp, elm);
    rb_set_parent(nodes, elm, tmp);
    tmp
}

/// Fix up colors after removal.
/// Corresponds to `RB_REMOVE_COLOR`.
fn rb_remove_color<T: HasRBEntry>(
    head: &mut RBHead,
    nodes: &mut [T],
    mut parent: usize,
    mut elm: usize,
) {
    while (elm == NONE || rb_is_black(nodes, elm)) && elm != head.root() {
        if rb_left(nodes, parent) == elm {
            let mut tmp = rb_right(nodes, parent);
            if rb_is_red(nodes, tmp) {
                rb_set_blackred(nodes, tmp, parent);
                rb_rotate_left(head, nodes, parent);
                tmp = rb_right(nodes, parent);
            }

            let tmp_left = rb_left(nodes, tmp);
            let tmp_right = rb_right(nodes, tmp);
            if (tmp_left == NONE || rb_is_black(nodes, tmp_left))
                && (tmp_right == NONE || rb_is_black(nodes, tmp_right))
            {
                rb_set_color(nodes, tmp, RBColor::Red);
                elm = parent;
                parent = rb_parent(nodes, elm);
            } else {
                if tmp_right == NONE || rb_is_black(nodes, tmp_right) {
                    let oleft = rb_left(nodes, tmp);
                    if oleft != NONE {
                        rb_set_color(nodes, oleft, RBColor::Black);
                    }
                    rb_set_color(nodes, tmp, RBColor::Red);
                    rb_rotate_right(head, nodes, tmp);
                    tmp = rb_right(nodes, parent);
                }

                rb_set_color(nodes, tmp, rb_color(nodes, parent));
                rb_set_color(nodes, parent, RBColor::Black);
                let tmp_right2 = rb_right(nodes, tmp);
                if tmp_right2 != NONE {
                    rb_set_color(nodes, tmp_right2, RBColor::Black);
                }

                rb_rotate_left(head, nodes, parent);
                elm = head.root();
                break;
            }
        } else {
            let mut tmp = rb_left(nodes, parent);
            if rb_is_red(nodes, tmp) {
                rb_set_blackred(nodes, tmp, parent);
                rb_rotate_right(head, nodes, parent);
                tmp = rb_left(nodes, parent);
            }

            let tmp_left = rb_left(nodes, tmp);
            let tmp_right = rb_right(nodes, tmp);
            if (tmp_left == NONE || rb_is_black(nodes, tmp_left))
                && (tmp_right == NONE || rb_is_black(nodes, tmp_right))
            {
                rb_set_color(nodes, tmp, RBColor::Red);
                elm = parent;
                parent = rb_parent(nodes, elm);
            } else {
                if tmp_left == NONE || rb_is_black(nodes, tmp_left) {
                    let oright = rb_right(nodes, tmp);
                    if oright != NONE {
                        rb_set_color(nodes, oright, RBColor::Black);
                    }
                    rb_set_color(nodes, tmp, RBColor::Red);
                    rb_rotate_left(head, nodes, tmp);
                    tmp = rb_left(nodes, parent);
                }

                rb_set_color(nodes, tmp, rb_color(nodes, parent));
                rb_set_color(nodes, parent, RBColor::Black);
                let tmp_left2 = rb_left(nodes, tmp);
                if tmp_left2 != NONE {
                    rb_set_color(nodes, tmp_left2, RBColor::Black);
                }

                rb_rotate_right(head, nodes, parent);
                elm = head.root();
                break;
            }
        }
    }

    if elm != NONE {
        rb_set_color(nodes, elm, RBColor::Black);
    }
}

/// Remove a node from the tree. Returns the index of the removed node (always `elm`).
/// Corresponds to `RB_REMOVE`.
pub fn rb_remove<T: HasRBEntry>(head: &mut RBHead, nodes: &mut [T], elm: usize) -> usize {
    let old = elm;
    let mut child: usize;
    let mut parent: usize;
    let color: RBColor;

    if rb_left(nodes, elm) == NONE {
        child = rb_right(nodes, elm);
        // Fall through to simple case below
        parent = rb_parent(nodes, elm);
        color = rb_color(nodes, elm);

        if child != NONE {
            rb_set_parent(nodes, child, parent);
        }
        if parent != NONE {
            if rb_left(nodes, parent) == elm {
                rb_set_left(nodes, parent, child);
            } else {
                rb_set_right(nodes, parent, child);
            }
        } else {
            head.set_root(child);
        }

        if color == RBColor::Black {
            rb_remove_color(head, nodes, parent, child);
        }

        return old;
    } else if rb_right(nodes, elm) == NONE {
        child = rb_left(nodes, elm);

        parent = rb_parent(nodes, elm);
        color = rb_color(nodes, elm);

        if child != NONE {
            rb_set_parent(nodes, child, parent);
        }
        if parent != NONE {
            if rb_left(nodes, parent) == elm {
                rb_set_left(nodes, parent, child);
            } else {
                rb_set_right(nodes, parent, child);
            }
        } else {
            head.set_root(child);
        }

        if color == RBColor::Black {
            rb_remove_color(head, nodes, parent, child);
        }

        return old;
    }

    // Both children exist — find in-order successor.
    let mut successor = rb_right(nodes, elm);
    loop {
        let left = rb_left(nodes, successor);
        if left == NONE {
            break;
        }
        successor = left;
    }

    child = rb_right(nodes, successor);
    parent = rb_parent(nodes, successor);
    color = rb_color(nodes, successor);

    if child != NONE {
        rb_set_parent(nodes, child, parent);
    }

    if parent != NONE {
        if rb_left(nodes, parent) == successor {
            rb_set_left(nodes, parent, child);
        } else {
            rb_set_right(nodes, parent, child);
        }
    } else {
        head.set_root(child);
    }

    if rb_parent(nodes, successor) == old {
        parent = successor;
    }

    // Copy old's entry into successor.
    let old_entry = *nodes[old].rb_entry();
    nodes[successor].set_rb_entry(old_entry);

    let old_parent = rb_parent(nodes, old);
    if old_parent != NONE {
        if rb_left(nodes, old_parent) == old {
            rb_set_left(nodes, old_parent, successor);
        } else {
            rb_set_right(nodes, old_parent, successor);
        }
    } else {
        head.set_root(successor);
    }

    let old_left = rb_left(nodes, old);
    rb_set_parent(nodes, old_left, successor);

    let old_right = rb_right(nodes, old);
    if old_right != NONE {
        rb_set_parent(nodes, old_right, successor);
    }

    if color == RBColor::Black {
        rb_remove_color(head, nodes, parent, child);
    }

    old
}

/// Fix up colors after insertion.
/// Corresponds to `RB_INSERT_COLOR`.
fn rb_insert_color<T: HasRBEntry>(head: &mut RBHead, nodes: &mut [T], mut elm: usize) {
    loop {
        let parent = rb_parent(nodes, elm);
        if parent == NONE || rb_is_black(nodes, parent) {
            break;
        }

        let gparent = rb_parent(nodes, parent);
        if parent == rb_left(nodes, gparent) {
            let tmp = rb_right(nodes, gparent);
            if tmp != NONE && rb_is_red(nodes, tmp) {
                rb_set_color(nodes, tmp, RBColor::Black);
                rb_set_blackred(nodes, parent, gparent);
                elm = gparent;
                continue;
            }

            if rb_right(nodes, parent) == elm {
                rb_rotate_left(head, nodes, parent);
                // After rotation: old elm is now parent of old parent
                // Swap references to match upstream
                elm = parent;
                let new_parent = rb_parent(nodes, elm);
                // parent and elm have swapped
                let _ = new_parent; // parent = new_parent below
            }

            let parent = rb_parent(nodes, elm);
            let gparent = rb_parent(nodes, parent);
            rb_set_blackred(nodes, parent, gparent);
            rb_rotate_right(head, nodes, gparent);
        } else {
            let tmp = rb_left(nodes, gparent);
            if tmp != NONE && rb_is_red(nodes, tmp) {
                rb_set_color(nodes, tmp, RBColor::Black);
                rb_set_blackred(nodes, parent, gparent);
                elm = gparent;
                continue;
            }

            if rb_left(nodes, parent) == elm {
                rb_rotate_right(head, nodes, parent);
                elm = parent;
            }

            let parent = rb_parent(nodes, elm);
            let gparent = rb_parent(nodes, parent);
            rb_set_blackred(nodes, parent, gparent);
            rb_rotate_left(head, nodes, gparent);
        }
    }

    rb_set_color(nodes, head.root(), RBColor::Black);
}

/// Insert a node into the tree using a comparator.
/// Returns `NONE` on success, or the index of the existing duplicate node.
/// Corresponds to `RB_INSERT`.
pub fn rb_insert<T: HasRBEntry, F>(head: &mut RBHead, nodes: &mut [T], elm: usize, cmp: F) -> usize
where
    F: Fn(&T, &T) -> i32,
{
    let mut parent = NONE;
    let mut tmp = head.root();
    let mut comp = 0i32;

    while tmp != NONE {
        parent = tmp;
        comp = cmp(&nodes[elm], &nodes[parent]);
        if comp < 0 {
            tmp = rb_left(nodes, tmp);
        } else if comp > 0 {
            tmp = rb_right(nodes, tmp);
        } else {
            return tmp;
        }
    }

    rb_set(nodes, elm, parent);

    if parent != NONE {
        if comp < 0 {
            rb_set_left(nodes, parent, elm);
        } else {
            rb_set_right(nodes, parent, elm);
        }
    } else {
        head.set_root(elm);
    }

    rb_insert_color(head, nodes, elm);
    NONE
}

/// Find a node matching `elm` using the comparator.
/// Returns `NONE` if not found.
/// Corresponds to `RB_FIND`.
pub fn rb_find<T: HasRBEntry, F>(head: &RBHead, nodes: &[T], elm: usize, cmp: F) -> usize
where
    F: Fn(&T, &T) -> i32,
{
    let mut tmp = head.root();
    while tmp != NONE {
        let comp = cmp(&nodes[elm], &nodes[tmp]);
        if comp < 0 {
            tmp = rb_left(nodes, tmp);
        } else if comp > 0 {
            tmp = rb_right(nodes, tmp);
        } else {
            return tmp;
        }
    }
    NONE
}

/// Find the first node >= `elm`. Returns `NONE` if not found.
/// Corresponds to `RB_NFIND`.
pub fn rb_nfind<T: HasRBEntry, F>(head: &RBHead, nodes: &[T], elm: usize, cmp: F) -> usize
where
    F: Fn(&T, &T) -> i32,
{
    let mut tmp = head.root();
    let mut res = NONE;
    while tmp != NONE {
        let comp = cmp(&nodes[elm], &nodes[tmp]);
        if comp < 0 {
            res = tmp;
            tmp = rb_left(nodes, tmp);
        } else if comp > 0 {
            tmp = rb_right(nodes, tmp);
        } else {
            return tmp;
        }
    }
    res
}

/// Find a node by key using a key comparator.
/// Corresponds to `RB_FIND_KEY`.
pub fn rb_find_key<T: HasRBEntry, K, F>(head: &RBHead, nodes: &[T], key: &K, cmp: F) -> usize
where
    F: Fn(&K, &T) -> i32,
{
    let mut tmp = head.root();
    while tmp != NONE {
        let comp = cmp(key, &nodes[tmp]);
        if comp < 0 {
            tmp = rb_left(nodes, tmp);
        } else if comp > 0 {
            tmp = rb_right(nodes, tmp);
        } else {
            return tmp;
        }
    }
    NONE
}

/// Find first node >= key. Corresponds to `RB_NFIND_KEY`.
pub fn rb_nfind_key<T: HasRBEntry, K, F>(head: &RBHead, nodes: &[T], key: &K, cmp: F) -> usize
where
    F: Fn(&K, &T) -> i32,
{
    let mut tmp = head.root();
    let mut res = NONE;
    while tmp != NONE {
        let comp = cmp(key, &nodes[tmp]);
        if comp < 0 {
            res = tmp;
            tmp = rb_left(nodes, tmp);
        } else if comp > 0 {
            tmp = rb_right(nodes, tmp);
        } else {
            return tmp;
        }
    }
    res
}

/// Find a node known to exist. Panics if the tree is malformed.
/// Corresponds to `RB_FIND_EXISTING`.
pub fn rb_find_existing<T: HasRBEntry, F>(head: &RBHead, nodes: &[T], elm: usize, cmp: F) -> usize
where
    F: Fn(&T, &T) -> i32,
{
    let mut tmp = head.root();
    loop {
        let comp = cmp(&nodes[elm], &nodes[tmp]);
        if comp < 0 {
            tmp = rb_left(nodes, tmp);
        } else if comp > 0 {
            tmp = rb_right(nodes, tmp);
        } else {
            return tmp;
        }
    }
}

/// Find a node by key known to exist. Panics if the tree is malformed.
/// Corresponds to `RB_FIND_EXISTING_KEY`.
pub fn rb_find_existing_key<T: HasRBEntry, K, F>(
    head: &RBHead,
    nodes: &[T],
    key: &K,
    cmp: F,
) -> usize
where
    F: Fn(&K, &T) -> i32,
{
    let mut tmp = head.root();
    loop {
        let comp = cmp(key, &nodes[tmp]);
        if comp < 0 {
            tmp = rb_left(nodes, tmp);
        } else if comp > 0 {
            tmp = rb_right(nodes, tmp);
        } else {
            return tmp;
        }
    }
}

/// Returns the in-order successor. Corresponds to `RB_NEXT`.
pub fn rb_next<T: HasRBEntry>(nodes: &[T], mut elm: usize) -> usize {
    if rb_right(nodes, elm) != NONE {
        elm = rb_right(nodes, elm);
        while rb_left(nodes, elm) != NONE {
            elm = rb_left(nodes, elm);
        }
    } else {
        if rb_parent(nodes, elm) != NONE && elm == rb_left(nodes, rb_parent(nodes, elm)) {
            elm = rb_parent(nodes, elm);
        } else {
            while rb_parent(nodes, elm) != NONE && elm == rb_right(nodes, rb_parent(nodes, elm)) {
                elm = rb_parent(nodes, elm);
            }
            elm = rb_parent(nodes, elm);
        }
    }
    elm
}

/// Returns the in-order predecessor. Corresponds to `RB_PREV`.
pub fn rb_prev<T: HasRBEntry>(nodes: &[T], mut elm: usize) -> usize {
    if rb_left(nodes, elm) != NONE {
        elm = rb_left(nodes, elm);
        while rb_right(nodes, elm) != NONE {
            elm = rb_right(nodes, elm);
        }
    } else {
        if rb_parent(nodes, elm) != NONE && elm == rb_right(nodes, rb_parent(nodes, elm)) {
            elm = rb_parent(nodes, elm);
        } else {
            while rb_parent(nodes, elm) != NONE && elm == rb_left(nodes, rb_parent(nodes, elm)) {
                elm = rb_parent(nodes, elm);
            }
            elm = rb_parent(nodes, elm);
        }
    }
    elm
}

/// Returns the minimum node. Corresponds to `RB_MIN`.
pub fn rb_min<T: HasRBEntry>(head: &RBHead, nodes: &[T]) -> usize {
    let mut tmp = head.root();
    let mut parent = NONE;
    while tmp != NONE {
        parent = tmp;
        tmp = rb_left(nodes, tmp);
    }
    parent
}

/// Returns the maximum node. Corresponds to `RB_MAX`.
pub fn rb_max<T: HasRBEntry>(head: &RBHead, nodes: &[T]) -> usize {
    let mut tmp = head.root();
    let mut parent = NONE;
    while tmp != NONE {
        parent = tmp;
        tmp = rb_right(nodes, tmp);
    }
    parent
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone)]
    struct TestNode {
        key: i32,
        entry: RBEntry,
    }

    impl TestNode {
        fn new(key: i32) -> Self {
            Self {
                key,
                entry: RBEntry::default(),
            }
        }
    }

    impl HasRBEntry for TestNode {
        fn rb_entry(&self) -> &RBEntry {
            &self.entry
        }
        fn rb_entry_mut(&mut self) -> &mut RBEntry {
            &mut self.entry
        }
        fn set_rb_entry(&mut self, entry: RBEntry) {
            self.entry = entry;
        }
    }

    fn cmp_test(a: &TestNode, b: &TestNode) -> i32 {
        if a.key < b.key {
            -1
        } else if a.key > b.key {
            1
        } else {
            0
        }
    }

    fn key_cmp(key: &i32, node: &TestNode) -> i32 {
        if *key < node.key {
            -1
        } else if *key > node.key {
            1
        } else {
            0
        }
    }

    #[test]
    fn test_insert_and_find() {
        let mut head = RBHead::new();
        let mut nodes = vec![
            TestNode::new(10),
            TestNode::new(5),
            TestNode::new(15),
            TestNode::new(3),
            TestNode::new(7),
        ];

        for i in 0..nodes.len() {
            let result = rb_insert(&mut head, &mut nodes, i, cmp_test);
            assert_eq!(result, NONE, "insert should succeed for unique keys");
        }

        assert!(!head.is_empty());

        // Find each node
        for i in 0..nodes.len() {
            let found = rb_find(&head, &nodes, i, cmp_test);
            assert_ne!(found, NONE);
            assert_eq!(nodes[found].key, nodes[i].key);
        }

        // Find by key
        let found = rb_find_key(&head, &nodes, &10, key_cmp);
        assert_ne!(found, NONE);
        assert_eq!(nodes[found].key, 10);

        // Not found
        let found = rb_find_key(&head, &nodes, &99, key_cmp);
        assert_eq!(found, NONE);
    }

    #[test]
    fn test_min_max() {
        let mut head = RBHead::new();
        let mut nodes = vec![
            TestNode::new(10),
            TestNode::new(5),
            TestNode::new(15),
            TestNode::new(3),
            TestNode::new(7),
        ];

        for i in 0..nodes.len() {
            rb_insert(&mut head, &mut nodes, i, cmp_test);
        }

        let min = rb_min(&head, &nodes);
        assert_ne!(min, NONE);
        assert_eq!(nodes[min].key, 3);

        let max = rb_max(&head, &nodes);
        assert_ne!(max, NONE);
        assert_eq!(nodes[max].key, 15);
    }

    #[test]
    fn test_next_prev() {
        let mut head = RBHead::new();
        let mut nodes = vec![
            TestNode::new(10),
            TestNode::new(5),
            TestNode::new(15),
            TestNode::new(3),
            TestNode::new(7),
        ];

        for i in 0..nodes.len() {
            rb_insert(&mut head, &mut nodes, i, cmp_test);
        }

        // Walk forward from min
        let mut current = rb_min(&head, &nodes);
        let mut keys = Vec::new();
        while current != NONE {
            keys.push(nodes[current].key);
            current = rb_next(&nodes, current);
        }
        assert_eq!(keys, vec![3, 5, 7, 10, 15]);

        // Walk backward from max
        let mut current = rb_max(&head, &nodes);
        let mut keys = Vec::new();
        while current != NONE {
            keys.push(nodes[current].key);
            current = rb_prev(&nodes, current);
        }
        assert_eq!(keys, vec![15, 10, 7, 5, 3]);
    }

    #[test]
    fn test_remove() {
        let mut head = RBHead::new();
        let mut nodes = vec![
            TestNode::new(10),
            TestNode::new(5),
            TestNode::new(15),
            TestNode::new(3),
            TestNode::new(7),
        ];

        for i in 0..nodes.len() {
            rb_insert(&mut head, &mut nodes, i, cmp_test);
        }

        // Find and remove the node with key 5
        let idx = rb_find_key(&head, &nodes, &5, key_cmp);
        assert_ne!(idx, NONE);
        rb_remove(&mut head, &mut nodes, idx);

        // Should no longer be findable
        let idx = rb_find_key(&head, &nodes, &5, key_cmp);
        assert_eq!(idx, NONE);

        // Other nodes still present
        for &key in &[3, 7, 10, 15] {
            let found = rb_find_key(&head, &nodes, &key, key_cmp);
            assert_ne!(found, NONE);
        }
    }

    #[test]
    fn test_duplicate_insert() {
        let mut head = RBHead::new();
        let mut nodes = vec![TestNode::new(10), TestNode::new(10)];

        let result = rb_insert(&mut head, &mut nodes, 0, cmp_test);
        assert_eq!(result, NONE); // success

        let result = rb_insert(&mut head, &mut nodes, 1, cmp_test);
        assert_ne!(result, NONE); // duplicate returns existing
        assert_eq!(nodes[result].key, 10);
    }

    #[test]
    fn test_nfind() {
        let mut head = RBHead::new();
        let mut nodes = vec![
            TestNode::new(10),
            TestNode::new(20),
            TestNode::new(30),
            TestNode::new(15), // search key
        ];

        rb_insert(&mut head, &mut nodes, 0, cmp_test);
        rb_insert(&mut head, &mut nodes, 1, cmp_test);
        rb_insert(&mut head, &mut nodes, 2, cmp_test);

        // nfind 15 should find 20 (first >= 15)
        let found = rb_nfind(&head, &nodes, 3, cmp_test);
        assert_ne!(found, NONE);
        assert_eq!(nodes[found].key, 20);
    }

    #[test]
    fn test_empty_tree() {
        let head = RBHead::new();
        let nodes: Vec<TestNode> = Vec::new();

        assert!(head.is_empty());
        assert_eq!(rb_min(&head, &nodes), NONE);
        assert_eq!(rb_max(&head, &nodes), NONE);
    }
}
