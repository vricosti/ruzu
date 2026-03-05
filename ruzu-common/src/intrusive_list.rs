//! Port of zuyu/src/common/intrusive_list.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! This is a simplified, index-based intrusive doubly-linked list for Rust.
//! The C++ version uses raw pointers embedded in the node structs themselves.
//! Since Rust does not allow self-referential structs safely, this
//! implementation uses an arena (Vec) with indices as "pointers".
//!
//! This is critical for the kernel scheduler.

/// Index used as a "pointer" within the intrusive list arena.
/// `usize::MAX` serves as the null sentinel (equivalent to pointing to self
/// in the C++ version's unlinked state).
pub type NodeIndex = usize;

/// Sentinel value representing "no node" / null.
pub const INVALID_INDEX: NodeIndex = usize::MAX;

/// A node in the intrusive list, stored in an external arena.
#[derive(Debug, Clone)]
pub struct IntrusiveListNode {
    pub prev: NodeIndex,
    pub next: NodeIndex,
}

impl Default for IntrusiveListNode {
    fn default() -> Self {
        Self {
            prev: INVALID_INDEX,
            next: INVALID_INDEX,
        }
    }
}

impl IntrusiveListNode {
    pub fn new() -> Self {
        Self::default()
    }

    /// A node is "linked" if it points to something other than the sentinel.
    pub fn is_linked(&self) -> bool {
        self.next != INVALID_INDEX
    }
}

/// An intrusive doubly-linked list that uses indices into an external arena.
///
/// The list itself stores a sentinel "root" node. The root's `next` points to
/// the first real element, and `prev` points to the last. An empty list has
/// the root pointing to itself (represented by `ROOT_SENTINEL`).
///
/// Users of this list must maintain their own arena (e.g., `Vec<T>`) and
/// provide a way to access each element's `IntrusiveListNode`. The list only
/// stores indices into that arena.
///
/// This mirrors the C++ `Common::IntrusiveList` and
/// `Common::impl::IntrusiveListImpl`.
pub struct IntrusiveList {
    /// Sentinel index representing the root. We use `INVALID_INDEX` as the
    /// root sentinel (the root is not stored in the arena).
    root_prev: NodeIndex,
    root_next: NodeIndex,
    len: usize,
}

impl Default for IntrusiveList {
    fn default() -> Self {
        Self::new()
    }
}

impl IntrusiveList {
    /// Create a new empty intrusive list.
    pub fn new() -> Self {
        Self {
            root_prev: INVALID_INDEX,
            root_next: INVALID_INDEX,
            len: 0,
        }
    }

    /// Returns `true` if the list is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Returns the number of elements in the list.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns the index of the first element, or `INVALID_INDEX` if empty.
    pub fn front(&self) -> NodeIndex {
        self.root_next
    }

    /// Returns the index of the last element, or `INVALID_INDEX` if empty.
    pub fn back(&self) -> NodeIndex {
        self.root_prev
    }

    /// Push `node_index` to the back of the list.
    ///
    /// The caller must ensure `nodes[node_index]` is not already linked.
    pub fn push_back(&mut self, node_index: NodeIndex, nodes: &mut [IntrusiveListNode]) {
        debug_assert!(!nodes[node_index].is_linked());

        if self.is_empty() {
            self.root_next = node_index;
            self.root_prev = node_index;
            nodes[node_index].prev = INVALID_INDEX;
            nodes[node_index].next = INVALID_INDEX;
        } else {
            let old_last = self.root_prev;
            nodes[old_last].next = node_index;
            nodes[node_index].prev = old_last;
            nodes[node_index].next = INVALID_INDEX;
            self.root_prev = node_index;
        }
        // Mark as linked by setting next to a special "end of list" marker.
        // We use INVALID_INDEX to mean "end of list / root", which is also the
        // unlinked state. To distinguish, we track via `len`.
        self.len += 1;
    }

    /// Push `node_index` to the front of the list.
    pub fn push_front(&mut self, node_index: NodeIndex, nodes: &mut [IntrusiveListNode]) {
        debug_assert!(!nodes[node_index].is_linked() || self.is_empty());

        if self.is_empty() {
            self.root_next = node_index;
            self.root_prev = node_index;
            nodes[node_index].prev = INVALID_INDEX;
            nodes[node_index].next = INVALID_INDEX;
        } else {
            let old_first = self.root_next;
            nodes[old_first].prev = node_index;
            nodes[node_index].next = old_first;
            nodes[node_index].prev = INVALID_INDEX;
            self.root_next = node_index;
        }
        self.len += 1;
    }

    /// Remove the front element. Returns its index, or `INVALID_INDEX` if empty.
    pub fn pop_front(&mut self, nodes: &mut [IntrusiveListNode]) -> NodeIndex {
        if self.is_empty() {
            return INVALID_INDEX;
        }
        let front = self.root_next;
        self.erase(front, nodes);
        front
    }

    /// Remove the back element. Returns its index, or `INVALID_INDEX` if empty.
    pub fn pop_back(&mut self, nodes: &mut [IntrusiveListNode]) -> NodeIndex {
        if self.is_empty() {
            return INVALID_INDEX;
        }
        let back = self.root_prev;
        self.erase(back, nodes);
        back
    }

    /// Remove the element at `node_index` from the list.
    pub fn erase(&mut self, node_index: NodeIndex, nodes: &mut [IntrusiveListNode]) {
        let prev = nodes[node_index].prev;
        let next = nodes[node_index].next;

        // Update previous node's next pointer
        if prev != INVALID_INDEX {
            nodes[prev].next = next;
        } else {
            // node was the first element
            self.root_next = next;
        }

        // Update next node's prev pointer
        if next != INVALID_INDEX {
            nodes[next].prev = prev;
        } else {
            // node was the last element
            self.root_prev = prev;
        }

        // Unlink the node
        nodes[node_index].prev = INVALID_INDEX;
        nodes[node_index].next = INVALID_INDEX;
        self.len -= 1;

        if self.len == 0 {
            self.root_next = INVALID_INDEX;
            self.root_prev = INVALID_INDEX;
        }
    }

    /// Insert `node_index` before the element at `before_index`.
    pub fn insert_before(
        &mut self,
        before_index: NodeIndex,
        node_index: NodeIndex,
        nodes: &mut [IntrusiveListNode],
    ) {
        if before_index == INVALID_INDEX {
            // Insert at end
            self.push_back(node_index, nodes);
            return;
        }

        let prev = nodes[before_index].prev;
        nodes[node_index].next = before_index;
        nodes[node_index].prev = prev;
        nodes[before_index].prev = node_index;

        if prev != INVALID_INDEX {
            nodes[prev].next = node_index;
        } else {
            self.root_next = node_index;
        }

        self.len += 1;
    }

    /// Clear the list, unlinking all nodes.
    pub fn clear(&mut self, nodes: &mut [IntrusiveListNode]) {
        let mut current = self.root_next;
        while current != INVALID_INDEX {
            let next = nodes[current].next;
            nodes[current].prev = INVALID_INDEX;
            nodes[current].next = INVALID_INDEX;
            current = next;
        }
        self.root_next = INVALID_INDEX;
        self.root_prev = INVALID_INDEX;
        self.len = 0;
    }

    /// Iterate over all node indices from front to back.
    pub fn iter<'a>(&'a self, nodes: &'a [IntrusiveListNode]) -> IntrusiveListIter<'a> {
        IntrusiveListIter {
            current: self.root_next,
            nodes,
            remaining: self.len,
        }
    }
}

/// Forward iterator over intrusive list node indices.
pub struct IntrusiveListIter<'a> {
    current: NodeIndex,
    nodes: &'a [IntrusiveListNode],
    remaining: usize,
}

impl<'a> Iterator for IntrusiveListIter<'a> {
    type Item = NodeIndex;

    fn next(&mut self) -> Option<NodeIndex> {
        if self.remaining == 0 || self.current == INVALID_INDEX {
            return None;
        }
        let idx = self.current;
        self.current = self.nodes[idx].next;
        self.remaining -= 1;
        Some(idx)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl<'a> ExactSizeIterator for IntrusiveListIter<'a> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_back_and_iter() {
        let mut nodes = vec![IntrusiveListNode::new(); 5];
        let mut list = IntrusiveList::new();

        list.push_back(0, &mut nodes);
        list.push_back(1, &mut nodes);
        list.push_back(2, &mut nodes);

        let indices: Vec<usize> = list.iter(&nodes).collect();
        assert_eq!(indices, vec![0, 1, 2]);
        assert_eq!(list.len(), 3);
    }

    #[test]
    fn test_push_front() {
        let mut nodes = vec![IntrusiveListNode::new(); 3];
        let mut list = IntrusiveList::new();

        list.push_front(0, &mut nodes);
        list.push_front(1, &mut nodes);
        list.push_front(2, &mut nodes);

        let indices: Vec<usize> = list.iter(&nodes).collect();
        assert_eq!(indices, vec![2, 1, 0]);
    }

    #[test]
    fn test_erase() {
        let mut nodes = vec![IntrusiveListNode::new(); 3];
        let mut list = IntrusiveList::new();

        list.push_back(0, &mut nodes);
        list.push_back(1, &mut nodes);
        list.push_back(2, &mut nodes);

        list.erase(1, &mut nodes);

        let indices: Vec<usize> = list.iter(&nodes).collect();
        assert_eq!(indices, vec![0, 2]);
        assert_eq!(list.len(), 2);
    }

    #[test]
    fn test_pop_front_back() {
        let mut nodes = vec![IntrusiveListNode::new(); 3];
        let mut list = IntrusiveList::new();

        list.push_back(0, &mut nodes);
        list.push_back(1, &mut nodes);
        list.push_back(2, &mut nodes);

        assert_eq!(list.pop_front(&mut nodes), 0);
        assert_eq!(list.pop_back(&mut nodes), 2);
        assert_eq!(list.len(), 1);
        assert_eq!(list.front(), 1);
    }

    #[test]
    fn test_clear() {
        let mut nodes = vec![IntrusiveListNode::new(); 3];
        let mut list = IntrusiveList::new();

        list.push_back(0, &mut nodes);
        list.push_back(1, &mut nodes);
        list.push_back(2, &mut nodes);

        list.clear(&mut nodes);
        assert!(list.is_empty());
        assert_eq!(list.len(), 0);
    }
}
