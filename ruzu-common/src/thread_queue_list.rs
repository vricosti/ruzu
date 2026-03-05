//! Port of zuyu/src/common/thread_queue_list.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::collections::VecDeque;

/// Priority-based thread queue list used by the kernel scheduler.
///
/// Mirrors the C++ `Common::ThreadQueueList<T, N>`. Contains `N` priority
/// levels, each with a double-ended queue of thread ids. Priority levels that
/// have been "prepared" (used at least once) are chained together via a linked
/// list so that `get_first` / `pop_first` can skip unused priorities.
///
/// `T` is the thread id type (typically a handle or index).
/// `N` is the number of priority levels.
pub struct ThreadQueueList<T: Clone + Default + PartialEq, const N: usize> {
    queues: [Queue<T>; N],
    /// Index of the first non-empty priority queue, or `None`.
    first: Option<usize>,
}

#[derive(Clone)]
struct Queue<T> {
    /// Index of the next non-empty priority queue, or `None` if unlinked.
    /// We use `Some(usize::MAX)` as the "unlinked" tag (matching C++ `UnlinkedTag()`).
    next_nonempty: Option<usize>,
    data: VecDeque<T>,
}

impl<T> Default for Queue<T> {
    fn default() -> Self {
        Self {
            next_nonempty: Some(usize::MAX), // unlinked tag
            data: VecDeque::new(),
        }
    }
}

impl<T> Queue<T> {
    fn is_unlinked(&self) -> bool {
        self.next_nonempty == Some(usize::MAX)
    }
}

impl<T: Clone + Default + PartialEq, const N: usize> Default for ThreadQueueList<T, N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone + Default + PartialEq, const N: usize> ThreadQueueList<T, N> {
    pub fn new() -> Self {
        Self {
            queues: std::array::from_fn(|_| Queue::default()),
            first: None,
        }
    }

    /// Check which priority level contains `uid`. Returns the priority, or
    /// `None` if not found. (Debug helper matching C++ `contains`.)
    pub fn contains(&self, uid: &T) -> Option<usize> {
        for i in 0..N {
            if self.queues[i].data.contains(uid) {
                return Some(i);
            }
        }
        None
    }

    /// Get the first element across all priority levels without removing it.
    /// Returns `T::default()` if all queues are empty.
    pub fn get_first(&self) -> T {
        let mut cur = self.first;
        while let Some(idx) = cur {
            if !self.queues[idx].data.is_empty() {
                return self.queues[idx].data.front().unwrap().clone();
            }
            cur = self.queues[idx].next_nonempty;
        }
        T::default()
    }

    /// Get the first element that passes `filter`. Returns `T::default()` if none found.
    pub fn get_first_filter<F>(&self, filter: F) -> T
    where
        F: Fn(&T) -> bool,
    {
        let mut cur = self.first;
        while let Some(idx) = cur {
            for item in &self.queues[idx].data {
                if filter(item) {
                    return item.clone();
                }
            }
            cur = self.queues[idx].next_nonempty;
        }
        T::default()
    }

    /// Pop the first element across all priority levels.
    /// Returns `T::default()` if empty.
    pub fn pop_first(&mut self) -> T {
        let mut cur = self.first;
        while let Some(idx) = cur {
            if !self.queues[idx].data.is_empty() {
                return self.queues[idx].data.pop_front().unwrap();
            }
            cur = self.queues[idx].next_nonempty;
        }
        T::default()
    }

    /// Pop the first element from a priority level better (lower number) than `priority`.
    /// Returns `T::default()` if none found.
    pub fn pop_first_better(&mut self, priority: usize) -> T {
        let mut cur = self.first;
        while let Some(idx) = cur {
            if idx >= priority {
                break;
            }
            if !self.queues[idx].data.is_empty() {
                return self.queues[idx].data.pop_front().unwrap();
            }
            cur = self.queues[idx].next_nonempty;
        }
        T::default()
    }

    /// Push `thread_id` to the front of the queue at `priority`.
    pub fn push_front(&mut self, priority: usize, thread_id: T) {
        self.queues[priority].data.push_front(thread_id);
    }

    /// Push `thread_id` to the back of the queue at `priority`.
    pub fn push_back(&mut self, priority: usize, thread_id: T) {
        self.queues[priority].data.push_back(thread_id);
    }

    /// Move a thread from `old_priority` to `new_priority`.
    pub fn move_thread(&mut self, thread_id: &T, old_priority: usize, new_priority: usize) {
        self.remove(old_priority, thread_id);
        self.prepare(new_priority);
        self.push_back(new_priority, thread_id.clone());
    }

    /// Remove `thread_id` from the queue at `priority`.
    pub fn remove(&mut self, priority: usize, thread_id: &T) {
        self.queues[priority].data.retain(|x| x != thread_id);
    }

    /// Rotate the queue at `priority` (move front to back).
    pub fn rotate(&mut self, priority: usize) {
        if self.queues[priority].data.len() > 1 {
            let front = self.queues[priority].data.pop_front().unwrap();
            self.queues[priority].data.push_back(front);
        }
    }

    /// Clear all queues.
    pub fn clear(&mut self) {
        for q in &mut self.queues {
            *q = Queue::default();
        }
        self.first = None;
    }

    /// Check if the queue at `priority` is empty.
    pub fn is_empty(&self, priority: usize) -> bool {
        self.queues[priority].data.is_empty()
    }

    /// Prepare a priority level for use (link it into the chain if not already linked).
    pub fn prepare(&mut self, priority: usize) {
        if self.queues[priority].is_unlinked() {
            self.link(priority);
        }
    }

    // -- private helpers --

    fn link(&mut self, priority: usize) {
        // Find the previous linked priority and insert after it
        for i in (0..priority).rev() {
            if !self.queues[i].is_unlinked() {
                let old_next = self.queues[i].next_nonempty;
                self.queues[i].next_nonempty = Some(priority);
                self.queues[priority].next_nonempty = old_next;
                return;
            }
        }
        // No previous linked priority found, insert at the beginning
        self.queues[priority].next_nonempty = self.first;
        self.first = Some(priority);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_operations() {
        let mut tql = ThreadQueueList::<u32, 4>::new();
        tql.prepare(1);
        tql.push_back(1, 100);
        tql.push_back(1, 200);

        assert_eq!(tql.get_first(), 100);
        assert_eq!(tql.pop_first(), 100);
        assert_eq!(tql.pop_first(), 200);
        assert_eq!(tql.pop_first(), 0); // default
    }

    #[test]
    fn test_priority_ordering() {
        let mut tql = ThreadQueueList::<u32, 4>::new();
        tql.prepare(2);
        tql.push_back(2, 200);
        tql.prepare(0);
        tql.push_back(0, 100);
        tql.prepare(3);
        tql.push_back(3, 300);

        assert_eq!(tql.get_first(), 100); // priority 0 comes first
        assert_eq!(tql.pop_first(), 100);
        assert_eq!(tql.pop_first(), 200);
        assert_eq!(tql.pop_first(), 300);
    }

    #[test]
    fn test_pop_first_better() {
        let mut tql = ThreadQueueList::<u32, 4>::new();
        tql.prepare(1);
        tql.push_back(1, 100);
        tql.prepare(2);
        tql.push_back(2, 200);

        // Only pop from priorities < 2
        assert_eq!(tql.pop_first_better(2), 100);
        assert_eq!(tql.pop_first_better(2), 0); // nothing better
    }

    #[test]
    fn test_remove_and_move() {
        let mut tql = ThreadQueueList::<u32, 4>::new();
        tql.prepare(1);
        tql.push_back(1, 100);
        tql.push_back(1, 200);

        tql.remove(1, &100);
        assert_eq!(tql.get_first(), 200);

        tql.prepare(0);
        tql.move_thread(&200, 1, 0);
        assert_eq!(tql.get_first(), 200);
        assert!(tql.is_empty(1));
    }

    #[test]
    fn test_rotate() {
        let mut tql = ThreadQueueList::<u32, 4>::new();
        tql.prepare(0);
        tql.push_back(0, 1);
        tql.push_back(0, 2);
        tql.push_back(0, 3);

        tql.rotate(0);
        assert_eq!(tql.get_first(), 2);
    }

    #[test]
    fn test_contains() {
        let mut tql = ThreadQueueList::<u32, 4>::new();
        tql.prepare(2);
        tql.push_back(2, 42);

        assert_eq!(tql.contains(&42), Some(2));
        assert_eq!(tql.contains(&99), None);
    }
}
