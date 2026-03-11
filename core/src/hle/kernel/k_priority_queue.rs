//! Port of zuyu/src/core/hle/kernel/k_priority_queue.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! KPriorityQueue — multi-core priority queue for thread scheduling.
//!
//! This is a complex data structure with per-core, per-priority linked lists
//! and bitset tracking of available priorities. In C++ it uses raw pointer-based
//! intrusive linked lists and concept-constrained templates.
//!
//! The full implementation requires KThread (which implements KPriorityQueueMember).
//! This file provides the structural framework; concrete usage will be wired up
//! when KThread is ported.

use crate::hardware_properties::NUM_CPU_CORES;

/// Number of priority levels (0..=63).
pub const NUM_PRIORITY: usize = 64;
/// Number of CPU cores.
pub const NUM_CORES: usize = NUM_CPU_CORES as usize;

/// Lowest thread priority.
pub const LOWEST_PRIORITY: i32 = 63;
/// Highest thread priority.
pub const HIGHEST_PRIORITY: i32 = 0;

/// Check if a core index is valid.
pub const fn is_valid_core(core: i32) -> bool {
    core >= 0 && (core as usize) < NUM_CORES
}

/// Check if a priority value is valid.
pub const fn is_valid_priority(priority: i32) -> bool {
    priority >= HIGHEST_PRIORITY && priority <= LOWEST_PRIORITY + 1
}

/// BitSet64 — a 64-bit bitset for tracking available priorities.
/// Mirrors `Common::BitSet64<NumPriority>` from upstream.
#[derive(Debug, Clone, Copy, Default)]
pub struct BitSet64 {
    bits: u64,
}

impl BitSet64 {
    pub const fn new() -> Self {
        Self { bits: 0 }
    }

    /// Set a bit at the given position.
    pub fn set_bit(&mut self, bit: i32) {
        debug_assert!(bit >= 0 && bit < 64);
        self.bits |= 1u64 << bit;
    }

    /// Clear a bit at the given position.
    pub fn clear_bit(&mut self, bit: i32) {
        debug_assert!(bit >= 0 && bit < 64);
        self.bits &= !(1u64 << bit);
    }

    /// Count leading zeros (returns 64 if all zero).
    pub const fn count_leading_zero(&self) -> u32 {
        if self.bits == 0 {
            64
        } else {
            self.bits.leading_zeros()
        }
    }

    /// Get the next set bit at or after `bit`.
    /// Returns 64 if no bit found.
    pub const fn get_next_set(&self, bit: i32) -> u32 {
        // Mask off bits below `bit + 1` and find first set.
        let masked = if bit + 1 >= 64 {
            0
        } else {
            self.bits & !((1u64 << (bit + 1)) - 1)
        };
        if masked == 0 {
            64
        } else {
            masked.trailing_zeros()
        }
    }
}

/// QueueEntry — intrusive linked list entry for priority queue membership.
/// Each thread has one entry per core.
///
/// Mirrors `Member::QueueEntry` from the C++ KPriorityQueueMember concept.
/// Stored by index (usize) rather than raw pointers.
#[derive(Debug, Clone, Default)]
pub struct QueueEntry {
    prev: Option<usize>, // index of previous member
    next: Option<usize>, // index of next member
}

impl QueueEntry {
    pub fn new() -> Self {
        Self {
            prev: None,
            next: None,
        }
    }

    pub fn initialize(&mut self) {
        self.prev = None;
        self.next = None;
    }

    pub fn get_prev(&self) -> Option<usize> {
        self.prev
    }

    pub fn get_next(&self) -> Option<usize> {
        self.next
    }

    pub fn set_prev(&mut self, prev: Option<usize>) {
        self.prev = prev;
    }

    pub fn set_next(&mut self, next: Option<usize>) {
        self.next = next;
    }
}

/// Trait for priority queue members.
/// Mirrors the C++ `KPriorityQueueMember` concept.
///
/// Types implementing this trait can be placed into a KPriorityQueue.
/// They must provide priority queue entries per-core, an affinity mask,
/// active core, priority, and dummy-thread flag.
pub trait KPriorityQueueMember {
    fn get_priority_queue_entry(&self, core: i32) -> &QueueEntry;
    fn get_priority_queue_entry_mut(&mut self, core: i32) -> &mut QueueEntry;
    fn get_affinity_mask_value(&self) -> u64;
    fn get_active_core(&self) -> i32;
    fn get_priority(&self) -> i32;
    fn is_dummy_thread(&self) -> bool;
}

/// KPerCoreQueue — per-priority, per-core doubly-linked list.
///
/// Mirrors upstream `KPriorityQueue::KPerCoreQueue`.
/// Uses indices into an external member array rather than raw pointers.
#[derive(Debug, Clone)]
pub struct KPerCoreQueue {
    /// Per-core root entries (head.next = first, head.prev = last).
    roots: [QueueEntry; NUM_CORES],
}

impl Default for KPerCoreQueue {
    fn default() -> Self {
        Self {
            roots: std::array::from_fn(|_| QueueEntry::new()),
        }
    }
}

impl KPerCoreQueue {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the front member index for a core.
    pub fn get_front(&self, core: i32) -> Option<usize> {
        self.roots[core as usize].get_next()
    }
}

/// KPriorityQueueImpl — the inner implementation with scheduled/suggested tracking.
///
/// Mirrors upstream `KPriorityQueue::KPriorityQueueImpl`.
#[derive(Debug, Clone)]
pub struct KPriorityQueueImpl {
    queues: Vec<KPerCoreQueue>, // indexed by priority
    available_priorities: [BitSet64; NUM_CORES],
}

impl KPriorityQueueImpl {
    pub fn new() -> Self {
        let mut queues = Vec::with_capacity(NUM_PRIORITY);
        for _ in 0..NUM_PRIORITY {
            queues.push(KPerCoreQueue::new());
        }
        Self {
            queues,
            available_priorities: [BitSet64::new(); NUM_CORES],
        }
    }

    /// Get the front member for a core (highest priority).
    pub fn get_front(&self, core: i32) -> Option<usize> {
        debug_assert!(is_valid_core(core));
        let priority = self.available_priorities[core as usize].count_leading_zero() as i32;
        if priority <= LOWEST_PRIORITY {
            self.queues[priority as usize].get_front(core)
        } else {
            None
        }
    }

    /// Get the front member for a specific priority and core.
    pub fn get_front_at_priority(&self, priority: i32, core: i32) -> Option<usize> {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority <= LOWEST_PRIORITY {
            self.queues[priority as usize].get_front(core)
        } else {
            None
        }
    }
}

/// KPriorityQueue — the full priority queue with scheduled and suggested queues.
///
/// Mirrors upstream `Kernel::KPriorityQueue<Member, NumCores_, LowestPriority, HighestPriority>`.
///
/// TODO: Full push/remove/change operations require concrete KThread integration.
/// The structural framework is in place; operations will be completed when KThread
/// is ported.
#[derive(Debug, Clone)]
pub struct KPriorityQueue {
    scheduled_queue: KPriorityQueueImpl,
    suggested_queue: KPriorityQueueImpl,
}

impl KPriorityQueue {
    pub fn new() -> Self {
        Self {
            scheduled_queue: KPriorityQueueImpl::new(),
            suggested_queue: KPriorityQueueImpl::new(),
        }
    }

    /// Get the front of the scheduled queue for a core.
    pub fn get_scheduled_front(&self, core: i32) -> Option<usize> {
        self.scheduled_queue.get_front(core)
    }

    /// Get the front of the scheduled queue for a specific priority and core.
    pub fn get_scheduled_front_at_priority(&self, core: i32, priority: i32) -> Option<usize> {
        self.scheduled_queue.get_front_at_priority(priority, core)
    }

    /// Get the front of the suggested queue for a core.
    pub fn get_suggested_front(&self, core: i32) -> Option<usize> {
        self.suggested_queue.get_front(core)
    }

    /// Get the front of the suggested queue for a specific priority and core.
    pub fn get_suggested_front_at_priority(&self, core: i32, priority: i32) -> Option<usize> {
        self.suggested_queue.get_front_at_priority(priority, core)
    }

    // TODO: Implement the following when KThread is ported:
    // - push_back(member)
    // - push_front(priority, member)
    // - remove(member)
    // - get_scheduled_next(core, member)
    // - get_suggested_next(core, member)
    // - move_to_scheduled_front(member)
    // - move_to_scheduled_back(member)
    // - change_priority(prev_priority, is_running, member)
    // - change_affinity_mask(prev_core, prev_affinity, member)
    // - change_core(prev_core, member, to_front)
}

impl Default for KPriorityQueue {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper: clear an affinity bit for a given core.
fn clear_affinity_bit(affinity: &mut u64, core: i32) {
    *affinity &= !(1u64 << core);
}

/// Helper: get the next core from an affinity mask, clearing the bit.
fn get_next_core(affinity: &mut u64) -> i32 {
    let core = affinity.trailing_zeros() as i32;
    clear_affinity_bit(affinity, core);
    core
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitset64() {
        let mut bs = BitSet64::new();
        assert_eq!(bs.count_leading_zero(), 64);

        bs.set_bit(5);
        assert_eq!(bs.count_leading_zero(), 58); // 63 - 5 = 58

        bs.set_bit(3);
        assert_eq!(bs.count_leading_zero(), 58); // highest bit is still 5, so 63 - 5 = 58

        bs.clear_bit(3);
        assert_eq!(bs.count_leading_zero(), 58);
    }

    #[test]
    fn test_bitset64_get_next_set() {
        let mut bs = BitSet64::new();
        bs.set_bit(5);
        bs.set_bit(10);
        bs.set_bit(20);

        assert_eq!(bs.get_next_set(3), 5);
        assert_eq!(bs.get_next_set(5), 10);
        assert_eq!(bs.get_next_set(10), 20);
        assert_eq!(bs.get_next_set(20), 64); // no more
    }

    #[test]
    fn test_priority_queue_creation() {
        let pq = KPriorityQueue::new();
        for core in 0..NUM_CORES as i32 {
            assert!(pq.get_scheduled_front(core).is_none());
            assert!(pq.get_suggested_front(core).is_none());
        }
    }

    #[test]
    fn test_valid_core_priority() {
        assert!(is_valid_core(0));
        assert!(is_valid_core(3));
        assert!(!is_valid_core(-1));
        assert!(!is_valid_core(NUM_CORES as i32));

        assert!(is_valid_priority(0));
        assert!(is_valid_priority(63));
        assert!(is_valid_priority(64)); // LowestPriority + 1 is valid
        assert!(!is_valid_priority(-1));
        assert!(!is_valid_priority(65));
    }
}
