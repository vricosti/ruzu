//! Port of zuyu/src/core/hle/kernel/k_priority_queue.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-16
//!
//! KPriorityQueue — multi-core priority queue for thread scheduling.
//!
//! Upstream uses intrusive linked lists with raw Member* pointers in QueueEntry.
//! We use thread_id (u64) as the link value, with external resolution via
//! a thread lookup table. QueueEntry stores Option<u64> for prev/next.

use crate::hardware_properties::NUM_CPU_CORES;

/// Number of priority levels (0..=63).
pub const NUM_PRIORITY: usize = 64;
/// Number of CPU cores.
pub const NUM_CORES: usize = NUM_CPU_CORES as usize;

/// Lowest thread priority.
pub const LOWEST_PRIORITY: i32 = 63;
/// Highest thread priority.
pub const HIGHEST_PRIORITY: i32 = 0;

pub const fn is_valid_core(core: i32) -> bool {
    core >= 0 && (core as usize) < NUM_CORES
}

pub const fn is_valid_priority(priority: i32) -> bool {
    priority >= HIGHEST_PRIORITY && priority <= LOWEST_PRIORITY + 1
}

// ---------------------------------------------------------------------------
// BitSet64
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Default)]
pub struct BitSet64 {
    bits: u64,
}

impl BitSet64 {
    pub const fn new() -> Self {
        Self { bits: 0 }
    }

    pub fn set_bit(&mut self, bit: i32) {
        debug_assert!(bit >= 0 && bit < 64);
        self.bits |= 1u64 << bit;
    }

    pub fn clear_bit(&mut self, bit: i32) {
        debug_assert!(bit >= 0 && bit < 64);
        self.bits &= !(1u64 << bit);
    }

    pub const fn count_leading_zero(&self) -> u32 {
        if self.bits == 0 {
            64
        } else {
            self.bits.leading_zeros()
        }
    }

    pub const fn get_next_set(&self, bit: i32) -> u32 {
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

// ---------------------------------------------------------------------------
// QueueEntry — per-core linked list node using thread_id
// ---------------------------------------------------------------------------

/// Intrusive linked list entry for priority queue membership.
/// Each thread has one entry per core (matching upstream Member::QueueEntry).
///
/// Upstream uses raw `Member*` pointers; we use `Option<u64>` thread_ids.
#[derive(Debug, Clone, Default)]
pub struct QueueEntry {
    prev: Option<u64>,
    next: Option<u64>,
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

    pub fn get_prev(&self) -> Option<u64> {
        self.prev
    }

    pub fn get_next(&self) -> Option<u64> {
        self.next
    }

    pub fn set_prev(&mut self, prev: Option<u64>) {
        self.prev = prev;
    }

    pub fn set_next(&mut self, next: Option<u64>) {
        self.next = next;
    }
}

// ---------------------------------------------------------------------------
// Thread accessor trait — used by the queue to read/write entries on threads
// ---------------------------------------------------------------------------

/// Trait for accessing priority queue data on threads.
/// Matches upstream KPriorityQueueMember concept.
///
/// The priority queue calls these to read/modify the per-core QueueEntry
/// stored inside each thread.
pub trait KPriorityQueueMember {
    fn get_priority_queue_entry(&self, core: i32) -> &QueueEntry;
    fn get_priority_queue_entry_mut(&mut self, core: i32) -> &mut QueueEntry;
    fn get_affinity_mask_value(&self) -> u64;
    fn get_active_core(&self) -> i32;
    fn get_priority(&self) -> i32;
    fn is_dummy_thread(&self) -> bool;
}

// ---------------------------------------------------------------------------
// ThreadAccessor — resolves thread_id to mutable access
// ---------------------------------------------------------------------------

/// Callback-based thread accessor for the priority queue.
/// Since threads are behind Arc<Mutex<>>, the queue can't hold references.
/// Instead, callers provide a ThreadAccessor that resolves thread_ids.
pub trait ThreadAccessor {
    fn with_thread<F, R>(&self, thread_id: u64, f: F) -> Option<R>
    where
        F: FnOnce(&dyn KPriorityQueueMember) -> R;

    fn with_thread_mut<F, R>(&self, thread_id: u64, f: F) -> Option<R>
    where
        F: FnOnce(&mut dyn KPriorityQueueMember) -> R;
}

// ---------------------------------------------------------------------------
// KPerCoreQueue — per-priority doubly-linked list using thread_ids
// ---------------------------------------------------------------------------

/// Per-priority, per-core doubly-linked list.
/// Matches upstream `KPriorityQueue::KPerCoreQueue`.
///
/// The root entries act as sentinel nodes:
/// root.next = head of list, root.prev = tail of list.
#[derive(Debug, Clone)]
pub struct KPerCoreQueue {
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

    /// Push a thread to the back of the queue for a core.
    /// Returns true if the queue was previously empty (first element).
    pub fn push_back(&mut self, core: i32, member_id: u64, accessor: &impl ThreadAccessor) -> bool {
        let tail_id = self.roots[core as usize].get_prev();

        // Link: member.prev = tail, member.next = None
        accessor.with_thread_mut(member_id, |member| {
            let entry = member.get_priority_queue_entry_mut(core);
            entry.set_prev(tail_id);
            entry.set_next(None);
        });

        // Link: tail.next = member (or root.next if empty)
        if let Some(tail_id) = tail_id {
            accessor.with_thread_mut(tail_id, |tail| {
                tail.get_priority_queue_entry_mut(core).set_next(Some(member_id));
            });
        } else {
            self.roots[core as usize].set_next(Some(member_id));
        }
        self.roots[core as usize].set_prev(Some(member_id));

        tail_id.is_none()
    }

    /// Push a thread to the front of the queue for a core.
    /// Returns true if the queue was previously empty.
    pub fn push_front(&mut self, core: i32, member_id: u64, accessor: &impl ThreadAccessor) -> bool {
        let head_id = self.roots[core as usize].get_next();

        // Link: member.prev = None, member.next = head
        accessor.with_thread_mut(member_id, |member| {
            let entry = member.get_priority_queue_entry_mut(core);
            entry.set_prev(None);
            entry.set_next(head_id);
        });

        // Link: head.prev = member (or root.prev if empty)
        if let Some(head_id) = head_id {
            accessor.with_thread_mut(head_id, |head| {
                head.get_priority_queue_entry_mut(core).set_prev(Some(member_id));
            });
        } else {
            self.roots[core as usize].set_prev(Some(member_id));
        }
        self.roots[core as usize].set_next(Some(member_id));

        head_id.is_none()
    }

    /// Remove a thread from the queue for a core.
    /// Returns true if the queue is now empty.
    pub fn remove(&mut self, core: i32, member_id: u64, accessor: &impl ThreadAccessor) -> bool {
        let (prev_id, next_id) = accessor
            .with_thread(member_id, |member| {
                let entry = member.get_priority_queue_entry(core);
                (entry.get_prev(), entry.get_next())
            })
            .unwrap_or((None, None));

        // Unlink prev -> next
        if let Some(prev_id) = prev_id {
            accessor.with_thread_mut(prev_id, |prev| {
                prev.get_priority_queue_entry_mut(core).set_next(next_id);
            });
        } else {
            self.roots[core as usize].set_next(next_id);
        }

        // Unlink next -> prev
        if let Some(next_id) = next_id {
            accessor.with_thread_mut(next_id, |next| {
                next.get_priority_queue_entry_mut(core).set_prev(prev_id);
            });
        } else {
            self.roots[core as usize].set_prev(prev_id);
        }

        self.get_front(core).is_none()
    }

    pub fn get_front(&self, core: i32) -> Option<u64> {
        self.roots[core as usize].get_next()
    }
}

// ---------------------------------------------------------------------------
// KPriorityQueueImpl
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct KPriorityQueueImpl {
    queues: Vec<KPerCoreQueue>,
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

    pub fn push_back(&mut self, priority: i32, core: i32, member_id: u64, accessor: &impl ThreadAccessor) {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority > LOWEST_PRIORITY { return; }

        let was_empty = self.queues[priority as usize].push_back(core, member_id, accessor);
        if was_empty {
            self.available_priorities[core as usize].set_bit(priority);
        }
        log::debug!(
            "KPriorityQueueImpl::push_back: p={} core={} thread={} was_empty={}",
            priority, core, member_id, was_empty,
        );
    }

    pub fn push_front(&mut self, priority: i32, core: i32, member_id: u64, accessor: &impl ThreadAccessor) {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority > LOWEST_PRIORITY { return; }

        if self.queues[priority as usize].push_front(core, member_id, accessor) {
            self.available_priorities[core as usize].set_bit(priority);
        }
    }

    pub fn remove(&mut self, priority: i32, core: i32, member_id: u64, accessor: &impl ThreadAccessor) {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority > LOWEST_PRIORITY { return; }

        if self.queues[priority as usize].remove(core, member_id, accessor) {
            self.available_priorities[core as usize].clear_bit(priority);
        }
    }

    pub fn get_front(&self, core: i32) -> Option<u64> {
        debug_assert!(is_valid_core(core));
        let priority = self.available_priorities[core as usize].count_leading_zero() as i32;
        if priority <= LOWEST_PRIORITY {
            self.queues[priority as usize].get_front(core)
        } else {
            None
        }
    }

    pub fn get_front_at_priority(&self, priority: i32, core: i32) -> Option<u64> {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority <= LOWEST_PRIORITY {
            self.queues[priority as usize].get_front(core)
        } else {
            None
        }
    }

    /// Get the next thread after `member_id` in the queue for `core`.
    /// If no next in current priority, jump to the front of the next priority.
    pub fn get_next(&self, core: i32, member_id: u64, member_priority: i32, accessor: &impl ThreadAccessor) -> Option<u64> {
        debug_assert!(is_valid_core(core));

        let next = accessor.with_thread(member_id, |member| {
            member.get_priority_queue_entry(core).get_next()
        })?;

        if next.is_some() {
            return next;
        }

        // Jump to the next priority level
        let next_priority = self.available_priorities[core as usize].get_next_set(member_priority) as i32;
        if next_priority <= LOWEST_PRIORITY {
            self.queues[next_priority as usize].get_front(core)
        } else {
            None
        }
    }

    pub fn move_to_front(&mut self, priority: i32, core: i32, member_id: u64, accessor: &impl ThreadAccessor) {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority <= LOWEST_PRIORITY {
            self.queues[priority as usize].remove(core, member_id, accessor);
            self.queues[priority as usize].push_front(core, member_id, accessor);
        }
    }

    pub fn move_to_back(&mut self, priority: i32, core: i32, member_id: u64, accessor: &impl ThreadAccessor) -> Option<u64> {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority <= LOWEST_PRIORITY {
            self.queues[priority as usize].remove(core, member_id, accessor);
            self.queues[priority as usize].push_back(core, member_id, accessor);
            self.queues[priority as usize].get_front(core)
        } else {
            None
        }
    }
}

// ---------------------------------------------------------------------------
// KPriorityQueue — the full priority queue
// ---------------------------------------------------------------------------

/// The full priority queue with scheduled and suggested queues.
/// Matches upstream `Kernel::KPriorityQueue<Member, NumCores_, LowestPriority, HighestPriority>`.
#[derive(Debug, Clone)]
pub struct KPriorityQueue {
    scheduled_queue: KPriorityQueueImpl,
    suggested_queue: KPriorityQueueImpl,
}

fn clear_affinity_bit(affinity: &mut u64, core: i32) {
    *affinity &= !(1u64 << core);
}

fn get_next_core(affinity: &mut u64) -> i32 {
    let core = affinity.trailing_zeros() as i32;
    clear_affinity_bit(affinity, core);
    core
}

impl KPriorityQueue {
    pub fn new() -> Self {
        Self {
            scheduled_queue: KPriorityQueueImpl::new(),
            suggested_queue: KPriorityQueueImpl::new(),
        }
    }

    // -- Getters --

    pub fn get_scheduled_front(&self, core: i32) -> Option<u64> {
        self.scheduled_queue.get_front(core)
    }

    pub fn get_scheduled_front_at_priority(&self, core: i32, priority: i32) -> Option<u64> {
        self.scheduled_queue.get_front_at_priority(priority, core)
    }

    pub fn get_suggested_front(&self, core: i32) -> Option<u64> {
        self.suggested_queue.get_front(core)
    }

    pub fn get_suggested_front_at_priority(&self, core: i32, priority: i32) -> Option<u64> {
        self.suggested_queue.get_front_at_priority(priority, core)
    }

    pub fn get_scheduled_next(&self, core: i32, member_id: u64, member_priority: i32, accessor: &impl ThreadAccessor) -> Option<u64> {
        self.scheduled_queue.get_next(core, member_id, member_priority, accessor)
    }

    pub fn get_suggested_next(&self, core: i32, member_id: u64, member_priority: i32, accessor: &impl ThreadAccessor) -> Option<u64> {
        self.suggested_queue.get_next(core, member_id, member_priority, accessor)
    }

    pub fn get_same_priority_next(&self, core: i32, member_id: u64, accessor: &impl ThreadAccessor) -> Option<u64> {
        accessor.with_thread(member_id, |member| {
            member.get_priority_queue_entry(core).get_next()
        })?
    }

    // -- Private push/remove with priority --

    fn push_back_impl(&mut self, priority: i32, member_id: u64, active_core: i32, affinity_mask: u64, accessor: &impl ThreadAccessor) {
        debug_assert!(is_valid_priority(priority));

        let mut affinity = affinity_mask;
        if active_core >= 0 {
            self.scheduled_queue.push_back(priority, active_core, member_id, accessor);
            clear_affinity_bit(&mut affinity, active_core);
        }

        while affinity != 0 {
            self.suggested_queue.push_back(priority, get_next_core(&mut affinity), member_id, accessor);
        }
    }

    fn push_front_impl(&mut self, priority: i32, member_id: u64, active_core: i32, affinity_mask: u64, accessor: &impl ThreadAccessor) {
        debug_assert!(is_valid_priority(priority));

        let mut affinity = affinity_mask;
        if active_core >= 0 {
            self.scheduled_queue.push_front(priority, active_core, member_id, accessor);
            clear_affinity_bit(&mut affinity, active_core);
        }

        // Note: Nintendo pushes onto the back of the suggested queue, not the front.
        while affinity != 0 {
            self.suggested_queue.push_back(priority, get_next_core(&mut affinity), member_id, accessor);
        }
    }

    fn remove_impl(&mut self, priority: i32, member_id: u64, active_core: i32, affinity_mask: u64, accessor: &impl ThreadAccessor) {
        debug_assert!(is_valid_priority(priority));

        let mut affinity = affinity_mask;
        if active_core >= 0 {
            self.scheduled_queue.remove(priority, active_core, member_id, accessor);
            clear_affinity_bit(&mut affinity, active_core);
        }

        while affinity != 0 {
            self.suggested_queue.remove(priority, get_next_core(&mut affinity), member_id, accessor);
        }
    }

    // -- Public mutators --

    /// Push a thread to the back of its queues (scheduled for active core, suggested for others).
    pub fn push_back(&mut self, member_id: u64, accessor: &impl ThreadAccessor) {
        let Some((priority, active_core, affinity, is_dummy)) = accessor.with_thread(member_id, |m| {
            (m.get_priority(), m.get_active_core(), m.get_affinity_mask_value(), m.is_dummy_thread())
        }) else {
            log::warn!("KPriorityQueue::push_back: thread {} not found in accessor", member_id);
            return;
        };

        if is_dummy {
            log::debug!("KPriorityQueue::push_back: thread {} is dummy, skipping", member_id);
            return;
        }
        log::debug!(
            "KPriorityQueue::push_back: thread {} priority={} active_core={} affinity={:#X}",
            member_id, priority, active_core, affinity
        );
        self.push_back_impl(priority, member_id, active_core, affinity, accessor);
    }

    /// Remove a thread from all its queues.
    pub fn remove(&mut self, member_id: u64, accessor: &impl ThreadAccessor) {
        let Some((priority, active_core, affinity, is_dummy)) = accessor.with_thread(member_id, |m| {
            (m.get_priority(), m.get_active_core(), m.get_affinity_mask_value(), m.is_dummy_thread())
        }) else { return; };

        if is_dummy { return; }
        self.remove_impl(priority, member_id, active_core, affinity, accessor);
    }

    pub fn move_to_scheduled_front(&mut self, member_id: u64, accessor: &impl ThreadAccessor) {
        let Some((priority, active_core, is_dummy)) = accessor.with_thread(member_id, |m| {
            (m.get_priority(), m.get_active_core(), m.is_dummy_thread())
        }) else { return; };

        if is_dummy { return; }
        self.scheduled_queue.move_to_front(priority, active_core, member_id, accessor);
    }

    pub fn move_to_scheduled_back(&mut self, member_id: u64, accessor: &impl ThreadAccessor) -> Option<u64> {
        let (priority, active_core, is_dummy) = accessor.with_thread(member_id, |m| {
            (m.get_priority(), m.get_active_core(), m.is_dummy_thread())
        })?;

        if is_dummy { return None; }
        self.scheduled_queue.move_to_back(priority, active_core, member_id, accessor)
    }

    /// Change a thread's priority in the queue.
    pub fn change_priority(&mut self, prev_priority: i32, is_running: bool, member_id: u64, accessor: &impl ThreadAccessor) {
        let Some((new_priority, active_core, affinity, is_dummy)) = accessor.with_thread(member_id, |m| {
            (m.get_priority(), m.get_active_core(), m.get_affinity_mask_value(), m.is_dummy_thread())
        }) else { return; };

        if is_dummy { return; }
        debug_assert!(is_valid_priority(prev_priority));

        self.remove_impl(prev_priority, member_id, active_core, affinity, accessor);

        if is_running {
            self.push_front_impl(new_priority, member_id, active_core, affinity, accessor);
        } else {
            self.push_back_impl(new_priority, member_id, active_core, affinity, accessor);
        }
    }

    /// Change a thread's affinity mask.
    pub fn change_affinity_mask(&mut self, prev_core: i32, prev_affinity: u64, member_id: u64, accessor: &impl ThreadAccessor) {
        let Some((priority, new_core, new_affinity, is_dummy)) = accessor.with_thread(member_id, |m| {
            (m.get_priority(), m.get_active_core(), m.get_affinity_mask_value(), m.is_dummy_thread())
        }) else { return; };

        if is_dummy { return; }

        // Remove from all old queues
        for core in 0..NUM_CORES as i32 {
            if prev_affinity & (1u64 << core) != 0 {
                if core == prev_core {
                    self.scheduled_queue.remove(priority, core, member_id, accessor);
                } else {
                    self.suggested_queue.remove(priority, core, member_id, accessor);
                }
            }
        }

        // Add to all new queues
        for core in 0..NUM_CORES as i32 {
            if new_affinity & (1u64 << core) != 0 {
                if core == new_core {
                    self.scheduled_queue.push_back(priority, core, member_id, accessor);
                } else {
                    self.suggested_queue.push_back(priority, core, member_id, accessor);
                }
            }
        }
    }

    /// Change a thread's active core.
    pub fn change_core(&mut self, prev_core: i32, member_id: u64, to_front: bool, accessor: &impl ThreadAccessor) {
        let Some((new_core, priority, is_dummy)) = accessor.with_thread(member_id, |m| {
            (m.get_active_core(), m.get_priority(), m.is_dummy_thread())
        }) else { return; };

        if is_dummy { return; }

        if prev_core != new_core {
            if prev_core >= 0 {
                self.scheduled_queue.remove(priority, prev_core, member_id, accessor);
            }
            if new_core >= 0 {
                self.suggested_queue.remove(priority, new_core, member_id, accessor);
                if to_front {
                    self.scheduled_queue.push_front(priority, new_core, member_id, accessor);
                } else {
                    self.scheduled_queue.push_back(priority, new_core, member_id, accessor);
                }
            }
            if prev_core >= 0 {
                self.suggested_queue.push_back(priority, prev_core, member_id, accessor);
            }
        }
    }
}

impl Default for KPriorityQueue {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitset64() {
        let mut bs = BitSet64::new();
        assert_eq!(bs.count_leading_zero(), 64);

        bs.set_bit(5);
        assert_eq!(bs.count_leading_zero(), 58);

        bs.set_bit(3);
        assert_eq!(bs.count_leading_zero(), 58);

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
        assert_eq!(bs.get_next_set(20), 64);
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
        assert!(is_valid_priority(64));
        assert!(!is_valid_priority(-1));
        assert!(!is_valid_priority(65));
    }
}
