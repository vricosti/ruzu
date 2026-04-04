//! Port of zuyu/src/core/hle/kernel/k_priority_queue.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-27
//!
//! KPriorityQueue — multi-core priority queue for thread scheduling.
//!
//! Upstream uses intrusive linked lists with raw Member* pointers in QueueEntry.
//! We store QueueEntry nodes internally (in a HashMap keyed by thread_id),
//! and cache thread properties so the PQ is self-contained — no external
//! thread locking is needed for PQ operations.

use std::collections::HashMap;

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

    /// Set bit at position `bit`. Upstream stores in reversed order:
    /// bit 0 = MSB (bit 63 of u64), bit 63 = LSB (bit 0 of u64).
    /// This way, `leading_zeros()` directly returns the priority number.
    pub fn set_bit(&mut self, bit: i32) {
        debug_assert!(bit >= 0 && bit < 64);
        self.bits |= 1u64 << (63 - bit);
    }

    pub fn clear_bit(&mut self, bit: i32) {
        debug_assert!(bit >= 0 && bit < 64);
        self.bits &= !(1u64 << (63 - bit));
    }

    /// Returns the index of the highest-priority (numerically lowest) set bit.
    /// Returns 64 if no bits are set.
    pub const fn count_leading_zero(&self) -> u32 {
        if self.bits == 0 {
            64
        } else {
            self.bits.leading_zeros()
        }
    }

    pub const fn raw(&self) -> u64 {
        self.bits
    }

    /// Returns the next set bit after `bit`, or 64 if none.
    pub const fn get_next_set(&self, bit: i32) -> u32 {
        // Mask out bit `bit` and all higher-priority (lower-numbered) bits.
        // In reversed storage, bit N is at position (63 - N).
        // We want to clear all bits at positions >= (63 - bit), i.e., keep bits < (63 - bit).
        let shift = 63 - bit;
        let masked = if shift <= 0 {
            0
        } else {
            self.bits & ((1u64 << shift) - 1)
        };
        if masked == 0 {
            64
        } else {
            masked.leading_zeros()
        }
    }
}

// ---------------------------------------------------------------------------
// QueueEntry — per-core linked list node using thread_id
// ---------------------------------------------------------------------------

/// Intrusive linked list entry for priority queue membership.
/// Stored internally in the PQ, keyed by (thread_id, core).
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
// EntryMap — internal storage for per-thread, per-core queue entries
// ---------------------------------------------------------------------------

/// Internal storage: thread_id → [QueueEntry; NUM_CORES].
/// Shared between scheduled and suggested queues (a thread is in at most
/// one list per core, so the entries don't conflict).
type EntryMap = HashMap<u64, [QueueEntry; NUM_CORES]>;

fn ensure_entry(entries: &mut EntryMap, id: u64) {
    entries
        .entry(id)
        .or_insert_with(|| std::array::from_fn(|_| QueueEntry::new()));
}

// ---------------------------------------------------------------------------
// ThreadProps — cached thread properties for lock-free scheduler access
// ---------------------------------------------------------------------------

/// Cached thread properties stored in the PQ.
/// Updated on push/remove/change operations.
/// Allows the scheduler migration loop to read thread properties
/// without locking individual KThread mutexes.
#[derive(Debug, Clone)]
pub struct ThreadProps {
    pub priority: i32,
    pub active_core: i32,
    pub affinity: u64,
    pub is_dummy: bool,
    /// Shared reference to the owning process's schedule_count.
    /// Allows IncrementScheduledCount without any lock acquisition.
    pub process_schedule_count: Option<std::sync::Arc<std::sync::atomic::AtomicI64>>,
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
    pub fn push_back(&mut self, core: i32, member_id: u64, entries: &mut EntryMap) -> bool {
        let c = core as usize;
        let tail_id = self.roots[c].get_prev();

        // Link: member.prev = tail, member.next = None
        {
            let e = entries
                .entry(member_id)
                .or_insert_with(|| std::array::from_fn(|_| QueueEntry::new()));
            e[c].set_prev(tail_id);
            e[c].set_next(None);
        }

        // Link: tail.next = member (or root.next if empty)
        if let Some(tid) = tail_id {
            if let Some(te) = entries.get_mut(&tid) {
                te[c].set_next(Some(member_id));
            }
        } else {
            self.roots[c].set_next(Some(member_id));
        }
        self.roots[c].set_prev(Some(member_id));

        tail_id.is_none()
    }

    /// Push a thread to the front of the queue for a core.
    /// Returns true if the queue was previously empty.
    pub fn push_front(&mut self, core: i32, member_id: u64, entries: &mut EntryMap) -> bool {
        let c = core as usize;
        let head_id = self.roots[c].get_next();

        // Link: member.prev = None, member.next = head
        {
            let e = entries
                .entry(member_id)
                .or_insert_with(|| std::array::from_fn(|_| QueueEntry::new()));
            e[c].set_prev(None);
            e[c].set_next(head_id);
        }

        // Link: head.prev = member (or root.prev if empty)
        if let Some(hid) = head_id {
            if let Some(he) = entries.get_mut(&hid) {
                he[c].set_prev(Some(member_id));
            }
        } else {
            self.roots[c].set_prev(Some(member_id));
        }
        self.roots[c].set_next(Some(member_id));

        head_id.is_none()
    }

    /// Remove a thread from the queue for a core.
    /// Returns true if the queue is now empty.
    pub fn remove(&mut self, core: i32, member_id: u64, entries: &mut EntryMap) -> bool {
        let c = core as usize;
        let (prev_id, next_id) = entries
            .get(&member_id)
            .map(|e| (e[c].get_prev(), e[c].get_next()))
            .unwrap_or((None, None));

        // Unlink prev -> next
        if let Some(pid) = prev_id {
            if let Some(pe) = entries.get_mut(&pid) {
                pe[c].set_next(next_id);
            }
        } else {
            self.roots[c].set_next(next_id);
        }

        // Unlink next -> prev
        if let Some(nid) = next_id {
            if let Some(ne) = entries.get_mut(&nid) {
                ne[c].set_prev(prev_id);
            }
        } else {
            self.roots[c].set_prev(prev_id);
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

    pub fn push_back(&mut self, priority: i32, core: i32, member_id: u64, entries: &mut EntryMap) {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority > LOWEST_PRIORITY {
            return;
        }

        let was_empty = self.queues[priority as usize].push_back(core, member_id, entries);
        if was_empty {
            self.available_priorities[core as usize].set_bit(priority);
        }
    }

    pub fn push_front(&mut self, priority: i32, core: i32, member_id: u64, entries: &mut EntryMap) {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority > LOWEST_PRIORITY {
            return;
        }

        if self.queues[priority as usize].push_front(core, member_id, entries) {
            self.available_priorities[core as usize].set_bit(priority);
        }
    }

    pub fn remove(&mut self, priority: i32, core: i32, member_id: u64, entries: &mut EntryMap) {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority > LOWEST_PRIORITY {
            return;
        }

        if self.queues[priority as usize].remove(core, member_id, entries) {
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
    pub fn get_next(
        &self,
        core: i32,
        member_id: u64,
        member_priority: i32,
        entries: &EntryMap,
    ) -> Option<u64> {
        debug_assert!(is_valid_core(core));

        let next = entries
            .get(&member_id)
            .and_then(|e| e[core as usize].get_next());

        if next.is_some() {
            return next;
        }

        // Jump to the next priority level
        let next_priority =
            self.available_priorities[core as usize].get_next_set(member_priority) as i32;
        if next_priority <= LOWEST_PRIORITY {
            self.queues[next_priority as usize].get_front(core)
        } else {
            None
        }
    }

    pub fn move_to_front(
        &mut self,
        priority: i32,
        core: i32,
        member_id: u64,
        entries: &mut EntryMap,
    ) {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority <= LOWEST_PRIORITY {
            self.queues[priority as usize].remove(core, member_id, entries);
            self.queues[priority as usize].push_front(core, member_id, entries);
        }
    }

    pub fn move_to_back(
        &mut self,
        priority: i32,
        core: i32,
        member_id: u64,
        entries: &mut EntryMap,
    ) -> Option<u64> {
        debug_assert!(is_valid_core(core));
        debug_assert!(is_valid_priority(priority));
        if priority <= LOWEST_PRIORITY {
            self.queues[priority as usize].remove(core, member_id, entries);
            self.queues[priority as usize].push_back(core, member_id, entries);
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
///
/// Entries and thread properties are stored internally, so PQ operations
/// do not require locking any KThread mutex.
#[derive(Debug, Clone)]
pub struct KPriorityQueue {
    scheduled_queue: KPriorityQueueImpl,
    suggested_queue: KPriorityQueueImpl,
    /// Linked list entries, shared between scheduled and suggested.
    /// A thread is in at most one list per core (either scheduled or suggested),
    /// so the per-core entries don't conflict.
    entries: EntryMap,
    /// Cached thread properties for lock-free scheduler access.
    thread_props: HashMap<u64, ThreadProps>,
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
            entries: HashMap::new(),
            thread_props: HashMap::new(),
        }
    }

    // -- Property cache access (for migration loop) --

    /// Get cached thread properties. Returns None if thread not in PQ.
    pub fn get_thread_props(&self, thread_id: u64) -> Option<&ThreadProps> {
        self.thread_props.get(&thread_id)
    }

    /// Upstream: IncrementScheduledCount(thread) — increments the owning process's
    /// schedule_count via the cached Arc<AtomicI64>. No locks needed.
    pub fn increment_scheduled_count(&self, thread_id: u64) {
        if let Some(props) = self.thread_props.get(&thread_id) {
            if let Some(ref counter) = props.process_schedule_count {
                counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }
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

    pub fn get_scheduled_next(
        &self,
        core: i32,
        member_id: u64,
        member_priority: i32,
    ) -> Option<u64> {
        self.scheduled_queue
            .get_next(core, member_id, member_priority, &self.entries)
    }

    pub fn get_suggested_next(
        &self,
        core: i32,
        member_id: u64,
        member_priority: i32,
    ) -> Option<u64> {
        self.suggested_queue
            .get_next(core, member_id, member_priority, &self.entries)
    }

    pub fn get_same_priority_next(&self, core: i32, member_id: u64) -> Option<u64> {
        self.entries
            .get(&member_id)
            .and_then(|e| e[core as usize].get_next())
    }

    // -- Private push/remove with priority --

    fn push_back_impl(
        &mut self,
        priority: i32,
        member_id: u64,
        active_core: i32,
        affinity_mask: u64,
    ) {
        debug_assert!(is_valid_priority(priority));

        let Self {
            scheduled_queue,
            suggested_queue,
            entries,
            ..
        } = self;
        let mut affinity = affinity_mask;
        if active_core >= 0 {
            scheduled_queue.push_back(priority, active_core, member_id, entries);
            clear_affinity_bit(&mut affinity, active_core);

            // Sanity check: the thread must be retrievable after push.
            let front = scheduled_queue.get_front_at_priority(priority, active_core);
            if front.is_none() {
                log::error!(
                    "PQ BUG: push_back_impl tid={} prio={} core={} — get_front returns None after push!",
                    member_id, priority, active_core
                );
            }
        }

        while affinity != 0 {
            suggested_queue.push_back(priority, get_next_core(&mut affinity), member_id, entries);
        }
    }

    fn push_front_impl(
        &mut self,
        priority: i32,
        member_id: u64,
        active_core: i32,
        affinity_mask: u64,
    ) {
        debug_assert!(is_valid_priority(priority));

        let Self {
            scheduled_queue,
            suggested_queue,
            entries,
            ..
        } = self;
        let mut affinity = affinity_mask;
        if active_core >= 0 {
            scheduled_queue.push_front(priority, active_core, member_id, entries);
            clear_affinity_bit(&mut affinity, active_core);
        }

        // Note: Nintendo pushes onto the back of the suggested queue, not the front.
        while affinity != 0 {
            suggested_queue.push_back(priority, get_next_core(&mut affinity), member_id, entries);
        }
    }

    fn remove_impl(&mut self, priority: i32, member_id: u64, active_core: i32, affinity_mask: u64) {
        debug_assert!(is_valid_priority(priority));

        let Self {
            scheduled_queue,
            suggested_queue,
            entries,
            ..
        } = self;
        let mut affinity = affinity_mask;
        if active_core >= 0 {
            scheduled_queue.remove(priority, active_core, member_id, entries);
            clear_affinity_bit(&mut affinity, active_core);
        }

        while affinity != 0 {
            suggested_queue.remove(priority, get_next_core(&mut affinity), member_id, entries);
        }
    }

    // -- Public mutators (properties passed directly) --

    /// Push a thread to the back of its queues (scheduled for active core, suggested for others).
    /// Matches upstream `KPriorityQueue::PushBack(member)`.
    pub fn push_back(
        &mut self,
        member_id: u64,
        priority: i32,
        active_core: i32,
        affinity: u64,
        is_dummy: bool,
        process_schedule_count: Option<std::sync::Arc<std::sync::atomic::AtomicI64>>,
    ) {
        if is_dummy {
            return;
        }
        if let Some(existing) = self.thread_props.get(&member_id).cloned() {
            self.remove_impl(
                existing.priority,
                member_id,
                existing.active_core,
                existing.affinity,
            );
        }
        ensure_entry(&mut self.entries, member_id);
        self.thread_props.insert(
            member_id,
            ThreadProps {
                priority,
                active_core,
                affinity,
                is_dummy,
                process_schedule_count,
            },
        );
        self.push_back_impl(priority, member_id, active_core, affinity);
    }

    /// Push a thread to the front of its scheduled queue and back of suggested queues.
    /// Matches upstream `KPriorityQueue::PushFront(member)`.
    pub fn push_front(
        &mut self,
        member_id: u64,
        priority: i32,
        active_core: i32,
        affinity: u64,
        is_dummy: bool,
        process_schedule_count: Option<std::sync::Arc<std::sync::atomic::AtomicI64>>,
    ) {
        if is_dummy {
            return;
        }
        if let Some(existing) = self.thread_props.get(&member_id).cloned() {
            self.remove_impl(
                existing.priority,
                member_id,
                existing.active_core,
                existing.affinity,
            );
        }
        ensure_entry(&mut self.entries, member_id);
        self.thread_props.insert(
            member_id,
            ThreadProps {
                priority,
                active_core,
                affinity,
                is_dummy,
                process_schedule_count,
            },
        );
        self.push_front_impl(priority, member_id, active_core, affinity);
    }

    /// Remove a thread from all its queues.
    /// Matches upstream `KPriorityQueue::Remove(member)`.
    pub fn remove(
        &mut self,
        member_id: u64,
        priority: i32,
        active_core: i32,
        affinity: u64,
        is_dummy: bool,
    ) {
        if is_dummy {
            return;
        }
        self.remove_impl(priority, member_id, active_core, affinity);
        self.thread_props.remove(&member_id);
    }

    pub fn move_to_scheduled_front(
        &mut self,
        member_id: u64,
        priority: i32,
        active_core: i32,
        is_dummy: bool,
    ) {
        if is_dummy {
            return;
        }
        self.scheduled_queue
            .move_to_front(priority, active_core, member_id, &mut self.entries);
    }

    pub fn move_to_scheduled_back(
        &mut self,
        member_id: u64,
        priority: i32,
        active_core: i32,
        is_dummy: bool,
    ) -> Option<u64> {
        if is_dummy {
            return None;
        }
        self.scheduled_queue
            .move_to_back(priority, active_core, member_id, &mut self.entries)
    }

    /// Change a thread's priority in the queue.
    /// Matches upstream `KPriorityQueue::ChangePriority(prev_priority, is_running, member)`.
    pub fn change_priority(
        &mut self,
        prev_priority: i32,
        is_running: bool,
        member_id: u64,
        new_priority: i32,
        active_core: i32,
        affinity: u64,
        is_dummy: bool,
    ) {
        if is_dummy {
            return;
        }
        debug_assert!(is_valid_priority(prev_priority));

        self.remove_impl(prev_priority, member_id, active_core, affinity);

        if is_running {
            self.push_front_impl(new_priority, member_id, active_core, affinity);
        } else {
            self.push_back_impl(new_priority, member_id, active_core, affinity);
        }

        // Update cached priority
        if let Some(props) = self.thread_props.get_mut(&member_id) {
            props.priority = new_priority;
        }
    }

    /// Change a thread's affinity mask.
    /// Matches upstream `KPriorityQueue::ChangeAffinityMask(prev_core, prev_affinity, member)`.
    pub fn change_affinity_mask(
        &mut self,
        prev_core: i32,
        prev_affinity: u64,
        member_id: u64,
        new_core: i32,
        new_affinity: u64,
        priority: i32,
        is_dummy: bool,
    ) {
        if is_dummy {
            return;
        }

        let Self {
            scheduled_queue,
            suggested_queue,
            entries,
            thread_props,
        } = self;

        // Remove from all old queues
        for core in 0..NUM_CORES as i32 {
            if prev_affinity & (1u64 << core) != 0 {
                if core == prev_core {
                    scheduled_queue.remove(priority, core, member_id, entries);
                } else {
                    suggested_queue.remove(priority, core, member_id, entries);
                }
            }
        }

        // Add to all new queues
        for core in 0..NUM_CORES as i32 {
            if new_affinity & (1u64 << core) != 0 {
                if core == new_core {
                    scheduled_queue.push_back(priority, core, member_id, entries);
                } else {
                    suggested_queue.push_back(priority, core, member_id, entries);
                }
            }
        }

        // Update cached properties
        if let Some(props) = thread_props.get_mut(&member_id) {
            props.active_core = new_core;
            props.affinity = new_affinity;
        }
    }

    /// Change a thread's active core.
    /// Matches upstream `KPriorityQueue::ChangeCore(prev_core, member, to_front)`.
    ///
    /// The caller must have already set the thread's active_core to `new_core`
    /// (or this function updates the cache — the actual KThread.core_id must
    /// be updated separately if needed).
    pub fn change_core(
        &mut self,
        prev_core: i32,
        member_id: u64,
        new_core: i32,
        priority: i32,
        is_dummy: bool,
        to_front: bool,
    ) {
        if is_dummy {
            return;
        }

        if prev_core != new_core {
            let Self {
                scheduled_queue,
                suggested_queue,
                entries,
                thread_props,
            } = self;
            if prev_core >= 0 {
                scheduled_queue.remove(priority, prev_core, member_id, entries);
            }
            if new_core >= 0 {
                suggested_queue.remove(priority, new_core, member_id, entries);
                if to_front {
                    scheduled_queue.push_front(priority, new_core, member_id, entries);
                } else {
                    scheduled_queue.push_back(priority, new_core, member_id, entries);
                }
            }
            if prev_core >= 0 {
                suggested_queue.push_back(priority, prev_core, member_id, entries);
            }

            // Update cached active_core
            if let Some(props) = thread_props.get_mut(&member_id) {
                props.active_core = new_core;
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
        assert_eq!(bs.count_leading_zero(), 5); // Highest priority (lowest number)

        bs.set_bit(3);
        assert_eq!(bs.count_leading_zero(), 3); // 3 < 5, so 3 is highest priority

        bs.clear_bit(3);
        assert_eq!(bs.count_leading_zero(), 5); // Back to 5
    }

    #[test]
    fn test_bitset64_get_next_set() {
        let mut bs = BitSet64::new();
        bs.set_bit(5);
        bs.set_bit(10);
        bs.set_bit(20);

        assert_eq!(bs.get_next_set(3), 5); // Next after 3 is 5
        assert_eq!(bs.get_next_set(5), 10); // Next after 5 is 10
        assert_eq!(bs.get_next_set(10), 20); // Next after 10 is 20
        assert_eq!(bs.get_next_set(20), 64); // Nothing after 20
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

    #[test]
    fn test_push_back_and_get_front() {
        let mut pq = KPriorityQueue::new();
        // Thread 100: priority 16, active core 3, affinity = cores 0-3
        pq.push_back(100, 16, 3, 0b1111, false, None);
        assert_eq!(pq.get_scheduled_front(3), Some(100));
        // Should be suggested on cores 0, 1, 2
        assert_eq!(pq.get_suggested_front(0), Some(100));
        assert_eq!(pq.get_suggested_front(1), Some(100));
        assert_eq!(pq.get_suggested_front(2), Some(100));
    }

    #[test]
    fn test_push_and_remove() {
        let mut pq = KPriorityQueue::new();
        pq.push_back(100, 16, 3, 0b1111, false, None);
        pq.remove(100, 16, 3, 0b1111, false);
        for core in 0..NUM_CORES as i32 {
            assert!(pq.get_scheduled_front(core).is_none());
            assert!(pq.get_suggested_front(core).is_none());
        }
    }

    #[test]
    fn test_multiple_threads_ordering() {
        let mut pq = KPriorityQueue::new();
        // Two threads on same core, same priority
        pq.push_back(100, 16, 3, 0b1000, false, None);
        pq.push_back(200, 16, 3, 0b1000, false, None);
        // First pushed should be front
        assert_eq!(pq.get_scheduled_front(3), Some(100));
        // Next should be 200
        assert_eq!(pq.get_scheduled_next(3, 100, 16), Some(200));
    }

    #[test]
    fn test_change_core() {
        let mut pq = KPriorityQueue::new();
        pq.push_back(100, 16, 3, 0b1111, false, None);
        assert_eq!(pq.get_scheduled_front(3), Some(100));
        // Migrate to core 0
        pq.change_core(3, 100, 0, 16, false, true);
        assert!(pq.get_scheduled_front(3).is_none());
        assert_eq!(pq.get_scheduled_front(0), Some(100));
        // Should now be suggested on core 3
        assert_eq!(pq.get_suggested_front(3), Some(100));
        // Cached props should reflect new core
        assert_eq!(pq.get_thread_props(100).unwrap().active_core, 0);
    }

    #[test]
    fn test_dummy_thread_ignored() {
        let mut pq = KPriorityQueue::new();
        pq.push_back(100, 16, 3, 0b1111, true, None); // is_dummy = true
        assert!(pq.get_scheduled_front(3).is_none());
    }

    #[test]
    fn test_reinserting_same_thread_does_not_leave_stale_membership() {
        let mut pq = KPriorityQueue::new();
        pq.push_back(100, 16, 3, 0b1111, false, None);
        pq.push_back(100, 16, 3, 0b1111, false, None);

        assert_eq!(pq.get_scheduled_front(3), Some(100));

        pq.remove(100, 16, 3, 0b1111, false);

        for core in 0..NUM_CORES as i32 {
            assert!(pq.get_scheduled_front(core).is_none());
            assert!(pq.get_suggested_front(core).is_none());
        }
    }

    #[test]
    fn test_priority_ordering_core1() {
        let mut pq = KPriorityQueue::new();
        pq.push_back(35, 43, 1, 0x2, false, None);
        pq.push_back(42, 31, 1, 0x2, false, None);
        assert_eq!(pq.get_scheduled_front(1), Some(42),
            "tid=42 (prio=31) should be front over tid=35 (prio=43)");
    }

    #[test]
    fn test_push_remove_push_preserves_higher_prio() {
        let mut pq = KPriorityQueue::new();
        // Simulate: tid=35 pushed, then removed (SleepThread), then re-pushed (wakeup)
        pq.push_back(35, 43, 1, 0x2, false, None);
        pq.remove(35, 43, 1, 0x2, false);
        pq.push_back(35, 43, 1, 0x2, false, None);
        // Then tid=42 pushed
        pq.push_back(42, 31, 1, 0x2, false, None);
        assert_eq!(pq.get_scheduled_front(1), Some(42),
            "After cycles, tid=42 should still be front");
    }

    #[test]
    fn test_many_push_remove_cycles() {
        let mut pq = KPriorityQueue::new();
        // Simulate many SleepThread/wakeup cycles for tid=35
        for _ in 0..100 {
            pq.push_back(35, 43, 1, 0x2, false, None);
            pq.remove(35, 43, 1, 0x2, false);
        }
        pq.push_back(35, 43, 1, 0x2, false, None);
        pq.push_back(42, 31, 1, 0x2, false, None);
        assert_eq!(pq.get_scheduled_front(1), Some(42),
            "After many cycles, PQ should still work");
    }
}
