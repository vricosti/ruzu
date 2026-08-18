// Port of dynarmic/interface/exclusive_monitor.h and backend/x64/exclusive_monitor.cpp
//
// Multi-core exclusive monitor for ARM load-exclusive/store-exclusive
// synchronization. Each processor marks an address via ReadAndMark, then
// attempts a store via DoExclusiveOperation which succeeds only if the
// address is still exclusively held.

use std::sync::atomic::{AtomicU32, Ordering};

/// 128-bit value storage (two u64s), matching Dynarmic::Vector.
pub type Vector = [u64; 2];

const RESERVATION_GRANULE_MASK: u64 = 0xFFFF_FFFF_FFFF_FFFF;
const INVALID_EXCLUSIVE_ADDRESS: u64 = 0xDEAD_DEAD_DEAD_DEAD;

/// Multi-core exclusive monitor.
///
/// Matches `Dynarmic::ExclusiveMonitor` — provides ReadAndMark / DoExclusiveOperation
/// for implementing ARM LDXR/STXR (load-exclusive/store-exclusive) across cores.
pub struct ExclusiveMonitor {
    lock: SpinLock,
    exclusive_addresses: Vec<u64>,
    exclusive_values: Vec<Vector>,
}

impl ExclusiveMonitor {
    /// Create a new ExclusiveMonitor for `processor_count` cores.
    ///
    /// Matches `Dynarmic::ExclusiveMonitor::ExclusiveMonitor(size_t processor_count)`.
    pub fn new(processor_count: usize) -> Self {
        Self {
            lock: SpinLock::new(),
            exclusive_addresses: vec![INVALID_EXCLUSIVE_ADDRESS; processor_count],
            exclusive_values: vec![[0u64; 2]; processor_count],
        }
    }

    /// Number of processors this monitor supports.
    pub fn get_processor_count(&self) -> usize {
        self.exclusive_addresses.len()
    }

    /// Mark a region as exclusive for `processor_id` and read the value via `op`.
    ///
    /// Matches `Dynarmic::ExclusiveMonitor::ReadAndMark<T>`.
    /// The closure `op` performs the actual memory read.
    pub fn read_and_mark<T: Copy + Default>(
        &mut self,
        processor_id: usize,
        address: u64,
        op: impl FnOnce() -> T,
    ) -> T {
        assert!(std::mem::size_of::<T>() <= std::mem::size_of::<Vector>());
        let masked_address = address & RESERVATION_GRANULE_MASK;

        self.lock.lock();
        self.exclusive_addresses[processor_id] = masked_address;
        let value = op();
        // Store value bytes into the vector slot.
        self.exclusive_values[processor_id] = [0u64; 2];
        unsafe {
            std::ptr::copy_nonoverlapping(
                &value as *const T as *const u8,
                self.exclusive_values[processor_id].as_mut_ptr() as *mut u8,
                std::mem::size_of::<T>(),
            );
        }
        self.lock.unlock();
        value
    }

    /// Attempt an exclusive store for `processor_id` at `address`.
    ///
    /// Matches `Dynarmic::ExclusiveMonitor::DoExclusiveOperation<T>`.
    /// If this processor still has exclusive access, calls `op(saved_value)`
    /// which should perform a compare-and-swap write. Returns true if the
    /// operation succeeded.
    pub fn do_exclusive_operation<T: Copy + Default>(
        &mut self,
        processor_id: usize,
        address: u64,
        op: impl FnOnce(T) -> bool,
    ) -> bool {
        assert!(std::mem::size_of::<T>() <= std::mem::size_of::<Vector>());
        if !self.check_and_clear(processor_id, address) {
            return false;
        }

        // Extract saved value.
        let mut saved_value = T::default();
        unsafe {
            std::ptr::copy_nonoverlapping(
                self.exclusive_values[processor_id].as_ptr() as *const u8,
                &mut saved_value as *mut T as *mut u8,
                std::mem::size_of::<T>(),
            );
        }
        let result = op(saved_value);

        self.lock.unlock();
        result
    }

    /// Clear all exclusive state for all processors.
    ///
    /// Matches `Dynarmic::ExclusiveMonitor::Clear()`.
    pub fn clear(&mut self) {
        self.lock.lock();
        self.exclusive_addresses.fill(INVALID_EXCLUSIVE_ADDRESS);
        self.lock.unlock();
    }

    /// Clear exclusive state for a single processor.
    ///
    /// Matches `Dynarmic::ExclusiveMonitor::ClearProcessor(processor_id)`.
    pub fn clear_processor(&mut self, processor_id: usize) {
        self.lock.lock();
        self.exclusive_addresses[processor_id] = INVALID_EXCLUSIVE_ADDRESS;
        self.lock.unlock();
    }

    /// Check if `processor_id` holds exclusive access to `address`.
    /// If so, clears all processors that share the same masked address
    /// and returns true (lock remains held for DoExclusiveOperation).
    /// If not, returns false (lock is released).
    ///
    /// Matches `Dynarmic::ExclusiveMonitor::CheckAndClear`.
    fn check_and_clear(&mut self, processor_id: usize, address: u64) -> bool {
        let masked_address = address & RESERVATION_GRANULE_MASK;

        self.lock.lock();
        if self.exclusive_addresses[processor_id] != masked_address {
            self.lock.unlock();
            return false;
        }

        // Clear all processors that share this address.
        for addr in &mut self.exclusive_addresses {
            if *addr == masked_address {
                *addr = INVALID_EXCLUSIVE_ADDRESS;
            }
        }
        // Lock remains held — caller (DoExclusiveOperation) will unlock.
        true
    }

    // Accessors for JIT backend integration (matching dynarmic friend functions).

    /// Get a mutable pointer to the exclusive address for a processor.
    pub fn get_address_ptr(&mut self, index: usize) -> *mut u64 {
        &mut self.exclusive_addresses[index]
    }

    /// Get a mutable pointer to the exclusive value for a processor.
    pub fn get_value_ptr(&mut self, index: usize) -> *mut Vector {
        &mut self.exclusive_values[index]
    }

    /// Get a pointer to the lock for JIT backend use.
    pub fn get_lock_ptr(&mut self) -> *mut SpinLock {
        &mut self.lock
    }

    /// Get a raw pointer to the lock's underlying u32 storage, suitable for
    /// the JIT-emitted inline lock sequence (`lock xchg dword [ptr]`).
    ///
    /// Matches upstream `GetExclusiveMonitorLockPointer` which returns
    /// `volatile int*` pointing at the same 4-byte storage as `SpinLock`'s
    /// `storage` field. The AtomicU32 has the same layout as a `u32`.
    pub fn get_lock_storage_ptr(&mut self) -> *mut u32 {
        &raw mut self.lock.locked as *mut u32
    }

    /// Processor count, exposed for the JIT-emitted `EmitExclusiveTestAndClear`
    /// loop that needs to invalidate other processors' reservations.
    pub fn processor_count(&self) -> usize {
        self.exclusive_addresses.len()
    }
}

// SAFETY: ExclusiveMonitor uses a SpinLock for thread safety.
unsafe impl Send for ExclusiveMonitor {}
unsafe impl Sync for ExclusiveMonitor {}

/// Simple spin lock matching `Dynarmic::SpinLock`.
///
/// Uses `AtomicU32` (4-byte storage) rather than `AtomicBool` (1-byte) to
/// match upstream Dynarmic's `volatile int* storage`. The JIT-emitted
/// inline lock/unlock sequences (`EmitSpinLockLock`/`EmitSpinLockUnlock` in
/// `spin_lock_x64.cpp`) use 32-bit `lock xchg [dword ptr]` against this
/// field, so byte alignment / width must match.
#[repr(C)]
pub struct SpinLock {
    locked: AtomicU32,
}

impl SpinLock {
    pub fn new() -> Self {
        Self {
            locked: AtomicU32::new(0),
        }
    }

    pub fn lock(&self) {
        while self
            .locked
            .compare_exchange_weak(0, 1, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            // Spin with a hint to reduce contention.
            std::hint::spin_loop();
        }
    }

    pub fn unlock(&self) {
        self.locked.store(0, Ordering::Release);
    }
}

impl Default for SpinLock {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_exclusive_rw() {
        let mut monitor = ExclusiveMonitor::new(4);
        assert_eq!(monitor.get_processor_count(), 4);

        // Processor 0 reads address 0x1000.
        let val: u32 = monitor.read_and_mark(0, 0x1000, || 42u32);
        assert_eq!(val, 42);

        // Processor 0 writes exclusively — should succeed.
        let success = monitor.do_exclusive_operation::<u32>(0, 0x1000, |saved| {
            assert_eq!(saved, 42);
            true // Simulate successful CAS
        });
        assert!(success);
    }

    #[test]
    fn test_exclusive_fails_after_clear() {
        let mut monitor = ExclusiveMonitor::new(2);

        monitor.read_and_mark::<u32>(0, 0x2000, || 99);
        monitor.clear_processor(0);

        let success = monitor.do_exclusive_operation::<u32>(0, 0x2000, |_| true);
        assert!(!success);
    }

    #[test]
    fn test_exclusive_cross_processor_invalidation() {
        let mut monitor = ExclusiveMonitor::new(2);

        // Both processors mark the same address.
        monitor.read_and_mark::<u32>(0, 0x3000, || 10);
        monitor.read_and_mark::<u32>(1, 0x3000, || 20);

        // Processor 0 does exclusive op — clears both.
        let success = monitor.do_exclusive_operation::<u32>(0, 0x3000, |saved| {
            assert_eq!(saved, 10);
            true
        });
        assert!(success);

        // Processor 1's exclusive should now fail.
        let success = monitor.do_exclusive_operation::<u32>(1, 0x3000, |_| true);
        assert!(!success);
    }

    #[test]
    fn test_exclusive_different_addresses() {
        let mut monitor = ExclusiveMonitor::new(2);

        monitor.read_and_mark::<u64>(0, 0x4000, || 100);
        monitor.read_and_mark::<u64>(1, 0x5000, || 200);

        // Both should succeed since different addresses.
        let s0 = monitor.do_exclusive_operation::<u64>(0, 0x4000, |saved| {
            assert_eq!(saved, 100);
            true
        });
        let s1 = monitor.do_exclusive_operation::<u64>(1, 0x5000, |saved| {
            assert_eq!(saved, 200);
            true
        });
        assert!(s0);
        assert!(s1);
    }

    #[test]
    fn test_exclusive_128bit() {
        let mut monitor = ExclusiveMonitor::new(1);

        let val: u128 = monitor.read_and_mark(0, 0x6000, || {
            0xDEAD_BEEF_CAFE_BABEu128 << 64 | 0x1234_5678_9ABC_DEF0u128
        });
        assert_eq!(
            val,
            0xDEAD_BEEF_CAFE_BABEu128 << 64 | 0x1234_5678_9ABC_DEF0u128
        );

        let success = monitor.do_exclusive_operation::<u128>(0, 0x6000, |saved| {
            assert_eq!(saved, val);
            true
        });
        assert!(success);
    }

    #[test]
    fn test_clear_all() {
        let mut monitor = ExclusiveMonitor::new(3);

        monitor.read_and_mark::<u32>(0, 0x7000, || 1);
        monitor.read_and_mark::<u32>(1, 0x8000, || 2);
        monitor.read_and_mark::<u32>(2, 0x9000, || 3);

        monitor.clear();

        assert!(!monitor.do_exclusive_operation::<u32>(0, 0x7000, |_| true));
        assert!(!monitor.do_exclusive_operation::<u32>(1, 0x8000, |_| true));
        assert!(!monitor.do_exclusive_operation::<u32>(2, 0x9000, |_| true));
    }
}
