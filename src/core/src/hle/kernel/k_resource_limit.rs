//! Port of Eden's core/hle/kernel/k_resource_limit.h and k_resource_limit.cpp.

use std::cell::UnsafeCell;

use super::k_light_condition_variable::KLightConditionVariable;
use super::k_light_lock::{KLightLock, KScopedLightLock};

/// Matches Svc::LimitableResource from upstream.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LimitableResource {
    PhysicalMemoryMax = 0,
    ThreadCountMax = 1,
    EventCountMax = 2,
    TransferMemoryCountMax = 3,
    SessionCountMax = 4,
    Count = 5,
}

pub const LIMITABLE_RESOURCE_COUNT: usize = LimitableResource::Count as usize;

pub fn is_valid_resource_type(r: LimitableResource) -> bool {
    (r as u32) < LimitableResource::Count as u32
}

/// Eden's `DefaultTimeout`: ten seconds, expressed in timer ticks.
const DEFAULT_TIMEOUT: i64 = 10_000_000_000;

type ResourceArray = [i64; LIMITABLE_RESOURCE_COUNT];

/// Port of `Kernel::KResourceLimit`.
///
/// Eden mutates the resource arrays through `const` methods while holding
/// `m_lock`. The `UnsafeCell` fields preserve that ownership model in Rust:
/// every access is serialized by the object's `KLightLock`.
pub struct KResourceLimit {
    m_limit_values: UnsafeCell<ResourceArray>,
    m_current_values: UnsafeCell<ResourceArray>,
    m_current_hints: UnsafeCell<ResourceArray>,
    m_peak_values: UnsafeCell<ResourceArray>,
    m_lock: KLightLock,
    m_waiter_count: UnsafeCell<i32>,
    m_cond_var: KLightConditionVariable,
}

// SAFETY: all accesses to the UnsafeCell fields are serialized by m_lock.
// The lock is released only while KLightConditionVariable has suspended the
// current guest thread, and no references into the cells cross that wait.
unsafe impl Send for KResourceLimit {}
unsafe impl Sync for KResourceLimit {}

impl KResourceLimit {
    pub fn new() -> Self {
        Self {
            m_limit_values: UnsafeCell::new([0; LIMITABLE_RESOURCE_COUNT]),
            m_current_values: UnsafeCell::new([0; LIMITABLE_RESOURCE_COUNT]),
            m_current_hints: UnsafeCell::new([0; LIMITABLE_RESOURCE_COUNT]),
            m_peak_values: UnsafeCell::new([0; LIMITABLE_RESOURCE_COUNT]),
            m_lock: KLightLock::new(),
            m_waiter_count: UnsafeCell::new(0),
            m_cond_var: KLightConditionVariable::new(),
        }
    }

    pub fn initialize(&self) {}
    pub fn finalize(&self) {}

    pub fn get_limit_value(&self, which: LimitableResource) -> i64 {
        let index = which as usize;
        let _lk = KScopedLightLock::new(&self.m_lock);
        // SAFETY: m_lock is held for all accesses to the resource arrays.
        unsafe {
            let value = (*self.m_limit_values.get())[index];
            debug_assert!(value >= 0);
            debug_assert!((*self.m_current_values.get())[index] <= value);
            debug_assert!(
                (*self.m_current_hints.get())[index] <= (*self.m_current_values.get())[index]
            );
            value
        }
    }

    pub fn get_current_value(&self, which: LimitableResource) -> i64 {
        let index = which as usize;
        let _lk = KScopedLightLock::new(&self.m_lock);
        // SAFETY: m_lock is held for all accesses to the resource arrays.
        unsafe {
            let value = (*self.m_current_values.get())[index];
            debug_assert!(value >= 0);
            debug_assert!(value <= (*self.m_limit_values.get())[index]);
            debug_assert!((*self.m_current_hints.get())[index] <= value);
            value
        }
    }

    pub fn get_peak_value(&self, which: LimitableResource) -> i64 {
        let index = which as usize;
        let _lk = KScopedLightLock::new(&self.m_lock);
        // SAFETY: m_lock is held for all accesses to the resource arrays.
        unsafe {
            let value = (*self.m_peak_values.get())[index];
            debug_assert!(value >= 0);
            debug_assert!(
                (*self.m_current_values.get())[index] <= (*self.m_limit_values.get())[index]
            );
            debug_assert!(
                (*self.m_current_hints.get())[index] <= (*self.m_current_values.get())[index]
            );
            value
        }
    }

    pub fn get_free_value(&self, which: LimitableResource) -> i64 {
        let index = which as usize;
        let _lk = KScopedLightLock::new(&self.m_lock);
        // SAFETY: m_lock is held for all accesses to the resource arrays.
        unsafe {
            let current = (*self.m_current_values.get())[index];
            let limit = (*self.m_limit_values.get())[index];
            debug_assert!(current >= 0);
            debug_assert!(current <= limit);
            debug_assert!((*self.m_current_hints.get())[index] <= current);
            limit - current
        }
    }

    pub fn set_limit_value(&self, which: LimitableResource, value: i64) -> Result<(), ()> {
        let index = which as usize;
        let _lk = KScopedLightLock::new(&self.m_lock);
        // SAFETY: m_lock is held for all accesses to the resource arrays.
        unsafe {
            let current = (*self.m_current_values.get())[index];
            if current > value {
                return Err(());
            }
            (*self.m_limit_values.get())[index] = value;
            (*self.m_peak_values.get())[index] = current;
        }
        Ok(())
    }

    /// Reserve with Eden's default absolute timeout of ten seconds.
    pub fn reserve(&self, which: LimitableResource, value: i64) -> bool {
        let timeout = current_hardware_tick().unwrap_or(0) + DEFAULT_TIMEOUT;
        self.reserve_with_timeout(which, value, timeout)
    }

    /// Reserve until `timeout`, which is an absolute hardware-timer tick.
    pub fn reserve_with_timeout(&self, which: LimitableResource, value: i64, timeout: i64) -> bool {
        debug_assert!(value >= 0);
        let index = which as usize;
        let _lk = KScopedLightLock::new(&self.m_lock);

        // SAFETY: m_lock is held. No UnsafeCell reference is retained across
        // m_cond_var.wait(), which temporarily releases m_lock.
        unsafe {
            debug_assert!(
                (*self.m_current_hints.get())[index] <= (*self.m_current_values.get())[index]
            );
            if (*self.m_current_hints.get())[index] >= (*self.m_limit_values.get())[index] {
                return false;
            }
        }

        loop {
            // SAFETY: m_lock is held at the start of every iteration.
            let should_wait = unsafe {
                let current = (*self.m_current_values.get())[index];
                let hint = (*self.m_current_hints.get())[index];
                let limit = (*self.m_limit_values.get())[index];
                debug_assert!(current <= limit);
                debug_assert!(hint <= current);

                if current.wrapping_add(value) <= current {
                    return false;
                }

                if current + value <= limit {
                    let new_current = current + value;
                    (*self.m_current_values.get())[index] = new_current;
                    (*self.m_current_hints.get())[index] = hint + value;
                    (*self.m_peak_values.get())[index] =
                        (*self.m_peak_values.get())[index].max(new_current);
                    return true;
                }

                hint + value <= limit
                    && (timeout < 0 || current_hardware_tick().unwrap_or(i64::MAX) < timeout)
            };

            if !should_wait {
                break;
            }

            // SAFETY: m_lock is held. KLightConditionVariable releases it
            // during the guest wait and reacquires it before returning.
            unsafe {
                *self.m_waiter_count.get() += 1;
            }
            self.m_cond_var.wait(&self.m_lock, timeout, false);
            // SAFETY: wait returned with m_lock held again.
            unsafe {
                *self.m_waiter_count.get() -= 1;
            }
        }

        false
    }

    pub fn release(&self, which: LimitableResource, value: i64) {
        self.release_with_hint(which, value, value);
    }

    pub fn release_with_hint(&self, which: LimitableResource, value: i64, hint: i64) {
        debug_assert!(value >= 0);
        debug_assert!(hint >= 0);

        let index = which as usize;
        let _lk = KScopedLightLock::new(&self.m_lock);
        // SAFETY: m_lock is held for all accesses to the resource arrays and
        // waiter count.
        unsafe {
            let current = (*self.m_current_values.get())[index];
            let current_hint = (*self.m_current_hints.get())[index];
            debug_assert!(current <= (*self.m_limit_values.get())[index]);
            debug_assert!(current_hint <= current);
            debug_assert!(value <= current);
            debug_assert!(hint <= current_hint);

            (*self.m_current_values.get())[index] = current - value;
            (*self.m_current_hints.get())[index] = current_hint - hint;

            if *self.m_waiter_count.get() != 0 {
                self.m_cond_var.broadcast();
            }
        }
    }

    pub fn post_destroy(_arg: usize) {}
}

fn current_hardware_tick() -> Option<i64> {
    super::kernel::get_hardware_timer_arc()
        .map(|timer| timer.get_tick())
        .or_else(super::kernel::get_current_hardware_tick)
}

impl Default for KResourceLimit {
    fn default() -> Self {
        Self::new()
    }
}

/// Creates a resource limit with the default values used for a process.
pub fn create_resource_limit_for_process(physical_memory_size: i64) -> KResourceLimit {
    let resource_limit = KResourceLimit::new();
    resource_limit.initialize();

    resource_limit
        .set_limit_value(LimitableResource::PhysicalMemoryMax, physical_memory_size)
        .expect("Failed to set PhysicalMemoryMax");
    resource_limit
        .set_limit_value(LimitableResource::ThreadCountMax, 800)
        .expect("Failed to set ThreadCountMax");
    resource_limit
        .set_limit_value(LimitableResource::EventCountMax, 900)
        .expect("Failed to set EventCountMax");
    resource_limit
        .set_limit_value(LimitableResource::TransferMemoryCountMax, 200)
        .expect("Failed to set TransferMemoryCountMax");
    resource_limit
        .set_limit_value(LimitableResource::SessionCountMax, 1133)
        .expect("Failed to set SessionCountMax");

    resource_limit
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resource_limit_tracks_current_free_and_peak_values() {
        let rl = KResourceLimit::new();
        rl.initialize();
        rl.set_limit_value(LimitableResource::PhysicalMemoryMax, 1000)
            .unwrap();
        assert_eq!(
            rl.get_limit_value(LimitableResource::PhysicalMemoryMax),
            1000
        );
        assert_eq!(
            rl.get_free_value(LimitableResource::PhysicalMemoryMax),
            1000
        );

        assert!(rl.reserve(LimitableResource::PhysicalMemoryMax, 500));
        assert_eq!(
            rl.get_current_value(LimitableResource::PhysicalMemoryMax),
            500
        );
        assert_eq!(rl.get_peak_value(LimitableResource::PhysicalMemoryMax), 500);
        assert_eq!(rl.get_free_value(LimitableResource::PhysicalMemoryMax), 500);

        rl.release(LimitableResource::PhysicalMemoryMax, 500);
        assert_eq!(
            rl.get_current_value(LimitableResource::PhysicalMemoryMax),
            0
        );
        assert_eq!(rl.get_peak_value(LimitableResource::PhysicalMemoryMax), 500);
    }

    #[test]
    fn reservation_rejects_limit_and_overflow_edges() {
        let rl = KResourceLimit::new();
        rl.set_limit_value(LimitableResource::ThreadCountMax, 2)
            .unwrap();
        assert!(rl.reserve_with_timeout(LimitableResource::ThreadCountMax, 2, -1));
        assert!(!rl.reserve_with_timeout(LimitableResource::ThreadCountMax, 1, -1));
        rl.release(LimitableResource::ThreadCountMax, 2);
        assert!(!rl.reserve_with_timeout(LimitableResource::ThreadCountMax, i64::MAX, -1));
    }

    #[test]
    fn create_resource_limit_uses_upstream_process_defaults() {
        let rl = create_resource_limit_for_process(0x1_0000_0000);
        assert_eq!(
            rl.get_limit_value(LimitableResource::PhysicalMemoryMax),
            0x1_0000_0000
        );
        assert_eq!(rl.get_limit_value(LimitableResource::ThreadCountMax), 800);
        assert_eq!(rl.get_limit_value(LimitableResource::EventCountMax), 900);
        assert_eq!(
            rl.get_limit_value(LimitableResource::TransferMemoryCountMax),
            200
        );
        assert_eq!(rl.get_limit_value(LimitableResource::SessionCountMax), 1133);
    }
}
