//! Port of zuyu/src/core/hle/kernel/k_resource_limit.h and k_resource_limit.cpp
//! Status: Stubbed (depends on KAutoObject, KLightLock, KLightConditionVariable, CoreTiming)
//! Derniere synchro: 2026-03-11

use std::sync::Mutex;

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

/// Default timeout for resource limit reservation: 10 seconds in nanoseconds.
const DEFAULT_TIMEOUT: i64 = 10_000_000_000;

type ResourceArray = [i64; LIMITABLE_RESOURCE_COUNT];

/// Port of Kernel::KResourceLimit.
///
/// Simplified: does not inherit KAutoObjectWithSlabHeapAndContainer.
/// Lock/condvar are stubbed with std::sync::Mutex (upstream uses KLightLock + KLightConditionVariable).
pub struct KResourceLimit {
    m_limit_values: ResourceArray,
    m_current_values: ResourceArray,
    m_current_hints: ResourceArray,
    m_peak_values: ResourceArray,
    m_lock: Mutex<()>,
    m_waiter_count: i32,
}

impl KResourceLimit {
    pub fn new() -> Self {
        Self {
            m_limit_values: [0; LIMITABLE_RESOURCE_COUNT],
            m_current_values: [0; LIMITABLE_RESOURCE_COUNT],
            m_current_hints: [0; LIMITABLE_RESOURCE_COUNT],
            m_peak_values: [0; LIMITABLE_RESOURCE_COUNT],
            m_lock: Mutex::new(()),
            m_waiter_count: 0,
        }
    }

    pub fn initialize(&mut self) {}
    pub fn finalize(&mut self) {}

    pub fn get_limit_value(&self, which: LimitableResource) -> i64 {
        let index = which as usize;
        let _lk = self.m_lock.lock().unwrap();
        let value = self.m_limit_values[index];
        debug_assert!(value >= 0);
        debug_assert!(self.m_current_values[index] <= self.m_limit_values[index]);
        debug_assert!(self.m_current_hints[index] <= self.m_current_values[index]);
        value
    }

    pub fn get_current_value(&self, which: LimitableResource) -> i64 {
        let index = which as usize;
        let _lk = self.m_lock.lock().unwrap();
        let value = self.m_current_values[index];
        debug_assert!(value >= 0);
        debug_assert!(self.m_current_values[index] <= self.m_limit_values[index]);
        debug_assert!(self.m_current_hints[index] <= self.m_current_values[index]);
        value
    }

    pub fn get_peak_value(&self, which: LimitableResource) -> i64 {
        let index = which as usize;
        let _lk = self.m_lock.lock().unwrap();
        let value = self.m_peak_values[index];
        debug_assert!(value >= 0);
        debug_assert!(self.m_current_values[index] <= self.m_limit_values[index]);
        debug_assert!(self.m_current_hints[index] <= self.m_current_values[index]);
        value
    }

    pub fn get_free_value(&self, which: LimitableResource) -> i64 {
        let index = which as usize;
        let _lk = self.m_lock.lock().unwrap();
        debug_assert!(self.m_current_values[index] >= 0);
        debug_assert!(self.m_current_values[index] <= self.m_limit_values[index]);
        debug_assert!(self.m_current_hints[index] <= self.m_current_values[index]);
        self.m_limit_values[index] - self.m_current_values[index]
    }

    /// Returns Ok(()) on success, Err(ResultInvalidState) if current > value.
    pub fn set_limit_value(&mut self, which: LimitableResource, value: i64) -> Result<(), ()> {
        let index = which as usize;
        let _lk = self.m_lock.lock().unwrap();
        if self.m_current_values[index] > value {
            return Err(());
        }
        self.m_limit_values[index] = value;
        self.m_peak_values[index] = self.m_current_values[index];
        Ok(())
    }

    /// Reserve resources. Simplified: no timed wait (condvar stubbed).
    pub fn reserve(&mut self, which: LimitableResource, value: i64) -> bool {
        debug_assert!(value >= 0);
        let index = which as usize;
        let _lk = self.m_lock.lock().unwrap();

        debug_assert!(self.m_current_hints[index] <= self.m_current_values[index]);
        if self.m_current_hints[index] >= self.m_limit_values[index] {
            return false;
        }

        // Check overflow.
        if self.m_current_values[index].wrapping_add(value) <= self.m_current_values[index] {
            return false;
        }

        if self.m_current_values[index] + value <= self.m_limit_values[index] {
            self.m_current_values[index] += value;
            self.m_current_hints[index] += value;
            self.m_peak_values[index] =
                self.m_peak_values[index].max(self.m_current_values[index]);
            return true;
        }

        false
    }

    pub fn release(&mut self, which: LimitableResource, value: i64) {
        self.release_with_hint(which, value, value);
    }

    pub fn release_with_hint(&mut self, which: LimitableResource, value: i64, hint: i64) {
        debug_assert!(value >= 0);
        debug_assert!(hint >= 0);

        let index = which as usize;
        let _lk = self.m_lock.lock().unwrap();
        debug_assert!(self.m_current_values[index] <= self.m_limit_values[index]);
        debug_assert!(self.m_current_hints[index] <= self.m_current_values[index]);
        debug_assert!(value <= self.m_current_values[index]);
        debug_assert!(hint <= self.m_current_hints[index]);

        self.m_current_values[index] -= value;
        self.m_current_hints[index] -= hint;
    }

    pub fn post_destroy(_arg: usize) {}
}

impl Default for KResourceLimit {
    fn default() -> Self {
        Self::new()
    }
}

/// Creates a resource limit with default values for a process.
/// Upstream: Kernel::CreateResourceLimitForProcess
pub fn create_resource_limit_for_process(physical_memory_size: i64) -> KResourceLimit {
    let mut resource_limit = KResourceLimit::new();
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
    fn test_resource_limit_basic() {
        let mut rl = KResourceLimit::new();
        rl.initialize();
        rl.set_limit_value(LimitableResource::PhysicalMemoryMax, 1000)
            .unwrap();
        assert_eq!(rl.get_limit_value(LimitableResource::PhysicalMemoryMax), 1000);
        assert_eq!(rl.get_free_value(LimitableResource::PhysicalMemoryMax), 1000);

        assert!(rl.reserve(LimitableResource::PhysicalMemoryMax, 500));
        assert_eq!(rl.get_current_value(LimitableResource::PhysicalMemoryMax), 500);
        assert_eq!(rl.get_free_value(LimitableResource::PhysicalMemoryMax), 500);

        rl.release(LimitableResource::PhysicalMemoryMax, 500);
        assert_eq!(rl.get_current_value(LimitableResource::PhysicalMemoryMax), 0);
    }

    #[test]
    fn test_create_resource_limit_for_process() {
        let rl = create_resource_limit_for_process(0x1_0000_0000);
        assert_eq!(
            rl.get_limit_value(LimitableResource::PhysicalMemoryMax),
            0x1_0000_0000
        );
        assert_eq!(rl.get_limit_value(LimitableResource::ThreadCountMax), 800);
        assert_eq!(rl.get_limit_value(LimitableResource::EventCountMax), 900);
        assert_eq!(rl.get_limit_value(LimitableResource::TransferMemoryCountMax), 200);
        assert_eq!(rl.get_limit_value(LimitableResource::SessionCountMax), 1133);
    }
}
