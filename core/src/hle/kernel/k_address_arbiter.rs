//! Port of zuyu/src/core/hle/kernel/k_address_arbiter.h/.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-21
//!
//! KAddressArbiter: implements userspace address-based thread arbitration
//! (futex-like). Uses a BTreeMap to track waiting threads by address,
//! with Condvar-based blocking and real guest memory read/write.
//!
//! Upstream stores `Core::System& m_system` and `KernelCore& m_kernel`.
//! Memory is accessed dynamically via `GetCurrentMemory(m_kernel)` which
//! resolves to `GetCurrentProcess(kernel).GetMemory()`. This supports
//! multiple processes — each arbiter call accesses the *current* process's
//! memory, not a fixed one.

use std::collections::BTreeMap;
use std::sync::{Arc, Condvar, Mutex, RwLock};

use crate::hle::kernel::k_process::{KProcess, ProcessMemoryData};
use crate::hle::kernel::kernel::KernelCore;
use crate::hle::result::ResultCode;

/// Type alias matching KProcess::SharedProcessMemory.
type SharedProcessMemory = Arc<RwLock<ProcessMemoryData>>;

/// Signal type for address arbiter operations.
/// Maps to Svc::SignalType.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignalType {
    Signal = 0,
    SignalAndIncrementIfEqual = 1,
    SignalAndModifyByWaitingCountIfEqual = 2,
}

/// Arbitration type for address arbiter wait operations.
/// Maps to Svc::ArbitrationType.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArbitrationType {
    WaitIfLessThan = 0,
    DecrementAndWaitIfLessThan = 1,
    WaitIfEqual = 2,
}

// --- Guest memory access helpers matching upstream k_address_arbiter.cpp:25-108 ---

/// Read a 32-bit value from guest memory.
/// Port of upstream `ReadFromUser`.
/// Upstream: `*out = GetCurrentMemory(kernel).Read32(GetInteger(address));`
fn read_from_user(memory: &SharedProcessMemory, address: u64) -> (bool, i32) {
    let mem = memory.read().unwrap();
    let value = mem.read_32(address) as i32;
    (true, value)
}

/// Atomically decrement the value at `address` if it is less than `value`.
/// Port of upstream `DecrementIfLessThan`.
/// Upstream uses ExclusiveMonitor CAS loop. We use a write lock for atomicity.
fn decrement_if_less_than(
    memory: &SharedProcessMemory,
    address: u64,
    value: i32,
) -> (bool, i32) {
    let mut mem = memory.write().unwrap();
    let current_value = mem.read_32(address) as i32;

    if current_value < value {
        mem.write_32(address, (current_value - 1) as u32);
    }

    (true, current_value)
}

/// Atomically update the value at `address` to `new_value` if it currently equals `value`.
/// Port of upstream `UpdateIfEqual`.
/// Upstream uses ExclusiveMonitor CAS loop. We use a write lock for atomicity.
fn update_if_equal(
    memory: &SharedProcessMemory,
    address: u64,
    value: i32,
    new_value: i32,
) -> (bool, i32) {
    let mut mem = memory.write().unwrap();
    let current_value = mem.read_32(address) as i32;

    if current_value == value {
        mem.write_32(address, new_value as u32);
    }

    (true, current_value)
}

/// Per-address wait state: tracks waiting thread count and provides a Condvar.
struct AddressWaitEntry {
    waiter_count: u32,
    cv: Arc<Condvar>,
}

/// Resolve the current process's memory from the kernel.
/// Matches upstream: `GetCurrentMemory(kernel)` →
/// `GetCurrentProcess(kernel).GetMemory()` →
/// `kernel.GetCurrentEmuThread().GetOwnerProcess().GetMemory()`
fn get_current_memory(kernel: &Arc<Mutex<KernelCore>>) -> Option<SharedProcessMemory> {
    let kernel_guard = kernel.lock().unwrap();
    let thread_arc = kernel_guard.get_current_emu_thread()?;
    let thread_guard = thread_arc.lock().unwrap();
    let process_arc = thread_guard.parent.as_ref()?.upgrade()?;
    let process_guard = process_arc.lock().unwrap();
    Some(process_guard.process_memory.clone())
}

/// Address arbiter for thread synchronization on userspace addresses.
///
/// Port of upstream `KAddressArbiter` (k_address_arbiter.h).
/// Upstream stores `Core::System& m_system` and `KernelCore& m_kernel`.
/// We store `Arc<Mutex<KernelCore>>` to resolve the current process's memory
/// dynamically via `get_current_memory()`, supporting multiple processes.
pub struct KAddressArbiter {
    /// Map from guest virtual address to wait state.
    wait_map: Mutex<BTreeMap<u64, AddressWaitEntry>>,
    /// Kernel reference for resolving current process memory.
    /// Upstream: `KernelCore& m_kernel` (from `m_system.Kernel()`).
    kernel: Option<Arc<Mutex<KernelCore>>>,
}

impl KAddressArbiter {
    pub fn new() -> Self {
        Self {
            wait_map: Mutex::new(BTreeMap::new()),
            kernel: None,
        }
    }

    /// Create with a kernel reference.
    /// Matches upstream `KAddressArbiter(Core::System& system)` which stores
    /// `m_system{system}, m_kernel{system.Kernel()}`.
    pub fn with_kernel(kernel: Arc<Mutex<KernelCore>>) -> Self {
        Self {
            wait_map: Mutex::new(BTreeMap::new()),
            kernel: Some(kernel),
        }
    }

    /// Set the kernel reference (for deferred initialization).
    pub fn set_kernel(&mut self, kernel: Arc<Mutex<KernelCore>>) {
        self.kernel = Some(kernel);
    }

    /// Resolve the current process's guest memory.
    /// Matches upstream `GetCurrentMemory(m_kernel)`.
    fn current_memory(&self) -> Option<SharedProcessMemory> {
        self.kernel.as_ref().and_then(get_current_memory)
    }

    /// Dispatch a signal operation to the given address.
    pub fn signal_to_address(
        &self,
        addr: u64,
        signal_type: SignalType,
        value: i32,
        count: i32,
    ) -> ResultCode {
        match signal_type {
            SignalType::Signal => self.signal(addr, count),
            SignalType::SignalAndIncrementIfEqual => {
                self.signal_and_increment_if_equal(addr, value, count)
            }
            SignalType::SignalAndModifyByWaitingCountIfEqual => {
                self.signal_and_modify_by_waiting_count_if_equal(addr, value, count)
            }
        }
    }

    /// Dispatch a wait operation on the given address.
    pub fn wait_for_address(
        &self,
        addr: u64,
        arb_type: ArbitrationType,
        value: i32,
        timeout: i64,
    ) -> ResultCode {
        match arb_type {
            ArbitrationType::WaitIfLessThan => self.wait_if_less_than(addr, value, false, timeout),
            ArbitrationType::DecrementAndWaitIfLessThan => {
                self.wait_if_less_than(addr, value, true, timeout)
            }
            ArbitrationType::WaitIfEqual => self.wait_if_equal(addr, value, timeout),
        }
    }

    // -- Private methods matching upstream --

    /// Signal waiting threads at the given address.
    /// Port of upstream `KAddressArbiter::Signal`.
    fn signal(&self, addr: u64, count: i32) -> ResultCode {
        let map = self.wait_map.lock().unwrap();
        if let Some(entry) = map.get(&addr) {
            let cv = entry.cv.clone();
            if count <= 0 {
                cv.notify_all();
            } else {
                for _ in 0..count {
                    cv.notify_one();
                }
            }
        }
        ResultCode::new(0)
    }

    /// Signal and atomically increment the value at address if equal.
    /// Port of upstream `KAddressArbiter::SignalAndIncrementIfEqual`.
    fn signal_and_increment_if_equal(
        &self,
        addr: u64,
        value: i32,
        count: i32,
    ) -> ResultCode {
        let memory = match self.current_memory() {
            Some(m) => m,
            None => return crate::hle::kernel::svc::svc_results::RESULT_INVALID_CURRENT_MEMORY,
        };

        let (succeeded, user_value) = update_if_equal(&memory, addr, value, value + 1);

        if !succeeded {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_CURRENT_MEMORY;
        }
        if user_value != value {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
        }

        self.signal(addr, count)
    }

    /// Signal and modify value based on waiting count.
    /// Port of upstream `KAddressArbiter::SignalAndModifyByWaitingCountIfEqual`.
    fn signal_and_modify_by_waiting_count_if_equal(
        &self,
        addr: u64,
        value: i32,
        count: i32,
    ) -> ResultCode {
        let memory = match self.current_memory() {
            Some(m) => m,
            None => return crate::hle::kernel::svc::svc_results::RESULT_INVALID_CURRENT_MEMORY,
        };

        // Determine new_value based on waiter count (matching upstream lines 192-217).
        let (has_waiters, waiter_count_at_addr) = {
            let map = self.wait_map.lock().unwrap();
            if let Some(entry) = map.get(&addr) {
                (entry.waiter_count > 0, entry.waiter_count as i32)
            } else {
                (false, 0)
            }
        };

        let new_value = if count <= 0 {
            if has_waiters { value - 2 } else { value + 1 }
        } else if has_waiters {
            if waiter_count_at_addr - 1 < count { value - 1 } else { value }
        } else {
            value + 1
        };

        let (succeeded, user_value) = if value != new_value {
            update_if_equal(&memory, addr, value, new_value)
        } else {
            read_from_user(&memory, addr)
        };

        if !succeeded {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_CURRENT_MEMORY;
        }
        if user_value != value {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
        }

        self.signal(addr, count)
    }

    /// Wait if the value at addr is less than the given value.
    /// Port of upstream `KAddressArbiter::WaitIfLessThan`.
    fn wait_if_less_than(
        &self,
        addr: u64,
        value: i32,
        decrement: bool,
        timeout: i64,
    ) -> ResultCode {
        let memory = match self.current_memory() {
            Some(m) => m,
            None => return crate::hle::kernel::svc::svc_results::RESULT_INVALID_CURRENT_MEMORY,
        };

        let (succeeded, user_value) = if decrement {
            decrement_if_less_than(&memory, addr, value)
        } else {
            read_from_user(&memory, addr)
        };

        if !succeeded {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_CURRENT_MEMORY;
        }
        if user_value >= value {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
        }
        if timeout == 0 {
            return crate::hle::kernel::svc::svc_results::RESULT_TIMED_OUT;
        }

        self.wait_on_address(addr, timeout)
    }

    /// Wait if the value at addr equals the given value.
    /// Port of upstream `KAddressArbiter::WaitIfEqual`.
    fn wait_if_equal(&self, addr: u64, value: i32, timeout: i64) -> ResultCode {
        let memory = match self.current_memory() {
            Some(m) => m,
            None => return crate::hle::kernel::svc::svc_results::RESULT_INVALID_CURRENT_MEMORY,
        };

        let (succeeded, user_value) = read_from_user(&memory, addr);

        if !succeeded {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_CURRENT_MEMORY;
        }
        if user_value != value {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
        }
        if timeout == 0 {
            return crate::hle::kernel::svc::svc_results::RESULT_TIMED_OUT;
        }

        self.wait_on_address(addr, timeout)
    }

    /// Common wait implementation: parks the calling thread until signaled or timeout.
    fn wait_on_address(&self, addr: u64, timeout: i64) -> ResultCode {
        let cv = {
            let mut map = self.wait_map.lock().unwrap();
            let entry = map.entry(addr).or_insert_with(|| AddressWaitEntry {
                waiter_count: 0,
                cv: Arc::new(Condvar::new()),
            });
            entry.waiter_count += 1;
            entry.cv.clone()
        };

        let park_mutex = Mutex::new(false);
        let guard = park_mutex.lock().unwrap();

        let timed_out = if timeout > 0 {
            let timeout_dur = std::time::Duration::from_nanos(timeout as u64);
            let result = cv.wait_timeout(guard, timeout_dur).unwrap();
            result.1.timed_out()
        } else if timeout < 0 {
            // Infinite wait (Svc::WaitInfinite = -1)
            let _guard = cv.wait(guard).unwrap();
            false
        } else {
            true
        };

        {
            let mut map = self.wait_map.lock().unwrap();
            if let Some(entry) = map.get_mut(&addr) {
                entry.waiter_count = entry.waiter_count.saturating_sub(1);
                if entry.waiter_count == 0 {
                    map.remove(&addr);
                }
            }
        }

        if timed_out {
            crate::hle::kernel::svc::svc_results::RESULT_TIMED_OUT
        } else {
            ResultCode::new(0)
        }
    }
}

impl Default for KAddressArbiter {
    fn default() -> Self {
        Self::new()
    }
}
