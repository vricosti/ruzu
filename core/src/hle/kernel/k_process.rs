//! Port of zuyu/src/core/hle/kernel/k_process.h / k_process.cpp
//! Status: Partial (structural port, complex methods stubbed)
//! Derniere synchro: 2026-03-11
//!
//! KProcess: the kernel process object. Preserves all state fields, enums,
//! and method signatures from upstream.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicI64, AtomicU16};

use super::k_capabilities::KCapabilities;
use super::k_handle_table::KHandleTable;
use super::k_process_page_table::KProcessPageTable;
use super::k_typed_address::KProcessAddress;
use crate::hardware_properties::NUM_CPU_CORES;

/// Number of watchpoints (cast from hardware_properties::NUM_WATCHPOINTS).
const NUM_WATCHPOINTS: usize = crate::hardware_properties::NUM_WATCHPOINTS as usize;

// ---------------------------------------------------------------------------
// DebugWatchpointType — matches upstream (k_process.h)
// ---------------------------------------------------------------------------

use bitflags::bitflags;

bitflags! {
    /// Debug watchpoint type flags.
    /// Matches upstream `DebugWatchpointType` (k_process.h).
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct DebugWatchpointType: u8 {
        const NONE = 0;
        const READ = 1 << 0;
        const WRITE = 1 << 1;
        const READ_OR_WRITE = Self::READ.bits() | Self::WRITE.bits();
    }
}

/// A debug watchpoint entry.
/// Matches upstream `DebugWatchpoint` (k_process.h).
#[derive(Debug, Clone, Copy, Default)]
pub struct DebugWatchpoint {
    pub start_address: KProcessAddress,
    pub end_address: KProcessAddress,
    pub type_: u8, // DebugWatchpointType bits
}

// ---------------------------------------------------------------------------
// Process State — matches upstream KProcess::State
// ---------------------------------------------------------------------------

/// Process state.
/// Matches upstream `KProcess::State` / `Svc::ProcessState` (k_process.h).
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessState {
    Created = 0,
    CreatedAttached = 1,
    Running = 2,
    Crashed = 3,
    RunningAttached = 4,
    Terminating = 5,
    Terminated = 6,
    DebugBreak = 7,
}

impl Default for ProcessState {
    fn default() -> Self {
        Self::Created
    }
}

// ---------------------------------------------------------------------------
// KProcess
// ---------------------------------------------------------------------------

/// The kernel process object.
/// Matches upstream `KProcess` class (k_process.h).
pub struct KProcess {
    // -- Page table --
    pub page_table: KProcessPageTable,
    pub used_kernel_memory_size: std::sync::atomic::AtomicUsize,

    // -- Thread-local page trees (stubbed as Vecs) --
    // In upstream these are intrusive red-black trees of KThreadLocalPage.
    // Stubbed here; full implementation depends on KThreadLocalPage integration.

    pub ideal_core_id: i32,
    // m_resource_limit — stubbed (raw pointer in upstream)
    // m_system_resource — stubbed
    pub memory_release_hint: usize,
    pub state: ProcessState,
    // m_state_lock — KLightLock
    // m_list_lock — KLightLock
    // m_cond_var — KConditionVariable
    // m_address_arbiter — KAddressArbiter
    pub entropy: [u64; 4],
    pub is_signaled: bool,
    pub is_initialized: bool,
    pub is_application: bool,
    pub is_default_application_system_resource: bool,
    pub is_hbl: bool,
    pub name: [u8; 13],
    pub num_running_threads: AtomicU16,
    pub flags: u32, // Svc::CreateProcessFlag
    // m_memory_pool — KMemoryManager::Pool
    pub schedule_count: i64,
    pub capabilities: KCapabilities,
    pub program_id: u64,
    pub process_id: u64,
    pub code_address: KProcessAddress,
    pub code_size: usize,
    pub main_thread_stack_size: usize,
    pub max_process_memory: usize,
    pub version: u32,
    pub handle_table: KHandleTable,
    pub plr_address: KProcessAddress,
    // m_exception_thread — Option<thread id>
    pub exception_thread_id: Option<u64>,
    // Thread list — stubbed as Vec of thread ids
    pub thread_list: Vec<u64>,
    // Shared memory list — stubbed
    pub is_suspended: bool,
    pub is_immortal: bool,
    pub is_handle_table_initialized: bool,
    // Per-core running threads
    pub running_threads: [Option<u64>; NUM_CPU_CORES as usize],
    pub running_thread_idle_counts: [u64; NUM_CPU_CORES as usize],
    pub running_thread_switch_counts: [u64; NUM_CPU_CORES as usize],
    pub pinned_threads: [Option<u64>; NUM_CPU_CORES as usize],
    pub watchpoints: [DebugWatchpoint; NUM_WATCHPOINTS],
    pub debug_page_refcounts: BTreeMap<u64, u64>,
    pub cpu_time: AtomicI64,
    pub num_process_switches: AtomicI64,
    pub num_thread_switches: AtomicI64,
    pub num_fpu_switches: AtomicI64,
    pub num_supervisor_calls: AtomicI64,
    pub num_ipc_messages: AtomicI64,
    pub num_ipc_replies: AtomicI64,
    pub num_ipc_receives: AtomicI64,
}

/// Initial process ID range.
impl KProcess {
    pub const INITIAL_PROCESS_ID_MIN: u64 = 1;
    pub const INITIAL_PROCESS_ID_MAX: u64 = 0x50;
    pub const PROCESS_ID_MIN: u64 = Self::INITIAL_PROCESS_ID_MAX + 1;
    pub const PROCESS_ID_MAX: u64 = u64::MAX;

    /// ASLR alignment (2 MiB).
    pub const ASLR_ALIGNMENT: usize = 2 * 1024 * 1024;

    /// Create a new process with default state.
    pub fn new() -> Self {
        Self {
            page_table: KProcessPageTable::new(),
            used_kernel_memory_size: std::sync::atomic::AtomicUsize::new(0),
            ideal_core_id: 0,
            memory_release_hint: 0,
            state: ProcessState::default(),
            entropy: [0u64; 4],
            is_signaled: false,
            is_initialized: false,
            is_application: false,
            is_default_application_system_resource: false,
            is_hbl: false,
            name: [0u8; 13],
            num_running_threads: AtomicU16::new(0),
            flags: 0,
            schedule_count: 0,
            capabilities: KCapabilities::new(),
            program_id: 0,
            process_id: 0,
            code_address: KProcessAddress::default(),
            code_size: 0,
            main_thread_stack_size: 0,
            max_process_memory: 0,
            version: 0,
            handle_table: KHandleTable::new(),
            plr_address: KProcessAddress::default(),
            exception_thread_id: None,
            thread_list: Vec::new(),
            is_suspended: false,
            is_immortal: false,
            is_handle_table_initialized: false,
            running_threads: [None; NUM_CPU_CORES as usize],
            running_thread_idle_counts: [0u64; NUM_CPU_CORES as usize],
            running_thread_switch_counts: [0u64; NUM_CPU_CORES as usize],
            pinned_threads: [None; NUM_CPU_CORES as usize],
            watchpoints: [DebugWatchpoint::default(); NUM_WATCHPOINTS],
            debug_page_refcounts: BTreeMap::new(),
            cpu_time: AtomicI64::new(0),
            num_process_switches: AtomicI64::new(0),
            num_thread_switches: AtomicI64::new(0),
            num_fpu_switches: AtomicI64::new(0),
            num_supervisor_calls: AtomicI64::new(0),
            num_ipc_messages: AtomicI64::new(0),
            num_ipc_replies: AtomicI64::new(0),
            num_ipc_receives: AtomicI64::new(0),
        }
    }

    // -- Getters matching upstream --

    pub fn get_name(&self) -> &str {
        let end = self.name.iter().position(|&b| b == 0).unwrap_or(self.name.len());
        std::str::from_utf8(&self.name[..end]).unwrap_or("")
    }

    pub fn get_program_id(&self) -> u64 {
        self.program_id
    }

    pub fn get_process_id(&self) -> u64 {
        self.process_id
    }

    pub fn get_state(&self) -> ProcessState {
        self.state
    }

    pub fn get_core_mask(&self) -> u64 {
        self.capabilities.get_core_mask()
    }

    pub fn get_physical_core_mask(&self) -> u64 {
        self.capabilities.get_physical_core_mask()
    }

    pub fn get_priority_mask(&self) -> u64 {
        self.capabilities.get_priority_mask()
    }

    pub fn get_ideal_core_id(&self) -> i32 {
        self.ideal_core_id
    }

    pub fn set_ideal_core_id(&mut self, core_id: i32) {
        self.ideal_core_id = core_id;
    }

    pub fn check_thread_priority(&self, prio: i32) -> bool {
        ((1u64 << prio) & self.get_priority_mask()) != 0
    }

    pub fn get_create_process_flags(&self) -> u32 {
        self.flags
    }

    pub fn is_64bit(&self) -> bool {
        // Svc::CreateProcessFlag::Is64Bit = bit 0
        (self.flags & 1) != 0
    }

    pub fn get_entry_point(&self) -> KProcessAddress {
        self.code_address
    }

    pub fn get_main_stack_size(&self) -> usize {
        self.main_thread_stack_size
    }

    pub fn get_random_entropy(&self, i: usize) -> u64 {
        self.entropy[i]
    }

    pub fn is_application(&self) -> bool {
        self.is_application
    }

    pub fn is_default_application_system_resource(&self) -> bool {
        self.is_default_application_system_resource
    }

    pub fn is_suspended(&self) -> bool {
        self.is_suspended
    }

    pub fn set_suspended(&mut self, suspended: bool) {
        self.is_suspended = suspended;
    }

    pub fn is_terminated(&self) -> bool {
        self.state == ProcessState::Terminated
    }

    pub fn is_permitted_svc(&self, svc_id: u32) -> bool {
        self.capabilities.is_permitted_svc(svc_id)
    }

    pub fn is_permitted_interrupt(&self, interrupt_id: u32) -> bool {
        self.capabilities.is_permitted_interrupt(interrupt_id)
    }

    pub fn is_permitted_debug(&self) -> bool {
        self.capabilities.is_permitted_debug()
    }

    pub fn can_force_debug(&self) -> bool {
        self.capabilities.can_force_debug()
    }

    pub fn is_hbl(&self) -> bool {
        self.is_hbl
    }

    pub fn is_initialized(&self) -> bool {
        self.is_initialized
    }

    pub fn is_signaled(&self) -> bool {
        self.is_signaled
    }

    pub fn get_process_local_region_address(&self) -> KProcessAddress {
        self.plr_address
    }

    pub fn add_cpu_time(&self, diff: i64) {
        self.cpu_time.fetch_add(diff, std::sync::atomic::Ordering::Relaxed);
    }

    pub fn get_cpu_time(&self) -> i64 {
        self.cpu_time.load(std::sync::atomic::Ordering::Relaxed)
    }

    pub fn get_scheduled_count(&self) -> i64 {
        self.schedule_count
    }

    pub fn increment_scheduled_count(&mut self) {
        self.schedule_count += 1;
    }

    pub fn set_running_thread(&mut self, core: i32, thread_id: u64, idle_count: u64, switch_count: u64) {
        let c = core as usize;
        self.running_threads[c] = Some(thread_id);
        self.running_thread_idle_counts[c] = idle_count;
        self.running_thread_switch_counts[c] = switch_count;
    }

    pub fn clear_running_thread(&mut self, thread_id: u64) {
        for slot in self.running_threads.iter_mut() {
            if *slot == Some(thread_id) {
                *slot = None;
            }
        }
    }

    pub fn get_pinned_thread(&self, core_id: i32) -> Option<u64> {
        self.pinned_threads[core_id as usize]
    }

    // -- Complex methods stubbed --

    /// Initialize the process.
    /// TODO: Port from k_process.cpp.
    pub fn initialize(&mut self) -> u32 {
        // TODO: Full implementation
        0
    }

    /// Exit the process.
    /// TODO: Port from k_process.cpp.
    pub fn exit(&mut self) {
        // TODO: Full implementation
    }

    /// Terminate the process.
    /// TODO: Port from k_process.cpp.
    pub fn terminate(&mut self) -> u32 {
        // TODO: Full implementation
        0
    }

    /// Run the process.
    /// TODO: Port from k_process.cpp.
    pub fn run(&mut self, _priority: i32, _stack_size: usize) -> u32 {
        // TODO: Full implementation
        0
    }

    /// Reset the process.
    /// TODO: Port from k_process.cpp.
    pub fn reset(&mut self) -> u32 {
        // TODO: Full implementation
        0
    }

    /// Finalize the process.
    /// TODO: Port from k_process.cpp.
    pub fn finalize(&mut self) {
        // TODO: Full implementation
    }

    /// Register a thread with this process.
    pub fn register_thread(&mut self, thread_id: u64) {
        self.thread_list.push(thread_id);
    }

    /// Unregister a thread from this process.
    pub fn unregister_thread(&mut self, thread_id: u64) {
        self.thread_list.retain(|&id| id != thread_id);
    }

    /// Insert a debug watchpoint. Returns false if no free slot.
    pub fn insert_watchpoint(
        &mut self,
        addr: KProcessAddress,
        size: u64,
        wp_type: DebugWatchpointType,
    ) -> bool {
        for wp in self.watchpoints.iter_mut() {
            if wp.type_ == DebugWatchpointType::NONE.bits() {
                wp.start_address = addr;
                wp.end_address = KProcessAddress::new(addr.get() + size);
                wp.type_ = wp_type.bits();
                return true;
            }
        }
        false
    }

    /// Remove a debug watchpoint.
    pub fn remove_watchpoint(
        &mut self,
        addr: KProcessAddress,
        size: u64,
        wp_type: DebugWatchpointType,
    ) -> bool {
        let end = KProcessAddress::new(addr.get() + size);
        for wp in self.watchpoints.iter_mut() {
            if wp.start_address == addr && wp.end_address == end && wp.type_ == wp_type.bits() {
                *wp = DebugWatchpoint::default();
                return true;
            }
        }
        false
    }

    /// Change the process state and signal.
    fn change_state(&mut self, new_state: ProcessState) {
        if self.state != new_state {
            self.state = new_state;
            self.is_signaled = true;
            // TODO: NotifyAvailable()
        }
    }

    pub fn set_debug_break(&mut self) {
        if self.state == ProcessState::RunningAttached {
            self.change_state(ProcessState::DebugBreak);
        }
    }

    pub fn set_attached(&mut self) {
        if self.state == ProcessState::DebugBreak {
            self.change_state(ProcessState::RunningAttached);
        }
    }
}

impl Default for KProcess {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_state_values() {
        assert_eq!(ProcessState::Created as u32, 0);
        assert_eq!(ProcessState::Terminated as u32, 6);
        assert_eq!(ProcessState::DebugBreak as u32, 7);
    }

    #[test]
    fn test_process_id_constants() {
        assert_eq!(KProcess::INITIAL_PROCESS_ID_MIN, 1);
        assert_eq!(KProcess::INITIAL_PROCESS_ID_MAX, 0x50);
        assert_eq!(KProcess::PROCESS_ID_MIN, 0x51);
    }
}
