//! Port of zuyu/src/core/hle/kernel/k_process.h / k_process.cpp
//! Status: Partial (structural port, complex methods stubbed)
//! Derniere synchro: 2026-03-11
//!
//! KProcess: the kernel process object. Preserves all state fields, enums,
//! and method signatures from upstream.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicI64, AtomicU16};
use std::sync::{Arc, Mutex, RwLock, Weak};

use super::code_set::CodeSet;
use super::k_capabilities::KCapabilities;
use super::k_event::KEvent;
use super::k_handle_table::MAX_TABLE_SIZE;
use super::k_handle_table::KHandleTable;
use super::k_memory_block::{
    KMemoryAttribute, KMemoryBlockDisableMergeAttribute, KMemoryPermission, KMemoryState,
    PAGE_SIZE,
};
use super::k_memory_block_manager::KMemoryBlockManager;
use super::k_process_page_table::KProcessPageTable;
use super::k_readable_event::KReadableEvent;
use super::k_scheduler::KScheduler;
use super::k_synchronization_object;
use super::k_synchronization_object::SynchronizationObjectState;
use super::k_thread::KThread;
use super::k_thread_local_page::{KThreadLocalPage, PAGE_SIZE as THREAD_LOCAL_PAGE_SIZE};
use super::k_typed_address::KProcessAddress;
use crate::hardware_properties::NUM_CPU_CORES;
use crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
use crate::hle::result::RESULT_SUCCESS;

// ---------------------------------------------------------------------------
// SharedProcessMemory — shared guest memory backing
// ---------------------------------------------------------------------------

/// The inner state of shared process memory.
///
/// This is separated from KProcess so that JIT callbacks can hold a reference
/// to the memory without needing a reference to the entire process.
/// Upstream achieves this via `Core::Memory::Memory&` obtained from
/// `process->GetMemory()`.
pub struct ProcessMemoryData {
    /// Flat guest memory backing.
    pub data: Vec<u8>,
    /// Base address of this memory in the guest address space.
    pub base: u64,
    /// Memory block manager tracking per-segment state and permissions.
    /// Used by QueryMemory SVC. Upstream: KProcess -> KProcessPageTable -> KMemoryBlockManager.
    pub block_manager: KMemoryBlockManager,
}

impl ProcessMemoryData {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            base: 0,
            block_manager: KMemoryBlockManager::new(),
        }
    }

    /// Read a single byte at guest virtual address.
    #[inline]
    pub fn read_8(&self, vaddr: u64) -> u8 {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset < self.data.len() {
            self.data[offset]
        } else {
            0
        }
    }

    /// Read a u16 (little-endian) at guest virtual address.
    #[inline]
    pub fn read_16(&self, vaddr: u64) -> u16 {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset + 2 <= self.data.len() {
            u16::from_le_bytes([self.data[offset], self.data[offset + 1]])
        } else {
            0
        }
    }

    /// Read a u32 (little-endian) at guest virtual address.
    #[inline]
    pub fn read_32(&self, vaddr: u64) -> u32 {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset + 4 <= self.data.len() {
            u32::from_le_bytes(self.data[offset..offset + 4].try_into().unwrap())
        } else {
            0
        }
    }

    /// Read a u64 (little-endian) at guest virtual address.
    #[inline]
    pub fn read_64(&self, vaddr: u64) -> u64 {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset + 8 <= self.data.len() {
            u64::from_le_bytes(self.data[offset..offset + 8].try_into().unwrap())
        } else {
            0
        }
    }

    /// Write a single byte at guest virtual address.
    #[inline]
    pub fn write_8(&mut self, vaddr: u64, value: u8) {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset < self.data.len() {
            self.data[offset] = value;
        }
    }

    /// Write a u16 (little-endian) at guest virtual address.
    #[inline]
    pub fn write_16(&mut self, vaddr: u64, value: u16) {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset + 2 <= self.data.len() {
            self.data[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
        }
    }

    /// Write a u32 (little-endian) at guest virtual address.
    #[inline]
    pub fn write_32(&mut self, vaddr: u64, value: u32) {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset + 4 <= self.data.len() {
            self.data[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
        } else {
            log::warn!("write_32 OOB: vaddr={:#x} base={:#x} offset={:#x} data_len={:#x}", vaddr, self.base, offset, self.data.len());
        }
    }

    /// Write a u64 (little-endian) at guest virtual address.
    #[inline]
    pub fn write_64(&mut self, vaddr: u64, value: u64) {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset + 8 <= self.data.len() {
            self.data[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
        }
    }

    /// Check if a virtual address range is valid.
    #[inline]
    pub fn is_valid_range(&self, vaddr: u64, size: usize) -> bool {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        offset + size <= self.data.len()
    }

    /// Check if a virtual address is in a writable memory region.
    /// Returns true for writable regions, false for read-only/execute-only.
    /// Also returns true for unmapped/FREE regions (to avoid blocking initial setup).
    /// This enforces page table write protection that C++ upstream gets from hardware.
    #[inline]
    pub fn is_writable(&self, vaddr: u64) -> bool {
        use super::k_memory_block::{KMemoryPermission, KMemoryState};
        if let Some(block) = self.block_manager.find_block(vaddr as usize) {
            let state = block.get_state();
            if state == KMemoryState::FREE {
                return true; // Unmapped — allow (setup writes)
            }
            let perm = block.get_permission();
            // Check if USER_WRITE bit is set
            perm.contains(KMemoryPermission::USER_WRITE)
        } else {
            true // No block info — allow
        }
    }

    /// Write a block of data at guest address.
    pub fn write_block(&mut self, vaddr: u64, data: &[u8]) {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        let end = offset + data.len();
        if end > self.data.len() {
            self.data.resize(end, 0);
        }
        self.data[offset..end].copy_from_slice(data);
    }

    /// Read a block of data from guest address.
    pub fn read_block(&self, vaddr: u64, size: usize) -> &[u8] {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        &self.data[offset..offset + size]
    }

    /// Allocate memory at the given base address.
    pub fn allocate(&mut self, base: u64, size: usize) {
        self.base = base;
        self.data = vec![0u8; size];
        // Initialize the block manager covering the full 32-bit or 64-bit address space.
        // The actual code region is [base, base+size), but QueryMemory needs to handle
        // addresses outside this range too. Use a generous address space.
        let addr_space_end = if base < 0x1_0000_0000 {
            0x1_0000_0000usize // 4 GiB for 32-bit
        } else {
            0x80_0000_0000usize // 512 GiB for 64-bit
        };
        let _ = self.block_manager.initialize(0, addr_space_end);
    }

    /// Update a region in the block manager to track memory state.
    /// Used during NSO loading to register text/rodata/data segments.
    pub fn update_region(
        &mut self,
        base: u64,
        size: u64,
        state: KMemoryState,
        perm: KMemoryPermission,
    ) {
        if size == 0 {
            return;
        }
        let num_pages = (size as usize) / PAGE_SIZE;
        self.block_manager.update(
            base as usize,
            num_pages,
            state,
            perm,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );
    }
}

/// Shared handle to process memory, clonable for JIT callbacks.
///
/// Corresponds to upstream `Core::Memory::Memory&` — the shared memory
/// reference that both the process and JIT callbacks hold.
pub type SharedProcessMemory = Arc<RwLock<ProcessMemoryData>>;

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
    // Use a Vec for now while keeping ownership in KProcess.
    pub thread_local_pages: Vec<KThreadLocalPage>,
    pub next_thread_local_page_address: u64,

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
    pub thread_objects: BTreeMap<u64, Arc<Mutex<KThread>>>,
    pub event_objects: BTreeMap<u64, Arc<Mutex<KEvent>>>,
    pub readable_event_objects: BTreeMap<u64, Arc<Mutex<KReadableEvent>>>,
    pub sync_object: SynchronizationObjectState,
    pub scheduler: Option<Weak<Mutex<KScheduler>>>,
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
    /// Guest process memory — shared with JIT callbacks.
    ///
    /// In upstream, `KProcess` owns `Core::Memory::Memory` and the JIT
    /// callbacks hold a reference to it via `process->GetMemory()`.
    /// Here we use `Arc<RwLock<ProcessMemoryData>>` for shared access.
    pub process_memory: SharedProcessMemory,
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
            thread_local_pages: Vec::new(),
            next_thread_local_page_address: 0,
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
            thread_objects: BTreeMap::new(),
            event_objects: BTreeMap::new(),
            readable_event_objects: BTreeMap::new(),
            sync_object: SynchronizationObjectState::new(),
            scheduler: None,
            is_suspended: false,
            is_immortal: false,
            is_handle_table_initialized: false,
            running_threads: [None; NUM_CPU_CORES as usize],
            running_thread_idle_counts: [0u64; NUM_CPU_CORES as usize],
            running_thread_switch_counts: [0u64; NUM_CPU_CORES as usize],
            pinned_threads: [None; NUM_CPU_CORES as usize],
            watchpoints: [DebugWatchpoint::default(); NUM_WATCHPOINTS],
            process_memory: Arc::new(RwLock::new(ProcessMemoryData::new())),
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

    pub fn link_waiter(&mut self, thread_id: u64) {
        let mut waiters = std::mem::take(&mut self.sync_object.waiters);
        waiters.link(
            self,
            super::k_synchronization_object::SynchronizationWaitNodeHandle {
                thread_id,
                wait_index: 0,
            },
        );
        self.sync_object.waiters = waiters;
    }

    pub fn unlink_waiter(&mut self, thread_id: u64) {
        let mut waiters = std::mem::take(&mut self.sync_object.waiters);
        waiters.unlink(self, thread_id, 0);
        self.sync_object.waiters = waiters;
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

    pub fn attach_scheduler(&mut self, scheduler: &Arc<Mutex<KScheduler>>) {
        self.scheduler = Some(Arc::downgrade(scheduler));
    }

    pub fn initialize_handle_table(&mut self) -> u32 {
        let size = match self.capabilities.get_handle_table_size() {
            0 => MAX_TABLE_SIZE as i32,
            value => value,
        };
        let result = self.handle_table.initialize(size);
        if result == RESULT_SUCCESS.get_inner_value() {
            self.is_handle_table_initialized = true;
        }
        result
    }

    pub fn ensure_handle_table_initialized(&mut self) -> u32 {
        if self.is_handle_table_initialized {
            RESULT_SUCCESS.get_inner_value()
        } else {
            self.initialize_handle_table()
        }
    }

    pub fn initialize_thread_local_region_base(&mut self, next_page_address: u64) {
        self.next_thread_local_page_address = next_page_address;
    }

    pub fn create_thread_local_region(&mut self) -> Option<KProcessAddress> {
        for page in &mut self.thread_local_pages {
            if let Some(region) = page.reserve() {
                return Some(region);
            }
        }

        if self.next_thread_local_page_address == 0 {
            return None;
        }

        let page_address = self.next_thread_local_page_address;
        self.next_thread_local_page_address += THREAD_LOCAL_PAGE_SIZE as u64;

        {
            let mut mem = self.process_memory.write().unwrap();
            let page_end = page_address + THREAD_LOCAL_PAGE_SIZE as u64;
            let new_total = (page_end - mem.base) as usize;
            if new_total > mem.data.len() {
                mem.data.resize(new_total, 0);
            }
            let page_offset = (page_address - mem.base) as usize;
            for byte in &mut mem.data[page_offset..page_offset + THREAD_LOCAL_PAGE_SIZE] {
                *byte = 0;
            }
            mem.update_region(
                page_address,
                THREAD_LOCAL_PAGE_SIZE as u64,
                KMemoryState::THREAD_LOCAL,
                KMemoryPermission::USER_READ_WRITE,
            );
        }

        let mut page = KThreadLocalPage::new(KProcessAddress::new(page_address));
        let region = page.reserve();
        self.thread_local_pages.push(page);
        region
    }

    pub fn delete_thread_local_region(&mut self, address: KProcessAddress) -> u32 {
        for page in &mut self.thread_local_pages {
            let start = page.get_address().get();
            let end = start + THREAD_LOCAL_PAGE_SIZE as u64;
            if (start..end).contains(&address.get()) {
                page.release(address);
                return RESULT_SUCCESS.get_inner_value();
            }
        }
        1
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
    pub fn reset(&mut self) -> u32 {
        if self.state != ProcessState::Terminated || !self.is_signaled {
            return RESULT_INVALID_STATE.get_inner_value();
        }

        self.is_signaled = false;
        RESULT_SUCCESS.get_inner_value()
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

    pub fn register_thread_object(&mut self, thread: Arc<Mutex<KThread>>) {
        let (thread_id, object_id) = {
            let thread = thread.lock().unwrap();
            (thread.thread_id, thread.object_id)
        };
        self.register_thread(thread_id);
        self.thread_objects.insert(object_id, thread);
    }

    pub fn unregister_thread_object_by_object_id(&mut self, object_id: u64) {
        if let Some(thread) = self.thread_objects.remove(&object_id) {
            let thread_id = thread.lock().unwrap().thread_id;
            self.unregister_thread(thread_id);
        }
    }

    pub fn get_thread_by_object_id(&self, object_id: u64) -> Option<Arc<Mutex<KThread>>> {
        self.thread_objects.get(&object_id).cloned()
    }

    pub fn get_thread_by_thread_id(&self, thread_id: u64) -> Option<Arc<Mutex<KThread>>> {
        self.thread_objects
            .values()
            .find(|thread| thread.lock().unwrap().thread_id == thread_id)
            .cloned()
    }

    pub fn register_event_object(&mut self, object_id: u64, event: Arc<Mutex<KEvent>>) {
        self.event_objects.insert(object_id, event);
    }

    pub fn unregister_event_object_by_object_id(&mut self, object_id: u64) {
        self.event_objects.remove(&object_id);
    }

    pub fn get_event_by_object_id(&self, object_id: u64) -> Option<Arc<Mutex<KEvent>>> {
        self.event_objects.get(&object_id).cloned()
    }

    pub fn register_readable_event_object(
        &mut self,
        object_id: u64,
        readable_event: Arc<Mutex<KReadableEvent>>,
    ) {
        self.readable_event_objects.insert(object_id, readable_event);
    }

    pub fn unregister_readable_event_object_by_object_id(&mut self, object_id: u64) {
        self.readable_event_objects.remove(&object_id);
    }

    pub fn get_readable_event_by_object_id(
        &self,
        object_id: u64,
    ) -> Option<Arc<Mutex<KReadableEvent>>> {
        self.readable_event_objects.get(&object_id).cloned()
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

    /// Write data to process memory at the given guest address.
    pub fn write_memory(&mut self, guest_addr: u64, data: &[u8]) {
        let mut mem = self.process_memory.write().unwrap();
        mem.write_block(guest_addr, data);
    }

    /// Load a module into process memory and apply per-segment permissions.
    ///
    /// Matches upstream `KProcess::LoadModule(CodeSet, KProcessAddress)`.
    pub fn load_module(&mut self, code_set: CodeSet, base_addr: u64) {
        {
            let mut mem = self.process_memory.write().unwrap();
            mem.write_block(base_addr, &code_set.memory);

            let reprotect_segment = |mem: &mut ProcessMemoryData,
                                     segment: &super::code_set::Segment,
                                     permission: KMemoryPermission| {
                if segment.size == 0 {
                    return;
                }

                let state = if permission == KMemoryPermission::USER_READ_EXECUTE
                    || permission == KMemoryPermission::USER_READ
                {
                    KMemoryState::CODE
                } else {
                    KMemoryState::CODE_DATA
                };

                mem.update_region(base_addr + segment.addr, segment.size as u64, state, permission);
            };

            reprotect_segment(
                &mut mem,
                code_set.code_segment(),
                KMemoryPermission::USER_READ_EXECUTE,
            );
            reprotect_segment(&mut mem, code_set.rodata_segment(), KMemoryPermission::USER_READ);
            reprotect_segment(
                &mut mem,
                code_set.data_segment(),
                KMemoryPermission::USER_READ_WRITE,
            );
        }
    }

    /// Read data from process memory at the given guest address (copies into a Vec).
    pub fn read_memory_vec(&self, guest_addr: u64, size: usize) -> Vec<u8> {
        let mem = self.process_memory.read().unwrap();
        mem.read_block(guest_addr, size).to_vec()
    }

    /// Allocate process memory for code loading.
    /// Sets the memory base and pre-allocates the given size.
    pub fn allocate_code_memory(&mut self, base: u64, size: usize) {
        let mut mem = self.process_memory.write().unwrap();
        mem.allocate(base, size);
    }

    /// Get a shared handle to the process memory for JIT callbacks.
    ///
    /// Corresponds to upstream `KProcess::GetMemory()` — returns a reference
    /// that the JIT callbacks can hold independently.
    pub fn get_shared_memory(&self) -> SharedProcessMemory {
        self.process_memory.clone()
    }

    /// Change the process state and signal.
    fn change_state(&mut self, new_state: ProcessState) {
        if self.state != new_state {
            self.state = new_state;
            self.is_signaled = true;
            k_synchronization_object::notify_available(
                self,
                self.process_id,
                crate::hle::result::RESULT_SUCCESS.get_inner_value(),
            );
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
