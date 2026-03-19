//! Port of zuyu/src/core/hle/kernel/k_process.h / k_process.cpp
//! Status: Partial (lifecycle methods ported, resource limits / system resource not yet wired)
//! Derniere synchro: 2026-03-17
//!
//! KProcess: the kernel process object. Preserves all state fields, enums,
//! and method signatures from upstream.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicI64, AtomicU16};
use std::sync::{Arc, Mutex, RwLock, Weak};

use super::code_set::CodeSet;
use super::k_capabilities::KCapabilities;
use super::k_client_session::KClientSession;
use super::k_resource_limit::{KResourceLimit, LimitableResource};
use super::k_condition_variable::KConditionVariable;
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
use super::k_priority_queue::{KPriorityQueueMember, ThreadAccessor};
use super::k_scheduler::KScheduler;
use super::k_session::KSession;
use super::k_memory_manager;
use super::k_synchronization_object;
use super::k_synchronization_object::SynchronizationObjectState;
use super::k_system_resource::KSecureSystemResource;
use super::k_thread::KThread;
use super::k_thread_local_page::{KThreadLocalPage, PAGE_SIZE as THREAD_LOCAL_PAGE_SIZE};
use super::k_worker_task_manager::{KWorkerTaskManager, WorkerType};
use super::k_typed_address::KProcessAddress;
use super::svc_common::Handle;
use super::svc_types::{CreateProcessFlag, ADDRESS_SPACE_MASK};
use crate::hardware_properties::NUM_CPU_CORES;
use crate::file_sys::program_metadata::{PoolPartition, ProgramAddressSpaceType, ProgramMetadata};
use crate::hle::kernel::svc::svc_results;
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
    /// Sparse guest pages outside the contiguous image/TLS/stack bootstrap area.
    /// This keeps large heap regions from forcing a single huge host allocation.
    pub sparse_pages: BTreeMap<u64, Vec<u8>>,
}

impl ProcessMemoryData {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            base: 0,
            block_manager: KMemoryBlockManager::new(),
            sparse_pages: BTreeMap::new(),
        }
    }

    /// Read a single byte at guest virtual address.
    #[inline]
    pub fn read_8(&self, vaddr: u64) -> u8 {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset < self.data.len() {
            self.data[offset]
        } else {
            self.read_sparse_8(vaddr)
        }
    }

    /// Read a u16 (little-endian) at guest virtual address.
    #[inline]
    pub fn read_16(&self, vaddr: u64) -> u16 {
        u16::from_le_bytes([self.read_8(vaddr), self.read_8(vaddr + 1)])
    }

    /// Read a u32 (little-endian) at guest virtual address.
    #[inline]
    pub fn read_32(&self, vaddr: u64) -> u32 {
        u32::from_le_bytes([
            self.read_8(vaddr),
            self.read_8(vaddr + 1),
            self.read_8(vaddr + 2),
            self.read_8(vaddr + 3),
        ])
    }

    /// Read a u64 (little-endian) at guest virtual address.
    #[inline]
    pub fn read_64(&self, vaddr: u64) -> u64 {
        u64::from_le_bytes([
            self.read_8(vaddr),
            self.read_8(vaddr + 1),
            self.read_8(vaddr + 2),
            self.read_8(vaddr + 3),
            self.read_8(vaddr + 4),
            self.read_8(vaddr + 5),
            self.read_8(vaddr + 6),
            self.read_8(vaddr + 7),
        ])
    }

    /// Write a single byte at guest virtual address.
    #[inline]
    pub fn write_8(&mut self, vaddr: u64, value: u8) {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset < self.data.len() {
            self.data[offset] = value;
        } else {
            self.write_sparse_8(vaddr, value);
        }
    }

    /// Write a u16 (little-endian) at guest virtual address.
    #[inline]
    pub fn write_16(&mut self, vaddr: u64, value: u16) {
        let bytes = value.to_le_bytes();
        self.write_8(vaddr, bytes[0]);
        self.write_8(vaddr + 1, bytes[1]);
    }

    /// Write a u32 (little-endian) at guest virtual address.
    #[inline]
    pub fn write_32(&mut self, vaddr: u64, value: u32) {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset + 4 <= self.data.len() {
            self.data[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
        } else {
            let bytes = value.to_le_bytes();
            for (index, byte) in bytes.into_iter().enumerate() {
                self.write_8(vaddr + index as u64, byte);
            }
        }
    }

    /// Write a u64 (little-endian) at guest virtual address.
    #[inline]
    pub fn write_64(&mut self, vaddr: u64, value: u64) {
        let bytes = value.to_le_bytes();
        for (index, byte) in bytes.into_iter().enumerate() {
            self.write_8(vaddr + index as u64, byte);
        }
    }

    /// Check if a virtual address range is valid.
    #[inline]
    pub fn is_valid_range(&self, vaddr: u64, size: usize) -> bool {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset
            .checked_add(size)
            .is_some_and(|end| end <= self.data.len())
        {
            return true;
        }

        if size == 0 {
            return false;
        }

        let start = vaddr as usize;
        let end = match start.checked_add(size) {
            Some(end) => end,
            None => return false,
        };

        let mut page = start & !(PAGE_SIZE - 1);
        while page < end {
            let Some(block) = self.block_manager.find_block(page) else {
                return false;
            };
            if block.get_state() == KMemoryState::FREE {
                return false;
            }
            page = page.saturating_add(PAGE_SIZE);
        }
        true
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
        if offset <= self.data.len() && end > self.data.len() {
            self.data.resize(end, 0);
        }
        if end <= self.data.len() {
            self.data[offset..end].copy_from_slice(data);
        } else {
            for (index, byte) in data.iter().copied().enumerate() {
                self.write_8(vaddr + index as u64, byte);
            }
        }
    }

    /// Read a block of data from guest address.
    pub fn read_block(&self, vaddr: u64, size: usize) -> &[u8] {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        &self.data[offset..offset + size]
    }

    pub fn read_bytes(&self, vaddr: u64, size: usize) -> Vec<u8> {
        let offset = vaddr.wrapping_sub(self.base) as usize;
        if offset
            .checked_add(size)
            .is_some_and(|end| end <= self.data.len())
        {
            return self.data[offset..offset + size].to_vec();
        }

        let mut out = vec![0u8; size];
        for (index, byte) in out.iter_mut().enumerate() {
            *byte = self.read_8(vaddr + index as u64);
        }
        out
    }

    /// Allocate memory at the given base address.
    pub fn allocate(&mut self, base: u64, size: usize) {
        self.base = base;
        self.data = vec![0u8; size];
        self.sparse_pages.clear();
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

    pub fn clear_sparse_range(&mut self, vaddr: u64, size: usize) {
        if size == 0 {
            return;
        }

        let start_page = vaddr & !((PAGE_SIZE as u64) - 1);
        let end_addr = vaddr.saturating_add(size as u64);
        let end_page = (end_addr.saturating_add(PAGE_SIZE as u64 - 1)) & !((PAGE_SIZE as u64) - 1);
        let mut page = start_page;
        while page < end_page {
            self.sparse_pages.remove(&page);
            page = page.saturating_add(PAGE_SIZE as u64);
        }
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

    fn read_sparse_8(&self, vaddr: u64) -> u8 {
        let page_base = vaddr & !((PAGE_SIZE as u64) - 1);
        let page_offset = (vaddr - page_base) as usize;
        self.sparse_pages
            .get(&page_base)
            .and_then(|page| page.get(page_offset).copied())
            .unwrap_or(0)
    }

    fn write_sparse_8(&mut self, vaddr: u64, value: u8) {
        let page_base = vaddr & !((PAGE_SIZE as u64) - 1);
        let page_offset = (vaddr - page_base) as usize;
        let page = self
            .sparse_pages
            .entry(page_base)
            .or_insert_with(|| vec![0u8; PAGE_SIZE]);
        page[page_offset] = value;
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
    /// Resource limit for this process.
    /// Matches upstream `KResourceLimit* m_resource_limit`.
    pub resource_limit: Option<Arc<Mutex<KResourceLimit>>>,
    /// System resource (slab managers for page table/memory blocks).
    /// Upstream: `KSystemResource* m_system_resource`.
    pub system_resource: Option<Arc<Mutex<KSecureSystemResource>>>,
    pub memory_release_hint: usize,
    pub state: ProcessState,
    // m_state_lock — KLightLock
    // m_list_lock — KLightLock
    pub cond_var: KConditionVariable,
    /// Reference to the global scheduler context that owns the priority queue.
    /// Upstream: accessed via kernel.GlobalSchedulerContext().
    pub global_scheduler_context: Option<Arc<Mutex<super::global_scheduler_context::GlobalSchedulerContext>>>,
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
    /// Memory pool for this process.
    /// Upstream: `KMemoryManager::Pool m_memory_pool`.
    pub memory_pool: k_memory_manager::Pool,
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
    pub session_objects: BTreeMap<u64, Arc<Mutex<KSession>>>,
    pub client_session_objects: BTreeMap<u64, Arc<Mutex<KClientSession>>>,
    pub event_objects: BTreeMap<u64, Arc<Mutex<KEvent>>>,
    pub readable_event_objects: BTreeMap<u64, Arc<Mutex<KReadableEvent>>>,
    pub sync_object: SynchronizationObjectState,
    pub self_reference: Option<Weak<Mutex<KProcess>>>,
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

    /// Rust-side orchestration helper for the upstream `KProcess::Exit()`
    /// sequence when the process is owned behind `Arc<Mutex<_>>`.
    ///
    /// This preserves lifecycle ownership in `k_process.rs` while allowing
    /// callers to perform the final current-thread exit after dropping the
    /// process mutex.
    pub fn exit_with_current_thread(process: &Arc<Mutex<KProcess>>) {
        let current_thread = {
            let process_guard = process.lock().unwrap();
            let current_thread_id = process_guard
                .scheduler
                .as_ref()
                .and_then(Weak::upgrade)
                .and_then(|scheduler| scheduler.lock().unwrap().get_scheduler_current_thread_id());
            current_thread_id.and_then(|thread_id| process_guard.get_thread_by_thread_id(thread_id))
        };

        process.lock().unwrap().exit();

        if let Some(thread) = current_thread {
            thread.lock().unwrap().exit();
        }
    }

    /// Create a new process with default state.
    pub fn new() -> Self {
        Self {
            page_table: KProcessPageTable::new(),
            used_kernel_memory_size: std::sync::atomic::AtomicUsize::new(0),
            thread_local_pages: Vec::new(),
            next_thread_local_page_address: 0,
            ideal_core_id: 0,
            resource_limit: None,
            system_resource: None,
            memory_release_hint: 0,
            state: ProcessState::default(),
            cond_var: KConditionVariable::new(),
            global_scheduler_context: None,
            entropy: {
                // Upstream: KProcess initializes entropy with random values
                // from KSystemControl::GenerateRandomRange. We use std random.
                use std::collections::hash_map::DefaultHasher;
                use std::hash::{Hash, Hasher};
                let mut e = [0u64; 4];
                for (i, val) in e.iter_mut().enumerate() {
                    let mut hasher = DefaultHasher::new();
                    (i as u64).hash(&mut hasher);
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap_or_default()
                        .as_nanos()
                        .hash(&mut hasher);
                    *val = hasher.finish();
                }
                e
            },
            is_signaled: false,
            is_initialized: false,
            is_application: false,
            is_default_application_system_resource: false,
            is_hbl: false,
            name: [0u8; 13],
            num_running_threads: AtomicU16::new(0),
            flags: 0,
            memory_pool: k_memory_manager::Pool::Application,
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
            session_objects: BTreeMap::new(),
            client_session_objects: BTreeMap::new(),
            event_objects: BTreeMap::new(),
            readable_event_objects: BTreeMap::new(),
            sync_object: SynchronizationObjectState::new(),
            self_reference: None,
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

    /// Upstream: `KMemoryManager::Pool GetMemoryPool() const`.
    pub fn get_memory_pool(&self) -> k_memory_manager::Pool {
        self.memory_pool
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

    pub fn bind_self_reference(&mut self, process: &Arc<Mutex<KProcess>>) {
        self.self_reference = Some(Arc::downgrade(process));
    }

    pub fn wait_condition_variable(
        &mut self,
        current_thread: &Arc<Mutex<KThread>>,
        address: u64,
        cv_key: u64,
        tag: u32,
        timeout: i64,
    ) -> u32 {
        // Mirror the signal/reprioritize ownership pattern: take the condition
        // variable owner out temporarily so it can operate on the process state
        // without a second mutable borrow of `self`.
        let mut cond_var = std::mem::take(&mut self.cond_var);
        let result = cond_var
            .wait_locked(self, current_thread, address, cv_key, tag, timeout)
            .get_inner_value();
        self.cond_var = cond_var;
        result
    }

    pub fn signal_condition_variable(&mut self, cv_key: u64, count: i32) {
        // Take cond_var out temporarily to avoid borrowing self while passing
        // &mut self to signal(). This is safe because no other code accesses
        // cond_var while we hold &mut self.
        let mut cond_var = std::mem::take(&mut self.cond_var);
        let _ = cond_var.signal(self, cv_key, count);
        self.cond_var = cond_var;
    }

    pub fn before_update_condition_variable_priority(&mut self, thread_id: u64) {
        let mut cond_var = std::mem::take(&mut self.cond_var);
        cond_var.before_update_priority(thread_id);
        self.cond_var = cond_var;
    }

    pub fn after_update_condition_variable_priority(
        &mut self,
        thread_key: super::k_thread::ConditionVariableThreadKey,
    ) {
        let mut cond_var = std::mem::take(&mut self.cond_var);
        cond_var.after_update_priority(thread_key);
        self.cond_var = cond_var;
    }

    pub fn remove_condition_variable_waiter(&mut self, thread_id: u64) {
        let mut cond_var = std::mem::take(&mut self.cond_var);
        cond_var.remove_waiter(thread_id);
        self.cond_var = cond_var;
    }

    // -- Priority queue operations --
    // Delegate to GlobalSchedulerContext which owns the PQ.
    // Matches upstream: GetPriorityQueue(kernel).PushBack/Remove/etc.

    pub fn push_back_to_priority_queue(&self, thread_id: u64) {
        if let Some(ref gsc) = self.global_scheduler_context {
            gsc.lock().unwrap().push_back_to_priority_queue(thread_id);
        }
    }

    pub fn remove_from_priority_queue(&self, thread_id: u64) {
        if let Some(ref gsc) = self.global_scheduler_context {
            gsc.lock().unwrap().remove_from_priority_queue(thread_id);
        }
    }

    pub fn change_priority_in_queue(&self, thread_id: u64, old_priority: i32, is_running: bool) {
        if let Some(ref gsc) = self.global_scheduler_context {
            gsc.lock().unwrap().change_priority_in_queue(thread_id, old_priority, is_running);
        }
    }

    pub fn get_scheduled_front(&self, core: i32) -> Option<u64> {
        self.global_scheduler_context
            .as_ref()?
            .lock()
            .unwrap()
            .get_scheduled_front(core)
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

    /// Configure where process-owned thread-local pages will begin.
    ///
    /// The first main thread created after this call will allocate its TLR from
    /// the returned page base via `create_thread_local_region()`, matching the
    /// upstream ownership where `KThread::InitializeUserThread()` asks the
    /// process to create the thread-local region.
    pub fn initialize_thread_local_region_allocation(&mut self, modules_end: u64) -> u64 {
        let gap = 0x4000u64;
        let base = modules_end + gap;
        let tls_page_base = (base + 0xFFF) & !0xFFF;
        self.initialize_thread_local_region_base(tls_page_base);
        tls_page_base
    }

    /// Bootstrap the main-thread stack region in process-owned guest memory.
    ///
    /// This is the current Rust-side owner for the stack portion of upstream
    /// `KProcess::Run()`. Once `KProcessPageTable::MapPages()` is ported, this
    /// helper should delegate there instead of updating the shared memory
    /// backing directly.
    pub fn initialize_main_thread_stack_region(
        &mut self,
        tls_region_end: u64,
        stack_size: usize,
    ) -> (u64, u64) {
        let aligned_stack_size = ((stack_size as u64) + (PAGE_SIZE as u64 - 1)) & !(PAGE_SIZE as u64 - 1);
        let stack_base = tls_region_end + 0x4000;
        let stack_top = stack_base + aligned_stack_size;

        {
            let mut mem = self.process_memory.write().unwrap();
            let new_total = (stack_top - mem.base) as usize;
            if new_total > mem.data.len() {
                mem.data.resize(new_total, 0);
            }

            let stack_offset = (stack_base - mem.base) as usize;
            let stack_len = aligned_stack_size as usize;
            for byte in &mut mem.data[stack_offset..stack_offset + stack_len] {
                *byte = 0;
            }

            mem.update_region(
                stack_base,
                aligned_stack_size,
                KMemoryState::STACK,
                KMemoryPermission::USER_READ_WRITE,
            );
        }

        self.main_thread_stack_size = aligned_stack_size as usize;
        self.page_table
            .set_stack_region(KProcessAddress::new(stack_base), aligned_stack_size as usize);
        let address_space_end = self
            .page_table
            .get_address_space_start()
            .get()
            .saturating_add(self.page_table.get_address_space_size() as u64);
        if address_space_end > stack_top {
            self.page_table.set_heap_region(
                KProcessAddress::new(stack_top),
                (address_space_end - stack_top) as usize,
            );
            self.max_process_memory = self.page_table.get_heap_region_size();
        }
        (stack_base, stack_top)
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

    /// Increment running thread count.
    /// Matches upstream `KProcess::IncrementRunningThreadCount()`.
    pub fn increment_running_thread_count(&self) {
        self.num_running_threads.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }

    /// Decrement running thread count.
    /// Matches upstream `KProcess::DecrementRunningThreadCount()` (k_process.cpp:756-762).
    ///
    /// Returns true if the count reached zero and the caller should terminate
    /// the process. Upstream calls `this->Terminate()` inline, but we cannot
    /// do that here because the caller typically holds thread locks that would
    /// deadlock with terminate()'s thread iteration. The caller must drop its
    /// locks and then call `process.terminate()`.
    pub fn decrement_running_thread_count(&mut self) -> bool {
        let prev = self.num_running_threads.fetch_sub(1, std::sync::atomic::Ordering::Relaxed);
        prev == 1
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

    // -- Lifecycle methods --

    /// Pin a thread on a given core.
    /// Matches upstream private `KProcess::PinThread(s32, KThread*)`.
    fn pin_thread(&mut self, core_id: i32, thread_id: u64) {
        assert!((0..NUM_CPU_CORES as i32).contains(&core_id));
        assert!(self.pinned_threads[core_id as usize].is_none());
        self.pinned_threads[core_id as usize] = Some(thread_id);
    }

    /// Unpin a thread on a given core.
    /// Matches upstream private `KProcess::UnpinThread(s32, KThread*)`.
    fn unpin_thread_on_core(&mut self, core_id: i32, thread_id: u64) {
        assert!((0..NUM_CPU_CORES as i32).contains(&core_id));
        assert_eq!(self.pinned_threads[core_id as usize], Some(thread_id));
        self.pinned_threads[core_id as usize] = None;
    }

    /// Initialize for a user process.
    /// Port of upstream `KProcess::Initialize(params, user_caps, res_limit, pool, aslr_space_start)`
    /// (the 5-arg overload at k_process.cpp:353-454).
    ///
    /// Sets up the page table, maps the code region, initializes capabilities,
    /// assigns a process ID, and calls the 3-arg `initialize()` for PLR/state setup.
    pub fn initialize_for_user(
        &mut self,
        name: &[u8],
        flags: u32,
        program_id: u64,
        code_address: u64,
        code_num_pages: u64,
        version: u32,
        system_resource_num_pages: u64,
        user_caps: &[u32],
        resource_limit: Option<Arc<Mutex<KResourceLimit>>>,
        pool: k_memory_manager::Pool,
        aslr_space_start: u64,
    ) -> u32 {
        use super::k_scoped_resource_reservation::KScopedResourceReservation;

        // Set members (upstream lines 358-361).
        self.memory_pool = pool;
        self.is_default_application_system_resource = false;
        self.is_immortal = false;

        // Get the memory sizes (upstream lines 363-367).
        let code_size = (code_num_pages as usize) * PAGE_SIZE;
        let _system_resource_size = (system_resource_num_pages as usize) * PAGE_SIZE;

        // Reserve memory for our code resource (upstream lines 369-372).
        let mut memory_reservation = KScopedResourceReservation::new(
            resource_limit.clone(),
            LimitableResource::PhysicalMemoryMax,
            code_size as i64,
        );
        if !memory_reservation.succeeded() {
            return svc_results::RESULT_LIMIT_REACHED.get_inner_value();
        }

        // System resource setup (upstream lines 374-400).
        // Upstream creates a KSecureSystemResource for system_resource_num_pages != 0,
        // or uses the kernel's global system resource. We store None for now —
        // secure system resource initialization depends on KSystemControl and physical
        // memory allocation that is not yet ported. The slab managers it provides are
        // needed for advanced page table operations (MapPageGroup, etc.), not for the
        // basic MapPages/SetHeapSize path.
        if system_resource_num_pages != 0 {
            log::warn!(
                "initialize_for_user: system_resource_num_pages={} — \
                 KSecureSystemResource::Initialize not yet ported, skipping",
                system_resource_num_pages
            );
            // TODO: Create and initialize KSecureSystemResource
        } else {
            let is_app = (flags & CreateProcessFlag::IS_APPLICATION.bits()) != 0;
            self.is_default_application_system_resource = is_app;
            // TODO: reference kernel's global system resource
        }

        // Setup page table (upstream lines 408-417).
        {
            let as_type = flags & ADDRESS_SPACE_MASK;
            let enable_aslr = (flags & CreateProcessFlag::ENABLE_ASLR.bits()) != 0;
            let enable_das_merge =
                (flags & CreateProcessFlag::DISABLE_DEVICE_ADDRESS_SPACE_MERGE.bits()) == 0;
            let result = self.page_table.initialize_for_process(
                as_type,
                enable_aslr,
                enable_das_merge,
                !enable_aslr, // from_back
                pool as u32,
                code_address as usize,
                code_size,
                resource_limit.clone(),
                None, // memory — TODO: wire Core::Memory::Memory
                aslr_space_start as usize,
            );
            if result != RESULT_SUCCESS.get_inner_value() {
                return result;
            }
        }

        // Ensure we can insert the code region (upstream lines 426-428).
        if !self.page_table.can_contain(
            KProcessAddress::new(code_address),
            code_size,
            KMemoryState::CODE,
        ) {
            self.page_table.finalize();
            return svc_results::RESULT_INVALID_MEMORY_REGION.get_inner_value();
        }

        // Map the code region (upstream lines 430-432).
        let map_result = self.page_table.map_pages_at_address(
            KProcessAddress::new(code_address),
            code_num_pages as usize,
            KMemoryState::CODE,
            KMemoryPermission::KERNEL_READ | KMemoryPermission::NOT_MAPPED,
        );
        if map_result != RESULT_SUCCESS.get_inner_value() {
            self.page_table.finalize();
            return map_result;
        }

        // Initialize capabilities (upstream line 434-435).
        let mut caps = std::mem::take(&mut self.capabilities);
        let caps_result = caps.initialize_for_user(
            user_caps,
            Some(&mut self.page_table),
        );
        self.capabilities = caps;
        if caps_result != RESULT_SUCCESS.get_inner_value() {
            self.page_table.finalize();
            return caps_result;
        }

        // Initialize the process ID (upstream lines 437-440).
        // Upstream: m_process_id = m_kernel.CreateNewUserProcessID();
        // We don't have a kernel reference here, so process_id must be set by the caller
        // or passed in. For now, use the already-assigned process_id (set by System::load).
        // TODO: wire kernel reference for proper ID generation.

        // Call the 3-arg Initialize for PLR and state setup (upstream line 449).
        let init_result = self.initialize(
            name,
            flags,
            program_id,
            code_address,
            code_num_pages,
            version,
            resource_limit,
            true, // is_real
        );
        if init_result != RESULT_SUCCESS.get_inner_value() {
            self.page_table.finalize();
            return init_result;
        }

        // Commit the code memory reservation (upstream line 452).
        memory_reservation.commit();

        RESULT_SUCCESS.get_inner_value()
    }

    /// Initialize the process base fields.
    /// Matches upstream `KProcess::Initialize(params, res_limit, is_real)`.
    ///
    /// This is the "base" initializer called by the two heavier overloads
    /// (for KIP and for user processes). It sets misc fields, computes
    /// max_process_memory, generates entropy, and marks the process as
    /// initialized.
    ///
    /// NOTE: `is_real` controls whether a PLR (process local region) is created.
    pub fn initialize(&mut self, name: &[u8], flags: u32, program_id: u64,
                      code_address: u64, code_num_pages: u64, version: u32,
                      resource_limit: Option<Arc<Mutex<KResourceLimit>>>,
                      is_real: bool) -> u32 {
        // Create and clear PLR if real.
        if is_real {
            if let Some(plr) = self.create_thread_local_region() {
                self.plr_address = plr;
                // Zero the PLR in guest memory (Svc::ThreadLocalRegionSize = 0x200).
                let mut mem = self.process_memory.write().unwrap();
                for i in 0..0x200usize {
                    mem.write_8(plr.get() + i as u64, 0);
                }
            }
        }

        // Copy in the name from parameters.
        self.name = [0u8; 13];
        let copy_len = name.len().min(12);
        self.name[..copy_len].copy_from_slice(&name[..copy_len]);
        // Null terminate
        self.name[copy_len] = 0;

        // Set misc fields.
        self.state = ProcessState::Created;
        self.main_thread_stack_size = 0;
        self.used_kernel_memory_size.store(0, std::sync::atomic::Ordering::Relaxed);
        self.ideal_core_id = 0;
        self.flags = flags;
        self.version = version;
        self.program_id = program_id;
        self.code_address = KProcessAddress::new(code_address);
        self.code_size = (code_num_pages as usize) * PAGE_SIZE;
        self.is_application = (flags & CreateProcessFlag::IS_APPLICATION.bits()) != 0;

        // Set thread fields.
        for i in 0..NUM_CPU_CORES as usize {
            self.running_threads[i] = None;
            self.pinned_threads[i] = None;
            self.running_thread_idle_counts[i] = 0;
            self.running_thread_switch_counts[i] = 0;
        }

        // Set max memory based on address space type.
        // Upstream reads from page_table.GetHeapRegionSize()/GetAliasRegionSize().
        let as_mask = flags & ADDRESS_SPACE_MASK;
        if as_mask == CreateProcessFlag::ADDRESS_SPACE_32_BIT_WITHOUT_ALIAS.bits() {
            self.max_process_memory = self.page_table.get_heap_region_size()
                + self.page_table.get_alias_region_size();
        } else {
            self.max_process_memory = self.page_table.get_heap_region_size();
        }

        // Generate random entropy.
        {
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};
            for (i, val) in self.entropy.iter_mut().enumerate() {
                let mut hasher = DefaultHasher::new();
                (i as u64).hash(&mut hasher);
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_nanos()
                    .hash(&mut hasher);
                *val = hasher.finish();
            }
        }

        // Clear remaining fields.
        self.num_running_threads.store(0, std::sync::atomic::Ordering::Relaxed);
        self.num_process_switches.store(0, std::sync::atomic::Ordering::Relaxed);
        self.num_thread_switches.store(0, std::sync::atomic::Ordering::Relaxed);
        self.num_fpu_switches.store(0, std::sync::atomic::Ordering::Relaxed);
        self.num_supervisor_calls.store(0, std::sync::atomic::Ordering::Relaxed);
        self.num_ipc_messages.store(0, std::sync::atomic::Ordering::Relaxed);

        self.is_signaled = false;
        self.exception_thread_id = None;
        self.is_suspended = false;
        self.memory_release_hint = 0;
        self.schedule_count = 0;
        self.is_handle_table_initialized = false;

        // Open a reference to our resource limit.
        // Upstream: m_resource_limit = res_limit; m_resource_limit->Open();
        self.resource_limit = resource_limit;

        // We're initialized!
        self.is_initialized = true;

        RESULT_SUCCESS.get_inner_value()
    }

    /// Port of upstream `KProcess::LoadFromMetadata` (k_process.cpp:1153-1235).
    ///
    /// Creates a resource limit, builds process flags from metadata,
    /// calls the 5-arg `initialize_for_user()`, and sets remaining properties.
    pub fn load_from_metadata(
        &mut self,
        metadata: &ProgramMetadata,
        code_size: u64,
        aslr_space_start: u64,
        is_hbl: bool,
    ) -> u32 {
        // Create a resource limit for the process (upstream lines 1156-1159).
        let pool = match metadata.get_pool_partition() {
            PoolPartition::Application => k_memory_manager::Pool::Application,
            PoolPartition::Applet => k_memory_manager::Pool::Applet,
            PoolPartition::System => k_memory_manager::Pool::System,
            PoolPartition::SystemNonSecure => k_memory_manager::Pool::SystemNonSecure,
        };

        // Upstream: const auto physical_memory_size = m_kernel.MemoryManager().GetSize(pool);
        // We don't have a kernel reference yet, so use a default physical memory size
        // (4 GiB for Application pool, matching typical Switch memory layout).
        // TODO: wire kernel.MemoryManager().GetSize(pool) when kernel reference is available.
        let physical_memory_size: i64 = match pool {
            k_memory_manager::Pool::Application => 0xCD500000, // ~3.2 GiB (Switch Application pool)
            k_memory_manager::Pool::Applet => 0x1FB00000,       // ~507 MiB
            _ => 0x2C600000,                                     // ~710 MiB (System)
        };

        let res_limit = Arc::new(Mutex::new(
            super::k_resource_limit::create_resource_limit_for_process(physical_memory_size),
        ));

        // Declare flags and code address (upstream lines 1167-1168).
        let mut flags = CreateProcessFlag::empty();
        let mut code_address: u64 = 0;

        // Determine if we are an application (upstream lines 1170-1174).
        if pool == k_memory_manager::Pool::Application {
            flags |= CreateProcessFlag::IS_APPLICATION;
        }

        // If we are 64-bit, create as such (upstream lines 1177-1179).
        if metadata.is_64_bit_program() {
            flags |= CreateProcessFlag::IS_64_BIT;
        }

        // Set the address space type and code address (upstream lines 1182-1204).
        match metadata.get_address_space_type() {
            ProgramAddressSpaceType::Is39Bit => {
                flags |= CreateProcessFlag::ADDRESS_SPACE_64_BIT;
                code_address = 0x8000_0000;
            }
            ProgramAddressSpaceType::Is36Bit => {
                flags |= CreateProcessFlag::ADDRESS_SPACE_64_BIT_DEPRECATED;
                code_address = 0x0800_0000;
            }
            ProgramAddressSpaceType::Is32Bit => {
                flags |= CreateProcessFlag::ADDRESS_SPACE_32_BIT;
                code_address = 0x0020_0000;
            }
            ProgramAddressSpaceType::Is32BitNoMap => {
                flags |= CreateProcessFlag::ADDRESS_SPACE_32_BIT_WITHOUT_ALIAS;
                code_address = 0x0020_0000;
            }
        }

        // Build parameters (upstream lines 1206-1215).
        let code_num_pages = code_size / PAGE_SIZE as u64;
        let system_resource_num_pages = metadata.get_system_resource_size() as u64 / PAGE_SIZE as u64;

        // Initialize for application process (upstream line 1222-1224).
        // Calls the 5-arg Initialize which sets up page table, maps code,
        // initializes capabilities, assigns process ID, and calls the 3-arg
        // Initialize for PLR/state.
        let result = self.initialize_for_user(
            metadata.get_name(),
            flags.bits(),
            metadata.get_title_id(),
            code_address + aslr_space_start,
            code_num_pages,
            0, // version
            system_resource_num_pages,
            metadata.get_kernel_capabilities(),
            Some(res_limit),
            pool,
            aslr_space_start,
        );
        if result != RESULT_SUCCESS.get_inner_value() {
            return result;
        }

        // Assign remaining properties (upstream lines 1227-1228).
        self.is_hbl = is_hbl;
        self.ideal_core_id = metadata.get_main_thread_core() as i32;

        // TODO: Upstream calls this->InitializeInterfaces() here (line 1231)
        // which creates ArmDynarmic32/64 per core. Currently done in main.rs.

        RESULT_SUCCESS.get_inner_value()
    }

    /// Start process termination.
    /// Matches upstream private `KProcess::StartTermination()`.
    ///
    /// Terminates child threads (other than the caller) and finalizes the
    /// handle table if the process isn't immortal.
    fn start_termination(&mut self, current_thread_id: Option<u64>) -> u32 {
        let terminate_result = self.terminate_children(current_thread_id);

        // Finalize the handle table when done, if the process isn't immortal.
        if !self.is_immortal && self.is_handle_table_initialized {
            self.handle_table.finalize();
            self.is_handle_table_initialized = false;
        }

        terminate_result
    }

    /// Finish process termination.
    /// Matches upstream `KProcess::FinishTermination()`.
    ///
    /// Only terminates if the process isn't immortal: releases resource limit
    /// hint, changes state to Terminated, and closes a reference.
    fn finish_termination(&mut self) {
        if !self.is_immortal {
            // Release resource limit hint.
            // Upstream: m_memory_release_hint = GetUsedNonSystemUserPhysicalMemorySize();
            //           m_resource_limit->Release(PhysicalMemoryMax, 0, m_memory_release_hint);
            self.memory_release_hint = self.code_size + self.main_thread_stack_size;
            if let Some(ref rl) = self.resource_limit {
                rl.lock().unwrap().release_with_hint(
                    LimitableResource::PhysicalMemoryMax,
                    0,
                    self.memory_release_hint as i64,
                );
            }

            // Change state.
            self.change_state(ProcessState::Terminated);

            // TODO: self.close() — reference counting not yet implemented.
        }
    }

    /// Exit the process (called by the current thread).
    /// Matches upstream `KProcess::Exit()`.
    ///
    /// Determines whether termination is needed, starts it if so, and
    /// registers the process for worker task completion. The final
    /// `GetCurrentThread(m_kernel).Exit()` step from upstream is still
    /// delegated to the caller because this port currently invokes
    /// `KProcess::exit()` while holding the process mutex.
    pub fn exit(&mut self) {
        // Determine whether we need to start terminating.
        let mut needs_terminate = false;
        {
            // Upstream: KScopedLightLock lk(m_state_lock);
            //           KScopedSchedulerLock sl(m_kernel);
            assert!(self.state != ProcessState::Created);
            assert!(self.state != ProcessState::CreatedAttached);
            assert!(self.state != ProcessState::Crashed);
            assert!(self.state != ProcessState::Terminated);
            if self.state == ProcessState::Running
                || self.state == ProcessState::RunningAttached
                || self.state == ProcessState::DebugBreak
            {
                self.change_state(ProcessState::Terminating);
                needs_terminate = true;
            }
        }

        // If we need to start termination, do so.
        if needs_terminate {
            let current_thread_id = self
                .scheduler
                .as_ref()
                .and_then(Weak::upgrade)
                .and_then(|scheduler| scheduler.lock().unwrap().get_scheduler_current_thread_id());
            self.start_termination(current_thread_id);

            if let Some(process) = self.self_reference.as_ref().and_then(Weak::upgrade) {
                KWorkerTaskManager::add_task_static(
                    0,
                    WorkerType::Exit,
                    Box::new(move || {
                        process.lock().unwrap().do_worker_task_impl();
                    }),
                );
            } else {
                self.finish_termination();
            }
        }

        // Upstream: GetCurrentThread(m_kernel).Exit().
        // The caller still owns that step in this port to avoid re-entering
        // thread exit while `self` is borrowed under the process mutex.
    }

    /// Terminate the process (called externally).
    /// Matches upstream `KProcess::Terminate()`.
    pub fn terminate(&mut self) -> u32 {
        // Determine whether we need to start terminating.
        let mut needs_terminate = false;
        {
            // Upstream: KScopedLightLock lk(m_state_lock);

            // Check whether we're allowed to terminate.
            // R_UNLESS(m_state != State::Created, ResultInvalidState);
            if self.state == ProcessState::Created {
                return RESULT_INVALID_STATE.get_inner_value();
            }
            // R_UNLESS(m_state != State::CreatedAttached, ResultInvalidState);
            if self.state == ProcessState::CreatedAttached {
                return RESULT_INVALID_STATE.get_inner_value();
            }

            // Upstream: KScopedSchedulerLock sl(m_kernel);
            if self.state == ProcessState::Running
                || self.state == ProcessState::RunningAttached
                || self.state == ProcessState::Crashed
                || self.state == ProcessState::DebugBreak
            {
                self.change_state(ProcessState::Terminating);
                needs_terminate = true;
            }
        }

        // If we need to terminate, do so.
        if needs_terminate {
            // Start termination.
            let start_result = self.start_termination(None);
            if start_result == RESULT_SUCCESS.get_inner_value() {
                // Finish termination.
                self.finish_termination();
            } else {
                if let Some(process) = self.self_reference.as_ref().and_then(Weak::upgrade) {
                    KWorkerTaskManager::add_task_static(
                        0,
                        WorkerType::Exit,
                        Box::new(move || {
                            process.lock().unwrap().do_worker_task_impl();
                        }),
                    );
                } else {
                    self.finish_termination();
                }
            }
        }

        RESULT_SUCCESS.get_inner_value()
    }

    /// Worker task implementation.
    /// Matches upstream `KProcess::DoWorkerTaskImpl()` (k_process.cpp:456-467).
    /// Called by KWorkerTaskManager after Exit() registers the process as a task.
    pub fn do_worker_task_impl(&mut self) {
        self.terminate_children(None);

        // Finalize the handle table, if we're not immortal.
        if !self.is_immortal && self.is_handle_table_initialized {
            self.handle_table.finalize();
            self.is_handle_table_initialized = false;
        }

        // Finish termination.
        self.finish_termination();
    }

    /// Terminate child threads, preserving upstream ownership in `k_process.cpp`.
    ///
    /// Upstream's `TerminateChildren(...)` does two passes:
    /// 1. request termination on every child other than the exempt thread
    /// 2. iterate again and synchronously `Terminate()` remaining children
    ///
    /// This port keeps the same ownership boundary and first-pass ordering.
    /// The second pass is still constrained by the cooperative runtime: we only
    /// use blocking `KThread::terminate_thread()` when the target is already in
    /// a state that can complete immediately without needing guest execution.
    fn terminate_children(&mut self, thread_to_not_terminate_id: Option<u64>) -> u32 {
        let children: Vec<Arc<Mutex<KThread>>> = self
            .thread_objects
            .values()
            .cloned()
            .collect();

        for child in &children {
            let mut guard = child.lock().unwrap();
            if Some(guard.thread_id) == thread_to_not_terminate_id {
                continue;
            }
            if guard.get_state() != super::k_thread::ThreadState::TERMINATED {
                guard.request_terminate();
            }
        }

        for child in children {
            let should_terminate = {
                let guard = child.lock().unwrap();
                if Some(guard.thread_id) == thread_to_not_terminate_id {
                    false
                } else {
                    let state = guard.get_state();
                    state == super::k_thread::ThreadState::INITIALIZED || guard.is_signaled()
                }
            };

            if should_terminate {
                let terminate_result = KThread::terminate_thread(&child);
                if terminate_result != RESULT_SUCCESS.get_inner_value() {
                    return terminate_result;
                }
            }
        }

        RESULT_SUCCESS.get_inner_value()
    }

    /// Bootstrap and run the process main thread for the guest runtime path.
    ///
    /// This is the current Rust-side owner for the subset of upstream
    /// `KProcess::Run()` that is already implemented here: initialize the
    /// handle table, allocate the main-thread stack, create and register the
    /// main thread, publish its handle, update process state, and mark the
    /// thread runnable.
    pub fn run(
        &mut self,
        priority: i32,
        stack_size: usize,
        main_thread_id: u64,
        main_object_id: u64,
        is_64bit: bool,
    ) -> Result<(Arc<Mutex<KThread>>, Handle, u64, u64), u32> {
        let state = self.state;
        if state != ProcessState::Created && state != ProcessState::CreatedAttached {
            return Err(RESULT_INVALID_STATE.get_inner_value());
        }

        let handle_result = self.ensure_handle_table_initialized();
        if handle_result != RESULT_SUCCESS.get_inner_value() {
            return Err(handle_result);
        }

        let self_weak = self
            .self_reference
            .clone()
            .ok_or_else(|| RESULT_INVALID_STATE.get_inner_value())?;
        let scheduler_weak = self.scheduler.clone();
        let entry_point = self.get_entry_point().get();
        let ideal_core_id = self.get_ideal_core_id();
        let tls_address = self
            .create_thread_local_region()
            .ok_or_else(|| RESULT_INVALID_STATE.get_inner_value())?;

        // Allocate stack.
        // Upstream: m_page_table.MapPages(&stack_bottom, stack_size/PageSize,
        //                                 KMemoryState::Stack, KMemoryPermission::UserReadWrite)
        // stack_top = stack_bottom + stack_size
        //
        // Upstream uses the page table's find-free MapPages which searches the
        // stack region for a free range. The stack region in a 32-bit address
        // space overlaps with the code region, and the page table tracks which
        // pages are free vs mapped.
        //
        // For now, place the stack after the TLS page that contains the thread's
        // TLR, matching the layout produced by the previous ad-hoc allocation.
        // This is safe because the TLS page is the last page before the heap.
        let tls_page = tls_address.get() & !(PAGE_SIZE as u64 - 1);
        let stack_base = tls_page + THREAD_LOCAL_PAGE_SIZE as u64;
        let stack_top = stack_base + stack_size as u64;
        self.main_thread_stack_size = stack_size;

        // Track the stack in the page table's memory block manager.
        let stack_num_pages = stack_size / PAGE_SIZE;
        let map_result = self.page_table.map_pages_at_address(
            KProcessAddress::new(stack_base),
            stack_num_pages,
            KMemoryState::STACK,
            KMemoryPermission::USER_READ_WRITE,
        );
        if map_result != RESULT_SUCCESS.get_inner_value() {
            log::warn!("run: stack MapPages failed ({:#x}), continuing with ad-hoc allocation", map_result);
        }

        // Ensure the stack memory is backed in ProcessMemoryData and
        // registered in its block manager (so QueryMemory returns the
        // correct state for the stack region, matching upstream).
        {
            let mut mem = self.process_memory.write().unwrap();
            let needed = (stack_top - mem.base) as usize;
            if needed > mem.data.len() {
                mem.data.resize(needed, 0);
            }
            mem.update_region(
                stack_base,
                stack_size as u64,
                KMemoryState::STACK,
                KMemoryPermission::USER_READ_WRITE,
            );
        }

        // Upstream: m_page_table.SetMaxHeapSize(m_max_process_memory -
        //           (m_main_thread_stack_size + m_code_size))
        let max_heap = self.max_process_memory.saturating_sub(self.main_thread_stack_size + self.code_size);
        self.page_table.set_max_heap_size(max_heap);

        let main_thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut thread = main_thread.lock().unwrap();
            let result = thread.initialize_user_thread_with_tls(
                entry_point,
                0,
                stack_top,
                priority,
                ideal_core_id,
                self_weak,
                scheduler_weak,
                tls_address,
                main_thread_id,
                main_object_id,
                is_64bit,
            );
            if result != RESULT_SUCCESS.get_inner_value() {
                return Err(result);
            }
            thread.thread_type = super::k_thread::ThreadType::Main;
        }

        let thread_handle = {
            self.register_thread_object(main_thread.clone());

            let thread_handle = self.handle_table.add(main_object_id)?;
            {
                let mut thread = main_thread.lock().unwrap();
                thread.thread_context.r[0] = 0;
                thread.thread_context.r[1] = thread_handle as u64;
            }

            self.change_state(match state {
                ProcessState::Created => ProcessState::Running,
                ProcessState::CreatedAttached => ProcessState::RunningAttached,
                _ => unreachable!("validated process state above"),
            });

            if let Some(scheduler) = self.scheduler.as_ref().and_then(Weak::upgrade) {
                let core_id = self.get_ideal_core_id();
                scheduler
                    .lock()
                    .unwrap()
                    .initialize(main_thread_id, 0, core_id);
            }

            thread_handle
        };

        {
            let mut thread = main_thread.lock().unwrap();
            let run_result = thread.run();
            if run_result != RESULT_SUCCESS.get_inner_value() {
                return Err(run_result);
            }
            let thread_id = thread.get_thread_id();
            drop(thread);

            // Increment running thread count (thread.run() no longer does this
            // to avoid deadlock when called with process lock held).
            self.increment_running_thread_count();
            self.push_back_to_priority_queue(thread_id);
        }

        Ok((main_thread, thread_handle, stack_base, stack_top))
    }

    /// Reset the process signal.
    /// Matches upstream `KProcess::Reset()`.
    ///
    /// Upstream condition: fail if state == Terminated, fail if !signaled.
    /// Valid when process is NOT terminated but IS signaled.
    pub fn reset(&mut self) -> u32 {
        // R_UNLESS(m_state != State::Terminated, ResultInvalidState);
        if self.state == ProcessState::Terminated {
            return RESULT_INVALID_STATE.get_inner_value();
        }
        // R_UNLESS(m_is_signaled, ResultInvalidState);
        if !self.is_signaled {
            return RESULT_INVALID_STATE.get_inner_value();
        }

        // Clear signaled.
        self.is_signaled = false;
        RESULT_SUCCESS.get_inner_value()
    }

    /// Finalize the process.
    /// Matches upstream `KProcess::Finalize()`.
    ///
    /// Cleans up the process local region, page table, shared memory,
    /// thread local pages, resource limits, and ARM interfaces.
    pub fn finalize(&mut self) {
        // Delete the process local region.
        if self.plr_address.get() != 0 {
            self.delete_thread_local_region(self.plr_address);
        }

        // Get the used memory size (for resource limit release).
        let used_memory_size = self.code_size + self.main_thread_stack_size;
        // TODO: When page table is fully wired:
        //   used_memory_size = self.get_used_non_system_user_physical_memory_size();

        // Finalize the page table.
        self.page_table.finalize();

        // TODO: Finish using our system resource (m_system_resource->Close()).
        // System resource is not yet wired in this port.

        // Free all shared memory infos.
        // TODO: When shared memory list is implemented, iterate and close.

        // Our thread local page list must be empty at this point.
        // (In practice, all TLRs should have been deleted during termination.)
        self.thread_local_pages.clear();

        // Release memory to the resource limit.
        // Upstream: m_resource_limit->Release(PhysicalMemoryMax, used_memory_size,
        //                                    used_memory_size - m_memory_release_hint);
        //           m_resource_limit->Close();
        if let Some(ref rl) = self.resource_limit {
            debug_assert!(used_memory_size >= self.memory_release_hint);
            let hint = (used_memory_size - self.memory_release_hint) as i64;
            rl.lock().unwrap().release_with_hint(
                LimitableResource::PhysicalMemoryMax,
                used_memory_size as i64,
                hint,
            );
        }
        // Drop our reference to the resource limit.
        self.resource_limit = None;

        // Clear thread and session objects.
        self.thread_objects.clear();
        self.session_objects.clear();
        self.client_session_objects.clear();
        self.event_objects.clear();
        self.readable_event_objects.clear();

        // Perform inherited finalization.
        // Upstream: KSynchronizationObject::Finalize();
        self.sync_object = SynchronizationObjectState::new();
    }

    /// Register a thread with this process.
    pub fn register_thread(&mut self, thread_id: u64) {
        self.thread_list.push(thread_id);
    }

    pub fn register_thread_object(&mut self, thread: Arc<Mutex<KThread>>) {
        let (thread_id, object_id) = {
            let mut thread_guard = thread.lock().unwrap();
            // Preserve upstream-style self ownership so KThread::Exit can queue
            // itself directly as a worker task.
            thread_guard.bind_self_reference(&thread);
            (thread_guard.thread_id, thread_guard.object_id)
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

    pub fn register_session_object(&mut self, object_id: u64, session: Arc<Mutex<KSession>>) {
        self.session_objects.insert(object_id, session);
    }

    pub fn unregister_session_object_by_object_id(&mut self, object_id: u64) {
        self.session_objects.remove(&object_id);
    }

    pub fn get_session_by_object_id(&self, object_id: u64) -> Option<Arc<Mutex<KSession>>> {
        self.session_objects.get(&object_id).cloned()
    }

    pub fn register_client_session_object(
        &mut self,
        object_id: u64,
        client_session: Arc<Mutex<KClientSession>>,
    ) {
        self.client_session_objects.insert(object_id, client_session);
    }

    pub fn unregister_client_session_object_by_object_id(&mut self, object_id: u64) {
        self.client_session_objects.remove(&object_id);
    }

    pub fn get_client_session_by_object_id(
        &self,
        object_id: u64,
    ) -> Option<Arc<Mutex<KClientSession>>> {
        self.client_session_objects.get(&object_id).cloned()
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
        let address_space_size = if base < 0x1_0000_0000 {
            0x1_0000_0000usize
        } else {
            0x80_0000_0000usize
        };
        let width = if base < 0x1_0000_0000 { 32 } else { 39 };
        self.page_table.configure_address_space(KProcessAddress::new(0), address_space_size, width);
        self.page_table
            .set_code_region(KProcessAddress::new(base), size);
    }

    pub fn set_heap_size(&mut self, size: usize) -> (u32, KProcessAddress) {
        let old_size = self.page_table.get_current_heap_size();
        let (result, heap_base) = self.page_table.set_heap_size(size);
        if result != RESULT_SUCCESS.get_inner_value() {
            return (result, heap_base);
        }

        let heap_base_u64 = heap_base.get();
        let mut mem = self.process_memory.write().unwrap();

        if size > old_size {
            mem.update_region(
                heap_base_u64 + old_size as u64,
                (size - old_size) as u64,
                KMemoryState::NORMAL,
                KMemoryPermission::USER_READ_WRITE,
            );
        } else if old_size > size {
            mem.update_region(
                heap_base_u64 + size as u64,
                (old_size - size) as u64,
                KMemoryState::FREE,
                KMemoryPermission::NONE,
            );
            mem.clear_sparse_range(heap_base_u64 + size as u64, old_size - size);
        }

        (RESULT_SUCCESS.get_inner_value(), heap_base)
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

/// ThreadAccessor impl for KProcess — allows the KPriorityQueue to
/// read/write thread queue entries through the process's thread table.
///
/// Upstream doesn't need this because KPriorityQueue uses raw Member*
/// pointers (which ARE the KThread objects). We need indirection because
/// our threads are behind Arc<Mutex<>>.
impl ThreadAccessor for KProcess {
    fn with_thread<F, R>(&self, thread_id: u64, f: F) -> Option<R>
    where
        F: FnOnce(&dyn KPriorityQueueMember) -> R,
    {
        let thread = self.get_thread_by_thread_id(thread_id)?;
        let guard = thread.lock().unwrap();
        Some(f(&*guard))
    }

    fn with_thread_mut<F, R>(&self, thread_id: u64, f: F) -> Option<R>
    where
        F: FnOnce(&mut dyn KPriorityQueueMember) -> R,
    {
        let thread = self.get_thread_by_thread_id(thread_id)?;
        let mut guard = thread.lock().unwrap();
        Some(f(&mut *guard))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::program_metadata::{ProgramAddressSpaceType, ProgramMetadata};
    use crate::hle::kernel::k_scheduler::KScheduler;

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

    #[test]
    fn run_bootstraps_process_owned_main_thread() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));

        {
            let mut process_guard = process.lock().unwrap();
            process_guard.code_address = KProcessAddress::new(0x100000);
            process_guard.allocate_code_memory(0x100000, 0x300000);
            process_guard.bind_self_reference(&process);
            process_guard.attach_scheduler(&scheduler);
            process_guard.initialize_thread_local_region_allocation(0x1c0000);
        }

        let (main_thread, main_thread_handle, stack_base, stack_top) =
            process
                .lock()
                .unwrap()
                .run(0, 0x100000, 1, 1, false)
                .expect("process runtime bootstrap should succeed");

        assert_ne!(main_thread_handle, 0);
        assert_eq!(process.lock().unwrap().state, ProcessState::Running);
        assert_eq!(
            scheduler.lock().unwrap().get_scheduler_current_thread_id(),
            Some(1)
        );

        let thread = main_thread.lock().unwrap();
        assert_eq!(thread.thread_context.pc, 0x100000);
        assert_eq!(thread.thread_context.sp, stack_top);
        assert_eq!(thread.thread_context.r[0], 0);
        assert_eq!(thread.thread_context.r[1], main_thread_handle as u64);
        assert_eq!(thread.get_tls_address().get(), 0x1c4000);
        assert_eq!(stack_base, 0x1c9000);
        assert_eq!(stack_top, 0x2c9000);
        assert_eq!(thread.get_state(), super::super::k_thread::ThreadState::RUNNABLE);
    }

    #[test]
    fn initialize_main_thread_stack_region_updates_process_owned_memory() {
        let mut process = KProcess::new();
        process.allocate_code_memory(0x100000, 0x400000);

        let (stack_base, stack_top) =
            process.initialize_main_thread_stack_region(0x201000, 0x100000);

        assert_eq!(stack_base, 0x205000);
        assert_eq!(stack_top, 0x305000);
        assert_eq!(process.main_thread_stack_size, 0x100000);

        let mem = process.process_memory.read().unwrap();
        let block = mem
            .block_manager
            .iter()
            .find(|block| block.get_address() == stack_base as usize)
            .expect("stack region must be tracked");
        assert_eq!(block.get_end_address(), stack_top as usize);
        assert_eq!(block.get_state(), KMemoryState::STACK);
        assert_eq!(block.get_permission(), KMemoryPermission::USER_READ_WRITE);
    }

    #[test]
    fn initialize_thread_local_region_allocation_feeds_first_main_thread_tlr() {
        let mut process = KProcess::new();
        process.allocate_code_memory(0x100000, 0x400000);

        let tls_page_base = process.initialize_thread_local_region_allocation(0x180000);
        let tls_region = process
            .create_thread_local_region()
            .expect("first thread local region should allocate");

        assert_eq!(tls_page_base, 0x184000);
        assert_eq!(tls_region.get(), tls_page_base);
        assert_eq!(process.next_thread_local_page_address, 0x185000);

        let mem = process.process_memory.read().unwrap();
        let block = mem
            .block_manager
            .iter()
            .find(|block| block.get_address() == tls_page_base as usize)
            .expect("tls page must be tracked");
        assert_eq!(block.get_state(), KMemoryState::THREAD_LOCAL);
        assert_eq!(block.get_permission(), KMemoryPermission::USER_READ_WRITE);
    }

    #[test]
    fn set_heap_size_maps_sparse_heap_region_without_resizing_contiguous_image() {
        let mut process = KProcess::new();
        process.allocate_code_memory(0x200000, 0x229a000);
        process.initialize_main_thread_stack_region(0x2396000, 0x100000);

        let contiguous_len_before = process.process_memory.read().unwrap().data.len();
        let (result, heap_base) = process.set_heap_size(0x78000000);
        assert_eq!(result, RESULT_SUCCESS.get_inner_value());
        assert_eq!(heap_base.get(), 0x249a000);
        assert_eq!(process.page_table.get_current_heap_size(), 0x78000000);
        assert!(process.process_memory.read().unwrap().is_valid_range(heap_base.get(), 4));

        {
            let mut mem = process.process_memory.write().unwrap();
            mem.write_32(heap_base.get(), 0x1234_5678);
            assert_eq!(mem.read_32(heap_base.get()), 0x1234_5678);
            assert_eq!(mem.data.len(), contiguous_len_before);
        }
    }

    #[test]
    fn load_from_metadata_sets_process_owned_entrypoint_and_launch_properties() {
        let mut metadata = ProgramMetadata::new();
        metadata.load_manual(
            false,
            ProgramAddressSpaceType::Is32Bit,
            0x2c,
            1,
            0x40000,
            0x0100_1520_0002_2000,
            0,
            0,
            vec![],
        );

        let mut process = KProcess::new();
        let result = process.load_from_metadata(&metadata, 0x120000);

        assert_eq!(result, RESULT_SUCCESS.get_inner_value());
        assert_eq!(process.get_entry_point().get(), 0x0020_0000);
        assert_eq!(process.get_program_id(), 0x0100_1520_0002_2000);
        assert_eq!(process.get_ideal_core_id(), 1);
        assert_eq!(process.get_main_stack_size(), 0x40000);
        assert!(!process.is_64bit());
        assert!(process.is_application());
    }

    #[test]
    fn exit_excludes_scheduler_current_thread_from_start_termination() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        scheduler.lock().unwrap().set_scheduler_current_thread_id(1);

        let current = Arc::new(Mutex::new(KThread::new()));
        {
            let mut guard = current.lock().unwrap();
            guard.thread_id = 1;
            guard.object_id = 10;
            guard.parent = Some(Arc::downgrade(&process));
            guard.set_state(super::super::k_thread::ThreadState::RUNNABLE);
        }

        let other = Arc::new(Mutex::new(KThread::new()));
        {
            let mut guard = other.lock().unwrap();
            guard.thread_id = 2;
            guard.object_id = 11;
            guard.parent = Some(Arc::downgrade(&process));
            guard.set_state(super::super::k_thread::ThreadState::RUNNABLE);
        }

        {
            let mut process_guard = process.lock().unwrap();
            process_guard.attach_scheduler(&scheduler);
            process_guard.state = ProcessState::Running;
            process_guard.register_thread_object(current.clone());
            process_guard.register_thread_object(other.clone());
            process_guard.exit();
        }

        assert!(!current.lock().unwrap().is_termination_requested());
        assert!(other.lock().unwrap().is_termination_requested());
    }

    #[test]
    fn register_thread_object_binds_thread_self_reference() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut guard = thread.lock().unwrap();
            guard.thread_id = 3;
            guard.object_id = 33;
        }

        process
            .lock()
            .unwrap()
            .register_thread_object(thread.clone());

        let rebound = thread
            .lock()
            .unwrap()
            .self_reference
            .as_ref()
            .and_then(Weak::upgrade)
            .expect("thread self reference must be bound during process registration");
        assert!(Arc::ptr_eq(&rebound, &thread));
    }

    #[test]
    fn exit_with_current_thread_also_exits_scheduler_current_thread() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        scheduler.lock().unwrap().set_scheduler_current_thread_id(1);

        let current = Arc::new(Mutex::new(KThread::new()));
        {
            let mut guard = current.lock().unwrap();
            guard.thread_id = 1;
            guard.object_id = 10;
            guard.parent = Some(Arc::downgrade(&process));
            guard.set_state(super::super::k_thread::ThreadState::RUNNABLE);
        }

        let other = Arc::new(Mutex::new(KThread::new()));
        {
            let mut guard = other.lock().unwrap();
            guard.thread_id = 2;
            guard.object_id = 11;
            guard.parent = Some(Arc::downgrade(&process));
            guard.set_state(super::super::k_thread::ThreadState::RUNNABLE);
        }

        {
            let mut process_guard = process.lock().unwrap();
            process_guard.attach_scheduler(&scheduler);
            process_guard.state = ProcessState::Running;
            process_guard.increment_running_thread_count();
            process_guard.increment_running_thread_count();
            process_guard.register_thread_object(current.clone());
            process_guard.register_thread_object(other.clone());
        }

        KProcess::exit_with_current_thread(&process);
        KWorkerTaskManager::wait_for_global_idle();

        assert!(current.lock().unwrap().is_termination_requested());
        assert!(other.lock().unwrap().is_termination_requested());
    }

    #[test]
    fn start_termination_synchronously_finishes_initialized_children() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let current = Arc::new(Mutex::new(KThread::new()));
        {
            let mut guard = current.lock().unwrap();
            guard.thread_id = 1;
            guard.object_id = 10;
            guard.parent = Some(Arc::downgrade(&process));
            guard.set_state(super::super::k_thread::ThreadState::RUNNABLE);
        }

        let initialized_child = Arc::new(Mutex::new(KThread::new()));
        {
            let mut guard = initialized_child.lock().unwrap();
            guard.thread_id = 2;
            guard.object_id = 11;
            guard.parent = Some(Arc::downgrade(&process));
            guard.set_state(super::super::k_thread::ThreadState::INITIALIZED);
        }

        {
            let mut process_guard = process.lock().unwrap();
            process_guard.state = ProcessState::Running;
            process_guard.register_thread_object(current.clone());
            process_guard.register_thread_object(initialized_child.clone());
            let result = process_guard.start_termination(Some(1));
            assert_eq!(result, RESULT_SUCCESS.get_inner_value());
        }

        assert_eq!(
            initialized_child.lock().unwrap().get_state(),
            super::super::k_thread::ThreadState::TERMINATED
        );
        assert!(!current.lock().unwrap().is_termination_requested());
    }
}
