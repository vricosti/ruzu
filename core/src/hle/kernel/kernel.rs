//! Port of zuyu/src/core/hle/kernel/kernel.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KernelCore: the main kernel class, managing all kernel subsystems
//! including schedulers, physical cores, memory layout, slab heaps,
//! shared memory objects, and the global scheduler context.
//!
//! Full implementation requires KProcess, KThread, KScheduler,
//! KMemoryManager, KMemoryLayout, KHardwareTimer, KHandleTable,
//! KResourceLimit, KWorkerTaskManager, and many other subsystems.

use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, Weak};

use super::k_memory_manager::KMemoryManager;
use super::k_thread::KThread;

use super::global_scheduler_context::GlobalSchedulerContext;
use super::init::init_slab_setup::KSlabResourceCounts;
use super::k_auto_object_container::KAutoObjectWithListContainer;
use super::k_hardware_timer::KHardwareTimer;
use super::k_object_name::KObjectNameGlobalData;
use super::physical_core::PhysicalCore;
use crate::core_timing::CoreTiming;
use crate::hardware_properties;

/// Constants from the upstream KernelCore::Impl.
const APPLICATION_MEMORY_BLOCK_SLAB_HEAP_SIZE: usize = 20000;
const SYSTEM_MEMORY_BLOCK_SLAB_HEAP_SIZE: usize = 10000;
const BLOCK_INFO_SLAB_HEAP_SIZE: usize = 4000;
const RESERVED_DYNAMIC_PAGE_COUNT: usize = 64;

/// Represents a single instance of the kernel.
///
/// Maps to upstream KernelCore and its inner Impl struct.
pub struct KernelCore {
    // -- Initialization state --
    is_multicore: bool,
    is_shutting_down: AtomicBool,
    is_phantom_mode_for_singlecore: AtomicBool,
    exception_exited: bool,

    // -- ID counters --
    next_object_id: AtomicU32,
    next_kernel_process_id: AtomicU64,
    next_user_process_id: AtomicU64,
    next_thread_id: AtomicU64,

    // -- Subsystems --
    hardware_timer: Option<Arc<Mutex<KHardwareTimer>>>,
    global_object_list_container: Option<KAutoObjectWithListContainer>,
    global_scheduler_context: Option<Arc<Mutex<GlobalSchedulerContext>>>,
    object_name_global_data: Option<KObjectNameGlobalData>,

    // -- Physical cores and schedulers --
    cores: Vec<PhysicalCore>,

    // -- Slab resource counts --
    slab_resource_counts: KSlabResourceCounts,

    // -- Process tracking --
    // application_process: Option<*mut KProcess>,
    // process_list: Vec<*mut KProcess>,
    process_list_lock: Mutex<()>,

    // -- Registered objects for leak tracking --
    registered_objects: Mutex<Vec<u64>>,
    registered_in_use_objects: Mutex<Vec<u64>>,

    // -- Host thread management --
    next_host_thread_id: AtomicU32,

    // -- Memory management --
    /// Physical memory manager. Upstream: `Impl::memory_manager`.
    memory_manager: KMemoryManager,

    // -- Current thread tracking --
    // Upstream: thread_local KThread* current_thread in Impl.
    // In cooperative model, only one guest thread runs at a time.
    current_emu_thread: Option<Weak<Mutex<KThread>>>,
}

// KProcess initial ID constants (matching upstream).
const INITIAL_PROCESS_ID_MIN: u64 = 1;
const PROCESS_ID_MIN: u64 = 81;

impl KernelCore {
    /// Construct a new kernel instance.
    pub fn new() -> Self {
        Self {
            is_multicore: true,
            is_shutting_down: AtomicBool::new(false),
            is_phantom_mode_for_singlecore: AtomicBool::new(false),
            exception_exited: false,

            next_object_id: AtomicU32::new(0),
            next_kernel_process_id: AtomicU64::new(INITIAL_PROCESS_ID_MIN),
            next_user_process_id: AtomicU64::new(PROCESS_ID_MIN),
            next_thread_id: AtomicU64::new(1),

            hardware_timer: None,
            global_object_list_container: None,
            global_scheduler_context: None,
            object_name_global_data: None,

            cores: Vec::new(),

            slab_resource_counts: KSlabResourceCounts::create_default(),

            process_list_lock: Mutex::new(()),
            registered_objects: Mutex::new(Vec::new()),
            registered_in_use_objects: Mutex::new(Vec::new()),

            memory_manager: KMemoryManager::new(),
            next_host_thread_id: AtomicU32::new(hardware_properties::NUM_CPU_CORES),
            current_emu_thread: None,
        }
    }

    /// Set whether emulation is multicore or single core.
    /// Must be called before Initialize.
    pub fn set_multicore(&mut self, is_multicore: bool) {
        self.is_multicore = is_multicore;
    }

    /// Initialize the kernel.
    pub fn initialize(&mut self) {
        self.hardware_timer = Some(Arc::new(Mutex::new(KHardwareTimer::new())));

        self.global_object_list_container = Some(KAutoObjectWithListContainer::new());
        self.global_scheduler_context = Some(Arc::new(Mutex::new(GlobalSchedulerContext::new())));

        self.is_phantom_mode_for_singlecore.store(false, Ordering::Relaxed);

        // Initialize slab resource counts.
        super::init::init_slab_setup::initialize_slab_resource_counts(
            &mut self.slab_resource_counts,
        );

        // Initialize physical cores.
        self.initialize_physical_cores();

        // Initialize global data.
        self.object_name_global_data = Some(KObjectNameGlobalData::new());
    }

    /// Wire the hardware timer to CoreTiming.
    /// Must be called after initialize() when System has CoreTiming available.
    pub fn wire_hardware_timer(&self, core_timing: Arc<std::sync::Mutex<CoreTiming>>) {
        if let Some(ref timer) = self.hardware_timer {
            KHardwareTimer::wire_callback(timer, core_timing);
        }
    }

    /// Shutdown the kernel.
    pub fn shutdown(&mut self) {
        self.is_shutting_down.store(true, Ordering::Relaxed);

        self.close_services();

        self.next_object_id.store(0, Ordering::Relaxed);
        self.next_kernel_process_id
            .store(INITIAL_PROCESS_ID_MIN, Ordering::Relaxed);
        self.next_user_process_id
            .store(PROCESS_ID_MIN, Ordering::Relaxed);
        self.next_thread_id.store(1, Ordering::Relaxed);

        // Clean up registered objects.
        {
            let mut in_use = self.registered_in_use_objects.lock().unwrap();
            in_use.clear();
        }
        {
            let mut registered = self.registered_objects.lock().unwrap();
            if !registered.is_empty() {
                log::debug!(
                    "{} kernel objects were dangling on shutdown!",
                    registered.len()
                );
                registered.clear();
            }
        }

        self.object_name_global_data = None;

        if let Some(ref container) = self.global_object_list_container {
            container.finalize();
        }
        self.global_object_list_container = None;

        if let Some(ref timer) = self.hardware_timer {
            timer.lock().unwrap().finalize();
        }
        self.hardware_timer = None;

        self.is_shutting_down.store(false, Ordering::Relaxed);
    }

    /// Close all active services.
    pub fn close_services(&self) {
        // TODO: Clear server managers.
    }

    /// Get the global scheduler context (Arc reference).
    pub fn global_scheduler_context(&self) -> Option<&Arc<Mutex<GlobalSchedulerContext>>> {
        self.global_scheduler_context.as_ref()
    }

    /// Get a physical core by index.
    pub fn physical_core(&self, id: usize) -> Option<&PhysicalCore> {
        self.cores.get(id)
    }

    /// Get the hardware timer (Arc reference).
    pub fn hardware_timer(&self) -> Option<&Arc<Mutex<KHardwareTimer>>> {
        self.hardware_timer.as_ref()
    }

    /// Get the object list container.
    pub fn object_list_container(&self) -> Option<&KAutoObjectWithListContainer> {
        self.global_object_list_container.as_ref()
    }

    /// Get the object name global data.
    pub fn object_name_global_data(&self) -> Option<&KObjectNameGlobalData> {
        self.object_name_global_data.as_ref()
    }

    /// Get the current emulation thread.
    /// Matches upstream `KernelCore::GetCurrentEmuThread()`.
    pub fn get_current_emu_thread(&self) -> Option<Arc<Mutex<KThread>>> {
        self.current_emu_thread.as_ref()?.upgrade()
    }

    /// Set the current emulation thread.
    /// Matches upstream `KernelCore::SetCurrentEmuThread(KThread*)`.
    pub fn set_current_emu_thread(&mut self, thread: Option<&Arc<Mutex<KThread>>>) {
        self.current_emu_thread = thread.map(Arc::downgrade);
    }

    /// Register a kernel object for leak tracking.
    pub fn register_kernel_object(&self, object_id: u64) {
        self.registered_objects.lock().unwrap().push(object_id);
    }

    /// Unregister a kernel object from leak tracking.
    pub fn unregister_kernel_object(&self, object_id: u64) {
        self.registered_objects
            .lock()
            .unwrap()
            .retain(|&id| id != object_id);
    }

    /// Register a kernel object as in-use.
    pub fn register_in_use_object(&self, object_id: u64) {
        self.registered_in_use_objects
            .lock()
            .unwrap()
            .push(object_id);
    }

    /// Unregister an in-use kernel object.
    pub fn unregister_in_use_object(&self, object_id: u64) {
        self.registered_in_use_objects
            .lock()
            .unwrap()
            .retain(|&id| id != object_id);
    }

    /// Whether the kernel is in multicore mode.
    pub fn is_multicore(&self) -> bool {
        self.is_multicore
    }

    /// Whether the kernel is shutting down.
    pub fn is_shutting_down(&self) -> bool {
        self.is_shutting_down.load(Ordering::Relaxed)
    }

    /// Workaround for single-core mode phantom mode.
    pub fn is_phantom_mode_for_single_core(&self) -> bool {
        self.is_phantom_mode_for_singlecore.load(Ordering::Relaxed)
    }

    /// Set the phantom mode for single core.
    pub fn set_is_phantom_mode_for_single_core(&self, value: bool) {
        self.is_phantom_mode_for_singlecore
            .store(value, Ordering::Relaxed);
    }

    /// Get the slab resource counts.
    pub fn slab_resource_counts(&self) -> &KSlabResourceCounts {
        &self.slab_resource_counts
    }

    /// Get the slab resource counts (mutable).
    pub fn slab_resource_counts_mut(&mut self) -> &mut KSlabResourceCounts {
        &mut self.slab_resource_counts
    }

    // -- Private methods --

    /// Create a new object ID.
    pub fn create_new_object_id(&self) -> u32 {
        self.next_object_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Create a new kernel process ID.
    pub fn create_new_kernel_process_id(&self) -> u64 {
        self.next_kernel_process_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Create a new user process ID.
    pub fn create_new_user_process_id(&self) -> u64 {
        self.next_user_process_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Create a new thread ID.
    pub fn create_new_thread_id(&self) -> u64 {
        self.next_thread_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Get the memory manager.
    /// Upstream: `KernelCore::MemoryManager()`.
    pub fn memory_manager(&self) -> &KMemoryManager {
        &self.memory_manager
    }

    /// Get the memory manager (mutable).
    pub fn memory_manager_mut(&mut self) -> &mut KMemoryManager {
        &mut self.memory_manager
    }

    fn initialize_physical_cores(&mut self) {
        self.cores.clear();
        for i in 0..hardware_properties::NUM_CPU_CORES as usize {
            self.cores.push(PhysicalCore::new(i, self.is_multicore));
        }
    }
}

impl Default for KernelCore {
    fn default() -> Self {
        Self::new()
    }
}
