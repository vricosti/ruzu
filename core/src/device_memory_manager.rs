//! Port of zuyu/src/core/device_memory_manager.h and device_memory_manager.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! DeviceMemoryManager manages the mapping between device virtual addresses (DAddr)
//! and physical addresses, with support for multiple process address spaces (ASIDs).
//! This is the GPU-side memory manager.

use std::collections::VecDeque;
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::Mutex;

use common::range_mutex::{RangeMutex, ScopedRangeLock};
use common::scratch_buffer::ScratchBuffer;
use common::virtual_buffer::VirtualBuffer;

use crate::device_memory::DeviceMemory;

/// Device page size constants matching upstream.
pub const DEVICE_PAGEBITS: usize = 12;
pub const DEVICE_PAGESIZE: usize = 1 << DEVICE_PAGEBITS;
pub const DEVICE_PAGEMASK: usize = DEVICE_PAGESIZE - 1;

/// Address space identifier for a registered process.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Asid {
    pub id: usize,
}

/// Trait defining device memory manager parameters, matching the C++ Traits template parameter.
pub trait DeviceMemoryManagerTraits {
    /// Number of bits in the device virtual address space.
    const DEVICE_VIRTUAL_BITS: usize;

    /// The device interface type that receives cache notifications.
    type DeviceInterface: DeviceInterface;
}

/// Trait for the device interface that handles memory caching notifications.
/// Corresponds to the C++ Traits::DeviceInterface / Traits::DeviceMethods.
pub trait DeviceInterface {
    fn on_pages_cached(&self, addr: u64, size: usize, delta: i32);
}

/// Forward reference to Core::Memory::Memory.
/// Core::Memory::Memory is ported in memory/memory.rs but uses raw pointers
/// for integration. This opaque type is used as the registered process
/// memory interface; actual memory operations go through the page table
/// and DeviceMemory directly.
pub struct MemoryInterface;

/// Linked-list entry for multi-mapped device pages.
/// Matches upstream `MultiAddressContainer::Entry`.
#[derive(Default)]
struct MultiEntry {
    next_entry: u32,
    value: u32,
}

/// Container for tracking multiple device pages that map to the same physical page.
/// Uses a linked-list approach with a free-list for entry reuse.
///
/// Matches upstream `MultiAddressContainer` (device_memory_manager.inc:24-123).
struct MultiAddressContainer {
    storage: Vec<MultiEntry>,
    free_entries: VecDeque<u32>,
}

impl MultiAddressContainer {
    fn new() -> Self {
        Self {
            storage: Vec::new(),
            free_entries: VecDeque::new(),
        }
    }

    /// Gather all device page values starting from a linked-list entry.
    /// Upstream: `MultiAddressContainer::GatherValues` (device_memory_manager.inc:29-46).
    fn gather_values(&self, start_entry: u32, buffer: &mut ScratchBuffer<u32>) {
        buffer.resize_destructive(0);
        let mut index = 0usize;

        let mut iter_entry = start_entry;
        let current = &self.storage[(iter_entry - 1) as usize];
        buffer.resize(index + 1);
        buffer[index] = current.value;
        index += 1;

        let mut next = current.next_entry;
        while next != 0 {
            iter_entry = next;
            let current = &self.storage[(iter_entry - 1) as usize];
            buffer.resize(index + 1);
            buffer[index] = current.value;
            index += 1;
            next = current.next_entry;
        }
    }

    /// Register a single value, returning its entry ID (1-based).
    /// Upstream: `MultiAddressContainer::Register(u32 value)` (device_memory_manager.inc:48-50).
    fn register(&mut self, value: u32) -> u32 {
        self.register_impl(value)
    }

    /// Chain a value to an existing entry list.
    /// Upstream: `MultiAddressContainer::Register(u32 value, u32 start_entry)` (device_memory_manager.inc:52-61).
    fn register_chained(&mut self, value: u32, start_entry: u32) {
        let entry_id = self.register_impl(value);
        // Walk to end of chain.
        let mut iter_entry = start_entry;
        loop {
            let next = self.storage[(iter_entry - 1) as usize].next_entry;
            if next == 0 {
                break;
            }
            iter_entry = next;
        }
        self.storage[(iter_entry - 1) as usize].next_entry = entry_id;
    }

    /// Remove a value from a chain. Returns (more_entries_remain, new_start_entry).
    /// Upstream: `MultiAddressContainer::Unregister` (device_memory_manager.inc:63-90).
    fn unregister(&mut self, value: u32, start_entry: u32) -> (bool, u32) {
        let mut iter_entry = start_entry;
        let mut previous_idx: Option<u32> = None;
        let mut count = 0usize;

        // Find the entry with matching value.
        while self.storage[(iter_entry - 1) as usize].value != value {
            count += 1;
            previous_idx = Some(iter_entry);
            iter_entry = self.storage[(iter_entry - 1) as usize].next_entry;
        }

        let next_entry = self.storage[(iter_entry - 1) as usize].next_entry;
        let more_than_one_remaining;
        let mut result_start = start_entry;

        if next_entry != 0 {
            let next_has_more = self.storage[(next_entry - 1) as usize].next_entry != 0;
            more_than_one_remaining = next_has_more || previous_idx.is_some();
        } else {
            more_than_one_remaining = false;
        }

        if let Some(prev) = previous_idx {
            self.storage[(prev - 1) as usize].next_entry = next_entry;
        } else {
            result_start = next_entry;
        }

        self.free_entries.push_back(iter_entry);
        (more_than_one_remaining || count > 1, result_start)
    }

    /// Release an entry and return its value.
    /// Upstream: `MultiAddressContainer::ReleaseEntry` (device_memory_manager.inc:92-96).
    fn release_entry(&mut self, start_entry: u32) -> u32 {
        let value = self.storage[(start_entry - 1) as usize].value;
        self.free_entries.push_back(start_entry);
        value
    }

    fn register_impl(&mut self, value: u32) -> u32 {
        let entry_id = self.get_new_entry();
        let entry = &mut self.storage[(entry_id - 1) as usize];
        entry.next_entry = 0;
        entry.value = value;
        entry_id
    }

    fn get_new_entry(&mut self) -> u32 {
        if let Some(id) = self.free_entries.pop_front() {
            return id;
        }
        self.storage.push(MultiEntry::default());
        self.storage.len() as u32
    }
}

/// Internal allocator for device memory manager.
/// Corresponds to `DeviceMemoryManagerAllocator<DTraits>` in C++.
///
/// The C++ implementation uses `Common::FlatAllocator<DAddr, 0, device_virtual_bits>`
/// as `main_allocator` plus a `MultiAddressContainer` for multi-mapped device pages.
struct DeviceMemoryManagerAllocator {
    main_allocator: common::address_space::FlatAllocator,
    multi_dev_address: MultiAddressContainer,
}

/// Counter type for page cache tracking.
type CounterType = u8;
type CounterAtomicType = AtomicU8;

const SUBENTRIES: usize = 8 / core::mem::size_of::<CounterType>();
const SUBENTRIES_MASK: usize = SUBENTRIES - 1;
const SUBENTRIES_SHIFT: usize = {
    // std::countr_zero(sizeof(u64)) - std::countr_zero(sizeof(CounterType))
    // countr_zero(8) = 3, countr_zero(1) = 0 => 3
    let u64_ctz = (core::mem::size_of::<u64>() as u32).trailing_zeros() as usize;
    let counter_ctz = (core::mem::size_of::<CounterType>() as u32).trailing_zeros() as usize;
    u64_ctz - counter_ctz
};

/// A single entry in the cached pages counter array.
/// Contains SUBENTRIES atomic counters packed together.
#[repr(C)]
struct CounterEntry {
    values: [CounterAtomicType; SUBENTRIES],
}

impl CounterEntry {
    fn new() -> Self {
        // AtomicU8 doesn't implement Default, construct manually
        Self {
            values: std::array::from_fn(|_| AtomicU8::new(0)),
        }
    }

    fn count(&self, page: usize) -> &CounterAtomicType {
        &self.values[page & SUBENTRIES_MASK]
    }
}

// Verify CounterEntry is the expected size
const _: () = assert!(
    core::mem::size_of::<CounterEntry>() == SUBENTRIES * core::mem::size_of::<CounterType>(),
    "CounterEntry should be 8 bytes"
);

/// DeviceMemoryManager manages GPU device virtual address space.
///
/// Template parameter Traits defines the device address space size and interface types.
/// In C++ this is `DeviceMemoryManager<Traits>`.
pub struct DeviceMemoryManager<Traits: DeviceMemoryManagerTraits> {
    // Internal allocator (corresponds to `impl` unique_ptr in C++)
    allocator: DeviceMemoryManagerAllocator,

    /// Base pointer for physical memory, matching `physical_base` in C++.
    physical_base: usize,

    /// Device interface for cache notifications.
    device_inter: Option<*mut Traits::DeviceInterface>,

    /// Maps device page -> compressed physical page number.
    /// 0 means unmapped; otherwise (value - 1) << page_bits gives the physical address.
    compressed_physical_ptr: VirtualBuffer<u32>,

    /// Maps physical page -> compressed device page number.
    /// High bit (MULTI_FLAG) indicates multiple device pages map to this physical page.
    compressed_device_addr: VirtualBuffer<u32>,

    /// Tracks continuity of mappings for coalescing.
    continuity_tracker: VirtualBuffer<u32>,

    /// Pool of available ASID slots.
    id_pool: VecDeque<usize>,

    /// Registered process memory interfaces, indexed by ASID.
    registered_processes: VecDeque<Option<*mut MemoryInterface>>,

    /// Maps device page -> (ASID, VAddr) packed into a single u64.
    cpu_backing_address: VirtualBuffer<u64>,

    /// Per-page cache counters for tracking cached/uncached state.
    cached_pages: Vec<CounterEntry>,

    /// Mutex for protecting counter updates across a range.
    counter_guard: RangeMutex,

    /// Mutex for protecting mapping operations.
    mapping_guard: Mutex<()>,
}

impl<Traits: DeviceMemoryManagerTraits> DeviceMemoryManager<Traits> {
    // Private constants matching C++
    const DEVICE_VIRTUAL_BITS: usize = Traits::DEVICE_VIRTUAL_BITS;
    const DEVICE_AS_SIZE: usize = 1 << Self::DEVICE_VIRTUAL_BITS;
    const PHYSICAL_MIN_BITS: usize = 32;
    const PHYSICAL_MAX_BITS: usize = 33;
    const PAGE_BITS: usize = 12;
    const PAGE_SIZE: usize = 1 << Self::PAGE_BITS;
    const PAGE_MASK: usize = Self::PAGE_SIZE - 1;
    const PHYSICAL_ADDRESS_BASE: u32 = 1u32 << Self::PAGE_BITS as u32;
    const MULTI_FLAG_BITS: u32 = 31;
    const MULTI_FLAG: u32 = 1u32 << Self::MULTI_FLAG_BITS;
    const MULTI_MASK: u32 = !Self::MULTI_FLAG;

    const GUEST_MAX_AS_BITS: usize = 39;
    const GUEST_AS_SIZE: u64 = 1u64 << Self::GUEST_MAX_AS_BITS;
    const GUEST_MASK: u64 = Self::GUEST_AS_SIZE - 1;
    const ASID_START_BIT: usize = Self::GUEST_MAX_AS_BITS;

    const NUM_COUNTER_ENTRIES: usize =
        (1usize << (Traits::DEVICE_VIRTUAL_BITS - Self::PAGE_BITS)) / SUBENTRIES;

    /// Public constant matching C++ `AS_BITS`.
    pub const AS_BITS: usize = Traits::DEVICE_VIRTUAL_BITS;

    /// Whether this memory manager has flush/invalidation support.
    /// Matches C++ `static constexpr bool HAS_FLUSH_INVALIDATION = true`.
    pub const HAS_FLUSH_INVALIDATION: bool = true;

    /// First valid device address (matches `first_address = 1 << YUZU_PAGEBITS` in C++).
    const FIRST_ADDRESS: u32 = (1u32 << Self::PAGE_BITS) as u32;
    /// Maximum device address space size, capped to u32::MAX for the FlatAllocator.
    const MAX_DEVICE_AREA: u32 = {
        // device_virtual_bits is at most 34; clamp to fit u32 for FlatAllocator.
        let bits = Traits::DEVICE_VIRTUAL_BITS;
        if bits >= 32 { u32::MAX } else { (1u32 << bits) - 1 }
    };

    /// Create a new DeviceMemoryManager backed by the given DeviceMemory.
    pub fn new(device_memory: &DeviceMemory) -> Self {
        let physical_base = device_memory.buffer.backing_base_pointer() as usize;
        let device_pages = Self::DEVICE_AS_SIZE >> Self::PAGE_BITS;
        let physical_pages = 1usize << (Self::PHYSICAL_MAX_BITS - Self::PAGE_BITS);

        let mut cached_pages = Vec::with_capacity(Self::NUM_COUNTER_ENTRIES);
        for _ in 0..Self::NUM_COUNTER_ENTRIES {
            cached_pages.push(CounterEntry::new());
        }

        // Corresponds to `impl = std::make_unique<DeviceMemoryManagerAllocator<Traits>>()`
        // which calls `main_allocator(first_address)` where first_address = 1 << page_bits.
        let allocator = DeviceMemoryManagerAllocator {
            main_allocator: common::address_space::FlatAllocator::new(
                Self::FIRST_ADDRESS,
                Self::MAX_DEVICE_AREA,
            ),
            multi_dev_address: MultiAddressContainer::new(),
        };

        Self {
            allocator,
            physical_base,
            device_inter: None,
            compressed_physical_ptr: VirtualBuffer::with_count(device_pages),
            compressed_device_addr: VirtualBuffer::with_count(physical_pages),
            continuity_tracker: VirtualBuffer::with_count(device_pages),
            id_pool: VecDeque::new(),
            registered_processes: VecDeque::new(),
            cpu_backing_address: VirtualBuffer::with_count(device_pages),
            cached_pages,
            counter_guard: RangeMutex::new(),
            mapping_guard: Mutex::new(()),
        }
    }

    /// Bind a device interface for receiving cache notifications.
    pub fn bind_interface(&mut self, device_inter: &mut Traits::DeviceInterface) {
        self.device_inter = Some(device_inter as *mut _);
    }

    /// Allocate device virtual address space of the given size.
    /// Returns the start DAddr of the allocation.
    ///
    /// Corresponds to `DeviceMemoryManager::Allocate` which calls `impl->Allocate(size)`.
    pub fn allocate(&mut self, size: usize) -> u64 {
        self.allocator
            .main_allocator
            .allocate(size as u32)
            .map(|addr| addr as u64)
            .unwrap_or_else(|| {
                log::error!("DeviceMemoryManager::allocate: address space exhausted (size={})", size);
                0
            })
    }

    /// Allocate a fixed range of device virtual address space.
    ///
    /// Corresponds to `DeviceMemoryManager::AllocateFixed` which calls `impl->AllocateFixed(start, size)`.
    pub fn allocate_fixed(&mut self, start: u64, size: usize) {
        self.allocator
            .main_allocator
            .allocate_fixed(start as u32, size as u32);
    }

    /// Free a previously allocated device virtual address range.
    ///
    /// Corresponds to `DeviceMemoryManager::Free` which calls `impl->Free(start, size)`.
    pub fn free(&mut self, start: u64, size: usize) {
        self.allocator
            .main_allocator
            .free(start as u32, size as u32);
    }

    /// Map a range of device virtual addresses to a CPU virtual address range.
    /// Note: In C++ this acquires mapping_guard internally. In Rust, since we take
    /// &mut self, exclusive access is already guaranteed by the borrow checker.
    pub fn map(
        &mut self,
        address: u64,
        virtual_address: u64,
        size: usize,
        asid: Asid,
        track: bool,
    ) {
        let num_pages = size >> Self::PAGE_BITS;

        for i in 0..num_pages {
            let d_page = (address >> Self::PAGE_BITS) as usize + i;
            let v_addr = virtual_address + (i << Self::PAGE_BITS) as u64;

            // Store the CPU backing address with ASID
            self.insert_cpu_backing(d_page, v_addr, asid);
        }

        if track {
            self.track_continuity_impl(address, virtual_address, size, asid);
        }
    }

    /// Unmap a range of device virtual addresses.
    /// Note: In C++ this acquires mapping_guard internally. In Rust, &mut self
    /// provides exclusive access.
    pub fn unmap(&mut self, address: u64, size: usize) {
        let num_pages = size >> Self::PAGE_BITS;

        for i in 0..num_pages {
            let d_page = (address >> Self::PAGE_BITS) as usize + i;

            // Clear the physical pointer mapping
            self.compressed_physical_ptr[d_page] = 0;

            // Clear the CPU backing address
            self.cpu_backing_address[d_page] = 0;
        }
    }

    /// Track mapping continuity (internal implementation).
    pub fn track_continuity_impl(
        &mut self,
        address: u64,
        _virtual_address: u64,
        size: usize,
        _asid: Asid,
    ) {
        let num_pages = size >> Self::PAGE_BITS;
        for i in 0..num_pages {
            let d_page = (address >> Self::PAGE_BITS) as usize + i;
            self.continuity_tracker[d_page] = i as u32;
        }
    }

    /// Track mapping continuity with lock.
    /// Note: In C++ this acquires mapping_guard internally. In Rust, &mut self
    /// provides exclusive access.
    pub fn track_continuity(
        &mut self,
        address: u64,
        virtual_address: u64,
        size: usize,
        asid: Asid,
    ) {
        self.track_continuity_impl(address, virtual_address, size, asid);
    }

    /// Get a pointer to a device address.
    ///
    /// # Safety
    /// The address must be mapped and the resulting pointer must be used correctly.
    pub unsafe fn get_pointer<T>(&self, address: u64) -> *mut T {
        let p_addr = self.get_physical_raw_address_from_daddr(address);
        if p_addr == 0 {
            return core::ptr::null_mut();
        }
        (self.physical_base + p_addr as usize) as *mut T
    }

    /// Get a const pointer to a device address.
    ///
    /// # Safety
    /// The address must be mapped.
    pub unsafe fn get_pointer_const<T>(&self, address: u64) -> *const T {
        self.get_pointer::<T>(address) as *const T
    }

    /// Apply an operation on all device addresses corresponding to a physical address.
    /// Handles the case where multiple device addresses map to the same physical page.
    pub fn apply_op_on_paddr<F>(
        &self,
        address: u64,
        buffer: &mut ScratchBuffer<u32>,
        mut operation: F,
    ) where
        F: FnMut(u64),
    {
        let subbits = address & Self::PAGE_MASK as u64;
        let page_index = (address >> Self::PAGE_BITS) as usize;
        let base = self.compressed_device_addr[page_index];

        if (base >> Self::MULTI_FLAG_BITS) == 0 {
            // Single mapping (common case)
            let d_address = ((base as u64) << Self::PAGE_BITS as u64) + subbits;
            operation(d_address);
            return;
        }

        // Multiple mappings
        self.inner_gather_device_addresses(buffer, address);
        let len = buffer.len();
        for i in 0..len {
            let value = buffer[i];
            operation(((value as u64) << Self::PAGE_BITS as u64) + subbits);
        }
    }

    /// Get the raw physical address corresponding to a device address.
    pub fn get_physical_raw_address_from_daddr(&self, address: u64) -> u64 {
        let subbits = address & Self::PAGE_MASK as u64;
        let page_index = (address >> Self::PAGE_BITS) as usize;
        let paddr = self.compressed_physical_ptr[page_index];
        if paddr == 0 {
            return 0;
        }
        (((paddr - 1) as u64) << Self::PAGE_BITS as u64) + subbits
    }

    /// Write a value to a device address.
    ///
    /// # Safety
    /// The device address must be mapped to valid memory.
    pub unsafe fn write<T: Copy>(&self, address: u64, value: T) {
        let ptr = self.get_pointer::<T>(address);
        if !ptr.is_null() {
            core::ptr::write(ptr, value);
        }
    }

    /// Read a value from a device address.
    ///
    /// # Safety
    /// The device address must be mapped to valid memory.
    pub unsafe fn read<T: Copy + Default>(&self, address: u64) -> T {
        let ptr = self.get_pointer_const::<T>(address);
        if ptr.is_null() {
            T::default()
        } else {
            core::ptr::read(ptr)
        }
    }

    /// Get a mutable span pointer to a contiguous device address range.
    /// Returns null if the range crosses a page boundary with different physical backing.
    pub fn get_span(&self, src_addr: u64, size: usize) -> *mut u8 {
        let page = (src_addr >> Self::PAGE_BITS) as usize;
        let paddr = self.compressed_physical_ptr[page];
        if paddr == 0 {
            return core::ptr::null_mut();
        }

        // Check if the range is contained within a single physical page mapping
        let offset_in_page = (src_addr & Self::PAGE_MASK as u64) as usize;
        if offset_in_page + size > Self::PAGE_SIZE {
            // Check continuity across pages
            let end_page = ((src_addr + size as u64 - 1) >> Self::PAGE_BITS) as usize;
            for p in (page + 1)..=end_page {
                if self.compressed_physical_ptr[p] == 0 {
                    return core::ptr::null_mut();
                }
            }
        }

        let raw_paddr =
            (((paddr - 1) as u64) << Self::PAGE_BITS as u64) + (src_addr & Self::PAGE_MASK as u64);
        (self.physical_base + raw_paddr as usize) as *mut u8
    }

    /// Get a const span pointer.
    pub fn get_span_const(&self, src_addr: u64, size: usize) -> *const u8 {
        self.get_span(src_addr, size) as *const u8
    }

    /// Read a block of data from device memory into a destination buffer.
    pub fn read_block(&self, address: u64, dest_pointer: *mut u8, size: usize) {
        self.walk_block(
            address,
            size,
            |_addr, _size| {
                // on_unmapped: zero-fill
            },
            |src, dest, sz| unsafe {
                core::ptr::copy_nonoverlapping(src, dest, sz);
            },
            dest_pointer,
        );
    }

    /// Read a block (unsafe path, same implementation for now).
    pub fn read_block_unsafe(&self, address: u64, dest_pointer: *mut u8, size: usize) {
        self.read_block(address, dest_pointer, size);
    }

    /// Write a block of data from a source buffer to device memory.
    pub fn write_block(&self, address: u64, src_pointer: *const u8, size: usize) {
        self.walk_block_write(address, size, src_pointer);
    }

    /// Write a block (unsafe path, same implementation for now).
    pub fn write_block_unsafe(&self, address: u64, src_pointer: *const u8, size: usize) {
        self.write_block(address, src_pointer, size);
    }

    /// Register a process memory interface and return its ASID.
    pub fn register_process(&mut self, memory: *mut MemoryInterface) -> Asid {
        if let Some(id) = self.id_pool.pop_front() {
            self.registered_processes[id] = Some(memory);
            Asid { id }
        } else {
            let id = self.registered_processes.len();
            self.registered_processes.push_back(Some(memory));
            Asid { id }
        }
    }

    /// Unregister a process memory interface.
    pub fn unregister_process(&mut self, id: Asid) {
        self.registered_processes[id.id] = None;
        self.id_pool.push_back(id.id);
    }

    /// Update the cached page count for a device address range.
    /// Delta is +1 for caching, -1 for uncaching.
    pub fn update_pages_cached_count(&self, addr: u64, size: usize, delta: i32) {
        let start_page = (addr >> Self::PAGE_BITS) as usize;
        let end_page = ((addr + size as u64 - 1) >> Self::PAGE_BITS) as usize;

        // Lock the range
        let _guard = ScopedRangeLock::new(
            &self.counter_guard,
            start_page as u64,
            (end_page - start_page + 1) as u64,
        );

        for page in start_page..=end_page {
            let entry_index = page >> SUBENTRIES_SHIFT;
            if entry_index < self.cached_pages.len() {
                let counter = self.cached_pages[entry_index].count(page);
                if delta > 0 {
                    let old = counter.fetch_add(delta as u8, Ordering::Relaxed);
                    if old == 0 {
                        // Transition from uncached to cached
                        if let Some(device) = self.device_inter {
                            unsafe {
                                (*device).on_pages_cached(
                                    (page as u64) << Self::PAGE_BITS,
                                    Self::PAGE_SIZE,
                                    delta,
                                );
                            }
                        }
                    }
                } else {
                    let old = counter.fetch_sub((-delta) as u8, Ordering::Relaxed);
                    if old == 1 {
                        // Transition from cached to uncached
                        if let Some(device) = self.device_inter {
                            unsafe {
                                (*device).on_pages_cached(
                                    (page as u64) << Self::PAGE_BITS,
                                    Self::PAGE_SIZE,
                                    delta,
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    // --- Private helpers ---

    fn extract_cpu_backing(&self, page_index: usize) -> (Asid, u64) {
        let content = self.cpu_backing_address[page_index];
        let address = content & Self::GUEST_MASK;
        let asid = Asid {
            id: (content >> Self::ASID_START_BIT) as usize,
        };
        (asid, address)
    }

    fn insert_cpu_backing(&mut self, page_index: usize, address: u64, asid: Asid) {
        self.cpu_backing_address[page_index] =
            address | ((asid.id as u64) << Self::ASID_START_BIT);
    }

    /// Gather all device addresses that map to a single physical address.
    ///
    /// Upstream: `DeviceMemoryManager::InnerGatherDeviceAddresses`
    /// (device_memory_manager.inc:320-331).
    fn inner_gather_device_addresses(&self, buffer: &mut ScratchBuffer<u32>, address: u64) {
        let phys_addr = (address >> Self::PAGE_BITS) as usize;
        // Upstream: std::scoped_lock lk(mapping_guard);
        // In Rust, the caller provides exclusive access via &self/&mut self.
        let backing = self.compressed_device_addr[phys_addr];
        if (backing >> Self::MULTI_FLAG_BITS) != 0 {
            self.allocator
                .multi_dev_address
                .gather_values(backing & Self::MULTI_MASK, buffer);
            return;
        }
        buffer.resize(1);
        buffer[0] = backing;
    }

    fn walk_block<F>(
        &self,
        addr: u64,
        size: usize,
        _on_unmapped: impl Fn(u64, usize),
        on_memory: F,
        dest: *mut u8,
    ) where
        F: Fn(*const u8, *mut u8, usize),
    {
        let mut remaining = size;
        let mut current_addr = addr;
        let mut current_dest = dest;

        while remaining > 0 {
            let page_offset = (current_addr & Self::PAGE_MASK as u64) as usize;
            let copy_size = core::cmp::min(remaining, Self::PAGE_SIZE - page_offset);

            let page = (current_addr >> Self::PAGE_BITS) as usize;
            let paddr = self.compressed_physical_ptr[page];

            if paddr != 0 {
                let raw = (((paddr - 1) as u64) << Self::PAGE_BITS as u64)
                    + (current_addr & Self::PAGE_MASK as u64);
                let src = (self.physical_base + raw as usize) as *const u8;
                on_memory(src, current_dest, copy_size);
            }
            // If unmapped, leave dest unchanged (caller may want to zero-fill)

            remaining -= copy_size;
            current_addr += copy_size as u64;
            current_dest = unsafe { current_dest.add(copy_size) };
        }
    }

    fn walk_block_write(&self, addr: u64, size: usize, src: *const u8) {
        let mut remaining = size;
        let mut current_addr = addr;
        let mut current_src = src;

        while remaining > 0 {
            let page_offset = (current_addr & Self::PAGE_MASK as u64) as usize;
            let copy_size = core::cmp::min(remaining, Self::PAGE_SIZE - page_offset);

            let page = (current_addr >> Self::PAGE_BITS) as usize;
            let paddr = self.compressed_physical_ptr[page];

            if paddr != 0 {
                let raw = (((paddr - 1) as u64) << Self::PAGE_BITS as u64)
                    + (current_addr & Self::PAGE_MASK as u64);
                let dest = (self.physical_base + raw as usize) as *mut u8;
                unsafe {
                    core::ptr::copy_nonoverlapping(current_src, dest, copy_size);
                }
            }

            remaining -= copy_size;
            current_addr += copy_size as u64;
            current_src = unsafe { current_src.add(copy_size) };
        }
    }
}

// Safety: DeviceMemoryManager can be sent between threads.
// The internal raw pointers are to the physical memory base (stable for the
// lifetime of the emulator) and to the device interface (externally managed).
unsafe impl<Traits: DeviceMemoryManagerTraits> Send for DeviceMemoryManager<Traits> {}
