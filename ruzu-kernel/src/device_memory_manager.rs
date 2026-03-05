//! Port of zuyu/src/core/device_memory_manager.h and zuyu/src/core/device_memory_manager.inc
//! Status: EN COURS
//! Derniere synchro: 2026-03-05
//!
//! Manages device (GPU) virtual address space mapping to physical device memory.
//! This is a template class in C++ parameterized by device traits; in Rust we use
//! a trait-based generic approach.

use std::collections::VecDeque;
use std::sync::atomic::AtomicU8;
use std::sync::Mutex;

use ruzu_common::{PAddr, VAddr};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Page bits for device memory (same as DEVICE_PAGEBITS in C++).
pub const DEVICE_PAGEBITS: usize = 12;
/// Device page size.
pub const DEVICE_PAGESIZE: usize = 1 << DEVICE_PAGEBITS;
/// Device page mask.
pub const DEVICE_PAGEMASK: usize = DEVICE_PAGESIZE - 1;

/// Device virtual address type.
pub type DAddr = u64;

// ---------------------------------------------------------------------------
// Asid (Address Space ID)
// ---------------------------------------------------------------------------

/// Address Space Identifier for process tracking.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Asid {
    pub id: usize,
}

// ---------------------------------------------------------------------------
// DeviceInterface trait
// ---------------------------------------------------------------------------

/// Trait that must be implemented by the device (GPU) to receive notifications
/// about memory region invalidation and flushing.
///
/// Equivalent to the `DeviceInterface` and `DeviceMethods` types from C++ Traits.
pub trait DeviceInterface: Send + Sync {
    /// Called when a memory region needs to be invalidated in device caches.
    fn invalidate_region(&self, addr: DAddr, size: usize);

    /// Called when a memory region needs to be flushed from device caches.
    fn flush_region(&self, addr: DAddr, size: usize);

    /// Called to mark a region as cached or uncached.
    fn mark_region_caching(&self, addr: u64, size: u64, cached: bool);
}

/// A no-op device interface for cases where no GPU is present.
pub struct NullDeviceInterface;

impl DeviceInterface for NullDeviceInterface {
    fn invalidate_region(&self, _addr: DAddr, _size: usize) {}
    fn flush_region(&self, _addr: DAddr, _size: usize) {}
    fn mark_region_caching(&self, _addr: u64, _size: u64, _cached: bool) {}
}

// ---------------------------------------------------------------------------
// MultiAddressContainer (internal helper)
// ---------------------------------------------------------------------------

/// Container for tracking multiple device addresses mapped to the same physical page.
///
/// Port of the anonymous `MultiAddressContainer` class in C++.
struct MultiAddressContainer {
    storage: VecDeque<MultiEntry>,
    free_entries: VecDeque<u32>,
}

#[derive(Default)]
struct MultiEntry {
    next_entry: u32,
    value: u32,
}

impl MultiAddressContainer {
    fn new() -> Self {
        Self {
            storage: VecDeque::new(),
            free_entries: VecDeque::new(),
        }
    }

    /// Gather all values starting from a given entry.
    fn gather_values(&self, start_entry: u32, buffer: &mut Vec<u32>) {
        buffer.clear();
        let mut iter_entry = start_entry;
        loop {
            let current = &self.storage[(iter_entry - 1) as usize];
            buffer.push(current.value);
            if current.next_entry == 0 {
                break;
            }
            iter_entry = current.next_entry;
        }
    }

    /// Register a new value, returning its entry ID.
    fn register(&mut self, value: u32) -> u32 {
        self.register_impl(value)
    }

    /// Register a new value and append it to the chain starting at `start_entry`.
    fn register_chained(&mut self, value: u32, start_entry: u32) {
        let entry_id = self.register_impl(value);
        let mut iter_entry = start_entry;
        loop {
            let current = &self.storage[(iter_entry - 1) as usize];
            if current.next_entry == 0 {
                break;
            }
            iter_entry = current.next_entry;
        }
        self.storage[(iter_entry - 1) as usize].next_entry = entry_id;
    }

    /// Unregister a value from the chain. Returns (more_than_one_remaining, new_start).
    fn unregister(&mut self, value: u32, start_entry: u32) -> (bool, u32) {
        let mut iter_entry = start_entry;
        let mut previous_idx: Option<usize> = None;
        let mut count = 0usize;

        // Find the entry with the matching value.
        loop {
            let current = &self.storage[(iter_entry - 1) as usize];
            if current.value == value {
                break;
            }
            count += 1;
            previous_idx = Some((iter_entry - 1) as usize);
            iter_entry = current.next_entry;
        }

        let current = &self.storage[(iter_entry - 1) as usize];
        let next_entry = current.next_entry;
        let more_than_one_remaining;
        let result_start;

        if next_entry != 0 {
            let next = &self.storage[(next_entry - 1) as usize];
            more_than_one_remaining = next.next_entry != 0 || previous_idx.is_some();
        } else {
            more_than_one_remaining = false;
        }

        if let Some(prev) = previous_idx {
            self.storage[prev].next_entry = next_entry;
            result_start = start_entry;
        } else {
            result_start = next_entry;
        }

        self.free_entries.push_back(iter_entry);
        (more_than_one_remaining || count > 1, result_start)
    }

    /// Release an entry and return its value.
    fn release_entry(&mut self, start_entry: u32) -> u32 {
        let current = &self.storage[(start_entry - 1) as usize];
        let value = current.value;
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
        self.storage.push_back(MultiEntry::default());
        self.storage.len() as u32
    }
}

// ---------------------------------------------------------------------------
// DeviceMemoryManager
// ---------------------------------------------------------------------------

/// Manages device (GPU) virtual address space.
///
/// Port of `DeviceMemoryManager<Traits>` from C++. Maps device virtual addresses
/// to physical addresses in device memory, tracks which CPU virtual address backs
/// each device page, and manages cache coherency counters.
pub struct DeviceMemoryManager {
    /// Base address of the physical memory buffer (device memory).
    physical_base: usize,

    /// Compressed physical address table. Maps device page -> physical page + 1 (0 = unmapped).
    compressed_physical_ptr: Vec<u32>,

    /// Compressed device address table. Maps physical page -> device page(s).
    compressed_device_addr: Vec<u32>,

    /// Continuity tracker for span optimization.
    continuity_tracker: Vec<u32>,

    /// CPU backing address for each device page (packed VAddr | Asid).
    cpu_backing_address: Vec<u64>,

    /// Multi-address container for pages mapped to multiple device addresses.
    multi_dev_address: MultiAddressContainer,

    /// Device interface for invalidation/flushing callbacks.
    device_inter: Option<Box<dyn DeviceInterface>>,

    /// Pool of free ASID slots.
    id_pool: VecDeque<usize>,

    /// Registered process memory interfaces (indexed by ASID).
    registered_processes: VecDeque<Option<usize>>, // Placeholder; would be Memory* in C++

    /// Cache page counter entries.
    cached_pages: Vec<CacheCounterEntry>,

    /// Guard for mapping operations.
    mapping_guard: Mutex<()>,

    // Configuration
    device_virtual_bits: usize,
    page_bits: usize,
    page_size: usize,
    page_mask: usize,
}

/// Cache counter entry (8 bytes of atomic counters).
struct CacheCounterEntry {
    values: [AtomicU8; 8],
}

impl CacheCounterEntry {
    fn new() -> Self {
        Self {
            values: [
                AtomicU8::new(0),
                AtomicU8::new(0),
                AtomicU8::new(0),
                AtomicU8::new(0),
                AtomicU8::new(0),
                AtomicU8::new(0),
                AtomicU8::new(0),
                AtomicU8::new(0),
            ],
        }
    }

    fn count(&self, page: usize) -> &AtomicU8 {
        &self.values[page & 7]
    }
}

/// Constants for multi-address tracking.
const MULTI_FLAG_BITS: u32 = 31;
const MULTI_FLAG: u32 = 1u32 << MULTI_FLAG_BITS;
const MULTI_MASK: u32 = !MULTI_FLAG;

/// Guest address space constants.
const GUEST_MAX_AS_BITS: usize = 39;
const GUEST_AS_SIZE: u64 = 1u64 << GUEST_MAX_AS_BITS;
const GUEST_MASK: u64 = GUEST_AS_SIZE - 1;
const ASID_START_BIT: usize = GUEST_MAX_AS_BITS;

impl DeviceMemoryManager {
    /// Create a new device memory manager.
    ///
    /// # Parameters
    /// - `physical_base`: Base address of the physical memory buffer.
    /// - `device_virtual_bits`: Number of bits in the device virtual address space (e.g., 34).
    /// - `physical_bits`: Number of bits for physical address tracking (32 or 33).
    pub fn new(physical_base: usize, device_virtual_bits: usize, physical_bits: usize) -> Self {
        let page_bits = 12;
        let page_size = 1usize << page_bits;
        let page_mask = page_size - 1;

        let device_as_size = 1usize << device_virtual_bits;
        let total_device_pages = device_as_size >> page_bits;
        let total_phys_pages = 1usize << (physical_bits - page_bits);

        let mut compressed_physical_ptr = Vec::with_capacity(total_device_pages);
        compressed_physical_ptr.resize(total_device_pages, 0u32);

        let mut compressed_device_addr = Vec::with_capacity(total_phys_pages);
        compressed_device_addr.resize(total_phys_pages, 0u32);

        let mut continuity_tracker = Vec::with_capacity(total_device_pages);
        continuity_tracker.resize(total_device_pages, 1u32);

        let mut cpu_backing_address = Vec::with_capacity(total_device_pages);
        cpu_backing_address.resize(total_device_pages, 0u64);

        // Cache counter entries (1 entry per 8 pages).
        let num_counter_entries = total_device_pages / 8;
        let mut cached_pages = Vec::with_capacity(num_counter_entries);
        for _ in 0..num_counter_entries {
            cached_pages.push(CacheCounterEntry::new());
        }

        Self {
            physical_base,
            compressed_physical_ptr,
            compressed_device_addr,
            continuity_tracker,
            cpu_backing_address,
            multi_dev_address: MultiAddressContainer::new(),
            device_inter: None,
            id_pool: VecDeque::new(),
            registered_processes: VecDeque::new(),
            cached_pages,
            mapping_guard: Mutex::new(()),
            device_virtual_bits,
            page_bits,
            page_size,
            page_mask,
        }
    }

    /// Bind a device interface for receiving invalidation/flush callbacks.
    pub fn bind_interface(&mut self, device_inter: Box<dyn DeviceInterface>) {
        self.device_inter = Some(device_inter);
    }

    /// Allocate device virtual address space of the given size.
    /// Returns the allocated device address.
    ///
    /// Note: This is a simplified version. The C++ code uses a FlatAllocator.
    pub fn allocate(&mut self, _size: usize) -> DAddr {
        // TODO: Implement proper FlatAllocator
        0
    }

    /// Allocate at a fixed device address.
    pub fn allocate_fixed(&mut self, _start: DAddr, _size: usize) {
        // TODO: Implement proper FlatAllocator
    }

    /// Free device virtual address space.
    pub fn free(&mut self, _start: DAddr, _size: usize) {
        // TODO: Implement proper FlatAllocator
    }

    /// Map device virtual address to CPU virtual address.
    pub fn map(&mut self, address: DAddr, virtual_address: VAddr, size: usize, asid: Asid) {
        // Note: In C++ this takes mapping_guard via scoped_lock. Since we have &mut self,
        // we already have exclusive access in Rust's ownership model.
        let start_page_d = (address >> self.page_bits) as usize;
        let num_pages = ruzu_common::align_up(size as u64, self.page_size as u64) as usize
            >> self.page_bits;

        for i in 0..num_pages {
            let new_vaddress = virtual_address + (i as u64) * (self.page_size as u64);
            // In C++, this looks up the host pointer via process_memory->GetPointerSilent.
            // Since we don't have the full Memory integration yet, we store the mapping info.
            self.insert_cpu_backing(start_page_d + i, new_vaddress, asid);
            // Physical pointer tracking would go here once DeviceMemory is implemented.
        }
    }

    /// Unmap a device virtual address range.
    pub fn unmap(&mut self, address: DAddr, size: usize) {
        let start_page_d = (address >> self.page_bits) as usize;
        let num_pages = ruzu_common::align_up(size as u64, self.page_size as u64) as usize
            >> self.page_bits;

        if let Some(ref dev) = self.device_inter {
            dev.invalidate_region(address, size);
        }

        // Note: &mut self provides exclusive access (like scoped_lock in C++).
        for i in 0..num_pages {
            let page = start_page_d + i;
            let phys_addr = self.compressed_physical_ptr[page];
            self.compressed_physical_ptr[page] = 0;
            self.cpu_backing_address[page] = 0;

            if phys_addr != 0 {
                let phys_idx = (phys_addr - 1) as usize;
                let base_dev = self.compressed_device_addr[phys_idx];
                if (base_dev >> MULTI_FLAG_BITS) == 0 {
                    self.compressed_device_addr[phys_idx] = 0;
                    continue;
                }
                let (more_entries, new_start) = self
                    .multi_dev_address
                    .unregister(page as u32, base_dev & MULTI_MASK);
                if !more_entries {
                    let val = self.multi_dev_address.release_entry(new_start);
                    self.compressed_device_addr[phys_idx] = val;
                } else {
                    self.compressed_device_addr[phys_idx] = new_start | MULTI_FLAG;
                }
            }
        }
    }

    /// Get a pointer to device memory at the given device address.
    pub fn get_pointer(&self, address: DAddr) -> Option<*mut u8> {
        let index = (address >> self.page_bits) as usize;
        let offset = address as usize & self.page_mask;
        let phys_addr = *self.compressed_physical_ptr.get(index)?;
        if phys_addr == 0 {
            return None;
        }
        let raw_addr = ((phys_addr as usize - 1) << self.page_bits) + offset;
        Some((self.physical_base + raw_addr) as *mut u8)
    }

    /// Read a block of bytes from device memory.
    pub fn read_block(&self, address: DAddr, dest: &mut [u8]) {
        if let Some(ref dev) = self.device_inter {
            dev.flush_region(address, dest.len());
        }
        self.read_block_into(address, dest);
    }

    /// Write a block of bytes to device memory.
    pub fn write_block(&mut self, address: DAddr, src: &[u8]) {
        self.walk_block_write(address, src);
        if let Some(ref dev) = self.device_inter {
            dev.invalidate_region(address, src.len());
        }
    }

    /// Read a block without GPU flushing.
    pub fn read_block_unsafe(&self, address: DAddr, dest: &mut [u8]) {
        self.read_block_into(address, dest);
    }

    /// Write a block without GPU invalidation.
    pub fn write_block_unsafe(&mut self, address: DAddr, src: &[u8]) {
        self.walk_block_write(address, src);
    }

    /// Get a contiguous span of device memory, or None if not contiguous.
    pub fn get_span(&self, src_addr: DAddr, size: usize) -> Option<*mut u8> {
        let page_index = (src_addr >> self.page_bits) as usize;
        let subbits = src_addr as usize & self.page_mask;
        let cont = *self.continuity_tracker.get(page_index)? as usize;
        if (cont << self.page_bits) >= size + subbits {
            self.get_pointer(src_addr)
        } else {
            None
        }
    }

    /// Register a process and return its ASID.
    pub fn register_process(&mut self) -> Asid {
        if let Some(id) = self.id_pool.pop_front() {
            self.registered_processes[id] = Some(id);
            Asid { id }
        } else {
            self.registered_processes.push_back(Some(self.registered_processes.len()));
            Asid {
                id: self.registered_processes.len() - 1,
            }
        }
    }

    /// Unregister a process.
    pub fn unregister_process(&mut self, asid: Asid) {
        self.registered_processes[asid.id] = None;
        self.id_pool.push_front(asid.id);
    }

    /// Get the physical raw address from a device address.
    pub fn get_physical_raw_address_from_daddr(&self, address: DAddr) -> PAddr {
        let subbits = address as usize & self.page_mask;
        let index = (address >> self.page_bits) as usize;
        let paddr = self.compressed_physical_ptr.get(index).copied().unwrap_or(0);
        if paddr == 0 {
            return 0;
        }
        (((paddr as usize - 1) << self.page_bits) + subbits) as PAddr
    }

    // -- Internal helpers ---------------------------------------------------

    /// Read a contiguous block into dest, walking the page table.
    fn read_block_into(&self, addr: DAddr, dest: &mut [u8]) {
        let size = dest.len();
        let mut remaining = size;
        let mut page_index = (addr >> self.page_bits) as usize;
        let mut page_offset = addr as usize & self.page_mask;
        let mut dest_offset = 0usize;

        while remaining > 0 {
            let next_pages = self.continuity_tracker.get(page_index).copied().unwrap_or(1) as usize;
            let copy_amount = ((next_pages << self.page_bits) - page_offset).min(remaining);
            let current_vaddr = ((page_index << self.page_bits) + page_offset) as u64;

            let phys_addr = self.compressed_physical_ptr.get(page_index).copied().unwrap_or(0);
            if phys_addr == 0 {
                log::error!("Unmapped Device ReadBlock @ {:#018X}", current_vaddr);
                dest[dest_offset..dest_offset + copy_amount].fill(0);
            } else {
                let raw_addr = ((phys_addr as usize - 1) << self.page_bits) + page_offset;
                let mem_ptr = (self.physical_base + raw_addr) as *const u8;
                unsafe {
                    std::ptr::copy_nonoverlapping(
                        mem_ptr,
                        dest.as_mut_ptr().add(dest_offset),
                        copy_amount,
                    );
                }
            }

            page_index += next_pages;
            page_offset = 0;
            dest_offset += copy_amount;
            remaining -= copy_amount;
        }
    }

    fn extract_cpu_backing(&self, page_index: usize) -> (Asid, VAddr) {
        let content = self.cpu_backing_address[page_index];
        let address = content & GUEST_MASK;
        let asid = Asid {
            id: (content >> ASID_START_BIT) as usize,
        };
        (asid, address)
    }

    fn insert_cpu_backing(&mut self, page_index: usize, address: VAddr, asid: Asid) {
        self.cpu_backing_address[page_index] = address | ((asid.id as u64) << ASID_START_BIT);
    }

    /// Walk for a write operation.
    fn walk_block_write(&self, addr: DAddr, src: &[u8]) {
        let size = src.len();
        let mut remaining = size;
        let mut page_index = (addr >> self.page_bits) as usize;
        let mut page_offset = addr as usize & self.page_mask;
        let mut src_offset = 0usize;

        while remaining > 0 {
            let next_pages = self.continuity_tracker.get(page_index).copied().unwrap_or(1) as usize;
            let copy_amount = ((next_pages << self.page_bits) - page_offset).min(remaining);
            let current_vaddr = ((page_index << self.page_bits) + page_offset) as u64;

            let phys_addr = self.compressed_physical_ptr.get(page_index).copied().unwrap_or(0);
            if phys_addr == 0 {
                log::error!(
                    "Unmapped Device WriteBlock @ {:#018X}",
                    current_vaddr,
                );
            } else {
                let raw_addr = ((phys_addr as usize - 1) << self.page_bits) + page_offset;
                let mem_ptr = (self.physical_base + raw_addr) as *mut u8;
                unsafe {
                    std::ptr::copy_nonoverlapping(
                        src.as_ptr().add(src_offset),
                        mem_ptr,
                        copy_amount,
                    );
                }
            }

            page_index += next_pages;
            page_offset = 0;
            src_offset += copy_amount;
            remaining -= copy_amount;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_asid() {
        let asid = Asid { id: 42 };
        assert_eq!(asid.id, 42);
    }

    #[test]
    fn test_multi_address_container() {
        let mut mac = MultiAddressContainer::new();
        let id1 = mac.register(100);
        let id2 = mac.register(200);
        assert!(id1 > 0);
        assert!(id2 > 0);
        assert_ne!(id1, id2);

        // Chain them
        mac.register_chained(300, id1);
        let mut buf = Vec::new();
        mac.gather_values(id1, &mut buf);
        assert_eq!(buf.len(), 2);
        assert!(buf.contains(&100));
        assert!(buf.contains(&300));
    }

    #[test]
    fn test_device_memory_manager_new() {
        let dmm = DeviceMemoryManager::new(0x1000_0000, 34, 32);
        assert_eq!(dmm.device_virtual_bits, 34);
        assert_eq!(dmm.page_bits, 12);
        assert_eq!(dmm.page_size, 4096);
    }

    #[test]
    fn test_register_unregister_process() {
        let mut dmm = DeviceMemoryManager::new(0, 20, 20); // small for testing
        let asid1 = dmm.register_process();
        let asid2 = dmm.register_process();
        assert_ne!(asid1.id, asid2.id);

        dmm.unregister_process(asid1);
        let asid3 = dmm.register_process();
        assert_eq!(asid3.id, asid1.id); // Reused
    }

    #[test]
    fn test_cpu_backing_insert_extract() {
        let mut dmm = DeviceMemoryManager::new(0, 20, 20);
        let asid = Asid { id: 3 };
        let vaddr: VAddr = 0x1234_5000;

        dmm.insert_cpu_backing(0, vaddr, asid);
        let (extracted_asid, extracted_vaddr) = dmm.extract_cpu_backing(0);
        assert_eq!(extracted_asid.id, asid.id);
        assert_eq!(extracted_vaddr, vaddr);
    }

    #[test]
    fn test_null_device_interface() {
        let ndi = NullDeviceInterface;
        ndi.invalidate_region(0, 0);
        ndi.flush_region(0, 0);
        ndi.mark_region_caching(0, 0, false);
    }
}
