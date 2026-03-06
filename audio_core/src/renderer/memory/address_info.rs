use crate::common::common::CpuAddr;
use crate::renderer::memory::memory_pool_info::MemoryPoolInfo;
use std::ptr;

#[derive(Debug, Clone, Copy)]
pub struct AddressInfo {
    cpu_address: CpuAddr,
    size: u64,
    memory_pool: *mut MemoryPoolInfo,
    dsp_address: CpuAddr,
}

impl AddressInfo {
    pub fn new(cpu_address: CpuAddr, size: u64) -> Self {
        Self {
            cpu_address,
            size,
            memory_pool: ptr::null_mut(),
            dsp_address: 0,
        }
    }

    pub fn setup(&mut self, cpu_address: CpuAddr, size: u64) {
        self.cpu_address = cpu_address;
        self.size = size;
        self.memory_pool = ptr::null_mut();
        self.dsp_address = 0;
    }

    pub fn get_cpu_addr(&self) -> CpuAddr {
        self.cpu_address
    }

    pub fn set_pool(&mut self, memory_pool: *mut MemoryPoolInfo) {
        self.memory_pool = memory_pool;
    }

    pub fn get_size(&self) -> u64 {
        self.size
    }

    pub fn get_force_mapped_dsp_addr(&self) -> CpuAddr {
        self.dsp_address
    }

    pub fn set_force_mapped_dsp_addr(&mut self, dsp_addr: CpuAddr) {
        self.dsp_address = dsp_addr;
    }

    pub fn has_mapped_memory_pool(&self) -> bool {
        if self.memory_pool.is_null() {
            return false;
        }
        unsafe { (*self.memory_pool).get_dsp_address() != 0 }
    }

    pub fn is_mapped(&self) -> bool {
        self.has_mapped_memory_pool() || self.dsp_address != 0
    }

    pub fn get_reference(&mut self, mark_in_use: bool) -> CpuAddr {
        if !self.has_mapped_memory_pool() {
            return self.dsp_address;
        }

        unsafe {
            if mark_in_use {
                (*self.memory_pool).set_used(true);
            }
            (*self.memory_pool).translate(self.cpu_address, self.size)
        }
    }
}

impl Default for AddressInfo {
    fn default() -> Self {
        Self::new(0, 0)
    }
}

// `AddressInfo` stores a non-owning pointer to a renderer-managed `MemoryPoolInfo`.
// The surrounding audio-core types are already synchronized externally via `Mutex`.
unsafe impl Send for AddressInfo {}
unsafe impl Sync for AddressInfo {}
