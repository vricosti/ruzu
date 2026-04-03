use crate::common::common::CpuAddr;
use std::mem::size_of;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum PoolLocation {
    Cpu = 1,
    Dsp = 2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum MemoryPoolState {
    Invalid,
    Acquired,
    RequestDetach,
    Detached,
    RequestAttach,
    Attached,
    Released,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum MemoryPoolResultState {
    Success,
    BadParam,
    MapFailed,
    InUse,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MemoryPoolInParameter {
    pub address: u64,
    pub size: u64,
    pub state: MemoryPoolState,
    pub in_use: bool,
    pub unk18: [u8; 8],
}

impl Default for MemoryPoolInParameter {
    fn default() -> Self {
        Self {
            address: 0,
            size: 0,
            state: MemoryPoolState::Invalid,
            in_use: false,
            unk18: [0; 8],
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MemoryPoolOutStatus {
    pub state: MemoryPoolState,
    pub unk04: [u8; 0x0C],
}

impl Default for MemoryPoolOutStatus {
    fn default() -> Self {
        Self {
            state: MemoryPoolState::Invalid,
            unk04: [0; 0x0C],
        }
    }
}

const _: () = assert!(size_of::<MemoryPoolInParameter>() == 0x20);
const _: () = assert!(size_of::<MemoryPoolOutStatus>() == 0x10);

#[derive(Debug, Clone, Copy)]
pub struct MemoryPoolInfo {
    cpu_address: CpuAddr,
    dsp_address: CpuAddr,
    size: u64,
    location: PoolLocation,
    in_use: bool,
}

impl MemoryPoolInfo {
    pub fn new(location: PoolLocation) -> Self {
        Self {
            cpu_address: 0,
            dsp_address: 0,
            size: 0,
            location,
            in_use: false,
        }
    }

    pub fn get_cpu_address(&self) -> CpuAddr {
        self.cpu_address
    }

    pub fn get_dsp_address(&self) -> CpuAddr {
        self.dsp_address
    }

    pub fn get_size(&self) -> u64 {
        self.size
    }

    pub fn get_location(&self) -> PoolLocation {
        self.location
    }

    pub fn set_cpu_address(&mut self, address: CpuAddr, size: u64) {
        self.cpu_address = address;
        self.size = size;
    }

    pub fn set_dsp_address(&mut self, address: CpuAddr) {
        self.dsp_address = address;
    }

    pub fn contains(&self, address: CpuAddr, size: u64) -> bool {
        let size = size as usize;
        self.cpu_address <= address
            && address.saturating_add(size) <= self.cpu_address.saturating_add(self.size as usize)
    }

    pub fn is_mapped(&self) -> bool {
        self.dsp_address != 0
    }

    pub fn translate(&self, address: CpuAddr, size: u64) -> CpuAddr {
        if !self.contains(address, size) || !self.is_mapped() {
            return 0;
        }
        self.dsp_address + (address - self.cpu_address)
    }

    pub fn set_used(&mut self, used: bool) {
        self.in_use = used;
    }

    pub fn is_used(&self) -> bool {
        self.in_use
    }
}

impl Default for MemoryPoolInfo {
    fn default() -> Self {
        Self::new(PoolLocation::Dsp)
    }
}
