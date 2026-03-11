// SPDX-FileCopyrightText: 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/core/heap_mapper.h
//! Port of zuyu/src/core/hle/service/nvdrv/core/heap_mapper.cpp

use std::sync::Mutex;

/// HeapMapper manages mapping regions of a process heap into the device address space.
pub struct HeapMapper {
    m_vaddress: u64,
    m_daddress: u64,
    m_size: usize,
    m_asid: u32,
    m_guard: Mutex<()>,
}

impl HeapMapper {
    pub fn new(start_vaddress: u64, start_daddress: u64, size: usize, asid: u32) -> Self {
        Self {
            m_vaddress: start_vaddress,
            m_daddress: start_daddress,
            m_size: size,
            m_asid: asid,
            m_guard: Mutex::new(()),
        }
    }

    pub fn is_in_bounds(&self, start: u64, size: usize) -> bool {
        let end = start + size as u64;
        start >= self.m_vaddress && end <= (self.m_vaddress + self.m_size as u64)
    }

    pub fn map(&self, start: u64, size: usize) -> u64 {
        let _lock = self.m_guard.lock().unwrap();
        // Stubbed: In the C++ code, this manages device memory mapping via range sets.
        self.m_daddress + (start - self.m_vaddress)
    }

    pub fn unmap(&self, start: u64, size: usize) {
        let _lock = self.m_guard.lock().unwrap();
        // Stubbed: In the C++ code, this manages device memory unmapping.
        log::debug!("HeapMapper::unmap(start={:#x}, size={:#x})", start, size);
    }

    pub fn get_region_start(&self) -> u64 {
        self.m_daddress
    }

    pub fn get_region_size(&self) -> usize {
        self.m_size
    }
}
