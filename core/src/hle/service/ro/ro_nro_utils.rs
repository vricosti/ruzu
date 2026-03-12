// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ro/ro_nro_utils.h
//! Port of zuyu/src/core/hle/service/ro/ro_nro_utils.cpp
//!
//! NRO memory mapping utility functions.

use crate::hle::result::ResultCode;
use super::ro_results;

/// A region of process memory to be mapped.
///
/// Corresponds to `ProcessMemoryRegion` in upstream ro_nro_utils.cpp.
struct ProcessMemoryRegion {
    pub address: u64,
    pub size: u64,
}

/// Get total size of process memory regions.
///
/// Corresponds to `GetTotalProcessMemoryRegionSize` in upstream.
fn get_total_process_memory_region_size(regions: &[ProcessMemoryRegion]) -> u64 {
    regions.iter().map(|r| r.size).sum()
}

/// Set up NRO process memory regions (NRO + optional BSS).
///
/// Corresponds to `SetupNroProcessMemoryRegions` in upstream.
fn setup_nro_process_memory_regions(
    nro_heap_address: u64,
    nro_heap_size: u64,
    bss_heap_address: u64,
    bss_heap_size: u64,
) -> Vec<ProcessMemoryRegion> {
    let mut regions = vec![ProcessMemoryRegion {
        address: nro_heap_address,
        size: nro_heap_size,
    }];

    if bss_heap_size > 0 {
        regions.push(ProcessMemoryRegion {
            address: bss_heap_address,
            size: bss_heap_size,
        });
    }

    regions
}

/// Map an NRO into a process's address space.
///
/// Corresponds to `MapNro` in upstream ro_nro_utils.cpp.
///
/// This requires kernel page table operations that are not yet available,
/// so the actual mapping is a todo. The structure and control flow match upstream.
pub fn map_nro(
    _nro_heap_address: u64,
    _nro_heap_size: u64,
    _bss_heap_address: u64,
    _bss_heap_size: u64,
) -> Result<u64, ResultCode> {
    // Set up the process memory regions.
    let _regions = setup_nro_process_memory_regions(
        _nro_heap_address,
        _nro_heap_size,
        _bss_heap_address,
        _bss_heap_size,
    );

    // TODO: Re-map the nro/bss as code memory in the destination process.
    // This requires MapProcessCodeMemory which depends on kernel page table support.
    // For now, return an error indicating we can't map.
    log::warn!("map_nro: kernel page table mapping not yet implemented");
    Err(ro_results::RESULT_OUT_OF_ADDRESS_SPACE)
}

/// Set NRO memory permissions (rx, ro, rw sections).
///
/// Corresponds to `SetNroPerms` in upstream ro_nro_utils.cpp.
pub fn set_nro_perms(
    _base_address: u64,
    _rx_size: u64,
    _ro_size: u64,
    _rw_size: u64,
) -> Result<(), ResultCode> {
    let _rx_offset: u64 = 0;
    let _ro_offset: u64 = _rx_size;
    let _rw_offset: u64 = _ro_offset + _ro_size;

    // TODO: SetProcessMemoryPermission for each section.
    // Requires kernel page table support.
    log::warn!("set_nro_perms: kernel page table permission setting not yet implemented");
    Ok(())
}

/// Unmap an NRO from a process's address space.
///
/// Corresponds to `UnmapNro` in upstream ro_nro_utils.cpp.
pub fn unmap_nro(
    _base_address: u64,
    _nro_heap_address: u64,
    _nro_heap_size: u64,
    _bss_heap_address: u64,
    _bss_heap_size: u64,
) -> Result<(), ResultCode> {
    // Set up the process memory regions.
    let _regions = setup_nro_process_memory_regions(
        _nro_heap_address,
        _nro_heap_size,
        _bss_heap_address,
        _bss_heap_size,
    );

    // TODO: UnmapProcessCodeMemory for each region in reverse.
    // Requires kernel page table support.
    log::warn!("unmap_nro: kernel page table unmapping not yet implemented");
    Ok(())
}
