// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ro/ro.h
//! Port of zuyu/src/core/hle/service/ro/ro.cpp
//!
//! RO service — read-only module loading ("ldr:ro" and "ro:1").

use crate::hle::result::ResultCode;
use super::ro_results;
use super::ro_types::{ModuleId, NroHeader, NrrHeader, NrrKind};

// Convenience definitions — matches upstream ro.cpp.
const MAX_SESSIONS: usize = 0x3;
const MAX_NRR_INFOS: usize = 0x40;
const MAX_NRO_INFOS: usize = 0x40;

const INVALID_PROCESS_ID: u64 = 0xFFFFFFFF_FFFFFFFF;
const INVALID_CONTEXT_ID: u64 = 0xFFFFFFFF_FFFFFFFF;

/// SHA-256 hash type.
type Sha256Hash = [u8; 32];

/// Per-NRO tracking information.
///
/// Corresponds to `NroInfo` in upstream ro.cpp.
#[derive(Clone)]
struct NroInfo {
    base_address: u64,
    nro_heap_address: u64,
    nro_heap_size: u64,
    bss_heap_address: u64,
    bss_heap_size: u64,
    code_size: u64,
    rw_size: u64,
    module_id: ModuleId,
}

impl Default for NroInfo {
    fn default() -> Self {
        Self {
            base_address: 0,
            nro_heap_address: 0,
            nro_heap_size: 0,
            bss_heap_address: 0,
            bss_heap_size: 0,
            code_size: 0,
            rw_size: 0,
            module_id: ModuleId::default(),
        }
    }
}

/// Per-NRR tracking information.
///
/// Corresponds to `NrrInfo` in upstream ro.cpp.
#[derive(Clone, Default)]
struct NrrInfo {
    nrr_heap_address: u64,
    nrr_heap_size: u64,
    hashes: Vec<Sha256Hash>,
}

/// Per-process context for RO service.
///
/// Corresponds to `ProcessContext` in upstream ro.cpp.
struct ProcessContext {
    nro_in_use: [bool; MAX_NRO_INFOS],
    nrr_in_use: [bool; MAX_NRR_INFOS],
    nro_infos: Vec<NroInfo>,
    nrr_infos: Vec<NrrInfo>,
    process_id: u64,
    in_use: bool,
}

impl Default for ProcessContext {
    fn default() -> Self {
        Self {
            nro_in_use: [false; MAX_NRO_INFOS],
            nrr_in_use: [false; MAX_NRR_INFOS],
            nro_infos: (0..MAX_NRO_INFOS).map(|_| NroInfo::default()).collect(),
            nrr_infos: (0..MAX_NRR_INFOS).map(|_| NrrInfo::default()).collect(),
            process_id: INVALID_PROCESS_ID,
            in_use: false,
        }
    }
}

impl ProcessContext {
    /// Initialize the context for a process.
    fn initialize(&mut self, process_id: u64) {
        assert!(!self.in_use);

        self.nro_in_use = [false; MAX_NRO_INFOS];
        self.nrr_in_use = [false; MAX_NRR_INFOS];
        for info in self.nro_infos.iter_mut() {
            *info = NroInfo::default();
        }
        for info in self.nrr_infos.iter_mut() {
            *info = NrrInfo::default();
        }

        self.process_id = process_id;
        self.in_use = true;
    }

    /// Finalize (release) the context.
    fn finalize(&mut self) {
        assert!(self.in_use);

        self.nro_in_use = [false; MAX_NRO_INFOS];
        self.nrr_in_use = [false; MAX_NRR_INFOS];
        for info in self.nro_infos.iter_mut() {
            *info = NroInfo::default();
        }
        for info in self.nrr_infos.iter_mut() {
            *info = NrrInfo::default();
        }

        self.process_id = INVALID_PROCESS_ID;
        self.in_use = false;
    }

    fn get_process_id(&self) -> u64 {
        self.process_id
    }

    fn is_free(&self) -> bool {
        !self.in_use
    }

    /// Find NRR info by heap address.
    fn get_nrr_info_index_by_address(&self, nrr_heap_address: u64) -> Result<usize, ResultCode> {
        for i in 0..MAX_NRR_INFOS {
            if self.nrr_in_use[i] && self.nrr_infos[i].nrr_heap_address == nrr_heap_address {
                return Ok(i);
            }
        }
        Err(ro_results::RESULT_NOT_REGISTERED)
    }

    /// Find a free NRR slot.
    fn get_free_nrr_info_index(&self) -> Result<usize, ResultCode> {
        for i in 0..MAX_NRR_INFOS {
            if !self.nrr_in_use[i] {
                return Ok(i);
            }
        }
        Err(ro_results::RESULT_TOO_MANY_NRR)
    }

    /// Find NRO info by base address.
    fn get_nro_info_index_by_address(&self, nro_address: u64) -> Result<usize, ResultCode> {
        for i in 0..MAX_NRO_INFOS {
            if self.nro_in_use[i] && self.nro_infos[i].base_address == nro_address {
                return Ok(i);
            }
        }
        Err(ro_results::RESULT_NOT_LOADED)
    }

    /// Find NRO info by module ID.
    fn get_nro_info_index_by_module_id(&self, module_id: &ModuleId) -> Result<usize, ResultCode> {
        for i in 0..MAX_NRO_INFOS {
            if self.nro_in_use[i] && self.nro_infos[i].module_id == *module_id {
                return Ok(i);
            }
        }
        Err(ro_results::RESULT_NOT_LOADED)
    }

    /// Find a free NRO slot.
    fn get_free_nro_info_index(&self) -> Result<usize, ResultCode> {
        for i in 0..MAX_NRO_INFOS {
            if !self.nro_in_use[i] {
                return Ok(i);
            }
        }
        Err(ro_results::RESULT_TOO_MANY_NRO)
    }

    /// Validate that the NRO hash is present in a registered NRR.
    ///
    /// Note: Actual memory read + SHA-256 requires kernel/memory integration.
    fn validate_has_nro_hash(
        &self,
        _base_address: u64,
        _nro_header: &NroHeader,
    ) -> Result<(), ResultCode> {
        // TODO: Calculate SHA-256 hash of NRO data from process memory,
        // then search all in-use NRR hash lists.
        // This requires kernel memory read support.
        log::warn!("validate_has_nro_hash: hash validation not yet implemented, allowing");
        Ok(())
    }

    /// Validate an NRO header and check constraints.
    ///
    /// Corresponds to `ProcessContext::ValidateNro` in upstream ro.cpp.
    fn validate_nro(
        &self,
        _base_address: u64,
        expected_nro_size: u64,
        expected_bss_size: u64,
        header: &NroHeader,
    ) -> Result<(ModuleId, u64, u64, u64), ResultCode> {
        // Validate magic.
        if !header.is_magic_valid() {
            return Err(ro_results::RESULT_INVALID_NRO);
        }

        // Read sizes from header.
        let nro_size = header.get_size() as u64;
        let text_ofs = header.get_text_offset() as u64;
        let text_size = header.get_text_size() as u64;
        let ro_ofs = header.get_ro_offset() as u64;
        let ro_size = header.get_ro_size() as u64;
        let rw_ofs = header.get_rw_offset() as u64;
        let rw_size = header.get_rw_size() as u64;
        let bss_size = header.get_bss_size() as u64;

        const PAGE_SIZE: u64 = 0x1000;

        // Validate sizes meet expected.
        if nro_size != expected_nro_size {
            return Err(ro_results::RESULT_INVALID_NRO);
        }
        if bss_size != expected_bss_size {
            return Err(ro_results::RESULT_INVALID_NRO);
        }

        // Validate all sizes are aligned.
        if text_size % PAGE_SIZE != 0 {
            return Err(ro_results::RESULT_INVALID_NRO);
        }
        if ro_size % PAGE_SIZE != 0 {
            return Err(ro_results::RESULT_INVALID_NRO);
        }
        if rw_size % PAGE_SIZE != 0 {
            return Err(ro_results::RESULT_INVALID_NRO);
        }
        if bss_size % PAGE_SIZE != 0 {
            return Err(ro_results::RESULT_INVALID_NRO);
        }

        // Validate sections are in order.
        if text_ofs > ro_ofs {
            return Err(ro_results::RESULT_INVALID_NRO);
        }
        if ro_ofs > rw_ofs {
            return Err(ro_results::RESULT_INVALID_NRO);
        }

        // Validate sections are sequential and contiguous.
        if text_ofs != 0 {
            return Err(ro_results::RESULT_INVALID_NRO);
        }
        if text_ofs + text_size != ro_ofs {
            return Err(ro_results::RESULT_INVALID_NRO);
        }
        if ro_ofs + ro_size != rw_ofs {
            return Err(ro_results::RESULT_INVALID_NRO);
        }
        if rw_ofs + rw_size != nro_size {
            return Err(ro_results::RESULT_INVALID_NRO);
        }

        // Check if NRO has already been loaded.
        let module_id = header.get_module_id();
        if self.get_nro_info_index_by_module_id(module_id).is_ok() {
            return Err(ro_results::RESULT_ALREADY_LOADED);
        }

        Ok((*module_id, text_size, ro_size, rw_size))
    }
}

/// Validate address and non-zero size (both page-aligned).
///
/// Corresponds to `ValidateAddressAndNonZeroSize` in upstream ro.cpp.
fn validate_address_and_non_zero_size(address: u64, size: u64) -> Result<(), ResultCode> {
    const PAGE_SIZE: u64 = 0x1000;
    if address % PAGE_SIZE != 0 {
        return Err(ro_results::RESULT_INVALID_ADDRESS);
    }
    if size == 0 {
        return Err(ro_results::RESULT_INVALID_SIZE);
    }
    if size % PAGE_SIZE != 0 {
        return Err(ro_results::RESULT_INVALID_SIZE);
    }
    if address >= address.wrapping_add(size) {
        // Overflow check — upstream: address < address + size
        return Err(ro_results::RESULT_INVALID_SIZE);
    }
    Ok(())
}

/// Validate address and size (both page-aligned, size may be zero).
///
/// Corresponds to `ValidateAddressAndSize` in upstream ro.cpp.
fn validate_address_and_size(address: u64, size: u64) -> Result<(), ResultCode> {
    const PAGE_SIZE: u64 = 0x1000;
    if address % PAGE_SIZE != 0 {
        return Err(ro_results::RESULT_INVALID_ADDRESS);
    }
    if size % PAGE_SIZE != 0 {
        return Err(ro_results::RESULT_INVALID_SIZE);
    }
    if size != 0 && address >= address.wrapping_add(size) {
        return Err(ro_results::RESULT_INVALID_SIZE);
    }
    Ok(())
}

/// Central RO context managing process contexts.
///
/// Corresponds to `RoContext` in upstream ro.cpp.
pub struct RoContext {
    process_contexts: Vec<ProcessContext>,
}

impl RoContext {
    pub fn new() -> Self {
        Self {
            process_contexts: (0..MAX_SESSIONS)
                .map(|_| ProcessContext::default())
                .collect(),
        }
    }

    /// Register a process.
    ///
    /// Corresponds to `RoContext::RegisterProcess` in upstream.
    pub fn register_process(&mut self, process_id: u64) -> Result<usize, ResultCode> {
        // Check if a process context already exists.
        if self.get_context_index_by_process_id(process_id).is_some() {
            return Err(ro_results::RESULT_INVALID_SESSION);
        }

        // Allocate a context.
        let context_id = self.allocate_context(process_id);
        Ok(context_id)
    }

    /// Validate that a context matches a process ID.
    pub fn validate_process(
        &self,
        context_id: usize,
        process_id: u64,
    ) -> Result<(), ResultCode> {
        if context_id as u64 == INVALID_CONTEXT_ID {
            return Err(ro_results::RESULT_INVALID_PROCESS);
        }
        let ctx = &self.process_contexts[context_id];
        if ctx.get_process_id() != process_id {
            return Err(ro_results::RESULT_INVALID_PROCESS);
        }
        Ok(())
    }

    /// Unregister a process.
    pub fn unregister_process(&mut self, context_id: usize) {
        self.free_context(context_id);
    }

    /// Register NRR module info.
    ///
    /// Corresponds to `RoContext::RegisterModuleInfo` in upstream.
    pub fn register_module_info(
        &mut self,
        context_id: usize,
        nrr_address: u64,
        nrr_size: u64,
        _nrr_kind: NrrKind,
        _enforce_nrr_kind: bool,
    ) -> Result<(), ResultCode> {
        let context = &mut self.process_contexts[context_id];

        // Validate address/size.
        validate_address_and_non_zero_size(nrr_address, nrr_size)?;

        // Check we have space for a new NRR.
        let nrr_index = context.get_free_nrr_info_index()?;

        // TODO: Read NRR header from process memory and populate hashes.
        // This requires kernel memory read support.

        // Set NRR info.
        context.nrr_in_use[nrr_index] = true;
        context.nrr_infos[nrr_index].nrr_heap_address = nrr_address;
        context.nrr_infos[nrr_index].nrr_heap_size = nrr_size;
        context.nrr_infos[nrr_index].hashes.clear();

        log::debug!(
            "RegisterModuleInfo: registered NRR at address={:#x}, size={:#x}",
            nrr_address,
            nrr_size
        );

        Ok(())
    }

    /// Unregister NRR module info.
    ///
    /// Corresponds to `RoContext::UnregisterModuleInfo` in upstream.
    pub fn unregister_module_info(
        &mut self,
        context_id: usize,
        nrr_address: u64,
    ) -> Result<(), ResultCode> {
        const PAGE_SIZE: u64 = 0x1000;
        let context = &mut self.process_contexts[context_id];

        // Validate address.
        if nrr_address % PAGE_SIZE != 0 {
            return Err(ro_results::RESULT_INVALID_ADDRESS);
        }

        // Check the NRR is loaded.
        let nrr_index = context.get_nrr_info_index_by_address(nrr_address)?;

        // Nintendo does this unconditionally.
        context.nrr_in_use[nrr_index] = false;
        context.nrr_infos[nrr_index] = NrrInfo::default();

        Ok(())
    }

    /// Unmap NRO module memory.
    ///
    /// Corresponds to `RoContext::UnmapManualLoadModuleMemory` in upstream.
    pub fn unmap_manual_load_module_memory(
        &mut self,
        context_id: usize,
        nro_address: u64,
    ) -> Result<(), ResultCode> {
        const PAGE_SIZE: u64 = 0x1000;
        let context = &mut self.process_contexts[context_id];

        // Validate address.
        if nro_address % PAGE_SIZE != 0 {
            return Err(ro_results::RESULT_INVALID_ADDRESS);
        }

        // Check the NRO is loaded.
        let nro_index = context.get_nro_info_index_by_address(nro_address)?;

        // Save backup and clear (Nintendo does this unconditionally).
        let _nro_backup = context.nro_infos[nro_index].clone();
        context.nro_in_use[nro_index] = false;
        context.nro_infos[nro_index] = NroInfo::default();

        // TODO: UnmapNro using backup info (requires kernel page table).
        log::warn!("unmap_manual_load_module_memory: kernel unmap not yet implemented");

        Ok(())
    }

    // Private context helpers.

    fn get_context_index_by_process_id(&self, process_id: u64) -> Option<usize> {
        for i in 0..MAX_SESSIONS {
            if self.process_contexts[i].get_process_id() == process_id {
                return Some(i);
            }
        }
        None
    }

    fn allocate_context(&mut self, process_id: u64) -> usize {
        for i in 0..MAX_SESSIONS {
            if self.process_contexts[i].is_free() {
                self.process_contexts[i].initialize(process_id);
                return i;
            }
        }
        // Failure to find a free context is an abort condition (matching upstream).
        panic!("RoContext: no free process context available");
    }

    fn free_context(&mut self, context_id: usize) {
        if (context_id as u64) != INVALID_CONTEXT_ID && context_id < MAX_SESSIONS {
            self.process_contexts[context_id].finalize();
        }
    }
}

/// IPC command IDs for RoInterface.
///
/// Corresponds to the function table in upstream `RoInterface`.
pub mod commands {
    pub const MAP_MANUAL_LOAD_MODULE_MEMORY: u32 = 0;
    pub const UNMAP_MANUAL_LOAD_MODULE_MEMORY: u32 = 1;
    pub const REGISTER_MODULE_INFO: u32 = 2;
    pub const UNREGISTER_MODULE_INFO: u32 = 3;
    pub const REGISTER_PROCESS_HANDLE: u32 = 4;
    pub const REGISTER_PROCESS_MODULE_INFO: u32 = 10;
}

/// RoInterface — service interface per session.
///
/// Corresponds to `RoInterface` in upstream ro.cpp.
pub struct RoInterface {
    pub context_id: u64,
    pub nrr_kind: NrrKind,
}

impl RoInterface {
    pub fn new(nrr_kind: NrrKind) -> Self {
        Self {
            context_id: INVALID_CONTEXT_ID,
            nrr_kind,
        }
    }
}

/// LoopProcess — registers "ldr:ro" and "ro:1" services.
///
/// Corresponds to `Service::RO::LoopProcess` in upstream ro.cpp.
pub fn loop_process() {
    log::debug!("RO::LoopProcess called");
    // TODO: Create shared RoContext and register "ldr:ro" (NrrKind::User)
    // and "ro:1" (NrrKind::JitPlugin) with ServerManager.
}
