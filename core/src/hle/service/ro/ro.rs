// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ro/ro.h
//! Port of zuyu/src/core/hle/service/ro/ro.cpp
//!
//! RO service — read-only module loading ("ldr:ro" and "ro:1").

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use sha2::{Digest, Sha256};

use crate::hle::kernel::k_process::KProcess;
use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::ro_nro_utils;
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
    /// Upstream: `Kernel::KProcess* m_process`.
    process: Option<Arc<Mutex<KProcess>>>,
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
            process: None,
            process_id: INVALID_PROCESS_ID,
            in_use: false,
        }
    }
}

impl ProcessContext {
    /// Initialize the context for a process.
    ///
    /// Corresponds to `ProcessContext::Initialize` in upstream.
    fn initialize(&mut self, process: Option<Arc<Mutex<KProcess>>>, process_id: u64) {
        assert!(!self.in_use);

        self.nro_in_use = [false; MAX_NRO_INFOS];
        self.nrr_in_use = [false; MAX_NRR_INFOS];
        for info in self.nro_infos.iter_mut() {
            *info = NroInfo::default();
        }
        for info in self.nrr_infos.iter_mut() {
            *info = NrrInfo::default();
        }

        self.process = process;
        self.process_id = process_id;
        self.in_use = true;
    }

    /// Finalize (release) the context.
    ///
    /// Corresponds to `ProcessContext::Finalize` in upstream.
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

        self.process = None;
        self.process_id = INVALID_PROCESS_ID;
        self.in_use = false;
    }

    fn get_process(&self) -> Option<&Arc<Mutex<KProcess>>> {
        self.process.as_ref()
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
    /// Corresponds to `ProcessContext::ValidateHasNroHash` in upstream ro.cpp.
    /// Reads NRO data from process memory, computes SHA-256, and searches NRR hash lists.
    fn validate_has_nro_hash(
        &self,
        base_address: u64,
        nro_header: &NroHeader,
    ) -> Result<(), ResultCode> {
        // Calculate hash.
        let hash: Sha256Hash = {
            let size = nro_header.get_size() as u64;

            let process_arc = self.process.as_ref()
                .ok_or(ro_results::RESULT_INVALID_PROCESS)?;
            let process = process_arc.lock().unwrap();
            let mem = process.process_memory.read().unwrap();
            let nro_data = mem.read_bytes(base_address, size as usize);

            let mut hasher = Sha256::new();
            hasher.update(&nro_data);
            let result = hasher.finalize();
            let mut hash = [0u8; 32];
            hash.copy_from_slice(&result);
            hash
        };

        for i in 0..MAX_NRR_INFOS {
            // Ensure we only check NRRs that are in use.
            if !self.nrr_in_use[i] {
                continue;
            }

            // Locate the hash within the hash list.
            if self.nrr_infos[i].hashes.iter().any(|h| *h == hash) {
                // The hash is valid!
                return Ok(());
            }
        }

        Err(ro_results::RESULT_NOT_AUTHORIZED)
    }

    /// Validate an NRO header and check constraints.
    ///
    /// Corresponds to `ProcessContext::ValidateNro` in upstream ro.cpp.
    fn validate_nro(
        &self,
        base_address: u64,
        expected_nro_size: u64,
        expected_bss_size: u64,
    ) -> Result<(ModuleId, u64, u64, u64), ResultCode> {
        // Ensure we have a process to work on.
        let process_arc = self.process.as_ref()
            .ok_or(ro_results::RESULT_INVALID_PROCESS)?;

        // Read the NRO header from process memory.
        let header: NroHeader = {
            let process = process_arc.lock().unwrap();
            let mem = process.process_memory.read().unwrap();
            let header_bytes = mem.read_bytes(base_address, std::mem::size_of::<NroHeader>());
            assert!(header_bytes.len() >= std::mem::size_of::<NroHeader>());
            unsafe { std::ptr::read_unaligned(header_bytes.as_ptr() as *const NroHeader) }
        };

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

        // Verify NRO hash.
        self.validate_has_nro_hash(base_address, &header)?;

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
    /// Upstream: `std::mt19937_64 generate_random`.
    /// We use a simple counter-based approach; the exact PRNG is not critical
    /// for correctness (upstream uses it only for ASLR trial addresses).
    random_state: u64,
}

impl RoContext {
    pub fn new() -> Self {
        Self {
            process_contexts: (0..MAX_SESSIONS)
                .map(|_| ProcessContext::default())
                .collect(),
            random_state: 0,
        }
    }

    /// Simple PRNG matching upstream's mt19937_64 usage pattern.
    /// Used only for ASLR address randomization in MapProcessCodeMemory.
    fn generate_random(&mut self) -> u64 {
        // Use a simple xorshift64* — the exact distribution doesn't matter,
        // only that we produce different trial addresses.
        let mut x = self.random_state.wrapping_add(0x9E3779B97F4A7C15);
        x = (x ^ (x >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
        x = (x ^ (x >> 27)).wrapping_mul(0x94D049BB133111EB);
        x ^= x >> 31;
        self.random_state = x;
        x
    }

    /// Register a process.
    ///
    /// Corresponds to `RoContext::RegisterProcess` in upstream.
    pub fn register_process(
        &mut self,
        process: Option<Arc<Mutex<KProcess>>>,
        process_id: u64,
    ) -> Result<usize, ResultCode> {
        // Check if a process context already exists.
        if self.get_context_index_by_process_id(process_id).is_some() {
            return Err(ro_results::RESULT_INVALID_SESSION);
        }

        // Allocate a context.
        let context_id = self.allocate_context(process, process_id);
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

        // Ensure we have a valid process to read from.
        let process_arc = context.get_process()
            .ok_or(ro_results::RESULT_INVALID_PROCESS)?
            .clone();

        // Read NRR header from process memory.
        let (num_hashes, hashes_offset) = {
            let process = process_arc.lock().unwrap();
            let mem = process.process_memory.read().unwrap();
            let header_bytes = mem.read_bytes(nrr_address, std::mem::size_of::<NrrHeader>());
            assert!(header_bytes.len() >= std::mem::size_of::<NrrHeader>());
            let header: NrrHeader = unsafe {
                std::ptr::read_unaligned(header_bytes.as_ptr() as *const NrrHeader)
            };
            (header.get_num_hashes() as usize, header.get_hashes_offset())
        };

        // Set NRR info.
        context.nrr_in_use[nrr_index] = true;
        context.nrr_infos[nrr_index].nrr_heap_address = nrr_address;
        context.nrr_infos[nrr_index].nrr_heap_size = nrr_size;

        // Read NRR hash list from process memory.
        let hash_data_size = std::mem::size_of::<Sha256Hash>() * num_hashes;
        let mut hashes = Vec::with_capacity(num_hashes);
        if hash_data_size > 0 {
            let process = process_arc.lock().unwrap();
            let mem = process.process_memory.read().unwrap();
            let hash_bytes = mem.read_bytes(
                nrr_address + hashes_offset as u64,
                hash_data_size,
            );
            for i in 0..num_hashes {
                let offset = i * 32;
                let mut hash = [0u8; 32];
                hash.copy_from_slice(&hash_bytes[offset..offset + 32]);
                hashes.push(hash);
            }
        }
        context.nrr_infos[nrr_index].hashes = hashes;

        log::debug!(
            "RegisterModuleInfo: registered NRR at address={:#x}, size={:#x}, num_hashes={}",
            nrr_address,
            nrr_size,
            num_hashes,
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

    /// Map NRO module memory.
    ///
    /// Corresponds to `RoContext::MapManualLoadModuleMemory` in upstream.
    pub fn map_manual_load_module_memory(
        &mut self,
        context_id: usize,
        nro_address: u64,
        nro_size: u64,
        bss_address: u64,
        bss_size: u64,
    ) -> Result<u64, ResultCode> {
        let context = &mut self.process_contexts[context_id];

        // Validate address/size.
        validate_address_and_non_zero_size(nro_address, nro_size)?;
        validate_address_and_size(bss_address, bss_size)?;

        let total_size = nro_size + bss_size;
        if total_size < nro_size {
            return Err(ro_results::RESULT_INVALID_SIZE);
        }
        if total_size < bss_size {
            return Err(ro_results::RESULT_INVALID_SIZE);
        }

        // Check we have space for a new NRO.
        let nro_index = context.get_free_nro_info_index()?;
        context.nro_infos[nro_index].nro_heap_address = nro_address;
        context.nro_infos[nro_index].nro_heap_size = nro_size;
        context.nro_infos[nro_index].bss_heap_address = bss_address;
        context.nro_infos[nro_index].bss_heap_size = bss_size;

        // Get the process for page table operations.
        let process_arc = context.get_process()
            .ok_or(ro_results::RESULT_INVALID_PROCESS)?
            .clone();

        // Map the NRO.
        // We need to create a random generator closure that captures our state.
        // Since self is borrowed mutably for context, we use a local random state.
        let mut random_state = self.random_state;
        let mut gen_random = move || -> u64 {
            let mut x = random_state.wrapping_add(0x9E3779B97F4A7C15);
            x = (x ^ (x >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
            x = (x ^ (x >> 27)).wrapping_mul(0x94D049BB133111EB);
            x ^= x >> 31;
            random_state = x;
            x
        };

        let base_address = {
            let mut process = process_arc.lock().unwrap();
            ro_nro_utils::map_nro(
                &mut process,
                nro_address,
                nro_size,
                bss_address,
                bss_size,
                &mut gen_random,
            )
        };

        // Update the random state.
        // (We consumed some random values in the closure.)
        self.random_state = gen_random();

        let base_address = match base_address {
            Ok(addr) => addr,
            Err(e) => return Err(e),
        };

        // Re-borrow context after the map operation.
        let context = &mut self.process_contexts[context_id];
        context.nro_infos[nro_index].base_address = base_address;

        // Validate the NRO (parsing region extents).
        let (module_id, rx_size, ro_size, rw_size) =
            match context.validate_nro(base_address, nro_size, bss_size) {
                Ok(result) => result,
                Err(e) => {
                    // On validation failure, unmap the NRO.
                    let mut process = process_arc.lock().unwrap();
                    let _ = ro_nro_utils::unmap_nro(
                        &mut process,
                        base_address,
                        nro_address,
                        nro_size,
                        bss_address,
                        bss_size,
                    );
                    return Err(e);
                }
            };

        // Set NRO perms.
        {
            let mut process = process_arc.lock().unwrap();
            ro_nro_utils::set_nro_perms(
                &mut process,
                base_address,
                rx_size,
                ro_size,
                rw_size + bss_size,
            ).map_err(|e| {
                // On permission failure, unmap the NRO.
                let _ = ro_nro_utils::unmap_nro(
                    &mut process,
                    base_address,
                    nro_address,
                    nro_size,
                    bss_address,
                    bss_size,
                );
                e
            })?;
        }

        // Re-borrow context after the permission operation.
        let context = &mut self.process_contexts[context_id];
        context.nro_in_use[nro_index] = true;
        context.nro_infos[nro_index].module_id = module_id;
        context.nro_infos[nro_index].code_size = rx_size + ro_size;
        context.nro_infos[nro_index].rw_size = rw_size;

        Ok(base_address)
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
        let nro_backup = context.nro_infos[nro_index].clone();
        context.nro_in_use[nro_index] = false;
        context.nro_infos[nro_index] = NroInfo::default();

        // Get the process for unmapping.
        let process_arc = context.get_process()
            .cloned();

        // Unmap using backup info.
        if let Some(process_arc) = process_arc {
            let mut process = process_arc.lock().unwrap();
            ro_nro_utils::unmap_nro(
                &mut process,
                nro_backup.base_address,
                nro_backup.nro_heap_address,
                nro_backup.code_size + nro_backup.rw_size,
                nro_backup.bss_heap_address,
                nro_backup.bss_heap_size,
            )
        } else {
            // No process available — upstream would still attempt the unmap
            // but we can't without a process reference.
            log::warn!("unmap_manual_load_module_memory: no process reference available for unmap");
            Ok(())
        }
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

    fn allocate_context(
        &mut self,
        process: Option<Arc<Mutex<KProcess>>>,
        process_id: u64,
    ) -> usize {
        for i in 0..MAX_SESSIONS {
            if self.process_contexts[i].is_free() {
                self.process_contexts[i].initialize(process, process_id);
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

/// RoInterface — service interface per session.
///
/// Corresponds to `RoInterface` in upstream ro.cpp.
pub struct RoInterface {
    pub context_id: u64,
    pub nrr_kind: NrrKind,
    name: String,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl RoInterface {
    pub fn new(name: &str, nrr_kind: NrrKind) -> Self {
        let handlers = build_handler_map(&[
            (0, None, "MapManualLoadModuleMemory"),
            (1, None, "UnmapManualLoadModuleMemory"),
            (2, None, "RegisterModuleInfo"),
            (3, None, "UnregisterModuleInfo"),
            (4, None, "RegisterProcessHandle"),
            (10, None, "RegisterProcessModuleInfo"),
        ]);

        Self {
            context_id: INVALID_CONTEXT_ID,
            nrr_kind,
            name: name.to_string(),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for RoInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str { &self.name }
}

impl ServiceFramework for RoInterface {
    fn get_service_name(&self) -> &str { &self.name }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> { &self.handlers_tipc }
}

/// LoopProcess — registers "ldr:ro" and "ro:1" services.
///
/// Corresponds to `Service::RO::LoopProcess` in upstream ro.cpp.
pub fn loop_process() {
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    let mut server_manager = ServerManager::new(crate::core::SystemRef::null());

    let stub_names = &["ldr:ro", "ro:1"];
    for &name in stub_names {
        let svc_name = name.to_string();
        server_manager.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(crate::hle::service::services::GenericStubService::new(&svc_name))
            }),
            16,
        );
    }

    ServerManager::run_server(server_manager);
}
