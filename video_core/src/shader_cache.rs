// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/shader_cache.h and video_core/shader_cache.cpp
//!
//! Shader binary caching and invalidation.

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use parking_lot::Mutex;

use crate::control::channel_state::ChannelState;
use crate::control::channel_state_cache::{ChannelInfo, ChannelSetupCaches};
use crate::engines::kepler_compute::KeplerCompute;
use crate::engines::maxwell_3d::{Maxwell3D, ShaderStageType};
use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
use crate::shader_environment::{ComputeEnvironment, GenericEnvironment, GraphicsEnvironment};
use shader_recompiler::frontend::instruction::{Instruction as MaxwellInstruction, Predicate};
use shader_recompiler::frontend::maxwell_opcodes::{decode_opcode, MaxwellOpcode};
use shader_recompiler::ir::flow_test::FlowTest;

/// Virtual address type.
pub type VAddr = u64;

const YUZU_PAGEBITS: u64 = 14;
const YUZU_PAGESIZE: u64 = 1 << YUZU_PAGEBITS;
const NUM_PROGRAMS: usize = 6;
const SHADER_INSTRUCTION_SIZE: u32 = 8;

static REFRESH_STAGES_LAST_STAGE: AtomicU64 = AtomicU64::new(0);
static REFRESH_STAGES_COUNTS: [AtomicU64; 16] = [
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
];
static MAKE_SHADER_INFO_LAST_STAGE: AtomicU64 = AtomicU64::new(0);
static MAKE_SHADER_INFO_COUNTS: [AtomicU64; 10] = [
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
];
static SHADER_REGISTER_LAST_STAGE: AtomicU64 = AtomicU64::new(0);
static SHADER_REGISTER_COUNTS: [AtomicU64; 10] = [
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
];

fn record_refresh_stages_stage(stage: usize) {
    if std::env::var_os("RUZU_PROFILE_REFRESH_STAGES_STALL").is_none() {
        return;
    }
    REFRESH_STAGES_LAST_STAGE.store(stage as u64, Ordering::Relaxed);
    if let Some(counter) = REFRESH_STAGES_COUNTS.get(stage) {
        counter.fetch_add(1, Ordering::Relaxed);
    }
}

pub fn dump_refresh_stages_stall_profile() {
    if REFRESH_STAGES_COUNTS[0].load(Ordering::Relaxed) == 0 {
        return;
    }
    const NAMES: [&str; 16] = [
        "enter",
        "after_channel",
        "after_maxwell_ptr",
        "before_dirty_gate",
        "after_dirty_gate",
        "after_gpu_memory",
        "after_stage_snapshot",
        "stage_loop",
        "before_gpu_to_cpu",
        "after_gpu_to_cpu",
        "before_try_get",
        "after_try_get",
        "before_make_shader_info",
        "after_make_shader_info",
        "loop_done",
        "exit",
    ];
    let last_stage = REFRESH_STAGES_LAST_STAGE.load(Ordering::Relaxed) as usize;
    let last_stage_name = NAMES.get(last_stage).copied().unwrap_or("unknown");
    eprintln!(
        "[REFRESH_STAGES_STALL_PROFILE] last_stage={} ({})",
        last_stage, last_stage_name
    );
    for (index, name) in NAMES.iter().enumerate() {
        eprintln!(
            "[REFRESH_STAGES_STALL_PROFILE]   {:02} {:<24} {}",
            index,
            name,
            REFRESH_STAGES_COUNTS[index].load(Ordering::Relaxed)
        );
    }
}

fn record_make_shader_info_stage(stage: usize) {
    if std::env::var_os("RUZU_PROFILE_MAKE_SHADER_INFO_STALL").is_none() {
        return;
    }
    MAKE_SHADER_INFO_LAST_STAGE.store(stage as u64, Ordering::Relaxed);
    if let Some(counter) = MAKE_SHADER_INFO_COUNTS.get(stage) {
        counter.fetch_add(1, Ordering::Relaxed);
    }
}

pub fn dump_make_shader_info_stall_profile() {
    if MAKE_SHADER_INFO_COUNTS[0].load(Ordering::Relaxed) == 0 {
        return;
    }
    const NAMES: [&str; 10] = [
        "enter",
        "before_analyze",
        "after_analyze",
        "before_walk_cfg",
        "after_walk_cfg",
        "after_calculate_hash",
        "after_read_size",
        "before_register",
        "after_register",
        "exit",
    ];
    let last_stage = MAKE_SHADER_INFO_LAST_STAGE.load(Ordering::Relaxed) as usize;
    let last_stage_name = NAMES.get(last_stage).copied().unwrap_or("unknown");
    eprintln!(
        "[MAKE_SHADER_INFO_STALL_PROFILE] last_stage={} ({})",
        last_stage, last_stage_name
    );
    for (index, name) in NAMES.iter().enumerate() {
        eprintln!(
            "[MAKE_SHADER_INFO_STALL_PROFILE]   {:02} {:<24} {}",
            index,
            name,
            MAKE_SHADER_INFO_COUNTS[index].load(Ordering::Relaxed)
        );
    }
}

fn record_shader_register_stage(stage: usize) {
    if std::env::var_os("RUZU_PROFILE_SHADER_REGISTER_STALL").is_none() {
        return;
    }
    SHADER_REGISTER_LAST_STAGE.store(stage as u64, Ordering::Relaxed);
    if let Some(counter) = SHADER_REGISTER_COUNTS.get(stage) {
        counter.fetch_add(1, Ordering::Relaxed);
    }
}

pub fn dump_shader_register_stall_profile() {
    if SHADER_REGISTER_COUNTS[0].load(Ordering::Relaxed) == 0 {
        return;
    }
    const NAMES: [&str; 10] = [
        "enter",
        "before_invalidation_lock",
        "after_invalidation_lock",
        "before_lookup_lock",
        "after_lookup_lock",
        "after_new_entry",
        "after_invalidation_pages",
        "after_storage_push",
        "after_update_pages_cached",
        "exit",
    ];
    let last_stage = SHADER_REGISTER_LAST_STAGE.load(Ordering::Relaxed) as usize;
    let last_stage_name = NAMES.get(last_stage).copied().unwrap_or("unknown");
    eprintln!(
        "[SHADER_REGISTER_STALL_PROFILE] last_stage={} ({})",
        last_stage, last_stage_name
    );
    for (index, name) in NAMES.iter().enumerate() {
        eprintln!(
            "[SHADER_REGISTER_STALL_PROFILE]   {:02} {:<28} {}",
            index,
            name,
            SHADER_REGISTER_COUNTS[index].load(Ordering::Relaxed)
        );
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum FlowStackToken {
    Ssy,
    Pbk,
    Pexit,
    Pret,
    Pcnt,
    Plongjmp,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct FlowStackEntry {
    token: FlowStackToken,
    target: u32,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct PendingShaderFlow {
    pc: u32,
    stack: Vec<FlowStackEntry>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BranchDisposition {
    AlwaysTaken,
    NeverTaken,
    Conditional,
}

/// Information about a compiled shader.
#[derive(Debug, Default)]
pub struct ShaderInfo {
    pub unique_hash: u64,
    pub size_bytes: usize,
}

/// An entry in the shader lookup cache.
struct Entry {
    addr_start: VAddr,
    addr_end: VAddr,
    data: *mut ShaderInfo,
    is_memory_marked: bool,
}

impl Entry {
    fn overlaps(&self, start: VAddr, end: VAddr) -> bool {
        start < self.addr_end && self.addr_start < end
    }
}

/// Shader cache that tracks compiled shaders by their guest memory address.
///
/// Handles invalidation when guest memory is modified.
pub struct ShaderCache {
    /// Shared `MaxwellDeviceMemoryManager` instance — same `Arc` is held
    /// by `Host1x::memory_manager`, the buffer cache, and the texture
    /// cache. Mirrors upstream's `MaxwellDeviceMemoryManager& device_memory`
    /// reference member.
    device_memory: Arc<MaxwellDeviceMemoryManager>,
    channel_caches: ChannelSetupCaches<ChannelInfo>,
    lookup_mutex: Mutex<()>,
    invalidation_mutex: Mutex<()>,
    lookup_cache: HashMap<u64, Box<Entry>>,
    invalidation_cache: HashMap<u64, Vec<*mut Entry>>,
    storage: Vec<Box<ShaderInfo>>,
    marked_for_removal: Vec<*mut Entry>,
    shader_infos: [Option<*const ShaderInfo>; NUM_PROGRAMS],
    last_shaders_valid: bool,
}

// Safety: Entry pointers are only used within locked sections.
unsafe impl Send for ShaderCache {}
unsafe impl Sync for ShaderCache {}

pub struct GraphicsEnvironments {
    pub envs: [GraphicsEnvironment; NUM_PROGRAMS],
    pub env_ptrs: [Option<usize>; NUM_PROGRAMS],
}

impl GraphicsEnvironments {
    pub fn span(&self) -> Vec<&GenericEnvironment> {
        self.env_ptrs
            .iter()
            .flatten()
            .map(|&index| self.envs[index].generic_environment())
            .collect()
    }
}

impl Default for GraphicsEnvironments {
    fn default() -> Self {
        Self {
            envs: std::array::from_fn(|_| GraphicsEnvironment::default()),
            env_ptrs: [None; NUM_PROGRAMS],
        }
    }
}

fn shader_stage_type_from_index(index: usize) -> ShaderStageType {
    match index {
        0 => ShaderStageType::VertexA,
        1 => ShaderStageType::VertexB,
        2 => ShaderStageType::TessInit,
        3 => ShaderStageType::Tessellation,
        4 => ShaderStageType::Geometry,
        5 => ShaderStageType::Fragment,
        _ => ShaderStageType::Invalid,
    }
}

impl ShaderCache {
    /// Port of upstream `ShaderCache::ShaderCache(MaxwellDeviceMemoryManager& device_memory_)`.
    /// Takes a shared `Arc` rather than a reference: the same instance is
    /// held by `Host1x`, the buffer cache, and the texture cache.
    pub fn new(device_memory: Arc<MaxwellDeviceMemoryManager>) -> Self {
        Self {
            device_memory,
            channel_caches: ChannelSetupCaches::new(),
            lookup_mutex: Mutex::new(()),
            invalidation_mutex: Mutex::new(()),
            lookup_cache: HashMap::new(),
            invalidation_cache: HashMap::new(),
            storage: Vec::new(),
            marked_for_removal: Vec::new(),
            shader_infos: [None; NUM_PROGRAMS],
            last_shaders_valid: false,
        }
    }

    /// Access the shared `MaxwellDeviceMemoryManager`. Same `Arc` as
    /// `Host1x::memory_manager()`.
    pub fn device_memory(&self) -> &Arc<MaxwellDeviceMemoryManager> {
        &self.device_memory
    }

    /// Port of the shared `ShaderCache` channel-owner `CreateChannel` edge.
    pub fn create_channel(&mut self, channel: &ChannelState) {
        self.channel_caches.create_channel(channel);
    }

    /// Port of the shared `ShaderCache` channel-owner `BindToChannel` edge.
    pub fn bind_to_channel(&mut self, channel_id: i32) {
        self.channel_caches.bind_to_channel(channel_id);
    }

    /// Port of the shared `ShaderCache` channel-owner `EraseChannel` edge.
    pub fn erase_channel(&mut self, channel_id: i32) {
        self.channel_caches.erase_channel(channel_id);
    }

    /// Reduced Rust accessor for the currently bound shared channel owner.
    pub fn current_channel_info(&self) -> Option<&ChannelInfo> {
        self.channel_caches.current_channel_state()
    }

    /// Reduced Rust accessor for the shared shader-stage cache state.
    pub fn last_shaders_valid(&self) -> bool {
        self.last_shaders_valid
    }

    /// Reduced Rust accessor for the shared shader-info owner slots.
    pub fn shader_info_slots(&self) -> &[Option<*const ShaderInfo>; NUM_PROGRAMS] {
        &self.shader_infos
    }

    /// Port of `ShaderCache::RefreshStages`.
    pub fn refresh_stages(&mut self, unique_hashes: &mut [u64; NUM_PROGRAMS]) -> bool {
        record_refresh_stages_stage(0);
        let Some(channel) = self.current_channel_info() else {
            self.last_shaders_valid = false;
            return false;
        };
        record_refresh_stages_stage(1);
        let maxwell_ptr = channel.maxwell3d as *mut Maxwell3D;
        if maxwell_ptr.is_null() {
            self.last_shaders_valid = false;
            return false;
        }
        record_refresh_stages_stage(2);
        let maxwell3d = unsafe { &mut *maxwell_ptr };
        record_refresh_stages_stage(3);
        if !maxwell3d.consume_dirty_shaders() {
            return self.last_shaders_valid;
        }
        record_refresh_stages_stage(4);
        let Some(gpu_memory) = channel.gpu_memory.as_ref().map(Arc::clone) else {
            self.last_shaders_valid = false;
            return false;
        };
        if maxwell3d.memory_manager().is_none() {
            self.last_shaders_valid = false;
            return false;
        }
        record_refresh_stages_stage(5);

        let base_addr = maxwell3d.program_region_address();
        let rasterize_enable = maxwell3d.rasterize_enable();
        let stage_infos: [crate::engines::maxwell_3d::ShaderStageInfo; NUM_PROGRAMS] =
            std::array::from_fn(|index| maxwell3d.shader_stage_info(index as u32));
        let stage_enabled: [bool; NUM_PROGRAMS] =
            std::array::from_fn(|index| maxwell3d.is_shader_stage_enabled(index as u32));
        record_refresh_stages_stage(6);
        for (index, unique_hash) in unique_hashes.iter_mut().enumerate() {
            record_refresh_stages_stage(7);
            let stage_info = stage_infos[index];
            let program_type = shader_stage_type_from_index(index);
            if !stage_enabled[index] {
                *unique_hash = 0;
                self.shader_infos[index] = None;
                continue;
            }
            if program_type == ShaderStageType::Fragment && !rasterize_enable {
                *unique_hash = 0;
                self.shader_infos[index] = None;
                continue;
            }

            if std::env::var_os("RUZU_TRACE_SHADER_WORDS").is_some() {
                eprintln!(
                    "[SHADER_STAGE_INFO] index={} program={:?} reg_program={:?} offset=0x{:X} base=0x{:X}",
                    index, program_type, stage_info.program_type, stage_info.offset, base_addr
                );
            }

            let shader_addr = base_addr + stage_info.offset as u64;
            record_refresh_stages_stage(8);
            let cpu_shader_addr = {
                let memory = gpu_memory.lock();
                memory.gpu_to_cpu_address(shader_addr)
            };
            record_refresh_stages_stage(9);
            if std::env::var_os("RUZU_TRACE_SHADER_WORDS").is_some() {
                match cpu_shader_addr {
                    Some(cpu) => {
                        eprintln!("[SHADER_VA] gpu=0x{:X} -> cpu=0x{:X}", shader_addr, cpu)
                    }
                    None => eprintln!(
                        "[SHADER_VA] gpu=0x{:X} -> UNMAPPED (gpu_to_cpu_address returned None)",
                        shader_addr
                    ),
                }
            }
            let Some(cpu_shader_addr) = cpu_shader_addr else {
                self.last_shaders_valid = false;
                return false;
            };

            record_refresh_stages_stage(10);
            let shader_ptr = if let Some(shader_info) = self.try_get(cpu_shader_addr) {
                record_refresh_stages_stage(11);
                shader_info as *const ShaderInfo
            } else {
                record_refresh_stages_stage(11);
                if program_type == ShaderStageType::Invalid {
                    *unique_hash = 0;
                    self.shader_infos[index] = None;
                    continue;
                }
                let mut env = GraphicsEnvironment::from_maxwell3d(
                    maxwell3d,
                    program_type,
                    base_addr,
                    stage_info.offset,
                );
                record_refresh_stages_stage(12);
                self.make_shader_info(env.generic_environment_mut(), cpu_shader_addr)
                    as *const ShaderInfo
            };
            record_refresh_stages_stage(13);

            let shader_info = unsafe { &*shader_ptr };
            self.shader_infos[index] = Some(shader_ptr);
            *unique_hash = shader_info.unique_hash;
        }

        record_refresh_stages_stage(14);
        self.last_shaders_valid = true;
        record_refresh_stages_stage(15);
        true
    }

    /// Port of `ShaderCache::ComputeShader`.
    pub fn compute_shader(&mut self) -> Option<&ShaderInfo> {
        let channel = self.current_channel_info()?;
        let kepler_ptr = channel.kepler_compute as *const KeplerCompute;
        if kepler_ptr.is_null() {
            return None;
        }
        let gpu_memory = channel.gpu_memory.as_ref().map(Arc::clone)?;
        let kepler_compute = unsafe { &*kepler_ptr };

        let program_base = kepler_compute.code_address();
        let qmd = kepler_compute.launch_description();
        let shader_addr = program_base + qmd.program_start as u64;
        let cpu_shader_addr = {
            let memory = gpu_memory.lock();
            memory.gpu_to_cpu_address(shader_addr)?
        };
        if let Some(shader_ptr) = self
            .try_get(cpu_shader_addr)
            .map(|shader| shader as *const _)
        {
            return Some(unsafe { &*shader_ptr });
        }

        let mut env =
            ComputeEnvironment::from_kepler_compute(kepler_compute, Arc::clone(&gpu_memory));
        Some(self.make_shader_info(env.generic_environment_mut(), cpu_shader_addr))
    }

    /// Port of `ShaderCache::GetGraphicsEnvironments`.
    pub fn get_graphics_environments(
        &self,
        result: &mut GraphicsEnvironments,
        unique_hashes: &[u64; NUM_PROGRAMS],
    ) {
        result.env_ptrs = [None; NUM_PROGRAMS];

        let Some(maxwell3d) = self.current_maxwell3d() else {
            return;
        };
        if maxwell3d.memory_manager().is_none() {
            return;
        }
        let base_addr = maxwell3d.program_region_address();
        let mut env_index = 0usize;

        for (index, unique_hash) in unique_hashes.iter().enumerate() {
            if *unique_hash == 0 {
                continue;
            }
            let Some(shader_ptr) = self.shader_infos[index] else {
                continue;
            };
            let stage_info = maxwell3d.shader_stage_info(index as u32);
            let program_type = shader_stage_type_from_index(index);
            if program_type == ShaderStageType::Invalid {
                continue;
            }

            let shader_info = unsafe { &*shader_ptr };
            let mut env = GraphicsEnvironment::from_maxwell3d(
                maxwell3d,
                program_type,
                base_addr,
                stage_info.offset,
            );
            env.set_cached_size(shader_info.size_bytes);
            result.envs[index] = env;
            result.env_ptrs[env_index] = Some(index);
            env_index += 1;
        }
    }

    /// Removes shaders inside a given region.
    pub fn invalidate_region(&mut self, addr: VAddr, size: usize) {
        // Port of `ShaderCache::InvalidateRegion`: upstream takes
        // `invalidation_mutex` before touching invalidation_cache and
        // marked_for_removal. Ruzu reaches this object through raw rasterizer
        // pointers from multiple CPU threads, so `&mut self` is not enough to
        // serialize the host HashMaps.
        let invalidation_mutex: *const Mutex<()> = &self.invalidation_mutex;
        let _invalidation_guard = unsafe { (*invalidation_mutex).lock() };
        self.invalidate_pages_in_region(addr, size);
        self.remove_pending_shaders();
    }

    /// Unmarks a memory region as cached and marks it for removal.
    pub fn on_cache_invalidation(&mut self, addr: VAddr, size: usize) {
        // Port of `ShaderCache::OnCacheInvalidation`.
        let invalidation_mutex: *const Mutex<()> = &self.invalidation_mutex;
        let _invalidation_guard = unsafe { (*invalidation_mutex).lock() };
        self.invalidate_pages_in_region(addr, size);
    }

    /// Flushes delayed removal operations.
    pub fn sync_guest_host(&mut self) {
        // Port of `ShaderCache::SyncGuestHost`.
        let invalidation_mutex: *const Mutex<()> = &self.invalidation_mutex;
        let _invalidation_guard = unsafe { (*invalidation_mutex).lock() };
        self.remove_pending_shaders();
    }

    /// Port of `ShaderCache::Register`.
    pub fn register(&mut self, data: Box<ShaderInfo>, addr: VAddr, size: usize) {
        // Upstream takes both mutexes here:
        // `std::scoped_lock lock{invalidation_mutex, lookup_mutex}`.
        record_shader_register_stage(0);
        let invalidation_mutex: *const Mutex<()> = &self.invalidation_mutex;
        let lookup_mutex: *const Mutex<()> = &self.lookup_mutex;
        record_shader_register_stage(1);
        let _invalidation_guard = unsafe { (*invalidation_mutex).lock() };
        record_shader_register_stage(2);
        record_shader_register_stage(3);
        let _lookup_guard = unsafe { (*lookup_mutex).lock() };
        record_shader_register_stage(4);

        let addr_end = addr + size as u64;
        let data_ptr = (&*data as *const ShaderInfo).cast_mut();
        let entry = self.new_entry(addr, addr_end, data_ptr);
        record_shader_register_stage(5);

        let page_end = (addr_end + YUZU_PAGESIZE - 1) >> YUZU_PAGEBITS;
        for page in (addr >> YUZU_PAGEBITS)..page_end {
            self.invalidation_cache.entry(page).or_default().push(entry);
        }
        record_shader_register_stage(6);

        self.storage.push(data);
        record_shader_register_stage(7);
        self.device_memory.update_pages_cached_count(addr, size, 1);
        record_shader_register_stage(8);
        record_shader_register_stage(9);
    }

    fn invalidate_pages_in_region(&mut self, addr: VAddr, size: usize) {
        let addr_end = addr + size as u64;
        let page_end = (addr_end + YUZU_PAGESIZE - 1) >> YUZU_PAGEBITS;
        for page in (addr >> YUZU_PAGEBITS)..page_end {
            self.invalidate_page_entries(page, addr, addr_end);
        }
    }

    fn remove_pending_shaders(&mut self) {
        if self.marked_for_removal.is_empty() {
            return;
        }

        // Remove duplicates (port of std::ranges::sort + std::unique in upstream).
        self.marked_for_removal.sort_by_key(|p| *p as usize);
        self.marked_for_removal.dedup();

        let mut removed_shaders: Vec<*mut ShaderInfo> = Vec::new();

        // Upstream `RemovePendingShaders` takes lookup_mutex while removing
        // entries from lookup_cache. Callers already hold invalidation_mutex.
        let lookup_mutex: *const Mutex<()> = &self.lookup_mutex;
        let _lookup_guard = unsafe { (*lookup_mutex).lock() };

        for &entry_ptr in &self.marked_for_removal {
            let entry = unsafe { &*entry_ptr };
            removed_shaders.push(entry.data);

            // Remove from lookup cache.
            self.lookup_cache.remove(&entry.addr_start);
        }
        self.marked_for_removal.clear();

        // Remove from storage (port of RemoveShadersFromStorage).
        if !removed_shaders.is_empty() {
            self.remove_shaders_from_storage(&removed_shaders);
        }
    }

    /// Port of `ShaderCache::InvalidatePageEntries`.
    fn invalidate_page_entries(&mut self, page: u64, addr: VAddr, addr_end: VAddr) {
        loop {
            let Some(entry_ptr) = self.invalidation_cache.get(&page).and_then(|entries| {
                entries
                    .iter()
                    .copied()
                    .find(|&entry| unsafe { (*entry).overlaps(addr, addr_end) })
            }) else {
                break;
            };
            let entry = unsafe { &mut *entry_ptr };
            self.unmark_memory(entry);
            self.remove_entry_from_invalidation_cache(entry);
            self.marked_for_removal.push(entry_ptr);
        }
    }

    /// Port of `ShaderCache::RemoveEntryFromInvalidationCache`.
    fn remove_entry_from_invalidation_cache(&mut self, entry: &Entry) {
        let page_end = (entry.addr_end + YUZU_PAGESIZE - 1) >> YUZU_PAGEBITS;
        for page in (entry.addr_start >> YUZU_PAGEBITS)..page_end {
            let Some(entries) = self.invalidation_cache.get_mut(&page) else {
                continue;
            };
            if let Some(position) = entries
                .iter()
                .position(|existing| std::ptr::eq(*existing, entry))
            {
                entries.remove(position);
            }
        }
    }

    /// Port of `ShaderCache::UnmarkMemory`.
    fn unmark_memory(&mut self, entry: &mut Entry) {
        if !entry.is_memory_marked {
            return;
        }
        entry.is_memory_marked = false;
        self.device_memory.update_pages_cached_count(
            entry.addr_start,
            (entry.addr_end - entry.addr_start) as usize,
            -1,
        );
    }

    /// Port of `ShaderCache::RemoveShadersFromStorage`.
    fn remove_shaders_from_storage(&mut self, removed_shaders: &[*mut ShaderInfo]) {
        self.storage.retain(|shader| {
            let ptr: *mut ShaderInfo = (&**shader as *const ShaderInfo).cast_mut();
            !removed_shaders.contains(&ptr)
        });
    }

    /// Port of `ShaderCache::NewEntry`.
    fn new_entry(&mut self, addr: VAddr, addr_end: VAddr, data: *mut ShaderInfo) -> *mut Entry {
        let mut entry = Box::new(Entry {
            addr_start: addr,
            addr_end,
            data,
            is_memory_marked: true,
        });
        let entry_ptr: *mut Entry = &mut *entry;
        self.lookup_cache.insert(addr, entry);
        entry_ptr
    }

    /// Try to get a cached shader at the given address.
    pub fn try_get(&self, addr: VAddr) -> Option<&ShaderInfo> {
        let _lock = self.lookup_mutex.lock();
        self.lookup_cache
            .get(&addr)
            .map(|entry| unsafe { &*entry.data })
    }

    /// Port of `ShaderCache::MakeShaderInfo`.
    pub fn make_shader_info(
        &mut self,
        env: &mut GenericEnvironment,
        cpu_addr: VAddr,
    ) -> &ShaderInfo {
        record_make_shader_info_stage(0);
        let mut info = Box::new(ShaderInfo::default());
        record_make_shader_info_stage(1);
        if let Some(cached_hash) = env.analyze() {
            record_make_shader_info_stage(2);
            info.unique_hash = cached_hash;
            info.size_bytes = env.cached_size_bytes();
        } else {
            record_make_shader_info_stage(2);
            record_make_shader_info_stage(3);
            self.walk_shader_control_flow(env);
            record_make_shader_info_stage(4);
            info.unique_hash = env.calculate_hash();
            record_make_shader_info_stage(5);
            info.size_bytes = env.read_size_bytes();
            record_make_shader_info_stage(6);
        }
        let size_bytes = info.size_bytes;
        record_make_shader_info_stage(7);
        self.register(info, cpu_addr, size_bytes);
        record_make_shader_info_stage(8);
        let result = self
            .try_get(cpu_addr)
            .expect("registered shader info must be reachable through lookup cache");
        record_make_shader_info_stage(9);
        result
    }

    pub fn current_maxwell3d(&self) -> Option<&Maxwell3D> {
        let channel = self.current_channel_info()?;
        let ptr = channel.maxwell3d as *const Maxwell3D;
        if ptr.is_null() {
            None
        } else {
            Some(unsafe { &*ptr })
        }
    }

    fn current_kepler_compute(&self) -> Option<&KeplerCompute> {
        let channel = self.current_channel_info()?;
        let ptr = channel.kepler_compute as *const KeplerCompute;
        if ptr.is_null() {
            None
        } else {
            Some(unsafe { &*ptr })
        }
    }

    fn current_gpu_memory(
        &self,
    ) -> Option<Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>> {
        self.current_channel_info()?
            .gpu_memory
            .as_ref()
            .map(Arc::clone)
    }

    fn walk_shader_control_flow(&self, env: &mut GenericEnvironment) {
        let mut pending = vec![PendingShaderFlow {
            pc: env.start_address(),
            stack: Vec::new(),
        }];
        let mut visited = HashSet::new();

        while let Some(mut state) = pending.pop() {
            loop {
                if !visited.insert(state.clone()) {
                    break;
                }

                let pc = state.pc;
                let insn_raw = env.read_instruction(pc);
                let insn = MaxwellInstruction::new(insn_raw);
                let opcode = decode_opcode(insn_raw);
                let disposition =
                    Self::branch_disposition(opcode, insn.pred(), insn.branch_flow_test());
                let next_pc = pc.wrapping_add(SHADER_INSTRUCTION_SIZE);

                match opcode {
                    Some(MaxwellOpcode::SSY) => {
                        state.stack.push(FlowStackEntry {
                            token: FlowStackToken::Ssy,
                            target: Self::relative_branch_target(pc, insn),
                        });
                        state.pc = next_pc;
                    }
                    Some(MaxwellOpcode::PBK) => {
                        state.stack.push(FlowStackEntry {
                            token: FlowStackToken::Pbk,
                            target: Self::relative_branch_target(pc, insn),
                        });
                        state.pc = next_pc;
                    }
                    Some(MaxwellOpcode::PCNT) => {
                        state.stack.push(FlowStackEntry {
                            token: FlowStackToken::Pcnt,
                            target: Self::relative_branch_target(pc, insn),
                        });
                        state.pc = next_pc;
                    }
                    Some(MaxwellOpcode::CAL) => {
                        let mut callee_stack = state.stack.clone();
                        callee_stack.push(FlowStackEntry {
                            token: FlowStackToken::Pret,
                            target: next_pc,
                        });
                        pending.push(PendingShaderFlow {
                            pc: Self::relative_branch_target(pc, insn),
                            stack: callee_stack,
                        });
                        state.pc = next_pc;
                    }
                    Some(MaxwellOpcode::JCAL) => {
                        let mut callee_stack = state.stack.clone();
                        callee_stack.push(FlowStackEntry {
                            token: FlowStackToken::Pret,
                            target: next_pc,
                        });
                        pending.push(PendingShaderFlow {
                            pc: insn.branch_absolute(),
                            stack: callee_stack,
                        });
                        state.pc = next_pc;
                    }
                    Some(MaxwellOpcode::BRA) => {
                        let target = Self::relative_branch_target(pc, insn);
                        if disposition != BranchDisposition::AlwaysTaken {
                            pending.push(PendingShaderFlow {
                                pc: next_pc,
                                stack: state.stack.clone(),
                            });
                        }
                        if disposition == BranchDisposition::NeverTaken {
                            state.pc = next_pc;
                        } else {
                            state.pc = target;
                        }
                    }
                    Some(MaxwellOpcode::JMP) => {
                        let target = insn.branch_absolute();
                        if disposition != BranchDisposition::AlwaysTaken {
                            pending.push(PendingShaderFlow {
                                pc: next_pc,
                                stack: state.stack.clone(),
                            });
                        }
                        if disposition == BranchDisposition::NeverTaken {
                            state.pc = next_pc;
                        } else {
                            state.pc = target;
                        }
                    }
                    Some(MaxwellOpcode::SYNC) => {
                        if !Self::continue_from_stack_target(
                            &mut pending,
                            &mut state,
                            next_pc,
                            FlowStackToken::Ssy,
                            disposition,
                        ) {
                            break;
                        }
                    }
                    Some(MaxwellOpcode::BRK) => {
                        if !Self::continue_from_stack_target(
                            &mut pending,
                            &mut state,
                            next_pc,
                            FlowStackToken::Pbk,
                            disposition,
                        ) {
                            break;
                        }
                    }
                    Some(MaxwellOpcode::CONT) => {
                        if !Self::continue_from_stack_target(
                            &mut pending,
                            &mut state,
                            next_pc,
                            FlowStackToken::Pcnt,
                            disposition,
                        ) {
                            break;
                        }
                    }
                    Some(MaxwellOpcode::RET) | Some(MaxwellOpcode::PRET) => {
                        if !Self::continue_from_stack_target(
                            &mut pending,
                            &mut state,
                            next_pc,
                            FlowStackToken::Pret,
                            disposition,
                        ) {
                            break;
                        }
                    }
                    Some(MaxwellOpcode::EXIT) | Some(MaxwellOpcode::PEXIT) => {
                        if !Self::continue_from_stack_target(
                            &mut pending,
                            &mut state,
                            next_pc,
                            FlowStackToken::Pexit,
                            disposition,
                        ) {
                            break;
                        }
                    }
                    Some(MaxwellOpcode::LONGJMP) | Some(MaxwellOpcode::PLONGJMP) => {
                        if !Self::continue_from_stack_target(
                            &mut pending,
                            &mut state,
                            next_pc,
                            FlowStackToken::Plongjmp,
                            disposition,
                        ) {
                            break;
                        }
                    }
                    Some(MaxwellOpcode::KIL)
                    | Some(MaxwellOpcode::BRX)
                    | Some(MaxwellOpcode::JMX) => {
                        if disposition != BranchDisposition::AlwaysTaken {
                            pending.push(PendingShaderFlow {
                                pc: next_pc,
                                stack: state.stack.clone(),
                            });
                        }
                        break;
                    }
                    _ => {
                        state.pc = next_pc;
                    }
                }
            }
        }
    }

    fn relative_branch_target(pc: u32, insn: MaxwellInstruction) -> u32 {
        pc.wrapping_add_signed(insn.branch_offset())
            .wrapping_add(SHADER_INSTRUCTION_SIZE)
    }

    fn branch_disposition(
        opcode: Option<MaxwellOpcode>,
        pred: Predicate,
        flow_test: Option<FlowTest>,
    ) -> BranchDisposition {
        let predicate_state = if pred.index == 7 {
            if pred.negated {
                BranchDisposition::NeverTaken
            } else {
                BranchDisposition::AlwaysTaken
            }
        } else {
            BranchDisposition::Conditional
        };

        let Some(opcode) = opcode else {
            return predicate_state;
        };
        let flow_state = if Self::opcode_has_flow_test(opcode) {
            match flow_test {
                Some(FlowTest::T) => BranchDisposition::AlwaysTaken,
                Some(FlowTest::F) => BranchDisposition::NeverTaken,
                Some(_) | None => BranchDisposition::Conditional,
            }
        } else {
            BranchDisposition::AlwaysTaken
        };

        match (predicate_state, flow_state) {
            (BranchDisposition::NeverTaken, _) | (_, BranchDisposition::NeverTaken) => {
                BranchDisposition::NeverTaken
            }
            (BranchDisposition::AlwaysTaken, BranchDisposition::AlwaysTaken) => {
                BranchDisposition::AlwaysTaken
            }
            _ => BranchDisposition::Conditional,
        }
    }

    fn opcode_has_flow_test(opcode: MaxwellOpcode) -> bool {
        matches!(
            opcode,
            MaxwellOpcode::BRA
                | MaxwellOpcode::BRX
                | MaxwellOpcode::EXIT
                | MaxwellOpcode::JMP
                | MaxwellOpcode::JMX
                | MaxwellOpcode::KIL
                | MaxwellOpcode::BRK
                | MaxwellOpcode::CONT
                | MaxwellOpcode::LONGJMP
                | MaxwellOpcode::PRET
                | MaxwellOpcode::RET
                | MaxwellOpcode::SYNC
        )
    }

    fn continue_from_stack_target(
        pending: &mut Vec<PendingShaderFlow>,
        state: &mut PendingShaderFlow,
        next_pc: u32,
        token: FlowStackToken,
        disposition: BranchDisposition,
    ) -> bool {
        if disposition != BranchDisposition::AlwaysTaken {
            pending.push(PendingShaderFlow {
                pc: next_pc,
                stack: state.stack.clone(),
            });
        }

        if disposition == BranchDisposition::NeverTaken {
            state.pc = next_pc;
            return true;
        }

        if let Some((target, new_stack)) = Self::pop_flow_stack_target(&state.stack, token) {
            state.pc = target;
            state.stack = new_stack;
            true
        } else {
            false
        }
    }

    fn pop_flow_stack_target(
        stack: &[FlowStackEntry],
        token: FlowStackToken,
    ) -> Option<(u32, Vec<FlowStackEntry>)> {
        let index = stack.iter().rposition(|entry| entry.token == token)?;
        let mut new_stack = Vec::with_capacity(stack.len().saturating_sub(1));
        new_stack.extend_from_slice(&stack[..index]);
        new_stack.extend_from_slice(&stack[index + 1..]);
        Some((stack[index].target, new_stack))
    }
}

impl Default for ShaderCache {
    /// Convenience default: fresh empty `MaxwellDeviceMemoryManager`.
    /// Production code constructs `ShaderCache` from
    /// `Host1x::memory_manager()`. Used by tests and standalone benches
    /// that don't need cross-cache invalidation.
    fn default() -> Self {
        Self::new(Arc::new(MaxwellDeviceMemoryManager::default()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engines::engine_interface::EngineInterface;
    use crate::engines::kepler_compute::QueueMetaData;
    use crate::engines::maxwell_3d::Maxwell3D;
    use crate::memory_manager::MemoryManager;
    use crate::shader_environment::GenericEnvironment;
    use parking_lot::Mutex as ParkingLotMutex;
    use std::sync::Arc;

    fn make_cpu_reader(
        cpu_base: u64,
        backing: Arc<Vec<u8>>,
    ) -> Arc<dyn Fn(u64, &mut [u8]) + Send + Sync> {
        Arc::new(move |cpu_addr: u64, dst: &mut [u8]| {
            dst.fill(0);
            let offset = cpu_addr.saturating_sub(cpu_base) as usize;
            if offset >= backing.len() {
                return;
            }
            let available = backing.len() - offset;
            let count = available.min(dst.len());
            dst[..count].copy_from_slice(&backing[offset..offset + count]);
        })
    }

    #[test]
    fn shader_cache_starts_with_upstream_shared_state_defaults() {
        let cache = ShaderCache::default();
        assert!(cache.current_channel_info().is_none());
        assert_eq!(cache.shader_info_slots(), &[None; NUM_PROGRAMS]);
        assert!(!cache.last_shaders_valid());
    }

    #[test]
    fn shader_cache_channel_owner_tracks_bound_channel_info() {
        let mut cache = ShaderCache::default();
        let mut channel = ChannelState::new(7);
        channel.program_id = 0x1234;
        channel.memory_manager = Some(Arc::new(ParkingLotMutex::new(MemoryManager::new(0))));
        channel.maxwell_3d = Some(Box::default());
        channel.kepler_compute = Some(Box::default());

        cache.create_channel(&channel);
        cache.bind_to_channel(7);

        let info = cache
            .current_channel_info()
            .expect("channel should be bound into shared shader-cache owner");
        assert_eq!(info.program_id, 0x1234);
        assert_ne!(info.maxwell3d, 0);
        assert_ne!(info.kepler_compute, 0);
        assert!(info.gpu_memory.is_some());
    }

    #[test]
    fn register_creates_lookup_and_invalidation_entries() {
        let mut cache = ShaderCache::default();
        cache.register(
            Box::new(ShaderInfo {
                unique_hash: 0x1234,
                size_bytes: 0x200,
            }),
            0x4000,
            0x200,
        );

        let shader = cache
            .try_get(0x4000)
            .expect("registered shader should be cached");
        assert_eq!(shader.unique_hash, 0x1234);
        assert!(cache
            .invalidation_cache
            .values()
            .any(|entries| !entries.is_empty()));
    }

    #[test]
    fn invalidate_region_erases_current_page_entry_in_place() {
        let mut cache = ShaderCache::default();
        cache.register(
            Box::new(ShaderInfo {
                unique_hash: 0x1234,
                size_bytes: 0x40,
            }),
            0x4000,
            0x40,
        );

        assert!(cache.try_get(0x4000).is_some());
        assert_eq!(
            cache
                .invalidation_cache
                .get(&(0x4000 >> YUZU_PAGEBITS))
                .map(Vec::len),
            Some(1)
        );

        cache.invalidate_region(0x4000, 4);

        assert!(cache.try_get(0x4000).is_none());
        assert!(cache.marked_for_removal.is_empty());
        assert!(cache.storage.is_empty());
        assert_eq!(
            cache
                .invalidation_cache
                .get(&(0x4000 >> YUZU_PAGEBITS))
                .map(Vec::len),
            Some(0)
        );
    }

    #[test]
    fn make_shader_info_registers_analyzed_shader() {
        let program_base = 0x1_0000_0000;
        let sentinel_offset = 0x80usize;
        let sentinel = 0xE2400FFF00000F00u64;

        let mut backing = vec![0u8; 0x2000];
        backing[sentinel_offset..sentinel_offset + 8].copy_from_slice(&sentinel.to_le_bytes());
        let backing = Arc::new(backing);
        let reader = Arc::new(move |gpu_addr: u64, dst: &mut [u8]| {
            dst.fill(0);
            let offset = (gpu_addr - program_base) as usize;
            if offset >= backing.len() {
                return;
            }
            let available = backing.len() - offset;
            let count = available.min(dst.len());
            dst[..count].copy_from_slice(&backing[offset..offset + count]);
        });

        let mut env = GenericEnvironment::new()
            .with_gpu_read(reader)
            .with_program(program_base, 0);
        let _ = env.read_instruction(0);
        let mut cache = ShaderCache::default();

        let shader = cache.make_shader_info(&mut env, 0x9000);
        assert_ne!(shader.unique_hash, 0);
        assert!(shader.size_bytes >= 8);
        assert!(cache.try_get(0x9000).is_some());
    }

    #[test]
    fn make_shader_info_slow_path_walks_branch_target_before_hashing() {
        const BRA_TOP10: u64 = 0x324;
        const EXIT_TOP10: u64 = 0x34C;
        const PRED_PT: u64 = 7;
        const FLOW_T: u64 = 15;

        fn encode_control_flow(top10: u64, branch_offset: i32) -> u64 {
            (top10 << 54)
                | (((branch_offset as u32 as u64) & 0x00FF_FFFF) << 20)
                | (PRED_PT << 16)
                | FLOW_T
        }

        let program_base = 0x2_0000_0000;
        let mut backing = vec![0u8; 0x80];
        let bra = encode_control_flow(BRA_TOP10, 0x18);
        let exit = encode_control_flow(EXIT_TOP10, 0);
        backing[0x00..0x08].copy_from_slice(&bra.to_le_bytes());
        backing[0x20..0x28].copy_from_slice(&exit.to_le_bytes());
        let backing = Arc::new(backing);
        let reader = Arc::new(move |gpu_addr: u64, dst: &mut [u8]| {
            dst.fill(0);
            let offset = (gpu_addr - program_base) as usize;
            if offset >= backing.len() {
                return;
            }
            let available = backing.len() - offset;
            let count = available.min(dst.len());
            dst[..count].copy_from_slice(&backing[offset..offset + count]);
        });

        let mut env = GenericEnvironment::new()
            .with_gpu_read(reader)
            .with_program(program_base, 0);
        let mut cache = ShaderCache::default();

        let shader = cache.make_shader_info(&mut env, 0xA000);
        assert_ne!(shader.unique_hash, 0);
        assert_eq!(shader.size_bytes, 0x28);
    }

    #[test]
    fn refresh_stages_hashes_enabled_vertexb_shader_for_bound_channel() {
        let gpu_base = 0x1_0000_0000;
        let cpu_base = 0x2000;
        let mut backing = vec![0u8; 0x2000];
        backing[0x180..0x188].copy_from_slice(&0xE2400FFFFF87000Fu64.to_le_bytes());
        let backing = Arc::new(backing);

        let memory_manager = Arc::new(ParkingLotMutex::new(MemoryManager::new(0)));
        memory_manager
            .lock()
            .map(gpu_base, cpu_base, 0x2000, 0, false);

        let mut maxwell = Maxwell3D::new();
        maxwell.set_memory_manager(Arc::clone(&memory_manager));
        maxwell.set_guest_memory_reader(make_cpu_reader(cpu_base, Arc::clone(&backing)));
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x582, 1, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x583, 0, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x810, 1 | (1 << 4), true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x811, 0x100, true);

        let mut channel = ChannelState::new(7);
        channel.program_id = 0x1234;
        channel.memory_manager = Some(Arc::clone(&memory_manager));
        channel.maxwell_3d = Some(Box::new(maxwell));
        channel.kepler_compute = Some(Box::default());

        let mut cache = ShaderCache::default();
        cache.create_channel(&channel);
        cache.bind_to_channel(7);

        let mut unique_hashes = [0u64; NUM_PROGRAMS];
        assert!(cache.refresh_stages(&mut unique_hashes));
        assert_ne!(unique_hashes[1], 0);
        assert!(cache.shader_info_slots()[1].is_some());
        assert!(cache.last_shaders_valid());
    }

    #[test]
    fn refresh_stages_respects_shader_dirty_gate() {
        let memory_manager = Arc::new(ParkingLotMutex::new(MemoryManager::new(0)));
        let mut maxwell = Maxwell3D::new();
        maxwell.set_memory_manager(Arc::clone(&memory_manager));
        maxwell.set_guest_memory_reader(make_cpu_reader(0, Arc::new(vec![0; 0x1000])));

        let mut channel = ChannelState::new(8);
        channel.program_id = 0x4321;
        channel.memory_manager = Some(Arc::clone(&memory_manager));
        channel.maxwell_3d = Some(Box::new(maxwell));
        channel.kepler_compute = Some(Box::default());

        let mut cache = ShaderCache::default();
        cache.create_channel(&channel);
        cache.bind_to_channel(8);

        let mut unique_hashes = [0xDEAD_BEEFu64; NUM_PROGRAMS];
        assert!(!cache.refresh_stages(&mut unique_hashes));
        unique_hashes = [0xDEAD_BEEFu64; NUM_PROGRAMS];
        assert!(!cache.refresh_stages(&mut unique_hashes));
        assert_eq!(unique_hashes, [0xDEAD_BEEFu64; NUM_PROGRAMS]);
        assert!(cache.shader_info_slots().iter().all(Option::is_none));
    }

    #[test]
    fn compute_shader_builds_from_bound_channel_compute_state() {
        let gpu_base = 0x1_0000_0000;
        let cpu_base = 0x4000;
        let mut backing = vec![0u8; 0x2000];
        backing[0x180..0x188].copy_from_slice(&0xE2400FFFFF87000Fu64.to_le_bytes());
        let backing = Arc::new(backing);

        let memory_manager = Arc::new(ParkingLotMutex::new(MemoryManager::new(0)));
        memory_manager
            .lock()
            .map(gpu_base, cpu_base, 0x2000, 0, false);

        let mut maxwell = Maxwell3D::new();
        maxwell.set_memory_manager(Arc::clone(&memory_manager));
        maxwell.set_guest_memory_reader(make_cpu_reader(cpu_base, Arc::clone(&backing)));

        let mut channel = ChannelState::new(9);
        channel.program_id = 0x5678;
        channel.memory_manager = Some(Arc::clone(&memory_manager));
        channel.maxwell_3d = Some(Box::new(maxwell));
        channel.kepler_compute = Some(Box::default());
        let kepler = channel
            .kepler_compute
            .as_mut()
            .expect("compute engine should exist for bound-channel shader-cache test");
        kepler.call_method(0x582, 1, true);
        kepler.call_method(0x583, 0, true);
        kepler.launch_description = QueueMetaData {
            program_start: 0x100,
            block_dim_x: 32,
            block_dim_y: 1,
            block_dim_z: 1,
            shared_alloc: 0x80,
            local_pos_alloc: 0x40,
            ..QueueMetaData::default()
        };

        let mut cache = ShaderCache::default();
        cache.create_channel(&channel);
        cache.bind_to_channel(9);

        let shader = cache
            .compute_shader()
            .expect("bound compute channel should build a shader info");
        assert_ne!(shader.unique_hash, 0);
        assert!(shader.size_bytes >= 8);
        assert!(cache.try_get(cpu_base + 0x100).is_some());
    }
}
