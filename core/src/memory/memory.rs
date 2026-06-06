//! Port of zuyu/src/core/memory.h / memory.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-17
//!
//! Core::Memory::Memory — bridges KPageTableBase with the dynarmic page table
//! and the DeviceMemory backing store.

#[cfg(target_os = "linux")]
use common::heap_tracker::HeapTracker;
use common::host_memory::HostMemory;
use common::page_table::{PageInfo, PageTable, PageType};
use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc, Mutex,
};

use crate::core::SystemRef;
use crate::device_memory::{dram_memory_map, DeviceMemory};
use crate::gpu_dirty_memory_manager::GpuDirtyMemoryManager;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Page size constants matching upstream YUZU_PAGEBITS / YUZU_PAGESIZE.
const PAGE_BITS: usize = 12;
const PAGE_SIZE: u64 = 1 << PAGE_BITS;
const PAGE_MASK: u64 = PAGE_SIZE - 1;

static RASTERIZER_MARK_CACHED_LAST_STAGE: AtomicU64 = AtomicU64::new(0);
static RASTERIZER_MARK_CACHED_COUNTS: [AtomicU64; 9] = [
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

fn record_rasterizer_mark_cached_stage(stage: usize) {
    if std::env::var_os("RUZU_PROFILE_RASTERIZER_MARK_CACHED_STALL").is_none() {
        return;
    }
    RASTERIZER_MARK_CACHED_LAST_STAGE.store(stage as u64, Ordering::Relaxed);
    if let Some(counter) = RASTERIZER_MARK_CACHED_COUNTS.get(stage) {
        counter.fetch_add(1, Ordering::Relaxed);
    }
}

pub fn dump_rasterizer_mark_cached_stall_profile() {
    if RASTERIZER_MARK_CACHED_COUNTS[0].load(Ordering::Relaxed) == 0 {
        return;
    }
    const NAMES: [&str; 9] = [
        "enter",
        "after_guard",
        "after_page_table",
        "after_num_pages",
        "before_page_loop",
        "in_page_loop",
        "after_page_loop",
        "exit",
        "early_return",
    ];
    let last_stage = RASTERIZER_MARK_CACHED_LAST_STAGE.load(Ordering::Relaxed) as usize;
    let last_stage_name = NAMES.get(last_stage).copied().unwrap_or("unknown");
    eprintln!(
        "[RASTERIZER_MARK_CACHED_STALL_PROFILE] last_stage={} ({})",
        last_stage, last_stage_name
    );
    for (index, name) in NAMES.iter().enumerate() {
        eprintln!(
            "[RASTERIZER_MARK_CACHED_STALL_PROFILE]   {:02} {:<24} {}",
            index,
            name,
            RASTERIZER_MARK_CACHED_COUNTS[index].load(Ordering::Relaxed)
        );
    }
}

/// Memory permission for mapping operations.
/// Matches upstream Common::MemoryPermission.
pub use common::host_memory::MemoryPermission;

/// Port of Core::Memory::Memory.
///
/// Manages the mapping between guest virtual addresses, physical addresses
/// (in DeviceMemory), and host pointers (in the PageTable used by dynarmic).
pub struct Memory {
    /// Upstream owner: `Core::System& system`.
    system: SystemRef,
    /// Pointer to the device memory backing store.
    device_memory: *const DeviceMemory,
    /// Pointer to the HostMemory buffer (used for fastmem arena base).
    buffer: *const HostMemory,
    /// On Linux: HeapTracker wrapping HostMemory for separate heap fault handling.
    /// Upstream: `std::optional<Common::HeapTracker> heap_tracker` + `HeapTracker* buffer`.
    #[cfg(target_os = "linux")]
    heap_tracker: Option<Box<HeapTracker>>,
    /// Current page table (set by SetCurrentPageTable when switching processes).
    current_page_table: *mut PageTable,
    /// Upstream owner: `std::span<Core::GPUDirtyMemoryManager> gpu_dirty_managers`.
    gpu_dirty_managers: Vec<Arc<Mutex<GpuDirtyMemoryManager>>>,
}

/// Parse a `RUZU_WATCH_BLOCK=ADDR:LEN[,ADDR:LEN...]` spec into byte ranges.
fn parse_block_watch_ranges() -> Vec<(u64, u64)> {
    let raw = match std::env::var("RUZU_WATCH_BLOCK") {
        Ok(s) => s,
        Err(_) => return Vec::new(),
    };
    let mut out = Vec::new();
    for tok in raw.split(',').map(str::trim).filter(|s| !s.is_empty()) {
        let (addr, size) = match tok.split_once(':') {
            Some((a, s)) => (a, s.parse::<u64>().unwrap_or(8)),
            None => (tok, 8),
        };
        let addr_str = addr
            .strip_prefix("0x")
            .or_else(|| addr.strip_prefix("0X"))
            .unwrap_or(addr);
        if let Ok(start) = u64::from_str_radix(addr_str, 16) {
            out.push((start, start.saturating_add(size)));
        }
    }
    out
}

fn check_block_watch(kind: &str, dest_addr: u64, src: &[u8]) {
    use std::sync::OnceLock;
    static RANGES: OnceLock<Vec<(u64, u64)>> = OnceLock::new();
    let ranges = RANGES.get_or_init(parse_block_watch_ranges);
    if ranges.is_empty() {
        return;
    }
    let end = dest_addr.saturating_add(src.len() as u64);
    let Some(&(rs, re)) = ranges.iter().find(|(s, e)| dest_addr < *e && end > *s) else {
        return;
    };
    let bt = std::backtrace::Backtrace::force_capture();
    let dump_len = src.len().min(64);
    eprintln!(
        "[BLOCK_WATCH:{kind}] dest=0x{dest_addr:016X} len={} (range hit 0x{rs:X}..0x{re:X}) bytes[..{dump_len}]={:02x?}\n{bt}",
        src.len(),
        &src[..dump_len]
    );
}

/// RUZU_TRACE_GET_POINTER_PAGE=0xPAGEVADDR — log every get_pointer*
/// call whose returned guest vaddr lies in the same 4 KB page as
/// PAGEVADDR. Includes a backtrace so we can identify the HLE caller
/// that's writing through a host pointer (bypassing write_64/
/// write_block). Throttled to avoid log spam. Used to hunt the STK
/// wedge: corruption at slot 0x814903F8 with no visible writer in the
/// W64 fastmem-direct, W64 slow-path callback, write_block, or
/// memory_write_128 paths.
fn trace_get_pointer_page(kind: &str, vaddr: u64) {
    use std::sync::OnceLock;
    static TARGET_PAGE: OnceLock<Option<u64>> = OnceLock::new();
    let target = TARGET_PAGE.get_or_init(|| {
        std::env::var("RUZU_TRACE_GET_POINTER_PAGE")
            .ok()
            .and_then(|s| u64::from_str_radix(s.trim().trim_start_matches("0x"), 16).ok())
            .map(|v| v & !0xFFFu64)
    });
    let Some(target_page) = target else { return };
    let vaddr_page = vaddr & !0xFFFu64;
    if vaddr_page != *target_page {
        return;
    }
    use std::sync::atomic::{AtomicU32, Ordering};
    static SHOWN: AtomicU32 = AtomicU32::new(0);
    let n = SHOWN.fetch_add(1, Ordering::Relaxed);
    if n < 40 || n.is_multiple_of(1000) {
        let bt = std::backtrace::Backtrace::force_capture();
        eprintln!("[GET_PTR_PAGE:{kind} #{n}] vaddr=0x{vaddr:016X}\n{bt}");
    }
}

/// Env-gated range tracer used by `write_8`/`write_16`/`write_32` to log every
/// host-side guest-memory write whose vaddr falls in a target range.
///
/// `RUZU_TRACE_MEMORY_W_RANGE="0xSTART:0xEND,..."` — log every write whose
/// `[vaddr, vaddr+size)` intersects any listed `[START, END)` half-open range.
/// Catches host-issued writes from any of the write_N helpers (the kernel /
/// HLE writers, plus the JIT memory-write callback path through write_32).
/// Combine with `RUZU_NO_FASTMEM_W32=1`/`_W16`/`_W8` to also catch the JIT
/// fastmem path (otherwise guest stores via fastmem bypass these helpers).
fn maybe_trace_write_in_range(vaddr: u64, size: u64, data: u64) {
    use std::sync::OnceLock;
    static RANGES: OnceLock<Vec<(u64, u64)>> = OnceLock::new();
    let ranges = RANGES.get_or_init(|| {
        let raw = match std::env::var("RUZU_TRACE_MEMORY_W_RANGE") {
            Ok(s) => s,
            Err(_) => return Vec::new(),
        };
        raw.split(',')
            .filter_map(|tok| {
                let tok = tok.trim();
                let mut parts = tok.split(':');
                let start =
                    u64::from_str_radix(parts.next()?.trim().trim_start_matches("0x"), 16).ok()?;
                let end =
                    u64::from_str_radix(parts.next()?.trim().trim_start_matches("0x"), 16).ok()?;
                Some((start, end))
            })
            .collect()
    });
    if ranges.is_empty() {
        return;
    }
    let write_end = vaddr + size;
    for &(s, e) in ranges {
        if vaddr < e && s < write_end {
            let bt = std::backtrace::Backtrace::force_capture();
            eprintln!(
                "[MEMORY_W{:01}] vaddr=0x{:016X} data=0x{:0width$X}\n{}",
                size * 8,
                vaddr,
                data,
                bt,
                width = (size as usize) * 2
            );
            break;
        }
    }
}

// SAFETY: Memory is used behind Arc<Mutex<>> and all raw pointers are
// to long-lived objects (DeviceMemory, HostMemory, PageTable) that outlive Memory.
unsafe impl Send for Memory {}
unsafe impl Sync for Memory {}

impl Memory {
    /// Create a new Memory instance.
    ///
    /// # Safety
    /// The caller must ensure that `device_memory` and `buffer` outlive this Memory.
    pub unsafe fn new(
        system: SystemRef,
        device_memory: *const DeviceMemory,
        buffer: *const HostMemory,
    ) -> Self {
        Self {
            system,
            device_memory,
            buffer,
            #[cfg(target_os = "linux")]
            heap_tracker: None,
            current_page_table: std::ptr::null_mut(),
            gpu_dirty_managers: Vec::new(),
        }
    }

    /// Upstream: `Memory::SetGPUDirtyManagers(std::span<Core::GPUDirtyMemoryManager>)`.
    pub fn set_gpu_dirty_managers(&mut self, managers: Vec<Arc<Mutex<GpuDirtyMemoryManager>>>) {
        self.gpu_dirty_managers = managers;
    }

    /// Get the fastmem arena base pointer (for JIT direct memory access).
    /// Returns null if DeviceMemory buffer is not available.
    pub fn fastmem_pointer(&self) -> *mut u8 {
        if self.buffer.is_null() {
            std::ptr::null_mut()
        } else {
            unsafe { (*self.buffer).virtual_base_pointer() }
        }
    }

    /// Set the current page table (called when switching processes).
    /// Set the current page table and wire up the fastmem arena.
    /// Matches upstream `Memory::Impl::SetCurrentPageTable`.
    ///
    /// Upstream conditionally sets fastmem_arena based on
    /// `process.IsApplication() && Settings::IsFastmemEnabled()`.
    /// Settings are available via `common::settings::values()` but fastmem
    /// enablement depends on the process context which varies at runtime.
    /// Raw pointer to current page table (for diagnostics).
    pub fn current_page_table_raw(&self) -> *mut PageTable {
        self.current_page_table
    }

    pub fn set_current_page_table(&mut self, page_table: *mut PageTable) {
        self.current_page_table = page_table;
        if !page_table.is_null() && !self.buffer.is_null() {
            let pt = unsafe { &mut *page_table };
            // Upstream: if (process.IsApplication() && Settings::IsFastmemEnabled())
            //     page_table.fastmem_arena = buffer.VirtualBasePointer();
            // else
            //     page_table.fastmem_arena = nullptr;
            pt.fastmem_arena = unsafe { (*self.buffer).virtual_base_pointer() };

            // On Linux, create a HeapTracker wrapping the HostMemory buffer.
            // Upstream: heap_tracker.emplace(system.DeviceMemory().buffer);
            //           buffer = std::addressof(*heap_tracker);
            #[cfg(target_os = "linux")]
            {
                let host_mem = unsafe { &mut *(self.buffer as *mut HostMemory) };
                self.heap_tracker = Some(Box::new(HeapTracker::new(host_mem)));
            }

            // RUZU_POLL_DIVERGE=0xVADDR[,0xVADDR,...] — spawn a background
            // thread that compares the value at each VADDR via two views:
            // (a) the VIRTUAL fastmem-arena pointer (= what JIT-direct sees)
            // (b) the BACKING region pointer (= what slow-path callbacks see).
            // If these differ, the wedge-causing coherency divergence is
            // confirmed. Logs first 64 divergences then every 1000th.
            // Useful only when something else triggers the wedge concurrently
            // (e.g. STK in multi-core mode), since slot 0x81490350 is only
            // mutated by JIT-direct writes.
            if let Ok(spec) = std::env::var("RUZU_POLL_DIVERGE") {
                let vaddrs: Vec<u64> = spec
                    .split(',')
                    .filter_map(|s| u64::from_str_radix(s.trim().trim_start_matches("0x"), 16).ok())
                    .collect();
                if !vaddrs.is_empty() {
                    let arena = pt.fastmem_arena as usize;
                    let pt_ptr = page_table as usize;
                    std::thread::Builder::new()
                        .name("ruzu-poll-diverge".into())
                        .spawn(move || {
                            use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
                            static FIRES: AtomicU64 = AtomicU64::new(0);
                            static TRAP_FIRED: AtomicBool = AtomicBool::new(false);
                            // Snapshot last seen values to avoid flooding.
                            let mut last_arena = vec![0u64; vaddrs.len()];
                            let mut last_backing = vec![0u64; vaddrs.len()];
                            // RUZU_POLL_DIVERGE_SLEEP_US=N — poll interval in microseconds.
                            // Default 50ms. Set 0 for tight spin loop.
                            let sleep_us: u64 = std::env::var("RUZU_POLL_DIVERGE_SLEEP_US")
                                .ok()
                                .and_then(|s| s.parse::<u64>().ok())
                                .unwrap_or(50_000);
                            // RUZU_POLL_TRAP_CORRUPT=1 — on first detection of the
                            // known STK corruption pattern at any polled vaddr, dump
                            // all thread states (from /proc/self/task) and SIGSTOP the
                            // whole process. The user can then `gdb -p PID` to inspect
                            // all 4 JIT thread CPU contexts without any prior gdb
                            // attachment (which is what perturbs timing and hides the
                            // wedge — the Heisenbug). This is non-perturbing for the
                            // writer: only reads volatile memory; the SIGSTOP fires
                            // after corruption has been committed.
                            //
                            // Custom pattern: RUZU_POLL_TRAP_CORRUPT_MASK=0xMASK and
                            // RUZU_POLL_TRAP_CORRUPT_VALUE=0xVALUE — trip when
                            // (val & mask) == value. Default (no mask/value given):
                            // (val >> 32) & 0xFFFF == 0x2101 && (val >> 48) & 0xFFFF
                            // == 0  (matches the 0x2101A3B140A0-class corruption).
                            let trap_enabled = std::env::var_os("RUZU_POLL_TRAP_CORRUPT")
                                .is_some();
                            let trap_mask: u64 = std::env::var("RUZU_POLL_TRAP_CORRUPT_MASK")
                                .ok()
                                .and_then(|s| {
                                    u64::from_str_radix(s.trim().trim_start_matches("0x"), 16)
                                        .ok()
                                })
                                .unwrap_or(0xFFFF_FFFF_0000_0000);
                            let trap_value: u64 = std::env::var("RUZU_POLL_TRAP_CORRUPT_VALUE")
                                .ok()
                                .and_then(|s| {
                                    u64::from_str_radix(s.trim().trim_start_matches("0x"), 16)
                                        .ok()
                                })
                                .unwrap_or(0x0000_2101_0000_0000);
                            let is_corrupt = move |v: u64| -> bool {
                                trap_enabled && (v & trap_mask) == trap_value
                            };
                            let do_trap = move |label: &str, vaddr: u64, val: u64| {
                                if TRAP_FIRED.swap(true, Ordering::SeqCst) {
                                    return;
                                }
                                eprintln!(
                                    "[POLL_TRAP_CORRUPT] {} vaddr=0x{:016X} val=0x{:016X} mask=0x{:016X} expect=0x{:016X}",
                                    label, vaddr, val, trap_mask, trap_value,
                                );
                                eprintln!("[POLL_TRAP_CORRUPT] dumping /proc/self/task/*/stat …");
                                if let Ok(entries) = std::fs::read_dir("/proc/self/task") {
                                    for ent in entries.flatten() {
                                        let p = ent.path();
                                        let tid = p.file_name()
                                            .and_then(|s| s.to_str())
                                            .unwrap_or("?")
                                            .to_string();
                                        let comm = std::fs::read_to_string(p.join("comm"))
                                            .unwrap_or_else(|_| String::from("?"));
                                        let stat = std::fs::read_to_string(p.join("stat"))
                                            .unwrap_or_else(|_| String::from("?"));
                                        // /proc/.../stat field 30 is `kstkeip` — kernel-recorded
                                        // instruction pointer. Pre-parsed cheap view:
                                        let fields: Vec<&str> = stat.split_whitespace().collect();
                                        let kstkeip = fields.get(29).copied().unwrap_or("?");
                                        let state = fields.get(2).copied().unwrap_or("?");
                                        eprintln!(
                                            "  tid={:>6} state={} kstkeip=0x{} comm={}",
                                            tid, state, kstkeip.trim_start_matches("0"),
                                            comm.trim_end(),
                                        );
                                    }
                                }
                                eprintln!(
                                    "[POLL_TRAP_CORRUPT] raising SIGSTOP. Attach gdb to inspect: gdb -p {}",
                                    unsafe { libc::getpid() },
                                );
                                unsafe { libc::raise(libc::SIGSTOP) };
                            };
                            // SEGV-safety: we DO NOT dereference pt_ptr in the loop. The
                            // PageTable pointer captured at thread spawn can be freed at
                            // any later point (process tear-down / set_current_page_table
                            // rebinding to a different process). Using it across that
                            // boundary is a UAF. Also: the fastmem arena is a 512GB
                            // sparse mmap with PROT_NONE on unmapped pages, so a direct
                            // volatile read at vaddr+arena SEGVs before the page is
                            // mapped. mincore() doesn't help (the page IS in a VMA, just
                            // PROT_NONE). We use process_vm_readv() which copies from
                            // another (or our own) process's address space via the kernel
                            // and returns -EFAULT cleanly on unreadable addresses without
                            // touching the calling thread's signal mask. The arena
                            // (HostMemory virtual base) lives for the program lifetime,
                            // so its pointer never dangles.
                            let _ = pt_ptr; // keep the capture (silences unused warning)
                            let self_pid = unsafe { libc::getpid() };
                            // Reads `dst.len()` bytes from `addr` in our own address
                            // space via the kernel-bridged process_vm_readv path. Returns
                            // None if the read fails (e.g. PROT_NONE page).
                            let try_read_safe = |addr: *const u8, dst: &mut [u8]| -> bool {
                                let local_iov = libc::iovec {
                                    iov_base: dst.as_mut_ptr() as *mut libc::c_void,
                                    iov_len: dst.len(),
                                };
                                let remote_iov = libc::iovec {
                                    iov_base: addr as *mut libc::c_void,
                                    iov_len: dst.len(),
                                };
                                let n = unsafe {
                                    libc::process_vm_readv(
                                        self_pid,
                                        &local_iov as *const _,
                                        1,
                                        &remote_iov as *const _,
                                        1,
                                        0,
                                    )
                                };
                                n == dst.len() as isize
                            };
                            loop {
                                if sleep_us > 0 {
                                    std::thread::sleep(std::time::Duration::from_micros(sleep_us));
                                }
                                for (i, &vaddr) in vaddrs.iter().enumerate() {
                                    let arena_host_addr =
                                        (arena + vaddr as usize) as *const u64;
                                    // Three SEGV-safe reads via process_vm_readv. If
                                    // the page isn't mapped (PROT_NONE) the syscall
                                    // returns EFAULT and we skip silently.
                                    let mut buf = [0u8; 8];
                                    if !try_read_safe(arena_host_addr as *const u8, &mut buf) {
                                        continue;
                                    }
                                    let arena_a = u64::from_le_bytes(buf);
                                    if !try_read_safe(arena_host_addr as *const u8, &mut buf) {
                                        continue;
                                    }
                                    let arena_b = u64::from_le_bytes(buf);
                                    if !try_read_safe(arena_host_addr as *const u8, &mut buf) {
                                        continue;
                                    }
                                    let arena_c = u64::from_le_bytes(buf);
                                    if is_corrupt(arena_c) {
                                        do_trap("arena", vaddr, arena_c);
                                    }
                                    // Keep `backing_*` vars as aliases of `arena_*` so the
                                    // subsequent diagnostic logging keeps working without
                                    // a larger restructure. The dual-view comparison
                                    // (originally arena vs backing) is now arena vs arena
                                    // — still useful for detecting torn writes via stable
                                    // re-reads.
                                    let backing_a = arena_a;
                                    let backing_b = arena_b;
                                    let backing_c = arena_c;
                                    let arena_stable = arena_a == arena_b && arena_b == arena_c;
                                    let backing_stable = backing_a == backing_b && backing_b == backing_c;
                                    let stable_diverge =
                                        arena_stable && backing_stable && arena_c != backing_c;
                                    let arena_changed = arena_c != last_arena[i];
                                    let backing_changed = backing_c != last_backing[i];
                                    // RUZU_POLL_DIVERGE_LOG_CHANGES=1 — log every change in the
                                    // polled vaddr's value (even if arena==backing). Useful for
                                    // tracking when a specific slot gets the corrupt pattern.
                                    if std::env::var_os("RUZU_POLL_DIVERGE_LOG_CHANGES").is_some()
                                        && (arena_changed || backing_changed)
                                    {
                                        let n = FIRES.fetch_add(1, Ordering::Relaxed);
                                        if n < 200 {
                                            eprintln!(
                                                "[POLL_CHANGE #{}] vaddr=0x{:016X} arena: 0x{:016X} → 0x{:016X}  backing: 0x{:016X} → 0x{:016X}",
                                                n, vaddr,
                                                last_arena[i], arena_c,
                                                last_backing[i], backing_c,
                                            );
                                        }
                                    }
                                    if stable_diverge {
                                        let n = FIRES.fetch_add(1, Ordering::Relaxed);
                                        if n < 64 || n % 1000 == 0 {
                                            eprintln!(
                                                "[POLL_DIVERGE_STABLE #{}] vaddr=0x{:016X} arena_host={:p} arena=0x{:016X} backing=0x{:016X}",
                                                n, vaddr, arena_host_addr,
                                                arena_c, backing_c,
                                            );
                                        }
                                    } else if arena_c != backing_c
                                        && FIRES.load(Ordering::Relaxed) < 4
                                    {
                                        eprintln!(
                                            "[POLL_DIVERGE_RACE] vaddr=0x{:016X} arena=[{:016X},{:016X},{:016X}] backing=[{:016X},{:016X},{:016X}] (race — not stable)",
                                            vaddr, arena_a, arena_b, arena_c,
                                            backing_a, backing_b, backing_c,
                                        );
                                    }
                                    last_arena[i] = arena_c;
                                    last_backing[i] = backing_c;
                                    let _ = (arena_changed, backing_changed);
                                }
                            }
                        })
                        .ok();
                }
            }
        }
    }

    /// Map a physical memory region into the guest virtual address space.
    ///
    /// Matches upstream `Memory::Impl::MapMemoryRegion`:
    /// - Updates PageTable entries (pointers, backing_addr, blocks) per page
    /// - Maps into fastmem arena if available
    ///
    /// # Arguments
    /// * `page_table` - The page table to update
    /// * `base` - Guest virtual address (page-aligned)
    /// * `size` - Size in bytes (page-aligned)
    /// * `target` - Physical address (≥ DramMemoryMap::Base)
    /// * `perms` - Memory permissions
    /// * `separate_heap` - Whether this is a separate heap mapping
    pub fn map_memory_region(
        &self,
        page_table: &mut PageTable,
        base: u64,
        size: u64,
        target: u64,
        perms: MemoryPermission,
        separate_heap: bool,
    ) {
        debug_assert!(
            (size & PAGE_MASK) == 0,
            "non-page aligned size: {:#x}",
            size
        );
        debug_assert!(
            (base & PAGE_MASK) == 0,
            "non-page aligned base: {:#x}",
            base
        );
        debug_assert!(
            target >= dram_memory_map::BASE,
            "Out of bounds target: {:#x}",
            target
        );

        self.map_pages(
            page_table,
            base / PAGE_SIZE,
            size / PAGE_SIZE,
            target,
            PageType::Memory,
        );

        // RUZU_TRACE_MAP_REGION=0xPAGE — log when a map_memory_region call
        // covers the specified 4KB page. Used to verify fastmem-arena
        // mapping for the mstate region (STK heap-shifted-pointer wedge).
        if let Ok(spec) = std::env::var("RUZU_TRACE_MAP_REGION") {
            if let Ok(target_page) = u64::from_str_radix(spec.trim().trim_start_matches("0x"), 16) {
                let page_aligned = target_page & !(PAGE_SIZE - 1);
                if base <= page_aligned && page_aligned < base + size {
                    eprintln!(
                        "[MAP_REGION] base=0x{:016X} size=0x{:X} target=0x{:016X} target-DRAM=0x{:X} fastmem_arena={:?} separate_heap={}",
                        base,
                        size,
                        target,
                        target.wrapping_sub(dram_memory_map::BASE),
                        page_table.fastmem_arena,
                        separate_heap,
                    );
                }
            }
        }
        // RUZU_TRACE_MAP_HOST_OFFSET=0xOFFSET — log every map covering the
        // specified memfd host_offset. Detects aliasing where multiple
        // guest VAs map to the same memfd page (which would corrupt one
        // when writing to the other).
        if let Ok(spec) = std::env::var("RUZU_TRACE_MAP_HOST_OFFSET") {
            if let Ok(target_offset) = u64::from_str_radix(spec.trim().trim_start_matches("0x"), 16)
            {
                let host_offset = target.wrapping_sub(dram_memory_map::BASE);
                let page_aligned = target_offset & !(PAGE_SIZE - 1);
                if host_offset <= page_aligned && page_aligned < host_offset + size {
                    eprintln!(
                        "[MAP_HOST_OFFSET] vaddr_base=0x{:016X} size=0x{:X} target=0x{:016X} host_offset=0x{:X}",
                        base, size, target, host_offset,
                    );
                }
            }
        }

        if !page_table.fastmem_arena.is_null() {
            // Upstream: buffer->Map(base, target - DramBase, size, perms, separate_heap)
            // On Linux, buffer is HeapTracker*; on non-Linux, buffer is HostMemory*.
            #[cfg(target_os = "linux")]
            if let Some(ref heap_tracker) = self.heap_tracker {
                heap_tracker.map(
                    base as usize,
                    (target - dram_memory_map::BASE) as usize,
                    size as usize,
                    perms,
                    separate_heap,
                );
            }
            #[cfg(not(target_os = "linux"))]
            unsafe {
                (*self.buffer).map(
                    base as usize,
                    (target - dram_memory_map::BASE) as usize,
                    size as usize,
                    perms,
                    separate_heap,
                );
            }
        }
    }

    /// Unmap a region of the guest virtual address space.
    ///
    /// Matches upstream `Memory::Impl::UnmapRegion`.
    pub fn unmap_region(
        &self,
        page_table: &mut PageTable,
        base: u64,
        size: u64,
        separate_heap: bool,
    ) {
        debug_assert!((size & PAGE_MASK) == 0);
        debug_assert!((base & PAGE_MASK) == 0);

        self.map_pages(
            page_table,
            base / PAGE_SIZE,
            size / PAGE_SIZE,
            0,
            PageType::Unmapped,
        );

        if !page_table.fastmem_arena.is_null() {
            #[cfg(target_os = "linux")]
            if let Some(ref heap_tracker) = self.heap_tracker {
                heap_tracker.unmap(base as usize, size as usize, separate_heap);
            }
            #[cfg(not(target_os = "linux"))]
            unsafe {
                (*self.buffer).unmap(base as usize, size as usize, separate_heap);
            }
        }
    }

    /// Change protection on a region of the guest virtual address space.
    ///
    /// Matches upstream `Memory::Impl::ProtectRegion`.
    pub fn protect_region(
        &self,
        page_table: &mut PageTable,
        vaddr: u64,
        size: u64,
        perms: MemoryPermission,
    ) {
        debug_assert!((size & PAGE_MASK) == 0);
        debug_assert!((vaddr & PAGE_MASK) == 0);

        if page_table.fastmem_arena.is_null() {
            return;
        }

        let mut protect_bytes: u64 = 0;
        let mut protect_begin: u64 = 0;

        let mut addr = vaddr;
        while addr < vaddr + size {
            let page_idx = (addr >> PAGE_BITS) as usize;
            let page_type = if page_idx < page_table.pointers.size() {
                page_table.pointers[page_idx].page_type()
            } else {
                PageType::Unmapped
            };

            match page_type {
                PageType::RasterizerCachedMemory => {
                    if protect_bytes > 0 {
                        self.protect_buffer(protect_begin as usize, protect_bytes as usize, perms);
                        protect_bytes = 0;
                    }
                }
                _ => {
                    if protect_bytes == 0 {
                        protect_begin = addr;
                    }
                    protect_bytes += PAGE_SIZE;
                }
            }

            addr += PAGE_SIZE;
        }

        if protect_bytes > 0 {
            self.protect_buffer(protect_begin as usize, protect_bytes as usize, perms);
        }
    }

    // =========================================================================
    // Read/Write via PageTable pointers
    // Matches upstream Core::Memory::Memory::Read/Write methods.
    // =========================================================================

    /// Get a host pointer for a guest virtual address (fast path).
    /// Matches upstream `Memory::Impl::GetPointerImpl`.
    ///
    /// Returns null if the page is unmapped.
    /// Route protect calls through HeapTracker on Linux, HostMemory otherwise.
    fn protect_buffer(&self, offset: usize, size: usize, perms: MemoryPermission) {
        #[cfg(target_os = "linux")]
        if let Some(ref heap_tracker) = self.heap_tracker {
            heap_tracker.protect(offset, size, perms);
            return;
        }
        unsafe {
            (*self.buffer).protect(offset, size, perms);
        }
    }

    #[inline]
    fn get_pointer_impl(&self, vaddr: u64) -> *mut u8 {
        // AARCH64 masks the upper 16 bits of all memory accesses.
        let vaddr = vaddr & 0xffff_ffff_ffff;

        if self.current_page_table.is_null() {
            return std::ptr::null_mut();
        }
        let pt = unsafe { &*self.current_page_table };
        let page_idx = (vaddr >> PAGE_BITS) as usize;
        if page_idx >= pt.pointers.size() {
            return std::ptr::null_mut();
        }

        let raw = pt.pointers[page_idx].raw_value();
        let pointer = PageInfo::extract_pointer(raw);
        if pointer != 0 {
            // Fast path: direct host pointer + vaddr
            return (pointer + vaddr as usize) as *mut u8;
        }

        // Slow path: check page type
        match PageInfo::extract_type(raw) {
            PageType::Unmapped => std::ptr::null_mut(),
            PageType::Memory => {
                // Upstream: ASSERT_MSG(false, "Mapped memory page without a pointer")
                debug_assert!(
                    false,
                    "Mapped memory page without a pointer @ {:#018x}",
                    vaddr
                );
                std::ptr::null_mut()
            }
            PageType::DebugMemory => self.get_pointer_from_debug_memory(vaddr),
            PageType::RasterizerCachedMemory => {
                self.get_pointer_from_rasterizer_cached_memory(vaddr)
            }
        }
    }

    /// Get pointer from debug memory (slow path).
    /// Matches upstream `Memory::Impl::GetPointerFromDebugMemory`.
    fn get_pointer_from_debug_memory(&self, vaddr: u64) -> *mut u8 {
        if self.current_page_table.is_null() {
            return std::ptr::null_mut();
        }
        let pt = unsafe { &*self.current_page_table };
        let page_idx = (vaddr >> PAGE_BITS) as usize;
        if page_idx >= pt.backing_addr.size() {
            return std::ptr::null_mut();
        }
        let backing = pt.backing_addr[page_idx] as usize;
        if backing == 0 {
            return std::ptr::null_mut();
        }
        unsafe {
            let dm = &*self.device_memory;
            dm.buffer
                .backing_base_pointer()
                .add(backing + vaddr as usize) as *mut u8
        }
    }

    /// Get pointer from rasterizer cached memory (slow path).
    /// Matches upstream `Memory::Impl::GetPointerFromRasterizerCachedMemory`.
    fn get_pointer_from_rasterizer_cached_memory(&self, vaddr: u64) -> *mut u8 {
        // For now, same as debug memory (rasterizer cache not yet implemented).
        self.get_pointer_from_debug_memory(vaddr)
    }

    /// Mark a CPU virtual-address range as cached (or no longer cached) by the
    /// rasterizer. Used by the GPU device-memory manager when shader/buffer/
    /// texture caches register or invalidate regions.
    ///
    /// Port of upstream `Memory::Impl::RasterizerMarkRegionCached`
    /// (`core/memory.cpp:793-844`). Walks each CPU page in the range and
    /// transitions its `PageType`:
    /// - `Memory`/`DebugMemory` → `RasterizerCachedMemory` when `cached`.
    /// - `RasterizerCachedMemory` → `Memory` when uncached (pointer recovered
    ///   via `get_pointer_from_rasterizer_cached_memory`, which uses the
    ///   per-page `backing_addr` table that survives the type transition).
    /// - `Unmapped` pages skipped (matches upstream — a process need not map
    ///   the GPU-cached region into its own AS, e.g. VRAM-only buffers).
    ///
    /// **Port simplifications**: the fastmem `mprotect()` is not yet emitted
    /// here (upstream's `Settings::values.use_reactive_flushing` path).
    /// ruzu's fastmem handler routes RasterizerCachedMemory pages through
    /// the slow callback automatically via `page_type_at` checks in the
    /// memory_read_*/write_* paths, so the page-type transition alone is
    /// enough to redirect CPU writes through the rasterizer's invalidation
    /// hook. Reactive flushing (the write-protect optimization) can be added
    /// in a follow-up.
    pub fn rasterizer_mark_region_cached(&self, vaddr: u64, size: u64, cached: bool) {
        record_rasterizer_mark_cached_stage(0);
        if vaddr == 0 || size == 0 || self.current_page_table.is_null() {
            record_rasterizer_mark_cached_stage(8);
            return;
        }
        record_rasterizer_mark_cached_stage(1);
        let pt = unsafe { &*self.current_page_table };
        record_rasterizer_mark_cached_stage(2);
        // Upstream computes `num_pages` as
        //   ((vaddr + size - 1) >> PAGEBITS) - (vaddr >> PAGEBITS) + 1
        // so single-byte writes still touch one page, and a write straddling
        // a page boundary touches two pages — even when `size < PAGE_SIZE`.
        let num_pages = ((vaddr + size - 1) >> PAGE_BITS) - (vaddr >> PAGE_BITS) + 1;
        record_rasterizer_mark_cached_stage(3);
        let mut current_vaddr = vaddr;
        record_rasterizer_mark_cached_stage(4);
        for _ in 0..num_pages {
            record_rasterizer_mark_cached_stage(5);
            let page_idx = (current_vaddr >> PAGE_BITS) as usize;
            if page_idx < pt.pointers.size() {
                let entry = &pt.pointers[page_idx];
                let ptype = PageInfo::extract_type(entry.raw_value());
                if cached {
                    match ptype {
                        PageType::Memory | PageType::DebugMemory => {
                            // Switch to RasterizerCachedMemory. Pointer is
                            // stored as 0; readers go through the slow path
                            // (`get_pointer_from_rasterizer_cached_memory`).
                            entry.store(0, PageType::RasterizerCachedMemory);
                        }
                        // Unmapped → skip (no CPU backing to track).
                        // RasterizerCachedMemory → already cached, common
                        // when multiple GPU regions map the same CPU page.
                        _ => {}
                    }
                } else {
                    if ptype == PageType::RasterizerCachedMemory {
                        let pointer = self.get_pointer_from_rasterizer_cached_memory(current_vaddr);
                        if !pointer.is_null() {
                            // Encode pointer as `ptr - vaddr` so the fastmem
                            // path can recover the host address with one
                            // addition (matches the PageInfo layout used by
                            // `map_pages`).
                            let encoded = (pointer as usize).wrapping_sub(current_vaddr as usize);
                            entry.store(encoded, PageType::Memory);
                        } else {
                            // No backing recoverable — fall back to debug.
                            entry.store(0, PageType::DebugMemory);
                        }
                    }
                }
            }
            current_vaddr += PAGE_SIZE as u64;
        }
        record_rasterizer_mark_cached_stage(6);
        record_rasterizer_mark_cached_stage(7);
    }

    fn current_physical_address(&self, vaddr: u64) -> Option<u64> {
        if self.current_page_table.is_null() {
            return None;
        }
        let pt = unsafe { &*self.current_page_table };
        let page_idx = (vaddr >> PAGE_BITS) as usize;
        if page_idx >= pt.backing_addr.size() {
            return None;
        }
        let backing = pt.backing_addr[page_idx];
        if backing == 0 {
            return None;
        }
        // backing was computed via wrapping_sub in map_pages, so the cancellation
        // here (backing + vaddr) must also wrap to recover the device address.
        Some(backing.wrapping_add(vaddr))
    }

    fn handle_rasterizer_write(&self, vaddr: u64, size: usize) {
        let Some(device_addr) = self.current_physical_address(vaddr) else {
            return;
        };

        let do_collection = self.system.is_null()
            || self
                .system
                .get()
                .gpu_core()
                .map(|gpu| gpu.on_cpu_write(device_addr, size as u64))
                .unwrap_or(false);
        if !do_collection {
            return;
        }

        if let Some(manager) = self.gpu_dirty_managers.first() {
            manager.lock().unwrap().collect(device_addr, size);
        }
    }

    fn page_type_at(&self, vaddr: u64) -> Option<PageType> {
        if self.current_page_table.is_null() {
            return None;
        }
        let pt = unsafe { &*self.current_page_table };
        let page_idx = (vaddr >> PAGE_BITS) as usize;
        if page_idx >= pt.pointers.size() {
            return None;
        }
        Some(PageInfo::extract_type(pt.pointers[page_idx].raw_value()))
    }

    fn perform_cache_operation<F>(
        &self,
        dest_addr: u64,
        size: usize,
        mut on_rasterizer: F,
    ) -> ResultCode
    where
        F: FnMut(u64, usize),
    {
        let mut remaining = size;
        let mut vaddr = dest_addr;

        while remaining > 0 {
            let page_offset = (vaddr & PAGE_MASK) as usize;
            let block_size = ((PAGE_SIZE as usize) - page_offset).min(remaining);

            if self.get_pointer_impl(vaddr).is_null() {
                // Upstream zuyu's cache helpers currently succeed without
                // checking page mappings. Preserve that behaviour for guest
                // cache-maintenance SVCs while still letting mapped
                // rasterizer-cached pages trigger the Rust-side coherency hook.
                vaddr += block_size as u64;
                remaining -= block_size;
                continue;
            }

            if self.page_type_at(vaddr) == Some(PageType::RasterizerCachedMemory) {
                on_rasterizer(vaddr, block_size);
            }

            vaddr += block_size as u64;
            remaining -= block_size;
        }

        RESULT_SUCCESS
    }

    /// Invalidates a range of bytes within the current process address space.
    ///
    /// Matches upstream `Memory::InvalidateDataCache`: rasterizer-cached ranges
    /// are downloaded from host GPU memory to guest memory.
    pub fn invalidate_data_cache(&self, dest_addr: u64, size: usize) -> ResultCode {
        self.perform_cache_operation(dest_addr, size, |current_vaddr, block_size| {
            let Some(device_addr) = self.current_physical_address(current_vaddr) else {
                return;
            };
            if !self.system.is_null() {
                if let Some(gpu) = self.system.get().gpu_core() {
                    let _ = gpu.on_cpu_read(device_addr, block_size as u64);
                }
            }
        })
    }

    /// Stores a range of bytes within the current process address space.
    ///
    /// Matches upstream `Memory::StoreDataCache`: CPU flush -> GPU invalidate.
    pub fn store_data_cache(&self, dest_addr: u64, size: usize) -> ResultCode {
        self.perform_cache_operation(dest_addr, size, |current_vaddr, block_size| {
            self.handle_rasterizer_write(current_vaddr, block_size);
        })
    }

    /// Flushes a range of bytes within the current process address space.
    ///
    /// Matches upstream `Memory::FlushDataCache`: CPU flush -> GPU invalidate.
    pub fn flush_data_cache(&self, dest_addr: u64, size: usize) -> ResultCode {
        self.perform_cache_operation(dest_addr, size, |current_vaddr, block_size| {
            self.handle_rasterizer_write(current_vaddr, block_size);
        })
    }

    /// Get a host pointer for a guest virtual address.
    /// Matches upstream `Memory::GetPointer`.
    pub fn get_pointer(&self, vaddr: u64) -> *mut u8 {
        // RUZU_TRACE_GET_POINTER_PAGE=0xPAGEVADDR — log every get_pointer
        // call returning a pointer within the same 4 KB page as PAGEVADDR.
        // Used to find HLE callers that bypass write_64/write_block.
        trace_get_pointer_page("get_pointer", vaddr);
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped GetPointer @ {:#018x}", vaddr);
        }
        ptr
    }

    /// Get a host pointer without logging on unmapped addresses.
    /// Matches upstream `Memory::GetPointerSilent`.
    pub fn get_pointer_silent(&self, vaddr: u64) -> *mut u8 {
        trace_get_pointer_page("get_pointer_silent", vaddr);
        self.get_pointer_impl(vaddr)
    }

    /// Read a value of type T from guest virtual address.
    /// Matches upstream `Memory::Impl::Read<T>`.
    #[inline]
    unsafe fn read_raw<T: Copy + Default>(&self, vaddr: u64) -> T {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            // RUZU_TRACE_UNMAPPED_BT=1 — capture host backtrace on the first
            // few unmapped reads so we can identify the calling subsystem
            // (HLE service code path, JIT trampoline, etc.). Throttled to
            // 5 entries to avoid log spam.
            if std::env::var_os("RUZU_TRACE_UNMAPPED_BT").is_some() {
                use std::sync::atomic::{AtomicU32, Ordering};
                static SHOWN: AtomicU32 = AtomicU32::new(0);
                let n = SHOWN.fetch_add(1, Ordering::Relaxed);
                if n < 5 {
                    let bt = std::backtrace::Backtrace::force_capture();
                    eprintln!(
                        "[UNMAPPED_BT #{}] vaddr=0x{:016X} size={}\n{}",
                        n,
                        vaddr,
                        std::mem::size_of::<T>() * 8,
                        bt
                    );
                }
            }
            log::error!(
                "Unmapped Read{} @ {:#018x}",
                std::mem::size_of::<T>() * 8,
                vaddr
            );
            return T::default();
        }
        std::ptr::read_unaligned(ptr as *const T)
    }

    /// Write a value of type T to guest virtual address.
    /// Matches upstream `Memory::Impl::Write<T>`.
    #[inline]
    unsafe fn write_raw<T: Copy>(&self, vaddr: u64, data: T) {
        // `RUZU_TRACE_RAW_WRITE_AT=0xVADDR` — log a backtrace whenever the
        // [vaddr, vaddr+sizeof(T)) range covers the target. `write_raw`
        // is the lowest-level guest-memory writer; all of `write_8/16/
        // 32/64`, `write_32_no_rasterizer`, `write_block_no_rasterizer`
        // and `write_block` ultimately funnel through here, so this
        // catches every Rust-side write regardless of the public entry
        // point.
        //
        // The target is parsed once via `OnceLock` so the hot path is
        // just an atomic load + range check; when unset, the cost is
        // one extra branch.
        {
            use std::sync::OnceLock;
            static TARGET: OnceLock<Option<u64>> = OnceLock::new();
            let target = *TARGET.get_or_init(|| {
                std::env::var("RUZU_TRACE_RAW_WRITE_AT")
                    .ok()
                    .and_then(|s| u64::from_str_radix(s.trim().trim_start_matches("0x"), 16).ok())
            });
            if let Some(target) = target {
                let size = std::mem::size_of::<T>() as u64;
                if vaddr <= target && target < vaddr + size {
                    let bt = std::backtrace::Backtrace::force_capture();
                    // For sizes ≤ 8, we can read back the bytes from the
                    // input data via raw memory copy to format them.
                    let mut buf = [0u8; 8];
                    let n = (size as usize).min(8);
                    std::ptr::copy_nonoverlapping(
                        &data as *const T as *const u8,
                        buf.as_mut_ptr(),
                        n,
                    );
                    let mut hex = String::new();
                    for b in &buf[..n] {
                        use std::fmt::Write;
                        let _ = write!(hex, "{:02x}", b);
                    }
                    eprintln!(
                        "[RAW_WRITE_AT] vaddr=0x{:016X} size={} bytes={}\n{}",
                        vaddr, size, hex, bt
                    );
                }
            }
        }

        // `RUZU_TRACE_RAW_WRITE_VALUE=0xVALUE` — log a backtrace whenever
        // `write_raw` is called with a 4-byte value equal to VALUE,
        // regardless of address. Used to hunt for a "magic" sentinel
        // value (e.g. 0x80000029 in MK8D task #112) without knowing
        // where it lands. Hot-path cost: 1 atomic load + 1 size check
        // + 1 value compare when enabled, 1 atomic load when not.
        {
            use std::sync::OnceLock;
            static TARGET_VALUE: OnceLock<Option<u32>> = OnceLock::new();
            let target_value = *TARGET_VALUE.get_or_init(|| {
                std::env::var("RUZU_TRACE_RAW_WRITE_VALUE")
                    .ok()
                    .and_then(|s| u32::from_str_radix(s.trim().trim_start_matches("0x"), 16).ok())
            });
            if let Some(target_value) = target_value {
                let size = std::mem::size_of::<T>();
                if size == 4 {
                    let val_u32 =
                        unsafe { std::ptr::read_unaligned(&data as *const T as *const u32) };
                    if val_u32 == target_value {
                        let bt = std::backtrace::Backtrace::force_capture();
                        eprintln!(
                            "[RAW_WRITE_VALUE] vaddr=0x{:016X} size={} value=0x{:08X}\n{}",
                            vaddr, size, val_u32, bt
                        );
                    }
                }
            }
        }

        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!(
                "Unmapped Write{} @ {:#018x}",
                std::mem::size_of::<T>() * 8,
                vaddr
            );
            return;
        }
        std::ptr::write_unaligned(ptr as *mut T, data);
    }

    /// Read a u8. Matches upstream `Memory::Read8`.
    pub fn read_8(&self, vaddr: u64) -> u8 {
        unsafe { self.read_raw::<u8>(vaddr) }
    }

    /// Read a u16 (LE). Matches upstream `Memory::Read16`.
    pub fn read_16(&self, vaddr: u64) -> u16 {
        if (vaddr & 1) == 0 {
            unsafe { self.read_raw::<u16>(vaddr) }
        } else {
            let a = self.read_8(vaddr) as u16;
            let b = self.read_8(vaddr + 1) as u16;
            (b << 8) | a
        }
    }

    /// Read a u32 (LE). Matches upstream `Memory::Read32`.
    pub fn read_32(&self, vaddr: u64) -> u32 {
        if (vaddr & 3) == 0 {
            unsafe { self.read_raw::<u32>(vaddr) }
        } else {
            let a = self.read_16(vaddr) as u32;
            let b = self.read_16(vaddr + 2) as u32;
            (b << 16) | a
        }
    }

    /// Read a u64 (LE). Matches upstream `Memory::Read64`.
    pub fn read_64(&self, vaddr: u64) -> u64 {
        if (vaddr & 7) == 0 {
            unsafe { self.read_raw::<u64>(vaddr) }
        } else {
            let a = self.read_32(vaddr) as u64;
            let b = self.read_32(vaddr + 4) as u64;
            (b << 32) | a
        }
    }

    /// Write a u8. Matches upstream `Memory::Write8`.
    pub fn write_8(&self, vaddr: u64, data: u8) {
        maybe_trace_write_in_range(vaddr, 1, data as u64);
        self.handle_rasterizer_write(vaddr, std::mem::size_of::<u8>());
        unsafe { self.write_raw::<u8>(vaddr, data) }
    }

    /// Write a u16 (LE). Matches upstream `Memory::Write16`.
    pub fn write_16(&self, vaddr: u64, data: u16) {
        maybe_trace_write_in_range(vaddr, 2, data as u64);
        self.handle_rasterizer_write(vaddr, std::mem::size_of::<u16>());
        if (vaddr & 1) == 0 {
            unsafe { self.write_raw::<u16>(vaddr, data) }
        } else {
            self.write_8(vaddr, data as u8);
            self.write_8(vaddr + 1, (data >> 8) as u8);
        }
    }

    /// Write a u32 (LE). Matches upstream `Memory::Write32`.
    pub fn write_32(&self, vaddr: u64, data: u32) {
        maybe_trace_write_in_range(vaddr, 4, data as u64);
        // `RUZU_TRACE_MEMORY_W32_AT_VADDR=0xVADDR` — log every Rust-side
        // `Memory::write_32` call whose vaddr matches. Counterpart to the
        // existing `RUZU_TRACE_MEMORY_W64_AT_VADDR`. Catches HLE / kernel
        // writes (like `write_to_user` in k_condition_variable) that
        // bypass the JIT memory_write_32 callback. Pair with
        // `RUZU_NO_FASTMEM_W32=1` to see ALL writes (guest + kernel).
        if let Ok(spec) = std::env::var("RUZU_TRACE_MEMORY_W32_AT_VADDR") {
            if let Ok(target) = u64::from_str_radix(spec.trim_start_matches("0x"), 16) {
                if vaddr <= target && target < vaddr + 4 {
                    let bt = std::backtrace::Backtrace::force_capture();
                    eprintln!(
                        "[MEMORY_W32] vaddr=0x{:016X} data=0x{:08X}\n{}",
                        vaddr, data, bt
                    );
                }
            }
        }
        self.handle_rasterizer_write(vaddr, std::mem::size_of::<u32>());
        if (vaddr & 3) == 0 {
            unsafe { self.write_raw::<u32>(vaddr, data) }
        } else {
            self.write_16(vaddr, data as u16);
            self.write_16(vaddr + 2, (data >> 16) as u16);
        }
    }

    /// Write a u32 (LE) without notifying the rasterizer.
    ///
    /// Kernel synchronization helpers use this for guest mutex/CV words while
    /// holding the global scheduler lock. Calling the rasterizer from there can
    /// invert Rust host locks (`Memory` -> shader cache) against the GPU thread
    /// (shader cache -> `Memory`). Guest/JIT writes must keep using `write_32`.
    pub fn write_32_no_rasterizer(&self, vaddr: u64, data: u32) {
        if (vaddr & 3) == 0 {
            unsafe { self.write_raw::<u32>(vaddr, data) }
        } else {
            unsafe {
                self.write_raw::<u8>(vaddr, data as u8);
                self.write_raw::<u8>(vaddr + 1, (data >> 8) as u8);
                self.write_raw::<u8>(vaddr + 2, (data >> 16) as u8);
                self.write_raw::<u8>(vaddr + 3, (data >> 24) as u8);
            }
        }
    }

    /// Write a byte block without notifying the rasterizer.
    ///
    /// Used for host-side HLE/service writes where ruzu already holds the global
    /// `Mutex<Memory>`. Guest/JIT writes must keep using `write_block`.
    pub fn write_block_no_rasterizer(&self, dest_addr: u64, src: &[u8]) -> bool {
        // `RUZU_TRACE_WRITE_BLOCK_AT=0xVADDR` — log every HLE-side
        // `write_block_no_rasterizer` whose [dest, dest+len) range covers
        // VADDR. Used to attribute non-fastmem writes (HLE WriteBuffer,
        // etc.) that bypass both JIT memory callbacks and the per-u32
        // `RUZU_TRACE_MEMORY_W32_AT_VADDR` hook in `write_32`.
        if let Ok(spec) = std::env::var("RUZU_TRACE_WRITE_BLOCK_AT") {
            if let Ok(target) = u64::from_str_radix(spec.trim_start_matches("0x"), 16) {
                if dest_addr <= target && target < dest_addr + src.len() as u64 {
                    let bt = std::backtrace::Backtrace::force_capture();
                    let off = (target - dest_addr) as usize;
                    let len = src.len();
                    let preview_end = (off + 16).min(len);
                    let mut preview = String::new();
                    for &b in &src[off..preview_end] {
                        use std::fmt::Write;
                        let _ = write!(preview, "{:02x}", b);
                    }
                    eprintln!(
                        "[WRITE_BLOCK_AT] dest=0x{:016X} len={:#x} off=0x{:X} preview={}\n{}",
                        dest_addr, len, off, preview, bt
                    );
                }
            }
        }
        let size = src.len();
        if size == 0 {
            return true;
        }

        if !self.address_space_contains(dest_addr, size) {
            log::error!("Unmapped WriteBlock @ {:#018x} size={:#x}", dest_addr, size);
            return false;
        }

        let mut remaining = size;
        let mut offset = 0usize;
        let mut vaddr = dest_addr;
        let mut user_accessible = true;

        while remaining > 0 {
            let page_offset = (vaddr & PAGE_MASK) as usize;
            let copy_amount = ((PAGE_SIZE as usize) - page_offset).min(remaining);

            let ptr = self.get_pointer_impl(vaddr);
            if ptr.is_null() {
                log::error!("Unmapped WriteBlock @ {:#018x}", vaddr);
                user_accessible = false;
            } else {
                unsafe {
                    std::ptr::copy_nonoverlapping(src[offset..].as_ptr(), ptr, copy_amount);
                }
            }

            vaddr += copy_amount as u64;
            offset += copy_amount;
            remaining -= copy_amount;
        }
        user_accessible
    }

    /// SEGV-safe variant for HLE IPC output buffers.
    ///
    /// `write_block_no_rasterizer` uses direct host pointers, matching the hot
    /// memory path. IPC out-buffers can race with guest-side unmapping/protection
    /// while the service thread is copying a large file chunk; using
    /// process_vm_writev turns a temporarily inaccessible host page into `EFAULT`
    /// instead of taking the emulator down with SIGSEGV.
    pub fn write_block_no_rasterizer_checked(&self, dest_addr: u64, src: &[u8]) -> bool {
        if let Ok(spec) = std::env::var("RUZU_TRACE_WRITE_BLOCK_AT") {
            if let Ok(target) = u64::from_str_radix(spec.trim_start_matches("0x"), 16) {
                if dest_addr <= target && target < dest_addr + src.len() as u64 {
                    let bt = std::backtrace::Backtrace::force_capture();
                    let off = (target - dest_addr) as usize;
                    let len = src.len();
                    let preview_end = (off + 16).min(len);
                    let mut preview = String::new();
                    for &b in &src[off..preview_end] {
                        use std::fmt::Write;
                        let _ = write!(preview, "{:02x}", b);
                    }
                    eprintln!(
                        "[WRITE_BLOCK_CHECKED_AT] dest=0x{:016X} len={:#x} off=0x{:X} preview={}\n{}",
                        dest_addr, len, off, preview, bt
                    );
                }
            }
        }
        let size = src.len();
        if size == 0 {
            return true;
        }

        if !self.address_space_contains(dest_addr, size) {
            log::error!(
                "Unmapped checked WriteBlock @ {:#018x} size={:#x}",
                dest_addr,
                size
            );
            return false;
        }

        let self_pid = unsafe { libc::getpid() };
        let first_ptr = self.get_pointer_impl(dest_addr);
        if !first_ptr.is_null() {
            let local_iov = libc::iovec {
                iov_base: src.as_ptr() as *mut libc::c_void,
                iov_len: size,
            };
            let remote_iov = libc::iovec {
                iov_base: first_ptr as *mut libc::c_void,
                iov_len: size,
            };
            let written = unsafe {
                libc::process_vm_writev(
                    self_pid,
                    &local_iov as *const _,
                    1,
                    &remote_iov as *const _,
                    1,
                    0,
                )
            };
            if written == size as isize {
                return true;
            }
            log::error!(
                "checked WriteBlock fast path failed @ {:#018x} size={:#x} written={}",
                dest_addr,
                size,
                written
            );
        }

        let mut remaining = size;
        let mut offset = 0usize;
        let mut vaddr = dest_addr;
        let mut user_accessible = true;

        while remaining > 0 {
            let page_offset = (vaddr & PAGE_MASK) as usize;
            let copy_amount = ((PAGE_SIZE as usize) - page_offset).min(remaining);

            let ptr = self.get_pointer_impl(vaddr);
            if ptr.is_null() {
                log::error!("Unmapped checked WriteBlock @ {:#018x}", vaddr);
                user_accessible = false;
            } else {
                let local_iov = libc::iovec {
                    iov_base: src[offset..].as_ptr() as *mut libc::c_void,
                    iov_len: copy_amount,
                };
                let remote_iov = libc::iovec {
                    iov_base: ptr as *mut libc::c_void,
                    iov_len: copy_amount,
                };
                let written = unsafe {
                    libc::process_vm_writev(
                        self_pid,
                        &local_iov as *const _,
                        1,
                        &remote_iov as *const _,
                        1,
                        0,
                    )
                };
                if written != copy_amount as isize {
                    log::error!(
                        "checked WriteBlock failed @ {:#018x} size={:#x} written={}",
                        vaddr,
                        copy_amount,
                        written
                    );
                    user_accessible = false;
                }
            }

            vaddr += copy_amount as u64;
            offset += copy_amount;
            remaining -= copy_amount;
        }
        user_accessible
    }

    /// Write a u64 (LE). Matches upstream `Memory::Write64`.
    pub fn write_64(&self, vaddr: u64, data: u64) {
        // RUZU_TRACE_MEMORY_W64_AT_VADDR=0xVADDR — log every call into
        // Memory::write_64 with vaddr matching. Catches any Rust-side
        // write (HLE, kernel, etc.) that bypasses the JIT callback.
        if let Ok(spec) = std::env::var("RUZU_TRACE_MEMORY_W64_AT_VADDR") {
            if let Ok(target) = u64::from_str_radix(spec.trim_start_matches("0x"), 16) {
                if vaddr == target {
                    let bt = std::backtrace::Backtrace::force_capture();
                    eprintln!(
                        "[MEMORY_W64] vaddr=0x{:016X} data=0x{:016X}\n{}",
                        vaddr, data, bt
                    );
                }
            }
        }
        self.handle_rasterizer_write(vaddr, std::mem::size_of::<u64>());
        if (vaddr & 7) == 0 {
            unsafe { self.write_raw::<u64>(vaddr, data) }
        } else {
            self.write_32(vaddr, data as u32);
            self.write_32(vaddr + 4, (data >> 32) as u32);
        }
    }

    /// Check if an address range is within the current address space.
    /// Matches upstream `AddressSpaceContains`.
    fn address_space_contains(&self, addr: u64, size: usize) -> bool {
        if self.current_page_table.is_null() {
            return false;
        }
        let pt = unsafe { &*self.current_page_table };
        let max_addr = 1u64 << pt.current_address_space_width_in_bits;
        let end = addr.checked_add(size as u64);
        match end {
            Some(e) => e >= addr && e <= max_addr,
            None => false,
        }
    }

    /// Read a block of data from guest memory.
    /// Matches upstream `Memory::ReadBlock` (via WalkBlock pattern).
    pub fn read_block(&self, src_addr: u64, dest: &mut [u8]) -> bool {
        let size = dest.len();
        let trace_read_ptr = std::env::var("RUZU_TRACE_READ_BLOCK_PTR")
            .ok()
            .and_then(|raw| {
                let raw = raw.trim();
                let digits = raw
                    .strip_prefix("0x")
                    .or_else(|| raw.strip_prefix("0X"))
                    .unwrap_or(raw);
                u64::from_str_radix(digits, 16)
                    .ok()
                    .or_else(|| raw.parse::<u64>().ok())
            });

        // Upstream: AddressSpaceContains check before walking pages.
        if !self.address_space_contains(src_addr, size) {
            log::error!("Unmapped ReadBlock @ {:#018x} size={:#x}", src_addr, size);
            dest.fill(0);
            return false;
        }

        let mut remaining = size;
        let mut offset = 0usize;
        let mut vaddr = src_addr;
        let mut user_accessible = true;

        while remaining > 0 {
            let page_offset = (vaddr & PAGE_MASK) as usize;
            let copy_amount = ((PAGE_SIZE as usize) - page_offset).min(remaining);

            let ptr = self.get_pointer_impl(vaddr);
            if ptr.is_null() {
                log::error!("Unmapped ReadBlock @ {:#018x}", vaddr);
                // Zero destination for unmapped pages, matching upstream.
                dest[offset..offset + copy_amount].fill(0);
                user_accessible = false;
            } else {
                if trace_read_ptr
                    .is_some_and(|target| vaddr <= target && target < vaddr + copy_amount as u64)
                {
                    log::info!(
                        "[READ_BLOCK_PTR] src=0x{:X} page_vaddr=0x{:X} ptr={:p} copy=0x{:X} size=0x{:X}",
                        src_addr,
                        vaddr,
                        ptr,
                        copy_amount,
                        size,
                    );
                }
                unsafe {
                    std::ptr::copy_nonoverlapping(ptr, dest[offset..].as_mut_ptr(), copy_amount);
                }
            }

            vaddr += copy_amount as u64;
            offset += copy_amount;
            remaining -= copy_amount;
        }
        user_accessible
    }

    /// SEGV-safe variant for host-side HLE consumers that read guest buffers.
    ///
    /// The normal `read_block` path copies from raw translated host pointers,
    /// matching upstream's fast path. Audio decode can observe transiently stale
    /// or protected host pages while consuming guest wave buffers; use
    /// `process_vm_readv` here so an inaccessible host page becomes `EFAULT`
    /// instead of a process-wide SIGSEGV.
    pub fn read_block_checked(&self, src_addr: u64, dest: &mut [u8]) -> bool {
        self.read_block_checked_impl(src_addr, dest, true)
    }

    /// Same as `read_block_checked`, but suppresses diagnostics for callers
    /// with an expected fallback path.
    pub fn read_block_checked_quiet(&self, src_addr: u64, dest: &mut [u8]) -> bool {
        self.read_block_checked_impl(src_addr, dest, false)
    }

    fn read_block_checked_impl(&self, src_addr: u64, dest: &mut [u8], log_errors: bool) -> bool {
        let size = dest.len();
        if size == 0 {
            return true;
        }

        if !self.address_space_contains(src_addr, size) {
            if log_errors {
                log::error!(
                    "Unmapped checked ReadBlock @ {:#018x} size={:#x}",
                    src_addr,
                    size
                );
            }
            dest.fill(0);
            return false;
        }

        let self_pid = unsafe { libc::getpid() };
        let first_ptr = self.get_pointer_impl(src_addr);
        if !first_ptr.is_null() {
            let local_iov = libc::iovec {
                iov_base: dest.as_mut_ptr() as *mut libc::c_void,
                iov_len: size,
            };
            let remote_iov = libc::iovec {
                iov_base: first_ptr as *mut libc::c_void,
                iov_len: size,
            };
            let read = unsafe {
                libc::process_vm_readv(
                    self_pid,
                    &local_iov as *const _,
                    1,
                    &remote_iov as *const _,
                    1,
                    0,
                )
            };
            if read == size as isize {
                return true;
            }
            if log_errors {
                log::error!(
                    "checked ReadBlock fast path failed @ {:#018x} size={:#x} read={}",
                    src_addr,
                    size,
                    read
                );
            }
        }

        let mut remaining = size;
        let mut offset = 0usize;
        let mut vaddr = src_addr;
        let mut user_accessible = true;

        while remaining > 0 {
            let page_offset = (vaddr & PAGE_MASK) as usize;
            let copy_amount = ((PAGE_SIZE as usize) - page_offset).min(remaining);

            let ptr = self.get_pointer_impl(vaddr);
            if ptr.is_null() {
                if log_errors {
                    log::error!("Unmapped checked ReadBlock @ {:#018x}", vaddr);
                }
                dest[offset..offset + copy_amount].fill(0);
                user_accessible = false;
            } else {
                let local_iov = libc::iovec {
                    iov_base: dest[offset..].as_mut_ptr() as *mut libc::c_void,
                    iov_len: copy_amount,
                };
                let remote_iov = libc::iovec {
                    iov_base: ptr as *mut libc::c_void,
                    iov_len: copy_amount,
                };
                let read = unsafe {
                    libc::process_vm_readv(
                        self_pid,
                        &local_iov as *const _,
                        1,
                        &remote_iov as *const _,
                        1,
                        0,
                    )
                };
                if read != copy_amount as isize {
                    if log_errors {
                        log::error!(
                            "checked ReadBlock failed @ {:#018x} size={:#x} read={}",
                            vaddr,
                            copy_amount,
                            read
                        );
                    }
                    dest[offset..offset + copy_amount].fill(0);
                    user_accessible = false;
                }
            }

            vaddr += copy_amount as u64;
            offset += copy_amount;
            remaining -= copy_amount;
        }
        user_accessible
    }

    /// Write a block of data to guest memory.
    /// Matches upstream `Memory::WriteBlock` (via WalkBlock pattern).
    pub fn write_block(&self, dest_addr: u64, src: &[u8]) -> bool {
        let size = src.len();

        // RUZU_WATCH_BLOCK=START:LEN — emit a backtrace + first 64 bytes of
        // the source whenever a block write touches [START, START+LEN). Used
        // to find HLE-side writers of guest memory that the JIT
        // memory_write_NN watch doesn't see.
        if std::env::var_os("RUZU_WATCH_BLOCK").is_some() {
            check_block_watch("write_block", dest_addr, src);
        }

        // Upstream: AddressSpaceContains check before walking pages.
        if !self.address_space_contains(dest_addr, size) {
            log::error!("Unmapped WriteBlock @ {:#018x} size={:#x}", dest_addr, size);
            if std::env::var_os("RUZU_TRACE_UNMAPPED_BT").is_some() {
                use std::sync::atomic::{AtomicU32, Ordering};
                static SHOWN: AtomicU32 = AtomicU32::new(0);
                let n = SHOWN.fetch_add(1, Ordering::Relaxed);
                if n < 5 {
                    let bt = std::backtrace::Backtrace::force_capture();
                    eprintln!(
                        "[UNMAPPED_WB_BT #{}] dest=0x{:016X} size=0x{:X}\n{}",
                        n, dest_addr, size, bt
                    );
                }
            }
            return false;
        }

        let mut remaining = size;
        let mut offset = 0usize;
        let mut vaddr = dest_addr;
        let mut user_accessible = true;

        while remaining > 0 {
            let page_offset = (vaddr & PAGE_MASK) as usize;
            let copy_amount = ((PAGE_SIZE as usize) - page_offset).min(remaining);

            let ptr = self.get_pointer_impl(vaddr);
            if ptr.is_null() {
                log::error!("Unmapped WriteBlock @ {:#018x}", vaddr);
                user_accessible = false;
            } else {
                self.handle_rasterizer_write(vaddr, copy_amount);
                unsafe {
                    std::ptr::copy_nonoverlapping(src[offset..].as_ptr(), ptr, copy_amount);
                }
            }

            vaddr += copy_amount as u64;
            offset += copy_amount;
            remaining -= copy_amount;
        }
        user_accessible
    }

    /// Zero a block of guest memory.
    /// Matches upstream `Memory::ZeroBlock` (via WalkBlock pattern).
    ///
    /// For pages backed by HostMemory, batches contiguous physical runs and
    /// calls `HostMemory::clear_backing_region`, which uses `madvise(MADV_REMOVE)`
    /// on Linux — a single syscall reclaims the backing pages lazily instead of
    /// memset'ing every byte. This matches upstream's
    /// `DeviceMemory().buffer.ClearBackingRegion` path and avoids the O(size)
    /// memset that was ~26% of ruzu's CPU during MK8D boot.
    /// Zero a region of physical memory (DeviceMemory backing buffer).
    /// Used by KPageTableBase pool-allocation callers that need to clear
    /// pages before mapping them into virtual address space (so the VA
    /// mapping doesn't yet exist for `zero_block`'s VA path).
    ///
    /// Upstream calls `ClearBackingRegion(m_system, block.GetAddress(),
    /// block.GetSize(), m_heap_fill_value)` per block in the page group.
    /// `m_heap_fill_value` is typically zero in the standard build profile;
    /// upstream uses non-zero values only when developer poisoning is on.
    pub fn zero_phys_block(&self, phys_addr: u64, size: usize) {
        if size == 0 {
            return;
        }
        let buffer = unsafe { &*self.buffer };
        let dm = unsafe { &*self.device_memory };
        let backing_base = buffer.backing_base_pointer() as usize;
        let backing_size = buffer.backing_size();
        // Translate phys → host pointer (DeviceMemory is identity-mapped
        // from `dram_memory_map::BASE` into the host backing buffer).
        let dram_base = crate::device_memory::dram_memory_map::BASE;
        if phys_addr < dram_base {
            log::error!("zero_phys_block: phys {:#x} below DRAM base", phys_addr);
            return;
        }
        let host_offset = (phys_addr - dram_base) as usize;
        if host_offset
            .checked_add(size)
            .map(|end| end > backing_size)
            .unwrap_or(true)
        {
            log::error!(
                "zero_phys_block: phys {:#x}+{:#x} out of backing (backing_size={:#x})",
                phys_addr,
                size,
                backing_size
            );
            return;
        }
        let _ = dm; // silences unused warning if device_memory accessor not needed
        unsafe {
            std::ptr::write_bytes((backing_base + host_offset) as *mut u8, 0u8, size);
        }
    }

    pub fn zero_block(&self, dest_addr: u64, size: usize) -> bool {
        if !self.address_space_contains(dest_addr, size) {
            log::error!("Unmapped ZeroBlock @ {:#018x} size={:#x}", dest_addr, size);
            return false;
        }

        let buffer = unsafe { &*self.buffer };
        let backing_base = buffer.backing_base_pointer() as usize;
        let backing_size = buffer.backing_size();

        let mut remaining = size;
        let mut vaddr = dest_addr;
        let mut user_accessible = true;

        while remaining > 0 {
            let page_offset = (vaddr & PAGE_MASK) as usize;
            let first_chunk = ((PAGE_SIZE as usize) - page_offset).min(remaining);

            let first_ptr = self.get_pointer_impl(vaddr);
            if first_ptr.is_null() {
                log::error!("Unmapped ZeroBlock @ {:#018x}", vaddr);
                user_accessible = false;
                vaddr += first_chunk as u64;
                remaining -= first_chunk;
                continue;
            }

            // Extend the run as long as following pages are contiguous in host
            // memory (ptr advances exactly by the chunk size each step).
            let mut run = first_chunk;
            let mut cur_vaddr = vaddr + first_chunk as u64;
            let mut expected_ptr = first_ptr as usize + first_chunk;
            while run < remaining {
                let chunk = (PAGE_SIZE as usize).min(remaining - run);
                let p = self.get_pointer_impl(cur_vaddr);
                if p.is_null() || (p as usize) != expected_ptr {
                    break;
                }
                run += chunk;
                cur_vaddr += chunk as u64;
                expected_ptr += chunk;
            }

            self.handle_rasterizer_write(vaddr, run);

            let phys_off = (first_ptr as usize).wrapping_sub(backing_base);
            if phys_off < backing_size && phys_off + run <= backing_size {
                buffer.clear_backing_region(phys_off, run, 0);
            } else {
                // Not in the HostMemory backing buffer (debug/rasterizer paths):
                // fall back to memset.
                unsafe {
                    std::ptr::write_bytes(first_ptr, 0, run);
                }
            }

            vaddr += run as u64;
            remaining -= run;
        }
        user_accessible
    }

    /// Copy a block within guest memory.
    /// Matches upstream `Memory::CopyBlock`.
    pub fn copy_block(&self, dest_addr: u64, src_addr: u64, size: usize) -> bool {
        let mut buf = vec![0u8; size];
        self.read_block(src_addr, &mut buf);
        self.write_block(dest_addr, &buf)
    }

    /// Check if a virtual address is valid (mapped).
    /// Matches upstream `Memory::IsValidVirtualAddress`.
    pub fn is_valid_virtual_address(&self, vaddr: u64) -> bool {
        if self.current_page_table.is_null() {
            return false;
        }
        let pt = unsafe { &*self.current_page_table };
        let page = (vaddr >> PAGE_BITS) as usize;
        if page >= pt.pointers.size() {
            return false;
        }
        let (pointer, ptype) = pt.pointers[page].pointer_type();
        pointer != 0 || ptype == PageType::RasterizerCachedMemory || ptype == PageType::DebugMemory
    }

    /// Check if a virtual address range is valid (all pages mapped).
    /// Matches upstream `Memory::IsValidVirtualAddressRange`.
    pub fn is_valid_virtual_address_range(&self, base: u64, size: u64) -> bool {
        let end = base + size;
        let mut page = base & !(PAGE_MASK);
        while page < end {
            if !self.is_valid_virtual_address(page) {
                return false;
            }
            page += PAGE_SIZE;
        }
        true
    }

    // =========================================================================
    // Exclusive Write (atomic CAS) via PageTable pointers
    // Matches upstream Core::Memory::Memory::WriteExclusive* methods.
    // =========================================================================

    /// Exclusive write u8 with atomic CAS.
    /// Matches upstream `Memory::WriteExclusive8`.
    pub fn write_exclusive_8(&self, vaddr: u64, value: u8, expected: u8) -> bool {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped WriteExclusive8 @ {:#018x}", vaddr);
            return true;
        }
        unsafe {
            let atomic = &*(ptr as *const std::sync::atomic::AtomicU8);
            atomic
                .compare_exchange(
                    expected,
                    value,
                    std::sync::atomic::Ordering::SeqCst,
                    std::sync::atomic::Ordering::SeqCst,
                )
                .is_ok()
        }
    }

    /// Exclusive write u16 with atomic CAS.
    /// Matches upstream `Memory::WriteExclusive16`.
    pub fn write_exclusive_16(&self, vaddr: u64, value: u16, expected: u16) -> bool {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped WriteExclusive16 @ {:#018x}", vaddr);
            return true;
        }
        unsafe {
            let atomic = &*(ptr as *const std::sync::atomic::AtomicU16);
            atomic
                .compare_exchange(
                    expected,
                    value,
                    std::sync::atomic::Ordering::SeqCst,
                    std::sync::atomic::Ordering::SeqCst,
                )
                .is_ok()
        }
    }

    /// Exclusive write u32 with atomic CAS.
    /// Matches upstream `Memory::WriteExclusive32`.
    pub fn write_exclusive_32(&self, vaddr: u64, value: u32, expected: u32) -> bool {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped WriteExclusive32 @ {:#018x}", vaddr);
            return true;
        }
        unsafe {
            let atomic = &*(ptr as *const std::sync::atomic::AtomicU32);
            atomic
                .compare_exchange(
                    expected,
                    value,
                    std::sync::atomic::Ordering::SeqCst,
                    std::sync::atomic::Ordering::SeqCst,
                )
                .is_ok()
        }
    }

    /// Exclusive write u64 with atomic CAS.
    /// Matches upstream `Memory::WriteExclusive64`.
    pub fn write_exclusive_64(&self, vaddr: u64, value: u64, expected: u64) -> bool {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped WriteExclusive64 @ {:#018x}", vaddr);
            return true;
        }
        unsafe {
            let atomic = &*(ptr as *const std::sync::atomic::AtomicU64);
            atomic
                .compare_exchange(
                    expected,
                    value,
                    std::sync::atomic::Ordering::SeqCst,
                    std::sync::atomic::Ordering::SeqCst,
                )
                .is_ok()
        }
    }

    /// Exclusive write 128-bit. Matches upstream `Memory::WriteExclusive128`
    /// which calls `Common::AtomicCompareAndSwap` (= `_InterlockedCompareExchange128`
    /// on MSVC, `__sync_bool_compare_and_swap` on GCC/Clang for `__int128`).
    ///
    /// On x86_64 hosts this lowers to a single `lock cmpxchg16b` — atomic
    /// against any concurrent 8/16-byte load or store at the same 16-byte
    /// boundary. The previous two-step 64-bit CAS implementation was a
    /// structural divergence from upstream: it could observe or produce
    /// torn writes when another core read/wrote the same 16-byte slot
    /// between the two halves.
    #[cfg(target_arch = "x86_64")]
    pub fn write_exclusive_128(
        &self,
        vaddr: u64,
        value_lo: u64,
        value_hi: u64,
        expected_lo: u64,
        expected_hi: u64,
    ) -> bool {
        let ptr = self.get_pointer_impl(vaddr);
        if ptr.is_null() {
            log::error!("Unmapped WriteExclusive128 @ {:#018x}", vaddr);
            return true;
        }
        // 16-byte alignment is a hardware requirement for cmpxchg16b.
        // dynarmic only emits STXP for 16-byte-aligned vaddrs per ARM ARM.
        unsafe { cmpxchg16b(ptr, value_lo, value_hi, expected_lo, expected_hi) }
    }

    /// Non-x86_64 fallback: two 64-bit CAS (NOT atomic across the boundary).
    /// Matches the previous behavior on platforms without `cmpxchg16b`.
    #[cfg(not(target_arch = "x86_64"))]
    pub fn write_exclusive_128(
        &self,
        vaddr: u64,
        value_lo: u64,
        value_hi: u64,
        expected_lo: u64,
        expected_hi: u64,
    ) -> bool {
        let lo_ok = self.write_exclusive_64(vaddr, value_lo, expected_lo);
        if !lo_ok {
            return false;
        }
        self.write_exclusive_64(vaddr + 8, value_hi, expected_hi)
    }
}

/// Atomic 128-bit compare-and-swap on a 16-byte aligned pointer.
///
/// Returns `true` if `[ptr..ptr+16] == expected_lo:expected_hi` and the
/// 16-byte value was swapped to `value_lo:value_hi`. Returns `false`
/// otherwise (and memory is unchanged).
///
/// # Safety
/// - `ptr` must point to a 16-byte aligned, writable u128 location.
/// - Concurrent accesses to the same location must use compatible atomic
///   operations (lock cmpxchg16b, lock dec, etc.) to be sequentially consistent.
#[cfg(target_arch = "x86_64")]
#[inline]
unsafe fn cmpxchg16b(
    ptr: *mut u8,
    value_lo: u64,
    value_hi: u64,
    expected_lo: u64,
    expected_hi: u64,
) -> bool {
    debug_assert!(
        (ptr as usize) & 0xF == 0,
        "cmpxchg16b: misaligned ptr {:p}",
        ptr
    );
    let success: u8;
    std::arch::asm!(
        "xchg rbx, {rbx_save}",
        "lock cmpxchg16b [{ptr}]",
        "setz {success}",
        "mov rbx, {rbx_save}",
        ptr = in(reg) ptr,
        rbx_save = inout(reg) value_lo => _,
        inout("rax") expected_lo => _,
        inout("rdx") expected_hi => _,
        in("rcx") value_hi,
        success = lateout(reg_byte) success,
        options(nostack),
    );
    success != 0
}

#[cfg(test)]
mod cmpxchg16b_tests {
    #[cfg(target_arch = "x86_64")]
    use super::cmpxchg16b;

    #[cfg(target_arch = "x86_64")]
    #[repr(align(16))]
    struct Aligned16([u64; 2]);

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn success_on_match() {
        let mut slot = Aligned16([0xDEADBEEF_CAFEBABE, 0x0123456789ABCDEF]);
        let ok = unsafe {
            cmpxchg16b(
                slot.0.as_mut_ptr() as *mut u8,
                0x1111_2222_3333_4444,
                0x5555_6666_7777_8888,
                0xDEADBEEF_CAFEBABE,
                0x0123456789ABCDEF,
            )
        };
        assert!(ok);
        assert_eq!(slot.0[0], 0x1111_2222_3333_4444);
        assert_eq!(slot.0[1], 0x5555_6666_7777_8888);
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn failure_on_mismatch_leaves_memory_unchanged() {
        let mut slot = Aligned16([0x1111_1111_1111_1111, 0x2222_2222_2222_2222]);
        let ok = unsafe {
            cmpxchg16b(
                slot.0.as_mut_ptr() as *mut u8,
                0xAAAA,
                0xBBBB,
                0xDEAD, // wrong expected
                0xBEEF,
            )
        };
        assert!(!ok);
        assert_eq!(slot.0[0], 0x1111_1111_1111_1111);
        assert_eq!(slot.0[1], 0x2222_2222_2222_2222);
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn failure_on_hi_mismatch_only() {
        // The atomicity contract specifically requires that mismatching
        // EITHER half causes the swap to fail. The previous two-step
        // 64-bit-CAS implementation would partially succeed (lo swapped,
        // hi failed → memory left with new_lo + old_hi). True
        // cmpxchg16b leaves BOTH halves unchanged.
        let mut slot = Aligned16([0xAAAA, 0xBBBB]);
        let ok = unsafe {
            cmpxchg16b(
                slot.0.as_mut_ptr() as *mut u8,
                0x1111,
                0x2222,
                0xAAAA, // matches lo
                0xCCCC, // does NOT match hi
            )
        };
        assert!(!ok);
        assert_eq!(slot.0[0], 0xAAAA);
        assert_eq!(slot.0[1], 0xBBBB);
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn round_trip_all_ones_to_all_zeros_and_back() {
        let mut slot = Aligned16([0u64, 0u64]);
        let ok1 = unsafe { cmpxchg16b(slot.0.as_mut_ptr() as *mut u8, u64::MAX, u64::MAX, 0, 0) };
        assert!(ok1);
        assert_eq!(slot.0, [u64::MAX, u64::MAX]);

        let ok2 = unsafe { cmpxchg16b(slot.0.as_mut_ptr() as *mut u8, 0, 0, u64::MAX, u64::MAX) };
        assert!(ok2);
        assert_eq!(slot.0, [0, 0]);
    }
}

impl Memory {
    /// Invalidate a separate heap fault address.
    ///
    /// Upstream: `Memory::InvalidateSeparateHeap(void* fault_address)` (memory.cpp:1104).
    /// On Linux, delegates to `HeapTracker::DeferredMapSeparateHeap(fault_address)`.
    /// On non-Linux, returns false.
    pub fn invalidate_separate_heap(&self, fault_address: *const u8) -> bool {
        #[cfg(target_os = "linux")]
        {
            if let Some(ref heap_tracker) = self.heap_tracker {
                return heap_tracker.deferred_map_separate_heap(fault_address);
            }
            false
        }
        #[cfg(not(target_os = "linux"))]
        {
            let _ = fault_address;
            false
        }
    }

    /// Internal: update page table entries for a range of pages.
    ///
    /// Matches upstream `Memory::Impl::MapPages`.
    fn map_pages(
        &self,
        page_table: &mut PageTable,
        base_page: u64,
        num_pages: u64,
        mut target: u64,
        page_type: PageType,
    ) {
        let end = base_page + num_pages;
        debug_assert!(
            (end as usize) <= page_table.pointers.size(),
            "out of range mapping at {:#x}",
            base_page * PAGE_SIZE
        );

        if target == 0 {
            debug_assert!(
                page_type != PageType::Memory,
                "Mapping memory page without a pointer @ {:#x}",
                base_page * PAGE_SIZE
            );

            let mut page = base_page as usize;
            while page < end as usize {
                page_table.pointers[page].store(0usize, page_type);
                page_table.backing_addr[page] = 0u64;
                page_table.blocks[page] = 0u64;
                page += 1;
            }
        } else {
            let orig_base = base_page;
            let mut page = base_page as usize;
            while page < end as usize {
                // Compute host pointer: DeviceMemory base + physical offset - virtual page offset.
                // The result is intended to be used as host_ptr + page*PAGE_SIZE, so the per-iteration
                // delta is a constant — debug builds otherwise hit "subtract with overflow" when the
                // virtual page index is numerically larger than the physical offset (release-mode
                // wraparound is the intended behavior; restore it explicitly).
                let host_ptr = unsafe {
                    let dm = &*self.device_memory;
                    (dm.buffer.backing_base_pointer() as usize)
                        .wrapping_add((target - dram_memory_map::BASE) as usize)
                        .wrapping_sub(page << PAGE_BITS)
                };
                let backing = (target as usize).wrapping_sub(page << PAGE_BITS);

                page_table.pointers[page].store(host_ptr, page_type);
                page_table.backing_addr[page] = backing as u64;
                page_table.blocks[page] = orig_base << (PAGE_BITS as u64);

                page += 1;
                target += PAGE_SIZE;
            }
        }
    }
}
