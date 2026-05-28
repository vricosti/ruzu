//! Non-blocking trace logging.
//!
//! Two-layer design:
//! 1. `Config` singleton — loaded once from a TOML file pointed to by
//!    `RUZU_TRACE_CONFIG` (or `~/.config/ruzu/trace.toml`). Replaces the
//!    historical `RUZU_*` env vars; each legacy env var is still honoured
//!    as a fallback so existing scripts keep working.
//! 2. Per-thread record ring — emitters push fixed-size `LogRecord`s into
//!    a thread-local SPSC ring; a single drain thread copies records to the
//!    configured sink (stderr/file) and formats them according to the
//!    category id. No allocation, no `write(2)` syscall on the hot path.
//!
//! Parity goal: the wire format (text on stderr) matches what `eprintln!` /
//! `LOG_WARNING` produced before, so `scripts/ipc_diff.py` and friends keep
//! working without modification.

use parking_lot::Mutex;
use std::cell::UnsafeCell;
use std::io::Write;
use std::mem::MaybeUninit;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU16, AtomicUsize, Ordering};
use std::sync::{Arc, OnceLock};
use std::thread;
use std::time::{Duration, Instant};

// =============================================================================
// LogRecord — fixed-size carrier passed through the ring.
// =============================================================================

/// Fixed-size record copied through the ring. 128 bytes so it fits in two
/// cache lines on x86_64 and amortises well across rings of any size.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct LogRecord {
    pub category: u16,
    pub tid: u16,
    pub arg_count: u8,
    pub _pad0: [u8; 3],
    pub timestamp_ns: u64,
    pub args: [u64; 14],
}

const _: () = assert!(std::mem::size_of::<LogRecord>() == 128);

impl LogRecord {
    pub fn new(category: u16, tid: u16, args: &[u64]) -> Self {
        let mut rec = LogRecord {
            category,
            tid,
            arg_count: args.len().min(14) as u8,
            _pad0: [0; 3],
            timestamp_ns: monotonic_ns(),
            args: [0; 14],
        };
        for (i, &v) in args.iter().take(14).enumerate() {
            rec.args[i] = v;
        }
        rec
    }
}

fn monotonic_ns() -> u64 {
    static ANCHOR: OnceLock<Instant> = OnceLock::new();
    let anchor = ANCHOR.get_or_init(Instant::now);
    anchor.elapsed().as_nanos() as u64
}

// =============================================================================
// Category ids and registries.
// =============================================================================

pub mod cat {
    /// IPC reply hex-dump. args = [seq, svc_id, cmd, ioctl, words, w0, w1, w2, w3, w4, w5, w6, w7, w8]
    pub const IPC_REPLY: u16 = 1;
    /// IPC request hex-dump. Same layout as IPC_REPLY.
    pub const IPC_REQUEST: u16 = 2;
    /// nvhost_as_gpu::MapBufferEx. args = [seq, handle, size, gpu_va, flags, kind]
    pub const GPU_VA_MAP: u16 = 3;
    /// nvhost_as_gpu::UnmapBuffer. args = [seq, gpu_va]
    pub const GPU_VA_UNMAP: u16 = 4;
    /// SetHeapSize. args = [size, result, address]
    pub const HEAP: u16 = 5;
    /// Per-tid SVC tracer. args = [t_ns, tid, core, svc_name_id, pc, lr, a0, a1, a2, a3]
    pub const TID_SVC: u16 = 6;
    /// JIT memory read watchpoint. args = [core, tid, pc, lr, vaddr, size, value_lo, value_hi]
    pub const WATCH_READ: u16 = 7;
    /// JIT memory write watchpoint. Same layout as WATCH_READ.
    pub const WATCH_WRITE: u16 = 8;
    /// STLEX (exclusive store) attempt. args = [pc, vaddr, value, expected, ok]
    pub const STLEX: u16 = 9;
    /// Single-cell unmapped JIT write. args = [tid, pc, lr, vaddr, size, value_lo, value_hi]
    pub const UNMAPPED_WRITE: u16 = 10;
    /// BL/PLT scan hit. args = [pc, target, kind_id, cond]
    /// kind_id: 0=B, 1=BL, 2=BLX
    pub const BL_HIT: u16 = 11;
    /// Handle attribution. args = [svc_id, cmd, value, value2, kind, has_value2]
    /// kind 0 (raw): value=handle, value2=client_valid(0/1)
    /// kind 1 (add): value=object_id, value2=new_handle (when has_value2=1)
    pub const IPC_HANDLE_OUT: u16 = 12;
    /// Domain object id emission. args = [svc_id, cmd, offset, domain_object_id]
    pub const IPC_DOMAIN_OUT: u16 = 13;
}

fn service_registry() -> &'static Mutex<Vec<String>> {
    static REG: OnceLock<Mutex<Vec<String>>> = OnceLock::new();
    REG.get_or_init(|| Mutex::new(Vec::with_capacity(64)))
}

/// Intern a service name into a u16 id. Called from the IPC dispatch site at
/// most a few times per service. Behind a Mutex because this is genuinely
/// rare; the hot path receives the already-interned id.
pub fn intern_service(name: &str) -> u16 {
    let mut g = service_registry().lock();
    if let Some(idx) = g.iter().position(|s| s == name) {
        return idx as u16;
    }
    g.push(name.to_string());
    (g.len() - 1) as u16
}

fn service_name(id: u16) -> String {
    let g = service_registry().lock();
    g.get(id as usize)
        .cloned()
        .unwrap_or_else(|| format!("svc#{id}"))
}

fn svc_name_registry() -> &'static Mutex<Vec<String>> {
    static REG: OnceLock<Mutex<Vec<String>>> = OnceLock::new();
    REG.get_or_init(|| Mutex::new(Vec::with_capacity(128)))
}

/// Intern an SVC name (e.g. "QueryMemory") into a stable u16 id. Separate
/// from `intern_service` so id spaces don't collide.
pub fn intern_svc_name(name: &str) -> u16 {
    let mut g = svc_name_registry().lock();
    if let Some(idx) = g.iter().position(|s| s == name) {
        return idx as u16;
    }
    g.push(name.to_string());
    (g.len() - 1) as u16
}

fn svc_name(id: u16) -> String {
    let g = svc_name_registry().lock();
    g.get(id as usize)
        .cloned()
        .unwrap_or_else(|| format!("svc#0x{:02X}", id))
}

// =============================================================================
// SPSC ring — one per emitting thread.
// =============================================================================

/// Single-producer single-consumer ring buffer. Producer is the owning
/// thread; consumer is the drain thread. Lock-free with two atomics; the
/// producer never blocks (records are dropped if the ring is full).
struct SpscRing {
    buf: Box<[UnsafeCell<MaybeUninit<LogRecord>>]>,
    capacity: usize,
    head: AtomicUsize, // producer writes, consumer reads
    tail: AtomicUsize, // consumer writes, producer reads
    drops: AtomicUsize,
}

// SAFETY: SpscRing is shared between the owning thread (producer) and the
// drain thread (consumer); the atomic head/tail discipline ensures that the
// two sides never touch the same slot at the same time.
unsafe impl Sync for SpscRing {}
unsafe impl Send for SpscRing {}

impl SpscRing {
    fn new(capacity: usize) -> Self {
        let mut v = Vec::with_capacity(capacity);
        for _ in 0..capacity {
            v.push(UnsafeCell::new(MaybeUninit::uninit()));
        }
        Self {
            buf: v.into_boxed_slice(),
            capacity,
            head: AtomicUsize::new(0),
            tail: AtomicUsize::new(0),
            drops: AtomicUsize::new(0),
        }
    }

    /// Called by the owning thread. Returns false if the ring is full (record
    /// dropped). Never blocks.
    fn push(&self, rec: LogRecord) -> bool {
        let head = self.head.load(Ordering::Relaxed);
        let next = (head + 1) % self.capacity;
        if next == self.tail.load(Ordering::Acquire) {
            self.drops.fetch_add(1, Ordering::Relaxed);
            return false;
        }
        // SAFETY: only the producer writes to slot[head]; consumer cannot
        // touch it until we publish via the head.store(Release) below.
        unsafe {
            (*self.buf[head].get()).write(rec);
        }
        self.head.store(next, Ordering::Release);
        true
    }

    /// Called by the drain thread.
    fn pop(&self) -> Option<LogRecord> {
        let tail = self.tail.load(Ordering::Relaxed);
        if tail == self.head.load(Ordering::Acquire) {
            return None;
        }
        // SAFETY: producer has published this slot; we own read-only access.
        let rec = unsafe { (*self.buf[tail].get()).assume_init_read() };
        self.tail.store((tail + 1) % self.capacity, Ordering::Release);
        Some(rec)
    }
}

// =============================================================================
// Ring registry — every emitting thread registers its ring once.
// =============================================================================

static REGISTRY: OnceLock<Mutex<Vec<Arc<SpscRing>>>> = OnceLock::new();

fn registry() -> &'static Mutex<Vec<Arc<SpscRing>>> {
    REGISTRY.get_or_init(|| Mutex::new(Vec::with_capacity(64)))
}

thread_local! {
    static MY_RING: std::cell::RefCell<Option<Arc<SpscRing>>> = const { std::cell::RefCell::new(None) };
}

fn my_ring() -> Arc<SpscRing> {
    MY_RING.with(|slot| {
        let mut borrow = slot.borrow_mut();
        if let Some(ring) = borrow.as_ref() {
            return ring.clone();
        }
        let ring = Arc::new(SpscRing::new(config().ring_capacity));
        registry().lock().push(ring.clone());
        *borrow = Some(ring.clone());
        ring
    })
}

// =============================================================================
// Config — TOML file singleton with env var fallbacks.
// =============================================================================

#[derive(Debug, Clone)]
pub struct Config {
    pub ipc_reply_dump: bool,
    pub ipc_request_dump: bool,
    pub gpu_va_trace: bool,
    pub heap_trace: bool,
    pub tid_svc_trace: bool,
    pub tid_svc_pc: bool,
    pub watch_read_write: bool,
    pub stlex_trace: bool,
    pub unmapped_write_trace: bool,
    pub bl_hit_trace: bool,
    pub ipc_handle_out: bool,
    pub ipc_domain_out: bool,
    /// Sink target: "stderr" or "file".
    pub output_target: String,
    /// File path when target == "file".
    pub output_file: String,
    /// Per-ring capacity (records). Total memory = N_threads * cap * 128B.
    pub ring_capacity: usize,
    /// Drain thread wakeup interval in microseconds.
    pub drain_interval_us: u64,
    /// If true, producers block (busy-wait) when ring is full instead of dropping.
    /// Default false — we prioritise non-blocking.
    pub lossless: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            ipc_reply_dump: false,
            ipc_request_dump: false,
            gpu_va_trace: false,
            heap_trace: false,
            tid_svc_trace: false,
            tid_svc_pc: false,
            watch_read_write: false,
            stlex_trace: false,
            unmapped_write_trace: false,
            bl_hit_trace: false,
            ipc_handle_out: false,
            ipc_domain_out: false,
            output_target: "stderr".to_string(),
            output_file: String::new(),
            ring_capacity: 16384,
            drain_interval_us: 1000,
            lossless: false,
        }
    }
}

/// Resolve config file path: `RUZU_TRACE_CONFIG` env var, else
/// `$XDG_CONFIG_HOME/ruzu/trace.toml`, else `$HOME/.config/ruzu/trace.toml`.
fn config_path() -> Option<PathBuf> {
    if let Ok(p) = std::env::var("RUZU_TRACE_CONFIG") {
        if !p.is_empty() {
            return Some(PathBuf::from(p));
        }
    }
    let base = std::env::var("XDG_CONFIG_HOME")
        .map(PathBuf::from)
        .ok()
        .or_else(|| std::env::var("HOME").ok().map(|h| PathBuf::from(h).join(".config")))?;
    let candidate = base.join("ruzu").join("trace.toml");
    candidate.exists().then_some(candidate)
}

fn load_from_file() -> Option<toml::Value> {
    let path = config_path()?;
    let bytes = std::fs::read_to_string(&path).ok()?;
    toml::from_str::<toml::Value>(&bytes).ok()
}

fn env_set(key: &str) -> bool {
    std::env::var_os(key).is_some()
}

fn env_string(key: &str) -> Option<String> {
    std::env::var(key).ok().filter(|s| !s.is_empty())
}

fn build_config() -> Config {
    let file = load_from_file();
    let get_bool = |section: &str, field: &str, env_key: &str, default: bool| -> bool {
        if let Some(v) = file
            .as_ref()
            .and_then(|t| t.get(section))
            .and_then(|s| s.get(field))
            .and_then(|x| x.as_bool())
        {
            return v;
        }
        if env_set(env_key) {
            return true;
        }
        default
    };
    let get_str = |section: &str, field: &str, env_key: &str, default: &str| -> String {
        if let Some(v) = file
            .as_ref()
            .and_then(|t| t.get(section))
            .and_then(|s| s.get(field))
            .and_then(|x| x.as_str())
        {
            return v.to_string();
        }
        if let Some(v) = env_string(env_key) {
            return v;
        }
        default.to_string()
    };
    let get_u64 = |section: &str, field: &str, default: u64| -> u64 {
        file.as_ref()
            .and_then(|t| t.get(section))
            .and_then(|s| s.get(field))
            .and_then(|x| x.as_integer())
            .map(|n| n.max(0) as u64)
            .unwrap_or(default)
    };
    let cfg = Config {
        ipc_reply_dump: get_bool("ipc", "reply_dump", "RUZU_IPC_REPLY_DUMP", false),
        ipc_request_dump: get_bool("ipc", "request_dump", "RUZU_IPC_REQUEST_DUMP", false),
        gpu_va_trace: get_bool("nvdrv", "gpu_va_trace", "RUZU_TRACE_GPU_VA", false),
        heap_trace: get_bool("svc", "heap_trace", "RUZU_TRACE_HEAP", false),
        tid_svc_trace: get_bool("svc", "tid_svc", "RUZU_TRACE_TID_SVC", false),
        tid_svc_pc: get_bool("svc", "tid_svc_pc", "RUZU_TRACE_TID_SVC_PC", false),
        watch_read_write: get_bool("jit", "watch_read_write", "RUZU_WATCH_ENABLED", false),
        stlex_trace: get_bool("jit", "stlex", "RUZU_TRACE_STLEX", false),
        unmapped_write_trace: get_bool("jit", "unmapped_write", "RUZU_TRACE_UNMAPPED_WRITE", false),
        bl_hit_trace: get_bool("jit", "bl_hit", "RUZU_TRACE_BL_HIT", false),
        ipc_handle_out: get_bool("ipc", "handle_out", "RUZU_IPC_HANDLE_OUT", false),
        ipc_domain_out: get_bool("ipc", "domain_out", "RUZU_IPC_DOMAIN_OUT", false),
        output_target: get_str("output", "target", "RUZU_TRACE_TARGET", "stderr"),
        output_file: get_str("output", "file_path", "RUZU_TRACE_FILE", ""),
        ring_capacity: get_u64("output", "ring_capacity", 16384) as usize,
        drain_interval_us: get_u64("output", "drain_interval_us", 1000),
        lossless: get_bool("output", "lossless", "RUZU_TRACE_LOSSLESS", false),
    };
    cfg
}

pub fn config() -> &'static Config {
    static CFG: OnceLock<Config> = OnceLock::new();
    CFG.get_or_init(|| {
        let cfg = build_config();
        // Spawn drain thread lazily, on first config access. Idempotent.
        ensure_drain_thread(&cfg);
        cfg
    })
}

/// Cheap predicate for the hot path. The OnceLock load is one acquire load.
#[inline]
pub fn is_enabled(category: u16) -> bool {
    let c = config();
    match category {
        cat::IPC_REPLY => c.ipc_reply_dump,
        cat::IPC_REQUEST => c.ipc_request_dump,
        cat::GPU_VA_MAP | cat::GPU_VA_UNMAP => c.gpu_va_trace,
        cat::HEAP => c.heap_trace,
        cat::TID_SVC => c.tid_svc_trace,
        cat::WATCH_READ | cat::WATCH_WRITE => c.watch_read_write,
        cat::STLEX => c.stlex_trace,
        cat::UNMAPPED_WRITE => c.unmapped_write_trace,
        cat::BL_HIT => c.bl_hit_trace,
        cat::IPC_HANDLE_OUT => c.ipc_handle_out,
        cat::IPC_DOMAIN_OUT => c.ipc_domain_out,
        _ => false,
    }
}

/// True if the TID_SVC record should also include PC/LR fields. Read once
/// by the dispatch site to skip the JIT context lookup when unwanted.
#[inline]
pub fn tid_svc_pc_enabled() -> bool {
    config().tid_svc_pc
}

// =============================================================================
// Emit — the only function the hot path needs.
// =============================================================================

/// Push a record. Non-blocking by default; if `lossless = true` the producer
/// busy-waits while the ring is full. Returns true on success.
#[inline]
pub fn emit(category: u16, args: &[u64]) -> bool {
    if !is_enabled(category) {
        return false;
    }
    emit_raw(category, args)
}

/// Emit without re-checking the config. Use when the caller already knows
/// the category is enabled (saves one branch on the hot path).
#[inline]
pub fn emit_raw(category: u16, args: &[u64]) -> bool {
    let tid = current_tid();
    let rec = LogRecord::new(category, tid, args);
    let ring = my_ring();
    if config().lossless {
        while !ring.push(rec) {
            std::hint::spin_loop();
        }
        true
    } else {
        ring.push(rec)
    }
}

fn current_tid() -> u16 {
    // We re-use the OS thread id rather than going through the guest kernel
    // bookkeeping — the trace system has to stay decoupled from the rest of
    // the engine, otherwise it cannot trace early-boot.
    thread_local! {
        static TID: std::cell::Cell<u16> = const { std::cell::Cell::new(0) };
    }
    TID.with(|t| {
        let v = t.get();
        if v != 0 {
            return v;
        }
        static NEXT: AtomicU16 = AtomicU16::new(1);
        let nv = NEXT.fetch_add(1, Ordering::Relaxed);
        t.set(nv);
        nv
    })
}

// =============================================================================
// Drain thread — formats records and writes them to the configured sink.
// =============================================================================

static DRAIN_STARTED: AtomicBool = AtomicBool::new(false);

fn ensure_drain_thread(cfg: &Config) {
    if DRAIN_STARTED.swap(true, Ordering::AcqRel) {
        return;
    }
    let interval = Duration::from_micros(cfg.drain_interval_us);
    let target = cfg.output_target.clone();
    let file_path = cfg.output_file.clone();
    thread::Builder::new()
        .name("ruzu-trace-drain".into())
        .spawn(move || drain_loop(interval, target, file_path))
        .expect("spawn drain thread");
}

fn drain_loop(interval: Duration, target: String, file_path: String) {
    let mut sink: Box<dyn Write + Send> = match target.as_str() {
        "file" if !file_path.is_empty() => Box::new(
            std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&file_path)
                .unwrap_or_else(|e| panic!("trace: cannot open {file_path}: {e}")),
        ),
        _ => Box::new(std::io::stderr()),
    };
    let mut buf = String::with_capacity(4096);
    loop {
        let rings: Vec<Arc<SpscRing>> = registry().lock().clone();
        let mut drained_any = false;
        for ring in &rings {
            while let Some(rec) = ring.pop() {
                buf.clear();
                format_into(&mut buf, &rec);
                let _ = sink.write_all(buf.as_bytes());
                drained_any = true;
            }
        }
        if drained_any {
            // Force flush so SIGTERM at the end of a timed run doesn't lose
            // records still buffered in stderr/the file handle.
            let _ = sink.flush();
        } else {
            thread::sleep(interval);
        }
    }
}

fn format_into(out: &mut String, rec: &LogRecord) {
    use std::fmt::Write as _;
    match rec.category {
        cat::IPC_REPLY => {
            let svc = service_name(rec.args[1] as u16);
            let _ = write!(out, "IPC_REPLY seq={} service={} cmd={}", rec.args[0], svc, rec.args[2]);
            let ioctl = rec.args[3] as u32;
            if ioctl != 0 {
                let _ = write!(out, " ioctl=0x{:08X}", ioctl);
            }
            let _ = write!(out, " words={} payload=", rec.args[4]);
            for i in 5..rec.arg_count as usize {
                let _ = write!(out, "{:08x} ", rec.args[i] as u32);
            }
            out.pop(); // trailing space
            out.push('\n');
        }
        cat::IPC_REQUEST => {
            let svc = service_name(rec.args[1] as u16);
            let _ = write!(out, "IPC_REQUEST seq={} service={} cmd={}", rec.args[0], svc, rec.args[2]);
            let ioctl = rec.args[3] as u32;
            if ioctl != 0 {
                let _ = write!(out, " ioctl=0x{:08X}", ioctl);
            }
            out.push_str(" payload=");
            for i in 4..rec.arg_count as usize {
                let _ = write!(out, "{:08x} ", rec.args[i] as u32);
            }
            out.pop();
            out.push('\n');
        }
        cat::GPU_VA_MAP => {
            let _ = writeln!(
                out,
                "GPU_VA seq={} op=map handle=0x{:X} size=0x{:X} gpu_va=0x{:X} flags=0x{:X} kind=0x{:X}",
                rec.args[0], rec.args[1], rec.args[2], rec.args[3], rec.args[4], rec.args[5]
            );
        }
        cat::GPU_VA_UNMAP => {
            let _ = writeln!(out, "GPU_VA seq={} op=unmap gpu_va=0x{:X}", rec.args[0], rec.args[1]);
        }
        cat::HEAP => {
            let _ = writeln!(
                out,
                "SVC_SET_HEAP_SIZE size=0x{:X} -> result=0x{:X} address=0x{:016X}",
                rec.args[0], rec.args[1], rec.args[2]
            );
        }
        cat::TID_SVC => {
            // args = [t_ns, tid, core, svc_name_id, pc, lr, a0, a1, a2, a3]
            let name = svc_name(rec.args[3] as u16);
            let _ = write!(
                out,
                "[TID_SVC] t_ns={} tid={} core={} svc={}",
                rec.args[0], rec.args[1], rec.args[2], name
            );
            let pc = rec.args[4] as u32;
            let lr = rec.args[5] as u32;
            if pc != 0 || lr != 0 {
                let _ = write!(out, " pc=0x{:08X} lr=0x{:08X}", pc, lr);
            }
            let _ = writeln!(
                out,
                " args=[0x{:X}, 0x{:X}, 0x{:X}, 0x{:X}]",
                rec.args[6], rec.args[7], rec.args[8], rec.args[9]
            );
        }
        cat::WATCH_READ => {
            let _ = writeln!(
                out,
                "[WATCH_READ ] core={} tid={} pc=0x{:08X} lr=0x{:08X} vaddr=0x{:X} size={} value=0x{:016X}{:016X}",
                rec.args[0] as i32,
                rec.args[1],
                rec.args[2] as u32,
                rec.args[3] as u32,
                rec.args[4],
                rec.args[5],
                rec.args[7],
                rec.args[6],
            );
        }
        cat::WATCH_WRITE => {
            let _ = writeln!(
                out,
                "[WATCH_WRITE] core={} tid={} pc=0x{:08X} lr=0x{:08X} vaddr=0x{:X} size={} value=0x{:016X}{:016X}",
                rec.args[0] as i32,
                rec.args[1],
                rec.args[2] as u32,
                rec.args[3] as u32,
                rec.args[4],
                rec.args[5],
                rec.args[7],
                rec.args[6],
            );
        }
        cat::STLEX => {
            let _ = writeln!(
                out,
                "[STLEX      ] pc=0x{:08X} vaddr=0x{:X} value=0x{:08X} expected=0x{:08X} ok={}",
                rec.args[0] as u32,
                rec.args[1],
                rec.args[2] as u32,
                rec.args[3] as u32,
                if rec.args[4] != 0 { "true" } else { "false" }
            );
        }
        cat::UNMAPPED_WRITE => {
            let _ = writeln!(
                out,
                "[UNMAPPED_WRITE] tid={} pc=0x{:08X} lr=0x{:08X} vaddr=0x{:X} size={} value=0x{:016X}{:016X}",
                rec.args[0],
                rec.args[1] as u32,
                rec.args[2] as u32,
                rec.args[3],
                rec.args[4],
                rec.args[6],
                rec.args[5],
            );
        }
        cat::BL_HIT => {
            let kind = match rec.args[2] {
                1 => "BL ",
                2 => "BLX",
                _ => "B  ",
            };
            let _ = writeln!(
                out,
                "[BL_HIT] pc=0x{:08X} target=0x{:08X} kind={} cond=0x{:X}",
                rec.args[0] as u32, rec.args[1] as u32, kind, rec.args[3]
            );
        }
        cat::IPC_HANDLE_OUT => {
            let svc = service_name(rec.args[0] as u16);
            match rec.args[4] {
                1 => {
                    // kind=add. args[5]=1 if new_handle present, 0 if None.
                    if rec.args[5] != 0 {
                        let _ = writeln!(
                            out,
                            "[IPC_HANDLE_OUT] kind=add service={} cmd={} object_id=0x{:X} new_handle=Some(\"0x{:08X}\")",
                            svc, rec.args[1], rec.args[2], rec.args[3] as u32,
                        );
                    } else {
                        let _ = writeln!(
                            out,
                            "[IPC_HANDLE_OUT] kind=add service={} cmd={} object_id=0x{:X} new_handle=None",
                            svc, rec.args[1], rec.args[2],
                        );
                    }
                }
                _ => {
                    let _ = writeln!(
                        out,
                        "[IPC_HANDLE_OUT] kind=raw service={} cmd={} handle=0x{:08X} client_valid={}",
                        svc,
                        rec.args[1],
                        rec.args[2] as u32,
                        if rec.args[3] != 0 { "true" } else { "false" }
                    );
                }
            }
        }
        cat::IPC_DOMAIN_OUT => {
            let svc = service_name(rec.args[0] as u16);
            let _ = writeln!(
                out,
                "[IPC_DOMAIN_OUT] service={} cmd={} offset={} domain_object_id={}",
                svc, rec.args[1], rec.args[2], rec.args[3]
            );
        }
        _ => {
            let _ = writeln!(out, "TRACE cat={} args={:?}", rec.category, &rec.args[..rec.arg_count as usize]);
        }
    }
}

// =============================================================================
// Tests.
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn record_is_128_bytes() {
        assert_eq!(std::mem::size_of::<LogRecord>(), 128);
    }

    #[test]
    fn spsc_push_pop_roundtrip() {
        let ring = SpscRing::new(8);
        let rec = LogRecord::new(cat::IPC_REPLY, 1, &[42, 1, 0, 0, 0]);
        assert!(ring.push(rec));
        let out = ring.pop().unwrap();
        assert_eq!(out.args[0], 42);
        assert_eq!(out.category, cat::IPC_REPLY);
        assert!(ring.pop().is_none());
    }

    #[test]
    fn spsc_drops_when_full() {
        let ring = SpscRing::new(4);
        let rec = LogRecord::new(cat::IPC_REPLY, 1, &[1]);
        // Capacity 4 means 3 usable slots (one slot reserved to distinguish full from empty).
        assert!(ring.push(rec));
        assert!(ring.push(rec));
        assert!(ring.push(rec));
        assert!(!ring.push(rec));
        assert_eq!(ring.drops.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn service_interning_is_stable() {
        let a = intern_service("nvdrv");
        let b = intern_service("nvdrv");
        let c = intern_service("sm:");
        assert_eq!(a, b);
        assert_ne!(a, c);
    }
}
