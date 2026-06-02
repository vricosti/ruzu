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
    /// Host-thread IPC routing stages. args = [stage_id, session_handle]
    pub const HOST_THREAD_IPC: u16 = 14;
    /// Fresh-session IPC diagnostic. args = [kind, guest_tid, client_obj, ss_ptr, wakeup_ptr,
    /// needs_setup, is_signaled, req_len, cur_req, mgr_wakeup_live]
    pub const PLU_IPC: u16 = 15;
    /// nvhost_ctrl::IocCtrlEventWait. args = [stage_id, syncpt_id, threshold, timeout,
    /// is_allocation, slot, value_raw, result_id, min_value, target]
    pub const NVHOST_CTRL_WAIT: u16 = 16;
    /// Scheduler state/PQ update. args = [stage, tid, old_state, new_state, prio, active_core,
    /// top0, top1, top2, top3]
    pub const SCHED_STATE: u16 = 17;
    /// WaitSynchronization object attribution. args =
    /// [stage, guest_tid, num_handles, timeout_ns, result, out_index,
    ///  h0, oid0, kind0, signaled0, h1, oid1, kind1, signaled1]
    pub const WAIT_SYNC: u16 = 18;
    /// nvnflinger hardware-composer / nvdisp present path. args are stage-specific:
    /// stage=1 acquire [stage, frame, consumer, status, slot, item_frame, swap_interval, release_frame, is_acquired]
    /// stage=2 hwc_layer [stage, frame, consumer, handle, offset, width, height, stride, format, transform, crop_l, crop_t, crop_r, crop_b]
    /// stage=3 nvdisp_layer [stage, index, handle, address, offset, width, height, stride, format, transform, crop_l, crop_t, crop_r, crop_b]
    pub const HWC: u16 = 19;
    /// OpenGL sampled texture binding attribution. args =
    /// [draw_seq, pipeline, stage, unit, desc_type, view_id, view_type, image_id,
    ///  gl_handle, width, height, depth, sample0_rgba, sample_mid_rgba]
    pub const TEXTURE_BIND: u16 = 20;
    /// OpenGL graphics constant-buffer binding attribution. One record is emitted
    /// per sampled vec4. args =
    /// [draw_seq, pipeline, stage, slot, binding_index, vec4_index, gpu_addr, size,
    ///  used_size, enabled, f32_0_bits, f32_1_bits, f32_2_bits, f32_3_bits]
    pub const CBUF_BIND: u16 = 21;
    /// OpenGL draw render-target attribution. args =
    /// [draw_seq, pipeline, fbo, width, height, rt_count, rt_map_pack,
    ///  rt0_gpu, rt0_format, rt0_size_pack, rt1_gpu, rt1_format, rt1_size_pack,
    ///  surface_size_pack]
    pub const RT_BIND: u16 = 22;
    /// Filtered OpenGL render-target readback. args =
    /// [draw_seq, pipeline, fbo, rt0_gpu, rt0_format, width, height,
    ///  sample_width, sample_height, rgb_nonzero, alpha_nonzero, checksum,
    ///  first_rgba, gl_error, center_rgba]
    pub const RT_SAMPLE: u16 = 23;
    /// OpenGL texture binding address attribution. args =
    /// [draw_seq, pipeline, stage, unit, view_id, image_id, view_gpu_addr,
    ///  width, height, sample0_rgba, sample_mid_rgba]
    pub const TEXTURE_BIND_ADDR: u16 = 24;
    /// Filtered OpenGL render-target sparse-grid readback. args =
    /// [draw_seq, pipeline, fbo, rt0_gpu, width, height, grid_w, grid_h,
    ///  hit_cells, nonzero_bytes, first_xy_pack, first_rgba, last_xy_pack, last_rgba]
    pub const RT_GRID: u16 = 25;
    /// OpenGL draw-state attribution. stage-specific args:
    /// stage=1 fbo/attachment [stage, draw_seq, pipeline, fbo, status, drawbuf0, attached,
    ///   attached_type, attached_level, attached_layer, attached_layered]
    /// stage=2 kill-state [stage, draw_seq, pipeline, rasterizer_discard, color_logic_op,
    ///   alpha_test, depth_clamp, primitive_restart, primitive_restart_fixed, sample_mask,
    ///   sample_mask_value, clip_mask, transform_feedback_active, transform_feedback_paused]
    /// stage=3 output-state [stage, draw_seq, pipeline, color_mask_pack, blend0, scissor0,
    ///   depth_test, depth_mask, stencil_test, cull_face, front_face, cull_face_mode,
    ///   polygon_offset_fill, framebuffer_srgb]
    /// stage=4 draw-result [stage, draw_seq, pipeline, query_created, any_samples,
    ///   gl_error_after_draw, indexed, primitive, vertices, instances]
    /// stage=5 draw-params [stage, draw_seq, pipeline, indexed, primitive, vertices,
    ///   instances, base_vertex, base_instance, index_offset, index_format,
    ///   vertex_first, vertex_count, index_first]
    pub const GL_DRAW_STATE: u16 = 26;
    /// Filtered OpenGL render-target sparse-grid readback with explicit phase.
    /// args = [phase, draw_seq, pipeline, fbo, rt0_gpu, width, height,
    ///  hit_cells, nonzero_bytes, first_xy_pack, first_rgba, last_xy_pack,
    ///  last_rgba, gl_error]
    pub const RT_GRID_PHASE: u16 = 27;
    /// OpenGL texture-cache image/view lifecycle attribution. args =
    /// [stage, view_id, image_id, image_texture, current_texture, default_handle,
    ///  color2d_handle, color_array2d_handle, view_type, format, size_pack,
    ///  gpu_addr, range_pack, flags]
    pub const IMAGE_VIEW: u16 = 28;
    /// Audio renderer device-sink sample attribution. args =
    /// [seq, sample_buffer, input_count, buffer_count, sample_count, frames,
    ///  min_i32, max_i32, nonzero, clipped, visited, first0, first1, first2]
    pub const AUDIO_DEVICE_SINK: u16 = 29;
    /// Host1x/NVDEC/VIC video pipeline attribution. stage-specific args:
    /// stage=1 nvdec submit [stage, fd, cmd_buffers, relocs, syncpts, fences, first_gpu, first_words]
    /// stage=2 vic submit [stage, fd, cmd_buffers, relocs, syncpts, fences, first_gpu, first_words]
    /// stage=3 nvdec execute [stage, id, codec, initialized]
    /// stage=4 decode_api [stage, op, codec, result, packet_size]
    /// stage=5 vic execute [stage, id, config_addr]
    pub const HOST1X_VIDEO: u16 = 30;
    /// Invalid IPC command buffer attribution. args =
    /// [seq, service_id, cmd, guest_tid, tls, w0, w1, w2, w3, w4, w5, w6, w7, w8]
    pub const IPC_INVALID: u16 = 31;
    /// IPC reply wake attribution. args =
    /// [seq, client_tid, state, wait_reason, wait_queue_present, guard_armed,
    ///  guard_pending, result, request_addr, request_size]
    pub const IPC_REPLY_WAKE: u16 = 32;
    /// BufferQueueProducer attribution. stage-specific args:
    /// stage=1 dequeue_enter [stage, seq, async, width, height, format, usage]
    /// stage=2 dequeue_return [stage, seq, status, slot, flags]
    /// stage=3 queue_enter [stage, seq, slot]
    /// stage=4 queue_commit [stage, seq, slot, frame, queue_len, is_droppable, acquire_called]
    /// stage=5 queue_return [stage, seq, status, slot]
    /// stage=6 cancel [stage, seq, slot]
    pub const BQP: u16 = 33;
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
    pub ipc_invalid_trace: bool,
    pub ipc_reply_wake_trace: bool,
    pub bqp_trace: bool,
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
    pub host_thread_ipc_trace: bool,
    pub plu_ipc_trace: bool,
    pub nvhost_ctrl_wait_trace: bool,
    pub sched_state_trace: bool,
    pub wait_sync_trace: bool,
    pub hwc_trace: bool,
    pub texture_bind_trace: bool,
    pub cbuf_bind_trace: bool,
    pub rt_bind_trace: bool,
    pub rt_sample_trace: bool,
    pub texture_bind_addr_trace: bool,
    pub gl_draw_state_trace: bool,
    pub image_view_trace: bool,
    pub audio_device_sink_trace: bool,
    pub host1x_video_trace: bool,
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
            ipc_invalid_trace: false,
            ipc_reply_wake_trace: false,
            bqp_trace: false,
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
            host_thread_ipc_trace: false,
            plu_ipc_trace: false,
            nvhost_ctrl_wait_trace: false,
            sched_state_trace: false,
            wait_sync_trace: false,
            hwc_trace: false,
            texture_bind_trace: false,
            cbuf_bind_trace: false,
            rt_bind_trace: false,
            rt_sample_trace: false,
            texture_bind_addr_trace: false,
            gl_draw_state_trace: false,
            image_view_trace: false,
            audio_device_sink_trace: false,
            host1x_video_trace: false,
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
        ipc_invalid_trace: get_bool("ipc", "invalid", "RUZU_TRACE_IPC_INVALID", false),
        ipc_reply_wake_trace: get_bool("ipc", "reply_wake", "RUZU_TRACE_IPC_REPLY_WAKE", false),
        bqp_trace: get_bool("nvnflinger", "bqp", "RUZU_TRACE_BQP_RING", false),
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
        host_thread_ipc_trace: get_bool(
            "ipc",
            "host_thread_ipc",
            "RUZU_TRACE_HOST_THREAD_IPC",
            false,
        ),
        plu_ipc_trace: get_bool("ipc", "plu_ipc", "RUZU_TRACE_PLU_IPC", false),
        nvhost_ctrl_wait_trace: get_bool(
            "nvdrv",
            "nvhost_ctrl_wait",
            "RUZU_TRACE_NVHOST_CTRL_WAIT",
            false,
        ),
        sched_state_trace: get_bool(
            "scheduler",
            "state",
            "RUZU_TRACE_SCHED_STATE_FAST",
            false,
        ),
        wait_sync_trace: get_bool("svc", "wait_sync", "RUZU_TRACE_WAIT_SYNC_RING", false),
        hwc_trace: get_bool("nvnflinger", "hwc", "RUZU_TRACE_HWC_RING", false),
        texture_bind_trace: get_bool(
            "opengl",
            "texture_bind",
            "RUZU_TRACE_TEXTURE_BIND_RING",
            false,
        ),
        cbuf_bind_trace: get_bool("opengl", "cbuf_bind", "RUZU_TRACE_CBUF_BIND_RING", false),
        rt_bind_trace: get_bool("opengl", "rt_bind", "RUZU_TRACE_RT_BIND_RING", false),
        rt_sample_trace: get_bool("opengl", "rt_sample", "RUZU_TRACE_RT_SAMPLE_RING", false),
        texture_bind_addr_trace: get_bool(
            "opengl",
            "texture_bind_addr",
            "RUZU_TRACE_TEXTURE_BIND_ADDR_RING",
            false,
        ),
        gl_draw_state_trace: get_bool(
            "opengl",
            "draw_state",
            "RUZU_TRACE_GL_DRAW_STATE_RING",
            false,
        ),
        image_view_trace: get_bool(
            "opengl",
            "image_view",
            "RUZU_TRACE_IMAGE_VIEW_RING",
            false,
        ),
        audio_device_sink_trace: get_bool(
            "audio",
            "device_sink",
            "RUZU_TRACE_AUDIO_DEVICE_SINK",
            false,
        ),
        host1x_video_trace: get_bool("host1x", "video", "RUZU_TRACE_HOST1X_VIDEO", false),
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
        cat::IPC_INVALID => c.ipc_invalid_trace,
        cat::IPC_REPLY_WAKE => c.ipc_reply_wake_trace,
        cat::BQP => c.bqp_trace,
        cat::GPU_VA_MAP | cat::GPU_VA_UNMAP => c.gpu_va_trace,
        cat::HEAP => c.heap_trace,
        cat::TID_SVC => c.tid_svc_trace,
        cat::WATCH_READ | cat::WATCH_WRITE => c.watch_read_write,
        cat::STLEX => c.stlex_trace,
        cat::UNMAPPED_WRITE => c.unmapped_write_trace,
        cat::BL_HIT => c.bl_hit_trace,
        cat::IPC_HANDLE_OUT => c.ipc_handle_out,
        cat::IPC_DOMAIN_OUT => c.ipc_domain_out,
        cat::HOST_THREAD_IPC => c.host_thread_ipc_trace,
        cat::PLU_IPC => c.plu_ipc_trace,
        cat::NVHOST_CTRL_WAIT => c.nvhost_ctrl_wait_trace,
        cat::SCHED_STATE => c.sched_state_trace,
        cat::WAIT_SYNC => c.wait_sync_trace,
        cat::HWC => c.hwc_trace,
        cat::TEXTURE_BIND => c.texture_bind_trace,
        cat::CBUF_BIND => c.cbuf_bind_trace,
        cat::RT_BIND => c.rt_bind_trace,
        cat::RT_SAMPLE => c.rt_sample_trace,
        cat::TEXTURE_BIND_ADDR => c.texture_bind_addr_trace,
        cat::RT_GRID => c.rt_sample_trace,
        cat::GL_DRAW_STATE => c.gl_draw_state_trace,
        cat::RT_GRID_PHASE => c.rt_sample_trace,
        cat::IMAGE_VIEW => c.image_view_trace,
        cat::AUDIO_DEVICE_SINK => c.audio_device_sink_trace,
        cat::HOST1X_VIDEO => c.host1x_video_trace,
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
        cat::IPC_INVALID => {
            let svc = service_name(rec.args[1] as u16);
            let _ = write!(
                out,
                "IPC_INVALID seq={} service={} cmd={} tid={} tls=0x{:X} payload=",
                rec.args[0], svc, rec.args[2], rec.args[3], rec.args[4]
            );
            for i in 5..rec.arg_count as usize {
                let _ = write!(out, "{:08x} ", rec.args[i] as u32);
            }
            out.pop();
            out.push('\n');
        }
        cat::IPC_REPLY_WAKE => {
            let _ = writeln!(
                out,
                "IPC_REPLY_WAKE seq={} client_tid={} state={} wait_reason={} wait_queue={} guard_armed={} guard_pending={} result=0x{:X} request_addr=0x{:X} request_size=0x{:X}",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
                rec.args[8],
                rec.args[9],
            );
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
        cat::HOST_THREAD_IPC => {
            let stage = match rec.args[0] {
                1 => "enqueue_begin",
                2 => "registered_with_host_thread",
                3 => "enqueue_end",
                4 => "client_begin_wait",
                5 => "client_resumed",
                6 => "spawn_fallback_begin",
                7 => "before_scheduler_lock",
                8 => "after_scheduler_lock",
                9 => "before_process_lock",
                10 => "after_process_lock",
                11 => "before_client_session_lock",
                12 => "after_client_session_lock",
                13 => "after_send_sync_request_with_process",
                14 => "before_current_thread_lock",
                15 => "after_current_thread_lock",
                16 => "after_begin_wait_guarded",
                17 => "before_parent_lookup",
                18 => "after_parent_lookup",
                19 => "before_parent_session_lock",
                20 => "after_parent_session_lock",
                21 => "before_on_request",
                22 => "after_on_request",
                23 => "before_server_session_lock",
                24 => "after_server_session_lock",
                25 => "before_server_on_request",
                26 => "after_server_on_request",
                27 => "before_notify_waiters",
                28 => "after_notify_waiters",
                29 => "before_manager_wakeup_upgrade",
                30 => "after_manager_wakeup_upgrade",
                31 => "before_manager_wakeup_signal",
                32 => "after_manager_wakeup_signal",
                33 => "before_notify_thread_lock",
                34 => "after_notify_thread_lock",
                35 => "before_locked_section_end",
                36 => "after_locked_section_end",
                37 => "before_belt_signal",
                38 => "after_belt_signal",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[HOST_THREAD_IPC] handle=0x{:X} stage={}",
                rec.args[1] as u32,
                stage,
            );
        }
        cat::PLU_IPC => {
            match rec.args[0] {
                1 => {
                    let _ = writeln!(
                        out,
                        "[PLU_IPC] tid={} client_obj={} ss_ptr=0x{:X} wakeup_ptr=0x{:X} needs_setup={} is_signaled={} req_len={} cur_req={} mgr_wakeup_live={}",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3],
                        rec.args[4],
                        rec.args[5] != 0,
                        rec.args[6] != 0,
                        rec.args[7],
                        rec.args[8] != 0,
                        rec.args[9] != 0,
                    );
                }
                _ => {
                    let _ = writeln!(out, "[PLU_IPC] kind={} args={:?}", rec.args[0], &rec.args[..rec.arg_count as usize]);
                }
            }
        }
        cat::NVHOST_CTRL_WAIT => {
            let stage = match rec.args[0] {
                1 => "begin",
                2 => "zero-threshold",
                3 => "signalled-immediate",
                4 => "signalled-after-update",
                5 => "timeout0-success",
                6 => "timeout0-timeout",
                7 => "wait-fallback-success",
                8 => "armed",
                9 => "bad-parameter",
                10 => "busy",
                _ => "unknown",
            };
            let result = match rec.args[7] {
                0 => "success",
                1 => "timeout",
                2 => "bad-parameter",
                3 => "busy",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[NVHOST_CTRL_WAIT] stage={} syncpt_id={} threshold={} timeout={} is_allocation={} slot={} value_raw=0x{:08X} result={} min_value={} target={}",
                stage,
                rec.args[1],
                rec.args[2],
                rec.args[3] as u32,
                rec.args[4] != 0,
                rec.args[5],
                rec.args[6] as u32,
                result,
                rec.args[8],
                rec.args[9],
            );
        }
        cat::SCHED_STATE => {
            let stage = match rec.args[0] {
                1 => "transition",
                2 => "pq_remove",
                3 => "pq_push",
                _ => "unknown",
            };
            let top = |idx: usize| -> String {
                let value = rec.args[idx];
                if value == u64::MAX {
                    "None".to_string()
                } else {
                    value.to_string()
                }
            };
            let _ = writeln!(
                out,
                "[SCHED_STATE] stage={} tid={} old=0x{:X} new=0x{:X} prio={} core={} top=[{},{},{},{}]",
                stage,
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4] as i32,
                rec.args[5] as i32,
                top(6),
                top(7),
                top(8),
                top(9),
            );
        }
        cat::WAIT_SYNC => {
            let stage = match rec.args[0] {
                1 => "enter",
                2 => "return",
                _ => "unknown",
            };
            let kind = |value: u64| -> &'static str {
                match value {
                    1 => "readable_event",
                    2 => "thread",
                    3 => "process",
                    4 => "event",
                    5 => "server_port",
                    6 => "server_session",
                    _ => "unknown",
                }
            };
            let _ = write!(
                out,
                "[WAIT_SYNC] t_ns={} stage={} tid={} num={} timeout={} result=0x{:08X} out_index={}",
                rec.timestamp_ns,
                stage,
                rec.args[1],
                rec.args[2],
                rec.args[3] as i64,
                rec.args[4] as u32,
                rec.args[5] as i32,
            );
            for base in [6usize, 10usize] {
                if base + 3 >= rec.arg_count as usize {
                    break;
                }
                let handle = rec.args[base] as u32;
                let object_id = rec.args[base + 1];
                if handle == 0 && object_id == 0 {
                    continue;
                }
                let _ = write!(
                    out,
                    " h{}=0x{:08X}:oid=0x{:X}:kind={}:signaled={}",
                    (base - 6) / 4,
                    handle,
                    object_id,
                    kind(rec.args[base + 2]),
                    rec.args[base + 3] != 0,
                );
            }
            out.push('\n');
        }
        cat::HWC => {
            match rec.args[0] {
                1 => {
                    let _ = writeln!(
                        out,
                        "[HWC] stage=acquire frame={} consumer={} status={} slot={} item_frame={} swap_interval={} release_frame={} is_acquired={}",
                        rec.args[1],
                        rec.args[2] as i32,
                        rec.args[3] as i32,
                        rec.args[4] as i32,
                        rec.args[5],
                        rec.args[6] as i32,
                        rec.args[7],
                        rec.args[8] != 0,
                    );
                }
                2 => {
                    let _ = writeln!(
                        out,
                        "[HWC] stage=compose_layer frame={} consumer={} handle=0x{:X} offset=0x{:X} size={}x{} stride={} format=0x{:X} transform=0x{:X} crop=({},{}..{},{})",
                        rec.args[1],
                        rec.args[2] as i32,
                        rec.args[3],
                        rec.args[4],
                        rec.args[5],
                        rec.args[6],
                        rec.args[7],
                        rec.args[8],
                        rec.args[9],
                        rec.args[10] as i32,
                        rec.args[11] as i32,
                        rec.args[12] as i32,
                        rec.args[13] as i32,
                    );
                }
                3 => {
                    let _ = writeln!(
                        out,
                        "[HWC] stage=nvdisp_layer index={} handle=0x{:X} address=0x{:X} offset=0x{:X} size={}x{} stride={} format=0x{:X} transform=0x{:X} crop=({},{}..{},{})",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3],
                        rec.args[4],
                        rec.args[5],
                        rec.args[6],
                        rec.args[7],
                        rec.args[8],
                        rec.args[9],
                        rec.args[10] as i32,
                        rec.args[11] as i32,
                        rec.args[12] as i32,
                        rec.args[13] as i32,
                    );
                }
                _ => {
                    let _ = writeln!(out, "[HWC] stage={} args={:?}", rec.args[0], &rec.args[..rec.arg_count as usize]);
                }
            }
        }
        cat::BQP => {
            match rec.args[0] {
                1 => {
                    let _ = writeln!(
                        out,
                        "[BQP] stage=dequeue_enter seq={} async={} size={}x{} format=0x{:X} usage=0x{:X}",
                        rec.args[1],
                        rec.args[2] != 0,
                        rec.args[3],
                        rec.args[4],
                        rec.args[5],
                        rec.args[6],
                    );
                }
                2 => {
                    let _ = writeln!(
                        out,
                        "[BQP] stage=dequeue_return seq={} status={} slot={} flags=0x{:X}",
                        rec.args[1],
                        rec.args[2] as i32,
                        rec.args[3] as i32,
                        rec.args[4],
                    );
                }
                3 => {
                    let _ = writeln!(
                        out,
                        "[BQP] stage=queue_enter seq={} slot={}",
                        rec.args[1],
                        rec.args[2] as i32,
                    );
                }
                4 => {
                    let _ = writeln!(
                        out,
                        "[BQP] stage=queue_commit seq={} slot={} frame={} queue_len={} droppable={} acquire_called={}",
                        rec.args[1],
                        rec.args[2] as i32,
                        rec.args[3],
                        rec.args[4],
                        rec.args[5] != 0,
                        rec.args[6] != 0,
                    );
                }
                5 => {
                    let _ = writeln!(
                        out,
                        "[BQP] stage=queue_return seq={} status={} slot={}",
                        rec.args[1],
                        rec.args[2] as i32,
                        rec.args[3] as i32,
                    );
                }
                6 => {
                    let _ = writeln!(
                        out,
                        "[BQP] stage=cancel seq={} slot={}",
                        rec.args[1],
                        rec.args[2] as i32,
                    );
                }
                _ => {
                    let _ = writeln!(out, "[BQP] stage={} args={:?}", rec.args[0], &rec.args[..rec.arg_count as usize]);
                }
            }
        }
        cat::TEXTURE_BIND => {
            let texture_type = match rec.args[4] {
                0 => "Color1D",
                1 => "ColorArray1D",
                2 => "Color2D",
                3 => "ColorArray2D",
                4 => "Color3D",
                5 => "ColorCube",
                6 => "ColorArrayCube",
                7 => "Buffer",
                8 => "Color2DRect",
                _ => "unknown",
            };
            let view_type = match rec.args[6] {
                0 => "E1D",
                1 => "E2D",
                2 => "Cube",
                3 => "E3D",
                4 => "E1DArray",
                5 => "E2DArray",
                6 => "CubeArray",
                7 => "Rect",
                8 => "Buffer",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[TEXTURE_BIND] draw_seq={} pipeline={} stage={} unit={} desc_type={} view_id={} view_type={} image_id={} handle={} size={}x{}x{} sample0=0x{:08X} sample_mid=0x{:08X}",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                rec.args[3],
                texture_type,
                rec.args[5],
                view_type,
                rec.args[7],
                rec.args[8],
                rec.args[9],
                rec.args[10],
                rec.args[11],
                rec.args[12],
                rec.args[13],
            );
        }
        cat::CBUF_BIND => {
            let _ = writeln!(
                out,
                "[CBUF_BIND] draw_seq={} pipeline={} stage={} slot={} binding_index={} vec4={} gpu=0x{:X} size={} used_size={} enabled={} f32=[{:.6},{:.6},{:.6},{:.6}]",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
                rec.args[8],
                rec.args[9],
                f32::from_bits(rec.args[10] as u32),
                f32::from_bits(rec.args[11] as u32),
                f32::from_bits(rec.args[12] as u32),
                f32::from_bits(rec.args[13] as u32),
            );
        }
        cat::RT_BIND => {
            let unpack_size = |packed: u64| -> (u32, u32) {
                ((packed >> 32) as u32, packed as u32)
            };
            let (rt0_w, rt0_h) = unpack_size(rec.args[9]);
            let (rt1_w, rt1_h) = unpack_size(rec.args[12]);
            let (surface_w, surface_h) = unpack_size(rec.args[13]);
            let _ = writeln!(
                out,
                "[RT_BIND] draw_seq={} pipeline={} fbo={} size={}x{} rt_count={} rt_map=0x{:X} rt0=gpu=0x{:X}/fmt=0x{:X}/{}x{} rt1=gpu=0x{:X}/fmt=0x{:X}/{}x{} surface={}x{}",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
                rec.args[8],
                rt0_w,
                rt0_h,
                rec.args[10],
                rec.args[11],
                rt1_w,
                rt1_h,
                surface_w,
                surface_h,
            );
        }
        cat::RT_SAMPLE => {
            let _ = writeln!(
                out,
                "[RT_SAMPLE] draw_seq={} pipeline={} fbo={} rt0_gpu=0x{:X} fmt=0x{:X} size={}x{} sample={}x{} rgb_nonzero={} alpha_nonzero={} checksum=0x{:X} first_rgba=0x{:08X} gl_error=0x{:X}",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
                rec.args[8],
                rec.args[9],
                rec.args[10],
                rec.args[11],
                rec.args[12],
                rec.args[13],
            );
        }
        cat::RT_GRID => {
            let unpack_xy = |packed: u64| -> (u32, u32) {
                ((packed >> 32) as u32, packed as u32)
            };
            let (first_x, first_y) = unpack_xy(rec.args[10]);
            let (last_x, last_y) = unpack_xy(rec.args[12]);
            let _ = writeln!(
                out,
                "[RT_GRID] draw_seq={} pipeline={} fbo={} rt0_gpu=0x{:X} size={}x{} grid={}x{} hit_cells={} nonzero_bytes={} first=({},{}):0x{:08X} last=({},{}):0x{:08X}",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
                rec.args[8],
                rec.args[9],
                first_x,
                first_y,
                rec.args[11],
                last_x,
                last_y,
                rec.args[13],
            );
        }
        cat::GL_DRAW_STATE => {
            match rec.args[0] {
                1 => {
                    let _ = writeln!(
                        out,
                        "[GL_DRAW_STATE] stage=fbo draw_seq={} pipeline={} fbo={} status=0x{:X} drawbuf0=0x{:X} attached={} type=0x{:X} level={} layer={} layered={}",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3],
                        rec.args[4],
                        rec.args[5],
                        rec.args[6],
                        rec.args[7],
                        rec.args[8] as i32,
                        rec.args[9] as i32,
                        rec.args[10] != 0,
                    );
                }
                2 => {
                    let _ = writeln!(
                        out,
                        "[GL_DRAW_STATE] stage=kill draw_seq={} pipeline={} rast_disc={} logic_op={} alpha_test={} depth_clamp={} prim_restart={} prim_restart_fixed={} sample_mask={} sample_mask_value=0x{:X} clip_mask=0x{:X} tf_active={} tf_paused={}",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3] != 0,
                        rec.args[4] != 0,
                        rec.args[5] != 0,
                        rec.args[6] != 0,
                        rec.args[7] != 0,
                        rec.args[8] != 0,
                        rec.args[9] != 0,
                        rec.args[10],
                        rec.args[11],
                        rec.args[12] != 0,
                        rec.args[13] != 0,
                    );
                }
                3 => {
                    let mask = rec.args[3];
                    let _ = writeln!(
                        out,
                        "[GL_DRAW_STATE] stage=output draw_seq={} pipeline={} cmask=[{},{},{},{}] blend0={} scissor0={} depth={} depth_mask={} stencil={} cull={} front_face=0x{:X} cull_face=0x{:X} polyoff={} srgb={}",
                        rec.args[1],
                        rec.args[2],
                        (mask & 0xff) != 0,
                        ((mask >> 8) & 0xff) != 0,
                        ((mask >> 16) & 0xff) != 0,
                        ((mask >> 24) & 0xff) != 0,
                        rec.args[4] != 0,
                        rec.args[5] != 0,
                        rec.args[6] != 0,
                        rec.args[7] != 0,
                        rec.args[8] != 0,
                        rec.args[9] != 0,
                        rec.args[10],
                        rec.args[11],
                        rec.args[12] != 0,
                        rec.args[13] != 0,
                    );
                }
                4 => {
                    let _ = writeln!(
                        out,
                        "[GL_DRAW_STATE] stage=draw_result draw_seq={} pipeline={} query={} any_samples={} gl_error=0x{:X} indexed={} primitive=0x{:X} vertices={} instances={}",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3] != 0,
                        rec.args[4] != 0,
                        rec.args[5],
                        rec.args[6] != 0,
                        rec.args[7],
                        rec.args[8],
                        rec.args[9],
                    );
                }
                5 => {
                    let _ = writeln!(
                        out,
                        "[GL_DRAW_STATE] stage=draw_params draw_seq={} pipeline={} indexed={} primitive=0x{:X} vertices={} instances={} base_vertex={} base_instance={} index_offset={} index_format=0x{:X} vertex_first={} vertex_count={} index_first={}",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3] != 0,
                        rec.args[4],
                        rec.args[5],
                        rec.args[6],
                        rec.args[7] as i64,
                        rec.args[8],
                        rec.args[9],
                        rec.args[10],
                        rec.args[11],
                        rec.args[12],
                        rec.args[13],
                    );
                }
                _ => {
                    let _ = writeln!(
                        out,
                        "[GL_DRAW_STATE] stage={} draw_seq={} pipeline={}",
                        rec.args[0],
                        rec.args[1],
                        rec.args[2],
                    );
                }
            }
        }
        cat::RT_GRID_PHASE => {
            let unpack_xy = |packed: u64| -> (u32, u32) {
                ((packed >> 32) as u32, packed as u32)
            };
            let (first_x, first_y) = unpack_xy(rec.args[9]);
            let (last_x, last_y) = unpack_xy(rec.args[11]);
            let phase = match rec.args[0] {
                0 => "pre",
                1 => "post",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[RT_GRID_PHASE] phase={} draw_seq={} pipeline={} fbo={} rt0_gpu=0x{:X} size={}x{} hit_cells={} nonzero_bytes={} first=({},{}):0x{:08X} last=({},{}):0x{:08X} gl_error=0x{:X}",
                phase,
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
                rec.args[8],
                first_x,
                first_y,
                rec.args[10],
                last_x,
                last_y,
                rec.args[12],
                rec.args[13],
            );
        }
        cat::TEXTURE_BIND_ADDR => {
            let _ = writeln!(
                out,
                "[TEXTURE_BIND_ADDR] draw_seq={} pipeline={} stage={} unit={} view_id={} image_id={} gpu=0x{:X} size={}x{} sample0=0x{:08X} sample_mid=0x{:08X}",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
                rec.args[8],
                rec.args[9],
                rec.args[10],
            );
        }
        cat::IMAGE_VIEW => {
            let stage = match rec.args[0] {
                1 => "image_touch",
                2 => "view_create",
                3 => "view_mismatch",
                4 => "fb_view_create",
                5 => "fb_view_mismatch",
                6 => "rt_view_create",
                7 => "rt_view_mismatch",
                8 => "cache_unmap_clear",
                _ => "unknown",
            };
            let view_type = match rec.args[8] {
                0 => "E1D",
                1 => "E2D",
                2 => "Cube",
                3 => "E3D",
                4 => "E1DArray",
                5 => "E2DArray",
                6 => "CubeArray",
                7 => "Rect",
                8 => "Buffer",
                _ => "unknown",
            };
            let size_w = rec.args[10] & 0xffff_ffff;
            let size_h = rec.args[10] >> 32;
            let range_level = rec.args[12] & 0xffff;
            let range_layer = (rec.args[12] >> 16) & 0xffff;
            let range_levels = (rec.args[12] >> 32) & 0xffff;
            let range_layers = (rec.args[12] >> 48) & 0xffff;
            let _ = writeln!(
                out,
                "[IMAGE_VIEW] stage={} view_id={} image_id={} image_tex={} current_tex={} default={} color2d={} color_array2d={} view_type={} format={} size={}x{} gpu=0x{:X} range=l{} ly{} +{}x{} flags=0x{:X}",
                stage,
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
                view_type,
                rec.args[9],
                size_w,
                size_h,
                rec.args[11],
                range_level,
                range_layer,
                range_levels,
                range_layers,
                rec.args[13],
            );
        }
        cat::AUDIO_DEVICE_SINK => {
            let min_value = rec.args[6] as u32 as i32;
            let max_value = rec.args[7] as u32 as i32;
            let first0 = rec.args[11] as u32 as i32;
            let first1 = rec.args[12] as u32 as i32;
            let first2 = rec.args[13] as u32 as i32;
            let _ = writeln!(
                out,
                "[AUDIO_DEVICE_SINK] seq={} sample_buffer=0x{:X} input_count={} buffer_count={} sample_count={} frames={} min={} max={} nonzero={} clipped={} visited={} first=[{}, {}, {}]",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                min_value,
                max_value,
                rec.args[8],
                rec.args[9],
                rec.args[10],
                first0,
                first1,
                first2,
            );
        }
        cat::HOST1X_VIDEO => {
            match rec.args[0] {
                1 | 2 => {
                    let channel = if rec.args[0] == 1 { "nvdec" } else { "vic" };
                    let _ = writeln!(
                        out,
                        "[HOST1X_VIDEO] stage=submit channel={} fd={} cmd_buffers={} relocs={} syncpts={} fences={} first_gpu=0x{:X} first_words={}",
                        channel,
                        rec.args[1] as i32,
                        rec.args[2],
                        rec.args[3],
                        rec.args[4],
                        rec.args[5],
                        rec.args[6],
                        rec.args[7],
                    );
                }
                3 => {
                    let _ = writeln!(
                        out,
                        "[HOST1X_VIDEO] stage=nvdec_execute id={} codec={} initialized={}",
                        rec.args[1] as i32,
                        rec.args[2],
                        rec.args[3] != 0,
                    );
                }
                4 => {
                    let op = match rec.args[1] {
                        1 => "initialize",
                        2 => "send_packet",
                        3 => "receive_frame",
                        _ => "unknown",
                    };
                    let _ = writeln!(
                        out,
                        "[HOST1X_VIDEO] stage=decode_api op={} codec={} result={} packet_size={}",
                        op,
                        rec.args[2],
                        rec.args[3] != 0,
                        rec.args[4],
                    );
                }
                5 => {
                    let _ = writeln!(
                        out,
                        "[HOST1X_VIDEO] stage=vic_execute id={} config_addr=0x{:X}",
                        rec.args[1] as i32,
                        rec.args[2],
                    );
                }
                _ => {
                    let _ = writeln!(
                        out,
                        "[HOST1X_VIDEO] stage={} args={:?}",
                        rec.args[0],
                        &rec.args[..rec.arg_count as usize]
                    );
                }
            }
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
