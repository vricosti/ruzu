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
    /// Userspace mutex priority-inheritance handoff. args =
    /// [stage, tid, addr, handle, tag, test_tag, owner_tid, next_tid, has_waiters, next_value,
    ///  result, owner_obj_id, waiter_count, extra]
    pub const LOCK_PI: u16 = 55;
    /// JIT memory read watchpoint. args = [core, tid, pc, lr, vaddr, size, value_lo, value_hi]
    pub const WATCH_READ: u16 = 7;
    /// JIT memory write watchpoint. Same layout as WATCH_READ.
    pub const WATCH_WRITE: u16 = 8;
    /// STLEX (exclusive store) attempt. args = [pc, vaddr, value, expected, ok]
    pub const STLEX: u16 = 9;
    /// Single-cell unmapped JIT write. args = [tid, pc, lr, vaddr, size, value_lo, value_hi]
    pub const UNMAPPED_WRITE: u16 = 10;
    /// Single-cell unmapped JIT read. args =
    /// [core, pc, lr, vaddr, size, x0, x8, x9, x19, x20, x21, x22, x25]
    pub const UNMAPPED_READ: u16 = 53;
    /// AArch64 exception context. args =
    /// stage=0 [stage, reason_id, core, tid, pc, lr, sp, x0, x8, x19, x20, x21, x23, x24]
    /// stage=1 [stage, base, qword0, qword1, qword2, qword3]
    /// stage=7 [stage, core, tid, x9, x10, x11, x12, x13, x14, x15, x18, x22, x25, x26]
    /// stage=8 [stage, reg_index, base, qword0, qword1, qword2, qword3]
    /// stage=9 [stage, core, tid, x27, x28]
    pub const A64_EXCEPTION_CTX: u16 = 54;
    /// svcBreak AArch64 context. Kept separate from A64_EXCEPTION_CTX so
    /// abort diagnostics do not enable the high-volume JIT exception stream.
    /// stage=0 [stage, reason, core, tid, pc, lr, sp, info1, info2]
    /// stage=1 [stage, base, qword0, qword1, qword2, qword3]
    pub const A64_BREAK_CTX: u16 = 56;
    /// svcSetThreadCoreMask lifecycle. args =
    /// [stage, caller_tid, target_tid, handle, core_id, affinity_mask, state, active_core,
    ///  current_core, result, is_pinned, waiter_count]
    pub const THREAD_CORE_MASK: u16 = 57;
    /// svcCreateThread lifecycle. args =
    /// [stage, caller_tid, entry_point, arg, stack_bottom, priority, requested_core, effective_core,
    ///  process_core_mask, result, out_handle, extra]
    pub const CREATE_THREAD: u16 = 58;
    /// SVC return-value match. args =
    /// [target_result, tid, core, svc_name_id, pc, lr, a0, a1, a2, a3, ret0, ret1, ret2, ret3]
    pub const SVC_ERRVAL: u16 = 60;
    /// SendSyncRequest enter/exit. args =
    /// [stage(0=enter,1=exit), guest_tid, handle, cmd, cmd_type, result, service_name_id]
    pub const SSR_IPC: u16 = 61;
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
    /// Scheduler state/PQ update.
    /// stage=1..3 state/PQ args = [stage, tid, old_state, new_state, prio, active_core,
    ///  top0, top1, top2, top3]
    /// stage=4 schedule_select args = [stage, cur_tid, highest_tid, target_tid, core,
    ///  needs_scheduling, interrupt_task, switch_from_schedule, top0, top1, top2, top3]
    /// stage=5 switch args = [stage, cur_tid, next_tid, 0, core, has_gsc, prev_tid]
    /// stage=6 scheduler_lock_unlock args = [stage, owner_sched_id, guest_tid, cores_needing,
    ///  lock_count_after_decrement]
    /// stage=7 scheduler_lock_enable args = [stage, owner_sched_id, guest_tid, cores_needing,
    ///  lock_count_after_decrement]
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
    ///  probe_x, probe_y, rgb_nonzero, alpha_nonzero, checksum,
    ///  probe_rgba, gl_error]
    pub const RT_SAMPLE: u16 = 23;
    /// OpenGL texture binding address attribution. args =
    /// [draw_seq, pipeline, stage, unit, view_id, image_id, view_gpu_addr,
    ///  width, height, gl_handle, desc_texture_type, desc_is_depth,
    ///  desc_is_multisample]
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
    /// stage=9 swizzle [stage, view_id, image_id, image_texture, gpu_addr, format,
    ///  view_type, source_swizzle_pack, gl_swizzle_pack, default_handle,
    ///  color2d_handle, color_array2d_handle]
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
    /// stage=6 vic timing [stage, id, step, elapsed_us, aux0, aux1, aux2]
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
    /// Present-time OpenGL texture readback by guest GPU address. args =
    /// [present_index, gpu_addr, image_id, texture, width, height, format,
    ///  gl_error, samples, rgb_nonzero, alpha_nonzero, checksum, first_rgba, last_rgba]
    pub const PRESENT_TEXTURE: u16 = 34;
    /// Audio renderer voice-command generation summary. args =
    /// [seq, sorted, in_use, skipped, active, processed_voices,
    ///  processed_channels, data_pushed, data_skip_was_playing, no_state,
    ///  no_command, connected_channels, valid_wavebuffers, nonzero_wavebuffer_addr]
    pub const AUDIO_VOICE: u16 = 35;
    /// OpenGL render-target depth/zeta attachment attribution. args =
    /// [framebuffer, view_id, image_id, zeta_gpu_addr, format, size_pack,
    ///  texture, attachment, buffer_bits, framebuffer_status, color0_view,
    ///  color1_view, draw_buffers_pack]
    pub const RT_DEPTH_ATTACH: u16 = 36;
    /// OpenGL render-target zeta snapshot attribution. args =
    /// [draw_seq, pipeline, framebuffer, zeta_enabled, zeta_gpu_addr,
    ///  zeta_format, zeta_size_pack, rt0_gpu_addr, rt0_format, rt0_size_pack,
    ///  depth_test_enable, depth_write_enable, depth_func, depth_mode]
    pub const RT_ZETA_BIND: u16 = 37;
    /// nvhost_gpu::SubmitGpfifo syncpoint attribution. args =
    /// [stage, seq, tid, bind_id, syncpoint_id, flags, increment,
    ///  fence_in_id, fence_in_value, fence_out_value, min_value, max_value]
    pub const SUBMIT_GPFIFO: u16 = 38;
    /// video_core Host1x syncpoint attribution. args =
    /// [stage, is_guest, syncpoint_id, expected_or_new, current, action_count]
    pub const HOST1X_SYNCPOINT: u16 = 39;
    /// video_core GPU thread submit attribution. args =
    /// [stage, fence, channel, list_count, prefetch_count, elapsed_us]
    pub const GPU_THREAD: u16 = 40;
    /// video_core DmaPusher step attribution. Stage 1-8 args =
    /// [stage, queue_len, subindex, addr, size, non_main, method_count, dma_get,
    ///  elapsed_us]. Stage 9 method aggregate args =
    /// [stage, command_count, dispatch_count, subchannel, method, calls, words,
    ///  elapsed_us, dma_get]
    pub const DMA_PUSHER: u16 = 41;
    /// OpenGL draw phase timing. args =
    /// [draw_seq, pipeline, indexed, primitive, vertices, instances, total_us,
    ///  pipeline_us, rt_us, build_us, configure_us, update_buffers_us,
    ///  bind_buffers_us, sync_draw_us]
    pub const GL_DRAW_PROFILE: u16 = 42;
    /// Present-time GL texture-view alias comparison. args =
    /// [present_index, gpu_addr, image_id, view_id, current_texture, view_texture,
    ///  width, height, current_checksum, view_checksum, current_rgb_nonzero,
    ///  view_rgb_nonzero, current_first_rgba, view_first_rgba]
    pub const PRESENT_ALIAS: u16 = 43;
    /// Texture-cache framebuffer image selection for presentation. args =
    /// [cpu_addr, gpu_addr, image_id, view_id, candidates, flags, modification_tick,
    ///  aliases, overlaps, width, height, image_format, fb_pixel_format, fb_blending]
    pub const PRESENT_IMAGE_SELECT: u16 = 44;
    /// SVC SendSyncRequest progress attribution. args =
    /// [stage, tid, handle, client_obj, tls, aux0, aux1, aux2]
    pub const SVC_IPC_PROGRESS: u16 = 45;
    /// IHOSBinderDriver transaction attribution. args =
    /// [stage, seq, id, transaction_id, flags, in_len, out_len, status]
    pub const BINDER_TXN: u16 = 46;
    /// OpenGL graphics-pipeline cache/compile attribution. args =
    /// [stage, seq, cache_len, key_raw, key_hash, h0, h1, h2, h3, h4, h5,
    ///  aux0, aux1, aux2]
    pub const GL_PIPELINE: u16 = 47;
    /// Mii service CharInfo/CoreData attribution. args =
    /// [stage, cmd, result, aux0, aux1, create_lo, create_hi, name0_3, name4_7,
    ///  traits0, traits1]
    pub const MII_SERVICE: u16 = 48;
    /// Final OpenGL present/composite attribution. args =
    /// [stage, frame, framebuffer_count, current_frame, draw_count, width,
    ///  height, gl_error]
    pub const PRESENT_COMPOSITE: u16 = 49;
    /// nvmap allocation attribution. args =
    /// [seq, kind, fd, handle, size, address, flags, align, heap_mask]
    /// kind: 1=IocCreate, 2=IocAlloc.
    pub const NVMAP_ALLOC: u16 = 50;
    /// Account profile response attribution. args =
    /// [stage, result, user_lo, user_hi, timestamp, name0_7, name8_15,
    ///  user_data0, user_data1]
    /// stage: 1=IProfileCommon::Get, 2=IProfileCommon::GetBase.
    pub const ACC_PROFILE: u16 = 51;
    /// HID NPad service command attribution. args =
    /// [cmd, result, aruid, arg0, arg1, out0, out1, out2]
    pub const HID_NPAD: u16 = 52;
    /// AudioOut buffer lifecycle attribution. args =
    /// [stage, session_id, state, tag, size, samples, total_or_registered, released_or_signal,
    ///  queue_or_played, result]
    pub const AUDIO_OUT_BUFFER: u16 = 59;
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
        self.tail
            .store((tail + 1) % self.capacity, Ordering::Release);
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
    pub svc_ipc_progress_trace: bool,
    pub binder_txn_trace: bool,
    pub bqp_trace: bool,
    pub ssr_ipc_trace: bool,
    pub gpu_va_trace: bool,
    pub heap_trace: bool,
    pub tid_svc_trace: bool,
    pub tid_svc_pc: bool,
    pub lock_pi_trace: bool,
    pub watch_read_write: bool,
    pub stlex_trace: bool,
    pub unmapped_write_trace: bool,
    pub unmapped_read_trace: bool,
    pub a64_exception_trace: bool,
    pub a64_break_trace: bool,
    pub thread_core_mask_trace: bool,
    pub create_thread_trace: bool,
    pub svc_errval_trace: bool,
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
    pub present_texture_trace: bool,
    pub audio_device_sink_trace: bool,
    pub audio_out_buffer_trace: bool,
    pub audio_voice_trace: bool,
    pub host1x_video_trace: bool,
    pub host1x_syncpoint_trace: bool,
    pub gpu_thread_trace: bool,
    pub dma_pusher_trace: bool,
    pub gl_draw_profile_trace: bool,
    pub gl_pipeline_trace: bool,
    pub present_composite_trace: bool,
    pub nvmap_alloc_trace: bool,
    pub present_alias_trace: bool,
    pub present_image_select_trace: bool,
    pub rt_depth_attach_trace: bool,
    pub rt_zeta_bind_trace: bool,
    pub submit_gpfifo_trace: bool,
    pub mii_service_trace: bool,
    pub acc_profile_trace: bool,
    pub hid_npad_trace: bool,
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
            svc_ipc_progress_trace: false,
            binder_txn_trace: false,
            bqp_trace: false,
            ssr_ipc_trace: false,
            gpu_va_trace: false,
            heap_trace: false,
            tid_svc_trace: false,
            tid_svc_pc: false,
            lock_pi_trace: false,
            watch_read_write: false,
            stlex_trace: false,
            unmapped_write_trace: false,
            unmapped_read_trace: false,
            a64_exception_trace: false,
            a64_break_trace: false,
            thread_core_mask_trace: false,
            create_thread_trace: false,
            svc_errval_trace: false,
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
            present_texture_trace: false,
            audio_device_sink_trace: false,
            audio_out_buffer_trace: false,
            audio_voice_trace: false,
            host1x_video_trace: false,
            host1x_syncpoint_trace: false,
            gpu_thread_trace: false,
            dma_pusher_trace: false,
            gl_draw_profile_trace: false,
            gl_pipeline_trace: false,
            present_composite_trace: false,
            nvmap_alloc_trace: false,
            present_alias_trace: false,
            present_image_select_trace: false,
            rt_depth_attach_trace: false,
            rt_zeta_bind_trace: false,
            submit_gpfifo_trace: false,
            mii_service_trace: false,
            acc_profile_trace: false,
            hid_npad_trace: false,
            output_target: "stderr".to_string(),
            output_file: String::new(),
            ring_capacity: 16384,
            drain_interval_us: 1000,
            lossless: false,
        }
    }
}

impl Config {
    fn any_trace_enabled(&self) -> bool {
        self.ipc_reply_dump
            || self.ipc_request_dump
            || self.ipc_invalid_trace
            || self.ipc_reply_wake_trace
            || self.svc_ipc_progress_trace
            || self.binder_txn_trace
            || self.bqp_trace
            || self.ssr_ipc_trace
            || self.gpu_va_trace
            || self.heap_trace
            || self.tid_svc_trace
            || self.lock_pi_trace
            || self.watch_read_write
            || self.stlex_trace
            || self.unmapped_write_trace
            || self.unmapped_read_trace
            || self.a64_exception_trace
            || self.a64_break_trace
            || self.thread_core_mask_trace
            || self.create_thread_trace
            || self.svc_errval_trace
            || self.bl_hit_trace
            || self.ipc_handle_out
            || self.ipc_domain_out
            || self.host_thread_ipc_trace
            || self.plu_ipc_trace
            || self.nvhost_ctrl_wait_trace
            || self.sched_state_trace
            || self.wait_sync_trace
            || self.hwc_trace
            || self.texture_bind_trace
            || self.cbuf_bind_trace
            || self.rt_bind_trace
            || self.rt_sample_trace
            || self.texture_bind_addr_trace
            || self.gl_draw_state_trace
            || self.image_view_trace
            || self.present_texture_trace
            || self.audio_device_sink_trace
            || self.audio_out_buffer_trace
            || self.audio_voice_trace
            || self.host1x_video_trace
            || self.host1x_syncpoint_trace
            || self.gpu_thread_trace
            || self.dma_pusher_trace
            || self.gl_draw_profile_trace
            || self.gl_pipeline_trace
            || self.present_composite_trace
            || self.nvmap_alloc_trace
            || self.present_alias_trace
            || self.present_image_select_trace
            || self.rt_depth_attach_trace
            || self.rt_zeta_bind_trace
            || self.submit_gpfifo_trace
            || self.mii_service_trace
            || self.acc_profile_trace
            || self.hid_npad_trace
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
        .or_else(|| {
            std::env::var("HOME")
                .ok()
                .map(|h| PathBuf::from(h).join(".config"))
        })?;
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
        svc_ipc_progress_trace: get_bool(
            "ipc",
            "svc_progress",
            "RUZU_TRACE_SVC_IPC_PROGRESS",
            false,
        ),
        binder_txn_trace: get_bool("nvnflinger", "binder", "RUZU_TRACE_BINDER_TXN_RING", false),
        bqp_trace: get_bool("nvnflinger", "bqp", "RUZU_TRACE_BQP_RING", false),
        ssr_ipc_trace: get_bool("svc", "ssr_ipc", "RUZU_DUMP_SSR", false),
        gpu_va_trace: get_bool("nvdrv", "gpu_va_trace", "RUZU_TRACE_GPU_VA", false),
        heap_trace: get_bool("svc", "heap_trace", "RUZU_TRACE_HEAP", false),
        tid_svc_trace: get_bool("svc", "tid_svc", "RUZU_TRACE_TID_SVC", false),
        tid_svc_pc: get_bool("svc", "tid_svc_pc", "RUZU_TRACE_TID_SVC_PC", false),
        lock_pi_trace: get_bool("svc", "lock_pi", "RUZU_TRACE_LOCK_PI", false),
        watch_read_write: get_bool("jit", "watch_read_write", "RUZU_WATCH_ENABLED", false),
        stlex_trace: get_bool("jit", "stlex", "RUZU_TRACE_STLEX", false),
        unmapped_write_trace: get_bool("jit", "unmapped_write", "RUZU_TRACE_UNMAPPED_WRITE", false),
        unmapped_read_trace: get_bool("jit", "unmapped_read", "RUZU_TRACE_UNMAPPED_READ", false),
        a64_exception_trace: get_bool("jit", "a64_exception", "RUZU_TRACE_A64_EXCEPTION", false),
        a64_break_trace: get_bool("jit", "a64_break", "RUZU_TRACE_A64_BREAK", false),
        thread_core_mask_trace: get_bool(
            "svc",
            "thread_core_mask",
            "RUZU_TRACE_THREAD_CORE_MASK",
            false,
        ),
        create_thread_trace: get_bool("svc", "create_thread", "RUZU_TRACE_CREATE_THREAD", false),
        svc_errval_trace: get_bool("svc", "errval", "RUZU_TRACE_SVC_ERRVAL", false)
            || env_set("RUZU_TRACE_INVALID_HANDLE_SVC"),
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
        sched_state_trace: get_bool("scheduler", "state", "RUZU_TRACE_SCHED_STATE_FAST", false),
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
        image_view_trace: get_bool("opengl", "image_view", "RUZU_TRACE_IMAGE_VIEW_RING", false),
        present_texture_trace: get_bool(
            "opengl",
            "present_texture",
            "RUZU_TRACE_PRESENT_TEXTURE_RING",
            false,
        ),
        audio_device_sink_trace: get_bool(
            "audio",
            "device_sink",
            "RUZU_TRACE_AUDIO_DEVICE_SINK",
            false,
        ),
        audio_out_buffer_trace: get_bool(
            "audio",
            "out_buffer",
            "RUZU_TRACE_AUDIO_OUT_BUFFER",
            false,
        ),
        audio_voice_trace: get_bool("audio", "voice", "RUZU_TRACE_AUDIO_VOICE", false),
        host1x_video_trace: get_bool("host1x", "video", "RUZU_TRACE_HOST1X_VIDEO", false),
        host1x_syncpoint_trace: get_bool(
            "host1x",
            "syncpoint",
            "RUZU_TRACE_HOST1X_SYNCPOINT_RING",
            false,
        ),
        gpu_thread_trace: get_bool("gpu", "thread", "RUZU_TRACE_GPU_THREAD_RING", false),
        dma_pusher_trace: get_bool("gpu", "dma_pusher", "RUZU_TRACE_DMA_PUSHER_RING", false),
        gl_draw_profile_trace: get_bool(
            "opengl",
            "draw_profile",
            "RUZU_TRACE_GL_DRAW_PROFILE_RING",
            false,
        ),
        gl_pipeline_trace: get_bool("opengl", "pipeline", "RUZU_TRACE_GL_PIPELINE", false),
        present_composite_trace: get_bool(
            "opengl",
            "present_composite",
            "RUZU_TRACE_PRESENT_COMPOSITE",
            false,
        ),
        nvmap_alloc_trace: get_bool("nvdrv", "nvmap_alloc", "RUZU_TRACE_NVMAP_ALLOC", false),
        present_alias_trace: get_bool(
            "opengl",
            "present_alias",
            "RUZU_TRACE_PRESENT_ALIAS_RING",
            false,
        ),
        present_image_select_trace: get_bool(
            "opengl",
            "present_image_select",
            "RUZU_TRACE_PRESENT_IMAGE_SELECT_RING",
            false,
        ),
        rt_depth_attach_trace: get_bool(
            "opengl",
            "rt_depth_attach",
            "RUZU_TRACE_RT_DEPTH_ATTACH_RING",
            false,
        ),
        rt_zeta_bind_trace: get_bool(
            "opengl",
            "rt_zeta_bind",
            "RUZU_TRACE_RT_ZETA_BIND_RING",
            false,
        ),
        submit_gpfifo_trace: get_bool(
            "nvdrv",
            "submit_gpfifo",
            "RUZU_TRACE_SUBMIT_GPFIFO_RING",
            false,
        ),
        mii_service_trace: get_bool("mii", "service", "RUZU_TRACE_MII_SERVICE", false),
        acc_profile_trace: get_bool("account", "profile", "RUZU_TRACE_ACC_PROFILE", false),
        hid_npad_trace: get_bool("hid", "npad", "RUZU_TRACE_HID_NPAD", false),
        output_target: get_str("output", "target", "RUZU_TRACE_TARGET", "stderr"),
        output_file: file
            .as_ref()
            .and_then(|t| t.get("output"))
            .and_then(|s| s.get("file_path").or_else(|| s.get("file")))
            .and_then(|x| x.as_str())
            .map(str::to_string)
            .or_else(|| env_string("RUZU_TRACE_FILE"))
            .unwrap_or_default(),
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
        // Keep clean runs free of trace-thread wakeups; emitters only need the
        // drain thread when a category is actually enabled.
        if cfg.any_trace_enabled() {
            ensure_drain_thread(&cfg);
        }
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
        cat::SVC_IPC_PROGRESS => c.svc_ipc_progress_trace,
        cat::SSR_IPC => c.ssr_ipc_trace,
        cat::BINDER_TXN => c.binder_txn_trace,
        cat::BQP => c.bqp_trace,
        cat::GPU_VA_MAP | cat::GPU_VA_UNMAP => c.gpu_va_trace,
        cat::HEAP => c.heap_trace,
        cat::TID_SVC => c.tid_svc_trace,
        cat::LOCK_PI => c.lock_pi_trace,
        cat::WATCH_READ | cat::WATCH_WRITE => c.watch_read_write,
        cat::STLEX => c.stlex_trace,
        cat::UNMAPPED_WRITE => c.unmapped_write_trace,
        cat::UNMAPPED_READ => c.unmapped_read_trace,
        cat::A64_EXCEPTION_CTX => c.a64_exception_trace,
        cat::A64_BREAK_CTX => c.a64_break_trace,
        cat::THREAD_CORE_MASK => c.thread_core_mask_trace,
        cat::CREATE_THREAD => c.create_thread_trace,
        cat::SVC_ERRVAL => c.svc_errval_trace,
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
        cat::PRESENT_TEXTURE => c.present_texture_trace,
        cat::AUDIO_DEVICE_SINK => c.audio_device_sink_trace,
        cat::AUDIO_OUT_BUFFER => c.audio_out_buffer_trace,
        cat::AUDIO_VOICE => c.audio_voice_trace,
        cat::HOST1X_VIDEO => c.host1x_video_trace,
        cat::HOST1X_SYNCPOINT => c.host1x_syncpoint_trace,
        cat::GPU_THREAD => c.gpu_thread_trace,
        cat::DMA_PUSHER => c.dma_pusher_trace,
        cat::GL_DRAW_PROFILE => c.gl_draw_profile_trace,
        cat::GL_PIPELINE => c.gl_pipeline_trace,
        cat::PRESENT_COMPOSITE => c.present_composite_trace,
        cat::NVMAP_ALLOC => c.nvmap_alloc_trace,
        cat::PRESENT_ALIAS => c.present_alias_trace,
        cat::PRESENT_IMAGE_SELECT => c.present_image_select_trace,
        cat::RT_DEPTH_ATTACH => c.rt_depth_attach_trace,
        cat::RT_ZETA_BIND => c.rt_zeta_bind_trace,
        cat::SUBMIT_GPFIFO => c.submit_gpfifo_trace,
        cat::MII_SERVICE => c.mii_service_trace,
        cat::ACC_PROFILE => c.acc_profile_trace,
        cat::HID_NPAD => c.hid_npad_trace,
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
            let _ = write!(
                out,
                "IPC_REPLY seq={} service={} cmd={}",
                rec.args[0], svc, rec.args[2]
            );
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
        cat::SSR_IPC => {
            // args = [stage(0=enter,1=exit), guest_tid, handle, cmd, cmd_type, result, service_name_id]
            let stage = if rec.args[0] == 0 { "ENTER" } else { "EXIT" };
            let svc = service_name(rec.args[6] as u16);
            let _ = writeln!(
                out,
                "[SSR] tid={} handle={:#x} service={} cmd={} cmd_type={} result={:#x} {}",
                rec.args[1], rec.args[2], svc, rec.args[3], rec.args[4], rec.args[5], stage
            );
        }
        cat::IPC_REQUEST => {
            let svc = service_name(rec.args[1] as u16);
            let _ = write!(
                out,
                "IPC_REQUEST seq={} service={} cmd={} tid={}",
                rec.args[0], svc, rec.args[2], rec.args[4]
            );
            let ioctl = rec.args[3] as u32;
            if ioctl != 0 {
                let _ = write!(out, " ioctl=0x{:08X}", ioctl);
            }
            out.push_str(" payload=");
            for i in 5..rec.arg_count as usize {
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
            let _ = writeln!(
                out,
                "GPU_VA seq={} op=unmap gpu_va=0x{:X}",
                rec.args[0], rec.args[1]
            );
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
            let pc = rec.args[4];
            let lr = rec.args[5];
            if pc != 0 || lr != 0 {
                let _ = write!(out, " pc=0x{:016X} lr=0x{:016X}", pc, lr);
            }
            let _ = writeln!(
                out,
                " args=[0x{:X}, 0x{:X}, 0x{:X}, 0x{:X}]",
                rec.args[6], rec.args[7], rec.args[8], rec.args[9]
            );
        }
        cat::SVC_ERRVAL => {
            // args = [target_result, tid, core, svc_name_id, pc, lr, a0, a1, a2, a3, ret0, ret1, ret2, ret3]
            let name = svc_name(rec.args[3] as u16);
            let _ = writeln!(
                out,
                "[SVC_ERRVAL] target=0x{:X} tid={} core={} svc={} pc=0x{:016X} lr=0x{:016X} args=[0x{:X}, 0x{:X}, 0x{:X}, 0x{:X}] ret=[0x{:X}, 0x{:X}, 0x{:X}, 0x{:X}]",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                name,
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
        cat::THREAD_CORE_MASK => {
            let stage = match rec.args[0] {
                1 => "enter",
                2 => "resolved",
                3 => "return",
                4 => "set_begin",
                5 => "wait_retry",
                6 => "wait_pinned",
                7 => "set_end",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[THREAD_CORE_MASK] stage={} caller_tid={} target_tid={} handle=0x{:X} core_id={} affinity=0x{:X} state=0x{:X} active_core={} current_core={} result=0x{:X} pinned={} waiters={}",
                stage,
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4] as i32,
                rec.args[5],
                rec.args[6],
                rec.args[7] as i32,
                rec.args[8] as i32,
                rec.args[9],
                rec.args[10],
                rec.args[11],
            );
        }
        cat::CREATE_THREAD => {
            let stage = match rec.args[0] {
                1 => "enter",
                2 => "invalid_core",
                3 => "invalid_priority",
                4 => "limit_reached",
                5 => "init_fail",
                6 => "handle_table_fail",
                7 => "success",
                8 => "out_of_handles",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[CREATE_THREAD] stage={} caller_tid={} entry=0x{:016X} arg=0x{:016X} stack=0x{:016X} priority={} requested_core={} effective_core={} process_core_mask=0x{:X} result=0x{:X} out_handle=0x{:X} extra=0x{:X}",
                stage,
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5] as i32,
                rec.args[6] as i32,
                rec.args[7] as i32,
                rec.args[8],
                rec.args[9],
                rec.args[10],
                rec.args[11],
            );
        }
        cat::LOCK_PI => {
            let stage = match rec.args[0] {
                1 => "wait_enter",
                2 => "wait_no_wait",
                3 => "wait_owner",
                4 => "wait_sleep",
                5 => "signal_enter",
                6 => "signal_next",
                7 => "signal_write",
                8 => "signal_wake",
                9 => "cv_wait_next",
                10 => "cv_wait_write_key",
                11 => "cv_wait_write_addr",
                12 => "cv_wait_enqueue",
                13 => "cv_signal_update",
                14 => "cv_signal_immediate",
                15 => "cv_signal_requeue",
                16 => "cv_signal_invalid",
                17 => "wait_self_owner",
                18 => "wait_zero_tag_tls",
                19 => "wait_tag_tls_mismatch",
                20 => "svc_wait_enter",
                21 => "svc_wait_return",
                22 => "svc_lock_handles",
                23 => "wait_tag_ctx",
                _ => "unknown",
            };
            if rec.args[0] == 23 {
                let _ = writeln!(
                    out,
                    "[LOCK_PI] stage={} tid={} addr=0x{:X} handle=0x{:X} tag=0x{:X} tls_handle=0x{:X} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} x0=0x{:016X} x1=0x{:016X} x2=0x{:016X} x20=0x{:016X} jit_tpidrro=0x{:016X}",
                    stage,
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
            } else {
                let _ = writeln!(
                    out,
                    "[LOCK_PI] stage={} tid={} addr=0x{:X} handle=0x{:X} tag=0x{:X} test_tag=0x{:X} owner_tid={} next_tid={} has_waiters={} next_value=0x{:X} result=0x{:X} owner_obj={} waiter_count={} extra=0x{:X}",
                    stage,
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
        }
        cat::WATCH_READ => {
            let _ = writeln!(
                out,
                "[WATCH_READ ] core={} tid={} pc=0x{:016X} lr=0x{:016X} vaddr=0x{:X} size={} value=0x{:016X}{:016X}",
                rec.args[0] as i32,
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[7],
                rec.args[6],
            );
        }
        cat::WATCH_WRITE => {
            let _ = writeln!(
                out,
                "[WATCH_WRITE] core={} tid={} pc=0x{:016X} lr=0x{:016X} vaddr=0x{:X} size={} value=0x{:016X}{:016X}",
                rec.args[0] as i32,
                rec.args[1],
                rec.args[2],
                rec.args[3],
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
                "[UNMAPPED_WRITE] tid={} pc=0x{:016X} lr=0x{:016X} vaddr=0x{:X} size={} value=0x{:016X}{:016X}",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[6],
                rec.args[5],
            );
        }
        cat::UNMAPPED_READ => {
            let _ = writeln!(
                out,
                "[UNMAPPED_READ ] core={} tid={} pc=0x{:016X} lr=0x{:016X} vaddr=0x{:X} size={} x0=0x{:016X} x8=0x{:016X} x9=0x{:016X} x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x22=0x{:016X} x25=0x{:016X}",
                rec.args[0] as i32,
                rec.tid,
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
            );
        }
        cat::A64_EXCEPTION_CTX => match rec.args[0] {
            0 => {
                let reason = match rec.args[1] {
                    1 => "PrefetchAbort",
                    2 => "DataAbort",
                    3 => "UndefinedInstruction",
                    _ => "Other",
                };
                let _ = writeln!(
                    out,
                    "[A64_EXCEPTION] reason={} core={} tid={} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} x0=0x{:016X} x8=0x{:016X} x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x23=0x{:016X} x24=0x{:016X}",
                    reason,
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
            1 => {
                let _ = writeln!(
                    out,
                    "[A64_EXCEPTION_MEM] base=0x{:016X} q0=0x{:016X} q1=0x{:016X} q2=0x{:016X} q3=0x{:016X}",
                    rec.args[1], rec.args[2], rec.args[3], rec.args[4], rec.args[5],
                );
            }
            2 | 3 => {
                let stage = if rec.args[0] == 2 {
                    "CONTEXT_SAVE"
                } else {
                    "CONTEXT_LOAD"
                };
                let _ = writeln!(
                    out,
                    "[A64_{}] core={} tid={} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} x0=0x{:016X} x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x29=0x{:016X}",
                    stage,
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
            6 => {
                let _ = writeln!(
                    out,
                    "[A64_EXCEPTION_REGS] core={} tid={} x1=0x{:016X} x2=0x{:016X} x3=0x{:016X} x4=0x{:016X} x5=0x{:016X} x6=0x{:016X} x7=0x{:016X} x8=0x{:016X} x16=0x{:016X} x17=0x{:016X} x29=0x{:016X}",
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
            7 => {
                let _ = writeln!(
                    out,
                    "[A64_EXCEPTION_REGS2] core={} tid={} x9=0x{:016X} x10=0x{:016X} x11=0x{:016X} x12=0x{:016X} x13=0x{:016X} x14=0x{:016X} x15=0x{:016X} x18=0x{:016X} x22=0x{:016X} x25=0x{:016X} x26=0x{:016X}",
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
            8 => {
                if rec.args[1] == u64::MAX {
                    let _ = writeln!(
                        out,
                        "[A64_EXCEPTION_PTR] base=0x{:016X} q0=0x{:016X} q1=0x{:016X} q2=0x{:016X} q3=0x{:016X}",
                        rec.args[2],
                        rec.args[3],
                        rec.args[4],
                        rec.args[5],
                        rec.args[6],
                    );
                } else {
                    let _ = writeln!(
                        out,
                        "[A64_EXCEPTION_PTR] x{}=0x{:016X} q0=0x{:016X} q1=0x{:016X} q2=0x{:016X} q3=0x{:016X}",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3],
                        rec.args[4],
                        rec.args[5],
                        rec.args[6],
                    );
                }
            }
            9 => {
                let _ = writeln!(
                    out,
                    "[A64_EXCEPTION_REGS3] core={} tid={} x27=0x{:016X} x28=0x{:016X}",
                    rec.args[1], rec.args[2], rec.args[3], rec.args[4],
                );
            }
            4 => {
                let _ = writeln!(
                    out,
                    "[A64_FALLBACK] core={} tid={} pc=0x{:016X} instr=0x{:08X} count={} jit_pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} x0=0x{:016X} x8=0x{:016X} x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x23=0x{:016X}",
                    rec.args[1],
                    rec.tid,
                    rec.args[2],
                    rec.args[3] as u32,
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
            5 => {
                let _ = writeln!(
                    out,
                    "[A64_NULL_FETCH] core={} tid={} fetch=0x{:016X} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} x0=0x{:016X} x1=0x{:016X} x2=0x{:016X} x3=0x{:016X} x4=0x{:016X} x5=0x{:016X} x19=0x{:016X} x20=0x{:016X}",
                    rec.args[1],
                    rec.tid,
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
            _ => {
                let _ = writeln!(out, "[A64_EXCEPTION] stage={}", rec.args[0]);
            }
        },
        cat::A64_BREAK_CTX => match rec.args[0] {
            0 => {
                let _ = writeln!(
                    out,
                    "[A64_BREAK] reason=0x{:X} core={} tid={} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} info1=0x{:016X} info2=0x{:016X}",
                    rec.args[1],
                    rec.args[2],
                    rec.args[3],
                    rec.args[4],
                    rec.args[5],
                    rec.args[6],
                    rec.args[7],
                    rec.args[8],
                );
            }
            1 => {
                let _ = writeln!(
                    out,
                    "[A64_BREAK_STACK] base=0x{:016X} q0=0x{:016X} q1=0x{:016X} q2=0x{:016X} q3=0x{:016X}",
                    rec.args[1], rec.args[2], rec.args[3], rec.args[4], rec.args[5],
                );
            }
            2 => {
                let _ = writeln!(
                    out,
                    "[A64_BREAK_INFO] base=0x{:016X} q0=0x{:016X} q1=0x{:016X} q2=0x{:016X} q3=0x{:016X}",
                    rec.args[1], rec.args[2], rec.args[3], rec.args[4], rec.args[5],
                );
            }
            3 => {
                let mut bytes = [0u8; 32];
                for (chunk_index, value) in rec.args[3..7].iter().enumerate() {
                    let raw = value.to_le_bytes();
                    bytes[chunk_index * 8..chunk_index * 8 + 8].copy_from_slice(&raw);
                }
                let len = (rec.args[2] as usize).min(bytes.len());
                let mut hex = String::new();
                let mut ascii = String::new();
                for &byte in &bytes[..len] {
                    let _ = write!(hex, "{:02X}", byte);
                    ascii.push(if byte.is_ascii_graphic() || byte == b' ' {
                        byte as char
                    } else {
                        '.'
                    });
                }
                let _ = writeln!(
                    out,
                    "[A64_BREAK_PTR] ptr=0x{:016X} len={} hex={} ascii=\"{}\"",
                    rec.args[1], len, hex, ascii,
                );
            }
            _ => {
                let _ = writeln!(out, "[A64_BREAK] stage={}", rec.args[0]);
            }
        },
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
                6 => "unused_spawn_fallback_removed",
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
                39 => "before_client_ipc_wait_thread_lock",
                40 => "after_client_ipc_wait_thread_lock",
                41 => "after_client_ipc_begin_wait",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[HOST_THREAD_IPC] handle=0x{:X} stage={}",
                rec.args[1] as u32, stage,
            );
        }
        cat::PLU_IPC => match rec.args[0] {
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
                let _ = writeln!(
                    out,
                    "[PLU_IPC] kind={} args={:?}",
                    rec.args[0],
                    &rec.args[..rec.arg_count as usize]
                );
            }
        },
        cat::SVC_IPC_PROGRESS => {
            let stage = match rec.args[0] {
                1 => "enter",
                2 => "resolved_client",
                3 => "inline_prepare",
                4 => "inline_receive_begin",
                5 => "inline_receive_end",
                6 => "handler_begin",
                7 => "handler_end",
                8 => "reply_begin",
                9 => "reply_end",
                10 => "return",
                11 => "resolve_manager_begin",
                12 => "resolve_manager_process",
                13 => "resolve_manager_parent_found",
                14 => "resolve_manager_parent_locked",
                15 => "resolve_manager_server_session",
                16 => "resolve_manager_server_locked",
                17 => "resolve_manager_end",
                18 => "host_service_name_begin",
                19 => "host_service_name_locked",
                20 => "host_service_name_end",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[SVC_IPC_PROGRESS] stage={}({}) tid={} handle=0x{:X} client_obj={} tls=0x{:X} aux=[0x{:X},0x{:X},0x{:X}]",
                stage,
                rec.args[0],
                rec.args[1],
                rec.args[2] as u32,
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
            );
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
                11 => "event-create",
                12 => "event-signal-host",
                13 => "query-hit",
                14 => "query-miss",
                _ => "unknown",
            };
            let result = match rec.args[7] {
                0 => "success",
                1 => "timeout",
                2 => "bad-parameter",
                3 => "busy",
                _ => "unknown",
            };
            if (11..=14).contains(&rec.args[0]) {
                let _ = writeln!(
                    out,
                    "[NVHOST_CTRL_WAIT] stage={} syncpt_id={} assigned_value={} slot={} value_raw=0x{:08X} event_status={} object_id=0x{:X}",
                    stage,
                    rec.args[1],
                    rec.args[2],
                    rec.args[5],
                    rec.args[6] as u32,
                    rec.args[7],
                    rec.args[8],
                );
            } else {
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
        }
        cat::SCHED_STATE => {
            let top = |idx: usize| -> String {
                let value = rec.args[idx];
                if value == u64::MAX {
                    "None".to_string()
                } else {
                    value.to_string()
                }
            };
            match rec.args[0] {
                1..=3 => {
                    let stage = match rec.args[0] {
                        1 => "transition",
                        2 => "pq_remove",
                        3 => "pq_push",
                        _ => unreachable!(),
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
                4 => {
                    let _ = writeln!(
                        out,
                        "[SCHED_STATE] stage=schedule_select cur={} highest={} target={} core={} needs={} interrupt={} switch_from={} top=[{},{},{},{}]",
                        top(1),
                        top(2),
                        top(3),
                        rec.args[4] as i32,
                        rec.args[5] != 0,
                        rec.args[6] != 0,
                        rec.args[7] != 0,
                        top(8),
                        top(9),
                        top(10),
                        top(11),
                    );
                }
                5 => {
                    let _ = writeln!(
                        out,
                        "[SCHED_STATE] stage=switch cur={} next={} core={} has_gsc={} prev={}",
                        top(1),
                        top(2),
                        rec.args[4] as i32,
                        rec.args[5] != 0,
                        top(6),
                    );
                }
                6 | 7 => {
                    let stage = if rec.args[0] == 6 {
                        "scheduler_lock_unlock"
                    } else {
                        "scheduler_lock_enable"
                    };
                    let _ = writeln!(
                        out,
                        "[SCHED_STATE] stage={} owner_sched_id={} guest_tid={} cores=0x{:X} lock_count={}",
                        stage,
                        rec.args[1],
                        top(2),
                        rec.args[3],
                        rec.args[4] as i32,
                    );
                }
                8..=13 => {
                    let stage = match rec.args[0] {
                        8 => "yield_to_switch_before",
                        9 => "yield_to_switch_after",
                        10 => "try_lock_target_before",
                        11 => "try_lock_target_after",
                        12 => "yield_to_thread_before",
                        13 => "yield_to_thread_after",
                        _ => unreachable!(),
                    };
                    let _ = writeln!(
                        out,
                        "[SCHED_STATE] stage={} cur={} target={} core={} target_active={} target_current={} target_prio={} has_ctx={} needs={}",
                        stage,
                        top(1),
                        top(2),
                        rec.args[3] as i32,
                        rec.args[4] as i32,
                        rec.args[5] as i32,
                        rec.args[6] as i32,
                        rec.args[7] != 0,
                        rec.args[8] != 0,
                    );
                    if rec.args.len() > 9 {
                        let _ = writeln!(
                            out,
                            "[SCHED_STATE] stage={}_ctx target_host=0x{:016X}",
                            stage, rec.args[9]
                        );
                    }
                }
                14 => {
                    let phase = match rec.args[1] {
                        1 => "pre_inner_stale_interrupt",
                        2 => "enter_inner_loop",
                        3 => "exit_inner_loop",
                        4 => "after_handle_interrupt",
                        5 => "after_reschedule",
                        _ => "unknown",
                    };
                    let _ = writeln!(
                        out,
                        "[SCHED_STATE] stage=core_dispatch phase={} core={} tid={} thread_current={} thread_active={} raw_state=0x{:X}",
                        phase,
                        rec.args[2],
                        rec.args[3],
                        rec.args[4] as i32,
                        rec.args[5] as i32,
                        rec.args[6],
                    );
                }
                15 => {
                    let _ = writeln!(
                        out,
                        "[SCHED_STATE] stage=apply_wait_result tid={} result=0x{:X} pc=0x{:016X} sp=0x{:016X} old_r0=0x{:016X} old_r1=0x{:016X} raw_state=0x{:X} has_wait_queue={} wait_reason={} x19=0x{:016X}",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3],
                        rec.args[4],
                        rec.args[5],
                        rec.args[6],
                        rec.args[7],
                        rec.args[8] != 0,
                        rec.args[9],
                        rec.args[10],
                    );
                }
                16 => {
                    let _ = writeln!(
                        out,
                        "[SCHED_STATE] stage=reload tid={} core={} pc=0x{:016X} sp=0x{:016X} r0=0x{:016X} r1=0x{:016X} lr=0x{:016X} tls=0x{:016X} jit_tpidrro=0x{:016X}",
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
                17 => {
                    let owner_core = if rec.args[5] == u64::MAX {
                        "None".to_string()
                    } else {
                        rec.args[5].to_string()
                    };
                    let scheduler_current = if rec.args[8] == u64::MAX {
                        "None".to_string()
                    } else {
                        rec.args[8].to_string()
                    };
                    let _ = writeln!(
                        out,
                        "[SCHED_STATE] stage=thread_core_mismatch kind={} tid={} ptr=0x{:016X} entering_core={} owner_core={} thread_current={} thread_active={} scheduler_current={} thread_host=0x{:016X} scheduler_host=0x{:016X}",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3],
                        rec.args[4],
                        owner_core,
                        rec.args[6] as i32,
                        rec.args[7] as i32,
                        scheduler_current,
                        rec.args.get(9).copied().unwrap_or(0),
                        rec.args.get(10).copied().unwrap_or(0),
                    );
                }
                _ => {
                    let _ = writeln!(out, "[SCHED_STATE] stage=unknown args={:?}", rec.args);
                }
            }
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
        cat::HWC => match rec.args[0] {
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
                let _ = writeln!(
                    out,
                    "[HWC] stage={} args={:?}",
                    rec.args[0],
                    &rec.args[..rec.arg_count as usize]
                );
            }
        },
        cat::BQP => match rec.args[0] {
            1 => {
                let _ = writeln!(
                        out,
                        "[BQP] stage=dequeue_enter seq={} async={} size={}x{} format=0x{:X} usage=0x{:X} tid={}",
                        rec.args[1],
                        rec.args[2] != 0,
                        rec.args[3],
                        rec.args[4],
                        rec.args[5],
                        rec.args[6],
                        rec.args.get(7).copied().unwrap_or(0),
                    );
            }
            2 => {
                let _ = writeln!(
                    out,
                    "[BQP] stage=dequeue_return seq={} status={} slot={} flags=0x{:X} tid={}",
                    rec.args[1],
                    rec.args[2] as i32,
                    rec.args[3] as i32,
                    rec.args[4],
                    rec.args.get(5).copied().unwrap_or(0),
                );
            }
            3 => {
                let _ = writeln!(
                    out,
                    "[BQP] stage=queue_enter seq={} slot={} tid={}",
                    rec.args[1],
                    rec.args[2] as i32,
                    rec.args.get(3).copied().unwrap_or(0),
                );
            }
            4 => {
                let _ = writeln!(
                        out,
                        "[BQP] stage=queue_commit seq={} slot={} frame={} queue_len={} droppable={} acquire_called={} tid={}",
                        rec.args[1],
                        rec.args[2] as i32,
                        rec.args[3],
                        rec.args[4],
                        rec.args[5] != 0,
                        rec.args[6] != 0,
                        rec.args.get(7).copied().unwrap_or(0),
                    );
            }
            5 => {
                let _ = writeln!(
                    out,
                    "[BQP] stage=queue_return seq={} status={} slot={} tid={}",
                    rec.args[1],
                    rec.args[2] as i32,
                    rec.args[3] as i32,
                    rec.args.get(4).copied().unwrap_or(0),
                );
            }
            6 => {
                let _ = writeln!(
                    out,
                    "[BQP] stage=cancel seq={} slot={} tid={}",
                    rec.args[1],
                    rec.args[2] as i32,
                    rec.args.get(3).copied().unwrap_or(0),
                );
            }
            7 => {
                let _ = writeln!(
                    out,
                    "[BQP] stage=request_buffer seq={} slot={} tid={}",
                    rec.args[1],
                    rec.args[2] as i32,
                    rec.args.get(3).copied().unwrap_or(0),
                );
            }
            20 => {
                let _ = writeln!(
                    out,
                    "[BQP] stage=signal_wait_event seq={} readable_event={} was_signaled={} tid={}",
                    rec.args[1],
                    rec.args[2],
                    rec.args[3] != 0,
                    rec.args.get(4).copied().unwrap_or(0),
                );
            }
            21 => {
                let _ = writeln!(
                    out,
                    "[BQP] stage=get_native_handle seq={} readable_event={} tid={}",
                    rec.args[1],
                    rec.args[2],
                    rec.args.get(3).copied().unwrap_or(0),
                );
            }
            22 => {
                let _ = writeln!(
                    out,
                    "[BQP] stage=register_wait_event seq={} event={} readable_event={} signaled={} tid={}",
                    rec.args[1],
                    rec.args[2],
                    rec.args[3],
                    rec.args[4] != 0,
                    rec.args.get(5).copied().unwrap_or(0),
                );
            }
            _ => {
                let _ = writeln!(
                    out,
                    "[BQP] stage={} args={:?}",
                    rec.args[0],
                    &rec.args[..rec.arg_count as usize]
                );
            }
        },
        cat::BINDER_TXN => {
            let stage = match rec.args[0] {
                1 => "enter",
                2 => "reply",
                3 => "missing_binder",
                4 => "reply_words",
                _ => "unknown",
            };
            let status = rec.args.get(7).copied().unwrap_or(0) as i32;
            if rec.args[0] == 4 {
                let _ = writeln!(
                    out,
                    "[BINDER_TXN] stage={} seq={} id={} txn={} off=0x{:X} w0={:08X} w1={:08X} w2={:08X} w3={:08X} w4={:08X} w5={:08X} w6={:08X} w7={:08X} w8={:08X}",
                    stage,
                    rec.args[1],
                    rec.args[2] as i32,
                    rec.args[3],
                    rec.args[4],
                    rec.args[5] as u32,
                    rec.args[6] as u32,
                    rec.args[7] as u32,
                    rec.args[8] as u32,
                    rec.args[9] as u32,
                    rec.args[10] as u32,
                    rec.args[11] as u32,
                    rec.args[12] as u32,
                    rec.args[13] as u32,
                );
            } else {
                let _ = writeln!(
                    out,
                    "[BINDER_TXN] stage={} seq={} id={} txn={} flags=0x{:X} in_len={} out_len={} status={}",
                    stage,
                    rec.args[1],
                    rec.args[2] as i32,
                    rec.args[3],
                    rec.args[4],
                    rec.args[5],
                    rec.args[6],
                    status,
                );
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
            let unpack_size =
                |packed: u64| -> (u32, u32) { ((packed >> 32) as u32, packed as u32) };
            if rec.args[0] == u64::MAX - 1 {
                let (width, height) = unpack_size(rec.args[10]);
                let _ = writeln!(
                        out,
                        "[RT_DRAW_RESULT] draw_seq={} pipeline={} query={} any_samples={} gl_error=0x{:X} indexed={} primitive=0x{:X} vertices={} instances={} rt0=0x{:X}/{}x{} fbo={}",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3] != 0,
                        rec.args[4] != 0,
                        rec.args[5],
                        rec.args[6] != 0,
                        rec.args[7],
                        rec.args[8],
                        rec.args[9],
                        rec.args[11],
                        width,
                        height,
                        rec.args[12],
                    );
                return;
            }
            if rec.args[0] == u64::MAX {
                let (width, height) = unpack_size(rec.args[9]);
                let _ = writeln!(
                        out,
                        "[RT_PIPELINE_STATUS] draw_seq={} pipeline_before={} pipeline_after={} has_sources=0x{:X} has_programs={} can_draw={} rt0=0x{:X}/fmt=0x{:X}/{}x{} fbo={} build_attempted={} build_failed={}",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3],
                        rec.args[4],
                        rec.args[5] != 0,
                        rec.args[6] != 0,
                        rec.args[7],
                        rec.args[8],
                        width,
                        height,
                        rec.args[10],
                        rec.args[11] != 0,
                        rec.args[12] != 0,
                    );
                return;
            }
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
        cat::RT_DEPTH_ATTACH => {
            let width = rec.args[5] >> 32;
            let height = rec.args[5] & 0xffff_ffff;
            let _ = writeln!(
                out,
                "[RT_DEPTH_ATTACH] fbo={} view_id={} image_id={} zeta_gpu=0x{:X} fmt=0x{:X} size={}x{} texture={} attachment=0x{:X} buffer_bits=0x{:X} status=0x{:X} color0_view={} color1_view={} draw_buffers=0x{:X}",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                width,
                height,
                rec.args[6],
                rec.args[7],
                rec.args[8],
                rec.args[9],
                rec.args[10],
                rec.args[11],
                rec.args[12],
            );
        }
        cat::RT_ZETA_BIND => {
            let zeta_width = rec.args[6] >> 32;
            let zeta_height = rec.args[6] & 0xffff_ffff;
            let rt0_width = rec.args[9] >> 32;
            let rt0_height = rec.args[9] & 0xffff_ffff;
            let _ = writeln!(
                out,
                "[RT_ZETA_BIND] draw_seq={} pipeline={} fbo={} zeta_enabled={} zeta_gpu=0x{:X} zeta_fmt=0x{:X} zeta_size={}x{} rt0_gpu=0x{:X} rt0_fmt=0x{:X} rt0_size={}x{} depth_test={} depth_write={} depth_func=0x{:X} depth_mode=0x{:X}",
                rec.args[0],
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                zeta_width,
                zeta_height,
                rec.args[7],
                rec.args[8],
                rt0_width,
                rt0_height,
                rec.args[10],
                rec.args[11],
                rec.args[12],
                rec.args[13],
            );
        }
        cat::SUBMIT_GPFIFO => {
            let stage = match rec.args[0] {
                1 => "after-max",
                2 => "after-push-main",
                3 => "after-push-fence",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[SUBMIT_GPFIFO] stage={} seq={} tid={} bind_id={} syncpt_id={} flags=0x{:X} increment={} fence_in={{id={}, value={}}} fence_out_value={} min={} max={}",
                stage,
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7] as i32,
                rec.args[8],
                rec.args[9],
                rec.args[10],
                rec.args[11],
            );
        }
        cat::RT_SAMPLE => {
            let _ = writeln!(
                out,
                "[RT_SAMPLE] draw_seq={} pipeline={} fbo={} rt0_gpu=0x{:X} fmt=0x{:X} size={}x{} probe=({}, {}) rgb_nonzero={} alpha_nonzero={} checksum=0x{:X} probe_rgba=0x{:08X} gl_error=0x{:X}",
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
            let unpack_xy = |packed: u64| -> (u32, u32) { ((packed >> 32) as u32, packed as u32) };
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
        cat::GL_DRAW_STATE => match rec.args[0] {
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
            6 => {
                let _ = writeln!(
                        out,
                        "[GL_DRAW_STATE] stage=pipeline_status draw_seq={} pipeline_before={} pipeline_after={} has_sources=0x{:X} has_programs={} can_draw={} rt0=0x{:X}/fmt=0x{:X}/{}x{} fbo={} build_attempted={} build_failed={}",
                        rec.args[1],
                        rec.args[2],
                        rec.args[3],
                        rec.args[4],
                        rec.args[5] != 0,
                        rec.args[6] != 0,
                        rec.args[7],
                        rec.args[8],
                        rec.args[9] >> 32,
                        rec.args[9] & 0xffff_ffff,
                        rec.args[10],
                        rec.args[11] != 0,
                        rec.args[12] != 0,
                    );
            }
            _ => {
                let _ = writeln!(
                    out,
                    "[GL_DRAW_STATE] stage={} draw_seq={} pipeline={}",
                    rec.args[0], rec.args[1], rec.args[2],
                );
            }
        },
        cat::RT_GRID_PHASE => {
            let unpack_xy = |packed: u64| -> (u32, u32) { ((packed >> 32) as u32, packed as u32) };
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
                "[TEXTURE_BIND_ADDR] draw_seq={} pipeline={} stage={} unit={} view_id={} image_id={} gpu=0x{:X} size={}x{} handle={} desc_type={} is_depth={} is_msaa={}",
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
                9 => "swizzle",
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
            if rec.args[0] == 9 {
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
                    "[IMAGE_VIEW] stage=swizzle view_id={} image_id={} image_tex={} gpu=0x{:X} format={} view_type={} source_swizzle=0x{:08X} gl_swizzle=0x{:016X} default={} color2d={} color_array2d={}",
                    rec.args[1],
                    rec.args[2],
                    rec.args[3],
                    rec.args[4],
                    rec.args[5],
                    view_type,
                    rec.args[7],
                    rec.args[8],
                    rec.args[9],
                    rec.args[10],
                    rec.args[11],
                );
                return;
            }
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
        cat::PRESENT_TEXTURE => {
            if rec.args[0] == u64::MAX {
                let _ = writeln!(
                    out,
                    "[PRESENT_LAYER] stage={} gpu=0x{:X} base=0x{:X} offset=0x{:X} size={}x{} stride={} fmt=0x{:X} aux0=0x{:X} aux1=0x{:X} aux2=0x{:X} aux3=0x{:X}",
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
                );
                return;
            }
            let image_id = if rec.args[2] == u64::MAX {
                "missing".to_string()
            } else {
                rec.args[2].to_string()
            };
            let _ = writeln!(
                out,
                "[PRESENT_TEXTURE] present={} gpu=0x{:X} image={} texture={} size={}x{} fmt=0x{:X} gl_error=0x{:X} samples={} rgb_nonzero={} alpha_nonzero={} checksum=0x{:X} first_rgba=0x{:08X} last_rgba=0x{:08X}",
                rec.args[0],
                rec.args[1],
                image_id,
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
        cat::PRESENT_ALIAS => {
            let image_id = if rec.args[2] == u64::MAX {
                "missing".to_string()
            } else {
                rec.args[2].to_string()
            };
            let view_id = if rec.args[3] == u64::MAX {
                "missing".to_string()
            } else {
                rec.args[3].to_string()
            };
            let _ = writeln!(
                out,
                "[PRESENT_ALIAS] present={} gpu=0x{:X} image={} view={} current_texture={} view_texture={} size={}x{} current_checksum=0x{:X} view_checksum=0x{:X} current_rgb_nonzero={} view_rgb_nonzero={} current_first_rgba=0x{:08X} view_first_rgba=0x{:08X}",
                rec.args[0],
                rec.args[1],
                image_id,
                view_id,
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
        cat::PRESENT_COMPOSITE => {
            let stage = match rec.args[0] {
                0 => "enter",
                1 => "empty",
                2 => "after_draw_screen",
                3 => "before_swap",
                4 => "after_swap",
                5 => "sdl_swap",
                6 => "sdl_make_current",
                7 => "sdl_done_current",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[PRESENT_COMPOSITE] stage={} frame={} framebuffers={} current_frame={} draw_count={} size={}x{} gl_error=0x{:X}",
                stage,
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
            );
        }
        cat::NVMAP_ALLOC => {
            let kind = match rec.args[1] {
                1 => "IocCreate",
                2 => "IocAlloc",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[NVMAP_ALLOC] seq={} kind={} fd={} handle=0x{:X} size=0x{:X} addr=0x{:X} flags=0x{:X} align=0x{:X} heap_mask=0x{:X}",
                rec.args[0],
                kind,
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
                rec.args[8],
            );
        }
        cat::ACC_PROFILE => {
            let stage = match rec.args[0] {
                1 => "get",
                2 => "get_base",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[ACC_PROFILE] stage={} result=0x{:08X} user_id={:016X}{:016X} timestamp=0x{:X} name_words=[{:016X},{:016X}] data_words=[{:016X},{:016X}]",
                stage,
                rec.args[1] as u32,
                rec.args[3],
                rec.args[2],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
                rec.args[8],
            );
        }
        cat::HID_NPAD => {
            let _ = writeln!(
                out,
                "[HID_NPAD] cmd={} result=0x{:08X} aruid=0x{:X} arg0=0x{:X} arg1=0x{:X} out0=0x{:X} out1=0x{:X} out2=0x{:X}",
                rec.args[0],
                rec.args[1] as u32,
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
            );
        }
        cat::PRESENT_IMAGE_SELECT => {
            let image_id = if rec.args[2] == u64::MAX {
                "missing".to_string()
            } else {
                rec.args[2].to_string()
            };
            let view_id = if rec.args[3] == u64::MAX {
                "missing".to_string()
            } else {
                rec.args[3].to_string()
            };
            let _ = writeln!(
                out,
                "[PRESENT_IMAGE_SELECT] cpu=0x{:X} gpu=0x{:X} image={} view={} candidates={} flags=0x{:X} tick={} aliases={} overlaps={} size={}x{} fmt=0x{:X} fb_fmt=0x{:X} blending={}",
                rec.args[0],
                rec.args[1],
                image_id,
                view_id,
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
        cat::AUDIO_OUT_BUFFER => {
            let stage = match rec.args[0] {
                1 => "append_enter",
                2 => "append_result",
                3 => "register",
                4 => "release",
                5 => "get_released",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[AUDIO_OUT_BUFFER] stage={} session={} state={} tag=0x{:X} size={} samples=0x{:X} total_or_registered={} released_or_signal={} queue_or_played={} result={}",
                stage,
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
        cat::AUDIO_VOICE => {
            let _ = writeln!(
                out,
                "[AUDIO_VOICE] seq={} sorted={} in_use={} skipped={} active={} processed_voices={} processed_channels={} data_pushed={} data_skip_was_playing={} no_state={} no_command={} connected_channels={} valid_wavebuffers={} nonzero_wavebuffer_addr={}",
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
        cat::HOST1X_VIDEO => match rec.args[0] {
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
                    rec.args[1] as i32, rec.args[2],
                );
            }
            6 => {
                let step = match rec.args[2] {
                    1 => "config_read",
                    2 => "get_frame",
                    3 => "read_yuv",
                    4 => "blend",
                    5 => "write_surface",
                    6 => "total",
                    _ => "unknown",
                };
                let _ = writeln!(
                        out,
                        "[HOST1X_VIDEO] stage=vic_timing id={} step={} elapsed_us={} aux0=0x{:X} aux1={} aux2={}",
                        rec.args[1] as i32,
                        step,
                        rec.args[3],
                        rec.args[4],
                        rec.args[5],
                        rec.args[6],
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
        },
        cat::HOST1X_SYNCPOINT => {
            let stage = match rec.args[0] {
                1 => "register",
                2 => "register-immediate",
                3 => "increment",
                4 => "wait-begin",
                5 => "wait-ready",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[HOST1X_SYNCPOINT] stage={} kind={} id={} value={} current={} actions={}",
                stage,
                if rec.args[1] != 0 { "guest" } else { "host" },
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
            );
        }
        cat::GPU_THREAD => {
            let stage = match rec.args[0] {
                1 => "push-submit",
                2 => "pop-submit-begin",
                3 => "pop-submit-end",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[GPU_THREAD] stage={} fence={} channel={} lists={} prefetch={} elapsed_us={}",
                stage, rec.args[1], rec.args[2] as i32, rec.args[3], rec.args[4], rec.args[5],
            );
        }
        cat::DMA_PUSHER => {
            let stage = match rec.args[0] {
                1 => "dispatch-begin",
                2 => "dispatch-end",
                3 => "step-begin",
                4 => "prefetch-begin",
                5 => "prefetch-end",
                6 => "header-begin",
                7 => "header-end",
                8 => "empty",
                9 => "method-agg",
                10 => "method-begin",
                11 => "method-end",
                12 => "multi-method-begin",
                13 => "multi-method-end",
                _ => "unknown",
            };
            if rec.args[0] == 9 {
                let _ = writeln!(
                    out,
                    "[DMA_PUSHER] stage={} command_count={} dispatches={} subch={} method=0x{:X} calls={} words={} elapsed_us={} dma_get=0x{:X}",
                    stage,
                    rec.args[1],
                    rec.args[2],
                    rec.args[3],
                    rec.args[4],
                    rec.args[5],
                    rec.args[6],
                    rec.args[7],
                    rec.args.get(8).copied().unwrap_or(0),
                );
                return;
            }
            if (10..=13).contains(&rec.args[0]) {
                let _ = writeln!(
                    out,
                    "[DMA_PUSHER] stage={} dispatch={} index={}/{} subch={} method=0x{:X} arg_or_words=0x{:X} pending={} dma_segment=0x{:X} elapsed_us={}",
                    stage,
                    rec.args[1],
                    rec.args[2],
                    rec.args[3],
                    rec.args[4],
                    rec.args[5],
                    rec.args[6],
                    rec.args[7],
                    rec.args[8],
                    rec.args.get(9).copied().unwrap_or(0),
                );
                return;
            }
            let _ = writeln!(
                out,
                "[DMA_PUSHER] stage={} queue_len={} subindex={} addr=0x{:X} size={} non_main={} method_count={} dma_get=0x{:X} elapsed_us={}",
                stage,
                rec.args[1],
                rec.args[2],
                rec.args[3],
                rec.args[4],
                rec.args[5],
                rec.args[6],
                rec.args[7],
                rec.args.get(8).copied().unwrap_or(0),
            );
        }
        cat::GL_DRAW_PROFILE => {
            let _ = writeln!(
                out,
                "[GL_DRAW_PROFILE_RING] draw_seq={} pipeline={} indexed={} primitive=0x{:X} vertices={} instances={} total_us={} pipeline_us={} rt_us={} build_us={} configure_us={} update_buffers_us={} bind_buffers_us={} sync_draw_us={}",
                rec.args[0],
                rec.args[1],
                rec.args[2] != 0,
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
        cat::GL_PIPELINE => {
            let stage = match rec.args[0] {
                1 => "slow_enter",
                2 => "cache_hit",
                3 => "cache_miss",
                4 => "create_begin",
                5 => "create_end",
                6 => "insert",
                7 => "dual_vertex_begin",
                8 => "dual_vertex_end",
                9 => "stage_begin",
                10 => "stage_analyze_failed",
                11 => "stage_compile_end",
                12 => "build_stage_success",
                13 => "build_stage_failed",
                14 => "build_pipeline_success",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[GL_PIPELINE] stage={} seq={} cache_len={} key_raw=0x{:X} key_hash=0x{:016X} hashes=[0x{:016X},0x{:016X},0x{:016X},0x{:016X},0x{:016X},0x{:016X}] aux=[0x{:X},0x{:X},0x{:X}]",
                stage,
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
        cat::MII_SERVICE => {
            let stage = match rec.args[0] {
                1 => "get",
                2 => "get1",
                3 => "update_latest",
                4 => "build_random",
                5 => "build_default",
                6 => "get2",
                7 => "get3",
                8 => "update_latest1",
                9 => "convert_core_to_char",
                10 => "convert_v3",
                11 => "convert_char_to_core",
                12 => "is_updated",
                13 => "get_count",
                14 => "set_interface_version",
                15 => "is_full_database",
                _ => "unknown",
            };
            let _ = writeln!(
                out,
                "[MII_SERVICE] stage={} cmd={} result=0x{:08X} aux0=0x{:X} aux1=0x{:X} create_id={:016X}{:016X} name_words=[{:016X},{:016X}] traits=[{:016X},{:016X}]",
                stage,
                rec.args[1],
                rec.args[2] as u32,
                rec.args[3],
                rec.args[4],
                rec.args[6],
                rec.args[5],
                rec.args[7],
                rec.args[8],
                rec.args[9],
                rec.args[10],
            );
        }
        _ => {
            let _ = writeln!(
                out,
                "TRACE cat={} args={:?}",
                rec.category,
                &rec.args[..rec.arg_count as usize]
            );
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
