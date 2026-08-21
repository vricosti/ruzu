// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/cdma_pusher.h and video_core/cdma_pusher.cpp
//!
//! CDMA (Channel DMA) command processing for Host1x channels (NvDec, Vic, etc.).

use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex};
use std::thread::JoinHandle;

use common::thread::{set_current_thread_priority, ThreadPriority};

use crate::host1x::control::{Control, Method as ControlMethod};
use crate::host1x::syncpoint_manager::SyncpointManager;

/// CDMA submission modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ChSubmissionMode {
    SetClass = 0,
    Incrementing = 1,
    NonIncrementing = 2,
    Mask = 3,
    Immediate = 4,
    Restart = 5,
    Gather = 6,
}

impl ChSubmissionMode {
    pub fn from_u32(val: u32) -> Option<Self> {
        match val {
            0 => Some(Self::SetClass),
            1 => Some(Self::Incrementing),
            2 => Some(Self::NonIncrementing),
            3 => Some(Self::Mask),
            4 => Some(Self::Immediate),
            5 => Some(Self::Restart),
            6 => Some(Self::Gather),
            _ => None,
        }
    }
}

/// Channel class IDs for Host1x devices.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ChClassId {
    NoClass = 0x0,
    Control = 0x1,
    VideoEncodeMpeg = 0x20,
    VideoEncodeNvEnc = 0x21,
    VideoStreamingVi = 0x30,
    VideoStreamingIsp = 0x32,
    VideoStreamingIspB = 0x34,
    VideoStreamingViI2c = 0x36,
    GraphicsVic = 0x5d,
    Graphics3D = 0x60,
    GraphicsGpu = 0x61,
    Tsec = 0xe0,
    TsecB = 0xe1,
    NvJpg = 0xc0,
    NvDec = 0xf0,
}

impl ChClassId {
    pub fn from_u32(val: u32) -> Self {
        match val {
            0x0 => Self::NoClass,
            0x1 => Self::Control,
            0x20 => Self::VideoEncodeMpeg,
            0x21 => Self::VideoEncodeNvEnc,
            0x30 => Self::VideoStreamingVi,
            0x32 => Self::VideoStreamingIsp,
            0x34 => Self::VideoStreamingIspB,
            0x36 => Self::VideoStreamingViI2c,
            0x5d => Self::GraphicsVic,
            0x60 => Self::Graphics3D,
            0x61 => Self::GraphicsGpu,
            0xe0 => Self::Tsec,
            0xe1 => Self::TsecB,
            0xc0 => Self::NvJpg,
            0xf0 => Self::NvDec,
            _ => Self::NoClass,
        }
    }
}

/// Channel command header (32-bit bitfield).
///
/// Upstream: `union ChCommandHeader { u32 raw; BitField<0,16,u32> value; ... }`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ChCommandHeader {
    pub raw: u32,
}

impl ChCommandHeader {
    /// Value field (bits 0..16).
    pub fn value(&self) -> u32 {
        self.raw & 0xFFFF
    }

    /// Method offset (bits 16..28).
    pub fn method_offset(&self) -> u32 {
        (self.raw >> 16) & 0xFFF
    }

    /// Submission mode (bits 28..32).
    pub fn submission_mode(&self) -> Option<ChSubmissionMode> {
        ChSubmissionMode::from_u32((self.raw >> 28) & 0xF)
    }
}

/// A parsed channel command.
#[derive(Debug, Clone)]
pub struct ChCommand {
    pub class_id: ChClassId,
    pub method_offset: i32,
    pub arguments: Vec<u32>,
}

/// THI (Tegra Host Interface) register layout. Matches upstream's
/// `ThiRegisters` (0x20 u32 entries).
pub const THI_NUM_REGS: usize = 0x20;

/// THI register offsets as method indices.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ThiMethod {
    IncSyncpt = 0,   // offsetof(increment_syncpt) / 4
    SetMethod0 = 16, // offsetof(method_0) / 4
    SetMethod1 = 17, // offsetof(method_1) / 4
}

/// THI registers structure.
#[derive(Debug, Clone, Default)]
pub struct ThiRegisters {
    pub reg_array: [u32; THI_NUM_REGS],
}

impl ThiRegisters {
    pub fn increment_syncpt(&self) -> u32 {
        self.reg_array[0]
    }

    pub fn method_0(&self) -> u32 {
        self.reg_array[16]
    }

    pub fn method_1(&self) -> u32 {
        self.reg_array[17]
    }
}

/// Per-device subclass hook for `ChClassId::*` cases other than `Control`.
///
/// Upstream `CDmaPusher` is abstract with a virtual `ProcessMethod`; ruzu
/// passes a trait object instead so concrete devices (NvDec, Vic, NvJpg)
/// implement their own dispatch.
pub trait ProcessMethodHook: Send {
    fn process_method(&mut self, method: u32, arg: u32);
}

/// No-op processor for devices that haven't been ported yet.
///
/// Used by `Host1x::start_device` when wiring NvDec / Vic with placeholder
/// behavior — `IncSyncpt` and the `Control` host-processor path still run,
/// matching the parts of upstream that don't depend on subclass logic.
pub struct NullProcessor;

impl ProcessMethodHook for NullProcessor {
    fn process_method(&mut self, method: u32, arg: u32) {
        log::trace!(
            "CDmaPusher::NullProcessor::process_method method=0x{:X} arg=0x{:X}",
            method,
            arg
        );
    }
}

/// State carried across the CDMA command stream.
///
/// Mirrors the four locals (`count`, `method_offset`, `mask`, `incrementing`)
/// in upstream's `CDmaPusher::ProcessEntries` outer scope. They persist across
/// `push_entries` calls since the parser is stateful (a single command list
/// can leave the parser mid-Mask or mid-Incrementing run).
#[derive(Default)]
struct ParserState {
    count: u32,
    method_offset: u32,
    mask: u32,
    incrementing: bool,
}

#[derive(Default)]
struct CommandQueue {
    command_lists: VecDeque<Vec<ChCommandHeader>>,
    stop_requested: bool,
}

struct CDmaPusherInner {
    syncpoint_manager: Arc<SyncpointManager>,
    host_processor: Mutex<Control>,
    process_method: Mutex<Box<dyn ProcessMethodHook>>,
    current_class: Mutex<ChClassId>,
    thi_regs: Mutex<ThiRegisters>,
    state: Mutex<ParserState>,
    command_queue: Mutex<CommandQueue>,
    command_cv: Condvar,
}

impl CDmaPusherInner {
    fn process_entries(self: &Arc<Self>) {
        set_current_thread_priority(ThreadPriority::High);

        loop {
            let entries = {
                let mut queue = self.command_queue.lock().unwrap();
                while queue.command_lists.is_empty() && !queue.stop_requested {
                    queue = self.command_cv.wait(queue).unwrap();
                }
                if queue.stop_requested {
                    return;
                }
                queue.command_lists.pop_front()
            };

            if let Some(entries) = entries {
                let mut state = self.state.lock().unwrap();
                for value in entries {
                    self.step_one(&mut state, value);
                }
            }
        }
    }

    /// Process one command header. Mirrors the body of upstream's
    /// `for (const auto value : command_list)` loop.
    fn step_one(&self, state: &mut ParserState, value: ChCommandHeader) {
        if state.mask != 0 {
            let lbs = state.mask.trailing_zeros();
            state.mask &= !(1u32 << lbs);
            self.execute_command(state.method_offset + lbs, value.raw);
            return;
        }
        if state.count != 0 {
            state.count -= 1;
            self.execute_command(state.method_offset, value.raw);
            if state.incrementing {
                state.method_offset = state.method_offset.wrapping_add(1);
            }
            return;
        }
        let mode = match value.submission_mode() {
            Some(m) => m,
            None => {
                log::error!(
                    "CDmaPusher: bad submission mode in header 0x{:08X}",
                    value.raw
                );
                return;
            }
        };
        let header_value = value.value();
        let header_method_offset = value.method_offset();
        match mode {
            ChSubmissionMode::SetClass => {
                state.mask = header_value & 0x3F;
                state.method_offset = header_method_offset;
                let class = ChClassId::from_u32((header_value >> 6) & 0x3FF);
                *self.current_class.lock().unwrap() = class;
            }
            ChSubmissionMode::Incrementing | ChSubmissionMode::NonIncrementing => {
                state.count = header_value;
                state.method_offset = header_method_offset;
                state.incrementing = mode == ChSubmissionMode::Incrementing;
            }
            ChSubmissionMode::Mask => {
                state.mask = header_value;
                state.method_offset = header_method_offset;
            }
            ChSubmissionMode::Immediate => {
                let data = header_value & 0xFFF;
                state.method_offset = header_method_offset;
                self.execute_command(state.method_offset, data);
            }
            ChSubmissionMode::Restart | ChSubmissionMode::Gather => {
                log::error!("CDmaPusher: ChSubmissionMode {:?} is not implemented", mode);
            }
        }
    }

    /// Invoke command-class devices to execute the command based on the
    /// current state. Mirrors upstream `CDmaPusher::ExecuteCommand`.
    fn execute_command(&self, method: u32, arg: u32) {
        let class = *self.current_class.lock().unwrap();
        match class {
            ChClassId::Control => {
                log::trace!(
                    "CDmaPusher: Class {:?} method 0x{:X} arg 0x{:X}",
                    class,
                    method,
                    arg
                );
                if let Some(m) = ControlMethod::from_u32(method) {
                    self.host_processor.lock().unwrap().process_method(m, arg);
                }
            }
            _ => {
                {
                    let mut regs = self.thi_regs.lock().unwrap();
                    if (method as usize) < THI_NUM_REGS {
                        regs.reg_array[method as usize] = arg;
                    }
                }
                match method {
                    0 => {
                        // ThiMethod::IncSyncpt
                        let syncpoint_id = arg & 0xFF;
                        let _cond = (arg >> 8) & 0xFF;
                        log::trace!(
                            "CDmaPusher: Class {:?} IncSyncpt syncpt {} cond {}",
                            class,
                            syncpoint_id,
                            _cond
                        );
                        self.syncpoint_manager.increment_guest(syncpoint_id);
                        self.syncpoint_manager.increment_host(syncpoint_id);
                    }
                    17 => {
                        // ThiMethod::SetMethod1 — dispatch to subclass via method_0.
                        let method_0 = self.thi_regs.lock().unwrap().method_0();
                        log::trace!(
                            "CDmaPusher: Class {:?} method 0x{:X} arg 0x{:X}",
                            class,
                            method_0,
                            arg
                        );
                        self.process_method
                            .lock()
                            .unwrap()
                            .process_method(method_0, arg);
                    }
                    _ => {}
                }
            }
        }
    }
}

/// CDMA command pusher for a Host1x channel.
///
/// Upstream: `Tegra::CDmaPusher`. Concrete device pushers (NvDec, Vic) are
/// constructed via `new_with_processor` passing a `ProcessMethodHook`
/// implementation. Like upstream, `push_entries` enqueues work and a
/// per-pusher worker thread drains command lists.
pub struct CDmaPusher {
    inner: Arc<CDmaPusherInner>,
    worker: Mutex<Option<JoinHandle<()>>>,
}

impl CDmaPusher {
    /// Construct a CDmaPusher with a no-op subclass processor (used by
    /// host1x device shims that don't yet implement device-specific methods).
    ///
    /// Mirrors upstream `CDmaPusher::CDmaPusher(Host1x&, s32 id)` — the `Host1x&`
    /// reference is reduced here to its only used field, the syncpoint manager,
    /// to avoid an `Arc<Host1x>` ↔ `Arc<CDmaPusher>` reference cycle when Host1x
    /// stores its devices. (memory_manager / GMMU is unused until guest memory
    /// gather support lands.)
    pub fn new(syncpoint_manager: Arc<SyncpointManager>, id: i32) -> Self {
        Self::new_with_processor(syncpoint_manager, id, Box::new(NullProcessor))
    }

    /// Construct a CDmaPusher with a custom subclass processor.
    pub fn new_with_processor(
        syncpoint_manager: Arc<SyncpointManager>,
        id: i32,
        process_method: Box<dyn ProcessMethodHook>,
    ) -> Self {
        let host_processor = Control::new(syncpoint_manager.clone());
        let inner = Arc::new(CDmaPusherInner {
            syncpoint_manager,
            host_processor: Mutex::new(host_processor),
            process_method: Mutex::new(process_method),
            current_class: Mutex::new(ChClassId::from_u32(id as u32)),
            thi_regs: Mutex::new(ThiRegisters::default()),
            state: Mutex::new(ParserState::default()),
            command_queue: Mutex::new(CommandQueue::default()),
            command_cv: Condvar::new(),
        });
        let worker_inner = Arc::clone(&inner);
        let worker = std::thread::Builder::new()
            .name("Host1x CDmaPusher".to_string())
            .spawn(move || worker_inner.process_entries())
            .expect("failed to spawn Host1x CDmaPusher worker");
        Self {
            inner,
            worker: Mutex::new(Some(worker)),
        }
    }

    /// Push command entries for processing.
    ///
    /// Upstream: `CDmaPusher::PushEntries` enqueues; the worker thread later
    /// drains.
    pub fn push_entries(&self, entries: Vec<ChCommandHeader>) {
        let mut queue = self.inner.command_queue.lock().unwrap();
        queue.command_lists.push_back(entries);
        self.inner.command_cv.notify_one();
    }

    #[cfg(test)]
    fn execute_command(&self, method: u32, arg: u32) {
        self.inner.execute_command(method, arg);
    }
}

impl Drop for CDmaPusher {
    fn drop(&mut self) {
        {
            let mut queue = self.inner.command_queue.lock().unwrap();
            queue.stop_requested = true;
            queue.command_lists.clear();
        }
        self.inner.command_cv.notify_one();
        if let Some(worker) = self.worker.lock().unwrap().take() {
            let _ = worker.join();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inc_syncpt_advances_host1x_syncpoint() {
        let sm = Arc::new(SyncpointManager::new());
        let pusher = CDmaPusher::new(sm.clone(), ChClassId::NvDec as i32);

        let initial = sm.get_host_syncpoint_value(3);
        pusher.execute_command(0, 3); // IncSyncpt syncpt_id=3
        let after = sm.get_host_syncpoint_value(3);
        assert_eq!(after, initial + 1);
    }

    #[test]
    fn control_class_load_then_wait_does_not_block_when_satisfied() {
        let sm = Arc::new(SyncpointManager::new());
        let pusher = CDmaPusher::new(sm.clone(), ChClassId::Control as i32);

        // Pre-increment host syncpoint so WaitSyncpt32 returns immediately.
        sm.increment_host(0);
        pusher.execute_command(ControlMethod::LoadSyncptPayload32 as u32, 1);
        pusher.execute_command(ControlMethod::WaitSyncpt32 as u32, 0);
    }

    #[test]
    fn push_entries_is_drained_by_worker_thread() {
        let sm = Arc::new(SyncpointManager::new());
        let pusher = CDmaPusher::new(sm.clone(), ChClassId::NvDec as i32);

        pusher.push_entries(vec![ChCommandHeader {
            raw: ((ChSubmissionMode::Immediate as u32) << 28)
                | ((ThiMethod::IncSyncpt as u32) << 16)
                | 5,
        }]);

        for _ in 0..100 {
            if sm.get_host_syncpoint_value(5) == 1 {
                return;
            }
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
        panic!("CDmaPusher worker did not drain queued IncSyncpt command");
    }

    #[test]
    fn parser_state_persists_across_queued_command_lists() {
        let sm = Arc::new(SyncpointManager::new());
        let pusher = CDmaPusher::new(sm.clone(), ChClassId::NvDec as i32);

        pusher.push_entries(vec![ChCommandHeader {
            raw: ((ChSubmissionMode::NonIncrementing as u32) << 28)
                | ((ThiMethod::IncSyncpt as u32) << 16)
                | 2,
        }]);
        pusher.push_entries(vec![ChCommandHeader { raw: 6 }, ChCommandHeader { raw: 7 }]);

        for _ in 0..100 {
            if sm.get_host_syncpoint_value(6) == 1 && sm.get_host_syncpoint_value(7) == 1 {
                return;
            }
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
        panic!("CDmaPusher parser state did not persist across queued command lists");
    }
}
