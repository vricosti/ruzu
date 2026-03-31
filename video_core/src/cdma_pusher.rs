// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/cdma_pusher.h and video_core/cdma_pusher.cpp
//!
//! CDMA (Channel DMA) command processing for Host1x channels (NvDec, Vic, etc.).

use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex};

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

/// THI (Tegra Host Interface) register layout.
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

/// The CDmaPusher processes Host1x channel commands in a dedicated thread.
///
/// Subclasses override `process_method` for device-specific handling (NvDec, Vic).
pub struct CDmaPusher {
    command_lists: Arc<Mutex<VecDeque<Vec<ChCommandHeader>>>>,
    command_cv: Arc<Condvar>,
    thi_regs: ThiRegisters,
    current_class: ChClassId,
    // In the full port:
    // host1x: &Host1x,
    // memory_manager: &MemoryManager,
    // host_processor: Box<dyn Control>,
    // thread: Option<JoinHandle<()>>,
}

impl CDmaPusher {
    /// Creates a new CDmaPusher for the given class ID.
    pub fn new(id: i32) -> Self {
        Self {
            command_lists: Arc::new(Mutex::new(VecDeque::new())),
            command_cv: Arc::new(Condvar::new()),
            thi_regs: ThiRegisters::default(),
            current_class: ChClassId::from_u32(id as u32),
        }
    }

    /// Push command entries for processing.
    pub fn push_entries(&self, entries: Vec<ChCommandHeader>) {
        let mut lists = self.command_lists.lock().unwrap();
        lists.push_back(entries);
        self.command_cv.notify_one();
    }

    /// Process the command entries (runs on the CDMA thread).
    ///
    /// In the full port, this runs in a dedicated thread with stop token support.
    pub fn process_entries(&mut self) {
        // NOTE: Full implementation spawns a dedicated thread that drains the command_lists
        // queue and calls execute_command for each entry. Requires Host1x integration.
        // Stubbed until Host1x and threading integration is complete.
        log::warn!("CdmaPusher::process_entries: Host1x integration not available, skipping");
    }

    /// Execute a single command based on current class state.
    fn execute_command(&mut self, method: u32, arg: u32) {
        match self.current_class {
            ChClassId::Control => {
                // NOTE: Full implementation calls host_processor.process_method(method, arg).
                // Stubbed until host_processor (Host1x HostProcessor) is integrated.
                log::warn!(
                    "CdmaPusher::execute_command: Control class not integrated, ignoring method 0x{:X} arg 0x{:X}",
                    method, arg
                );
            }
            _ => {
                self.thi_regs.reg_array[method as usize] = arg;
                match method {
                    0 => {
                        // ThiMethod::IncSyncpt
                        let syncpoint_id = arg & 0xFF;
                        let _cond = (arg >> 8) & 0xFF;
                        log::trace!(
                            "Class {:?} IncSyncpt syncpt {} cond {}",
                            self.current_class,
                            syncpoint_id,
                            _cond
                        );
                        // NOTE: Full implementation calls
                        //   syncpoint_manager.increment_guest(syncpoint_id)
                        //   syncpoint_manager.increment_host(syncpoint_id)
                        // Stubbed until Host1x syncpoint_manager is integrated.
                        log::warn!(
                            "CdmaPusher: IncSyncpt syncpt {} not forwarded — Host1x not integrated",
                            syncpoint_id
                        );
                    }
                    17 => {
                        // ThiMethod::SetMethod1
                        let method_0 = self.thi_regs.method_0();
                        log::trace!(
                            "Class {:?} method 0x{:X} arg 0x{:X}",
                            self.current_class,
                            method_0,
                            arg
                        );
                        self.process_method(method_0, arg);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Device-specific method processing. Override in subclasses.
    fn process_method(&mut self, _method: u32, _arg: u32) {
        // NOTE: This is a virtual dispatch point in C++ (pure virtual in the base class).
        // Device-specific implementations (VIC, NVDEC, etc.) override this.
        // The base implementation is unreachable in the full port.
        log::warn!(
            "CdmaPusher::process_method: no device-specific handler for method 0x{:X}, ignoring",
            _method
        );
    }
}
