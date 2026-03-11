// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_gpu.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_gpu.cpp

use std::collections::HashMap;
use std::sync::Mutex;

use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::devices::nvmap::{read_struct, write_struct};
use crate::hle::service::nvdrv::nvdata::*;

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CtxObjects {
    Ctx2D = 0x902D,
    Ctx3D = 0xB197,
    CtxCompute = 0xB1C0,
    CtxKepler = 0xA140,
    CtxDMA = 0xB0B5,
    CtxChannelGPFIFO = 0xB06F,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlSetNvmapFD {
    pub nvmap_fd: i32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlChannelSetTimeout {
    pub timeout: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlAllocGPFIFO {
    pub num_entries: u32,
    pub flags: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlClientData {
    pub data: u64,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlZCullBind {
    pub gpu_va: u64,
    pub mode: u32,
    pub _pad: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlZCullBind>() == 16);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlSetErrorNotifier {
    pub offset: u64,
    pub size: u64,
    pub mem: u32,
    pub _pad: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlSetErrorNotifier>() == 24);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlChannelSetPriority {
    pub priority: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlSetTimeslice {
    pub timeslice: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlAllocGpfifoEx2 {
    pub num_entries: u32,
    pub flags: u32,
    pub unk0: u32,
    pub fence_out: NvFence,
    pub unk1: u32,
    pub unk2: u32,
    pub unk3: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlAllocGpfifoEx2>() == 32);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlAllocObjCtx {
    pub class_num: u32,
    pub flags: u32,
    pub obj_id: u64,
}
const _: () = assert!(std::mem::size_of::<IoctlAllocObjCtx>() == 16);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlSubmitGpfifo {
    pub address: u64,
    pub num_entries: u32,
    pub flags: u32,
    pub fence: NvFence,
}
const _: () = assert!(std::mem::size_of::<IoctlSubmitGpfifo>() == 24);

impl IoctlSubmitGpfifo {
    pub fn fence_wait(&self) -> bool {
        self.flags & 1 != 0
    }
    pub fn fence_increment(&self) -> bool {
        (self.flags >> 1) & 1 != 0
    }
    pub fn suppress_wfi(&self) -> bool {
        (self.flags >> 4) & 1 != 0
    }
    pub fn increment_value(&self) -> bool {
        (self.flags >> 8) & 1 != 0
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlGetWaitbase {
    pub unknown: u32,
    pub value: u32,
}

/// nvhost_gpu device.
pub struct NvHostGpu {
    nvmap_fd: Mutex<i32>,
    user_data: Mutex<u64>,
    zcull_params: Mutex<IoctlZCullBind>,
    channel_priority: Mutex<u32>,
    channel_timeslice: Mutex<u32>,
    sessions: Mutex<HashMap<DeviceFD, SessionId>>,
}

impl NvHostGpu {
    pub fn new() -> Self {
        Self {
            nvmap_fd: Mutex::new(0),
            user_data: Mutex::new(0),
            zcull_params: Mutex::new(IoctlZCullBind::default()),
            channel_priority: Mutex::new(0),
            channel_timeslice: Mutex::new(0),
            sessions: Mutex::new(HashMap::new()),
        }
    }

    pub fn set_nvmap_fd(&self, params: &mut IoctlSetNvmapFD) -> NvResult {
        log::debug!("nvhost_gpu::SetNVMAPfd called, fd={}", params.nvmap_fd);
        *self.nvmap_fd.lock().unwrap() = params.nvmap_fd;
        NvResult::Success
    }

    pub fn set_client_data(&self, params: &mut IoctlClientData) -> NvResult {
        log::debug!("nvhost_gpu::SetClientData called");
        *self.user_data.lock().unwrap() = params.data;
        NvResult::Success
    }

    pub fn get_client_data(&self, params: &mut IoctlClientData) -> NvResult {
        log::debug!("nvhost_gpu::GetClientData called");
        params.data = *self.user_data.lock().unwrap();
        NvResult::Success
    }

    pub fn zcull_bind(&self, params: &mut IoctlZCullBind) -> NvResult {
        *self.zcull_params.lock().unwrap() = *params;
        log::debug!("nvhost_gpu::ZCullBind called, gpu_va={:X}, mode={:X}", params.gpu_va, params.mode);
        NvResult::Success
    }

    pub fn set_error_notifier(&self, params: &mut IoctlSetErrorNotifier) -> NvResult {
        log::warn!(
            "nvhost_gpu::SetErrorNotifier (STUBBED) called, offset={:X}, size={:X}, mem={:X}",
            params.offset,
            params.size,
            params.mem
        );
        NvResult::Success
    }

    pub fn set_channel_priority(&self, params: &mut IoctlChannelSetPriority) -> NvResult {
        *self.channel_priority.lock().unwrap() = params.priority;
        log::debug!("nvhost_gpu::SetChannelPriority (STUBBED) called, priority={:X}", params.priority);
        NvResult::Success
    }

    pub fn alloc_gpfifo_ex2(&self, params: &mut IoctlAllocGpfifoEx2, _fd: DeviceFD) -> NvResult {
        log::warn!(
            "nvhost_gpu::AllocGPFIFOEx2 (STUBBED) called, num_entries={:X}, flags={:X}",
            params.num_entries,
            params.flags
        );
        // Stubbed: Full implementation requires GPU channel state and syncpoint manager.
        params.fence_out = NvFence::default();
        NvResult::Success
    }

    pub fn allocate_object_context(&self, params: &mut IoctlAllocObjCtx) -> NvResult {
        log::warn!(
            "nvhost_gpu::AllocateObjectContext (STUBBED) called, class_num={:X}, flags={:X}",
            params.class_num,
            params.flags
        );
        params.obj_id = 0x0;
        NvResult::Success
    }

    pub fn submit_gpfifo_base1(
        &self,
        params: &mut IoctlSubmitGpfifo,
        _commands: &[u8],
    ) -> NvResult {
        log::trace!(
            "nvhost_gpu::SubmitGPFIFOBase1 called, gpfifo={:X}, num_entries={:X}, flags={:X}",
            params.address,
            params.num_entries,
            params.flags
        );
        // Stubbed: Full implementation requires GPU push buffer submission.
        params.flags = 0;
        NvResult::Success
    }

    pub fn get_waitbase(&self, params: &mut IoctlGetWaitbase) -> NvResult {
        log::info!("nvhost_gpu::GetWaitbase called, unknown=0x{:X}", params.unknown);
        params.value = 0;
        NvResult::Success
    }

    pub fn channel_set_timeout(&self, params: &mut IoctlChannelSetTimeout) -> NvResult {
        log::info!("nvhost_gpu::ChannelSetTimeout called, timeout=0x{:X}", params.timeout);
        NvResult::Success
    }

    pub fn channel_set_timeslice(&self, params: &mut IoctlSetTimeslice) -> NvResult {
        log::info!("nvhost_gpu::ChannelSetTimeslice called, timeslice=0x{:X}", params.timeslice);
        *self.channel_timeslice.lock().unwrap() = params.timeslice;
        NvResult::Success
    }
}

impl NvDevice for NvHostGpu {
    fn ioctl1(
        &self,
        fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        output: &mut [u8],
    ) -> NvResult {
        match command.group() {
            0x0 => match command.cmd() {
                0x3 => {
                    let mut params: IoctlGetWaitbase = read_struct(input);
                    let r = self.get_waitbase(&mut params);
                    write_struct(output, &params);
                    r
                }
                _ => {
                    log::error!("Unimplemented ioctl={:08X}", command.raw);
                    NvResult::NotImplemented
                }
            },
            b'H' => match command.cmd() {
                0x1 => {
                    let mut params: IoctlSetNvmapFD = read_struct(input);
                    let r = self.set_nvmap_fd(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x3 => {
                    let mut params: IoctlChannelSetTimeout = read_struct(input);
                    let r = self.channel_set_timeout(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x8 | 0x1b => {
                    // SubmitGPFIFOBase1 (0x8 without kickoff, 0x1b with kickoff)
                    let fixed_size = std::mem::size_of::<IoctlSubmitGpfifo>();
                    let mut params: IoctlSubmitGpfifo = read_struct(input);
                    let var_data = if input.len() > fixed_size {
                        &input[fixed_size..]
                    } else {
                        &[]
                    };
                    let r = self.submit_gpfifo_base1(&mut params, var_data);
                    write_struct(output, &params);
                    r
                }
                0x9 => {
                    let mut params: IoctlAllocObjCtx = read_struct(input);
                    let r = self.allocate_object_context(&mut params);
                    write_struct(output, &params);
                    r
                }
                0xb => {
                    let mut params: IoctlZCullBind = read_struct(input);
                    let r = self.zcull_bind(&mut params);
                    write_struct(output, &params);
                    r
                }
                0xc => {
                    let mut params: IoctlSetErrorNotifier = read_struct(input);
                    let r = self.set_error_notifier(&mut params);
                    write_struct(output, &params);
                    r
                }
                0xd => {
                    let mut params: IoctlChannelSetPriority = read_struct(input);
                    let r = self.set_channel_priority(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x1a => {
                    let mut params: IoctlAllocGpfifoEx2 = read_struct(input);
                    let r = self.alloc_gpfifo_ex2(&mut params, fd);
                    write_struct(output, &params);
                    r
                }
                0x1d => {
                    let mut params: IoctlSetTimeslice = read_struct(input);
                    let r = self.channel_set_timeslice(&mut params);
                    write_struct(output, &params);
                    r
                }
                _ => {
                    log::error!("Unimplemented ioctl={:08X}", command.raw);
                    NvResult::NotImplemented
                }
            },
            b'G' => match command.cmd() {
                0x14 => {
                    let mut params: IoctlClientData = read_struct(input);
                    let r = self.set_client_data(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x15 => {
                    let mut params: IoctlClientData = read_struct(input);
                    let r = self.get_client_data(&mut params);
                    write_struct(output, &params);
                    r
                }
                _ => {
                    log::error!("Unimplemented ioctl={:08X}", command.raw);
                    NvResult::NotImplemented
                }
            },
            _ => {
                log::error!("Unimplemented ioctl={:08X}", command.raw);
                NvResult::NotImplemented
            }
        }
    }

    fn ioctl2(
        &self,
        _fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        inline_input: &[u8],
        output: &mut [u8],
    ) -> NvResult {
        match command.group() {
            b'H' => match command.cmd() {
                0x1b => {
                    // SubmitGPFIFOBase2
                    let mut params: IoctlSubmitGpfifo = read_struct(input);
                    let r = self.submit_gpfifo_base1(&mut params, inline_input);
                    write_struct(output, &params);
                    r
                }
                _ => {
                    log::error!("Unimplemented ioctl={:08X}", command.raw);
                    NvResult::NotImplemented
                }
            },
            _ => {
                log::error!("Unimplemented ioctl={:08X}", command.raw);
                NvResult::NotImplemented
            }
        }
    }

    fn ioctl3(
        &self,
        _fd: DeviceFD,
        command: Ioctl,
        _input: &[u8],
        _output: &mut [u8],
        _inline_output: &mut [u8],
    ) -> NvResult {
        log::error!("Unimplemented ioctl={:08X}", command.raw);
        NvResult::NotImplemented
    }

    fn on_open(&self, session_id: SessionId, fd: DeviceFD) {
        let mut sessions = self.sessions.lock().unwrap();
        sessions.insert(fd, session_id);
    }

    fn on_close(&self, fd: DeviceFD) {
        let mut sessions = self.sessions.lock().unwrap();
        sessions.remove(&fd);
    }

    fn query_event(&self, event_id: u32) -> Option<u32> {
        match event_id {
            1 | 2 | 3 => Some(event_id),
            _ => {
                log::error!("Unknown Ctrl GPU Event {}", event_id);
                None
            }
        }
    }
}
