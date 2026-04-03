// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_gpu.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_gpu.cpp

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::{Arc, Mutex};

use crate::core::SystemRef;
use crate::gpu_core::{
    GpuChannelHandle, GpuCommandHeader, GpuCommandList, GpuCommandListHeader,
    GpuMemoryManagerHandle,
};
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::service::nvdrv::core::container::Container;
use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::core::syncpoint_manager::SyncpointManager;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::devices::nvmap::{read_struct, write_struct};
use crate::hle::service::nvdrv::nvdata::*;
use crate::hle::service::nvdrv::nvdrv::EventInterface;

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

const METHOD_SYNCPOINT_PAYLOAD: u32 = 0x1C;
const METHOD_SYNCPOINT_OPERATION: u32 = 0x1D;
const METHOD_WAIT_FOR_IDLE: u32 = 0x1E;
const SUBMISSION_MODE_INCREASING: u32 = 1;

fn build_command_header(method: u32, arg_count: u32, mode: u32) -> GpuCommandHeader {
    GpuCommandHeader {
        raw: (method & 0x1FFF) | ((arg_count & 0x1FFF) << 16) | ((mode & 0x7) << 29),
    }
}

fn build_fence_action(op: u32, syncpoint_id: u32) -> GpuCommandHeader {
    GpuCommandHeader {
        raw: (op & 1) | ((syncpoint_id & 0x00FF_FFFF) << 8),
    }
}

fn build_wait_command_list(fence: NvFence) -> GpuCommandList {
    GpuCommandList {
        command_lists: Vec::new(),
        prefetch_command_list: vec![
            build_command_header(METHOD_SYNCPOINT_PAYLOAD, 1, SUBMISSION_MODE_INCREASING),
            GpuCommandHeader { raw: fence.value },
            build_command_header(METHOD_SYNCPOINT_OPERATION, 1, SUBMISSION_MODE_INCREASING),
            build_fence_action(0, fence.id as u32),
        ],
    }
}

fn build_increment_command_list(fence: NvFence) -> GpuCommandList {
    GpuCommandList {
        command_lists: Vec::new(),
        prefetch_command_list: vec![
            build_command_header(METHOD_SYNCPOINT_PAYLOAD, 1, SUBMISSION_MODE_INCREASING),
            GpuCommandHeader { raw: 0 },
            build_command_header(METHOD_SYNCPOINT_OPERATION, 1, SUBMISSION_MODE_INCREASING),
            build_fence_action(1, fence.id as u32),
            build_command_header(METHOD_SYNCPOINT_OPERATION, 1, SUBMISSION_MODE_INCREASING),
            build_fence_action(1, fence.id as u32),
        ],
    }
}

fn build_increment_with_wfi_command_list(fence: NvFence) -> GpuCommandList {
    let mut result = GpuCommandList {
        command_lists: Vec::new(),
        prefetch_command_list: vec![
            build_command_header(METHOD_WAIT_FOR_IDLE, 1, SUBMISSION_MODE_INCREASING),
            GpuCommandHeader { raw: 0 },
        ],
    };
    result
        .prefetch_command_list
        .extend(build_increment_command_list(fence).prefetch_command_list);
    result
}

/// nvhost_gpu device.
pub struct NvHostGpu {
    system: SystemRef,
    sm_exception_breakpoint_int_report_event: Arc<Mutex<KReadableEvent>>,
    sm_exception_breakpoint_pause_report_event: Arc<Mutex<KReadableEvent>>,
    error_notifier_event: Arc<Mutex<KReadableEvent>>,
    syncpoint_manager: *const SyncpointManager,
    container: *const Container,
    channel_syncpoint: AtomicU32,
    channel_initialized: AtomicBool,
    nvmap_fd: Mutex<i32>,
    user_data: Mutex<u64>,
    zcull_params: Mutex<IoctlZCullBind>,
    channel_priority: Mutex<u32>,
    channel_timeslice: Mutex<u32>,
    channel_state: Arc<dyn GpuChannelHandle>,
    bound_address_space: AtomicBool,
    sessions: Mutex<HashMap<DeviceFD, SessionId>>,
}

unsafe impl Send for NvHostGpu {}
unsafe impl Sync for NvHostGpu {}

impl NvHostGpu {
    pub fn new(
        system: SystemRef,
        events_interface: Arc<EventInterface>,
        container: &Container,
    ) -> Self {
        let channel_state = system
            .get()
            .gpu_core()
            .expect("GPU core must be initialized before nvhost_gpu open")
            .allocate_channel_handle();
        let channel_syncpoint = container.get_syncpoint_manager().allocate_syncpoint(false);
        Self {
            system,
            sm_exception_breakpoint_int_report_event: events_interface
                .create_event("GpuChannelSMExceptionBreakpointInt"),
            sm_exception_breakpoint_pause_report_event: events_interface
                .create_event("GpuChannelSMExceptionBreakpointPause"),
            error_notifier_event: events_interface.create_event("GpuChannelErrorNotifier"),
            syncpoint_manager: container.get_syncpoint_manager() as *const _,
            container: container as *const _,
            channel_syncpoint: AtomicU32::new(channel_syncpoint),
            channel_initialized: AtomicBool::new(false),
            nvmap_fd: Mutex::new(0),
            user_data: Mutex::new(0),
            zcull_params: Mutex::new(IoctlZCullBind::default()),
            channel_priority: Mutex::new(0),
            channel_timeslice: Mutex::new(0),
            channel_state,
            bound_address_space: AtomicBool::new(false),
            sessions: Mutex::new(HashMap::new()),
        }
    }

    fn syncpoint_manager(&self) -> &SyncpointManager {
        unsafe { &*self.syncpoint_manager }
    }

    fn container(&self) -> &Container {
        unsafe { &*self.container }
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
        log::debug!(
            "nvhost_gpu::ZCullBind called, gpu_va={:X}, mode={:X}",
            params.gpu_va,
            params.mode
        );
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
        log::debug!(
            "nvhost_gpu::SetChannelPriority (STUBBED) called, priority={:X}",
            params.priority
        );
        NvResult::Success
    }

    pub fn alloc_gpfifo_ex2(&self, params: &mut IoctlAllocGpfifoEx2, fd: DeviceFD) -> NvResult {
        log::warn!(
            "nvhost_gpu::AllocGPFIFOEx2 (STUBBED) called, num_entries={:X}, flags={:X}",
            params.num_entries,
            params.flags
        );

        if self.channel_initialized.swap(true, Ordering::AcqRel) {
            log::error!("nvhost_gpu::AllocGPFIFOEx2 called on already initialized channel");
            return NvResult::AlreadyAllocated;
        }

        let program_id = self
            .sessions
            .lock()
            .unwrap()
            .get(&fd)
            .copied()
            .and_then(|session_id| self.container().get_session_process(session_id))
            .map(|process| process.lock().unwrap().get_program_id())
            .unwrap_or(0);
        self.channel_state.init_channel(program_id);
        let channel_syncpoint = self.channel_syncpoint.load(Ordering::Acquire);
        params.fence_out = self
            .syncpoint_manager()
            .get_syncpoint_fence(channel_syncpoint);
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

    fn submit_gpfifo_impl(
        &self,
        params: &mut IoctlSubmitGpfifo,
        entries: GpuCommandList,
    ) -> NvResult {
        log::trace!(
            "nvhost_gpu::SubmitGPFIFOImpl called, gpfifo={:X}, num_entries={:X}, flags={:X}",
            params.address,
            params.num_entries,
            params.flags
        );

        let channel_syncpoint = self.channel_syncpoint.load(Ordering::Acquire);
        let bind_id = self.channel_state.bind_id();
        let gpu = self
            .system
            .get()
            .gpu_core()
            .expect("GPU core must remain available while nvhost_gpu is alive");
        if params.fence_wait() {
            if params.increment_value() {
                return NvResult::BadParameter;
            }

            if !self.syncpoint_manager().is_fence_signalled(&params.fence) {
                gpu.push_gpu_entries(bind_id, build_wait_command_list(params.fence));
            }
        }

        params.fence.id = channel_syncpoint as i32;
        let increment = (if params.fence_increment() { 2 } else { 0 })
            + if params.increment_value() {
                params.fence.value
            } else {
                0
            };
        params.fence.value = self
            .syncpoint_manager()
            .increment_syncpoint_max_ext(channel_syncpoint, increment);
        gpu.push_gpu_entries(bind_id, entries);

        if params.fence_increment() {
            if params.suppress_wfi() {
                gpu.push_gpu_entries(bind_id, build_increment_command_list(params.fence));
            } else {
                gpu.push_gpu_entries(bind_id, build_increment_with_wfi_command_list(params.fence));
            }
        }

        params.flags = 0;
        NvResult::Success
    }

    pub fn submit_gpfifo_base1(
        &self,
        params: &mut IoctlSubmitGpfifo,
        commands: &[u8],
        kickoff: bool,
    ) -> NvResult {
        let command_count = params.num_entries as usize;
        let entry_size = std::mem::size_of::<GpuCommandListHeader>();
        log::trace!(
            "nvhost_gpu::SubmitGPFIFOBase1 kickoff={} num_entries={} address=0x{:X} input_len=0x{:X}",
            kickoff,
            params.num_entries,
            params.address,
            commands.len()
        );
        let command_lists = if kickoff {
            let Some(process) = self.system.get().current_process() else {
                log::error!("nvhost_gpu::SubmitGPFIFOBase1 kickoff path without current process");
                return NvResult::InvalidState;
            };
            let bytes = process.read_memory_vec(params.address, command_count * entry_size);
            bytes
                .chunks_exact(entry_size)
                .take(command_count)
                .map(|chunk| GpuCommandListHeader {
                    raw: u64::from_le_bytes(chunk.try_into().unwrap()),
                })
                .collect::<Vec<_>>()
        } else {
            let available_entries = commands.len() / entry_size;
            if command_count > available_entries {
                log::error!(
                    "nvhost_gpu::SubmitGPFIFOBase1 invalid size num_entries={} available_entries={}",
                    params.num_entries,
                    available_entries
                );
                return NvResult::InvalidSize;
            }
            commands
                .chunks_exact(entry_size)
                .take(command_count)
                .map(|chunk| GpuCommandListHeader {
                    raw: u64::from_le_bytes(chunk.try_into().unwrap()),
                })
                .collect::<Vec<_>>()
        };

        self.submit_gpfifo_impl(
            params,
            GpuCommandList {
                command_lists,
                prefetch_command_list: Vec::new(),
            },
        )
    }

    pub fn submit_gpfifo_base2(&self, params: &mut IoctlSubmitGpfifo, commands: &[u8]) -> NvResult {
        let command_count = params.num_entries as usize;
        let entry_size = std::mem::size_of::<GpuCommandListHeader>();
        let available_entries = commands.len() / entry_size;
        log::trace!(
            "nvhost_gpu::SubmitGPFIFOBase2 num_entries={} address=0x{:X} inline_len=0x{:X}",
            params.num_entries,
            params.address,
            commands.len()
        );
        if command_count > available_entries {
            log::error!(
                "nvhost_gpu::SubmitGPFIFOBase2 invalid size num_entries={} available_entries={}",
                params.num_entries,
                available_entries
            );
            return NvResult::InvalidSize;
        }

        let command_lists = commands
            .chunks_exact(entry_size)
            .take(command_count)
            .map(|chunk| GpuCommandListHeader {
                raw: u64::from_le_bytes(chunk.try_into().unwrap()),
            })
            .collect();
        self.submit_gpfifo_impl(
            params,
            GpuCommandList {
                command_lists,
                prefetch_command_list: Vec::new(),
            },
        )
    }

    pub fn get_waitbase(&self, params: &mut IoctlGetWaitbase) -> NvResult {
        log::trace!(
            "nvhost_gpu::GetWaitbase called, unknown=0x{:X}",
            params.unknown
        );
        params.value = 0;
        NvResult::Success
    }

    pub fn channel_set_timeout(&self, params: &mut IoctlChannelSetTimeout) -> NvResult {
        log::trace!(
            "nvhost_gpu::ChannelSetTimeout called, timeout=0x{:X}",
            params.timeout
        );
        NvResult::Success
    }

    pub fn channel_set_timeslice(&self, params: &mut IoctlSetTimeslice) -> NvResult {
        log::trace!(
            "nvhost_gpu::ChannelSetTimeslice called, timeslice=0x{:X}",
            params.timeslice
        );
        *self.channel_timeslice.lock().unwrap() = params.timeslice;
        NvResult::Success
    }

    pub fn bind_address_space(&self, memory_manager: Arc<dyn GpuMemoryManagerHandle>) {
        self.channel_state.bind_memory_manager(memory_manager);
        self.bound_address_space.store(true, Ordering::Release);
    }

    #[cfg(test)]
    pub(crate) fn has_bound_address_space(&self) -> bool {
        self.bound_address_space.load(Ordering::Acquire)
    }
}

impl Drop for NvHostGpu {
    fn drop(&mut self) {
        let channel_syncpoint = self.channel_syncpoint.load(Ordering::Acquire);
        if channel_syncpoint != 0 {
            self.syncpoint_manager().free_syncpoint(channel_syncpoint);
        }
    }
}

impl NvDevice for NvHostGpu {
    fn ioctl1(&self, fd: DeviceFD, command: Ioctl, input: &[u8], output: &mut [u8]) -> NvResult {
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
                0x8 => {
                    let fixed_size = std::mem::size_of::<IoctlSubmitGpfifo>();
                    let mut params: IoctlSubmitGpfifo = read_struct(input);
                    let var_data = if input.len() > fixed_size {
                        &input[fixed_size..]
                    } else {
                        &[]
                    };
                    let r = self.submit_gpfifo_base1(&mut params, var_data, false);
                    write_struct(output, &params);
                    r
                }
                0x1b => {
                    let fixed_size = std::mem::size_of::<IoctlSubmitGpfifo>();
                    let mut params: IoctlSubmitGpfifo = read_struct(input);
                    let var_data = if input.len() > fixed_size {
                        &input[fixed_size..]
                    } else {
                        &[]
                    };
                    let r = self.submit_gpfifo_base1(&mut params, var_data, true);
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
                    let mut params: IoctlSubmitGpfifo = read_struct(input);
                    let r = self.submit_gpfifo_base2(&mut params, inline_input);
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

    fn query_event(&self, event_id: u32) -> Option<Arc<Mutex<KReadableEvent>>> {
        match event_id {
            1 => Some(Arc::clone(&self.sm_exception_breakpoint_int_report_event)),
            2 => Some(Arc::clone(&self.sm_exception_breakpoint_pause_report_event)),
            3 => Some(Arc::clone(&self.error_notifier_event)),
            _ => {
                log::error!("Unknown Ctrl GPU Event {}", event_id);
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use super::{IoctlAllocGpfifoEx2, IoctlSubmitGpfifo, NvHostGpu};
    use crate::gpu_core::{
        GpuChannelHandle, GpuCommandList, GpuCoreInterface, GpuMemoryManagerHandle,
    };
    use crate::hle::service::nvdrv::core::container::Container;
    use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
    use crate::hle::service::nvdrv::nvdata::{NvFence, NvResult};
    use crate::hle::service::nvdrv::nvdrv::EventInterface;

    #[derive(Default)]
    struct FakeGpuCore {
        pushed: Mutex<Vec<(i32, GpuCommandList)>>,
    }

    struct FakeGpuMemoryManagerHandle;
    struct FakeGpuChannelHandle {
        bind_id: i32,
    }

    impl GpuChannelHandle for FakeGpuChannelHandle {
        fn bind_memory_manager(&self, _memory_manager: Arc<dyn GpuMemoryManagerHandle>) {}

        fn init_channel(&self, _program_id: u64) {}

        fn bind_id(&self) -> i32 {
            self.bind_id
        }
    }

    impl GpuMemoryManagerHandle for FakeGpuMemoryManagerHandle {
        fn as_any(&self) -> &(dyn std::any::Any + Send + Sync) {
            self
        }

        fn map(
            &self,
            _gpu_addr: u64,
            _device_addr: u64,
            _size: u64,
            _kind: u32,
            _is_big_pages: bool,
        ) {
        }

        fn map_sparse(&self, _gpu_addr: u64, _size: u64, _is_big_pages: bool) {}

        fn unmap(&self, _gpu_addr: u64, _size: u64) {}
    }

    impl GpuCoreInterface for FakeGpuCore {
        fn as_any(&self) -> &(dyn std::any::Any + Send) {
            self
        }

        fn allocate_channel_handle(&self) -> Arc<dyn GpuChannelHandle> {
            Arc::new(FakeGpuChannelHandle { bind_id: 1 })
        }

        fn allocate_memory_manager_handle(
            &self,
            _address_space_bits: u64,
            _split_address: u64,
            _big_page_bits: u64,
            _page_bits: u64,
        ) -> Arc<dyn GpuMemoryManagerHandle> {
            Arc::new(FakeGpuMemoryManagerHandle)
        }

        fn init_address_space(&self, _memory_manager: Arc<dyn GpuMemoryManagerHandle>) {}

        fn push_gpu_entries(&self, channel_id: i32, entries: GpuCommandList) {
            self.pushed.lock().unwrap().push((channel_id, entries));
        }

        fn request_composite(
            &self,
            _layers: Vec<crate::gpu_core::FramebufferConfig>,
            _fences: Vec<crate::hle::service::nvdrv::nvdata::NvFence>,
        ) {
        }

        fn on_cpu_write(&self, _addr: u64, _size: u64) -> bool {
            false
        }
    }

    #[test]
    fn alloc_gpfifo_ex2_allocates_syncpoint_fence() {
        let mut system = crate::core::System::new_for_test();
        system.set_gpu_core(Box::new(FakeGpuCore::default()));
        let container = Container::new();
        let events = Arc::new(EventInterface::new(crate::core::SystemRef::from_ref(
            &system,
        )));
        let gpu = NvHostGpu::new(
            crate::core::SystemRef::from_ref(&system),
            events,
            &container,
        );
        let mut params = IoctlAllocGpfifoEx2::default();

        let result = gpu.alloc_gpfifo_ex2(&mut params, 1);

        assert_eq!(result, NvResult::Success);
        assert_ne!(params.fence_out.id, 0);
    }

    #[test]
    fn submit_gpfifo_base1_returns_signalled_fence_and_clears_flags() {
        let mut system = crate::core::System::new_for_test();
        system.set_gpu_core(Box::new(FakeGpuCore::default()));
        let container = Container::new();
        let events = Arc::new(EventInterface::new(crate::core::SystemRef::from_ref(
            &system,
        )));
        let gpu = NvHostGpu::new(
            crate::core::SystemRef::from_ref(&system),
            events,
            &container,
        );
        let mut alloc = IoctlAllocGpfifoEx2::default();
        assert_eq!(gpu.alloc_gpfifo_ex2(&mut alloc, 1), NvResult::Success);

        let mut params = IoctlSubmitGpfifo {
            num_entries: 1,
            flags: (1 << 1) | (1 << 8),
            fence: NvFence { id: 0, value: 3 },
            ..Default::default()
        };
        let commands = [0u8; 8];

        let result = gpu.submit_gpfifo_base1(&mut params, &commands, false);

        assert_eq!(result, NvResult::Success);
        assert_eq!(params.flags, 0);
        assert_eq!(params.fence.id, alloc.fence_out.id);
        assert_eq!(params.fence.value, 5);
        let gpu = system
            .gpu_core()
            .unwrap()
            .as_any()
            .downcast_ref::<FakeGpuCore>()
            .unwrap();
        let pushed = gpu.pushed.lock().unwrap();
        assert_eq!(pushed.len(), 2);
        assert_eq!(pushed[0].0, 1);
        assert_eq!(pushed[0].1.command_lists.len(), 1);
        assert_eq!(pushed[1].1.prefetch_command_list.len(), 6);
    }

    #[test]
    fn query_event_returns_three_known_events() {
        let mut system = crate::core::System::new_for_test();
        system.set_gpu_core(Box::new(FakeGpuCore::default()));
        let container = Container::new();
        let events = Arc::new(EventInterface::new(crate::core::SystemRef::from_ref(
            &system,
        )));
        let gpu = NvHostGpu::new(
            crate::core::SystemRef::from_ref(&system),
            events,
            &container,
        );

        assert!(gpu.query_event(1).is_some());
        assert!(gpu.query_event(2).is_some());
        assert!(gpu.query_event(3).is_some());
        assert!(gpu.query_event(4).is_none());
    }
}
