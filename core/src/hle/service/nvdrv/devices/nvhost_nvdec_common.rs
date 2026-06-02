// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_nvdec_common.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_nvdec_common.cpp

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;

use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::core::syncpoint_manager::ChannelType;
use crate::hle::service::nvdrv::core::{
    container::Container, nvmap::NvMap, syncpoint_manager::SyncpointManager,
};
use crate::hle::service::nvdrv::devices::nvmap::{read_struct, write_struct};
use crate::hle::service::nvdrv::nvdata::*;
use crate::{core::SystemRef, host1x_core::Host1xChannelType};

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlSetNvmapFD {
    pub nvmap_fd: i32,
}
const _: () = assert!(std::mem::size_of::<IoctlSetNvmapFD>() == 4);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlSubmitCommandBuffer {
    pub id: u32,
    pub offset: u32,
    pub count: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlSubmitCommandBuffer>() == 0xC);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlSubmit {
    pub cmd_buffer_count: u32,
    pub relocation_count: u32,
    pub syncpoint_count: u32,
    pub fence_count: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlSubmit>() == 0x10);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct CommandBuffer {
    pub memory_id: i32,
    pub offset: u32,
    pub word_count: i32,
}
const _: () = assert!(std::mem::size_of::<CommandBuffer>() == 0xC);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Reloc {
    pub cmdbuffer_memory: i32,
    pub cmdbuffer_offset: i32,
    pub target: i32,
    pub target_offset: i32,
}
const _: () = assert!(std::mem::size_of::<Reloc>() == 0x10);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SyncptIncr {
    pub id: u32,
    pub increments: u32,
    pub unk0: u32,
    pub unk1: u32,
    pub unk2: u32,
}
const _: () = assert!(std::mem::size_of::<SyncptIncr>() == 0x14);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlGetSyncpoint {
    pub param: u32,
    pub value: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlGetWaitbase {
    pub unknown: u32,
    pub value: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlGetWaitbase>() == 0x8);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlMapBuffer {
    pub num_entries: u32,
    pub data_address: u32,
    pub attach_host_ch_das: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlMapBuffer>() == 0x0C);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct MapBufferEntry {
    pub map_handle: u32,
    pub map_address: u32,
}
const _: () = assert!(std::mem::size_of::<MapBufferEntry>() == 0x8);

/// Common base for NVDEC/VIC devices.
pub struct NvHostNvDecCommon {
    system: SystemRef,
    pub channel_syncpoint: u32,
    nvmap: Arc<NvMap>,
    syncpoint_manager: Arc<SyncpointManager>,
    nvmap_fd: Mutex<i32>,
    submit_timeout: Mutex<u32>,
    channel_type: ChannelType,
    pub sessions: Mutex<HashMap<DeviceFD, SessionId>>,
}

impl NvHostNvDecCommon {
    pub fn new(system: SystemRef, container: &Container, channel_type: ChannelType) -> Self {
        let channel_syncpoint = container.get_syncpoint_manager().allocate_syncpoint(false);
        Self {
            system,
            channel_syncpoint,
            nvmap: container.get_nv_map_file_handle(),
            syncpoint_manager: container.get_syncpoint_manager_handle(),
            nvmap_fd: Mutex::new(0),
            submit_timeout: Mutex::new(0),
            channel_type,
            sessions: Mutex::new(HashMap::new()),
        }
    }

    pub fn set_nvmap_fd(&self, params: &mut IoctlSetNvmapFD) -> NvResult {
        log::debug!(
            "nvhost_nvdec_common::SetNVMAPfd called, fd={}",
            params.nvmap_fd
        );
        *self.nvmap_fd.lock().unwrap() = params.nvmap_fd;
        NvResult::Success
    }

    pub fn submit(&self, params: &mut IoctlSubmit, data: &mut [u8], fd: DeviceFD) -> NvResult {
        log::debug!(
            "nvhost_nvdec_common::Submit fd={} cmd_buffers={} relocs={} syncpts={} fences={}",
            fd,
            params.cmd_buffer_count,
            params.relocation_count,
            params.syncpoint_count,
            params.fence_count
        );

        let mut offset = 0usize;
        let mut command_buffers =
            read_vec::<CommandBuffer>(data, params.cmd_buffer_count as usize, &mut offset);
        let relocs = read_vec::<Reloc>(data, params.relocation_count as usize, &mut offset);
        let reloc_shifts = read_vec::<u32>(data, params.relocation_count as usize, &mut offset);
        let syncpt_increments =
            read_vec::<SyncptIncr>(data, params.syncpoint_count as usize, &mut offset);
        let mut fence_thresholds = read_vec::<u32>(data, params.fence_count as usize, &mut offset);
        let trace_stage = match self.channel_type {
            ChannelType::NvDec => 1,
            ChannelType::VIC => 2,
            _ => 0,
        };

        for (index, syncpt_incr) in syncpt_increments.iter().enumerate() {
            if let Some(threshold) = fence_thresholds.get_mut(index) {
                *threshold = self
                    .syncpoint_manager
                    .increment_syncpoint_max_ext(syncpt_incr.id, syncpt_incr.increments);
            }
        }

        let Some(host1x) = self.system.get().host1x_core() else {
            log::error!("nvhost_nvdec_common::Submit called without Host1x core");
            return NvResult::InvalidState;
        };
        let Some(memory) = self.system.get().get_svc_memory() else {
            log::error!("nvhost_nvdec_common::Submit called without application memory");
            return NvResult::InvalidState;
        };

        for cmd_buffer in &mut command_buffers {
            if cmd_buffer.word_count <= 0 {
                continue;
            }
            let Some(object) = self.nvmap.get_handle(cmd_buffer.memory_id as u32) else {
                log::error!(
                    "nvhost_nvdec_common::Submit unknown command buffer nvmap handle=0x{:X}",
                    cmd_buffer.memory_id
                );
                return NvResult::InvalidState;
            };
            let address = {
                let object = object.lock_inner();
                object.address.wrapping_add(cmd_buffer.offset as u64)
            };
            let word_count = cmd_buffer.word_count as usize;
            if trace_stage != 0 {
                let _ = common::trace::emit(
                    common::trace::cat::HOST1X_VIDEO,
                    &[
                        trace_stage,
                        fd as u64,
                        params.cmd_buffer_count as u64,
                        params.relocation_count as u64,
                        params.syncpoint_count as u64,
                        params.fence_count as u64,
                        address,
                        word_count as u64,
                    ],
                );
            }
            let mut bytes = vec![0u8; word_count.saturating_mul(std::mem::size_of::<u32>())];
            memory.lock().unwrap().read_block(address, &mut bytes);
            let cmdlist = bytes
                .chunks_exact(std::mem::size_of::<u32>())
                .map(|word| u32::from_le_bytes([word[0], word[1], word[2], word[3]]))
                .collect::<Vec<_>>();
            host1x.push_entries(fd, cmdlist);
        }

        offset = 0;
        write_vec(data, &command_buffers, &mut offset);
        write_vec(data, &relocs, &mut offset);
        write_vec(data, &reloc_shifts, &mut offset);
        write_vec(data, &syncpt_increments, &mut offset);
        write_vec(data, &fence_thresholds, &mut offset);

        NvResult::Success
    }

    pub fn get_syncpoint(&self, params: &mut IoctlGetSyncpoint) -> NvResult {
        log::debug!(
            "nvhost_nvdec_common::GetSyncpoint called, id={}",
            params.param
        );
        params.value = self.channel_syncpoint;
        NvResult::Success
    }

    pub fn get_waitbase(&self, params: &mut IoctlGetWaitbase) -> NvResult {
        log::error!("nvhost_nvdec_common::GetWaitbase called");
        params.value = 0;
        NvResult::Success
    }

    pub fn map_buffer(
        &self,
        _params: &mut IoctlMapBuffer,
        entries: &mut [MapBufferEntry],
        _fd: DeviceFD,
    ) -> NvResult {
        for entry in entries {
            entry.map_address = self.nvmap.pin_handle(entry.map_handle, true) as u32;
        }
        NvResult::Success
    }

    pub fn unmap_buffer(
        &self,
        params: &mut IoctlMapBuffer,
        entries: &mut [MapBufferEntry],
    ) -> NvResult {
        for entry in entries {
            self.nvmap.unpin_handle(entry.map_handle);
            *entry = MapBufferEntry::default();
        }
        *params = IoctlMapBuffer::default();
        NvResult::Success
    }

    pub fn set_submit_timeout(&self, _timeout: u32) -> NvResult {
        log::warn!("nvhost_nvdec_common::SetSubmitTimeout (STUBBED) called");
        NvResult::Success
    }

    pub fn channel_type(&self) -> ChannelType {
        self.channel_type
    }

    pub fn host1x_channel_type(&self) -> Host1xChannelType {
        match self.channel_type {
            ChannelType::MsEnc => Host1xChannelType::MsEnc,
            ChannelType::VIC => Host1xChannelType::Vic,
            ChannelType::GPU => Host1xChannelType::Gpu,
            ChannelType::NvDec => Host1xChannelType::NvDec,
            ChannelType::Display => Host1xChannelType::Display,
            ChannelType::NvJpg => Host1xChannelType::NvJpg,
            ChannelType::TSec => Host1xChannelType::TSec,
            ChannelType::Max => Host1xChannelType::Max,
        }
    }

    pub fn start_host1x_device(&self, fd: DeviceFD) {
        if let Some(host1x) = self.system.get().host1x_core() {
            host1x.start_device(fd, self.host1x_channel_type(), self.channel_syncpoint);
        } else {
            log::error!("nvhost_nvdec_common::OnOpen missing Host1x core");
        }
    }

    pub fn stop_host1x_device(&self, fd: DeviceFD) {
        if let Some(host1x) = self.system.get().host1x_core() {
            host1x.stop_device(fd, self.host1x_channel_type());
        }
    }
}

fn read_vec<T: Copy + Default>(input: &[u8], count: usize, offset: &mut usize) -> Vec<T> {
    let mut out = vec![T::default(); count];
    let bytes = count.saturating_mul(std::mem::size_of::<T>());
    if count != 0 && input.len() >= offset.saturating_add(bytes) {
        unsafe {
            std::ptr::copy_nonoverlapping(
                input.as_ptr().add(*offset),
                out.as_mut_ptr() as *mut u8,
                bytes,
            );
        }
        *offset += bytes;
    }
    out
}

fn write_vec<T: Copy>(output: &mut [u8], input: &[T], offset: &mut usize) {
    let bytes = input.len().saturating_mul(std::mem::size_of::<T>());
    if !input.is_empty() && output.len() >= offset.saturating_add(bytes) {
        unsafe {
            std::ptr::copy_nonoverlapping(
                input.as_ptr() as *const u8,
                output.as_mut_ptr().add(*offset),
                bytes,
            );
        }
        *offset += bytes;
    }
}

fn read_variable_entries<T: Copy + Default>(input: &[u8]) -> Vec<T> {
    let entry_size = std::mem::size_of::<T>();
    if entry_size == 0 {
        return Vec::new();
    }
    let count = input.len() / entry_size;
    let mut offset = 0;
    read_vec(input, count, &mut offset)
}

fn write_variable_entries<T: Copy>(output: &mut [u8], fixed_size: usize, entries: &[T]) {
    if output.len() <= fixed_size || entries.is_empty() {
        return;
    }
    let bytes = entries.len().saturating_mul(std::mem::size_of::<T>());
    let copy_len = bytes.min(output.len() - fixed_size);
    unsafe {
        std::ptr::copy_nonoverlapping(
            entries.as_ptr() as *const u8,
            output.as_mut_ptr().add(fixed_size),
            copy_len,
        );
    }
}

/// Ioctl dispatch for nvdec-common-type devices.
pub fn ioctl1_common(
    common: &NvHostNvDecCommon,
    fd: DeviceFD,
    command: Ioctl,
    input: &[u8],
    output: &mut [u8],
) -> NvResult {
    match command.group() {
        0x0 => match command.cmd() {
            0x1 => {
                // Submit (fixed + variable)
                let mut params: IoctlSubmit = read_struct(input);
                let fixed_size = std::mem::size_of::<IoctlSubmit>().min(input.len());
                let mut data = input.get(fixed_size..).unwrap_or_default().to_vec();
                let r = common.submit(&mut params, &mut data, fd);
                write_struct(output, &params);
                if output.len() > fixed_size {
                    let copy_len = data.len().min(output.len() - fixed_size);
                    output[fixed_size..fixed_size + copy_len].copy_from_slice(&data[..copy_len]);
                }
                r
            }
            0x2 => {
                let mut params: IoctlGetSyncpoint = read_struct(input);
                let r = common.get_syncpoint(&mut params);
                write_struct(output, &params);
                r
            }
            0x3 => {
                let mut params: IoctlGetWaitbase = read_struct(input);
                let r = common.get_waitbase(&mut params);
                write_struct(output, &params);
                r
            }
            0x7 => {
                let timeout: u32 = read_struct(input);
                let r = common.set_submit_timeout(timeout);
                write_struct(output, &timeout);
                r
            }
            0x9 => {
                // MapBuffer (fixed + variable)
                let mut params: IoctlMapBuffer = read_struct(input);
                let fixed_size = std::mem::size_of::<IoctlMapBuffer>().min(input.len());
                let mut entries = read_variable_entries::<MapBufferEntry>(
                    input.get(fixed_size..).unwrap_or_default(),
                );
                let r = common.map_buffer(&mut params, &mut entries, fd);
                write_struct(output, &params);
                write_variable_entries(output, fixed_size, &entries);
                r
            }
            0xa => {
                // UnmapBuffer (fixed + variable)
                let mut params: IoctlMapBuffer = read_struct(input);
                let fixed_size = std::mem::size_of::<IoctlMapBuffer>().min(input.len());
                let mut entries = read_variable_entries::<MapBufferEntry>(
                    input.get(fixed_size..).unwrap_or_default(),
                );
                let r = common.unmap_buffer(&mut params, &mut entries);
                write_struct(output, &params);
                write_variable_entries(output, fixed_size, &entries);
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
                let r = common.set_nvmap_fd(&mut params);
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
