// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_nvdec_common.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_nvdec_common.cpp

use std::collections::HashMap;
use std::sync::Mutex;

use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::core::syncpoint_manager::ChannelType;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::devices::nvmap::{read_struct, write_struct};
use crate::hle::service::nvdrv::nvdata::*;

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

/// Common base for NVDEC/VIC devices.
pub struct NvHostNvDecCommon {
    pub channel_syncpoint: u32,
    nvmap_fd: Mutex<i32>,
    submit_timeout: Mutex<u32>,
    channel_type: ChannelType,
    pub sessions: Mutex<HashMap<DeviceFD, SessionId>>,
}

impl NvHostNvDecCommon {
    pub fn new(channel_type: ChannelType) -> Self {
        Self {
            channel_syncpoint: 0, // Stubbed: would be allocated from syncpoint_manager
            nvmap_fd: Mutex::new(0),
            submit_timeout: Mutex::new(0),
            channel_type,
            sessions: Mutex::new(HashMap::new()),
        }
    }

    pub fn set_nvmap_fd(&self, params: &mut IoctlSetNvmapFD) -> NvResult {
        log::debug!("nvhost_nvdec_common::SetNVMAPfd called, fd={}", params.nvmap_fd);
        *self.nvmap_fd.lock().unwrap() = params.nvmap_fd;
        NvResult::Success
    }

    pub fn submit(&self, _params: &mut IoctlSubmit, _data: &mut [u8], _fd: DeviceFD) -> NvResult {
        log::debug!("nvhost_nvdec_common::Submit called (stubbed)");
        // Stubbed: Full implementation requires host1x command submission.
        NvResult::Success
    }

    pub fn get_syncpoint(&self, params: &mut IoctlGetSyncpoint) -> NvResult {
        log::debug!("nvhost_nvdec_common::GetSyncpoint called, id={}", params.param);
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
        _entries: &mut [MapBufferEntry],
        _fd: DeviceFD,
    ) -> NvResult {
        log::debug!("nvhost_nvdec_common::MapBuffer called (stubbed)");
        // Stubbed: would pin NvMap handles
        NvResult::Success
    }

    pub fn unmap_buffer(
        &self,
        _params: &mut IoctlMapBuffer,
        _entries: &mut [MapBufferEntry],
    ) -> NvResult {
        log::debug!("nvhost_nvdec_common::UnmapBuffer called (stubbed)");
        NvResult::Success
    }

    pub fn set_submit_timeout(&self, _timeout: u32) -> NvResult {
        log::warn!("nvhost_nvdec_common::SetSubmitTimeout (STUBBED) called");
        NvResult::Success
    }

    pub fn channel_type(&self) -> ChannelType {
        self.channel_type
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
                // Pass data portion for reading/writing sub-buffers
                let mut data = output.to_vec();
                let r = common.submit(&mut params, &mut data, fd);
                write_struct(output, &params);
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
                let r = common.map_buffer(&mut params, &mut [], fd);
                write_struct(output, &params);
                r
            }
            0xa => {
                // UnmapBuffer (fixed + variable)
                let mut params: IoctlMapBuffer = read_struct(input);
                let r = common.unmap_buffer(&mut params, &mut []);
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
