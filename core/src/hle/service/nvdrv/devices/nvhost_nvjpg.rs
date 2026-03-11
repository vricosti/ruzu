// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_nvjpg.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_nvjpg.cpp

use std::sync::Mutex;

use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::devices::nvmap::{read_struct, write_struct};
use crate::hle::service::nvdrv::nvdata::{DeviceFD, Ioctl, NvResult};

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlSetNvmapFD {
    pub nvmap_fd: i32,
}
const _: () = assert!(std::mem::size_of::<IoctlSetNvmapFD>() == 4);

/// nvhost_nvjpg device.
pub struct NvHostNvJpg {
    nvmap_fd: Mutex<i32>,
}

impl NvHostNvJpg {
    pub fn new() -> Self {
        Self {
            nvmap_fd: Mutex::new(0),
        }
    }

    pub fn set_nvmap_fd(&self, params: &mut IoctlSetNvmapFD) -> NvResult {
        log::debug!("nvhost_nvjpg::SetNVMAPfd called, fd={}", params.nvmap_fd);
        *self.nvmap_fd.lock().unwrap() = params.nvmap_fd;
        NvResult::Success
    }
}

impl NvDevice for NvHostNvJpg {
    fn ioctl1(
        &self,
        _fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        output: &mut [u8],
    ) -> NvResult {
        match command.group() {
            b'H' => match command.cmd() {
                0x1 => {
                    let mut params: IoctlSetNvmapFD = read_struct(input);
                    let r = self.set_nvmap_fd(&mut params);
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
        _input: &[u8],
        _inline_input: &[u8],
        _output: &mut [u8],
    ) -> NvResult {
        log::error!("Unimplemented ioctl={:08X}", command.raw);
        NvResult::NotImplemented
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

    fn on_open(&self, _session_id: SessionId, _fd: DeviceFD) {}
    fn on_close(&self, _fd: DeviceFD) {}
}
