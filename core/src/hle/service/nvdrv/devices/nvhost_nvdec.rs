// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_nvdec.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_nvdec.cpp

use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::core::container::Container;
use crate::hle::service::nvdrv::core::syncpoint_manager::ChannelType;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::devices::nvhost_nvdec_common::{self, NvHostNvDecCommon};
use crate::hle::service::nvdrv::nvdata::{DeviceFD, Ioctl, NvResult};
use crate::core::SystemRef;

/// nvhost_nvdec device.
pub struct NvHostNvDec {
    common: NvHostNvDecCommon,
}

impl NvHostNvDec {
    pub fn new(system: SystemRef, container: &Container) -> Self {
        Self {
            common: NvHostNvDecCommon::new(system, container, ChannelType::NvDec),
        }
    }
}

impl NvDevice for NvHostNvDec {
    fn ioctl1(&self, fd: DeviceFD, command: Ioctl, input: &[u8], output: &mut [u8]) -> NvResult {
        nvhost_nvdec_common::ioctl1_common(&self.common, fd, command, input, output)
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

    fn on_open(&self, session_id: SessionId, fd: DeviceFD) {
        log::info!("NVDEC video stream started");
        self.common.system().get().set_nvdec_active(true);
        let mut sessions = self.common.sessions.lock().unwrap();
        sessions.insert(fd, session_id);
        drop(sessions);
        self.common.start_host1x_device(fd);
    }

    fn on_close(&self, fd: DeviceFD) {
        log::info!("NVDEC video stream ended");
        self.common.stop_host1x_device(fd);
        self.common.system().get().set_nvdec_active(false);
        let mut sessions = self.common.sessions.lock().unwrap();
        sessions.remove(&fd);
    }
}
