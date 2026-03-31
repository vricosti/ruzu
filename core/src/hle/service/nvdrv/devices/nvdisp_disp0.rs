// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvdisp_disp0.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvdisp_disp0.cpp

use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::nvdata::{DeviceFD, Ioctl, NvResult};

/// nvdisp_disp0 device: display compositor.
pub struct NvDispDisp0 {
    // Stubbed: container and nvmap references would be stored here.
}

impl NvDispDisp0 {
    pub fn new() -> Self {
        Self {}
    }

    /// Performs a screen flip, compositing each buffer.
    /// Stubbed: Full implementation requires GPU RequestComposite and NvMap handle addresses.
    pub fn composite(&self, _sorted_layers: &[()]) {
        // Stubbed: In the C++ code, this builds FramebufferConfig from layers and calls
        // system.GPU().RequestComposite().
        log::debug!("nvdisp_disp0::Composite called (stubbed)");
    }
}

impl NvDevice for NvDispDisp0 {
    fn ioctl1(&self, _fd: DeviceFD, command: Ioctl, _input: &[u8], _output: &mut [u8]) -> NvResult {
        log::error!("Unimplemented ioctl={:08X}", command.raw);
        NvResult::NotImplemented
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

    fn query_event(
        &self,
        event_id: u32,
    ) -> Option<
        std::sync::Arc<std::sync::Mutex<crate::hle::kernel::k_readable_event::KReadableEvent>>,
    > {
        log::error!("Unknown DISP Event {}", event_id);
        None
    }
}
