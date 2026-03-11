// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvdevice.h

use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::nvdata::{DeviceFD, Ioctl, NvResult};

/// Represents an abstract nvidia device node. It is to be subclassed by concrete device nodes to
/// implement the ioctl interface.
pub trait NvDevice {
    /// Handles an ioctl1 request.
    fn ioctl1(
        &self,
        fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        output: &mut [u8],
    ) -> NvResult;

    /// Handles an ioctl2 request.
    fn ioctl2(
        &self,
        fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        inline_input: &[u8],
        output: &mut [u8],
    ) -> NvResult;

    /// Handles an ioctl3 request.
    fn ioctl3(
        &self,
        fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        output: &mut [u8],
        inline_output: &mut [u8],
    ) -> NvResult;

    /// Called once a device is opened.
    fn on_open(&self, session_id: SessionId, fd: DeviceFD);

    /// Called once a device is closed.
    fn on_close(&self, fd: DeviceFD);

    /// Queries an event by id. Returns None if no event exists for the given id.
    fn query_event(&self, _event_id: u32) -> Option<u32> {
        None
    }
}
