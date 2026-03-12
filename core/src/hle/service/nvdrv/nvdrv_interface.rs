// SPDX-FileCopyrightText: 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: 2021 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/nvdrv_interface.h
//! Port of zuyu/src/core/hle/service/nvdrv/nvdrv_interface.cpp

use std::sync::Arc;

use super::core::container::SessionId;
use super::nvdata::*;
use super::nvdrv::Module;

/// IPC command table for NVDRV:
/// - 0: Open
/// - 1: Ioctl
/// - 2: Close
/// - 3: Initialize
/// - 4: QueryEvent
/// - 5: MapSharedMem (unimplemented)
/// - 6: GetStatus
/// - 7: SetAruidForTest (unimplemented)
/// - 8: SetAruid
/// - 9: DumpGraphicsMemoryInfo
/// - 10: InitializeDevtools (unimplemented)
/// - 11: Ioctl2
/// - 12: Ioctl3
/// - 13: SetGraphicsFirmwareMemoryMarginEnabled
pub struct NvdrvInterface {
    nvdrv: Arc<Module>,
    pid: u64,
    is_initialized: bool,
    session_id: SessionId,
    output_buffer: Vec<u8>,
    inline_output_buffer: Vec<u8>,
}

impl NvdrvInterface {
    pub fn new(nvdrv: Arc<Module>) -> Self {
        Self {
            nvdrv,
            pid: 0,
            is_initialized: false,
            session_id: SessionId { id: 0 },
            output_buffer: Vec::new(),
            inline_output_buffer: Vec::new(),
        }
    }

    pub fn get_module(&self) -> Arc<Module> {
        Arc::clone(&self.nvdrv)
    }

    /// Port of NVDRV::Open
    pub fn open(&mut self, device_name: &str) -> (DeviceFD, NvResult) {
        log::debug!("Open called");
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return (0, NvResult::NotInitialized);
        }

        if device_name == "/dev/nvhost-prof-gpu" {
            log::warn!("/dev/nvhost-prof-gpu cannot be opened in production");
            return (0, NvResult::NotSupported);
        }

        let fd = self.nvdrv.open(device_name, self.session_id);
        let result = if fd != INVALID_NVDRV_FD {
            NvResult::Success
        } else {
            NvResult::FileOperationFailed
        };
        (fd, result)
    }

    /// Port of NVDRV::Ioctl1
    pub fn ioctl1(&mut self, fd: DeviceFD, command: Ioctl, input: &[u8], output: &mut [u8]) -> NvResult {
        log::debug!("Ioctl1 called fd={}, ioctl=0x{:08X}", fd, command.raw);
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return NvResult::NotInitialized;
        }

        let nv_result = self.nvdrv.ioctl1(fd, command, input, output);
        nv_result
    }

    /// Port of NVDRV::Ioctl2
    pub fn ioctl2(
        &mut self,
        fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        inline_input: &[u8],
        output: &mut [u8],
    ) -> NvResult {
        log::debug!("Ioctl2 called fd={}, ioctl=0x{:08X}", fd, command.raw);
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return NvResult::NotInitialized;
        }

        let nv_result = self.nvdrv.ioctl2(fd, command, input, inline_input, output);
        nv_result
    }

    /// Port of NVDRV::Ioctl3
    pub fn ioctl3(
        &mut self,
        fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        output: &mut [u8],
        inline_output: &mut [u8],
    ) -> NvResult {
        log::debug!("Ioctl3 called fd={}, ioctl=0x{:08X}", fd, command.raw);
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return NvResult::NotInitialized;
        }

        let nv_result = self.nvdrv.ioctl3(fd, command, input, output, inline_output);
        nv_result
    }

    /// Port of NVDRV::Close
    pub fn close(&mut self, fd: DeviceFD) -> NvResult {
        log::debug!("Close called");
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return NvResult::NotInitialized;
        }

        self.nvdrv.close(fd)
    }

    /// Port of NVDRV::Initialize
    pub fn initialize(&mut self) -> NvResult {
        log::warn!("(STUBBED) Initialize called");
        if self.is_initialized {
            return NvResult::Success;
        }

        let container = self.nvdrv.get_container();
        self.session_id = container.open_session();
        self.is_initialized = true;
        NvResult::Success
    }

    /// Port of NVDRV::QueryEvent
    pub fn query_event(&self, fd: DeviceFD, event_id: u32) -> (NvResult, Option<u32>) {
        if !self.is_initialized {
            log::error!("NvServices is not initialized!");
            return (NvResult::NotInitialized, None);
        }

        self.nvdrv.query_event(fd, event_id)
    }

    /// Port of NVDRV::SetAruid
    pub fn set_aruid(&mut self, pid: u64) {
        self.pid = pid;
        log::warn!("(STUBBED) SetAruid called, pid=0x{:X}", pid);
    }

    /// Port of NVDRV::SetGraphicsFirmwareMemoryMarginEnabled
    pub fn set_graphics_firmware_memory_margin_enabled(&self) {
        log::warn!("(STUBBED) SetGraphicsFirmwareMemoryMarginEnabled called");
    }

    /// Port of NVDRV::GetStatus
    pub fn get_status(&self) -> NvResult {
        log::warn!("(STUBBED) GetStatus called");
        NvResult::Success
    }

    /// Port of NVDRV::DumpGraphicsMemoryInfo
    pub fn dump_graphics_memory_info(&self) {
        log::debug!("DumpGraphicsMemoryInfo called");
    }
}

impl Drop for NvdrvInterface {
    fn drop(&mut self) {
        if self.is_initialized {
            let container = self.nvdrv.get_container();
            container.close_session(self.session_id);
        }
    }
}
