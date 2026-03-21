// SPDX-FileCopyrightText: 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: 2021 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/nvdrv.h
//! Port of zuyu/src/core/hle/service/nvdrv/nvdrv.cpp

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use super::core::container::{Container, SessionId};
use super::devices::nvdevice::NvDevice;
use super::devices::nvdisp_disp0::NvDispDisp0;
use super::devices::nvhost_as_gpu::NvHostAsGpu;
use super::devices::nvhost_ctrl::NvHostCtrl;
use super::devices::nvhost_ctrl_gpu::NvHostCtrlGpu;
use super::devices::nvhost_gpu::NvHostGpu;
use super::devices::nvhost_nvdec::NvHostNvDec;
use super::devices::nvhost_nvjpg::NvHostNvJpg;
use super::devices::nvhost_vic::NvHostVic;
use super::devices::nvmap::NvMapDevice;
use super::nvdata::*;

/// EventInterface manages kernel event creation and destruction for nvdrv.
///
/// Port of EventInterface from nvdrv.h/nvdrv.cpp.
/// In the C++ code, this manages KEvent creation/destruction via ServiceContext.
/// Since we don't have a full kernel event system, this is a lightweight placeholder
/// that tracks event allocations.
pub struct EventInterface {
    guard: Mutex<()>,
    next_event_id: Mutex<u32>,
}

impl EventInterface {
    pub fn new() -> Self {
        Self {
            guard: Mutex::new(()),
            next_event_id: Mutex::new(0),
        }
    }

    /// Creates a new event with the given name.
    /// In the C++ code: module.service_context.CreateEvent(name)
    /// Returns an opaque event identifier.
    pub fn create_event(&self, name: &str) -> u32 {
        let _lock = self.guard.lock().unwrap();
        let mut next_id = self.next_event_id.lock().unwrap();
        let id = *next_id;
        *next_id += 1;
        log::debug!("EventInterface::create_event('{}') -> {}", name, id);
        id
    }

    /// Frees a previously created event.
    /// In the C++ code: module.service_context.CloseEvent(event)
    pub fn free_event(&self, _event_id: u32) {
        let _lock = self.guard.lock().unwrap();
        // In the C++ code, this calls service_context.CloseEvent(event)
        // Since we don't have kernel events yet, this is a no-op.
    }
}

/// The main nvdrv module, managing device file descriptors and dispatching ioctls.
pub struct Module {
    system: crate::core::SystemRef,
    container: Container,
    next_fd: Mutex<DeviceFD>,
    open_files: Mutex<HashMap<DeviceFD, Arc<dyn NvDevice + Send + Sync>>>,
    events_interface: EventInterface,
}

impl Module {
    pub fn new(system: crate::core::SystemRef) -> Arc<Self> {
        Arc::new(Self {
            system,
            container: Container::new(),
            next_fd: Mutex::new(1),
            open_files: Mutex::new(HashMap::new()),
            events_interface: EventInterface::new(),
        })
    }

    pub fn verify_fd(&self, fd: DeviceFD) -> NvResult {
        if fd < 0 {
            log::error!("Invalid DeviceFD={}!", fd);
            return NvResult::InvalidState;
        }
        let files = self.open_files.lock().unwrap();
        if !files.contains_key(&fd) {
            log::error!("Could not find DeviceFD={}!", fd);
            return NvResult::NotImplemented;
        }
        NvResult::Success
    }

    pub fn open(&self, device_name: &str, session_id: SessionId) -> DeviceFD {
        let mut next_fd = self.next_fd.lock().unwrap();
        let fd = *next_fd;
        *next_fd += 1;
        drop(next_fd);

        let device: Arc<dyn NvDevice + Send + Sync> = match device_name {
            "/dev/nvhost-as-gpu" => Arc::new(NvHostAsGpu::new()),
            "/dev/nvhost-gpu" => Arc::new(NvHostGpu::new()),
            "/dev/nvhost-ctrl-gpu" => Arc::new(NvHostCtrlGpu::new()),
            "/dev/nvmap" => Arc::new(NvMapDevice::new(self.container.get_nv_map_file())),
            "/dev/nvdisp_disp0" => Arc::new(NvDispDisp0::new()),
            "/dev/nvhost-ctrl" => {
                Arc::new(NvHostCtrl::new(self.container.get_syncpoint_manager()))
            }
            "/dev/nvhost-nvdec" => Arc::new(NvHostNvDec::new()),
            "/dev/nvhost-nvjpg" => Arc::new(NvHostNvJpg::new()),
            "/dev/nvhost-vic" => Arc::new(NvHostVic::new()),
            _ => {
                log::error!("Trying to open unknown device {}", device_name);
                return INVALID_NVDRV_FD;
            }
        };

        device.on_open(session_id, fd);

        let mut files = self.open_files.lock().unwrap();
        files.insert(fd, device);

        fd
    }

    pub fn ioctl1(
        &self,
        fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        output: &mut [u8],
    ) -> NvResult {
        if fd < 0 {
            log::error!("Invalid DeviceFD={}!", fd);
            return NvResult::InvalidState;
        }
        let files = self.open_files.lock().unwrap();
        if let Some(device) = files.get(&fd) {
            let device = Arc::clone(device);
            drop(files);
            device.ioctl1(fd, command, input, output)
        } else {
            log::error!("Could not find DeviceFD={}!", fd);
            NvResult::NotImplemented
        }
    }

    pub fn ioctl2(
        &self,
        fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        inline_input: &[u8],
        output: &mut [u8],
    ) -> NvResult {
        if fd < 0 {
            log::error!("Invalid DeviceFD={}!", fd);
            return NvResult::InvalidState;
        }
        let files = self.open_files.lock().unwrap();
        if let Some(device) = files.get(&fd) {
            let device = Arc::clone(device);
            drop(files);
            device.ioctl2(fd, command, input, inline_input, output)
        } else {
            log::error!("Could not find DeviceFD={}!", fd);
            NvResult::NotImplemented
        }
    }

    pub fn ioctl3(
        &self,
        fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        output: &mut [u8],
        inline_output: &mut [u8],
    ) -> NvResult {
        if fd < 0 {
            log::error!("Invalid DeviceFD={}!", fd);
            return NvResult::InvalidState;
        }
        let files = self.open_files.lock().unwrap();
        if let Some(device) = files.get(&fd) {
            let device = Arc::clone(device);
            drop(files);
            device.ioctl3(fd, command, input, output, inline_output)
        } else {
            log::error!("Could not find DeviceFD={}!", fd);
            NvResult::NotImplemented
        }
    }

    pub fn close(&self, fd: DeviceFD) -> NvResult {
        if fd < 0 {
            log::error!("Invalid DeviceFD={}!", fd);
            return NvResult::InvalidState;
        }
        let mut files = self.open_files.lock().unwrap();
        if let Some(device) = files.remove(&fd) {
            device.on_close(fd);
            NvResult::Success
        } else {
            log::error!("Could not find DeviceFD={}!", fd);
            NvResult::NotImplemented
        }
    }

    /// Queries an event for a given device fd and event_id.
    ///
    /// Port of Module::QueryEvent from nvdrv.cpp.
    /// Returns (NvResult, Option<event_token>) where event_token is an opaque identifier.
    pub fn query_event(&self, fd: DeviceFD, event_id: u32) -> (NvResult, Option<u32>) {
        if fd < 0 {
            log::error!("Invalid DeviceFD={}!", fd);
            return (NvResult::InvalidState, None);
        }

        let files = self.open_files.lock().unwrap();
        let device = match files.get(&fd) {
            Some(d) => Arc::clone(d),
            None => {
                log::error!("Could not find DeviceFD={}!", fd);
                return (NvResult::NotImplemented, None);
            }
        };
        drop(files);

        match device.query_event(event_id) {
            Some(event) => (NvResult::Success, Some(event)),
            None => (NvResult::BadParameter, None),
        }
    }

    pub fn get_container(&self) -> &Container {
        &self.container
    }

    pub fn get_events_interface(&self) -> &EventInterface {
        &self.events_interface
    }
}
