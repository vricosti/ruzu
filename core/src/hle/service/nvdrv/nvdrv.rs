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
use crate::core::SystemRef;
use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_scheduler::KScheduler;
use crate::hle::kernel::k_process::ProcessLock;

/// EventInterface manages kernel event creation and destruction for nvdrv.
///
/// Port of EventInterface from nvdrv.h/nvdrv.cpp.
/// In the C++ code, this manages KEvent creation/destruction via ServiceContext.
/// Since we don't have a full kernel event system, this is a lightweight placeholder
/// that tracks event allocations.
pub struct EventInterface {
    system: SystemRef,
}

impl EventInterface {
    pub fn new(system: SystemRef) -> Self {
        Self { system }
    }

    pub fn system(&self) -> SystemRef {
        self.system
    }

    /// Creates a persistent readable event owned by nvdrv.
    pub fn create_event(&self, name: &str) -> Arc<Mutex<KReadableEvent>> {
        let object_id = self.system.get().kernel().unwrap().create_new_object_id() as u64;
        let readable_event = Arc::new(Mutex::new(KReadableEvent::new()));
        readable_event.lock().unwrap().initialize(0, object_id);
        log::debug!(
            "EventInterface::create_event('{}') -> object_id={}",
            name,
            object_id
        );
        readable_event
    }

    /// Frees a previously created event.
    /// In the C++ code: module.service_context.CloseEvent(event)
    pub fn free_event(&self, _event: Arc<Mutex<KReadableEvent>>) {}
}

/// The main nvdrv module, managing device file descriptors and dispatching ioctls.
pub struct Module {
    system: crate::core::SystemRef,
    container: Container,
    next_fd: Mutex<DeviceFD>,
    open_files: Mutex<HashMap<DeviceFD, Arc<dyn NvDevice + Send + Sync>>>,
    gpu_files: Mutex<HashMap<DeviceFD, Arc<NvHostGpu>>>,
    disp_files: Mutex<HashMap<DeviceFD, Arc<NvDispDisp0>>>,
    nvmap_files: Mutex<HashMap<DeviceFD, Arc<NvMapDevice>>>,
    events_interface: Arc<EventInterface>,
}

impl Module {
    pub fn new(system: crate::core::SystemRef) -> Arc<Self> {
        Arc::new(Self {
            system,
            container: Container::new_with_system(system),
            next_fd: Mutex::new(1),
            open_files: Mutex::new(HashMap::new()),
            gpu_files: Mutex::new(HashMap::new()),
            disp_files: Mutex::new(HashMap::new()),
            nvmap_files: Mutex::new(HashMap::new()),
            events_interface: Arc::new(EventInterface::new(system)),
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
            "/dev/nvhost-as-gpu" => Arc::new(NvHostAsGpu::new(self.system, self, &self.container)),
            "/dev/nvhost-gpu" => {
                let gpu = Arc::new(NvHostGpu::new(
                    self.system,
                    Arc::clone(&self.events_interface),
                    &self.container,
                ));
                self.gpu_files.lock().unwrap().insert(fd, Arc::clone(&gpu));
                gpu
            }
            "/dev/nvhost-ctrl-gpu" => Arc::new(NvHostCtrlGpu::new(
                self.system,
                Arc::clone(&self.events_interface),
            )),
            "/dev/nvmap" => {
                let nvmap = Arc::new(NvMapDevice::new(
                    self.container.get_nv_map_file(),
                    &self.container,
                ));
                self.nvmap_files
                    .lock()
                    .unwrap()
                    .insert(fd, Arc::clone(&nvmap));
                nvmap
            }
            "/dev/nvdisp_disp0" => {
                let disp = Arc::new(NvDispDisp0::new(
                    self.system,
                    self.container.get_nv_map_file(),
                ));
                self.disp_files
                    .lock()
                    .unwrap()
                    .insert(fd, Arc::clone(&disp));
                disp
            }
            "/dev/nvhost-ctrl" => Arc::new(NvHostCtrl::new(
                Arc::clone(&self.events_interface),
                self.container.get_syncpoint_manager(),
            )),
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

        log::info!(
            "[NVDRV_OPEN] fd={} device={} session_id={}",
            fd, device_name, session_id.id
        );

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
        {
            use std::collections::HashMap;
            use std::sync::{Mutex, OnceLock};
            // Per (fd, cmd, group) counter. First occurrence always logs;
            // subsequent ones log at power-of-two increments per combo.
            static COMBO_COUNTS: OnceLock<Mutex<HashMap<(i32, u32, u8), u64>>> = OnceLock::new();
            let counts = COMBO_COUNTS.get_or_init(|| Mutex::new(HashMap::new()));
            let key = (fd, command.cmd(), command.group());
            let mut map = counts.lock().unwrap();
            let n = map.entry(key).or_insert(0);
            let current = *n;
            *n += 1;
            drop(map);
            if current == 0 || current.is_power_of_two() {
                log::info!(
                    "[NVDRV_IOCTL1] fd={} cmd=0x{:X} group=0x{:X} n={}",
                    fd, command.cmd(), command.group(), current
                );
            }
        }
        let files = self.open_files.lock().unwrap();
        let result = if let Some(device) = files.get(&fd) {
            let device = Arc::clone(device);
            drop(files);
            device.ioctl1(fd, command, input, output)
        } else {
            log::error!("Could not find DeviceFD={}!", fd);
            NvResult::NotImplemented
        };
        if std::env::var_os("RUZU_TRACE_IOCTL_FP").is_some() {
            // Show up to 64 bytes so we see full MapBufferEx (40B) / SubmitGPFIFO fence tails.
            let n = output.len().min(64);
            let mut hex = String::new();
            for (i, b) in output.iter().enumerate().take(n) {
                if i > 0 && (i & 3) == 0 {
                    hex.push(' ');
                }
                use std::fmt::Write;
                let _ = write!(hex, "{:02x}", b);
            }
            eprintln!(
                "[IOCTL_FP] fd={} ioctl=0x{:08X} out_sz=0x{:x} nv_result=0x{:x} head={}",
                fd,
                command.raw,
                output.len(),
                result as u32,
                hex
            );
        }
        result
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
            self.gpu_files.lock().unwrap().remove(&fd);
            self.disp_files.lock().unwrap().remove(&fd);
            self.nvmap_files.lock().unwrap().remove(&fd);
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
    pub fn query_event(
        &self,
        fd: DeviceFD,
        event_id: u32,
    ) -> (NvResult, Option<Arc<Mutex<KReadableEvent>>>) {
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

        let result = device.query_event(event_id);
        let object_id = result.as_ref().map(|e| e.lock().unwrap().object_id);
        log::info!(
            "[NVDRV_QUERY_EVENT] fd={} event_id={} object_id={:?}",
            fd, event_id, object_id
        );
        match result {
            Some(event) => (NvResult::Success, Some(event)),
            None => (NvResult::BadParameter, None),
        }
    }

    pub fn register_query_event_owner(
        &self,
        fd: DeviceFD,
        event_id: u32,
        process: Arc<ProcessLock>,
        scheduler: Arc<Mutex<KScheduler>>,
    ) {
        if fd < 0 {
            return;
        }

        let files = self.open_files.lock().unwrap();
        let Some(device) = files.get(&fd).cloned() else {
            return;
        };
        drop(files);

        device.register_query_event_owner(event_id, process, scheduler);
    }

    pub fn get_container(&self) -> &Container {
        &self.container
    }

    pub fn get_events_interface(&self) -> &Arc<EventInterface> {
        &self.events_interface
    }

    pub fn get_gpu_device(&self, fd: DeviceFD) -> Option<Arc<NvHostGpu>> {
        self.gpu_files.lock().unwrap().get(&fd).cloned()
    }

    pub fn get_disp_device(&self, fd: DeviceFD) -> Option<Arc<NvDispDisp0>> {
        self.disp_files.lock().unwrap().get(&fd).cloned()
    }

    pub fn get_nvmap_device(&self, fd: DeviceFD) -> Option<Arc<NvMapDevice>> {
        self.nvmap_files.lock().unwrap().get(&fd).cloned()
    }
}
