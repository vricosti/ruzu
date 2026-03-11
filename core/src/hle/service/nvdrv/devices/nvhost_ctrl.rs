// SPDX-FileCopyrightText: 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: 2021 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_ctrl.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_ctrl.cpp

use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Mutex;

use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::core::syncpoint_manager::SyncpointManager;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::devices::nvmap::{read_struct, write_struct};
use crate::hle::service::nvdrv::nvdata::*;

/// Union for SyncpointEventValue bit fields.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SyncpointEventValue {
    pub raw: u32,
}

impl SyncpointEventValue {
    pub fn partial_slot(&self) -> u32 {
        self.raw & 0xF
    }

    pub fn syncpoint_id(&self) -> u32 {
        (self.raw >> 4) & 0x0FFFFFFF
    }

    pub fn slot(&self) -> u16 {
        self.raw as u16
    }

    pub fn syncpoint_id_for_allocation(&self) -> u16 {
        ((self.raw >> 16) & 0x0FFF) as u16
    }

    pub fn event_allocated(&self) -> bool {
        ((self.raw >> 28) & 1) != 0
    }
}

struct InternalEvent {
    status: AtomicU32,
    fails: u32,
    assigned_syncpt: u32,
    assigned_value: u32,
    registered: bool,
}

impl Default for InternalEvent {
    fn default() -> Self {
        Self {
            status: AtomicU32::new(EventState::Available as u32),
            fails: 0,
            assigned_syncpt: 0,
            assigned_value: 0,
            registered: false,
        }
    }
}

impl InternalEvent {
    fn is_being_used(&self) -> bool {
        let current_status = self.status.load(Ordering::Acquire);
        current_status == EventState::Waiting as u32
            || current_status == EventState::Cancelling as u32
            || current_status == EventState::Signalling as u32
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocSyncptReadParams {
    pub id: u32,
    pub value: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocSyncptIncrParams {
    pub id: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocSyncptWaitParams {
    pub id: u32,
    pub thresh: u32,
    pub timeout: i32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocCtrlEventClearParams {
    pub event_id: SyncpointEventValue,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocCtrlEventWaitParams {
    pub fence: NvFence,
    pub timeout: u32,
    pub value: SyncpointEventValue,
}
const _: () = assert!(std::mem::size_of::<IocCtrlEventWaitParams>() == 16);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocCtrlEventRegisterParams {
    pub user_event_id: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocCtrlEventUnregisterParams {
    pub user_event_id: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IocCtrlEventUnregisterBatchParams {
    pub user_events: u64,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct IocGetConfigParams {
    pub domain_str: [u8; 0x41],
    pub param_str: [u8; 0x41],
    pub config_str: [u8; 0x101],
}
const _: () = assert!(std::mem::size_of::<IocGetConfigParams>() == 387);

impl Default for IocGetConfigParams {
    fn default() -> Self {
        Self {
            domain_str: [0u8; 0x41],
            param_str: [0u8; 0x41],
            config_str: [0u8; 0x101],
        }
    }
}

/// nvhost_ctrl device.
pub struct NvHostCtrl {
    syncpoint_manager: *const SyncpointManager,
    events: Mutex<Vec<InternalEvent>>,
    events_mask: Mutex<u64>,
}

// Safety: NvHostCtrl is only accessed from service thread.
unsafe impl Send for NvHostCtrl {}
unsafe impl Sync for NvHostCtrl {}

impl NvHostCtrl {
    pub fn new(syncpoint_manager: &SyncpointManager) -> Self {
        let mut events = Vec::with_capacity(MAX_NV_EVENTS as usize);
        for _ in 0..MAX_NV_EVENTS {
            events.push(InternalEvent::default());
        }
        Self {
            syncpoint_manager: syncpoint_manager as *const _,
            events: Mutex::new(events),
            events_mask: Mutex::new(0),
        }
    }

    fn syncpoint_manager(&self) -> &SyncpointManager {
        unsafe { &*self.syncpoint_manager }
    }

    pub fn nv_os_get_config_u32(&self, _params: &mut IocGetConfigParams) -> NvResult {
        log::trace!("nvhost_ctrl::NvOsGetConfigU32 called");
        NvResult::ConfigVarNotFound
    }

    pub fn ioc_ctrl_event_wait(
        &self,
        params: &mut IocCtrlEventWaitParams,
        is_allocation: bool,
    ) -> NvResult {
        log::debug!(
            "nvhost_ctrl::IocCtrlEventWait syncpt_id={}, threshold={}, timeout={}, is_allocation={}",
            params.fence.id,
            params.fence.value,
            params.timeout,
            is_allocation
        );

        let fence_id = params.fence.id as u32;

        if fence_id >= MAX_SYNC_POINTS {
            return NvResult::BadParameter;
        }

        if params.fence.value == 0 {
            if !self
                .syncpoint_manager()
                .is_syncpoint_allocated(params.fence.id as u32)
            {
                log::warn!("Unallocated syncpt_id={}", params.fence.id);
            } else {
                params.value.raw = self.syncpoint_manager().read_syncpoint_min_value(fence_id);
            }
            return NvResult::Success;
        }

        if self.syncpoint_manager().is_fence_signalled(&params.fence) {
            params.value.raw = self.syncpoint_manager().read_syncpoint_min_value(fence_id);
            return NvResult::Success;
        }

        let new_value = self.syncpoint_manager().update_min(fence_id);
        if self.syncpoint_manager().is_fence_signalled(&params.fence) {
            params.value.raw = new_value;
            return NvResult::Success;
        }

        // Stubbed: Full event wait requires host1x syncpoint manager integration.
        // Return timeout for non-zero timeout, or success if already signalled.
        if params.timeout == 0 {
            return NvResult::Timeout;
        }

        NvResult::Timeout
    }

    pub fn ioc_ctrl_event_register(&self, params: &mut IocCtrlEventRegisterParams) -> NvResult {
        let event_id = params.user_event_id;
        log::debug!("nvhost_ctrl::IocCtrlEventRegister called, user_event_id: {:X}", event_id);
        if event_id >= MAX_NV_EVENTS {
            return NvResult::BadParameter;
        }

        let mut events = self.events.lock().unwrap();
        if events[event_id as usize].registered {
            // Free existing first
            if events[event_id as usize].is_being_used() {
                return NvResult::Busy;
            }
            events[event_id as usize].registered = false;
            let mut mask = self.events_mask.lock().unwrap();
            *mask &= !(1u64 << event_id);
        }

        // Create new event
        events[event_id as usize].status.store(EventState::Available as u32, Ordering::Release);
        events[event_id as usize].registered = true;
        events[event_id as usize].fails = 0;
        events[event_id as usize].assigned_syncpt = 0;
        let mut mask = self.events_mask.lock().unwrap();
        *mask |= 1u64 << event_id;

        NvResult::Success
    }

    pub fn ioc_ctrl_event_unregister(&self, params: &mut IocCtrlEventUnregisterParams) -> NvResult {
        let event_id = params.user_event_id & 0x00FF;
        log::debug!("nvhost_ctrl::IocCtrlEventUnregister called, user_event_id: {:X}", event_id);

        if event_id >= MAX_NV_EVENTS {
            return NvResult::BadParameter;
        }

        let mut events = self.events.lock().unwrap();
        let event = &mut events[event_id as usize];
        if !event.registered {
            return NvResult::Success;
        }
        if event.is_being_used() {
            return NvResult::Busy;
        }
        event.registered = false;
        event.status.store(EventState::Available as u32, Ordering::Release);
        let mut mask = self.events_mask.lock().unwrap();
        *mask &= !(1u64 << event_id);

        NvResult::Success
    }

    pub fn ioc_ctrl_event_unregister_batch(
        &self,
        params: &mut IocCtrlEventUnregisterBatchParams,
    ) -> NvResult {
        let mut event_mask = params.user_events;
        log::debug!("nvhost_ctrl::IocCtrlEventUnregisterBatch called, event_mask: {:X}", event_mask);

        let mut events = self.events.lock().unwrap();
        while event_mask != 0 {
            let event_id = event_mask.trailing_zeros() as u32;
            event_mask &= !(1u64 << event_id);

            if event_id >= MAX_NV_EVENTS {
                return NvResult::BadParameter;
            }

            let event = &mut events[event_id as usize];
            if !event.registered {
                continue;
            }
            if event.is_being_used() {
                return NvResult::Busy;
            }
            event.registered = false;
            event.status.store(EventState::Available as u32, Ordering::Release);
            let mut mask = self.events_mask.lock().unwrap();
            *mask &= !(1u64 << event_id);
        }

        NvResult::Success
    }

    pub fn ioc_ctrl_clear_event_wait(&self, params: &mut IocCtrlEventClearParams) -> NvResult {
        let event_id = params.event_id.slot() as u32;
        log::debug!("nvhost_ctrl::IocCtrlClearEventWait called, event_id: {:X}", event_id);

        if event_id >= MAX_NV_EVENTS {
            return NvResult::BadParameter;
        }

        let mut events = self.events.lock().unwrap();
        let event = &mut events[event_id as usize];

        // Stubbed: Full implementation requires host1x syncpoint manager deregistration.
        event.fails += 1;
        event.status.store(EventState::Cancelled as u32, Ordering::Release);

        NvResult::Success
    }
}

impl NvDevice for NvHostCtrl {
    fn ioctl1(
        &self,
        _fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        output: &mut [u8],
    ) -> NvResult {
        match command.group() {
            0x0 => match command.cmd() {
                0x1b => {
                    let mut params: IocGetConfigParams = read_struct(input);
                    let r = self.nv_os_get_config_u32(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x1c => {
                    let mut params: IocCtrlEventClearParams = read_struct(input);
                    let r = self.ioc_ctrl_clear_event_wait(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x1d => {
                    let mut params: IocCtrlEventWaitParams = read_struct(input);
                    let r = self.ioc_ctrl_event_wait(&mut params, true);
                    write_struct(output, &params);
                    r
                }
                0x1e => {
                    let mut params: IocCtrlEventWaitParams = read_struct(input);
                    let r = self.ioc_ctrl_event_wait(&mut params, false);
                    write_struct(output, &params);
                    r
                }
                0x1f => {
                    let mut params: IocCtrlEventRegisterParams = read_struct(input);
                    let r = self.ioc_ctrl_event_register(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x20 => {
                    let mut params: IocCtrlEventUnregisterParams = read_struct(input);
                    let r = self.ioc_ctrl_event_unregister(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x21 => {
                    let mut params: IocCtrlEventUnregisterBatchParams = read_struct(input);
                    let r = self.ioc_ctrl_event_unregister_batch(&mut params);
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

    fn query_event(&self, event_id: u32) -> Option<u32> {
        let desired_event = SyncpointEventValue { raw: event_id };
        let allocated = desired_event.event_allocated();
        let slot = if allocated {
            desired_event.partial_slot()
        } else {
            desired_event.slot() as u32
        };

        if slot >= MAX_NV_EVENTS {
            log::error!("Event slot {} out of range", slot);
            return None;
        }

        let syncpoint_id = if allocated {
            desired_event.syncpoint_id_for_allocation() as u32
        } else {
            desired_event.syncpoint_id()
        };

        let events = self.events.lock().unwrap();
        let event = &events[slot as usize];
        if event.registered && event.assigned_syncpt == syncpoint_id {
            return Some(slot);
        }

        log::error!("Slot:{}, SyncpointID:{}, requested", slot, syncpoint_id);
        None
    }
}
