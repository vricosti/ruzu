// SPDX-FileCopyrightText: 2021 yuzu Emulator Project
// SPDX-FileCopyrightText: 2021 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_ctrl.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_ctrl.cpp

use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Weak;
use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_process::KProcess;
use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::kernel::k_scheduler::KScheduler;
use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::core::syncpoint_manager::SyncpointManager;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::devices::nvmap::{read_struct, write_struct};
use crate::hle::service::nvdrv::nvdata::*;
use crate::hle::service::nvdrv::nvdrv::EventInterface;

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
    readable_event: Option<Arc<Mutex<KReadableEvent>>>,
    status: AtomicU32,
    fails: u32,
    assigned_syncpt: u32,
    assigned_value: u32,
    registered: bool,
    wait_handle: Option<u64>,
    owner: Option<QueriedEventOwner>,
}

struct QueriedEventOwner {
    process: Weak<Mutex<KProcess>>,
    scheduler: Weak<Mutex<KScheduler>>,
}

impl Default for InternalEvent {
    fn default() -> Self {
        Self {
            readable_event: None,
            status: AtomicU32::new(EventState::Available as u32),
            fails: 0,
            assigned_syncpt: 0,
            assigned_value: 0,
            registered: false,
            wait_handle: None,
            owner: None,
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
    events_interface: Arc<EventInterface>,
    syncpoint_manager: *const SyncpointManager,
    events: Arc<Mutex<Vec<InternalEvent>>>,
    events_mask: Mutex<u64>,
}

// Safety: NvHostCtrl is only accessed from service thread.
unsafe impl Send for NvHostCtrl {}
unsafe impl Sync for NvHostCtrl {}

impl NvHostCtrl {
    fn bytes_to_cstr(bytes: &[u8]) -> String {
        let len = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
        String::from_utf8_lossy(&bytes[..len]).into_owned()
    }

    pub fn new(
        events_interface: Arc<EventInterface>,
        syncpoint_manager: &SyncpointManager,
    ) -> Self {
        let mut events = Vec::with_capacity(MAX_NV_EVENTS as usize);
        for _ in 0..MAX_NV_EVENTS {
            events.push(InternalEvent::default());
        }
        Self {
            events_interface,
            syncpoint_manager: syncpoint_manager as *const _,
            events: Arc::new(Mutex::new(events)),
            events_mask: Mutex::new(0),
        }
    }

    fn syncpoint_manager(&self) -> &SyncpointManager {
        unsafe { &*self.syncpoint_manager }
    }

    fn create_nv_event(&self, events: &mut [InternalEvent], mask: &mut u64, event_id: u32) {
        let event = &mut events[event_id as usize];
        event.readable_event = Some(
            self.events_interface
                .create_event(&format!("NVCTRL::NvEvent_{}", event_id)),
        );
        event
            .status
            .store(EventState::Available as u32, Ordering::Release);
        event.registered = true;
        event.fails = 0;
        event.assigned_syncpt = 0;
        event.assigned_value = 0;
        event.wait_handle = None;
        event.owner = None;
        *mask |= 1u64 << event_id;
    }

    fn free_nv_event(&self, events: &mut [InternalEvent], mask: &mut u64, event_id: u32) {
        let event = &mut events[event_id as usize];
        if let Some(readable_event) = event.readable_event.take() {
            self.events_interface.free_event(readable_event);
        }
        event
            .status
            .store(EventState::Available as u32, Ordering::Release);
        event.registered = false;
        event.assigned_syncpt = 0;
        event.assigned_value = 0;
        event.wait_handle = None;
        event.fails = 0;
        event.owner = None;
        *mask &= !(1u64 << event_id);
    }

    fn signal_event_from_slot(events: &Arc<Mutex<Vec<InternalEvent>>>, slot: u32) {
        let owner_and_event = {
            let events_guard = events.lock().unwrap();
            let event = &events_guard[slot as usize];
            (
                event
                    .owner
                    .as_ref()
                    .and_then(|owner| Some((owner.process.upgrade()?, owner.scheduler.upgrade()?))),
                event.readable_event.clone(),
            )
        };

        let (Some((process, scheduler)), Some(readable_event)) = owner_and_event else {
            return;
        };

        let mut process = process.lock().unwrap();
        readable_event
            .lock()
            .unwrap()
            .signal(&mut process, &scheduler);
    }

    fn free_event_locked(
        &self,
        events: &mut [InternalEvent],
        mask: &mut u64,
        slot: u32,
    ) -> NvResult {
        if slot >= MAX_NV_EVENTS {
            return NvResult::BadParameter;
        }

        let event = &events[slot as usize];
        if !event.registered {
            return NvResult::Success;
        }

        if event.is_being_used() {
            return NvResult::Busy;
        }

        self.free_nv_event(events, mask, slot);
        NvResult::Success
    }

    fn find_free_nv_event(
        &self,
        events: &mut [InternalEvent],
        mask: &mut u64,
        syncpoint_id: u32,
    ) -> u32 {
        let mut slot = MAX_NV_EVENTS;
        let mut free_slot = MAX_NV_EVENTS;

        for i in 0..MAX_NV_EVENTS {
            let event = &events[i as usize];
            if event.registered {
                if !event.is_being_used() {
                    slot = i;
                    if event.assigned_syncpt == syncpoint_id {
                        return slot;
                    }
                }
            } else if free_slot == MAX_NV_EVENTS {
                free_slot = i;
            }
        }

        if free_slot < MAX_NV_EVENTS {
            self.create_nv_event(events, mask, free_slot);
            return free_slot;
        }

        if slot < MAX_NV_EVENTS {
            return slot;
        }

        log::error!("Failed to allocate an NV event");
        0
    }

    pub fn nv_os_get_config_u32(&self, params: &mut IocGetConfigParams) -> NvResult {
        log::debug!(
            "nvhost_ctrl::NvOsGetConfigU32 called, domain='{}' param='{}' config='{}'",
            Self::bytes_to_cstr(&params.domain_str),
            Self::bytes_to_cstr(&params.param_str),
            Self::bytes_to_cstr(&params.config_str)
        );
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
        let existing_event_id = params.value.raw;

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

        let result = {
            let mut events = self.events.lock().unwrap();
            let mut mask = self.events_mask.lock().unwrap();
            let slot = if is_allocation {
                params.value.raw = 0;
                self.find_free_nv_event(&mut events, &mut mask, fence_id)
            } else {
                params.value.raw
            };

            let target_value = params.fence.value;

            if slot >= MAX_NV_EVENTS {
                return NvResult::BadParameter;
            }

            if params.timeout == 0 {
                if events[slot as usize].fails > 2 {
                    events[slot as usize].fails = 0;
                    self.syncpoint_manager().wait_host(fence_id, target_value);
                    params.value.raw = target_value;
                    NvResult::Success
                } else {
                    NvResult::Timeout
                }
            } else {
                if !events[slot as usize].registered {
                    return NvResult::BadParameter;
                }
                if events[slot as usize].is_being_used() {
                    return NvResult::BadParameter;
                }

                if events[slot as usize].fails > 2 {
                    events[slot as usize].fails = 0;
                    self.syncpoint_manager().wait_host(fence_id, target_value);
                    params.value.raw = target_value;
                    NvResult::Success
                } else {
                    let event = &mut events[slot as usize];
                    params.value.raw = 0;
                    event
                        .status
                        .store(EventState::Waiting as u32, Ordering::Release);
                    event.assigned_syncpt = fence_id;
                    event.assigned_value = target_value;
                    if is_allocation {
                        params.value.raw = slot | ((fence_id & 0x0FFF) << 16) | (1 << 28);
                    } else {
                        params.value.raw = slot | (fence_id << 4);
                    }
                    let system = self.events_interface.system();
                    let events_ref = Arc::clone(&self.events);
                    event.wait_handle = self.syncpoint_manager().register_host_action(
                        fence_id,
                        target_value,
                        Box::new(move || {
                            let should_signal = {
                                let mut events = events_ref.lock().unwrap();
                                let event = &mut events[slot as usize];
                                event
                                    .status
                                    .swap(EventState::Signalling as u32, Ordering::AcqRel)
                                    == EventState::Waiting as u32
                            };

                            if should_signal {
                                let _ = system;
                                Self::signal_event_from_slot(&events_ref, slot);
                            }

                            let mut events = events_ref.lock().unwrap();
                            events[slot as usize]
                                .status
                                .store(EventState::Signalled as u32, Ordering::Release);
                        }),
                    );

                    NvResult::Timeout
                }
            }
        };

        if !is_allocation && existing_event_id < MAX_NV_EVENTS {
            if let Ok(mut events) = self.events.lock() {
                events[existing_event_id as usize].fails = 0;
            }
        }

        result
    }

    pub fn ioc_ctrl_event_register(&self, params: &mut IocCtrlEventRegisterParams) -> NvResult {
        let event_id = params.user_event_id;
        log::debug!(
            "nvhost_ctrl::IocCtrlEventRegister called, user_event_id: {:X}",
            event_id
        );
        if event_id >= MAX_NV_EVENTS {
            return NvResult::BadParameter;
        }

        let mut mask = self.events_mask.lock().unwrap();
        let mut events = self.events.lock().unwrap();
        if events[event_id as usize].registered {
            let result = self.free_event_locked(&mut events, &mut mask, event_id);
            if result != NvResult::Success {
                return result;
            }
        }

        self.create_nv_event(&mut events, &mut mask, event_id);

        NvResult::Success
    }

    pub fn ioc_ctrl_event_unregister(&self, params: &mut IocCtrlEventUnregisterParams) -> NvResult {
        let event_id = params.user_event_id & 0x00FF;
        log::debug!(
            "nvhost_ctrl::IocCtrlEventUnregister called, user_event_id: {:X}",
            event_id
        );

        if event_id >= MAX_NV_EVENTS {
            return NvResult::BadParameter;
        }

        let mut mask = self.events_mask.lock().unwrap();
        let mut events = self.events.lock().unwrap();
        self.free_event_locked(&mut events, &mut mask, event_id)
    }

    pub fn ioc_ctrl_event_unregister_batch(
        &self,
        params: &mut IocCtrlEventUnregisterBatchParams,
    ) -> NvResult {
        let mut event_mask = params.user_events;
        log::debug!(
            "nvhost_ctrl::IocCtrlEventUnregisterBatch called, event_mask: {:X}",
            event_mask
        );

        let mut mask = self.events_mask.lock().unwrap();
        let mut events = self.events.lock().unwrap();
        while event_mask != 0 {
            let event_id = event_mask.trailing_zeros() as u32;
            event_mask &= !(1u64 << event_id);

            let result = self.free_event_locked(&mut events, &mut mask, event_id);
            if result != NvResult::Success {
                return result;
            }
        }

        NvResult::Success
    }

    pub fn ioc_ctrl_clear_event_wait(&self, params: &mut IocCtrlEventClearParams) -> NvResult {
        let event_id = params.event_id.slot() as u32;
        log::debug!(
            "nvhost_ctrl::IocCtrlClearEventWait called, event_id: {:X}",
            event_id
        );

        if event_id >= MAX_NV_EVENTS {
            return NvResult::BadParameter;
        }

        let mut events = self.events.lock().unwrap();
        let event = &mut events[event_id as usize];

        if !event.registered {
            return NvResult::Success;
        }

        if event
            .status
            .swap(EventState::Cancelling as u32, Ordering::AcqRel)
            == EventState::Waiting as u32
        {
            if let Some(wait_handle) = event.wait_handle.take() {
                self.syncpoint_manager()
                    .deregister_host_action(event.assigned_syncpt, wait_handle);
            }
            self.syncpoint_manager().update_min(event.assigned_syncpt);
        }
        event.fails += 1;
        event
            .status
            .store(EventState::Cancelled as u32, Ordering::Release);
        if let Some(readable_event) = &event.readable_event {
            let _ = readable_event.lock().unwrap().clear();
        }

        NvResult::Success
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::{
        IocCtrlEventClearParams, IocCtrlEventRegisterParams, IocCtrlEventWaitParams, NvHostCtrl,
    };
    use crate::core::{System, SystemRef};
    use crate::hle::service::nvdrv::core::syncpoint_manager::SyncpointManager;
    use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
    use crate::hle::service::nvdrv::nvdata::{NvFence, NvResult};
    use crate::hle::service::nvdrv::nvdrv::EventInterface;

    #[test]
    fn event_wait_allocation_populates_allocated_event_value() {
        let system = System::new_for_test();
        let events = Arc::new(EventInterface::new(SystemRef::from_ref(&system)));
        let syncpoints = SyncpointManager::new();
        let ctrl = NvHostCtrl::new(events, &syncpoints);
        let fence_id = syncpoints.allocate_syncpoint(false);

        let mut params = IocCtrlEventWaitParams {
            fence: NvFence {
                id: fence_id as i32,
                value: 5,
            },
            timeout: 1,
            ..Default::default()
        };

        let result = ctrl.ioc_ctrl_event_wait(&mut params, true);

        assert_eq!(result, NvResult::Timeout);
        assert!(params.value.event_allocated());
        assert_eq!(
            params.value.syncpoint_id_for_allocation() as u32,
            fence_id & 0x0FFF
        );
        assert!(ctrl.query_event(params.value.raw).is_some());

        std::mem::forget(system);
    }

    #[test]
    fn clear_event_wait_cancels_registered_slot() {
        let system = System::new_for_test();
        let events = Arc::new(EventInterface::new(SystemRef::from_ref(&system)));
        let syncpoints = SyncpointManager::new();
        let ctrl = NvHostCtrl::new(events, &syncpoints);
        let mut register = IocCtrlEventRegisterParams { user_event_id: 2 };
        assert_eq!(
            ctrl.ioc_ctrl_event_register(&mut register),
            NvResult::Success
        );

        let mut clear = IocCtrlEventClearParams {
            event_id: super::SyncpointEventValue { raw: 2 },
        };

        assert_eq!(
            ctrl.ioc_ctrl_clear_event_wait(&mut clear),
            NvResult::Success
        );

        std::mem::forget(system);
    }
}

impl NvDevice for NvHostCtrl {
    fn ioctl1(&self, _fd: DeviceFD, command: Ioctl, input: &[u8], output: &mut [u8]) -> NvResult {
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

    fn query_event(&self, event_id: u32) -> Option<Arc<Mutex<KReadableEvent>>> {
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
            return event.readable_event.clone();
        }

        log::error!("Slot:{}, SyncpointID:{}, requested", slot, syncpoint_id);
        None
    }

    fn register_query_event_owner(
        &self,
        event_id: u32,
        process: Arc<Mutex<KProcess>>,
        scheduler: Arc<Mutex<KScheduler>>,
    ) {
        let desired_event = SyncpointEventValue { raw: event_id };
        let allocated = desired_event.event_allocated();
        let slot = if allocated {
            desired_event.partial_slot()
        } else {
            desired_event.slot() as u32
        };

        if slot >= MAX_NV_EVENTS {
            return;
        }

        let mut events = self.events.lock().unwrap();
        let event = &mut events[slot as usize];
        if event.registered {
            event.owner = Some(QueriedEventOwner {
                process: Arc::downgrade(&process),
                scheduler: Arc::downgrade(&scheduler),
            });
        }
    }
}
