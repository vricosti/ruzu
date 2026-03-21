// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/hid_registration.h
//! Port of zuyu/src/core/hle/service/am/hid_registration.cpp

use std::sync::Arc;

use hid_core::resource_manager::ResourceManager;

use crate::hle::service::os::process::Process;

/// Port of HidRegistration
///
/// Manages HID resource registration for an applet process.
/// On construction, registers the process's applet resource user ID
/// with the HID resource manager and enables vibration. On destruction,
/// unregisters vibration and the applet resource user ID.
pub struct HidRegistration {
    /// Upstream: `Process& m_process`.
    process: *const Process,
    /// Upstream: obtained via `system.ServiceManager().GetService<HID::IHidServer>("hid")`
    /// then `m_hid_server->GetResourceManager()`.
    resource_manager: Option<Arc<parking_lot::Mutex<ResourceManager>>>,
    /// Cached PID for Drop.
    pid: u64,
}

unsafe impl Send for HidRegistration {}
unsafe impl Sync for HidRegistration {}

impl HidRegistration {
    /// Creates a new HidRegistration.
    ///
    /// Upstream constructor calls:
    /// - RegisterAppletResourceUserId(pid, true)
    /// - SetAruidValidForVibration(pid, true)
    pub fn new(process: &Process, resource_manager: Option<Arc<parking_lot::Mutex<ResourceManager>>>) -> Self {
        let pid = process.get_process_id();

        if let Some(ref rm) = resource_manager {
            let rm = rm.lock();
            rm.register_applet_resource_user_id(pid, true);
            rm.set_aruid_valid_for_vibration(pid, true);
        }

        Self {
            process: process as *const Process,
            resource_manager,
            pid,
        }
    }

    /// Forward input enable/disable to HID resource manager.
    ///
    /// Upstream calls:
    /// - SetAruidValidForVibration(pid, enable)
    /// - EnableInput(pid, enable)
    pub fn enable_applet_to_get_input(&self, enable: bool) {
        if let Some(ref rm) = self.resource_manager {
            let rm = rm.lock();
            rm.set_aruid_valid_for_vibration(self.pid, enable);
            rm.enable_input(self.pid, enable);
        }
    }
}

impl Drop for HidRegistration {
    /// Upstream destructor calls:
    /// - SetAruidValidForVibration(pid, false)
    /// - UnregisterAppletResourceUserId(pid)
    fn drop(&mut self) {
        if let Some(ref rm) = self.resource_manager {
            let rm = rm.lock();
            rm.set_aruid_valid_for_vibration(self.pid, false);
            rm.unregister_applet_resource_user_id(self.pid);
        }
    }
}
