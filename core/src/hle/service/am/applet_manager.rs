// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/applet_manager.h
//! Port of zuyu/src/core/hle/service/am/applet_manager.cpp

use std::sync::{Arc, Mutex};

use super::am_types::*;
use super::applet::Applet;
use super::window_system::WindowSystem;
use crate::hle::kernel::k_process::KProcess;
use crate::hle::service::os::process::Process;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LaunchType {
    FrontendInitiated,
    ApplicationInitiated,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct FrontendAppletParameters {
    pub program_id: ProgramId,
    pub applet_id: AppletId,
    pub applet_type: AppletType,
    pub launch_type: Option<LaunchType>,
    pub program_index: i32,
    pub previous_program_index: i32,
}

/// Port of AppletManager
///
/// Manages the lifecycle of applets. In the Rust port this stores an
/// `Arc<Mutex<WindowSystem>>` rather than an upstream raw pointer so the
/// shared AM service state can be accessed safely across threads.
pub struct AppletManager {
    /// Lock protecting mutable state — upstream: std::mutex m_lock
    lock: Mutex<AppletManagerInner>,
}

struct AppletManagerInner {
    /// WindowSystem owner — upstream: WindowSystem* m_window_system
    window_system: Option<Arc<Mutex<WindowSystem>>>,
    pending_parameters: Option<FrontendAppletParameters>,
    pending_process: Option<Arc<Mutex<KProcess>>>,
}

impl AppletManager {
    pub fn new() -> Self {
        Self {
            lock: Mutex::new(AppletManagerInner {
                window_system: None,
                pending_parameters: None,
                pending_process: None,
            }),
        }
    }

    /// Upstream: void SetWindowSystem(WindowSystem* window_system)
    pub fn set_window_system(&self, window_system: Option<Arc<Mutex<WindowSystem>>>) {
        let mut inner = self.lock.lock().unwrap();
        inner.window_system = window_system;
        Self::flush_pending_locked(&mut inner);
    }

    /// Upstream: void CreateAndInsertByFrontendAppletParameters(std::unique_ptr<Process>, params)
    pub fn create_and_insert_by_frontend_applet_parameters(
        &self,
        process: Arc<Mutex<KProcess>>,
        params: FrontendAppletParameters,
    ) {
        let mut inner = self.lock.lock().unwrap();
        inner.pending_process = Some(process);
        inner.pending_parameters = Some(params);
        Self::flush_pending_locked(&mut inner);
    }

    /// Upstream: void RequestExit()
    pub fn request_exit(&self) {
        let inner = self.lock.lock().unwrap();
        if let Some(ws) = inner.window_system.clone() {
            ws.lock().unwrap().on_exit_requested();
        }
    }

    /// Upstream: void OperationModeChanged()
    pub fn operation_mode_changed(&self) {
        let inner = self.lock.lock().unwrap();
        if let Some(ws) = inner.window_system.clone() {
            ws.lock().unwrap().on_operation_mode_changed();
        }
    }

    fn flush_pending_locked(inner: &mut AppletManagerInner) {
        let Some(window_system) = inner.window_system.clone() else {
            return;
        };
        let Some(process) = inner.pending_process.take() else {
            return;
        };
        let Some(params) = inner.pending_parameters.take() else {
            inner.pending_process = Some(process);
            return;
        };

        let mut applet = Applet::new(params.applet_id == AppletId::Application);
        applet.process = Process::with_process(process.clone());
        applet.aruid.pid = process.lock().unwrap().get_process_id();
        applet.program_id = params.program_id;
        applet.applet_id = params.applet_id;
        applet.applet_type = params.applet_type;
        applet.previous_program_index = params.previous_program_index;
        applet.is_process_running = true;
        applet.lifecycle_manager.set_focus_state(FocusState::InFocus);
        applet.update_suspension_state_locked(true);

        let applet = Arc::new(Mutex::new(applet));
        let mut ws = window_system.lock().unwrap();
        if params.applet_id == AppletId::QLaunch {
            {
                let mut applet_guard = applet.lock().unwrap();
                applet_guard.lifecycle_manager.set_focus_handling_mode(false);
                applet_guard
                    .lifecycle_manager
                    .set_out_of_focus_suspending_enabled(false);
            }
            ws.track_applet(applet, false);
            ws.request_home_menu_to_get_foreground();
        } else {
            ws.track_applet(applet, true);
            ws.request_application_to_get_foreground();
        }
    }
}
