// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/applet_manager.h
//! Port of zuyu/src/core/hle/service/am/applet_manager.cpp

use std::sync::Mutex;

use super::am_types::*;
use super::window_system::WindowSystem;

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
/// Manages the lifecycle of applets. In the full implementation this
/// coordinates with WindowSystem via condition variables; here it stores
/// a raw pointer to the WindowSystem matching upstream's `WindowSystem*`.
pub struct AppletManager {
    /// Lock protecting mutable state — upstream: std::mutex m_lock
    lock: Mutex<AppletManagerInner>,
}

struct AppletManagerInner {
    /// WindowSystem pointer — upstream: WindowSystem* m_window_system
    window_system: Option<*const WindowSystem>,
}

// SAFETY: The raw pointer to WindowSystem follows the same lifetime contract as
// upstream: the WindowSystem outlives the AppletManager. Access is synchronized
// via the mutex.
unsafe impl Send for AppletManagerInner {}
unsafe impl Sync for AppletManagerInner {}

impl AppletManager {
    pub fn new() -> Self {
        Self {
            lock: Mutex::new(AppletManagerInner {
                window_system: None,
            }),
        }
    }

    /// Upstream: void SetWindowSystem(WindowSystem* window_system)
    pub fn set_window_system(&self, window_system: Option<*const WindowSystem>) {
        let mut inner = self.lock.lock().unwrap();
        inner.window_system = window_system;
    }

    /// Upstream: void RequestExit()
    pub fn request_exit(&self) {
        let inner = self.lock.lock().unwrap();
        if let Some(ws) = inner.window_system {
            // SAFETY: WindowSystem outlives AppletManager per upstream contract.
            unsafe { &*ws }.on_exit_requested();
        }
    }

    /// Upstream: void OperationModeChanged()
    pub fn operation_mode_changed(&self) {
        let inner = self.lock.lock().unwrap();
        if let Some(ws) = inner.window_system {
            // SAFETY: WindowSystem outlives AppletManager per upstream contract.
            unsafe { &*ws }.on_operation_mode_changed();
        }
    }
}
