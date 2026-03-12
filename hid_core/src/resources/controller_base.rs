// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/controller_base.h and controller_base.cpp

use std::sync::{Arc, Mutex};

use common::ResultCode;

use crate::resources::applet_resource::AppletResource;
use crate::resources::shared_memory_format::SharedMemoryFormat;

/// Shared controller activation state and resource references.
///
/// Upstream ControllerBase holds:
///   - is_activated: bool
///   - applet_resource: shared_ptr<AppletResource>
///   - shared_mutex: recursive_mutex*
///   - hid_core: HIDCore&
///
/// In Rust, each concrete controller embeds a ControllerActivation to hold
/// the activation flag and optional shared resource references.
pub struct ControllerActivation {
    pub is_activated: bool,
}

impl ControllerActivation {
    pub fn new() -> Self {
        Self {
            is_activated: false,
        }
    }

    /// Port of ControllerBase::Activate().
    ///
    /// If already activated, returns success immediately.
    /// Otherwise sets is_activated = true. The caller is responsible
    /// for invoking on_init() on the concrete controller after this returns
    /// true for the first time. Returns true if newly activated.
    pub fn activate(&mut self) -> (ResultCode, bool) {
        if self.is_activated {
            return (ResultCode::SUCCESS, false);
        }
        self.is_activated = true;
        (ResultCode::SUCCESS, true)
    }

    /// Port of ControllerBase::Activate(u64 aruid).
    /// Upstream delegates to Activate().
    pub fn activate_with_aruid(&mut self, _aruid: u64) -> (ResultCode, bool) {
        self.activate()
    }

    /// Port of ControllerBase::DeactivateController().
    ///
    /// Returns true if the controller was previously activated (caller should
    /// invoke on_release() on the concrete controller).
    pub fn deactivate(&mut self) -> bool {
        let was_activated = self.is_activated;
        self.is_activated = false;
        was_activated
    }

    /// Port of ControllerBase::IsControllerActivated().
    pub fn is_controller_activated(&self) -> bool {
        self.is_activated
    }
}

impl Default for ControllerActivation {
    fn default() -> Self {
        Self::new()
    }
}
