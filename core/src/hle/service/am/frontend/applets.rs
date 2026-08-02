// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applets.h
//! Port of zuyu/src/core/hle/service/am/frontend/applets.cpp
//!
//! Base FrontendApplet class and FrontendAppletHolder.

use std::sync::Arc;

use hid_core::hid_core::HIDCore;
use parking_lot::Mutex;

use crate::core::SystemRef;
use crate::frontend::applets::controller::{ControllerApplet, DefaultControllerApplet};
use crate::frontend::applets::software_keyboard::{
    DefaultSoftwareKeyboardApplet, SoftwareKeyboardApplet,
};
use crate::hle::result::ResultCode;
use crate::hle::service::am::am_types::{AppletId, LibraryAppletMode};
use crate::hle::service::am::applet_data_broker::AppletDataBroker;

use super::applet_controller::Controller;
use super::applet_mii_edit::MiiEdit;
use super::applet_software_keyboard::SoftwareKeyboard;

/// Base trait for all frontend applet implementations.
///
/// Port of FrontendApplet class.
pub trait FrontendApplet: Send + Sync {
    fn initialize(&mut self);
    fn get_status(&self) -> ResultCode;
    fn execute_interactive(&mut self);
    fn execute(&mut self);
    fn request_exit(&mut self);
    fn get_library_applet_mode(&self) -> LibraryAppletMode;
    fn is_initialized(&self) -> bool;
    /// Rust ownership adaptation for upstream `FrontendApplet::Exit()`, whose
    /// weak Applet reference cannot be stored while Applet owns this trait object.
    fn is_complete(&self) -> bool;
}

/// Holds the set of frontend applet implementations.
///
/// Port of FrontendAppletHolder class.
pub struct FrontendAppletHolder {
    current_applet_id: AppletId,
    controller: Arc<dyn ControllerApplet>,
    software_keyboard: Arc<dyn SoftwareKeyboardApplet>,
}

impl FrontendAppletHolder {
    pub fn new(hid_core: Arc<Mutex<HIDCore>>) -> Self {
        Self {
            current_applet_id: AppletId::None,
            controller: Arc::new(DefaultControllerApplet::new(hid_core)),
            software_keyboard: Arc::new(DefaultSoftwareKeyboardApplet::new()),
        }
    }

    pub fn get_current_applet_id(&self) -> AppletId {
        self.current_applet_id
    }

    pub fn set_current_applet_id(&mut self, applet_id: AppletId) {
        self.current_applet_id = applet_id;
    }

    pub fn get_applet(
        &self,
        system: SystemRef,
        broker: Arc<AppletDataBroker>,
        id: AppletId,
        mode: LibraryAppletMode,
    ) -> Option<Box<dyn FrontendApplet>> {
        match id {
            AppletId::Controller => Some(Box::new(Controller::new(
                system,
                broker,
                mode,
                Arc::clone(&self.controller),
            ))),
            AppletId::MiiEdit => Some(Box::new(MiiEdit::new(system, broker, mode))),
            AppletId::SoftwareKeyboard => Some(Box::new(SoftwareKeyboard::new(
                system,
                broker,
                mode,
                Arc::clone(&self.software_keyboard),
            ))),
            _ => None,
        }
    }
}
