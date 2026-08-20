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
use crate::hle::service::am::applet::Applet;
use crate::hle::service::am::applet_data_broker::AppletDataBroker;

use super::applet_cabinet::CabinetMode;
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
#[derive(Default)]
pub struct FrontendAppletSet {
    pub controller: Option<Arc<dyn ControllerApplet>>,
    pub software_keyboard: Option<Arc<dyn SoftwareKeyboardApplet>>,
}

pub struct FrontendAppletHolder {
    cabinet_mode: CabinetMode,
    current_applet_id: AppletId,
    frontend: FrontendAppletSet,
}

impl FrontendAppletHolder {
    pub fn new(hid_core: Arc<Mutex<HIDCore>>) -> Self {
        Self {
            cabinet_mode: CabinetMode::default(),
            current_applet_id: AppletId::None,
            frontend: FrontendAppletSet {
                controller: Some(Arc::new(DefaultControllerApplet::new(hid_core))),
                software_keyboard: Some(Arc::new(DefaultSoftwareKeyboardApplet::new())),
            },
        }
    }

    pub fn get_frontend_applet_set(&self) -> &FrontendAppletSet {
        &self.frontend
    }

    /// Replace every supplied frontend implementation while preserving the
    /// existing default for omitted entries.
    ///
    /// Port of `FrontendAppletHolder::SetFrontendAppletSet`.
    pub fn set_frontend_applet_set(&mut self, mut set: FrontendAppletSet) {
        if set.controller.is_some() {
            self.frontend.controller = set.controller.take();
        }
        if set.software_keyboard.is_some() {
            self.frontend.software_keyboard = set.software_keyboard.take();
        }
    }

    pub fn get_cabinet_mode(&self) -> CabinetMode {
        self.cabinet_mode
    }

    pub fn set_cabinet_mode(&mut self, mode: CabinetMode) {
        self.cabinet_mode = mode;
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
        applet: std::sync::Weak<std::sync::Mutex<Applet>>,
        broker: Arc<AppletDataBroker>,
        id: AppletId,
        mode: LibraryAppletMode,
    ) -> Option<Box<dyn FrontendApplet>> {
        match id {
            AppletId::Controller => Some(Box::new(Controller::new(
                system,
                applet,
                broker,
                mode,
                Arc::clone(
                    self.frontend
                        .controller
                        .as_ref()
                        .expect("default controller applet is installed"),
                ),
            ))),
            AppletId::MiiEdit => Some(Box::new(MiiEdit::new(system, broker, mode))),
            AppletId::SoftwareKeyboard => Some(Box::new(SoftwareKeyboard::new(
                system,
                applet,
                broker,
                mode,
                Arc::clone(
                    self.frontend
                        .software_keyboard
                        .as_ref()
                        .expect("default software keyboard applet is installed"),
                ),
            ))),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::applets::applet::Applet;
    use crate::frontend::applets::controller::{ControllerParameters, ReconfigureCallback};

    struct TestControllerApplet;

    impl Applet for TestControllerApplet {
        fn close(&self) {}
    }

    impl ControllerApplet for TestControllerApplet {
        fn reconfigure_controllers(
            &self,
            callback: ReconfigureCallback,
            _parameters: &ControllerParameters,
        ) {
            callback(true);
        }
    }

    #[test]
    fn stores_frontend_selected_applet_and_cabinet_mode() {
        let hid_core = Arc::new(Mutex::new(HIDCore::new()));
        let mut holder = FrontendAppletHolder::new(hid_core);

        holder.set_current_applet_id(AppletId::Cabinet);
        holder.set_cabinet_mode(CabinetMode::StartFormatter);

        assert_eq!(holder.get_current_applet_id(), AppletId::Cabinet);
        assert_eq!(holder.get_cabinet_mode(), CabinetMode::StartFormatter);
    }

    #[test]
    fn frontend_applet_set_replaces_only_supplied_implementations() {
        let hid_core = Arc::new(Mutex::new(HIDCore::new()));
        let mut holder = FrontendAppletHolder::new(hid_core);
        let original_keyboard = Arc::clone(
            holder
                .get_frontend_applet_set()
                .software_keyboard
                .as_ref()
                .unwrap(),
        );
        let controller: Arc<dyn ControllerApplet> = Arc::new(TestControllerApplet);

        holder.set_frontend_applet_set(FrontendAppletSet {
            controller: Some(Arc::clone(&controller)),
            software_keyboard: None,
        });

        assert!(Arc::ptr_eq(
            holder
                .get_frontend_applet_set()
                .controller
                .as_ref()
                .unwrap(),
            &controller,
        ));
        assert!(Arc::ptr_eq(
            holder
                .get_frontend_applet_set()
                .software_keyboard
                .as_ref()
                .unwrap(),
            &original_keyboard,
        ));
    }
}
