// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/apm/apm_interface.h
//! Port of zuyu/src/core/hle/service/apm/apm_interface.cpp
//!
//! APM and APM_Sys service interfaces, and ISession.

use std::sync::{Arc, Mutex};
use super::apm::Module;
use super::apm_controller::{Controller, CpuBoostMode, PerformanceConfiguration, PerformanceMode};

/// IPC command IDs for APM
pub mod apm_commands {
    pub const OPEN_SESSION: u32 = 0;
    pub const GET_PERFORMANCE_MODE: u32 = 1;
    pub const IS_CPU_OVERCLOCK_ENABLED: u32 = 6;
}

/// IPC command IDs for APM_Sys
pub mod apm_sys_commands {
    pub const REQUEST_PERFORMANCE_MODE: u32 = 0;
    pub const GET_PERFORMANCE_EVENT: u32 = 1;
    pub const GET_THROTTLING_STATE: u32 = 2;
    pub const GET_LAST_THROTTLING_STATE: u32 = 3;
    pub const CLEAR_LAST_THROTTLING_STATE: u32 = 4;
    pub const LOAD_AND_APPLY_SETTINGS: u32 = 5;
    pub const SET_CPU_BOOST_MODE: u32 = 6;
    pub const GET_CURRENT_PERFORMANCE_CONFIGURATION: u32 = 7;
}

/// IPC command IDs for ISession
pub mod session_commands {
    pub const SET_PERFORMANCE_CONFIGURATION: u32 = 0;
    pub const GET_PERFORMANCE_CONFIGURATION: u32 = 1;
    pub const SET_CPU_OVERCLOCK_ENABLED: u32 = 2;
}

/// ISession interface.
///
/// Corresponds to `ISession` in upstream `apm_interface.cpp`.
pub struct ISession {
    controller: Arc<Mutex<Controller>>,
}

impl ISession {
    pub fn new(controller: Arc<Mutex<Controller>>) -> Self {
        Self { controller }
    }

    pub fn set_performance_configuration(
        &self,
        mode: PerformanceMode,
        config: PerformanceConfiguration,
    ) {
        log::debug!(
            "ISession::set_performance_configuration called, mode={:?}, config={:?}",
            mode,
            config
        );
        self.controller
            .lock()
            .unwrap()
            .set_performance_configuration(mode, config);
    }

    pub fn get_performance_configuration(
        &self,
        mode: PerformanceMode,
    ) -> PerformanceConfiguration {
        log::debug!(
            "ISession::get_performance_configuration called, mode={:?}",
            mode
        );
        self.controller
            .lock()
            .unwrap()
            .get_current_performance_configuration(mode)
    }

    pub fn set_cpu_overclock_enabled(&self, enabled: bool) {
        log::warn!(
            "(STUBBED) ISession::set_cpu_overclock_enabled called, enabled={}",
            enabled
        );
    }
}

/// APM service ("apm", "apm:am").
///
/// Corresponds to `APM` class in upstream `apm_interface.h`.
pub struct APM {
    module: Arc<Module>,
    controller: Arc<Mutex<Controller>>,
    name: String,
}

impl APM {
    pub fn new(module: Arc<Module>, controller: Arc<Mutex<Controller>>, name: &str) -> Self {
        Self {
            module,
            controller,
            name: name.to_string(),
        }
    }

    pub fn open_session(&self) -> ISession {
        log::debug!("APM({})::open_session called", self.name);
        ISession::new(self.controller.clone())
    }

    pub fn get_performance_mode(&self) -> PerformanceMode {
        log::debug!("APM({})::get_performance_mode called", self.name);
        self.controller.lock().unwrap().get_current_performance_mode()
    }

    pub fn is_cpu_overclock_enabled(&self) -> bool {
        log::warn!("(STUBBED) APM({})::is_cpu_overclock_enabled called", self.name);
        false
    }
}

/// APM_Sys service ("apm:sys").
///
/// Corresponds to `APM_Sys` class in upstream `apm_interface.h`.
pub struct ApmSys {
    controller: Arc<Mutex<Controller>>,
}

impl ApmSys {
    pub fn new(controller: Arc<Mutex<Controller>>) -> Self {
        Self { controller }
    }

    pub fn get_performance_event(&self) -> ISession {
        log::debug!("ApmSys::get_performance_event called");
        ISession::new(self.controller.clone())
    }

    pub fn set_cpu_boost_mode(&self, mode: CpuBoostMode) {
        log::debug!("ApmSys::set_cpu_boost_mode called, mode={:?}", mode);
        self.controller.lock().unwrap().set_from_cpu_boost_mode(mode);
    }

    pub fn get_current_performance_configuration(&self) -> PerformanceConfiguration {
        log::debug!("ApmSys::get_current_performance_configuration called");
        let mut ctrl = self.controller.lock().unwrap();
        let mode = ctrl.get_current_performance_mode();
        ctrl.get_current_performance_configuration(mode)
    }
}
