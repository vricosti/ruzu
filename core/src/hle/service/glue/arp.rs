// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/arp.h
//! Port of zuyu/src/core/hle/service/glue/arp.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::errors;
use super::glue_manager::{ARPManager, ApplicationLaunchProperty};

/// IPC command IDs for ARP_R
pub mod arp_r_commands {
    pub const GET_APPLICATION_LAUNCH_PROPERTY: u32 = 0;
    pub const GET_APPLICATION_LAUNCH_PROPERTY_WITH_APPLICATION_ID: u32 = 1;
    pub const GET_APPLICATION_CONTROL_PROPERTY: u32 = 2;
    pub const GET_APPLICATION_CONTROL_PROPERTY_WITH_APPLICATION_ID: u32 = 3;
    pub const GET_APPLICATION_INSTANCE_UNREGISTRATION_NOTIFIER: u32 = 4;
    pub const LIST_APPLICATION_INSTANCE_ID: u32 = 5;
    pub const GET_MICRO_APPLICATION_INSTANCE_ID: u32 = 6;
    pub const GET_APPLICATION_CERTIFICATE: u32 = 7;
    pub const GET_PREOMIA_APPLICATION_LAUNCH_PROPERTY: u32 = 9998;
    pub const GET_PREOMIA_APPLICATION_CONTROL_PROPERTY: u32 = 9999;
}

/// IPC command IDs for ARP_W
pub mod arp_w_commands {
    pub const ACQUIRE_REGISTRAR: u32 = 0;
    pub const UNREGISTER_APPLICATION_INSTANCE: u32 = 1;
    pub const ACQUIRE_UPDATER: u32 = 2;
}

/// IPC command IDs for IRegistrar
pub mod registrar_commands {
    pub const ISSUE: u32 = 0;
    pub const SET_APPLICATION_LAUNCH_PROPERTY: u32 = 1;
    pub const SET_APPLICATION_CONTROL_PROPERTY: u32 = 2;
}

/// ARP_R service ("arp:r").
///
/// Corresponds to `ARP_R` in upstream `arp.h`.
pub struct ArpR {
    pub service_name: &'static str,
    // TODO: reference to ARPManager
}

impl ArpR {
    pub fn new() -> Self {
        Self {
            service_name: "arp:r",
        }
    }
}

/// ARP_W service ("arp:w").
///
/// Corresponds to `ARP_W` in upstream `arp.h`.
pub struct ArpW {
    pub service_name: &'static str,
    // TODO: reference to ARPManager
}

impl ArpW {
    pub fn new() -> Self {
        Self {
            service_name: "arp:w",
        }
    }
}

/// IRegistrar: inner service of ARP_W.
///
/// Corresponds to `IRegistrar` in upstream `arp.cpp`.
pub struct IRegistrar {
    pub issued: bool,
    pub launch: ApplicationLaunchProperty,
    pub control: Vec<u8>,
}

impl IRegistrar {
    pub fn new() -> Self {
        Self {
            issued: false,
            launch: ApplicationLaunchProperty::default(),
            control: Vec::new(),
        }
    }

    pub fn issue(&mut self, process_id: u64) -> ResultCode {
        log::debug!("IRegistrar::issue called, process_id={:016X}", process_id);
        if process_id == 0 {
            return errors::RESULT_INVALID_PROCESS_ID;
        }
        if self.issued {
            return errors::RESULT_ALREADY_BOUND;
        }
        self.issued = true;
        RESULT_SUCCESS
    }

    pub fn set_application_launch_property(&mut self, launch: ApplicationLaunchProperty) -> ResultCode {
        if self.issued {
            return errors::RESULT_ALREADY_BOUND;
        }
        self.launch = launch;
        RESULT_SUCCESS
    }

    pub fn set_application_control_property(&mut self, control: Vec<u8>) -> ResultCode {
        if self.issued {
            return errors::RESULT_ALREADY_BOUND;
        }
        self.control = control;
        RESULT_SUCCESS
    }
}
