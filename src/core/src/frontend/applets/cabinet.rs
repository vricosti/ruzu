// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/applets/cabinet.h and cabinet.cpp
//! Cabinet (Amiibo) applet interface.

use parking_lot::Mutex;
use std::sync::Arc;

use super::applet::Applet;
use crate::hle::service::nfc::common::device::NfcDevice;
use crate::hle::service::nfp::nfp_types::{CabinetMode, RegisterInfo, TagInfo};

/// Parameters for the cabinet applet.
///
/// Corresponds to upstream `Core::Frontend::CabinetParameters`.
#[derive(Debug, Clone, Default)]
pub struct CabinetParameters {
    pub tag_info: TagInfo,
    pub register_info: RegisterInfo,
    pub mode: CabinetMode,
}

/// Callback type for cabinet applet results.
///
/// Corresponds to upstream `CabinetCallback`.
pub type CabinetCallback = Box<dyn FnOnce(bool, String) + Send>;

/// Cabinet applet trait.
///
/// Corresponds to upstream `Core::Frontend::CabinetApplet`.
pub trait CabinetApplet: Applet {
    fn show_cabinet_applet(
        &self,
        callback: CabinetCallback,
        parameters: &CabinetParameters,
        nfp_device: Arc<Mutex<NfcDevice>>,
    );
}

/// Default (stub) cabinet applet implementation.
///
/// Corresponds to upstream `Core::Frontend::DefaultCabinetApplet`.
pub struct DefaultCabinetApplet;

impl Applet for DefaultCabinetApplet {
    fn close(&self) {}
}

impl CabinetApplet for DefaultCabinetApplet {
    fn show_cabinet_applet(
        &self,
        callback: CabinetCallback,
        _parameters: &CabinetParameters,
        _nfp_device: Arc<Mutex<NfcDevice>>,
    ) {
        log::warn!("(STUBBED) called");
        callback(false, String::new());
    }
}
