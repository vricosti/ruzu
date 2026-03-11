// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/applets/cabinet.h and cabinet.cpp
//! Cabinet (Amiibo) applet interface.

use super::applet::Applet;

/// Corresponds to upstream `Service::NFP::CabinetMode`.
/// TODO: Import from hle::service::nfp when available.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum CabinetMode {
    StartNicknameAndOwnerSettings = 0,
    StartGameDataEraser = 1,
    StartRestorer = 2,
    StartFormatter = 3,
}

impl Default for CabinetMode {
    fn default() -> Self {
        Self::StartNicknameAndOwnerSettings
    }
}

/// Corresponds to upstream `Service::NFP::TagInfo`.
/// TODO: Import from hle::service::nfp when available.
#[derive(Debug, Clone)]
pub struct TagInfo {
    pub uuid: [u8; 10],
    pub uuid_length: u8,
    pub padding1: [u8; 0x15],
    pub protocol: u32,
    pub tag_type: u32,
    pub padding2: [u8; 0x30],
}

impl Default for TagInfo {
    fn default() -> Self {
        Self {
            uuid: [0; 10],
            uuid_length: 0,
            padding1: [0; 0x15],
            protocol: 0,
            tag_type: 0,
            padding2: [0; 0x30],
        }
    }
}

/// Corresponds to upstream `Service::NFP::RegisterInfo`.
/// TODO: Import from hle::service::nfp when available.
#[derive(Debug, Clone)]
pub struct RegisterInfo {
    // Placeholder - full struct from nfp_types.h
    pub _placeholder: [u8; 0x100],
}

impl Default for RegisterInfo {
    fn default() -> Self {
        Self {
            _placeholder: [0; 0x100],
        }
    }
}

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
        // TODO: nfp_device: Arc<Mutex<NfcDevice>> when available
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
    ) {
        log::warn!("(STUBBED) called");
        callback(false, String::new());
    }
}
