// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_general.h
//! Port of zuyu/src/core/hle/service/am/frontend/applet_general.cpp
//!
//! Contains Auth, PhotoViewer, and StubApplet frontend applets.

use crate::hle::service::am::am_types::AppletId;

/// Port of AuthAppletType
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AuthAppletType {
    ShowParentalAuthentication = 0,
    RegisterParentalPasscode = 1,
    ChangeParentalPasscode = 2,
}

/// Auth frontend applet stub.
pub struct Auth {
    complete: bool,
    successful: bool,
    auth_type: AuthAppletType,
}

impl Auth {
    pub fn new() -> Self {
        Self {
            complete: false,
            successful: false,
            auth_type: AuthAppletType::ShowParentalAuthentication,
        }
    }
}

/// Port of PhotoViewerAppletMode
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PhotoViewerAppletMode {
    CurrentApp = 0,
    AllApps = 1,
}

/// PhotoViewer frontend applet stub.
pub struct PhotoViewer {
    complete: bool,
    mode: PhotoViewerAppletMode,
}

impl PhotoViewer {
    pub fn new() -> Self {
        Self {
            complete: false,
            mode: PhotoViewerAppletMode::CurrentApp,
        }
    }
}

/// Stub applet for unimplemented applet types.
pub struct StubApplet {
    id: AppletId,
}

impl StubApplet {
    pub fn new(id: AppletId) -> Self {
        Self { id }
    }
}
