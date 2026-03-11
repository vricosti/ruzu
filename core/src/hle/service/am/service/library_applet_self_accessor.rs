// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/library_applet_self_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/library_applet_self_accessor.cpp

use crate::hle::service::am::am_types::{AppletId, AppletIdentityInfo, LibraryAppletMode};

/// Library applet info.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct LibraryAppletInfo {
    pub applet_id: AppletId,
    pub library_applet_mode: LibraryAppletMode,
}
const _: () = assert!(core::mem::size_of::<LibraryAppletInfo>() == 0x8);

/// Error code.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct ErrorCode {
    pub category: u32,
    pub number: u32,
}
const _: () = assert!(core::mem::size_of::<ErrorCode>() == 0x8);

/// Error context.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ErrorContext {
    pub error_type: u8,
    pub _padding: [u8; 0x7],
    pub data: [u8; 0x1f4],
    pub result: u32,
}
const _: () = assert!(core::mem::size_of::<ErrorContext>() == 0x200);

impl Default for ErrorContext {
    fn default() -> Self {
        Self {
            error_type: 0,
            _padding: [0u8; 0x7],
            data: [0u8; 0x1f4],
            result: 0,
        }
    }
}

/// ILibraryAppletSelfAccessor service.
pub struct ILibraryAppletSelfAccessor {
    // TODO: Applet, AppletDataBroker references
}

impl ILibraryAppletSelfAccessor {
    pub fn new() -> Self {
        Self {}
    }
}
