// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/control.h` and `control.cpp`.
//!
//! Host1x control channel — processes syncpoint wait methods.

use crate::host1x::syncpoint_manager::SyncpointManager;
use log::trace;
use std::sync::Arc;

/// Control methods for the Host1x control channel.
///
/// Port of `Tegra::Host1x::Control::Method`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Method {
    WaitSyncpt = 0x8,
    LoadSyncptPayload32 = 0x4e,
    WaitSyncpt32 = 0x50,
}

impl Method {
    pub fn from_u32(value: u32) -> Option<Self> {
        match value {
            0x8 => Some(Method::WaitSyncpt),
            0x4e => Some(Method::LoadSyncptPayload32),
            0x50 => Some(Method::WaitSyncpt32),
            _ => None,
        }
    }
}

/// Host1x control channel processor.
///
/// Port of `Tegra::Host1x::Control`.
pub struct Control {
    syncpoint_manager: Arc<SyncpointManager>,
    syncpoint_value: u32,
}

impl Control {
    pub fn new(syncpoint_manager: Arc<SyncpointManager>) -> Self {
        Self {
            syncpoint_manager,
            syncpoint_value: 0,
        }
    }

    /// Writes the method into the state; invokes Execute() if encountered.
    ///
    /// Port of `Control::ProcessMethod`.
    pub fn process_method(&mut self, method: Method, argument: u32) {
        match method {
            Method::LoadSyncptPayload32 => {
                self.syncpoint_value = argument;
            }
            Method::WaitSyncpt | Method::WaitSyncpt32 => {
                self.execute(argument);
            }
        }
    }

    /// For Host1x, execute is waiting on a syncpoint previously written into the state.
    ///
    /// Port of `Control::Execute`.
    fn execute(&self, data: u32) {
        trace!(
            "Control wait syncpt {} value {}",
            data,
            self.syncpoint_value
        );
        self.syncpoint_manager.wait_host(data, self.syncpoint_value);
    }
}
