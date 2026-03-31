// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/unique_pad/unique_pad.h and unique_pad.cpp

use crate::resources::controller_base::ControllerActivation;
use crate::resources::shared_memory_format::UniquePadSharedMemoryFormat;

/// UniquePad controller — a mostly-stubbed controller that only writes the
/// unique pad shared memory header each update (same pattern as Digitizer).
pub struct UniquePad {
    pub activation: ControllerActivation,
}

impl UniquePad {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    /// Port of UniquePad::OnUpdate.
    ///
    /// Upstream:
    ///   get active aruid -> AruidData
    ///   header = data->shared_memory_format->unique_pad.header
    ///   header.timestamp = core_timing.GetGlobalTimeNs().count()
    ///   header.total_entry_count = 17
    ///   header.entry_count = 0
    ///   header.last_entry_index = 0
    ///
    /// Note: upstream does NOT lock shared_mutex for UniquePad (unlike other controllers).
    pub fn on_update(
        &mut self,
        shared_memory: &mut UniquePadSharedMemoryFormat,
        timestamp_ns: i64,
    ) {
        shared_memory.header.timestamp = timestamp_ns;
        shared_memory.header.total_entry_count = 17;
        shared_memory.header.entry_count = 0;
        shared_memory.header.last_entry_index = 0;
    }
}

impl Default for UniquePad {
    fn default() -> Self {
        Self::new()
    }
}
