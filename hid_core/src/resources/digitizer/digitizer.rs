// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/digitizer/digitizer.h and digitizer.cpp

use crate::resources::controller_base::ControllerActivation;

/// Digitizer controller — a mostly-stubbed controller that only writes the
/// digitizer shared memory header each update.
pub struct Digitizer {
    pub activation: ControllerActivation,
}

impl Digitizer {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    /// Port of Digitizer::OnUpdate.
    ///
    /// Upstream logic:
    ///   lock shared_mutex
    ///   get active aruid -> AruidData
    ///   header = data->shared_memory_format->digitizer.header
    ///   header.timestamp = core_timing.GetGlobalTimeNs().count()
    ///   header.total_entry_count = 17
    ///   header.entry_count = 0
    ///   header.last_entry_index = 0
    pub fn on_update(&mut self) {
        // Requires shared memory wiring to write timestamp/counts.
    }
}

impl Default for Digitizer {
    fn default() -> Self {
        Self::new()
    }
}
