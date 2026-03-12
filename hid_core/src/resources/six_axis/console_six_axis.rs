// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/six_axis/console_six_axis.h and console_six_axis.cpp

use crate::resources::controller_base::ControllerActivation;

/// ConsoleSixAxis controller — reads console motion data from EmulatedConsole
/// and writes into ConsoleSixAxisSensorSharedMemoryFormat.
pub struct ConsoleSixAxis {
    pub activation: ControllerActivation,
}

impl ConsoleSixAxis {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    /// Port of ConsoleSixAxis::OnUpdate.
    ///
    /// Upstream logic:
    ///   lock shared_mutex
    ///   get active aruid -> AruidData
    ///   shared_memory = data->shared_memory_format->console
    ///   if not activated: return
    ///   motion_status = console->GetMotion()
    ///   shared_memory.sampling_number++
    ///   shared_memory.is_seven_six_axis_sensor_at_rest = motion_status.is_at_rest
    ///   shared_memory.verticalization_error = motion_status.verticalization_error
    ///   shared_memory.gyro_bias = motion_status.gyro_bias
    pub fn on_update(&mut self) {
        // Requires emulated console and shared memory wiring.
    }
}

impl Default for ConsoleSixAxis {
    fn default() -> Self {
        Self::new()
    }
}
