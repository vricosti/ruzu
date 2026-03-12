// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/six_axis/seven_six_axis.h and seven_six_axis.cpp

use crate::hid_types::Vec3f;
use crate::resources::controller_base::ControllerActivation;
use crate::resources::ring_lifo::Lifo;

/// Internal state for the seven-six-axis sensor.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct SevenSixAxisState {
    pub _unused: [u32; 2],
    pub timestamp: u64,
    pub sampling_number: u64,
    pub unknown: u64,
    pub accel: Vec3f,
    pub gyro: Vec3f,
    /// Quaternion: [x, y, z, w]
    pub quaternion: [f32; 4],
}
const _: () = assert!(std::mem::size_of::<SevenSixAxisState>() == 0x48);

/// SevenSixAxis controller — reads console motion from EmulatedConsole and
/// writes a LIFO buffer to transfer memory. Used for
/// InitializeSevenSixAxisSensor / ResetSevenSixAxisSensorTimestamp.
pub struct SevenSixAxis {
    pub activation: ControllerActivation,
    seven_sixaxis_lifo: Lifo<SevenSixAxisState, 0x21>,
    last_saved_timestamp: u64,
    last_global_timestamp: u64,
    next_seven_sixaxis_state: SevenSixAxisState,
    transfer_memory: u64,
}

impl SevenSixAxis {
    pub fn new() -> Self {
        Self {
            activation: ControllerActivation::new(),
            seven_sixaxis_lifo: Lifo::default(),
            last_saved_timestamp: 0,
            last_global_timestamp: 0,
            next_seven_sixaxis_state: SevenSixAxisState::default(),
            transfer_memory: 0,
        }
    }

    pub fn on_init(&mut self) {}
    pub fn on_release(&mut self) {}

    /// Port of SevenSixAxis::OnUpdate.
    ///
    /// Upstream logic:
    ///   if not activated or transfer_memory == 0: clear lifo, return
    ///   last_entry = seven_sixaxis_lifo.ReadCurrentEntry().state
    ///   next.sampling_number = last_entry.sampling_number + 1
    ///   motion_status = console->GetMotion()
    ///   last_global_timestamp = core_timing.GetGlobalTimeNs().count()
    ///   next.unknown = 1
    ///   next.timestamp = last_global_timestamp - last_saved_timestamp
    ///   next.accel = motion_status.accel
    ///   next.gyro = motion_status.gyro
    ///   next.quaternion = swizzled(motion_status.quaternion)
    ///   seven_sixaxis_lifo.WriteNextEntry(next)
    ///   system.ApplicationMemory().WriteBlock(transfer_memory, &lifo, sizeof(lifo))
    pub fn on_update(&mut self) {
        if !self.activation.is_controller_activated() || self.transfer_memory == 0 {
            self.seven_sixaxis_lifo.buffer_count = 0;
            self.seven_sixaxis_lifo.buffer_tail = 0;
            return;
        }
        // Full update requires system/console integration.
        let _ = &self.next_seven_sixaxis_state;
        let _ = self.last_global_timestamp;
        let _ = self.last_saved_timestamp;
    }

    /// Called on InitializeSevenSixAxisSensor
    pub fn set_transfer_memory_address(&mut self, t_mem: u64) {
        self.transfer_memory = t_mem;
    }

    /// Called on ResetSevenSixAxisSensorTimestamp
    pub fn reset_timestamp(&mut self) {
        self.last_saved_timestamp = self.last_global_timestamp;
    }
}

impl Default for SevenSixAxis {
    fn default() -> Self {
        Self::new()
    }
}
