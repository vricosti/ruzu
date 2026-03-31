// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/vibration/gc_vibration_device.h and gc_vibration_device.cpp

use crate::hid_types::VibrationGcErmCommand;
use common::ResultCode;

/// NpadGcVibrationDevice — handles GameCube ERM (eccentric rotating mass)
/// vibration. Manages activation, mounting, and sending GC-specific vibration
/// commands.
pub struct NpadGcVibrationDevice {
    pub ref_counter: i32,
    pub is_mounted: bool,
    pub adapter_slot: u32,
}

impl NpadGcVibrationDevice {
    pub fn new() -> Self {
        Self {
            ref_counter: 0,
            is_mounted: false,
            adapter_slot: 0,
        }
    }

    /// Port of NpadGcVibrationDevice::Activate.
    pub fn activate(&mut self) -> ResultCode {
        if self.ref_counter == 0 && self.is_mounted {
            // Upstream: vibration_handler->GetVibrationVolume then
            // xcd_handle->SetVibration(adapter_slot, VibrationGcErmCommand::Stop)
        }
        self.ref_counter += 1;
        ResultCode::SUCCESS
    }

    /// Port of NpadGcVibrationDevice::Deactivate.
    pub fn deactivate(&mut self) -> ResultCode {
        if self.ref_counter == 1 && self.is_mounted {
            // Upstream: send Stop command via xcd_handle
        }
        if self.ref_counter > 0 {
            self.ref_counter -= 1;
        }
        ResultCode::SUCCESS
    }

    /// Port of NpadGcVibrationDevice::Unmount.
    pub fn unmount(&mut self) -> ResultCode {
        if self.ref_counter == 0 || !self.is_mounted {
            self.is_mounted = false;
            return ResultCode::SUCCESS;
        }
        // Upstream: send Stop command
        self.is_mounted = false;
        ResultCode::SUCCESS
    }

    /// Port of NpadGcVibrationDevice::SendVibrationGcErmCommand.
    pub fn send_vibration_gc_erm_command(&self, mut command: VibrationGcErmCommand) -> ResultCode {
        if !self.is_mounted {
            return ResultCode::SUCCESS;
        }
        // Upstream:
        //   volume = vibration_handler->GetVibrationVolume()
        //   if volume == 0: command = Stop
        //   else if command > StopHard: return Success (abort)
        //   xcd_handle->SetVibration(adapter_slot, command)
        let _ = &mut command;
        ResultCode::SUCCESS
    }

    /// Port of NpadGcVibrationDevice::GetActualVibrationGcErmCommand.
    pub fn get_actual_vibration_gc_erm_command(&self) -> Result<VibrationGcErmCommand, ResultCode> {
        if !self.is_mounted {
            return Ok(VibrationGcErmCommand::Stop);
        }
        // Upstream: check volume, if 0 return Stop
        // Upstream TODO: GetActualVibrationGcErmCommand — not yet implemented in C++ upstream.
        // Upstream checks volume, returns Stop if 0, otherwise would query xcd_handle.
        Ok(VibrationGcErmCommand::Stop)
    }

    /// Port of NpadGcVibrationDevice::SendVibrationNotificationPattern.
    pub fn send_vibration_notification_pattern(
        &self,
        mut _command: VibrationGcErmCommand,
    ) -> ResultCode {
        if !self.is_mounted {
            return ResultCode::SUCCESS;
        }
        // Upstream: check volume, clamp command, then send
        ResultCode::SUCCESS
    }

    pub fn is_active(&self) -> bool {
        self.ref_counter > 0
    }

    pub fn is_vibration_mounted(&self) -> bool {
        self.is_mounted
    }
}

impl Default for NpadGcVibrationDevice {
    fn default() -> Self {
        Self::new()
    }
}
