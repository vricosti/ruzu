// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/ringcon.h` and `ringcon.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! Ring-Con controller protocol implementation.

use common::input::DriverResult;

use super::common_protocol::{JoyconCommonProtocol, ScopedSetBlocking};
use super::joycon_types::{ReportMode, SubCommand};

/// Port of `RingConProtocol` class from ringcon.h / ringcon.cpp
pub struct RingConProtocol {
    protocol: JoyconCommonProtocol,
    is_enabled: bool,
}

impl RingConProtocol {
    /// Port of RingConProtocol::RingConProtocol
    pub fn new() -> Self {
        Self {
            protocol: JoyconCommonProtocol::new(),
            is_enabled: false,
        }
    }

    /// Port of RingConProtocol::EnableRingCon
    ///
    /// Sequence: SetReportMode(STANDARD_FULL_60HZ) -> EnableMCU(true) ->
    /// ConfigureMCU(Standby mode).
    pub fn enable_ring_con(&mut self) -> DriverResult {
        log::debug!("enable_ring_con: enabling Ring-Con");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;

        if result == DriverResult::Success {
            result = self.protocol.set_report_mode(ReportMode::StandardFull60Hz);
        }
        if result == DriverResult::Success {
            result = self.protocol.enable_mcu(true);
        }
        if result == DriverResult::Success {
            // ConfigureMCU(Standby): upstream passes MCUConfig{ConfigureMCU, SetDeviceMode, Standby}
            // configure_mcu() currently stubs to NotSupported; matches the upstream flow structurally.
            result = self.protocol.configure_mcu();
        }

        result
    }

    /// Port of RingConProtocol::DisableRingCon
    pub fn disable_ring_con(&mut self) -> DriverResult {
        log::debug!("disable_ring_con: disabling Ring-Con");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;

        if result == DriverResult::Success {
            result = self.protocol.enable_mcu(false);
        }

        self.is_enabled = false;
        result
    }

    /// Port of RingConProtocol::StartRingconPolling
    ///
    /// Checks if the Ring-Con is connected, then configures it.
    pub fn start_ringcon_polling(&mut self) -> DriverResult {
        log::debug!("start_ringcon_polling: enabling Ring-Con");
        let _sb = ScopedSetBlocking::new(&mut self.protocol);
        let mut result = DriverResult::Success;
        let mut is_connected = false;

        if result == DriverResult::Success {
            result = self.is_ring_connected(&mut is_connected);
        }
        if result == DriverResult::Success && is_connected {
            log::info!("start_ringcon_polling: Ring-Con detected");
            result = self.configure_ring();
        }
        if result == DriverResult::Success {
            self.is_enabled = true;
        }

        result
    }

    /// Port of RingConProtocol::IsEnabled
    pub fn is_enabled(&self) -> bool {
        self.is_enabled
    }

    // ---- Private methods ----

    /// Port of RingConProtocol::IsRingConnected
    ///
    /// Polls GET_EXTERNAL_DEVICE_INFO subcommand until the response shows
    /// ExternalDeviceId::RingController. Depends on the hidapi handle.
    fn is_ring_connected(&mut self, is_connected: &mut bool) -> DriverResult {
        log::debug!("is_ring_connected: checking for Ring-Con");
        *is_connected = false;
        // Upstream sends GET_EXTERNAL_DEVICE_INFO up to 42 times and checks
        // output.external_device_id == RingController. Depends on the hidapi handle.
        log::warn!("is_ring_connected: no hidapi handle available");
        DriverResult::NoDeviceDetected
    }

    /// Port of RingConProtocol::ConfigureRing
    ///
    /// Sends SET_EXTERNAL_FORMAT_CONFIG followed by ENABLE_EXTERNAL_POLLING with
    /// specific configuration bytes.
    fn configure_ring(&mut self) -> DriverResult {
        log::debug!("configure_ring: configuring Ring-Con");

        const RING_CONFIG: [u8; 37] = [
            0x06, 0x03, 0x25, 0x06, 0x00, 0x00, 0x00, 0x00, 0x1C, 0x16, 0xED, 0x34, 0x36, 0x00,
            0x00, 0x00, 0x0A, 0x64, 0x0B, 0xE6, 0xA9, 0x22, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x90, 0xA8, 0xE1, 0x34, 0x36,
        ];

        let result = self
            .protocol
            .send_sub_command(SubCommand::SetExternalFormatConfig, &RING_CONFIG);

        if result != DriverResult::Success {
            return result;
        }

        const RINGCON_DATA: [u8; 4] = [0x04, 0x01, 0x01, 0x02];
        self.protocol
            .send_sub_command(SubCommand::EnableExternalPolling, &RINGCON_DATA)
    }
}
