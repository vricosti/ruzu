// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/helpers/joycon_protocol/ringcon.h` and `ringcon.cpp`.
//!
//! Based on dkms-hid-nintendo implementation, CTCaer joycon toolkit and dekuNukem reverse
//! engineering.
//!
//! Ring-Con controller protocol implementation.

use common::input::DriverResult;

use super::common_protocol::JoyconCommonProtocol;

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
    pub fn enable_ring_con(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of RingConProtocol::DisableRingCon
    pub fn disable_ring_con(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of RingConProtocol::StartRingconPolling
    pub fn start_ringcon_polling(&mut self) -> DriverResult {
        todo!()
    }

    /// Port of RingConProtocol::IsEnabled
    pub fn is_enabled(&self) -> bool {
        self.is_enabled
    }

    // ---- Private methods ----

    fn is_ring_connected(&mut self, _is_connected: &mut bool) -> DriverResult {
        todo!()
    }

    fn configure_ring(&mut self) -> DriverResult {
        todo!()
    }
}
