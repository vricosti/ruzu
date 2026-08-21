// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/apm/apm_controller.h
//! Port of zuyu/src/core/hle/service/apm/apm_controller.cpp
//!
//! APM Controller: manages performance mode and configuration.

use std::collections::HashMap;

/// PerformanceConfiguration enum. Upstream: `PerformanceConfiguration` in `apm_controller.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PerformanceConfiguration(u32);

#[allow(non_upper_case_globals)]
impl PerformanceConfiguration {
    pub const Config1: Self = Self(0x00010000);
    pub const Config2: Self = Self(0x00010001);
    pub const Config3: Self = Self(0x00010002);
    pub const Config4: Self = Self(0x00020000);
    pub const Config5: Self = Self(0x00020001);
    pub const Config6: Self = Self(0x00020002);
    pub const Config7: Self = Self(0x00020003);
    pub const Config8: Self = Self(0x00020004);
    pub const Config9: Self = Self(0x00020005);
    pub const Config10: Self = Self(0x00020006);
    pub const Config11: Self = Self(0x92220007);
    pub const Config12: Self = Self(0x92220008);
    pub const Config13: Self = Self(0x92220009);
    pub const Config14: Self = Self(0x9222000A);
    pub const Config15: Self = Self(0x9222000B);
    pub const Config16: Self = Self(0x9222000C);

    pub const fn from_raw(raw: u32) -> Self {
        Self(raw)
    }

    pub const fn raw(self) -> u32 {
        self.0
    }
}

/// CpuBoostMode enum. Upstream: `CpuBoostMode` in `apm_controller.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct CpuBoostMode(u32);

#[allow(non_upper_case_globals)]
impl CpuBoostMode {
    pub const Normal: Self = Self(0);
    pub const FastLoad: Self = Self(1);
    pub const Partial: Self = Self(2);

    pub const fn from_raw(raw: u32) -> Self {
        Self(raw)
    }

    pub const fn raw(self) -> u32 {
        self.0
    }
}

/// PerformanceMode enum. Upstream: `PerformanceMode` in `apm_controller.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PerformanceMode(i32);

#[allow(non_upper_case_globals)]
impl PerformanceMode {
    pub const Invalid: Self = Self(-1);
    pub const Normal: Self = Self(0);
    pub const Boost: Self = Self(1);

    pub const fn from_raw(raw: i32) -> Self {
        Self(raw)
    }

    pub const fn raw(self) -> i32 {
        self.0
    }
}

/// Default performance configuration.
const DEFAULT_PERFORMANCE_CONFIGURATION: PerformanceConfiguration =
    PerformanceConfiguration::Config7;

/// Configuration-to-speed mapping. Upstream: `config_to_speed` in `apm_controller.cpp`.
const CONFIG_TO_SPEED: [(PerformanceConfiguration, u32); 16] = [
    (PerformanceConfiguration::Config1, 1020),
    (PerformanceConfiguration::Config2, 1020),
    (PerformanceConfiguration::Config3, 1224),
    (PerformanceConfiguration::Config4, 1020),
    (PerformanceConfiguration::Config5, 1020),
    (PerformanceConfiguration::Config6, 1224),
    (PerformanceConfiguration::Config7, 1020),
    (PerformanceConfiguration::Config8, 1020),
    (PerformanceConfiguration::Config9, 1020),
    (PerformanceConfiguration::Config10, 1020),
    (PerformanceConfiguration::Config11, 1020),
    (PerformanceConfiguration::Config12, 1020),
    (PerformanceConfiguration::Config13, 1785),
    (PerformanceConfiguration::Config14, 1785),
    (PerformanceConfiguration::Config15, 1020),
    (PerformanceConfiguration::Config16, 1020),
];

/// Boost mode to configuration mapping.
const BOOST_MODE_TO_CONFIG_MAP: [PerformanceConfiguration; 3] = [
    PerformanceConfiguration::Config7,
    PerformanceConfiguration::Config13,
    PerformanceConfiguration::Config15,
];

/// APM Controller. Manages performance mode and configuration state.
///
/// Corresponds to `Controller` class in upstream `apm_controller.h`.
pub struct Controller {
    configs: HashMap<PerformanceMode, PerformanceConfiguration>,
}

impl Controller {
    pub fn new() -> Self {
        let mut configs = HashMap::new();
        configs.insert(PerformanceMode::Normal, DEFAULT_PERFORMANCE_CONFIGURATION);
        configs.insert(PerformanceMode::Boost, DEFAULT_PERFORMANCE_CONFIGURATION);
        Self { configs }
    }

    pub fn set_performance_configuration(
        &mut self,
        mode: PerformanceMode,
        config: PerformanceConfiguration,
    ) {
        let speed = CONFIG_TO_SPEED
            .iter()
            .find(|(c, _)| *c == config)
            .map(|(_, s)| *s);

        if let Some(mhz) = speed {
            self.set_clock_speed(mhz);
            self.configs.insert(mode, config);
        } else {
            log::error!(
                "Invalid performance configuration value provided: {:?}",
                config
            );
        }
    }

    pub fn set_from_cpu_boost_mode(&mut self, mode: CpuBoostMode) {
        if let Some(&config) = BOOST_MODE_TO_CONFIG_MAP.get(mode.raw() as usize) {
            self.set_performance_configuration(PerformanceMode::Boost, config);
        } else {
            log::error!("{:?} invalid mode", mode);
        }
    }

    pub fn get_current_performance_mode(&self) -> PerformanceMode {
        if common::settings::is_docked_mode(&common::settings::values()) {
            PerformanceMode::Boost
        } else {
            PerformanceMode::Normal
        }
    }

    pub fn get_current_performance_configuration(
        &mut self,
        mode: PerformanceMode,
    ) -> PerformanceConfiguration {
        if !self.configs.contains_key(&mode) {
            self.configs.insert(mode, DEFAULT_PERFORMANCE_CONFIGURATION);
        }
        *self.configs.get(&mode).unwrap()
    }

    fn set_clock_speed(&self, mhz: u32) {
        log::debug!("Controller::set_clock_speed called, mhz={:#08x}", mhz);
        // Upstream TODO(DarkLordZach): Actually signal core_timing to change clock speed.
        // Upstream leaves this unimplemented as well — no core_timing integration exists.
    }
}
