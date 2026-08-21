// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/apm/apm_controller.h
//! Port of zuyu/src/core/hle/service/apm/apm_controller.cpp
//!
//! APM Controller: manages performance mode and configuration.

use std::collections::HashMap;

/// PerformanceConfiguration enum. Upstream: `PerformanceConfiguration` in `apm_controller.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum PerformanceConfiguration {
    Config1 = 0x00010000,
    Config2 = 0x00010001,
    Config3 = 0x00010002,
    Config4 = 0x00020000,
    Config5 = 0x00020001,
    Config6 = 0x00020002,
    Config7 = 0x00020003,
    Config8 = 0x00020004,
    Config9 = 0x00020005,
    Config10 = 0x00020006,
    Config11 = 0x92220007,
    Config12 = 0x92220008,
    Config13 = 0x92220009,
    Config14 = 0x9222000A,
    Config15 = 0x9222000B,
    Config16 = 0x9222000C,
}

/// CpuBoostMode enum. Upstream: `CpuBoostMode` in `apm_controller.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum CpuBoostMode {
    Normal = 0,
    FastLoad = 1,
    Partial = 2,
}

/// PerformanceMode enum. Upstream: `PerformanceMode` in `apm_controller.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(i32)]
pub enum PerformanceMode {
    Invalid = -1,
    Normal = 0,
    Boost = 1,
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
    configs: HashMap<i32, PerformanceConfiguration>,
}

impl Controller {
    pub fn new() -> Self {
        let mut configs = HashMap::new();
        configs.insert(
            PerformanceMode::Normal as i32,
            DEFAULT_PERFORMANCE_CONFIGURATION,
        );
        configs.insert(
            PerformanceMode::Boost as i32,
            DEFAULT_PERFORMANCE_CONFIGURATION,
        );
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
            self.configs.insert(mode as i32, config);
        } else {
            log::error!(
                "Invalid performance configuration value provided: {:?}",
                config
            );
        }
    }

    pub fn set_from_cpu_boost_mode(&mut self, mode: CpuBoostMode) {
        let config = BOOST_MODE_TO_CONFIG_MAP[mode as usize];
        self.set_performance_configuration(PerformanceMode::Boost, config);
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
        let key = mode as i32;
        if !self.configs.contains_key(&key) {
            self.configs.insert(key, DEFAULT_PERFORMANCE_CONFIGURATION);
        }
        *self.configs.get(&key).unwrap()
    }

    fn set_clock_speed(&self, mhz: u32) {
        log::debug!("Controller::set_clock_speed called, mhz={:#08x}", mhz);
        // Upstream TODO(DarkLordZach): Actually signal core_timing to change clock speed.
        // Upstream leaves this unimplemented as well — no core_timing integration exists.
    }
}
