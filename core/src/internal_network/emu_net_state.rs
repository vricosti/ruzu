// SPDX-FileCopyrightText: Copyright 2026 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `core/internal_network/emu_net_state.{h,cpp}`.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{LazyLock, Mutex, MutexGuard};

use super::network::{translate_ipv4, IPv4Address};
use super::network_interface::{get_selected_network_interface, HostAdapterKind};

#[derive(Debug, Clone)]
pub struct EmuNetStateData {
    pub connected: bool,
    pub via_wifi: bool,
    pub ssid: [u8; 20],
    pub bars: u8,
    pub secure: bool,
    pub ip: IPv4Address,
    pub mask: IPv4Address,
    pub gw: IPv4Address,
}

impl Default for EmuNetStateData {
    fn default() -> Self {
        Self {
            connected: false,
            via_wifi: false,
            ssid: [0; 20],
            bars: 0,
            secure: false,
            ip: [0; 4],
            mask: [0; 4],
            gw: [0; 4],
        }
    }
}

pub struct EmuNetState {
    pub wifi_enabled: AtomicBool,
    pub ethernet_enabled: AtomicBool,
    data: Mutex<EmuNetStateData>,
}

impl EmuNetState {
    fn new() -> Self {
        Self {
            wifi_enabled: AtomicBool::new(true),
            ethernet_enabled: AtomicBool::new(true),
            data: Mutex::new(EmuNetStateData::default()),
        }
    }

    pub fn get() -> &'static Self {
        static INSTANCE: LazyLock<EmuNetState> = LazyLock::new(EmuNetState::new);
        &INSTANCE
    }

    pub fn lock(&self) -> MutexGuard<'_, EmuNetStateData> {
        self.data.lock().expect("emulated network state poisoned")
    }
}

pub fn quality_to_bars(quality: u8) -> u8 {
    if quality == 0 {
        0
    } else if quality < 34 {
        1
    } else if quality < 67 {
        2
    } else {
        3
    }
}

pub fn refresh_from_host() {
    let state = EmuNetState::get();
    let mut data = state.lock();
    let Some(selected) = get_selected_network_interface() else {
        data.connected = false;
        data.via_wifi = false;
        state.wifi_enabled.store(false, Ordering::Relaxed);
        state.ethernet_enabled.store(false, Ordering::Relaxed);
        data.ssid.fill(0);
        data.secure = false;
        data.bars = 0;
        return;
    };

    state.wifi_enabled.store(
        !*common::settings::values().airplane_mode.get_value(),
        Ordering::Relaxed,
    );
    state.ethernet_enabled.store(
        selected.kind == HostAdapterKind::Ethernet,
        Ordering::Relaxed,
    );
    data.connected = true;
    data.via_wifi = selected.kind == HostAdapterKind::Wifi;
    data.ssid.fill(0);
    let name = selected.name.as_bytes();
    let length = name.len().min(data.ssid.len() - 1);
    data.ssid[..length].copy_from_slice(&name[..length]);
    data.secure = true;
    data.ip = translate_ipv4(selected.ip_address);
    data.mask = translate_ipv4(selected.subnet_mask);
    data.gw = translate_ipv4(selected.gateway);

    #[cfg(target_os = "windows")]
    {
        // Windows WLAN SSID/signal querying remains platform glue; keep Eden's
        // Ethernet result and a conservative Wi-Fi signal until that API is
        // available in the Rust platform layer.
        data.bars = if data.via_wifi { 2 } else { 3 };
    }
    #[cfg(not(target_os = "windows"))]
    {
        data.bars = if data.via_wifi { 2 } else { 3 };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn quality_thresholds_match_eden() {
        assert_eq!(quality_to_bars(0), 0);
        assert_eq!(quality_to_bars(1), 1);
        assert_eq!(quality_to_bars(33), 1);
        assert_eq!(quality_to_bars(34), 2);
        assert_eq!(quality_to_bars(66), 2);
        assert_eq!(quality_to_bars(67), 3);
        assert_eq!(quality_to_bars(u8::MAX), 3);
    }
}
