// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/hid_core.h and hid_core/hid_core.cpp

use crate::frontend::emulated_console::EmulatedConsole;
use crate::frontend::emulated_controller::EmulatedController;
use crate::frontend::emulated_devices::EmulatedDevices;
use crate::hid_types::*;
use crate::hid_util;

/// Number of emulated controllers
pub const AVAILABLE_CONTROLLERS: usize = 10;

pub struct HIDCore {
    player_1: Box<EmulatedController>,
    player_2: Box<EmulatedController>,
    player_3: Box<EmulatedController>,
    player_4: Box<EmulatedController>,
    player_5: Box<EmulatedController>,
    player_6: Box<EmulatedController>,
    player_7: Box<EmulatedController>,
    player_8: Box<EmulatedController>,
    other: Box<EmulatedController>,
    handheld: Box<EmulatedController>,
    console: Box<EmulatedConsole>,
    devices: Box<EmulatedDevices>,
    supported_style_tag: NpadStyleTag,
    last_active_controller: NpadIdType,
}

impl HIDCore {
    pub fn new() -> Self {
        Self {
            player_1: Box::new(EmulatedController::new(NpadIdType::Player1)),
            player_2: Box::new(EmulatedController::new(NpadIdType::Player2)),
            player_3: Box::new(EmulatedController::new(NpadIdType::Player3)),
            player_4: Box::new(EmulatedController::new(NpadIdType::Player4)),
            player_5: Box::new(EmulatedController::new(NpadIdType::Player5)),
            player_6: Box::new(EmulatedController::new(NpadIdType::Player6)),
            player_7: Box::new(EmulatedController::new(NpadIdType::Player7)),
            player_8: Box::new(EmulatedController::new(NpadIdType::Player8)),
            other: Box::new(EmulatedController::new(NpadIdType::Other)),
            handheld: Box::new(EmulatedController::new(NpadIdType::Handheld)),
            console: Box::new(EmulatedConsole::new()),
            devices: Box::new(EmulatedDevices::new()),
            supported_style_tag: NpadStyleTag {
                raw: NpadStyleSet::ALL,
            },
            last_active_controller: NpadIdType::Handheld,
        }
    }

    pub fn get_emulated_controller(&self, npad_id_type: NpadIdType) -> &EmulatedController {
        match npad_id_type {
            NpadIdType::Player1 => &self.player_1,
            NpadIdType::Player2 => &self.player_2,
            NpadIdType::Player3 => &self.player_3,
            NpadIdType::Player4 => &self.player_4,
            NpadIdType::Player5 => &self.player_5,
            NpadIdType::Player6 => &self.player_6,
            NpadIdType::Player7 => &self.player_7,
            NpadIdType::Player8 => &self.player_8,
            NpadIdType::Other => &self.other,
            NpadIdType::Handheld => &self.handheld,
            _ => panic!("Invalid NpadIdType={:?}", npad_id_type),
        }
    }

    pub fn get_emulated_controller_mut(
        &mut self,
        npad_id_type: NpadIdType,
    ) -> &mut EmulatedController {
        match npad_id_type {
            NpadIdType::Player1 => &mut self.player_1,
            NpadIdType::Player2 => &mut self.player_2,
            NpadIdType::Player3 => &mut self.player_3,
            NpadIdType::Player4 => &mut self.player_4,
            NpadIdType::Player5 => &mut self.player_5,
            NpadIdType::Player6 => &mut self.player_6,
            NpadIdType::Player7 => &mut self.player_7,
            NpadIdType::Player8 => &mut self.player_8,
            NpadIdType::Other => &mut self.other,
            NpadIdType::Handheld => &mut self.handheld,
            _ => panic!("Invalid NpadIdType={:?}", npad_id_type),
        }
    }

    pub fn get_emulated_controller_by_index(&self, index: usize) -> &EmulatedController {
        self.get_emulated_controller(hid_util::index_to_npad_id_type(index))
    }

    pub fn get_emulated_controller_by_index_mut(
        &mut self,
        index: usize,
    ) -> &mut EmulatedController {
        self.get_emulated_controller_mut(hid_util::index_to_npad_id_type(index))
    }

    pub fn get_emulated_console(&self) -> &EmulatedConsole {
        &self.console
    }

    pub fn get_emulated_console_mut(&mut self) -> &mut EmulatedConsole {
        &mut self.console
    }

    pub fn get_emulated_devices(&self) -> &EmulatedDevices {
        &self.devices
    }

    pub fn get_emulated_devices_mut(&mut self) -> &mut EmulatedDevices {
        &mut self.devices
    }

    pub fn set_supported_style_tag(&mut self, style_tag: NpadStyleTag) {
        self.supported_style_tag.raw = style_tag.raw;
        self.player_1.set_supported_npad_style_tag(self.supported_style_tag);
        self.player_2.set_supported_npad_style_tag(self.supported_style_tag);
        self.player_3.set_supported_npad_style_tag(self.supported_style_tag);
        self.player_4.set_supported_npad_style_tag(self.supported_style_tag);
        self.player_5.set_supported_npad_style_tag(self.supported_style_tag);
        self.player_6.set_supported_npad_style_tag(self.supported_style_tag);
        self.player_7.set_supported_npad_style_tag(self.supported_style_tag);
        self.player_8.set_supported_npad_style_tag(self.supported_style_tag);
        self.other.set_supported_npad_style_tag(self.supported_style_tag);
        self.handheld.set_supported_npad_style_tag(self.supported_style_tag);
    }

    pub fn get_supported_style_tag(&self) -> NpadStyleTag {
        self.supported_style_tag
    }

    /// Counts the connected players from P1-P8
    pub fn get_player_count(&self) -> i8 {
        let mut active_players: i8 = 0;
        for player_index in 0..(AVAILABLE_CONTROLLERS - 2) {
            let controller = self.get_emulated_controller_by_index(player_index);
            if controller.is_connected() {
                active_players += 1;
            }
        }
        active_players
    }

    /// Returns the first connected npad id
    pub fn get_first_npad_id(&self) -> NpadIdType {
        for player_index in 0..AVAILABLE_CONTROLLERS {
            let controller = self.get_emulated_controller_by_index(player_index);
            if controller.is_connected() {
                return controller.get_npad_id_type();
            }
        }
        NpadIdType::Player1
    }

    /// Returns the first disconnected npad id
    pub fn get_first_disconnected_npad_id(&self) -> NpadIdType {
        for player_index in 0..AVAILABLE_CONTROLLERS {
            let controller = self.get_emulated_controller_by_index(player_index);
            if !controller.is_connected() {
                return controller.get_npad_id_type();
            }
        }
        NpadIdType::Player1
    }

    pub fn set_last_active_controller(&mut self, npad_id: NpadIdType) {
        self.last_active_controller = npad_id;
    }

    pub fn get_last_active_controller(&self) -> NpadIdType {
        self.last_active_controller
    }

    pub fn enable_all_controller_configuration(&mut self) {
        self.player_1.enable_configuration();
        self.player_2.enable_configuration();
        self.player_3.enable_configuration();
        self.player_4.enable_configuration();
        self.player_5.enable_configuration();
        self.player_6.enable_configuration();
        self.player_7.enable_configuration();
        self.player_8.enable_configuration();
        self.other.enable_configuration();
        self.handheld.enable_configuration();
    }

    pub fn disable_all_controller_configuration(&mut self) {
        self.player_1.disable_configuration();
        self.player_2.disable_configuration();
        self.player_3.disable_configuration();
        self.player_4.disable_configuration();
        self.player_5.disable_configuration();
        self.player_6.disable_configuration();
        self.player_7.disable_configuration();
        self.player_8.disable_configuration();
        self.other.disable_configuration();
        self.handheld.disable_configuration();
    }

    pub fn reload_input_devices(&mut self) {
        self.player_1.reload_from_settings();
        self.player_2.reload_from_settings();
        self.player_3.reload_from_settings();
        self.player_4.reload_from_settings();
        self.player_5.reload_from_settings();
        self.player_6.reload_from_settings();
        self.player_7.reload_from_settings();
        self.player_8.reload_from_settings();
        self.other.reload_from_settings();
        self.handheld.reload_from_settings();
        self.console.reload_from_settings();
        self.devices.reload_from_settings();
    }

    pub fn unload_input_devices(&mut self) {
        self.player_1.unload_input();
        self.player_2.unload_input();
        self.player_3.unload_input();
        self.player_4.unload_input();
        self.player_5.unload_input();
        self.player_6.unload_input();
        self.player_7.unload_input();
        self.player_8.unload_input();
        self.other.unload_input();
        self.handheld.unload_input();
        self.console.unload_input();
        self.devices.unload_input();
    }
}

impl Default for HIDCore {
    fn default() -> Self {
        Self::new()
    }
}
