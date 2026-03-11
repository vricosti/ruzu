// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/frontend/applets/controller.h and controller.cpp
//! Controller configuration applet interface.

use super::applet::Applet;

/// Corresponds to upstream `BorderColor` (std::array<u8, 4>).
pub type BorderColor = [u8; 4];

/// Corresponds to upstream `ExplainText` (std::array<char, 0x81>).
pub type ExplainText = [u8; 0x81];

/// Parameters for the controller applet.
///
/// Corresponds to upstream `Core::Frontend::ControllerParameters`.
#[derive(Debug, Clone)]
pub struct ControllerParameters {
    pub min_players: i8,
    pub max_players: i8,
    pub keep_controllers_connected: bool,
    pub enable_single_mode: bool,
    pub enable_border_color: bool,
    pub border_colors: Vec<BorderColor>,
    pub enable_explain_text: bool,
    pub explain_text: Vec<ExplainText>,
    pub allow_pro_controller: bool,
    pub allow_handheld: bool,
    pub allow_dual_joycons: bool,
    pub allow_left_joycon: bool,
    pub allow_right_joycon: bool,
    pub allow_gamecube_controller: bool,
}

impl Default for ControllerParameters {
    fn default() -> Self {
        Self {
            min_players: 0,
            max_players: 0,
            keep_controllers_connected: false,
            enable_single_mode: false,
            enable_border_color: false,
            border_colors: Vec::new(),
            enable_explain_text: false,
            explain_text: Vec::new(),
            allow_pro_controller: false,
            allow_handheld: false,
            allow_dual_joycons: false,
            allow_left_joycon: false,
            allow_right_joycon: false,
            allow_gamecube_controller: false,
        }
    }
}

/// Callback type for controller reconfiguration results.
///
/// Corresponds to upstream `ControllerApplet::ReconfigureCallback`.
pub type ReconfigureCallback = Box<dyn FnOnce(bool) + Send>;

/// Controller applet trait.
///
/// Corresponds to upstream `Core::Frontend::ControllerApplet`.
pub trait ControllerApplet: Applet {
    fn reconfigure_controllers(
        &self,
        callback: ReconfigureCallback,
        parameters: &ControllerParameters,
    );
}

/// Default (stub) controller applet implementation.
///
/// Corresponds to upstream `Core::Frontend::DefaultControllerApplet`.
/// NOTE: Upstream holds a reference to HIDCore and performs actual controller
/// configuration logic. This stub simply calls the callback with `true`.
pub struct DefaultControllerApplet;

impl Applet for DefaultControllerApplet {
    fn close(&self) {}
}

impl ControllerApplet for DefaultControllerApplet {
    fn reconfigure_controllers(
        &self,
        callback: ReconfigureCallback,
        parameters: &ControllerParameters,
    ) {
        log::info!(
            "called, deducing the best configuration based on the given parameters!"
        );

        let min_supported_players = if parameters.enable_single_mode {
            1
        } else {
            parameters.min_players as usize
        };

        // TODO: Upstream disconnects handheld, then iterates over available controllers,
        // connecting the minimum required players with the highest-priority controller type.
        // This requires HIDCore integration.
        // For now, just log and accept.
        let _ = min_supported_players;

        callback(true);
    }
}
