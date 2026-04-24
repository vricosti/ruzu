// SPDX-FileCopyrightText: 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! SDL2 configuration.
//!
//! Port of `yuzu_cmd/sdl_config.h` and `yuzu_cmd/sdl_config.cpp`.
//!
//! `SdlConfig` is the concrete `Config` implementation for the SDL2
//! command-line frontend. It extends the base `Config` infrastructure from
//! `frontend_common` with SDL2-specific default key bindings.
//!
//! # Default key bindings
//!
//! The constants below mirror the C++ static arrays declared in `sdl_config.h`
//! and defined in `sdl_config.cpp`. They map logical controller inputs to
//! SDL scancode integers. The SDL scancode values come from `sdl2::keyboard::Scancode`.
//!
//! | C++ constant                     | Rust constant                      |
//! |----------------------------------|------------------------------------|
//! | `SdlConfig::default_buttons`     | `DEFAULT_BUTTONS`                  |
//! | `SdlConfig::default_motions`     | `DEFAULT_MOTIONS`                  |
//! | `SdlConfig::default_analogs`     | `DEFAULT_ANALOGS`                  |
//! | `SdlConfig::default_stick_mod`   | `DEFAULT_STICK_MOD`                |
//! | `SdlConfig::default_ringcon_analogs` | `DEFAULT_RINGCON_ANALOGS`      |

// ---------------------------------------------------------------------------
// Upstream numeric scancode constants
//
// These values are SDL_Scancode integers copied from SDL headers.
// They are defined here because they form the default key bindings that
// belong in this file (upstream `sdl_config.cpp`).
// ---------------------------------------------------------------------------

/// SDL_SCANCODE_A
const SDL_SCANCODE_A: i32 = 4;
/// SDL_SCANCODE_S
const SDL_SCANCODE_S: i32 = 22;
/// SDL_SCANCODE_Z
const SDL_SCANCODE_Z: i32 = 29;
/// SDL_SCANCODE_X
const SDL_SCANCODE_X: i32 = 27;
/// SDL_SCANCODE_T
const SDL_SCANCODE_T: i32 = 23;
/// SDL_SCANCODE_G
const SDL_SCANCODE_G: i32 = 10;
/// SDL_SCANCODE_F
const SDL_SCANCODE_F: i32 = 9;
/// SDL_SCANCODE_H
const SDL_SCANCODE_H: i32 = 11;
/// SDL_SCANCODE_Q
const SDL_SCANCODE_Q: i32 = 20;
/// SDL_SCANCODE_W
const SDL_SCANCODE_W: i32 = 26;
/// SDL_SCANCODE_M
const SDL_SCANCODE_M: i32 = 16;
/// SDL_SCANCODE_N
const SDL_SCANCODE_N: i32 = 17;
/// SDL_SCANCODE_1
const SDL_SCANCODE_1: i32 = 30;
/// SDL_SCANCODE_2
const SDL_SCANCODE_2: i32 = 31;
/// SDL_SCANCODE_B
const SDL_SCANCODE_B: i32 = 5;
/// SDL_SCANCODE_7
const SDL_SCANCODE_7: i32 = 36;
/// SDL_SCANCODE_8
const SDL_SCANCODE_8: i32 = 37;
/// SDL_SCANCODE_UP
const SDL_SCANCODE_UP: i32 = 82;
/// SDL_SCANCODE_DOWN
const SDL_SCANCODE_DOWN: i32 = 81;
/// SDL_SCANCODE_LEFT
const SDL_SCANCODE_LEFT: i32 = 80;
/// SDL_SCANCODE_RIGHT
const SDL_SCANCODE_RIGHT: i32 = 79;
/// SDL_SCANCODE_I
const SDL_SCANCODE_I: i32 = 12;
/// SDL_SCANCODE_K
const SDL_SCANCODE_K: i32 = 14;
/// SDL_SCANCODE_J
const SDL_SCANCODE_J: i32 = 13;
/// SDL_SCANCODE_L
const SDL_SCANCODE_L: i32 = 15;
/// SDL_SCANCODE_D
const SDL_SCANCODE_D: i32 = 7;

// ---------------------------------------------------------------------------
// NativeButton / NativeAnalog / NativeMotion button counts
//
// These mirror the upstream `Settings::NativeButton::NumButtons` etc.
// counts. They are used to size the default binding arrays and must stay
// in sync with the settings crate when that is ported.
// ---------------------------------------------------------------------------

/// Number of native buttons. Maps to `Settings::NativeButton::NumButtons`.
pub const NUM_BUTTONS: usize = 15;

/// Number of native analogs. Maps to `Settings::NativeAnalog::NumAnalogs`.
pub const NUM_ANALOGS: usize = 2;

/// Number of native motions. Maps to `Settings::NativeMotion::NumMotions`.
pub const NUM_MOTIONS: usize = 2;

// ---------------------------------------------------------------------------
// Default key binding constants
//
// Maps to C++ static const arrays defined in `sdl_config.cpp`.
// ---------------------------------------------------------------------------

/// Default SDL scancode bindings for each NativeButton.
///
/// Maps to C++ `SdlConfig::default_buttons`.
pub const DEFAULT_BUTTONS: [i32; NUM_BUTTONS] = [
    SDL_SCANCODE_A,
    SDL_SCANCODE_S,
    SDL_SCANCODE_Z,
    SDL_SCANCODE_X,
    SDL_SCANCODE_T,
    SDL_SCANCODE_G,
    SDL_SCANCODE_F,
    SDL_SCANCODE_H,
    SDL_SCANCODE_Q,
    SDL_SCANCODE_W,
    SDL_SCANCODE_M,
    SDL_SCANCODE_N,
    SDL_SCANCODE_1,
    SDL_SCANCODE_2,
    SDL_SCANCODE_B,
];

/// Default SDL scancode bindings for each NativeMotion.
///
/// Maps to C++ `SdlConfig::default_motions`.
pub const DEFAULT_MOTIONS: [i32; NUM_MOTIONS] = [SDL_SCANCODE_7, SDL_SCANCODE_8];

/// Default SDL scancode bindings for each NativeAnalog (4 keys per axis:
/// up, down, left, right).
///
/// Maps to C++ `SdlConfig::default_analogs`.
pub const DEFAULT_ANALOGS: [[i32; 4]; NUM_ANALOGS] = [
    [
        SDL_SCANCODE_UP,
        SDL_SCANCODE_DOWN,
        SDL_SCANCODE_LEFT,
        SDL_SCANCODE_RIGHT,
    ],
    [
        SDL_SCANCODE_I,
        SDL_SCANCODE_K,
        SDL_SCANCODE_J,
        SDL_SCANCODE_L,
    ],
];

/// Default stick modifier keys (one per analog).
///
/// Maps to C++ `SdlConfig::default_stick_mod`.
pub const DEFAULT_STICK_MOD: [i32; 2] = [SDL_SCANCODE_D, 0];

/// Default ring-controller analog bindings.
///
/// Maps to C++ `SdlConfig::default_ringcon_analogs`.
pub const DEFAULT_RINGCON_ANALOGS: [i32; 2] = [0, 0];

// ---------------------------------------------------------------------------
// SdlConfig
// ---------------------------------------------------------------------------

/// SDL2 frontend configuration.
///
/// Maps to C++ class `SdlConfig` in `yuzu_cmd/sdl_config.h`.
///
/// Inherits from `Config` (via trait in `frontend_common::config`).
///
/// Note: The base `Config` infrastructure (INI read/write, BeginGroup/EndGroup,
/// ReadStringSetting/WriteStringSetting, Initialize/Reload/SaveValues/WriteToIni)
/// is not yet ported from `frontend_common`. All config I/O operations are
/// stubbed with log::warn until `frontend_common::config` is available.
pub struct SdlConfig {
    /// Whether this is a global (non-custom) config instance.
    /// Maps to C++ `Config::global`.
    is_global: bool,
}

impl SdlConfig {
    /// Loads configuration from `config_path`, or uses the default path if
    /// `None`. Reads SDL values and immediately saves them back.
    ///
    /// Maps to C++ `SdlConfig::SdlConfig`.
    pub fn new(config_path: Option<String>) -> Self {
        // Upstream: Initialize(config_path); ReadSdlValues(); SaveSdlValues();
        // frontend_common::Config::Initialize / ReadSdlValues / SaveSdlValues not yet ported.
        log::warn!(
            "SdlConfig::new: frontend_common Config not yet ported; \
             config_path={:?} will be ignored",
            config_path
        );
        let mut instance = SdlConfig { is_global: true };
        instance.read_sdl_values();
        instance.save_sdl_values();
        instance
    }

    /// Reloads all config values from disk and saves them back.
    ///
    /// Maps to C++ `SdlConfig::ReloadAllValues`.
    pub fn reload_all_values(&mut self) {
        // Upstream: Reload(); ReadSdlValues(); SaveSdlValues();
        log::warn!("SdlConfig::reload_all_values: Config::Reload not yet ported");
        self.read_sdl_values();
        self.save_sdl_values();
    }

    /// Saves all config values to disk.
    ///
    /// Maps to C++ `SdlConfig::SaveAllValues`.
    pub fn save_all_values(&mut self) {
        // Upstream: SaveValues(); SaveSdlValues();
        log::warn!("SdlConfig::save_all_values: Config::SaveValues not yet ported");
        self.save_sdl_values();
    }

    // -----------------------------------------------------------------------
    // Read helpers
    // -----------------------------------------------------------------------

    /// Reads all SDL-specific config values.
    ///
    /// Maps to C++ `SdlConfig::ReadSdlValues`.
    fn read_sdl_values(&mut self) {
        self.read_sdl_control_values();
    }

    /// Reads SDL control (button/analog/motion) config values.
    ///
    /// Maps to C++ `SdlConfig::ReadSdlControlValues`.
    fn read_sdl_control_values(&mut self) {
        // Upstream:
        //   BeginGroup(Settings::TranslateCategory(Settings::Category::Controls))
        //   Settings::values.players.SetGlobal(!IsCustomConfig())
        //   for p in 0..players.size(): ReadSdlPlayerValues(p)
        //   if !IsCustomConfig(): ReadDebugControlValues(); ReadHidbusValues()
        //   EndGroup()
        //
        // Settings::values and Config::BeginGroup not yet ported.
        log::debug!("SdlConfig::read_sdl_control_values: Settings not yet ported, using defaults");

        for p in 0..8 {
            self.read_sdl_player_values(p);
        }
        self.read_debug_control_values();
        self.read_hidbus_values();
    }

    /// Reads key bindings for a single player slot.
    ///
    /// Maps to C++ `SdlConfig::ReadSdlPlayerValues`.
    fn read_sdl_player_values(&mut self, player_index: usize) {
        // Upstream reads buttons/analogs/motions from INI for player_index,
        // falling back to DEFAULT_BUTTONS / DEFAULT_ANALOGS / DEFAULT_MOTIONS.
        // INI read infrastructure (Config::ReadStringSetting) not yet ported.
        let _ = player_index;
        log::trace!(
            "SdlConfig::read_sdl_player_values({}): Config::ReadStringSetting not yet ported",
            player_index
        );
    }

    /// Reads debug-pad control values.
    ///
    /// Maps to C++ `SdlConfig::ReadDebugControlValues`.
    fn read_debug_control_values(&mut self) {
        // Upstream reads debug_pad_buttons and debug_pad_analogs from INI.
        // INI read infrastructure not yet ported.
        log::trace!(
            "SdlConfig::read_debug_control_values: Config::ReadStringSetting not yet ported"
        );
    }

    /// Reads Hidbus (ring controller) values.
    ///
    /// Maps to C++ `SdlConfig::ReadHidbusValues`.
    fn read_hidbus_values(&mut self) {
        // Upstream: reads "ring_controller" key using DEFAULT_RINGCON_ANALOGS as default.
        // INI read infrastructure not yet ported.
        log::trace!("SdlConfig::read_hidbus_values: Config::ReadStringSetting not yet ported");
    }

    // -----------------------------------------------------------------------
    // Save helpers
    // -----------------------------------------------------------------------

    /// Saves all SDL-specific config values and flushes to disk.
    ///
    /// Maps to C++ `SdlConfig::SaveSdlValues`.
    fn save_sdl_values(&mut self) {
        // Upstream: LOG_DEBUG(Config, "Saving SDL configuration values")
        //           SaveSdlControlValues(); WriteToIni()
        log::debug!("SdlConfig::save_sdl_values: Config::WriteToIni not yet ported");
        self.save_sdl_control_values();
    }

    /// Saves SDL control (button/analog/motion) config values.
    ///
    /// Maps to C++ `SdlConfig::SaveSdlControlValues`.
    fn save_sdl_control_values(&mut self) {
        // Upstream:
        //   BeginGroup(Controls)
        //   Settings::values.players.SetGlobal(!IsCustomConfig())
        //   for p in 0..players.size(): SaveSdlPlayerValues(p)
        //   if !IsCustomConfig(): SaveDebugControlValues(); SaveHidbusValues()
        //   EndGroup()
        log::trace!("SdlConfig::save_sdl_control_values: Config infrastructure not yet ported");

        for p in 0..8 {
            self.save_sdl_player_values(p);
        }
        self.save_debug_control_values();
        self.save_hidbus_values();
    }

    /// Saves key bindings for a single player slot.
    ///
    /// Maps to C++ `SdlConfig::SaveSdlPlayerValues`.
    fn save_sdl_player_values(&mut self, player_index: usize) {
        // Upstream writes buttons/analogs/motions to INI for player_index.
        // INI write infrastructure (Config::WriteStringSetting) not yet ported.
        let _ = player_index;
        log::trace!(
            "SdlConfig::save_sdl_player_values({}): Config::WriteStringSetting not yet ported",
            player_index
        );
    }

    /// Saves debug-pad control values.
    ///
    /// Maps to C++ `SdlConfig::SaveDebugControlValues`.
    fn save_debug_control_values(&mut self) {
        // Upstream writes debug_pad_buttons and debug_pad_analogs to INI.
        // INI write infrastructure not yet ported.
        log::trace!(
            "SdlConfig::save_debug_control_values: Config::WriteStringSetting not yet ported"
        );
    }

    /// Saves Hidbus (ring controller) values.
    ///
    /// Maps to C++ `SdlConfig::SaveHidbusValues`.
    fn save_hidbus_values(&mut self) {
        // Upstream: writes "ring_controller" key.
        // INI write infrastructure not yet ported.
        log::trace!("SdlConfig::save_hidbus_values: Config::WriteStringSetting not yet ported");
    }
}

impl Drop for SdlConfig {
    /// If this is a global config, saves all values on drop.
    ///
    /// Maps to C++ `SdlConfig::~SdlConfig`.
    fn drop(&mut self) {
        // Upstream: if (global) { SdlConfig::SaveAllValues(); }
        if self.is_global {
            self.save_all_values();
        }
    }
}
