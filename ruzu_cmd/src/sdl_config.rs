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

use common::settings_input::{native_analog, native_button, native_motion};
use frontend_common::config::{BaseConfig, ConfigType};
use input_common::main_common::{generate_analog_param_from_keys, generate_keyboard_param};

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
pub const NUM_BUTTONS: usize = native_button::NUM_BUTTONS;

/// Number of native analogs. Maps to `Settings::NativeAnalog::NumAnalogs`.
pub const NUM_ANALOGS: usize = native_analog::NUM_ANALOGS;

/// Number of native motions. Maps to `Settings::NativeMotion::NumMotions`.
pub const NUM_MOTIONS: usize = native_motion::NUM_MOTIONS;

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
    // The upstream std::array has 22 entries but only 15 explicit
    // initializers. C++ zero-initializes the remaining slots.
    0,
    0,
    0,
    0,
    0,
    0,
    0,
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
pub struct SdlConfig {
    /// Whether this is a global (non-custom) config instance.
    /// Maps to C++ `Config::global`.
    is_global: bool,
    config_path: std::path::PathBuf,
    base: BaseConfig,
}

impl SdlConfig {
    /// Loads configuration from `config_path`, or uses the default path if
    /// `None`. Reads SDL values and immediately saves them back.
    ///
    /// Maps to C++ `SdlConfig::SdlConfig`.
    pub fn new(config_path: Option<String>) -> Self {
        let config_path = resolve_config_path(config_path);
        let mut base = BaseConfig::new(ConfigType::GlobalConfig);
        base.initialize(&config_path);
        let mut instance = SdlConfig {
            is_global: true,
            config_path,
            base,
        };
        instance.read_base_control_values();
        instance.read_system_values();
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

    /// Runs the `Config::ReadPlayerValues` part of base `ReadControlValues`.
    ///
    /// Upstream executes this from `Config::Initialize` before the derived
    /// `ReadSdlValues` pass fills the SDL bindings.
    fn read_base_control_values(&mut self) {
        self.base.begin_group("Controls");
        {
            let mut values = common::settings::values_mut();
            values.players.set_global(true);
        }
        let player_count = common::settings::values().players.get_value().len();
        for player_index in 0..player_count {
            self.base.read_player_values(player_index);
        }
        self.base.end_group();
    }

    /// Reads all SDL-specific config values.
    ///
    /// Maps to C++ `SdlConfig::ReadSdlValues`.
    fn read_sdl_values(&mut self) {
        self.read_sdl_control_values();
    }

    /// Reads the subset of global System settings needed by the SDL frontend.
    ///
    /// Upstream `Config::ReadSystemValues` calls `ReadCategory(System)`, which
    /// includes `rng_seed_enabled` and `rng_seed`. The full frontend_common
    /// Config layer is not ported yet, so keep this narrow bridge in the SDL
    /// config owner instead of applying RNG policy in `main.rs`.
    fn read_system_values(&mut self) {
        let Ok(contents) = std::fs::read_to_string(&self.config_path) else {
            log::debug!(
                "SdlConfig::read_system_values: config_path={:?} not readable, using Settings defaults",
                self.config_path
            );
            return;
        };
        read_rng_seed_settings_from_ini(&contents);
    }

    /// Reads SDL control (button/analog/motion) config values.
    ///
    /// Maps to C++ `SdlConfig::ReadSdlControlValues`.
    fn read_sdl_control_values(&mut self) {
        self.base.begin_group("Controls");
        {
            common::settings::values_mut().players.set_global(true);
        }
        let player_count = common::settings::values().players.get_value().len();
        for p in 0..player_count {
            self.read_sdl_player_values(p);
        }
        self.read_debug_control_values();
        self.read_hidbus_values();
        self.base.end_group();
    }

    /// Reads key bindings for a single player slot.
    ///
    /// Maps to C++ `SdlConfig::ReadSdlPlayerValues`.
    fn read_sdl_player_values(&mut self, player_index: usize) {
        let mut values = common::settings::values_mut();
        let player = &mut values.players.get_value_mut()[player_index];
        read_sdl_player_values_into(&self.base, player_index, player);
    }

    /// Reads debug-pad control values.
    ///
    /// Maps to C++ `SdlConfig::ReadDebugControlValues`.
    fn read_debug_control_values(&mut self) {
        let mut buttons = std::array::from_fn(|_| String::new());
        for (index, mapping) in native_button::MAPPING.iter().enumerate() {
            let default_param = generate_keyboard_param(DEFAULT_BUTTONS[index]);
            let key = format!("debug_pad_{mapping}");
            let value = self.base.read_string_setting(&key, Some(&default_param));
            buttons[index] = if value.is_empty() {
                default_param
            } else {
                value
            };
        }

        let mut analogs = std::array::from_fn(|_| String::new());
        for (index, mapping) in native_analog::MAPPING.iter().enumerate() {
            let keys = DEFAULT_ANALOGS[index];
            let default_param = generate_analog_param_from_keys(
                keys[0],
                keys[1],
                keys[2],
                keys[3],
                DEFAULT_STICK_MOD[index],
                0.5,
            );
            let key = format!("debug_pad_{mapping}");
            let value = self.base.read_string_setting(&key, Some(&default_param));
            analogs[index] = if value.is_empty() {
                default_param
            } else {
                value
            };
        }

        let mut values = common::settings::values_mut();
        values.debug_pad_buttons = buttons;
        values.debug_pad_analogs = analogs;
    }

    /// Reads Hidbus (ring controller) values.
    ///
    /// Maps to C++ `SdlConfig::ReadHidbusValues`.
    fn read_hidbus_values(&mut self) {
        let default_param = generate_analog_param_from_keys(
            0,
            0,
            DEFAULT_RINGCON_ANALOGS[0],
            DEFAULT_RINGCON_ANALOGS[1],
            0,
            0.05,
        );
        let value = self
            .base
            .read_string_setting("ring_controller", Some(&default_param));
        common::settings::values_mut().ringcon_analogs = if value.is_empty() {
            default_param
        } else {
            value
        };
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

fn read_sdl_player_values_into(
    base: &BaseConfig,
    player_index: usize,
    player: &mut common::settings_input::PlayerInput,
) {
    let player_prefix = format!("player_{player_index}_");

    for (index, mapping) in native_button::MAPPING.iter().enumerate() {
        let default_param = generate_keyboard_param(DEFAULT_BUTTONS[index]);
        let key = format!("{player_prefix}{mapping}");
        let value = base.read_string_setting(&key, Some(&default_param));
        player.buttons[index] = if value.is_empty() {
            default_param
        } else {
            value
        };
    }

    for (index, mapping) in native_analog::MAPPING.iter().enumerate() {
        let keys = DEFAULT_ANALOGS[index];
        let default_param = generate_analog_param_from_keys(
            keys[0],
            keys[1],
            keys[2],
            keys[3],
            DEFAULT_STICK_MOD[index],
            0.5,
        );
        let key = format!("{player_prefix}{mapping}");
        let value = base.read_string_setting(&key, Some(&default_param));
        player.analogs[index] = if value.is_empty() {
            default_param
        } else {
            value
        };
    }

    for (index, mapping) in native_motion::MAPPING.iter().enumerate() {
        let default_param = generate_keyboard_param(DEFAULT_MOTIONS[index]);
        let key = format!("{player_prefix}{mapping}");
        let value = base.read_string_setting(&key, Some(&default_param));
        player.motions[index] = if value.is_empty() {
            default_param
        } else {
            value
        };
    }
}

fn resolve_config_path(config_path: Option<String>) -> std::path::PathBuf {
    config_path
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| {
            common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::ConfigDir)
                .join("sdl2-config.ini")
        })
}

fn read_rng_seed_settings_from_ini(contents: &str) {
    let rng_seed_enabled_default =
        parse_ini_bool(contents, "System", "rng_seed_enabled\\default").unwrap_or(true);
    let rng_seed_default = parse_ini_bool(contents, "System", "rng_seed\\default").unwrap_or(true);

    let mut values = common::settings::values_mut();
    if !rng_seed_enabled_default {
        if let Some(enabled) = parse_ini_bool(contents, "System", "rng_seed_enabled") {
            values.rng_seed_enabled.set_value(enabled);
        }
    } else {
        values.rng_seed_enabled.set_value(false);
    }

    if !rng_seed_default {
        if let Some(seed) = parse_ini_u32(contents, "System", "rng_seed") {
            values.rng_seed.set_value(seed);
        }
    } else {
        values.rng_seed.set_value(0);
    }
}

fn parse_ini_bool(contents: &str, section: &str, key: &str) -> Option<bool> {
    parse_ini_value(contents, section, key).and_then(|value| match value {
        "true" | "1" => Some(true),
        "false" | "0" => Some(false),
        _ => None,
    })
}

fn parse_ini_u32(contents: &str, section: &str, key: &str) -> Option<u32> {
    parse_ini_value(contents, section, key).and_then(|value| {
        let value = value
            .strip_prefix("0x")
            .or_else(|| value.strip_prefix("0X"))
            .unwrap_or(value);
        u32::from_str_radix(value, 16)
            .or_else(|_| value.parse::<u32>())
            .ok()
    })
}

fn parse_ini_value<'a>(contents: &'a str, section: &str, key: &str) -> Option<&'a str> {
    let mut in_section = false;
    for raw_line in contents.lines() {
        let line = raw_line.trim();
        if line.is_empty() || line.starts_with(';') || line.starts_with('#') {
            continue;
        }
        if let Some(name) = line
            .strip_prefix('[')
            .and_then(|line| line.strip_suffix(']'))
        {
            in_section = name == section;
            continue;
        }
        if !in_section {
            continue;
        }
        let Some((found_key, value)) = line.split_once('=') else {
            continue;
        };
        if found_key.trim() == key {
            return Some(value.trim());
        }
    }
    None
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

#[cfg(test)]
mod tests {
    use super::{
        read_rng_seed_settings_from_ini, read_sdl_player_values_into, resolve_config_path,
        DEFAULT_BUTTONS, NUM_BUTTONS,
    };
    use common::param_package::ParamPackage;
    use common::settings_input::{native_button, PlayerInput};
    use frontend_common::config::{BaseConfig, ConfigType};

    #[test]
    fn default_button_array_matches_upstream_zero_initialization() {
        assert_eq!(NUM_BUTTONS, native_button::NUM_BUTTONS);
        assert_eq!(DEFAULT_BUTTONS[0], 4);
        assert!(DEFAULT_BUTTONS[15..].iter().all(|value| *value == 0));
    }

    #[test]
    fn reads_sdl_player_bindings_and_defaults() {
        let mut base = BaseConfig::new(ConfigType::GlobalConfig);
        base.load_ini(
            r#"
            [Controls]
            player_0_button_a\default=false
            player_0_button_a="engine:sdl,port:0,button:1"
            "#,
        );
        base.begin_group("Controls");
        let mut player = PlayerInput::default();

        read_sdl_player_values_into(&base, 0, &mut player);

        let button_a = ParamPackage::from_serialized(&player.buttons[0]);
        assert_eq!(button_a.get_str("engine", ""), "sdl");
        assert_eq!(button_a.get_int("button", -1), 1);
        let button_b = ParamPackage::from_serialized(&player.buttons[1]);
        assert_eq!(button_b.get_str("engine", ""), "keyboard");
        assert_eq!(button_b.get_int("code", -1), DEFAULT_BUTTONS[1]);
    }

    #[test]
    fn reads_rng_seed_from_system_group_when_not_default() {
        let old_enabled = *common::settings::values().rng_seed_enabled.get_value();
        let old_seed = *common::settings::values().rng_seed.get_value();
        {
            let mut values = common::settings::values_mut();
            values.rng_seed_enabled.set_value(false);
            values.rng_seed.set_value(0);
        }

        read_rng_seed_settings_from_ini(
            r#"
            [System]
            rng_seed_enabled\default=false
            rng_seed_enabled=true
            rng_seed\default=false
            rng_seed=0x1234ABCD
            "#,
        );

        {
            let values = common::settings::values();
            assert!(*values.rng_seed_enabled.get_value());
            assert_eq!(*values.rng_seed.get_value(), 0x1234_ABCD);
        }

        let mut values = common::settings::values_mut();
        values.rng_seed_enabled.set_value(old_enabled);
        values.rng_seed.set_value(old_seed);
    }

    #[test]
    fn default_rng_seed_entries_reset_to_upstream_defaults() {
        let old_enabled = *common::settings::values().rng_seed_enabled.get_value();
        let old_seed = *common::settings::values().rng_seed.get_value();
        {
            let mut values = common::settings::values_mut();
            values.rng_seed_enabled.set_value(true);
            values.rng_seed.set_value(0xFFFF_FFFF);
        }

        read_rng_seed_settings_from_ini(
            r#"
            [System]
            rng_seed_enabled\default=true
            rng_seed_enabled=true
            rng_seed\default=true
            rng_seed=0x1234ABCD
            "#,
        );

        {
            let values = common::settings::values();
            assert!(!*values.rng_seed_enabled.get_value());
            assert_eq!(*values.rng_seed.get_value(), 0);
        }

        let mut values = common::settings::values_mut();
        values.rng_seed_enabled.set_value(old_enabled);
        values.rng_seed.set_value(old_seed);
    }

    #[test]
    fn explicit_config_path_is_used_verbatim() {
        assert_eq!(
            resolve_config_path(Some("/tmp/custom-sdl2-config.ini".to_string())),
            std::path::PathBuf::from("/tmp/custom-sdl2-config.ini")
        );
    }
}
