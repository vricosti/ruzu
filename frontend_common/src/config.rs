// SPDX-FileCopyrightText: 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/frontend_common/config.h and config.cpp
//!
//! Provides the base `Config` trait and configuration management infrastructure
//! for reading/writing settings from INI files.

use std::collections::BTreeMap;
use std::path::Path;

use common::settings_common::InputSetting;
use common::settings_input::{
    ControllerType, PlayerInput, JOYCON_BODY_NEON_BLUE, JOYCON_BODY_NEON_RED,
    JOYCON_BUTTONS_NEON_BLUE, JOYCON_BUTTONS_NEON_RED,
};

// ---------------------------------------------------------------------------
// ConfigType
// ---------------------------------------------------------------------------

/// The type of configuration.
/// Maps to C++ `Config::ConfigType`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConfigType {
    GlobalConfig,
    PerGameConfig,
    InputProfile,
}

// ---------------------------------------------------------------------------
// ConfigArray (internal helper)
// ---------------------------------------------------------------------------

/// Internal representation of a config array context.
/// Maps to C++ `Config::ConfigArray`.
#[derive(Clone, Debug)]
struct ConfigArray {
    name: String,
    size: i32,
    index: i32,
}

// ---------------------------------------------------------------------------
// Special characters for output adjustment
// ---------------------------------------------------------------------------

/// Special characters that trigger quoting in output strings.
/// Maps to C++ `Config::special_characters`.
const SPECIAL_CHARACTERS: [char; 18] = [
    '!', '#', '$', '%', '^', '&', '*', '|', ';', '\'', '"', ',', '<', '>', '?', '`', '~', '=',
];

// ---------------------------------------------------------------------------
// Config trait
// ---------------------------------------------------------------------------

/// Base configuration management trait.
/// Maps to C++ `Config` class.
///
/// Derived config implementations must implement the platform-specific
/// read/save methods.
pub trait Config {
    /// Returns the config type.
    fn config_type(&self) -> ConfigType;

    /// Returns whether this is a global config.
    fn is_global(&self) -> bool {
        self.config_type() == ConfigType::GlobalConfig
    }

    /// Returns whether this is a custom (per-game) config.
    fn is_custom_config(&self) -> bool {
        self.config_type() == ConfigType::PerGameConfig
    }

    /// Returns the path to the configuration file.
    fn get_config_file_path(&self) -> &str;

    /// Checks if a key exists in the given section.
    fn exists(&self, section: &str, key: &str) -> bool;

    // -----------------------------------------------------------------------
    // Platform-specific methods (pure virtual in C++)
    // -----------------------------------------------------------------------

    /// Reload all values (platform-specific and global).
    fn reload_all_values(&mut self);

    /// Save all values (platform-specific and global).
    fn save_all_values(&mut self);

    fn read_hidbus_values(&mut self);
    fn read_debug_control_values(&mut self);
    fn read_path_values(&mut self);
    fn read_shortcut_values(&mut self);
    fn read_ui_values(&mut self);
    fn read_ui_gamelist_values(&mut self);
    fn read_ui_layout_values(&mut self);
    fn read_multiplayer_values(&mut self);

    fn save_hidbus_values(&mut self);
    fn save_debug_control_values(&mut self);
    fn save_path_values(&mut self);
    fn save_shortcut_values(&mut self);
    fn save_ui_values(&mut self);
    fn save_ui_gamelist_values(&mut self);
    fn save_ui_layout_values(&mut self);
    fn save_multiplayer_values(&mut self);
}

// ---------------------------------------------------------------------------
// Helper functions (static methods from C++ Config class)
// ---------------------------------------------------------------------------

/// Adjusts a key string by replacing `/` with `\` and spaces with `%20`.
/// Maps to C++ `Config::AdjustKey`.
pub fn adjust_key(key: &str) -> String {
    key.replace('/', "\\").replace(' ', "%20")
}

/// Adjusts an output string for INI serialization.
/// Maps to C++ `Config::AdjustOutputString`.
pub fn adjust_output_string(string: &str) -> String {
    let mut adjusted = string.replace('\\', "/");

    // Handle double-slash normalization (non-Android)
    if adjusted.starts_with("//") {
        adjusted = adjusted.replace("//", "/");
        adjusted.insert(0, '/');
    } else {
        adjusted = adjusted.replace("//", "/");
    }

    // Needed for backwards compatibility with QSettings deserialization
    for &ch in &SPECIAL_CHARACTERS {
        if adjusted.contains(ch) {
            adjusted.insert(0, '"');
            adjusted.push('"');
            break;
        }
    }
    adjusted
}

/// Converts a value to its string representation.
/// Maps to C++ `Config::ToString` template.
pub fn to_string_bool(value: bool) -> String {
    if value {
        "true".to_string()
    } else {
        "false".to_string()
    }
}

/// Converts an integer to string.
pub fn to_string_i64(value: i64) -> String {
    value.to_string()
}

/// Converts an unsigned integer to string.
pub fn to_string_u64(value: u64) -> String {
    value.to_string()
}

// ---------------------------------------------------------------------------
// BaseConfig (shared state for concrete Config implementations)
// ---------------------------------------------------------------------------

/// Shared base state for `Config` implementations.
/// Maps to the non-virtual data members of C++ `Config`.
///
/// Concrete implementations would embed this and delegate to it for the
/// common read/write/group/array logic.
pub struct BaseConfig {
    pub config_type: ConfigType,
    pub config_loc: String,
    pub global: bool,
    pub key_stack: Vec<String>,
    pub array_stack: Vec<ConfigArrayEntry>,
    ini: BTreeMap<String, BTreeMap<String, String>>,
}

/// Public version of ConfigArray for use in BaseConfig.
#[derive(Clone, Debug)]
pub struct ConfigArrayEntry {
    pub name: String,
    pub size: i32,
    pub index: i32,
}

impl BaseConfig {
    pub fn new(config_type: ConfigType) -> Self {
        Self {
            global: config_type == ConfigType::GlobalConfig,
            config_type,
            config_loc: String::new(),
            key_stack: Vec::new(),
            array_stack: Vec::new(),
            ini: BTreeMap::new(),
        }
    }

    /// Loads the INI document owned by the config instance.
    ///
    /// Maps to `Config::Initialize` followed by `Config::SetUpIni`.
    pub fn initialize(&mut self, config_path: &Path) {
        self.config_loc = config_path.to_string_lossy().into_owned();
        let contents = std::fs::read_to_string(config_path).unwrap_or_default();
        self.load_ini(&contents);
    }

    /// Replaces the loaded INI document. Kept public for focused config tests.
    pub fn load_ini(&mut self, contents: &str) {
        self.ini.clear();
        let mut section = String::new();

        for raw_line in contents.lines() {
            let line = raw_line.trim();
            if line.is_empty() || line.starts_with(';') || line.starts_with('#') {
                continue;
            }
            if let Some(name) = line
                .strip_prefix('[')
                .and_then(|line| line.strip_suffix(']'))
            {
                section = name.to_string();
                continue;
            }
            let Some((key, value)) = line.split_once('=') else {
                continue;
            };
            self.ini
                .entry(section.clone())
                .or_default()
                .insert(key.trim().to_string(), value.trim().to_string());
        }
    }

    /// Begins a configuration group.
    pub fn begin_group(&mut self, group: &str) {
        assert!(
            self.array_stack.is_empty(),
            "Can't begin a group while reading/writing from a config array"
        );
        self.key_stack.push(adjust_key(group));
    }

    /// Ends the current configuration group.
    pub fn end_group(&mut self) {
        assert!(
            !self.key_stack.is_empty(),
            "Can't end a group if you haven't started one yet"
        );
        assert!(
            self.array_stack.is_empty(),
            "Can't end a group when reading/writing from a config array"
        );
        self.key_stack.pop();
    }

    /// Gets the current section (first key stack entry).
    pub fn get_section(&self) -> String {
        if self.key_stack.is_empty() {
            String::new()
        } else {
            self.key_stack[0].clone()
        }
    }

    /// Gets the current group path (key stack entries after the first).
    pub fn get_group(&self) -> String {
        if self.key_stack.len() <= 1 {
            return String::new();
        }
        let mut key = String::new();
        for i in 1..self.key_stack.len() {
            key.push_str(&self.key_stack[i]);
            key.push('\\');
        }
        key
    }

    /// Gets the full key including group and array context.
    pub fn get_full_key(&self, key: &str, skip_array_index: bool) -> String {
        if self.array_stack.is_empty() {
            return format!("{}{}", self.get_group(), adjust_key(key));
        }

        let mut array_key = String::new();
        for (i, entry) in self.array_stack.iter().enumerate() {
            if !entry.name.is_empty() {
                array_key.push_str(&entry.name);
                array_key.push('\\');
            }

            if !skip_array_index || (self.array_stack.len() - 1 != i && self.array_stack.len() > 1)
            {
                array_key.push_str(&entry.index.to_string());
                array_key.push('\\');
            }
        }
        format!("{}{}{}", self.get_group(), array_key, adjust_key(key))
    }

    fn read_raw(&self, key: &str) -> Option<&str> {
        let section = self.get_section();
        let full_key = self.get_full_key(key, false);
        self.ini
            .get(&section)
            .and_then(|values| values.get(&full_key))
            .map(String::as_str)
    }

    fn parse_bool(value: &str) -> Option<bool> {
        let value = value.trim_matches('"').as_bytes();
        match value.first().map(u8::to_ascii_lowercase) {
            Some(b't' | b'y' | b'1') => Some(true),
            Some(b'f' | b'n' | b'0') => Some(false),
            Some(b'o') => match value.get(1).map(u8::to_ascii_lowercase) {
                Some(b'n') => Some(true),
                Some(b'f') => Some(false),
                _ => None,
            },
            _ => None,
        }
    }

    /// Maps to `Config::ReadBooleanSetting`.
    pub fn read_boolean_setting(&self, key: &str, default_value: Option<bool>) -> bool {
        let Some(default_value) = default_value else {
            return self
                .read_raw(key)
                .and_then(Self::parse_bool)
                .unwrap_or(false);
        };

        let use_default = self
            .read_raw(&format!("{key}\\default"))
            .and_then(Self::parse_bool)
            .unwrap_or(false);
        if use_default {
            default_value
        } else {
            self.read_raw(key)
                .and_then(Self::parse_bool)
                .unwrap_or(default_value)
        }
    }

    /// Maps to `Config::ReadIntegerSetting`.
    pub fn read_integer_setting(&self, key: &str, default_value: Option<i64>) -> i64 {
        let Some(default_value) = default_value else {
            return self
                .read_raw(key)
                .and_then(|value| value.trim_matches('"').parse().ok())
                .unwrap_or(0);
        };

        let use_default = self
            .read_raw(&format!("{key}\\default"))
            .and_then(Self::parse_bool)
            .unwrap_or(true);
        if use_default {
            default_value
        } else {
            self.read_raw(key)
                .and_then(|value| value.trim_matches('"').parse().ok())
                .unwrap_or(default_value)
        }
    }

    /// Maps to `Config::ReadStringSetting`.
    pub fn read_string_setting(&self, key: &str, default_value: Option<&str>) -> String {
        let mut result = match default_value {
            None => self.read_raw(key).unwrap_or_default().to_string(),
            Some(default_value) => {
                let use_default = self
                    .read_raw(&format!("{key}\\default"))
                    .and_then(Self::parse_bool)
                    .unwrap_or(true);
                if use_default {
                    default_value.to_string()
                } else {
                    self.read_raw(key).unwrap_or(default_value).to_string()
                }
            }
        };

        // Upstream removes quotes after SimpleIni returns the value.
        result.retain(|character| character != '"');
        if default_value.is_some() {
            result = result.replace("//", "/");
        }
        result
    }

    /// Maps to `Config::ReadSystemValues` and its two `ReadCategory` calls.
    pub fn read_system_values(&mut self) {
        self.begin_group("System");
        {
            let mut values = common::settings::values_mut();
            self.read_system_values_into(&mut values);
        }
        self.end_group();
    }

    fn read_system_values_into(&self, values: &mut common::settings::Values) {
        use common::settings_enums::{AudioMode, ConsoleMode, Language, Region, TimeZone};

        let language = self.read_integer_setting(
            "language_index",
            Some(*values.language_index.get_default() as i64),
        );
        values.language_index.set_value(
            Language::from_u32(language as u32).unwrap_or(*values.language_index.get_default()),
        );

        let region = self.read_integer_setting(
            "region_index",
            Some(*values.region_index.get_default() as i64),
        );
        values.region_index.set_value(
            Region::from_u32(region as u32).unwrap_or(*values.region_index.get_default()),
        );

        let time_zone = self.read_integer_setting(
            "time_zone_index",
            Some(*values.time_zone_index.get_default() as i64),
        );
        values.time_zone_index.set_value(
            TimeZone::from_u32(time_zone as u32).unwrap_or(*values.time_zone_index.get_default()),
        );

        values
            .custom_rtc_enabled
            .set_value(self.read_boolean_setting(
                "custom_rtc_enabled",
                Some(*values.custom_rtc_enabled.get_default()),
            ));
        values
            .custom_rtc_offset
            .set_value(self.read_integer_setting(
                "custom_rtc_offset",
                Some(*values.custom_rtc_offset.get_default()),
            ));
        values.rng_seed_enabled.set_value(self.read_boolean_setting(
            "rng_seed_enabled",
            Some(*values.rng_seed_enabled.get_default()),
        ));
        values
            .rng_seed
            .set_value(self.read_u32_setting("rng_seed", *values.rng_seed.get_default()));
        values.device_name.set_value(
            self.read_string_setting("device_name", Some(values.device_name.get_default())),
        );
        values.current_user.set_value(self.read_integer_setting(
            "current_user",
            Some(*values.current_user.get_default() as i64),
        ) as i32);

        let console_mode = self.read_integer_setting(
            "use_docked_mode",
            Some(*values.use_docked_mode.get_default() as i64),
        );
        values.use_docked_mode.set_value(
            ConsoleMode::from_u32(console_mode as u32)
                .unwrap_or(*values.use_docked_mode.get_default()),
        );

        let sound_mode = self.read_integer_setting(
            "sound_index",
            Some(*values.sound_index.get_default() as i64),
        );
        values.sound_index.set_value(
            AudioMode::from_u32(sound_mode as u32).unwrap_or(*values.sound_index.get_default()),
        );
    }

    fn read_u32_setting(&self, key: &str, default_value: u32) -> u32 {
        let use_default = self
            .read_raw(&format!("{key}\\default"))
            .and_then(Self::parse_bool)
            .unwrap_or(true);
        if use_default {
            return default_value;
        }
        self.read_raw(key)
            .and_then(|value| {
                let value = value.trim_matches('"');
                value
                    .strip_prefix("0x")
                    .or_else(|| value.strip_prefix("0X"))
                    .map_or_else(
                        || value.parse().ok(),
                        |hex| u32::from_str_radix(hex, 16).ok(),
                    )
            })
            .unwrap_or(default_value)
    }

    /// Maps to `Config::ReadPlayerValues`.
    pub fn read_player_values(&self, player_index: usize) {
        let configuring_global = common::settings::is_configuring_global();
        let mut values = common::settings::values_mut();
        self.read_player_values_into(player_index, &mut values.players, configuring_global);
    }

    fn read_player_values_into(
        &self,
        player_index: usize,
        players: &mut InputSetting<[PlayerInput; 10]>,
        configuring_global: bool,
    ) {
        let player_prefix = if self.config_type == ConfigType::InputProfile {
            String::new()
        } else {
            format!("player_{player_index}_")
        };
        let profile_name = self.read_string_setting(&format!("{player_prefix}profile_name"), None);

        if self.config_type == ConfigType::PerGameConfig {
            if profile_name.is_empty() {
                let mut global_player = players.get_value_explicit(true)[player_index].clone();
                global_player.profile_name.clear();
                players.get_value_mut()[player_index] = global_player;
                return;
            }
            players.get_value_mut()[player_index].profile_name = profile_name.clone();
        }

        if player_prefix.is_empty() && configuring_global {
            let controller = controller_type_from_config(self.read_integer_setting(
                &format!("{player_prefix}type"),
                Some(ControllerType::ProController as i64),
            ));
            if matches!(
                controller,
                ControllerType::LeftJoycon | ControllerType::RightJoycon
            ) {
                players.get_value_mut()[player_index].controller_type = controller;
            }
            return;
        }

        if self.global {
            players.get_value_explicit_mut(true)[player_index].profile_name = profile_name.clone();
        }

        let player = &mut players.get_value_mut()[player_index];
        player.connected = self.read_boolean_setting(
            &format!("{player_prefix}connected"),
            Some(player_index == 0),
        );
        player.controller_type = controller_type_from_config(self.read_integer_setting(
            &format!("{player_prefix}type"),
            Some(ControllerType::ProController as i64),
        ));
        player.vibration_enabled =
            self.read_boolean_setting(&format!("{player_prefix}vibration_enabled"), Some(true));
        player.vibration_strength = self
            .read_integer_setting(&format!("{player_prefix}vibration_strength"), Some(100))
            as i32;
        player.body_color_left = self.read_integer_setting(
            &format!("{player_prefix}body_color_left"),
            Some(JOYCON_BODY_NEON_BLUE as i64),
        ) as u32;
        player.body_color_right = self.read_integer_setting(
            &format!("{player_prefix}body_color_right"),
            Some(JOYCON_BODY_NEON_RED as i64),
        ) as u32;
        player.button_color_left = self.read_integer_setting(
            &format!("{player_prefix}button_color_left"),
            Some(JOYCON_BUTTONS_NEON_BLUE as i64),
        ) as u32;
        player.button_color_right = self.read_integer_setting(
            &format!("{player_prefix}button_color_right"),
            Some(JOYCON_BUTTONS_NEON_RED as i64),
        ) as u32;
    }

    /// Begins a config array.
    pub fn begin_array(&mut self, array: &str) -> i32 {
        self.array_stack.push(ConfigArrayEntry {
            name: adjust_key(array),
            size: 0,
            index: 0,
        });
        // NOTE: Would read "size" from the INI file. Stubbed to return 0.
        0
    }

    /// Ends the current config array.
    pub fn end_array(&mut self) {
        assert!(
            !self.array_stack.is_empty(),
            "Can't end a config array before starting one"
        );

        let mut size = 0;
        if self.array_stack.last().unwrap().index != 0 {
            size = self.array_stack.last().unwrap().size;
        }

        // NOTE: Would write "size" to the INI file. Stubbed.
        let _ = size;

        self.array_stack.pop();
    }

    /// Sets the current array index.
    pub fn set_array_index(&mut self, index: i32) {
        assert!(
            !self.array_stack.is_empty(),
            "Can't set the array index if you haven't started one yet"
        );

        let array_index = index + 1;
        if let Some(entry) = self.array_stack.last_mut() {
            entry.size = array_index;
            entry.index = array_index;
        }
    }
}

fn controller_type_from_config(value: i64) -> ControllerType {
    ControllerType::try_from(value as u8).unwrap_or(ControllerType::ProController)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_adjust_key() {
        assert_eq!(adjust_key("some/path"), "some\\path");
        assert_eq!(adjust_key("hello world"), "hello%20world");
    }

    #[test]
    fn test_adjust_output_string_special_chars() {
        let result = adjust_output_string("value!test");
        assert!(result.starts_with('"'));
        assert!(result.ends_with('"'));
    }

    #[test]
    fn test_adjust_output_string_no_special() {
        let result = adjust_output_string("simple");
        assert_eq!(result, "simple");
    }

    #[test]
    fn test_base_config_group_stack() {
        let mut cfg = BaseConfig::new(ConfigType::GlobalConfig);
        cfg.begin_group("Controls");
        assert_eq!(cfg.get_section(), "Controls");
        cfg.end_group();
        assert!(cfg.key_stack.is_empty());
    }

    #[test]
    fn test_base_config_full_key() {
        let mut cfg = BaseConfig::new(ConfigType::GlobalConfig);
        cfg.begin_group("Section");
        let key = cfg.get_full_key("mykey", false);
        assert_eq!(key, "mykey");
        cfg.end_group();
    }

    #[test]
    fn read_settings_honor_upstream_default_markers() {
        let mut cfg = BaseConfig::new(ConfigType::GlobalConfig);
        cfg.load_ini(
            r#"
            [Controls]
            enabled\default=true
            enabled=false
            count\default=false
            count=42
            binding\default=false
            binding="engine:sdl,button:1"
            "#,
        );
        cfg.begin_group("Controls");

        assert!(cfg.read_boolean_setting("enabled", Some(true)));
        assert_eq!(cfg.read_integer_setting("count", Some(7)), 42);
        assert_eq!(
            cfg.read_string_setting("binding", Some("fallback")),
            "engine:sdl,button:1"
        );
    }

    #[test]
    fn read_system_values_honors_configured_locale_and_defaults() {
        let mut cfg = BaseConfig::new(ConfigType::GlobalConfig);
        cfg.load_ini(
            r#"
            [System]
            language_index\default=false
            language_index=2
            region_index\default=false
            region_index=2
            time_zone_index\default=true
            time_zone_index=4
            rng_seed_enabled\default=false
            rng_seed_enabled=true
            rng_seed\default=false
            rng_seed=0x1234ABCD
            sound_index\default=false
            sound_index=2
            "#,
        );
        cfg.begin_group("System");
        let mut values = common::settings::Values::default();

        cfg.read_system_values_into(&mut values);

        assert_eq!(
            *values.language_index.get_value(),
            common::settings_enums::Language::French
        );
        assert_eq!(
            *values.region_index.get_value(),
            common::settings_enums::Region::Europe
        );
        assert_eq!(
            *values.time_zone_index.get_value(),
            common::settings_enums::TimeZone::Auto
        );
        assert!(*values.rng_seed_enabled.get_value());
        assert_eq!(*values.rng_seed.get_value(), 0x1234_ABCD);
        assert_eq!(
            *values.sound_index.get_value(),
            common::settings_enums::AudioMode::Surround
        );
    }

    #[test]
    fn read_player_values_matches_global_player_defaults() {
        let mut cfg = BaseConfig::new(ConfigType::GlobalConfig);
        cfg.load_ini(
            r#"
            [Controls]
            player_0_connected\default=false
            player_0_connected=false
            player_0_type\default=false
            player_0_type=5
            player_0_vibration_strength\default=false
            player_0_vibration_strength=63
            "#,
        );
        cfg.begin_group("Controls");

        let mut players = InputSetting::<[PlayerInput; 10]>::new();
        cfg.read_player_values_into(0, &mut players, true);
        cfg.read_player_values_into(1, &mut players, true);
        let first = &players.get_value()[0];
        let second = &players.get_value()[1];

        assert!(!first.connected);
        assert_eq!(first.controller_type, ControllerType::GameCube);
        assert_eq!(first.vibration_strength, 63);
        assert!(!second.connected);
        assert_eq!(second.controller_type, ControllerType::ProController);
    }

    #[test]
    fn missing_global_config_connects_only_player_one() {
        let mut cfg = BaseConfig::new(ConfigType::GlobalConfig);
        cfg.load_ini("");
        cfg.begin_group("Controls");

        let mut players = InputSetting::<[PlayerInput; 10]>::new();
        cfg.read_player_values_into(0, &mut players, true);
        cfg.read_player_values_into(1, &mut players, true);

        assert!(players.get_value()[0].connected);
        assert!(!players.get_value()[1].connected);
    }

    #[test]
    fn per_game_empty_profile_copies_global_player() {
        let mut cfg = BaseConfig::new(ConfigType::PerGameConfig);
        cfg.load_ini(
            r#"
            [Controls]
            player_0_profile_name=
            "#,
        );
        cfg.begin_group("Controls");

        let mut players = InputSetting::<[PlayerInput; 10]>::new();
        players.get_value_explicit_mut(true)[0].connected = true;
        players.get_value_explicit_mut(true)[0].profile_name = "global".to_string();
        players.set_global(false);

        cfg.read_player_values_into(0, &mut players, false);

        assert!(players.get_value()[0].connected);
        assert!(players.get_value()[0].profile_name.is_empty());
    }

    #[test]
    fn test_to_string_bool() {
        assert_eq!(to_string_bool(true), "true");
        assert_eq!(to_string_bool(false), "false");
    }
}
