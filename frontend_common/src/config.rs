// SPDX-FileCopyrightText: 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/frontend_common/config.h and config.cpp
//!
//! Provides the base `Config` trait and configuration management infrastructure
//! for reading/writing settings from INI files.

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
    fn test_to_string_bool() {
        assert_eq!(to_string_bool(true), "true");
        assert_eq!(to_string_bool(false), "false");
    }
}
