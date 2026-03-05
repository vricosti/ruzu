//! Port of zuyu/src/common/param_package.h and zuyu/src/common/param_package.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! A string-based key-value container supporting serialization/deserialization.
//! Used for input configuration and other parameter stores.

use std::collections::HashMap;

use log::{error, trace};

const KEY_VALUE_SEPARATOR: char = ':';
const PARAM_SEPARATOR: char = ',';

const ESCAPE_CHARACTER: char = '$';
const KEY_VALUE_SEPARATOR_ESCAPE: &str = "$0";
const PARAM_SEPARATOR_ESCAPE: &str = "$1";
const ESCAPE_CHARACTER_ESCAPE: &str = "$2";

/// A placeholder for empty param packages to avoid empty strings.
const EMPTY_PLACEHOLDER: &str = "[empty]";

/// A string-based key-value container supporting serializing to and deserializing from a string.
#[derive(Debug, Clone, Default)]
pub struct ParamPackage {
    data: HashMap<String, String>,
}

impl ParamPackage {
    /// Create a new empty ParamPackage.
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    /// Deserialize a ParamPackage from a serialized string.
    pub fn from_serialized(serialized: &str) -> Self {
        let mut pkg = Self::new();

        if serialized == EMPTY_PLACEHOLDER {
            return pkg;
        }

        for pair in serialized.split(PARAM_SEPARATOR) {
            let parts: Vec<&str> = pair.splitn(2, KEY_VALUE_SEPARATOR).collect();
            if parts.len() != 2 {
                error!("invalid key pair {}", pair);
                continue;
            }

            let key = unescape(parts[0]);
            let value = unescape(parts[1]);
            pkg.data.insert(key, value);
        }

        pkg
    }

    /// Create from an iterator of key-value pairs.
    pub fn from_pairs<I, K, V>(pairs: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: Into<String>,
        V: Into<String>,
    {
        Self {
            data: pairs
                .into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
        }
    }

    /// Serialize the ParamPackage to a string.
    pub fn serialize(&self) -> String {
        if self.data.is_empty() {
            return EMPTY_PLACEHOLDER.to_string();
        }

        let mut result = String::new();
        for (i, (key, value)) in self.data.iter().enumerate() {
            if i > 0 {
                result.push(PARAM_SEPARATOR);
            }
            result.push_str(&escape(key));
            result.push(KEY_VALUE_SEPARATOR);
            result.push_str(&escape(value));
        }
        result
    }

    /// Get a string value by key, returning the default if not found.
    pub fn get_str(&self, key: &str, default_value: &str) -> String {
        match self.data.get(key) {
            Some(v) => v.clone(),
            None => {
                trace!("key '{}' not found", key);
                default_value.to_string()
            }
        }
    }

    /// Get an integer value by key, returning the default if not found or on parse error.
    pub fn get_int(&self, key: &str, default_value: i32) -> i32 {
        match self.data.get(key) {
            Some(v) => match v.parse::<i32>() {
                Ok(n) => n,
                Err(_) => {
                    error!("failed to convert {} to int", v);
                    default_value
                }
            },
            None => {
                trace!("key '{}' not found", key);
                default_value
            }
        }
    }

    /// Get a float value by key, returning the default if not found or on parse error.
    pub fn get_float(&self, key: &str, default_value: f32) -> f32 {
        match self.data.get(key) {
            Some(v) => match v.parse::<f32>() {
                Ok(n) => n,
                Err(_) => {
                    error!("failed to convert {} to float", v);
                    default_value
                }
            },
            None => {
                trace!("key '{}' not found", key);
                default_value
            }
        }
    }

    /// Set a string value.
    pub fn set_str(&mut self, key: &str, value: String) {
        self.data.insert(key.to_string(), value);
    }

    /// Set an integer value.
    pub fn set_int(&mut self, key: &str, value: i32) {
        self.data.insert(key.to_string(), value.to_string());
    }

    /// Set a float value.
    pub fn set_float(&mut self, key: &str, value: f32) {
        self.data.insert(key.to_string(), value.to_string());
    }

    /// Check if a key exists.
    pub fn has(&self, key: &str) -> bool {
        self.data.contains_key(key)
    }

    /// Remove a key.
    pub fn erase(&mut self, key: &str) {
        self.data.remove(key);
    }

    /// Remove all keys.
    pub fn clear(&mut self) {
        self.data.clear();
    }
}

fn escape(s: &str) -> String {
    s.replace(ESCAPE_CHARACTER, ESCAPE_CHARACTER_ESCAPE)
        .replace(PARAM_SEPARATOR, PARAM_SEPARATOR_ESCAPE)
        .replace(KEY_VALUE_SEPARATOR, KEY_VALUE_SEPARATOR_ESCAPE)
}

fn unescape(s: &str) -> String {
    s.replace(KEY_VALUE_SEPARATOR_ESCAPE, &KEY_VALUE_SEPARATOR.to_string())
        .replace(PARAM_SEPARATOR_ESCAPE, &PARAM_SEPARATOR.to_string())
        .replace(ESCAPE_CHARACTER_ESCAPE, &ESCAPE_CHARACTER.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty() {
        let pkg = ParamPackage::new();
        assert_eq!(pkg.serialize(), "[empty]");
    }

    #[test]
    fn test_roundtrip() {
        let mut pkg = ParamPackage::new();
        pkg.set_str("engine", "sdl".to_string());
        pkg.set_int("port", 26760);
        pkg.set_float("deadzone", 0.15);

        let serialized = pkg.serialize();
        let pkg2 = ParamPackage::from_serialized(&serialized);

        assert_eq!(pkg2.get_str("engine", ""), "sdl");
        assert_eq!(pkg2.get_int("port", 0), 26760);
        assert!((pkg2.get_float("deadzone", 0.0) - 0.15).abs() < 0.001);
    }

    #[test]
    fn test_empty_placeholder() {
        let pkg = ParamPackage::from_serialized("[empty]");
        assert!(!pkg.has("anything"));
    }

    #[test]
    fn test_has_and_erase() {
        let mut pkg = ParamPackage::new();
        pkg.set_str("key", "value".to_string());
        assert!(pkg.has("key"));
        pkg.erase("key");
        assert!(!pkg.has("key"));
    }
}
