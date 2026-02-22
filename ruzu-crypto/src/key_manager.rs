// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Key management for NCA decryption.
//!
//! Parses `prod.keys` and `title.keys` files used by yuzu/Ryujinx to store
//! the cryptographic keys needed to decrypt NCA content archives.

use std::collections::HashMap;
use std::path::Path;
use thiserror::Error;

/// A 128-bit (16 byte) key, used for AES-128.
pub type Key128 = [u8; 16];

/// A 256-bit (32 byte) key, used for AES-XTS header decryption.
pub type Key256 = [u8; 32];

/// Errors from key management operations.
#[derive(Debug, Error)]
pub enum KeyError {
    #[error("I/O error reading key file: {0}")]
    Io(#[from] std::io::Error),

    #[error("invalid hex in key file: line {line}: {detail}")]
    InvalidHex { line: usize, detail: String },

    #[error("key not found: {0}")]
    KeyNotFound(String),
}

/// Standard key names used in NCA decryption.
pub mod key_names {
    pub const HEADER_KEY: &str = "header_key";
    pub const KEY_AREA_KEY_APPLICATION_PREFIX: &str = "key_area_key_application_";
    pub const KEY_AREA_KEY_OCEAN_PREFIX: &str = "key_area_key_ocean_";
    pub const KEY_AREA_KEY_SYSTEM_PREFIX: &str = "key_area_key_system_";
    pub const TITLEKEK_PREFIX: &str = "titlekek_";
}

/// Manages cryptographic keys for Switch content decryption.
///
/// Keys are loaded from `prod.keys` (system keys) and `title.keys` (per-title keys).
/// The file format is simple: `key_name = hex_value`, one per line.
#[derive(Debug, Clone)]
pub struct KeyManager {
    /// System keys from prod.keys (key_name -> raw bytes).
    keys: HashMap<String, Vec<u8>>,
    /// Title keys from title.keys (rights_id_hex -> title_key_128).
    title_keys: HashMap<String, Key128>,
}

impl KeyManager {
    /// Create an empty key manager.
    pub fn new() -> Self {
        Self {
            keys: HashMap::new(),
            title_keys: HashMap::new(),
        }
    }

    /// Load keys from a directory containing `prod.keys` and optionally `title.keys`.
    pub fn load_from_directory(&mut self, dir: &Path) -> Result<(), KeyError> {
        let prod_path = dir.join("prod.keys");
        if prod_path.exists() {
            self.load_prod_keys(&prod_path)?;
            log::info!(
                "Loaded {} system keys from {}",
                self.keys.len(),
                prod_path.display()
            );
        } else {
            log::warn!("prod.keys not found at {}", prod_path.display());
        }

        let title_path = dir.join("title.keys");
        if title_path.exists() {
            self.load_title_keys(&title_path)?;
            log::info!(
                "Loaded {} title keys from {}",
                self.title_keys.len(),
                title_path.display()
            );
        }

        Ok(())
    }

    /// Parse a `prod.keys` file (format: `key_name = hex_value`).
    pub fn load_prod_keys(&mut self, path: &Path) -> Result<(), KeyError> {
        let content = std::fs::read_to_string(path)?;
        for (line_num, line) in content.lines().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') || line.starts_with(';') {
                continue;
            }
            if let Some((name, hex_val)) = parse_key_line(line) {
                let bytes = hex::decode(hex_val).map_err(|e| KeyError::InvalidHex {
                    line: line_num + 1,
                    detail: e.to_string(),
                })?;
                self.keys.insert(name.to_lowercase(), bytes);
            }
        }
        Ok(())
    }

    /// Parse a `title.keys` file (format: `rights_id_hex = title_key_hex`).
    pub fn load_title_keys(&mut self, path: &Path) -> Result<(), KeyError> {
        let content = std::fs::read_to_string(path)?;
        for (line_num, line) in content.lines().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') || line.starts_with(';') {
                continue;
            }
            if let Some((rights_id, hex_val)) = parse_key_line(line) {
                let bytes = hex::decode(hex_val).map_err(|e| KeyError::InvalidHex {
                    line: line_num + 1,
                    detail: e.to_string(),
                })?;
                if bytes.len() == 16 {
                    let mut key = [0u8; 16];
                    key.copy_from_slice(&bytes);
                    self.title_keys.insert(rights_id.to_lowercase(), key);
                }
            }
        }
        Ok(())
    }

    /// Register a title key (e.g., extracted from a ticket).
    pub fn add_title_key(&mut self, rights_id: &str, key: Key128) {
        self.title_keys.insert(rights_id.to_lowercase(), key);
    }

    /// Look up the 256-bit header key used for NCA header XTS decryption.
    pub fn header_key(&self) -> Option<Key256> {
        self.get_key_256(key_names::HEADER_KEY)
    }

    /// Look up a titlekek by revision (0-based).
    pub fn titlekek(&self, revision: u8) -> Option<Key128> {
        let name = format!("{}{:02x}", key_names::TITLEKEK_PREFIX, revision);
        self.get_key_128(&name)
    }

    /// Look up a key-area key for a given crypto type and revision.
    ///
    /// `key_type`: 0=Application, 1=Ocean, 2=System
    pub fn key_area_key(&self, key_type: u8, revision: u8) -> Option<Key128> {
        let prefix = match key_type {
            0 => key_names::KEY_AREA_KEY_APPLICATION_PREFIX,
            1 => key_names::KEY_AREA_KEY_OCEAN_PREFIX,
            2 => key_names::KEY_AREA_KEY_SYSTEM_PREFIX,
            _ => return None,
        };
        let name = format!("{}{:02x}", prefix, revision);
        self.get_key_128(&name)
    }

    /// Look up a title key by rights ID (hex string).
    pub fn title_key(&self, rights_id: &str) -> Option<Key128> {
        self.title_keys.get(&rights_id.to_lowercase()).copied()
    }

    /// Look up a 128-bit key by name.
    pub fn get_key_128(&self, name: &str) -> Option<Key128> {
        let bytes = self.keys.get(&name.to_lowercase())?;
        if bytes.len() >= 16 {
            let mut key = [0u8; 16];
            key.copy_from_slice(&bytes[..16]);
            Some(key)
        } else {
            None
        }
    }

    /// Look up a 256-bit key by name.
    pub fn get_key_256(&self, name: &str) -> Option<Key256> {
        let bytes = self.keys.get(&name.to_lowercase())?;
        if bytes.len() >= 32 {
            let mut key = [0u8; 32];
            key.copy_from_slice(&bytes[..32]);
            Some(key)
        } else {
            None
        }
    }

    /// Check if any keys are loaded.
    pub fn has_keys(&self) -> bool {
        !self.keys.is_empty()
    }

    /// Number of system keys loaded.
    pub fn system_key_count(&self) -> usize {
        self.keys.len()
    }

    /// Number of title keys loaded.
    pub fn title_key_count(&self) -> usize {
        self.title_keys.len()
    }
}

impl Default for KeyManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse a single `key = value` line, returning (name, hex_value).
fn parse_key_line(line: &str) -> Option<(&str, &str)> {
    let mut parts = line.splitn(2, '=');
    let name = parts.next()?.trim();
    let value = parts.next()?.trim();
    if name.is_empty() || value.is_empty() {
        return None;
    }
    Some((name, value))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn test_parse_key_line() {
        assert_eq!(
            parse_key_line("header_key = 00112233"),
            Some(("header_key", "00112233"))
        );
        assert_eq!(
            parse_key_line("  key  =  aabb  "),
            Some(("key", "aabb"))
        );
        assert_eq!(parse_key_line("# comment"), None);
        assert_eq!(parse_key_line(""), None);
        assert_eq!(parse_key_line("no_equals"), None);
        assert_eq!(parse_key_line("empty_val = "), None);
    }

    #[test]
    fn test_load_prod_keys() {
        let dir = tempfile::tempdir().unwrap();
        let prod_path = dir.path().join("prod.keys");
        let mut f = std::fs::File::create(&prod_path).unwrap();
        writeln!(f, "# Test keys").unwrap();
        writeln!(
            f,
            "header_key = 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
        )
        .unwrap();
        writeln!(f, "titlekek_00 = aabbccddeeff00112233445566778899").unwrap();
        writeln!(
            f,
            "key_area_key_application_00 = 00112233445566778899aabbccddeeff"
        )
        .unwrap();

        let mut km = KeyManager::new();
        km.load_prod_keys(&prod_path).unwrap();

        assert_eq!(km.system_key_count(), 3);

        let hk = km.header_key().expect("should have header_key");
        assert_eq!(hk[0], 0x01);
        assert_eq!(hk[15], 0xef);
        assert_eq!(hk[16], 0x01);

        let tk = km.titlekek(0).expect("should have titlekek_00");
        assert_eq!(tk[0], 0xaa);

        let kak = km.key_area_key(0, 0).expect("should have kak_app_00");
        assert_eq!(kak[0], 0x00);
        assert_eq!(kak[1], 0x11);
    }

    #[test]
    fn test_load_title_keys() {
        let dir = tempfile::tempdir().unwrap();
        let title_path = dir.path().join("title.keys");
        let mut f = std::fs::File::create(&title_path).unwrap();
        writeln!(
            f,
            "01000000000000000000000000000001 = aabbccddeeff00112233445566778899"
        )
        .unwrap();

        let mut km = KeyManager::new();
        km.load_title_keys(&title_path).unwrap();

        assert_eq!(km.title_key_count(), 1);
        let tk = km
            .title_key("01000000000000000000000000000001")
            .expect("should find title key");
        assert_eq!(tk[0], 0xaa);
    }

    #[test]
    fn test_add_title_key() {
        let mut km = KeyManager::new();
        km.add_title_key("deadbeef00000000deadbeef00000001", [0x42; 16]);

        let tk = km
            .title_key("deadbeef00000000deadbeef00000001")
            .expect("should find manually added key");
        assert_eq!(tk, [0x42; 16]);
    }

    #[test]
    fn test_missing_key_returns_none() {
        let km = KeyManager::new();
        assert!(km.header_key().is_none());
        assert!(km.titlekek(0).is_none());
        assert!(km.key_area_key(0, 0).is_none());
        assert!(km.title_key("nonexistent").is_none());
    }

    #[test]
    fn test_load_from_directory() {
        let dir = tempfile::tempdir().unwrap();
        let prod_path = dir.path().join("prod.keys");
        let mut f = std::fs::File::create(&prod_path).unwrap();
        writeln!(f, "titlekek_00 = 00112233445566778899aabbccddeeff").unwrap();

        let mut km = KeyManager::new();
        km.load_from_directory(dir.path()).unwrap();
        assert!(km.titlekek(0).is_some());
    }
}
