// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! NACP (Nintendo Application Control Property) parser.
//!
//! The NACP is a 0x4000-byte structure found in Control NCA or NRO ASET data.
//! It contains application metadata: title, developer, version, etc.
//! There are 16 language entries, each with a name (0x200 bytes) and
//! developer (0x100 bytes), both null-terminated UTF-8.

use byteorder::{LittleEndian, ReadBytesExt};
use std::io::Cursor;
use thiserror::Error;

/// Total NACP size.
pub const NACP_SIZE: usize = 0x4000;

/// Number of language entries.
const LANGUAGE_COUNT: usize = 16;

/// Size of a language entry (name + developer).
const LANGUAGE_ENTRY_SIZE: usize = 0x300;

/// Size of the application name field within a language entry.
const NAME_SIZE: usize = 0x200;

/// Size of the developer field within a language entry.
const DEVELOPER_SIZE: usize = 0x100;

/// Version string offset within NACP.
const VERSION_OFFSET: usize = 0x3060;

/// Version string size.
const VERSION_SIZE: usize = 0x10;

/// Title ID offset within NACP.
const TITLE_ID_OFFSET: usize = 0x3038;

/// Errors from NACP parsing.
#[derive(Debug, Error)]
pub enum NacpError {
    #[error("NACP data too small: need {NACP_SIZE} bytes, got {0}")]
    DataTooSmall(usize),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
}

/// Language indices matching Switch system languages.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum NacpLanguage {
    AmericanEnglish = 0,
    BritishEnglish = 1,
    Japanese = 2,
    French = 3,
    German = 4,
    LatinAmericanSpanish = 5,
    Spanish = 6,
    Italian = 7,
    Dutch = 8,
    CanadianFrench = 9,
    Portuguese = 10,
    Russian = 11,
    Korean = 12,
    TraditionalChinese = 13,
    SimplifiedChinese = 14,
    BrazilianPortuguese = 15,
}

/// A single language entry from the NACP.
#[derive(Debug, Clone)]
pub struct NacpLanguageEntry {
    /// Application name (up to 0x200 bytes, null-terminated).
    pub name: String,
    /// Developer/publisher name (up to 0x100 bytes, null-terminated).
    pub developer: String,
}

/// Parsed NACP data.
#[derive(Debug, Clone)]
pub struct Nacp {
    /// Language entries (16 total).
    pub languages: Vec<NacpLanguageEntry>,
    /// Display version string.
    pub version: String,
    /// Application title ID (from NACP, if present).
    pub title_id: u64,
}

impl Nacp {
    /// Parse NACP from raw bytes (must be at least 0x4000 bytes).
    pub fn parse(data: &[u8]) -> Result<Self, NacpError> {
        if data.len() < NACP_SIZE {
            return Err(NacpError::DataTooSmall(data.len()));
        }

        let mut languages = Vec::with_capacity(LANGUAGE_COUNT);
        for i in 0..LANGUAGE_COUNT {
            let base = i * LANGUAGE_ENTRY_SIZE;
            let name = read_null_terminated_string(&data[base..base + NAME_SIZE]);
            let dev_base = base + NAME_SIZE;
            let developer = read_null_terminated_string(&data[dev_base..dev_base + DEVELOPER_SIZE]);
            languages.push(NacpLanguageEntry { name, developer });
        }

        let version = read_null_terminated_string(&data[VERSION_OFFSET..VERSION_OFFSET + VERSION_SIZE]);

        let mut cur = Cursor::new(&data[TITLE_ID_OFFSET..TITLE_ID_OFFSET + 8]);
        let title_id = cur.read_u64::<LittleEndian>()?;

        Ok(Self {
            languages,
            version,
            title_id,
        })
    }

    /// Get the application name, preferring AmericanEnglish, then first non-empty.
    pub fn get_application_name(&self) -> String {
        // Try AmericanEnglish first
        if let Some(entry) = self.languages.get(NacpLanguage::AmericanEnglish as usize) {
            if !entry.name.is_empty() {
                return entry.name.clone();
            }
        }

        // Try BritishEnglish
        if let Some(entry) = self.languages.get(NacpLanguage::BritishEnglish as usize) {
            if !entry.name.is_empty() {
                return entry.name.clone();
            }
        }

        // Fall back to first non-empty
        for entry in &self.languages {
            if !entry.name.is_empty() {
                return entry.name.clone();
            }
        }

        "Unknown".to_string()
    }

    /// Get the developer name, preferring AmericanEnglish.
    pub fn get_developer(&self) -> String {
        if let Some(entry) = self.languages.get(NacpLanguage::AmericanEnglish as usize) {
            if !entry.developer.is_empty() {
                return entry.developer.clone();
            }
        }

        for entry in &self.languages {
            if !entry.developer.is_empty() {
                return entry.developer.clone();
            }
        }

        "Unknown".to_string()
    }
}

/// Read a null-terminated UTF-8 string from a byte slice.
fn read_null_terminated_string(data: &[u8]) -> String {
    let end = data.iter().position(|&b| b == 0).unwrap_or(data.len());
    String::from_utf8_lossy(&data[..end]).into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn build_nacp(
        names: &[(usize, &str, &str)],
        version: &str,
        title_id: u64,
    ) -> Vec<u8> {
        let mut data = vec![0u8; NACP_SIZE];

        for &(lang_idx, name, developer) in names {
            let base = lang_idx * LANGUAGE_ENTRY_SIZE;
            let name_bytes = name.as_bytes();
            let dev_bytes = developer.as_bytes();

            let name_len = name_bytes.len().min(NAME_SIZE - 1);
            data[base..base + name_len].copy_from_slice(&name_bytes[..name_len]);

            let dev_base = base + NAME_SIZE;
            let dev_len = dev_bytes.len().min(DEVELOPER_SIZE - 1);
            data[dev_base..dev_base + dev_len].copy_from_slice(&dev_bytes[..dev_len]);
        }

        let ver_bytes = version.as_bytes();
        let ver_len = ver_bytes.len().min(VERSION_SIZE - 1);
        data[VERSION_OFFSET..VERSION_OFFSET + ver_len].copy_from_slice(&ver_bytes[..ver_len]);

        data[TITLE_ID_OFFSET..TITLE_ID_OFFSET + 8].copy_from_slice(&title_id.to_le_bytes());

        data
    }

    #[test]
    fn test_parse_nacp() {
        let data = build_nacp(
            &[
                (0, "My Game", "My Developer"),
                (2, "マイゲーム", "マイデベロッパー"),
            ],
            "1.0.0",
            0x0100_0000_0000_0001,
        );

        let nacp = Nacp::parse(&data).unwrap();
        assert_eq!(nacp.get_application_name(), "My Game");
        assert_eq!(nacp.get_developer(), "My Developer");
        assert_eq!(nacp.version, "1.0.0");
        assert_eq!(nacp.title_id, 0x0100_0000_0000_0001);
    }

    #[test]
    fn test_fallback_to_non_english() {
        // No AmericanEnglish or BritishEnglish, only Japanese
        let data = build_nacp(
            &[(2, "日本語ゲーム", "日本語開発者")],
            "2.0",
            0,
        );

        let nacp = Nacp::parse(&data).unwrap();
        assert_eq!(nacp.get_application_name(), "日本語ゲーム");
        assert_eq!(nacp.get_developer(), "日本語開発者");
    }

    #[test]
    fn test_empty_nacp() {
        let data = vec![0u8; NACP_SIZE];
        let nacp = Nacp::parse(&data).unwrap();
        assert_eq!(nacp.get_application_name(), "Unknown");
        assert_eq!(nacp.get_developer(), "Unknown");
    }

    #[test]
    fn test_nacp_too_small() {
        let data = vec![0u8; 100];
        assert!(matches!(Nacp::parse(&data), Err(NacpError::DataTooSmall(_))));
    }
}
