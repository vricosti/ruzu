// SPDX-FileCopyrightText: Copyright 2026 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of Eden `core/launch_timestamp_cache.h` and `launch_timestamp_cache.cpp`.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::{LazyLock, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

use common::fs::path_util::{get_ruzu_path, RuzuPath};
use serde_json::{Map, Value};

const DEFAULT_LAUNCH_TIMESTAMP: i64 = 1_767_225_600;

#[derive(Default)]
struct CacheState {
    timestamps: BTreeMap<u64, i64>,
    counts: BTreeMap<u64, u64>,
    loaded: bool,
}

static CACHE_STATE: LazyLock<Mutex<CacheState>> =
    LazyLock::new(|| Mutex::new(CacheState::default()));

fn get_cache_path() -> PathBuf {
    get_ruzu_path(RuzuPath::CacheDir).join("launched.json")
}

fn parse_title_id(key: &str) -> Option<u64> {
    let mut key = key.trim_start();
    let negative = key.starts_with('-');
    if key.starts_with(['-', '+']) {
        key = &key[1..];
    }
    if let Some(rest) = key.strip_prefix("0x").or_else(|| key.strip_prefix("0X")) {
        key = rest;
    }
    let digits = key
        .char_indices()
        .take_while(|(_, character)| character.is_ascii_hexdigit())
        .last()
        .map_or(0, |(index, character)| index + character.len_utf8());
    if digits == 0 {
        return None;
    }
    let value = u64::from_str_radix(&key[..digits], 16).ok()?;
    Some(if negative {
        0u64.wrapping_sub(value)
    } else {
        value
    })
}

fn load_from_path(state: &mut CacheState, path: &Path) {
    if state.loaded {
        return;
    }
    state.loaded = true;

    if !path.exists() {
        return;
    }

    let data = match std::fs::read_to_string(path) {
        Ok(data) => data,
        Err(error) => {
            log::warn!(
                "Failed to read launch timestamp cache {}: {error}",
                path.display()
            );
            return;
        }
    };

    let json: Value = match serde_json::from_str(&data) {
        Ok(json) => json,
        Err(error) => {
            log::warn!("Failed to parse launch timestamp cache: {error}");
            return;
        }
    };
    let Some(entries) = json.as_object() else {
        return;
    };

    for (key, value) in entries {
        let Some(title_id) = parse_title_id(key) else {
            continue;
        };
        if let Some(entry) = value.as_object() {
            if let Some(timestamp) = entry.get("timestamp").and_then(Value::as_i64) {
                state.timestamps.insert(title_id, timestamp);
            }
            if let Some(count) = entry.get("launch_count").and_then(Value::as_u64) {
                state.counts.insert(title_id, count);
            }
        } else if let Some(timestamp) = value.as_i64() {
            state.timestamps.insert(title_id, timestamp);
        }
    }
}

fn load(state: &mut CacheState) {
    load_from_path(state, &get_cache_path());
}

fn save_to_path(state: &CacheState, path: &Path) -> std::io::Result<()> {
    let mut json = Map::new();
    for (&title_id, &timestamp) in &state.timestamps {
        let mut entry = Map::new();
        entry.insert("timestamp".to_string(), Value::from(timestamp));
        entry.insert(
            "launch_count".to_string(),
            Value::from(state.counts.get(&title_id).copied().unwrap_or(0)),
        );
        json.insert(format!("{title_id:016X}"), Value::Object(entry));
    }

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let data = serde_json::to_string_pretty(&Value::Object(json))?;
    std::fs::write(path, data)
}

fn save(state: &CacheState) {
    let path = get_cache_path();
    if let Err(error) = save_to_path(state, &path) {
        log::warn!(
            "Failed to write launch timestamp cache {}: {error}",
            path.display()
        );
    }
}

fn now_seconds() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |duration| duration.as_secs() as i64)
}

pub fn save_launch_timestamp(title_id: u64) {
    let mut state = CACHE_STATE.lock().unwrap();
    load(&mut state);
    state.timestamps.insert(title_id, now_seconds());
    *state.counts.entry(title_id).or_default() += 1;
    save(&state);
}

pub fn get_launch_timestamp(title_id: u64) -> i64 {
    let mut state = CACHE_STATE.lock().unwrap();
    load(&mut state);
    state
        .timestamps
        .get(&title_id)
        .copied()
        .unwrap_or(DEFAULT_LAUNCH_TIMESTAMP)
}

pub fn get_launch_count(title_id: u64) -> u64 {
    let mut state = CACHE_STATE.lock().unwrap();
    load(&mut state);
    state.counts.get(&title_id).copied().unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unique_test_path(name: &str) -> PathBuf {
        std::env::temp_dir().join(format!(
            "ruzu-launch-cache-{name}-{}-{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ))
    }

    #[test]
    fn loads_current_and_legacy_cache_entries() {
        let path = unique_test_path("load");
        std::fs::write(
            &path,
            r#"{
                "0000000000000001": {"timestamp": 42, "launch_count": 3},
                "0000000000000002": 17,
                "not-hex": {"timestamp": 99, "launch_count": 1}
            }"#,
        )
        .unwrap();
        let mut state = CacheState::default();

        load_from_path(&mut state, &path);

        assert_eq!(state.timestamps.get(&1), Some(&42));
        assert_eq!(state.counts.get(&1), Some(&3));
        assert_eq!(state.timestamps.get(&2), Some(&17));
        assert_eq!(state.counts.get(&2), None);
        assert_eq!(state.timestamps.len(), 2);
        std::fs::remove_file(path).unwrap();
    }

    #[test]
    fn title_id_parser_matches_stoull_prefix_rules() {
        assert_eq!(parse_title_id("  +0x10suffix"), Some(0x10));
        assert_eq!(parse_title_id("-1"), Some(u64::MAX));
        assert_eq!(parse_title_id("not-hex"), None);
    }

    #[test]
    fn saves_uppercase_title_ids_and_zero_missing_counts() {
        let path = unique_test_path("save");
        let mut state = CacheState::default();
        state.timestamps.insert(0xabcdef, 123);

        save_to_path(&state, &path).unwrap();

        let json: Value = serde_json::from_slice(&std::fs::read(&path).unwrap()).unwrap();
        let entry = &json["0000000000ABCDEF"];
        assert_eq!(entry["timestamp"], 123);
        assert_eq!(entry["launch_count"], 0);
        std::fs::remove_file(path).unwrap();
    }

    #[test]
    fn absent_title_uses_edens_fixed_default_timestamp() {
        assert_eq!(DEFAULT_LAUNCH_TIMESTAMP, 1_767_225_600);
    }
}
