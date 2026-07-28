// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust counterpart of the game-directory half of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/qt_config.cpp`
// (`Config::ReadUIValues` / `Config::SaveUIValues`, the `Paths\gamedirs\…`
// array).
//
// Upstream persists `UISettings::values.game_dirs` with
// `QSettings::beginWriteArray("gamedirs")`, which writes a `size` key plus one
// group of keys per entry. `beginReadArray` then iterates `0..size` and ignores
// higher-numbered keys, so removing a directory only rewrites `size` and leaves
// the old group behind. Both halves of that behaviour matter and are reproduced
// here.
//
// The file written is **ruzu's own** config (`RuzuPath::ConfigDir`), never
// yuzu's — yuzu's is read once at first launch by `crate::config_import` and
// copied in, and is never written to afterwards.

use std::io;
use std::path::PathBuf;

use common::fs::path_util::{get_ruzu_path, RuzuPath};

use crate::uisettings::GameDir;

/// Key prefix for every game-directory setting.
const GAMEDIRS_PREFIX: &str = "Paths\\gamedirs\\";

/// The INI section the game-directory keys live in. Upstream's `QSettings`
/// group for the whole UI config is `UI`, and the `Paths\` part is a key
/// prefix inside it, not a section of its own.
const UI_SECTION: &str = "[UI]";

/// Path of ruzu's own configuration file.
pub fn config_path() -> PathBuf {
    get_ruzu_path(RuzuPath::ConfigDir).join("qt-config.ini")
}

/// Read the configured game directories — upstream `Config::ReadUIValues`'s
/// `gamedirs` array.
pub fn load_game_dirs() -> Vec<GameDir> {
    match std::fs::read_to_string(config_path()) {
        Ok(contents) => parse_game_dirs(&contents),
        Err(_) => Vec::new(),
    }
}

/// Persist `dirs` back into ruzu's config — upstream `Config::SaveUIValues`.
///
/// Every other key in the file is preserved byte-for-byte: only the
/// `Paths\gamedirs\…` lines are replaced, in place, at the position the first
/// one occupied.
pub fn save_game_dirs(dirs: &[GameDir]) -> io::Result<()> {
    let path = config_path();
    let contents = std::fs::read_to_string(&path).unwrap_or_default();
    let updated = replace_game_dirs(&contents, dirs);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&path, updated)
}

/// Parse the `Paths\gamedirs\…` block of a yuzu-schema INI.
///
/// `Paths\gamedirs\size` is authoritative: entries numbered above it are stale
/// leftovers from a previously longer array and must be ignored, exactly as
/// `QSettings::beginReadArray` ignores them. A stale entry nested inside a live
/// one would otherwise make every game under it appear twice.
pub fn parse_game_dirs(contents: &str) -> Vec<GameDir> {
    use std::collections::BTreeMap;

    let mut size: Option<u32> = None;
    let mut paths: BTreeMap<u32, String> = BTreeMap::new();
    let mut deep: BTreeMap<u32, bool> = BTreeMap::new();
    let mut expanded: BTreeMap<u32, bool> = BTreeMap::new();

    for line in contents.lines() {
        let line = line.trim();
        let Some((key, value)) = line.split_once('=') else {
            continue;
        };
        let Some(rest) = key.strip_prefix(GAMEDIRS_PREFIX) else {
            continue;
        };
        let Some((index_str, field)) = rest.split_once('\\') else {
            if rest == "size" {
                size = value.trim().parse().ok();
            }
            continue;
        };
        let Ok(index) = index_str.parse::<u32>() else {
            continue;
        };
        // `…\default` suffixes record whether the value is at its default; they
        // are metadata, not the value, and must not override it.
        match field {
            "path" => {
                paths.insert(index, value.to_owned());
            }
            "deep_scan" => {
                deep.insert(index, is_true(value));
            }
            "expanded" => {
                expanded.insert(index, is_true(value));
            }
            _ => {}
        }
    }

    paths
        .into_iter()
        // yuzu's arrays are 1-based on disk, so `size = N` covers 1..=N.
        .filter(|(index, _)| size.is_none_or(|size| *index <= size))
        .map(|(index, path)| GameDir {
            path,
            deep_scan: deep.get(&index).copied().unwrap_or(false),
            expanded: expanded.get(&index).copied().unwrap_or(true),
        })
        .collect()
}

/// Return `contents` with its `Paths\gamedirs\…` lines replaced by `dirs`.
fn replace_game_dirs(contents: &str, dirs: &[GameDir]) -> String {
    let had_trailing_newline = contents.is_empty() || contents.ends_with('\n');

    let is_gamedir_line = |line: &str| {
        line.trim()
            .split_once('=')
            .is_some_and(|(key, _)| key.starts_with(GAMEDIRS_PREFIX))
    };

    let mut out: Vec<String> = Vec::new();
    let mut block_written = false;
    for line in contents.lines() {
        if is_gamedir_line(line) {
            // Emit the whole new block where the first old line sat, and drop
            // every other old line.
            if !block_written {
                out.extend(render_game_dirs(dirs));
                block_written = true;
            }
            continue;
        }
        out.push(line.to_string());
    }

    if !block_written {
        // No existing block: append under `[UI]`, creating the section if the
        // file does not have one yet.
        if !out.iter().any(|line| line.trim() == UI_SECTION) {
            if !out.is_empty() {
                out.push(String::new());
            }
            out.push(UI_SECTION.to_string());
        }
        out.extend(render_game_dirs(dirs));
    }

    let mut text = out.join("\n");
    if had_trailing_newline && !text.is_empty() {
        text.push('\n');
    }
    text
}

/// Render the `Paths\gamedirs\…` lines for `dirs`, in upstream's key order and
/// with the `…\default` markers `QSettings` writes alongside each value.
fn render_game_dirs(dirs: &[GameDir]) -> Vec<String> {
    let mut lines = Vec::with_capacity(dirs.len() * 5 + 1);
    lines.push(format!("{GAMEDIRS_PREFIX}size={}", dirs.len()));
    for (position, dir) in dirs.iter().enumerate() {
        // 1-based on disk, matching what yuzu writes.
        let index = position + 1;
        lines.push(format!("{GAMEDIRS_PREFIX}{index}\\path={}", dir.path));
        lines.push(format!(
            "{GAMEDIRS_PREFIX}{index}\\deep_scan\\default={}",
            !dir.deep_scan
        ));
        lines.push(format!(
            "{GAMEDIRS_PREFIX}{index}\\deep_scan={}",
            dir.deep_scan
        ));
        lines.push(format!(
            "{GAMEDIRS_PREFIX}{index}\\expanded\\default={}",
            dir.expanded
        ));
        lines.push(format!(
            "{GAMEDIRS_PREFIX}{index}\\expanded={}",
            dir.expanded
        ));
    }
    lines
}

/// INI booleans, which yuzu writes as `true` / `false` but older configs may
/// carry as `1` / `0`.
fn is_true(value: &str) -> bool {
    matches!(value.trim(), "true" | "1")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A config with a stale 5th entry nested inside the 4th — the shape a
    /// removed-then-re-added game directory leaves behind.
    const CONFIG_WITH_STALE_ENTRY: &str = concat!(
        "[UI]\n",
        "Paths\\gamedirs\\size=4\n",
        "Paths\\gamedirs\\1\\path=SDMC\n",
        "Paths\\gamedirs\\1\\deep_scan=false\n",
        "Paths\\gamedirs\\2\\path=UserNAND\n",
        "Paths\\gamedirs\\3\\path=SysNAND\n",
        "Paths\\gamedirs\\4\\path=/games/roms\n",
        "Paths\\gamedirs\\4\\deep_scan\\default=false\n",
        "Paths\\gamedirs\\4\\deep_scan=true\n",
        "Paths\\gamedirs\\5\\path=/games/roms/Mario Kart 8 Deluxe [NSP]\n",
        "Paths\\gamedirs\\5\\deep_scan=false\n",
    );

    fn dir(path: &str, deep_scan: bool) -> GameDir {
        GameDir {
            path: path.to_string(),
            deep_scan,
            expanded: true,
        }
    }

    #[test]
    fn stale_entries_past_size_are_ignored() {
        let dirs = parse_game_dirs(CONFIG_WITH_STALE_ENTRY);
        assert_eq!(dirs.len(), 4);
        assert!(!dirs.iter().any(|d| d.path.contains("Mario Kart")));
    }

    #[test]
    fn provider_tokens_are_kept_but_flagged_as_non_paths() {
        let dirs = parse_game_dirs(CONFIG_WITH_STALE_ENTRY);
        assert!(!dirs[0].is_filesystem_path()); // SDMC
        assert!(dirs[3].is_filesystem_path()); // /games/roms
    }

    #[test]
    fn default_suffixed_keys_do_not_override_the_value() {
        // `…\deep_scan\default=false` must not win over `…\deep_scan=true`.
        let dirs = parse_game_dirs(CONFIG_WITH_STALE_ENTRY);
        assert!(dirs[3].deep_scan);
    }

    #[test]
    fn missing_size_keeps_every_entry() {
        let config = "Paths\\gamedirs\\1\\path=/a\nPaths\\gamedirs\\2\\path=/b\n";
        assert_eq!(parse_game_dirs(config).len(), 2);
    }

    #[test]
    fn round_trips_through_save_and_load() {
        let dirs = vec![dir("/games/a", true), dir("/games/b", false)];
        let written = replace_game_dirs("[UI]\n", &dirs);
        assert_eq!(parse_game_dirs(&written), dirs);
    }

    #[test]
    fn writing_preserves_every_other_key() {
        let original = concat!(
            "[Controls]\n",
            "player_0_type=0\n",
            "[UI]\n",
            "Paths\\gamedirs\\size=1\n",
            "Paths\\gamedirs\\1\\path=/old\n",
            "Multiplayer\\nickname=vric\n",
        );
        let updated = replace_game_dirs(original, &[dir("/new", false)]);
        assert!(updated.contains("player_0_type=0"));
        assert!(updated.contains("Multiplayer\\nickname=vric"));
        assert!(updated.contains("[Controls]"));
        assert!(updated.contains("Paths\\gamedirs\\1\\path=/new"));
        assert!(!updated.contains("/old"));
    }

    #[test]
    fn writing_removes_stale_entries_rather_than_leaving_them() {
        // The whole point of rewriting in place: entry 5 must not survive, or
        // the next reader with a larger `size` would pick it up again.
        let updated = replace_game_dirs(CONFIG_WITH_STALE_ENTRY, &[dir("/games/roms", true)]);
        assert!(!updated.contains("Mario Kart 8 Deluxe [NSP]"));
        assert_eq!(parse_game_dirs(&updated), vec![dir("/games/roms", true)]);
    }

    #[test]
    fn block_is_written_once_at_the_first_old_position() {
        let updated = replace_game_dirs(CONFIG_WITH_STALE_ENTRY, &[dir("/games/roms", true)]);
        assert_eq!(updated.matches("Paths\\gamedirs\\size=").count(), 1);
    }

    #[test]
    fn ui_section_is_created_when_absent() {
        let updated = replace_game_dirs("[Controls]\nplayer_0_type=0\n", &[dir("/a", false)]);
        assert!(updated.contains("[UI]"));
        // The new keys must land after the section header, not before it.
        let ui = updated.find("[UI]").unwrap();
        let key = updated.find("Paths\\gamedirs\\size=").unwrap();
        assert!(ui < key);
    }

    #[test]
    fn empty_list_writes_size_zero() {
        let updated = replace_game_dirs(CONFIG_WITH_STALE_ENTRY, &[]);
        assert!(updated.contains("Paths\\gamedirs\\size=0"));
        assert_eq!(parse_game_dirs(&updated), Vec::new());
    }

    #[test]
    fn indices_are_one_based_like_yuzu() {
        let updated = replace_game_dirs("[UI]\n", &[dir("/a", false), dir("/b", false)]);
        assert!(updated.contains("Paths\\gamedirs\\1\\path=/a"));
        assert!(updated.contains("Paths\\gamedirs\\2\\path=/b"));
        assert!(!updated.contains("Paths\\gamedirs\\0\\"));
    }
}
