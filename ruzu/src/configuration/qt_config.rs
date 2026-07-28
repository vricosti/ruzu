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
use common::settings_input::{native_analog, native_button, native_motion};

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

/// The INI section the per-player control bindings live in.
const CONTROLS_SECTION: &str = "[Controls]";

/// Read every player's bindings — upstream `QtConfig::ReadQtPlayerValues`,
/// called once per player from `Config::ReadControlValues`.
///
/// Upstream falls back to a generated keyboard mapping when a key is missing;
/// an absent key here leaves the slot at whatever `PlayerInput::default()` set,
/// which is the same empty-string state the dialog renders as `[not set]`.
pub fn load_control_values() {
    let Ok(contents) = std::fs::read_to_string(config_path()) else {
        return;
    };
    let values = parse_controls(&contents);
    if values.is_empty() {
        return;
    }

    let mut settings = common::settings::values_mut();
    let players = settings.players.get_value_mut();
    for (index, player) in players.iter_mut().enumerate() {
        let prefix = format!("player_{index}_");
        for (slot, name) in native_button::MAPPING.iter().enumerate() {
            if let Some(param) = values.get(&format!("{prefix}{name}")) {
                player.buttons[slot] = param.clone();
            }
        }
        for (slot, name) in native_analog::MAPPING.iter().enumerate() {
            if let Some(param) = values.get(&format!("{prefix}{name}")) {
                player.analogs[slot] = param.clone();
            }
        }
        for (slot, name) in native_motion::MAPPING.iter().enumerate() {
            if let Some(param) = values.get(&format!("{prefix}{name}")) {
                player.motions[slot] = param.clone();
            }
        }
    }
}

/// Persist every player's bindings — upstream `QtConfig::SaveQtPlayerValues`.
///
/// Only the `[Controls]` keys this function owns are rewritten; every other
/// line in the file is preserved, the same way `save_game_dirs` leaves the rest
/// of the INI alone.
pub fn save_control_values() -> io::Result<()> {
    let path = config_path();
    let contents = std::fs::read_to_string(&path).unwrap_or_default();

    let mut entries: Vec<(String, String)> = Vec::new();
    {
        let settings = common::settings::values();
        for (index, player) in settings.players.get_value().iter().enumerate() {
            let prefix = format!("player_{index}_");
            for (slot, name) in native_button::MAPPING.iter().enumerate() {
                entries.push((format!("{prefix}{name}"), player.buttons[slot].clone()));
            }
            for (slot, name) in native_analog::MAPPING.iter().enumerate() {
                entries.push((format!("{prefix}{name}"), player.analogs[slot].clone()));
            }
            for (slot, name) in native_motion::MAPPING.iter().enumerate() {
                entries.push((format!("{prefix}{name}"), player.motions[slot].clone()));
            }
        }
    }

    let updated = replace_controls(&contents, &entries);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&path, updated)
}

/// Parse the `player_N_…` keys out of the `[Controls]` section.
///
/// Upstream writes each binding as a `key\default=` line followed by the value,
/// quoted because the parameter string contains commas. Both the quotes and the
/// companion `\default` line are handled here; keys whose `\default` is `true`
/// still carry their value, so they are read like any other.
pub fn parse_controls(contents: &str) -> std::collections::BTreeMap<String, String> {
    let mut values = std::collections::BTreeMap::new();
    let mut in_section = false;

    for line in contents.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_section = trimmed == CONTROLS_SECTION;
            continue;
        }
        if !in_section {
            continue;
        }
        let Some((key, value)) = trimmed.split_once('=') else {
            continue;
        };
        let key = key.trim();
        // `key\default=` is metadata about the neighbouring key, not a binding.
        if key.ends_with("\\default") || !key.starts_with("player_") {
            continue;
        }
        values.insert(key.to_string(), unquote(value.trim()).to_string());
    }

    values
}

/// Replace the `player_N_…` lines of `contents` with `entries`.
///
/// The rewritten block is dropped where the first existing binding sat, so a
/// hand-edited file keeps its shape; a file with no `[Controls]` section at all
/// gains one at the end.
pub fn replace_controls(contents: &str, entries: &[(String, String)]) -> String {
    let is_binding = |line: &str| {
        let trimmed = line.trim();
        let Some((key, _)) = trimmed.split_once('=') else {
            return false;
        };
        key.trim().starts_with("player_")
    };

    let rendered: Vec<String> = entries
        .iter()
        .flat_map(|(key, value)| {
            // Upstream's `WriteStringSetting` emits the `\default` marker first;
            // a binding written from the dialog is never the built-in default.
            [
                format!("{key}\\default=false"),
                format!("{key}=\"{value}\""),
            ]
        })
        .collect();

    let mut output: Vec<String> = Vec::new();
    let mut in_section = false;
    let mut written = false;
    let mut saw_section = false;

    for line in contents.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            // Leaving `[Controls]` without having met a binding: append here so
            // the keys land in their own section rather than the next one.
            if in_section && !written {
                output.extend(rendered.iter().cloned());
                written = true;
            }
            in_section = trimmed == CONTROLS_SECTION;
            saw_section |= in_section;
            output.push(line.to_string());
            continue;
        }
        if in_section && is_binding(line) {
            if !written {
                output.extend(rendered.iter().cloned());
                written = true;
            }
            continue;
        }
        output.push(line.to_string());
    }

    if !written {
        if !saw_section {
            output.push(CONTROLS_SECTION.to_string());
        }
        output.extend(rendered);
    }

    let mut text = output.join("\n");
    text.push('\n');
    text
}

/// Strip the surrounding quotes yuzu writes around values containing commas.
fn unquote(value: &str) -> &str {
    value
        .strip_prefix('"')
        .and_then(|rest| rest.strip_suffix('"'))
        .unwrap_or(value)
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

    /// The binding a real Xbox pad produces, as yuzu writes it.
    const SDL_BINDING: &str =
        "engine:sdl,port:0,guid:030000005e040000000b000015050000,button:1";

    #[test]
    fn control_bindings_survive_a_save_and_reload() {
        // The whole point of the Controls page: what the dialog wrote must come
        // back byte-for-byte on the next launch.
        let entries = vec![
            ("player_0_button_a".to_string(), SDL_BINDING.to_string()),
            (
                "player_0_lstick".to_string(),
                "engine:sdl,axis_x:0,axis_y:1,offset_x:-0.03".to_string(),
            ),
        ];
        let written = replace_controls("", &entries);
        let parsed = parse_controls(&written);

        assert_eq!(parsed.get("player_0_button_a").map(String::as_str), Some(SDL_BINDING));
        assert_eq!(
            parsed.get("player_0_lstick").map(String::as_str),
            Some("engine:sdl,axis_x:0,axis_y:1,offset_x:-0.03")
        );
    }

    #[test]
    fn bindings_are_quoted_so_their_commas_survive() {
        // A parameter string is a comma-separated list; written bare it would
        // still round-trip through this parser but would break every other INI
        // reader, yuzu's included.
        let entries = vec![("player_0_button_a".to_string(), SDL_BINDING.to_string())];
        let written = replace_controls("", &entries);
        assert!(written.contains(&format!("player_0_button_a=\"{SDL_BINDING}\"")));
        // Upstream pairs each key with its `\default` marker.
        assert!(written.contains("player_0_button_a\\default=false"));
    }

    #[test]
    fn saving_controls_leaves_the_rest_of_the_file_alone() {
        // `save_control_values` shares the config file with every other
        // setting; clobbering a neighbouring section would lose them.
        let original = "[UI]\nPaths\\gamedirs\\size=1\n[Controls]\nplayer_0_button_a\\default=false\nplayer_0_button_a=\"old\"\ntouchscreen_enabled=true\n[Core]\nuse_multi_core=true\n";
        let entries = vec![("player_0_button_a".to_string(), "new".to_string())];
        let updated = replace_controls(original, &entries);

        assert!(updated.contains("Paths\\gamedirs\\size=1"));
        assert!(updated.contains("touchscreen_enabled=true"));
        assert!(updated.contains("use_multi_core=true"));
        assert!(updated.contains("player_0_button_a=\"new\""));
        assert!(!updated.contains("\"old\""));
    }

    #[test]
    fn a_config_without_a_controls_section_gains_one() {
        let entries = vec![("player_0_button_a".to_string(), "x".to_string())];
        let updated = replace_controls("[UI]\nsomething=1\n", &entries);
        assert!(updated.contains("[Controls]"));
        assert!(updated.contains("something=1"));

        // And the new keys must land inside that section, not before it.
        let section = updated.find("[Controls]").unwrap();
        let key = updated.find("player_0_button_a").unwrap();
        assert!(key > section);
    }

    #[test]
    fn bindings_from_another_section_are_not_read_as_controls() {
        // `player_` keys only mean a binding inside [Controls]; a same-named key
        // elsewhere must not leak in.
        let contents = "[UI]\nplayer_0_button_a=\"decoy\"\n[Controls]\nplayer_0_button_b=\"real\"\n";
        let parsed = parse_controls(contents);
        assert!(!parsed.contains_key("player_0_button_a"));
        assert_eq!(parsed.get("player_0_button_b").map(String::as_str), Some("real"));
    }

    #[test]
    fn the_default_marker_is_not_mistaken_for_a_binding() {
        let contents = "[Controls]\nplayer_0_button_a\\default=false\nplayer_0_button_a=\"v\"\n";
        let parsed = parse_controls(contents);
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed.get("player_0_button_a").map(String::as_str), Some("v"));
    }

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
