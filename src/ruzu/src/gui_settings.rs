// SPDX-FileCopyrightText: Copyright 2025 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! GTK counterpart of Eden's `src/qt_common/gui_settings.{h,cpp}`.
//!
//! The X11 preference must be available before GTK is initialized, so it
//! cannot live only in the regular frontend settings loaded after startup.

use std::io;
use std::path::{Path, PathBuf};

use common::fs::path_util::{get_ruzu_path, RuzuPath};

const FORCE_X11_KEY: &str = "gui_force_x11";

pub fn gui_config_path() -> PathBuf {
    get_ruzu_path(RuzuPath::ConfigDir).join("gui_config.ini")
}

pub fn set_force_x11(state: bool) -> io::Result<()> {
    set_force_x11_in(&gui_config_path(), state)
}

pub fn get_force_x11() -> bool {
    get_force_x11_in(&gui_config_path())
}

fn set_force_x11_in(path: &Path, state: bool) -> io::Result<()> {
    let contents = std::fs::read_to_string(path).unwrap_or_default();
    let mut lines: Vec<String> = contents.lines().map(str::to_owned).collect();
    let assignment = format!("{FORCE_X11_KEY}={state}");
    if let Some(line) = lines.iter_mut().find(|line| {
        line.split_once('=')
            .is_some_and(|(key, _)| key.trim() == FORCE_X11_KEY)
    }) {
        *line = assignment;
    } else {
        lines.push(assignment);
    }
    let mut output = lines.join("\n");
    output.push('\n');
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, output)
}

fn get_force_x11_in(path: &Path) -> bool {
    std::fs::read_to_string(path)
        .unwrap_or_default()
        .lines()
        .filter_map(|line| line.split_once('='))
        .find(|(key, _)| key.trim() == FORCE_X11_KEY)
        .is_some_and(|(_, value)| matches!(value.trim(), "1" | "true" | "yes" | "on"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn force_x11_defaults_false_and_round_trips_without_losing_other_keys() {
        let root = tempfile::tempdir().unwrap();
        let path = root.path().join("gui_config.ini");
        assert!(!get_force_x11_in(&path));

        std::fs::write(&path, "another_setting=true\n").unwrap();
        set_force_x11_in(&path, true).unwrap();
        assert!(get_force_x11_in(&path));
        assert!(std::fs::read_to_string(&path)
            .unwrap()
            .contains("another_setting=true"));

        set_force_x11_in(&path, false).unwrap();
        assert!(!get_force_x11_in(&path));
    }
}
