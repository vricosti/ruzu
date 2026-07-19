// SPDX-License-Identifier: GPL-3.0-or-later
//
// One-time import of an existing yuzu configuration into ruzu.
//
// ruzu reuses yuzu's INI config schema (`frontend_common::config` is a port of
// yuzu's `Config`), and its config directory is a sibling of yuzu's under the
// same XDG base (`…/ruzu` vs `…/yuzu`). On first run, if a yuzu config exists
// and ruzu doesn't yet have the corresponding file, copy yuzu's settings so
// users keep their configuration. A marker file records that the check ran, so
// the detection happens exactly once — not on every startup.

use std::fs;
use std::path::{Path, PathBuf};

use common::fs::path_util::{get_data_directory, get_ruzu_path, RuzuPath};

/// Marker written after the one-time check, so it never runs again.
const IMPORT_MARKER: &str = ".yuzu-import-done";

/// Config files shared verbatim between yuzu and ruzu (identical INI schema):
/// the GUI config and the SDL/CLI config.
const CONFIG_FILES: &[&str] = &["qt-config.ini", "sdl2-config.ini"];

/// Run the yuzu-config import exactly once (guarded by a marker file). Safe to
/// call on every startup; it returns immediately once the marker exists.
pub fn import_yuzu_config_once() {
    let ruzu_dir = get_ruzu_path(RuzuPath::ConfigDir);
    let marker = ruzu_dir.join(IMPORT_MARKER);
    if marker.exists() {
        return; // Already checked on a previous run.
    }

    match yuzu_config_dir() {
        Some(yuzu_dir) => import_from(&yuzu_dir, &ruzu_dir),
        None => log::info!("No yuzu configuration found; nothing to import"),
    }

    // Record that the one-time check ran, even when nothing was imported, so it
    // does not repeat on subsequent startups.
    if let Err(e) = fs::create_dir_all(&ruzu_dir) {
        log::warn!("Could not create ruzu config dir {}: {e}", ruzu_dir.display());
        return;
    }
    if let Err(e) = fs::write(&marker, b"imported\n") {
        log::warn!("Could not write import marker {}: {e}", marker.display());
    }
}

/// Locate yuzu's config directory. yuzu (like ruzu) puts config under
/// `$XDG_DATA_HOME/yuzu/config` when `$XDG_DATA_HOME/yuzu` exists, otherwise
/// under `$XDG_CONFIG_HOME/yuzu`. Returns the first candidate that actually
/// holds a config file we can import.
fn yuzu_config_dir() -> Option<PathBuf> {
    let mut candidates = Vec::new();
    let data_yuzu = get_data_directory("XDG_DATA_HOME").join("yuzu");
    if data_yuzu.is_dir() {
        candidates.push(data_yuzu.join("config"));
    }
    candidates.push(get_data_directory("XDG_CONFIG_HOME").join("yuzu"));

    candidates
        .into_iter()
        .find(|dir| CONFIG_FILES.iter().any(|name| dir.join(name).is_file()))
}

/// Copy any config files that yuzu has and ruzu does not yet have.
fn import_from(yuzu_dir: &Path, ruzu_dir: &Path) {
    if let Err(e) = fs::create_dir_all(ruzu_dir) {
        log::warn!("Could not create ruzu config dir {}: {e}", ruzu_dir.display());
        return;
    }

    let mut imported = 0;
    for name in CONFIG_FILES {
        let src = yuzu_dir.join(name);
        let dst = ruzu_dir.join(name);
        if !src.is_file() {
            continue;
        }
        if dst.exists() {
            log::info!("ruzu config {} already exists; keeping it", dst.display());
            continue;
        }
        match fs::copy(&src, &dst) {
            Ok(_) => {
                imported += 1;
                log::info!("Imported yuzu config {} -> {}", src.display(), dst.display());
            }
            Err(e) => log::warn!("Failed to import {}: {e}", src.display()),
        }
    }

    if imported > 0 {
        log::info!(
            "Imported {imported} yuzu config file(s) into {}",
            ruzu_dir.display()
        );
    }
}
