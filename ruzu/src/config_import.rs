// SPDX-License-Identifier: GPL-3.0-or-later
//
// One-time offer to import an existing yuzu configuration into ruzu.
//
// ruzu reuses yuzu's INI config schema (`frontend_common::config` is a port of
// yuzu's `Config`), and its config directory is a sibling of yuzu's under the
// same XDG base (`…/ruzu` vs `…/yuzu`). On first run, if ruzu has no
// configuration of its own and a yuzu one exists, the user is *asked* whether
// to bring their settings over — the copy never happens behind their back.
//
// ruzu only ever reads yuzu's directory; everything it writes goes to its own.
//
// A marker file records that the offer was made, so it happens exactly once
// whichever way the user answers.

use std::fs;
use std::path::{Path, PathBuf};

use common::fs::path_util::{get_data_directory, get_ruzu_path, RuzuPath};

/// Marker written after the offer is made, so it is never repeated.
const IMPORT_MARKER: &str = ".yuzu-import-done";

/// Config files shared verbatim between yuzu and ruzu (identical INI schema):
/// the GUI config and the SDL/CLI config.
const CONFIG_FILES: &[&str] = &["qt-config.ini", "sdl2-config.ini"];

/// A yuzu configuration that ruzu could import, found on a first run.
pub struct AvailableImport {
    /// yuzu's config directory — read only, never written to.
    pub yuzu_dir: PathBuf,
    /// ruzu's own config directory, where the copies land.
    ruzu_dir: PathBuf,
}

/// Look for a yuzu configuration worth offering to import.
///
/// Returns `None` — meaning "say nothing, just start" — when any of these hold:
///  * the offer was already made on a previous run (marker present);
///  * ruzu already has a configuration of its own, so it is not a first run;
///  * no yuzu configuration exists;
///  * both directories resolve to the same path, in which case importing would
///    mean writing into yuzu's directory.
pub fn available_import() -> Option<AvailableImport> {
    let ruzu_dir = get_ruzu_path(RuzuPath::ConfigDir);

    if ruzu_dir.join(IMPORT_MARKER).exists() {
        return None;
    }
    if has_config(&ruzu_dir) {
        log::debug!("ruzu is already configured; not offering a yuzu import");
        return None;
    }

    let yuzu_dir = yuzu_config_dir()?;
    if same_dir(&yuzu_dir, &ruzu_dir) {
        log::warn!(
            "yuzu and ruzu config directories resolve to the same path ({}); \
             not offering an import so nothing is written into yuzu's directory",
            yuzu_dir.display()
        );
        return None;
    }

    log::info!("Found an importable yuzu configuration at {}", yuzu_dir.display());
    Some(AvailableImport { yuzu_dir, ruzu_dir })
}

impl AvailableImport {
    /// Copy yuzu's configuration into ruzu's directory, then record that the
    /// offer was answered.
    pub fn accept(&self) {
        import_from(&self.yuzu_dir, &self.ruzu_dir);
        self.mark_answered();
    }

    /// Record that the user declined, so the offer is not repeated.
    pub fn decline(&self) {
        log::info!("yuzu configuration import declined");
        self.mark_answered();
    }

    /// Write the marker into **ruzu's** directory.
    fn mark_answered(&self) {
        if let Err(e) = fs::create_dir_all(&self.ruzu_dir) {
            log::warn!(
                "Could not create ruzu config dir {}: {e}",
                self.ruzu_dir.display()
            );
            return;
        }
        let marker = self.ruzu_dir.join(IMPORT_MARKER);
        if let Err(e) = fs::write(&marker, b"answered\n") {
            log::warn!("Could not write import marker {}: {e}", marker.display());
        }
    }
}

/// Whether `dir` holds any of the configuration files ruzu reads.
fn has_config(dir: &Path) -> bool {
    CONFIG_FILES.iter().any(|name| dir.join(name).is_file())
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

    candidates.into_iter().find(|dir| has_config(dir))
}

/// Whether two paths denote the same directory, resolving symlinks where
/// possible so a symlinked config dir is still recognised as the same target.
fn same_dir(a: &Path, b: &Path) -> bool {
    match (fs::canonicalize(a), fs::canonicalize(b)) {
        (Ok(a), Ok(b)) => a == b,
        // A directory that does not exist yet cannot alias an existing one.
        _ => a == b,
    }
}

/// Copy any config files that yuzu has and ruzu does not yet have.
fn import_from(yuzu_dir: &Path, ruzu_dir: &Path) {
    if let Err(e) = fs::create_dir_all(ruzu_dir) {
        log::warn!(
            "Could not create ruzu config dir {}: {e}",
            ruzu_dir.display()
        );
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

    log::info!(
        "Imported {imported} yuzu config file(s) into {}",
        ruzu_dir.display()
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn has_config_detects_either_config_file() {
        let dir = tempdir();
        assert!(!has_config(&dir));
        fs::write(dir.join("qt-config.ini"), b"[UI]\n").unwrap();
        assert!(has_config(&dir));

        let dir2 = tempdir();
        fs::write(dir2.join("sdl2-config.ini"), b"[UI]\n").unwrap();
        assert!(has_config(&dir2));
    }

    #[test]
    fn import_copies_config_and_never_writes_to_the_source() {
        let yuzu = tempdir();
        let ruzu = tempdir();
        fs::write(yuzu.join("qt-config.ini"), b"[UI]\nfoo=bar\n").unwrap();

        let before: Vec<_> = listing(&yuzu);
        import_from(&yuzu, &ruzu);

        assert_eq!(
            fs::read_to_string(ruzu.join("qt-config.ini")).unwrap(),
            "[UI]\nfoo=bar\n"
        );
        // The source directory must be untouched — same entries, same content.
        assert_eq!(listing(&yuzu), before);
    }

    #[test]
    fn import_never_clobbers_an_existing_ruzu_config() {
        let yuzu = tempdir();
        let ruzu = tempdir();
        fs::write(yuzu.join("qt-config.ini"), b"from-yuzu\n").unwrap();
        fs::write(ruzu.join("qt-config.ini"), b"mine\n").unwrap();

        import_from(&yuzu, &ruzu);

        assert_eq!(
            fs::read_to_string(ruzu.join("qt-config.ini")).unwrap(),
            "mine\n"
        );
    }

    #[test]
    fn marker_is_written_into_ruzus_directory_not_yuzus() {
        let yuzu = tempdir();
        let ruzu = tempdir();
        fs::write(yuzu.join("qt-config.ini"), b"x\n").unwrap();

        let import = AvailableImport {
            yuzu_dir: yuzu.clone(),
            ruzu_dir: ruzu.clone(),
        };
        import.decline();

        assert!(ruzu.join(IMPORT_MARKER).exists());
        assert!(
            !yuzu.join(IMPORT_MARKER).exists(),
            "the marker must never land in yuzu's directory"
        );
    }

    #[test]
    fn declining_does_not_copy_anything() {
        let yuzu = tempdir();
        let ruzu = tempdir();
        fs::write(yuzu.join("qt-config.ini"), b"x\n").unwrap();

        AvailableImport {
            yuzu_dir: yuzu,
            ruzu_dir: ruzu.clone(),
        }
        .decline();

        assert!(!ruzu.join("qt-config.ini").exists());
    }

    #[test]
    fn same_dir_detects_aliasing() {
        let dir = tempdir();
        assert!(same_dir(&dir, &dir));
        assert!(!same_dir(&dir, &tempdir()));
    }

    /// A unique empty directory under the system temp dir.
    fn tempdir() -> PathBuf {
        use std::sync::atomic::{AtomicU32, Ordering};
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        let path = std::env::temp_dir().join(format!(
            "ruzu-config-import-test-{}-{}",
            std::process::id(),
            COUNTER.fetch_add(1, Ordering::Relaxed)
        ));
        let _ = fs::remove_dir_all(&path);
        fs::create_dir_all(&path).unwrap();
        path
    }

    /// `(file name, contents)` for every file in `dir`, sorted.
    fn listing(dir: &Path) -> Vec<(String, Vec<u8>)> {
        let mut out: Vec<_> = fs::read_dir(dir)
            .unwrap()
            .filter_map(Result::ok)
            .map(|e| {
                (
                    e.file_name().to_string_lossy().into_owned(),
                    fs::read(e.path()).unwrap_or_default(),
                )
            })
            .collect();
        out.sort();
        out
    }
}
