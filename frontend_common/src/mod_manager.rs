// SPDX-FileCopyrightText: Copyright 2026 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of Eden `src/frontend_common/mod_manager.{h,cpp}`.

use std::fs;
use std::path::{Path, PathBuf};

use common::fs::path_util::{get_ruzu_path, RuzuPath};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModInstallResult {
    Cancelled,
    Failed,
    Success,
}

const VALID_MOD_DIRECTORIES: [&str; 5] = ["exefs", "romfs", "romfs_ext", "cheats", "romfslite"];

/// Eden `FrontendCommon::GetModFolder`.
pub fn get_mod_folder(root: &Path) -> Vec<PathBuf> {
    let mut paths = Vec::new();
    collect_mod_folders(root, &mut paths);
    paths
}

fn collect_mod_folders(directory: &Path, paths: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(directory) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        if path
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| VALID_MOD_DIRECTORIES.contains(&name))
        {
            if let Some(parent) = path.parent() {
                let parent = parent.to_path_buf();
                if !paths.contains(&parent) {
                    paths.push(parent);
                }
            }
        }
        collect_mod_folders(&path, paths);
    }
}

/// Eden `FrontendCommon::InstallMod`.
pub fn install_mod(path: &Path, program_id: u64, copy: bool) -> ModInstallResult {
    let Some(mod_name) = path.file_name() else {
        return ModInstallResult::Failed;
    };
    let mod_dir = get_ruzu_path(RuzuPath::LoadDir)
        .join(format!("{program_id:016X}"))
        .join(mod_name);

    if let Err(error) = fs::remove_dir_all(&mod_dir) {
        if error.kind() != std::io::ErrorKind::NotFound {
            log::error!(
                "Mod install failed while replacing {}: {error}",
                mod_dir.display()
            );
            return ModInstallResult::Failed;
        }
    }

    if let Err(error) = copy_directory(path, &mod_dir) {
        log::error!("Mod install failed with message {error}");
        return ModInstallResult::Failed;
    }
    if !copy {
        if let Err(error) = fs::remove_dir_all(path) {
            log::error!(
                "Mod install failed while removing {}: {error}",
                path.display()
            );
            return ModInstallResult::Failed;
        }
    }

    log::info!(
        "Copied mod from {} to {}",
        path.display(),
        mod_dir.display()
    );
    ModInstallResult::Success
}

fn copy_directory(source: &Path, destination: &Path) -> std::io::Result<()> {
    fs::create_dir_all(destination)?;
    for entry in fs::read_dir(source)? {
        let entry = entry?;
        let source_path = entry.path();
        let destination_path = destination.join(entry.file_name());
        if source_path.is_dir() {
            copy_directory(&source_path, &destination_path)?;
        } else {
            fs::copy(source_path, destination_path)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn discovers_each_mod_root_once() {
        let temporary = tempfile::tempdir().unwrap();
        let first = temporary.path().join("First");
        let second = temporary.path().join("nested").join("Second");
        fs::create_dir_all(first.join("exefs")).unwrap();
        fs::create_dir_all(first.join("romfs")).unwrap();
        fs::create_dir_all(second.join("romfslite")).unwrap();
        fs::create_dir_all(temporary.path().join("not-a-mod")).unwrap();

        let found = get_mod_folder(temporary.path());
        assert_eq!(found.iter().filter(|path| *path == &first).count(), 1);
        assert_eq!(found.iter().filter(|path| *path == &second).count(), 1);
        assert_eq!(found.len(), 2);
    }

    #[test]
    fn copies_directory_recursively() {
        let temporary = tempfile::tempdir().unwrap();
        let source = temporary.path().join("Example");
        let destination = temporary.path().join("Destination");
        fs::create_dir_all(source.join("romfs").join("nested")).unwrap();
        fs::write(source.join("romfs").join("nested").join("file.bin"), b"mod").unwrap();

        copy_directory(&source, &destination).unwrap();
        assert_eq!(
            fs::read(destination.join("romfs").join("nested").join("file.bin")).unwrap(),
            b"mod"
        );
    }
}
