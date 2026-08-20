// SPDX-FileCopyrightText: Copyright 2026 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Rust counterpart of Eden `frontend_common/firmware_manager.{h,cpp}`.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use common::fs::path_util::{get_ruzu_path, RuzuPath};
use ruzu_core::crypto::key_manager::KeyManager;

/// Upstream `FirmwareManager::KeyInstallResult`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KeyInstallResult {
    Success,
    InvalidDir,
    ErrorFailedCopy,
    ErrorWrongFilename,
    ErrorFailedInit,
}

/// Upstream `FirmwareManager::InstallKeys`.
pub fn install_keys(location: &Path, expected_extension: &str) -> KeyInstallResult {
    log::info!("Installing key files from {}", location.display());

    let result = copy_key_files_to(
        location,
        expected_extension,
        &get_ruzu_path(RuzuPath::KeysDir),
    );
    if result != KeyInstallResult::Success {
        return result;
    }

    KeyManager::instance().lock().unwrap().reload_keys();
    if crate::content_manager::are_keys_present() {
        KeyInstallResult::Success
    } else {
        KeyInstallResult::ErrorFailedInit
    }
}

fn copy_key_files_to(
    location: &Path,
    expected_extension: &str,
    keys_dir: &Path,
) -> KeyInstallResult {
    if !location.to_string_lossy().ends_with(expected_extension) {
        return KeyInstallResult::ErrorWrongFilename;
    }

    let Some(source_dir) = location.parent() else {
        return KeyInstallResult::InvalidDir;
    };
    if !source_dir.is_dir() {
        return KeyInstallResult::InvalidDir;
    }

    let mut source_key_files = Vec::<PathBuf>::new();
    if location.is_file() {
        source_key_files.push(location.to_path_buf());
    }
    for optional in ["title.keys", "key_retail.bin"] {
        let candidate = source_dir.join(optional);
        if candidate.is_file() {
            source_key_files.push(candidate);
        }
    }
    if source_key_files.is_empty() || !location.is_file() {
        return KeyInstallResult::ErrorWrongFilename;
    }

    if let Err(error) = ensure_keys_directory(keys_dir) {
        log::error!(
            "Could not prepare keys directory {}: {error}",
            keys_dir.display()
        );
        return KeyInstallResult::ErrorFailedCopy;
    }

    for key_file in source_key_files {
        let Some(filename) = key_file.file_name() else {
            return KeyInstallResult::ErrorFailedCopy;
        };
        let destination = keys_dir.join(filename);
        // Rust's copy operation must not receive the same source and
        // destination. Keeping this no-op guard also preserves keys selected
        // directly from the installed directory.
        if same_file(&key_file, &destination) {
            continue;
        }
        if let Err(error) = fs::copy(&key_file, &destination) {
            log::error!(
                "Failed to copy file {} to {}: {error}",
                key_file.display(),
                destination.display()
            );
            return KeyInstallResult::ErrorFailedCopy;
        }
    }

    KeyInstallResult::Success
}

/// Ruzu's Share migration can leave a dangling directory link when its source
/// emulator is moved. Preserve valid links; replace only a link whose target no
/// longer exists. Ordinary files and non-directory targets remain errors.
fn ensure_keys_directory(keys_dir: &Path) -> io::Result<()> {
    let metadata = match fs::symlink_metadata(keys_dir) {
        Ok(metadata) => Some(metadata),
        Err(error) if error.kind() == io::ErrorKind::NotFound => None,
        Err(error) => return Err(error),
    };

    let Some(metadata) = metadata else {
        return fs::create_dir_all(keys_dir);
    };

    if is_directory_link(&metadata) {
        return match fs::metadata(keys_dir) {
            Ok(target) if target.is_dir() => Ok(()),
            Ok(_) => Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!(
                    "keys link target is not a directory: {}",
                    keys_dir.display()
                ),
            )),
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                remove_directory_link(keys_dir, &metadata)?;
                fs::create_dir_all(keys_dir)
            }
            Err(error) => Err(error),
        };
    }

    if metadata.is_dir() {
        Ok(())
    } else {
        Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            format!("keys path is not a directory: {}", keys_dir.display()),
        ))
    }
}

#[cfg(unix)]
fn is_directory_link(metadata: &fs::Metadata) -> bool {
    metadata.file_type().is_symlink()
}

#[cfg(windows)]
fn is_directory_link(metadata: &fs::Metadata) -> bool {
    use std::os::windows::fs::MetadataExt;

    const FILE_ATTRIBUTE_REPARSE_POINT: u32 = 0x0000_0400;
    metadata.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0
}

#[cfg(not(any(unix, windows)))]
fn is_directory_link(_metadata: &fs::Metadata) -> bool {
    false
}

#[cfg(unix)]
fn remove_directory_link(path: &Path, _metadata: &fs::Metadata) -> io::Result<()> {
    fs::remove_file(path)
}

#[cfg(windows)]
fn remove_directory_link(path: &Path, metadata: &fs::Metadata) -> io::Result<()> {
    if metadata.is_dir() {
        fs::remove_dir(path)
    } else {
        fs::remove_file(path)
    }
}

#[cfg(not(any(unix, windows)))]
fn remove_directory_link(path: &Path, _metadata: &fs::Metadata) -> io::Result<()> {
    fs::remove_file(path)
}

fn same_file(first: &Path, second: &Path) -> bool {
    match (fs::canonicalize(first), fs::canonicalize(second)) {
        (Ok(first), Ok(second)) => first == second,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn copies_the_selected_and_adjacent_key_files_with_overwrite() {
        let root = tempfile::tempdir().unwrap();
        let source = root.path().join("source");
        let destination = root.path().join("destination");
        fs::create_dir_all(&source).unwrap();
        fs::create_dir_all(&destination).unwrap();
        fs::write(source.join("prod.keys"), b"new prod").unwrap();
        fs::write(source.join("title.keys"), b"new title").unwrap();
        fs::write(source.join("key_retail.bin"), b"new retail").unwrap();
        fs::write(destination.join("prod.keys"), b"old prod").unwrap();

        assert_eq!(
            copy_key_files_to(&source.join("prod.keys"), "keys", &destination),
            KeyInstallResult::Success
        );
        assert_eq!(
            fs::read(destination.join("prod.keys")).unwrap(),
            b"new prod"
        );
        assert_eq!(
            fs::read(destination.join("title.keys")).unwrap(),
            b"new title"
        );
        assert_eq!(
            fs::read(destination.join("key_retail.bin")).unwrap(),
            b"new retail"
        );
    }

    #[cfg(unix)]
    #[test]
    fn replaces_a_broken_share_link_with_a_real_keys_directory() {
        let root = tempfile::tempdir().unwrap();
        let source = root.path().join("source");
        let destination = root.path().join("keys");
        fs::create_dir_all(&source).unwrap();
        fs::write(source.join("prod.keys"), b"prod").unwrap();
        std::os::unix::fs::symlink(root.path().join("missing-source"), &destination).unwrap();

        assert_eq!(
            copy_key_files_to(&source.join("prod.keys"), "keys", &destination),
            KeyInstallResult::Success
        );
        assert!(fs::symlink_metadata(&destination)
            .unwrap()
            .file_type()
            .is_dir());
        assert_eq!(fs::read(destination.join("prod.keys")).unwrap(), b"prod");
    }

    #[cfg(unix)]
    #[test]
    fn preserves_a_valid_share_link_and_writes_through_it() {
        let root = tempfile::tempdir().unwrap();
        let source = root.path().join("source");
        let shared = root.path().join("shared");
        let destination = root.path().join("keys");
        fs::create_dir_all(&source).unwrap();
        fs::create_dir_all(&shared).unwrap();
        fs::write(source.join("prod.keys"), b"prod").unwrap();
        std::os::unix::fs::symlink(&shared, &destination).unwrap();

        assert_eq!(
            copy_key_files_to(&source.join("prod.keys"), "keys", &destination),
            KeyInstallResult::Success
        );
        assert!(fs::symlink_metadata(&destination)
            .unwrap()
            .file_type()
            .is_symlink());
        assert_eq!(fs::read(shared.join("prod.keys")).unwrap(), b"prod");
    }
}
