// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK counterpart of Eden `src/qt_common/util/content.cpp`'s firmware
// source helpers (`InstallFirmware(location, recursive)` and
// `UnzipFirmwareToTmp`). Frontend dialogs remain in `main_window.rs`, matching
// Eden's thin `MainWindow::OnInstallFirmware*` ownership.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

fn firmware_cache_dir() -> PathBuf {
    std::env::temp_dir().join("ruzu").join("firmware")
}

/// Collect firmware NCA files, recursively only for an extracted ZIP archive.
pub fn firmware_ncas(location: &Path, recursive: bool) -> io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    collect_firmware_ncas(location, recursive, &mut files)?;
    files.sort();
    Ok(files)
}

fn collect_firmware_ncas(
    location: &Path,
    recursive: bool,
    files: &mut Vec<PathBuf>,
) -> io::Result<()> {
    for entry in fs::read_dir(location)? {
        let entry = entry?;
        let path = entry.path();
        if recursive && path.is_dir() {
            collect_firmware_ncas(&path, true, files)?;
        } else if path.is_file() && path.extension().is_some_and(|extension| extension == "nca") {
            files.push(path);
        }
    }
    Ok(())
}

/// Extract a firmware ZIP into Ruzu's fixed temporary firmware directory.
/// `enclosed_name` rejects entries escaping the extraction root.
pub fn unzip_firmware_to_tmp(location: &Path) -> Result<PathBuf, String> {
    let temporary = firmware_cache_dir();
    if temporary.exists() {
        fs::remove_dir_all(&temporary).map_err(|error| error.to_string())?;
    }
    fs::create_dir_all(&temporary).map_err(|error| error.to_string())?;

    let archive_file = fs::File::open(location).map_err(|error| error.to_string())?;
    let mut archive = zip::ZipArchive::new(archive_file).map_err(|error| error.to_string())?;
    if archive.is_empty() {
        return Err("The firmware ZIP is empty.".to_string());
    }
    for index in 0..archive.len() {
        let mut entry = archive.by_index(index).map_err(|error| error.to_string())?;
        let Some(enclosed_name) = entry.enclosed_name() else {
            return Err("The firmware ZIP contains an unsafe path.".to_string());
        };
        let destination = temporary.join(enclosed_name);
        if entry.is_dir() {
            fs::create_dir_all(&destination).map_err(|error| error.to_string())?;
        } else {
            if let Some(parent) = destination.parent() {
                fs::create_dir_all(parent).map_err(|error| error.to_string())?;
            }
            let mut output = fs::File::create(&destination).map_err(|error| error.to_string())?;
            io::copy(&mut entry, &mut output).map_err(|error| error.to_string())?;
        }
    }
    Ok(temporary)
}

pub fn cleanup_firmware_tmp() -> io::Result<()> {
    let temporary = firmware_cache_dir();
    if temporary.exists() {
        fs::remove_dir_all(temporary)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn recursive_scan_finds_nested_ncas_but_folder_scan_does_not() {
        let temp = tempfile::tempdir().unwrap();
        fs::write(temp.path().join("root.nca"), b"homebrew firmware fixture").unwrap();
        fs::create_dir(temp.path().join("nested")).unwrap();
        fs::write(
            temp.path().join("nested/update.nca"),
            b"homebrew firmware fixture",
        )
        .unwrap();

        assert_eq!(firmware_ncas(temp.path(), false).unwrap().len(), 1);
        assert_eq!(firmware_ncas(temp.path(), true).unwrap().len(), 2);
    }

    #[test]
    fn firmware_zip_rejects_parent_traversal() {
        let temp = tempfile::tempdir().unwrap();
        let archive_path = temp.path().join("firmware.zip");
        let archive_file = fs::File::create(&archive_path).unwrap();
        let mut archive = zip::ZipWriter::new(archive_file);
        archive
            .start_file("../escape.nca", zip::write::SimpleFileOptions::default())
            .unwrap();
        archive.write_all(b"homebrew firmware fixture").unwrap();
        archive.finish().unwrap();

        assert!(unzip_firmware_to_tmp(&archive_path).is_err());
        let _ = cleanup_firmware_tmp();
    }
}
