// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/common/fs/fs.h and zuyu/src/common/fs/fs.cpp
//! Comprehensive filesystem API: file operations, directory operations,
//! and generic filesystem queries.

use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use log::{debug, error};

use super::file::IOFile;
use super::fs_types::*;
use super::fs_util::path_to_utf8_string;
use super::path_util::validate_path;

// =====================
// File Operations
// =====================

/// Creates a new file at path with the specified size.
///
/// Maps to upstream `NewFile`.
pub fn new_file(path: &Path, size: u64) -> bool {
    if !validate_path(path) {
        error!(
            "Input path is not valid, path={}",
            path_to_utf8_string(path)
        );
        return false;
    }

    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() && !exists(parent) {
            error!(
                "Parent directory of path={} does not exist",
                path_to_utf8_string(path)
            );
            return false;
        }
    }

    if exists(path) {
        error!(
            "Filesystem object at path={} exists",
            path_to_utf8_string(path)
        );
        return false;
    }

    let io_file = IOFile::new(
        path,
        FileAccessMode::Write,
        FileType::BinaryFile,
        FileShareFlag::ShareReadOnly,
    );

    if !io_file.is_open() {
        error!(
            "Failed to create a file at path={}",
            path_to_utf8_string(path)
        );
        return false;
    }

    if !io_file.set_size(size) {
        error!(
            "Failed to resize the file at path={} to size={}",
            path_to_utf8_string(path),
            size
        );
        return false;
    }

    debug!(
        "Successfully created a file at path={} with size={}",
        path_to_utf8_string(path),
        size
    );

    true
}

/// Removes a file at path.
/// Returns `true` if file removal succeeds or file does not exist.
///
/// Maps to upstream `RemoveFile`.
pub fn remove_file(path: &Path) -> bool {
    if !validate_path(path) {
        error!(
            "Input path is not valid, path={}",
            path_to_utf8_string(path)
        );
        return false;
    }

    if !exists(path) {
        debug!(
            "Filesystem object at path={} does not exist",
            path_to_utf8_string(path)
        );
        return true;
    }

    if !is_file(path) {
        error!(
            "Filesystem object at path={} is not a file",
            path_to_utf8_string(path)
        );
        return false;
    }

    match fs::remove_file(path) {
        Ok(()) => {
            debug!(
                "Successfully removed the file at path={}",
                path_to_utf8_string(path)
            );
            true
        }
        Err(e) => {
            error!(
                "Failed to remove the file at path={}, ec_message={}",
                path_to_utf8_string(path),
                e
            );
            false
        }
    }
}

/// Renames a file from old_path to new_path.
///
/// Maps to upstream `RenameFile`.
pub fn rename_file(old_path: &Path, new_path: &Path) -> bool {
    if !validate_path(old_path) || !validate_path(new_path) {
        error!(
            "One or both input path(s) is not valid, old_path={}, new_path={}",
            path_to_utf8_string(old_path),
            path_to_utf8_string(new_path)
        );
        return false;
    }

    if !exists(old_path) {
        error!(
            "Filesystem object at old_path={} does not exist",
            path_to_utf8_string(old_path)
        );
        return false;
    }

    if !is_file(old_path) {
        error!(
            "Filesystem object at old_path={} is not a file",
            path_to_utf8_string(old_path)
        );
        return false;
    }

    if exists(new_path) {
        error!(
            "Filesystem object at new_path={} exists",
            path_to_utf8_string(new_path)
        );
        return false;
    }

    match fs::rename(old_path, new_path) {
        Ok(()) => {
            debug!(
                "Successfully renamed the file from old_path={} to new_path={}",
                path_to_utf8_string(old_path),
                path_to_utf8_string(new_path)
            );
            true
        }
        Err(e) => {
            error!(
                "Failed to rename the file from old_path={} to new_path={}, ec_message={}",
                path_to_utf8_string(old_path),
                path_to_utf8_string(new_path),
                e
            );
            false
        }
    }
}

/// Opens a file at path with the specified file access mode.
/// Returns `None` on failure.
///
/// Maps to upstream `FileOpen`.
pub fn file_open(
    path: &Path,
    mode: FileAccessMode,
    file_type: FileType,
    flag: FileShareFlag,
) -> Option<Arc<IOFile>> {
    if !validate_path(path) {
        error!(
            "Input path is not valid, path={}",
            path_to_utf8_string(path)
        );
        return None;
    }

    if exists(path) && !is_file(path) {
        error!(
            "Filesystem object at path={} exists and is not a regular file",
            path_to_utf8_string(path)
        );
        return None;
    }

    let io_file = IOFile::new(path, mode, file_type, flag);

    if !io_file.is_open() {
        error!(
            "Failed to open the file at path={} with mode={}, type={}, flag={}",
            path_to_utf8_string(path),
            mode,
            file_type,
            flag
        );
        return None;
    }

    debug!(
        "Successfully opened the file at path={} with mode={}, type={}, flag={}",
        path_to_utf8_string(path),
        mode,
        file_type,
        flag
    );

    Some(Arc::new(io_file))
}

// =====================
// Directory Operations
// =====================

/// Creates a directory at path.
/// Returns `true` if directory creation succeeds or directory already exists.
///
/// Maps to upstream `CreateDir`.
pub fn create_dir(path: &Path) -> bool {
    if !validate_path(path) {
        error!(
            "Input path is not valid, path={}",
            path_to_utf8_string(path)
        );
        return false;
    }

    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() && !exists(parent) {
            error!(
                "Parent directory of path={} does not exist",
                path_to_utf8_string(path)
            );
            return false;
        }
    }

    if is_dir(path) {
        debug!(
            "Filesystem object at path={} exists and is a directory",
            path_to_utf8_string(path)
        );
        return true;
    }

    match fs::create_dir(path) {
        Ok(()) => {
            debug!(
                "Successfully created the directory at path={}",
                path_to_utf8_string(path)
            );
            true
        }
        Err(e) => {
            error!(
                "Failed to create the directory at path={}, ec_message={}",
                path_to_utf8_string(path),
                e
            );
            false
        }
    }
}

/// Recursively creates a directory at path.
/// Returns `true` if directory creation succeeds or directory already exists.
///
/// Maps to upstream `CreateDirs`.
pub fn create_dirs(path: &Path) -> bool {
    if !validate_path(path) {
        error!(
            "Input path is not valid, path={}",
            path_to_utf8_string(path)
        );
        return false;
    }

    if is_dir(path) {
        debug!(
            "Filesystem object at path={} exists and is a directory",
            path_to_utf8_string(path)
        );
        return true;
    }

    match fs::create_dir_all(path) {
        Ok(()) => {
            debug!(
                "Successfully created the directories at path={}",
                path_to_utf8_string(path)
            );
            true
        }
        Err(e) => {
            error!(
                "Failed to create the directories at path={}, ec_message={}",
                path_to_utf8_string(path),
                e
            );
            false
        }
    }
}

/// Creates the parent directory of a given path.
///
/// Maps to upstream `CreateParentDir`.
pub fn create_parent_dir(path: &Path) -> bool {
    match path.parent() {
        Some(parent) => create_dir(parent),
        None => true,
    }
}

/// Recursively creates the parent directory of a given path.
///
/// Maps to upstream `CreateParentDirs`.
pub fn create_parent_dirs(path: &Path) -> bool {
    match path.parent() {
        Some(parent) => create_dirs(parent),
        None => true,
    }
}

/// Removes a directory at path.
/// Returns `true` if directory removal succeeds or directory does not exist.
///
/// Maps to upstream `RemoveDir`.
pub fn remove_dir(path: &Path) -> bool {
    if !validate_path(path) {
        error!(
            "Input path is not valid, path={}",
            path_to_utf8_string(path)
        );
        return false;
    }

    if !exists(path) {
        debug!(
            "Filesystem object at path={} does not exist",
            path_to_utf8_string(path)
        );
        return true;
    }

    if !is_dir(path) {
        error!(
            "Filesystem object at path={} is not a directory",
            path_to_utf8_string(path)
        );
        return false;
    }

    match fs::remove_dir(path) {
        Ok(()) => {
            debug!(
                "Successfully removed the directory at path={}",
                path_to_utf8_string(path)
            );
            true
        }
        Err(e) => {
            error!(
                "Failed to remove the directory at path={}, ec_message={}",
                path_to_utf8_string(path),
                e
            );
            false
        }
    }
}

/// Removes all the contents within the given directory and removes the directory itself.
///
/// Maps to upstream `RemoveDirRecursively`.
pub fn remove_dir_recursively(path: &Path) -> bool {
    if !validate_path(path) {
        error!(
            "Input path is not valid, path={}",
            path_to_utf8_string(path)
        );
        return false;
    }

    if !exists(path) {
        debug!(
            "Filesystem object at path={} does not exist",
            path_to_utf8_string(path)
        );
        return true;
    }

    if !is_dir(path) {
        error!(
            "Filesystem object at path={} is not a directory",
            path_to_utf8_string(path)
        );
        return false;
    }

    match fs::remove_dir_all(path) {
        Ok(()) => {
            debug!(
                "Successfully removed the directory and its contents at path={}",
                path_to_utf8_string(path)
            );
            true
        }
        Err(e) => {
            error!(
                "Failed to remove the directory and its contents at path={}, ec_message={}",
                path_to_utf8_string(path),
                e
            );
            false
        }
    }
}

/// Removes all the contents within the given directory without removing the directory itself.
///
/// Maps to upstream `RemoveDirContentsRecursively`.
pub fn remove_dir_contents_recursively(path: &Path) -> bool {
    if !validate_path(path) {
        error!(
            "Input path is not valid, path={}",
            path_to_utf8_string(path)
        );
        return false;
    }

    if !exists(path) {
        debug!(
            "Filesystem object at path={} does not exist",
            path_to_utf8_string(path)
        );
        return true;
    }

    if !is_dir(path) {
        error!(
            "Filesystem object at path={} is not a directory",
            path_to_utf8_string(path)
        );
        return false;
    }

    let entries = match fs::read_dir(path) {
        Ok(entries) => entries,
        Err(e) => {
            error!(
                "Failed to completely enumerate the directory at path={}, ec_message={}",
                path_to_utf8_string(path),
                e
            );
            return false;
        }
    };

    for entry_result in entries {
        let entry = match entry_result {
            Ok(e) => e,
            Err(e) => {
                error!(
                    "Failed to completely enumerate the directory at path={}, ec_message={}",
                    path_to_utf8_string(path),
                    e
                );
                return false;
            }
        };

        let entry_path = entry.path();

        if entry_path.is_dir() {
            // Recursively remove directory contents then the directory itself
            if !remove_dir_contents_recursively(&entry_path) {
                return false;
            }
            if let Err(e) = fs::remove_dir(&entry_path) {
                error!(
                    "Failed to remove the filesystem object at path={}, ec_message={}",
                    path_to_utf8_string(&entry_path),
                    e
                );
                return false;
            }
        } else {
            if let Err(e) = fs::remove_file(&entry_path) {
                error!(
                    "Failed to remove the filesystem object at path={}, ec_message={}",
                    path_to_utf8_string(&entry_path),
                    e
                );
                return false;
            }
        }
    }

    debug!(
        "Successfully removed all the contents of the directory at path={}",
        path_to_utf8_string(path)
    );

    true
}

/// Renames a directory from old_path to new_path.
///
/// Maps to upstream `RenameDir`.
pub fn rename_dir(old_path: &Path, new_path: &Path) -> bool {
    if !validate_path(old_path) || !validate_path(new_path) {
        error!(
            "One or both input path(s) is not valid, old_path={}, new_path={}",
            path_to_utf8_string(old_path),
            path_to_utf8_string(new_path)
        );
        return false;
    }

    if !exists(old_path) {
        error!(
            "Filesystem object at old_path={} does not exist",
            path_to_utf8_string(old_path)
        );
        return false;
    }

    if !is_dir(old_path) {
        error!(
            "Filesystem object at old_path={} is not a directory",
            path_to_utf8_string(old_path)
        );
        return false;
    }

    if exists(new_path) {
        error!(
            "Filesystem object at new_path={} exists",
            path_to_utf8_string(new_path)
        );
        return false;
    }

    match fs::rename(old_path, new_path) {
        Ok(()) => {
            debug!(
                "Successfully renamed the file from old_path={} to new_path={}",
                path_to_utf8_string(old_path),
                path_to_utf8_string(new_path)
            );
            true
        }
        Err(e) => {
            error!(
                "Failed to rename the file from old_path={} to new_path={}, ec_message={}",
                path_to_utf8_string(old_path),
                path_to_utf8_string(new_path),
                e
            );
            false
        }
    }
}

/// Iterates over the directory entries of a given directory.
/// Does not iterate over sub-directories.
///
/// Maps to upstream `IterateDirEntries`.
pub fn iterate_dir_entries(path: &Path, callback: &DirEntryCallable, filter: DirEntryFilter) {
    if !validate_path(path) {
        error!(
            "Input path is not valid, path={}",
            path_to_utf8_string(path)
        );
        return;
    }

    if !exists(path) {
        error!(
            "Filesystem object at path={} does not exist",
            path_to_utf8_string(path)
        );
        return;
    }

    if !is_dir(path) {
        error!(
            "Filesystem object at path={} is not a directory",
            path_to_utf8_string(path)
        );
        return;
    }

    let mut callback_error = false;

    let entries = match fs::read_dir(path) {
        Ok(entries) => entries,
        Err(e) => {
            error!(
                "Failed to visit all the directory entries of path={}, ec_message={}",
                path_to_utf8_string(path),
                e
            );
            return;
        }
    };

    for entry_result in entries {
        let entry = match entry_result {
            Ok(e) => e,
            Err(e) => {
                error!(
                    "Failed to visit all the directory entries of path={}, ec_message={}",
                    path_to_utf8_string(path),
                    e
                );
                return;
            }
        };

        let entry_path = entry.path();
        let is_entry_dir = entry_path.is_dir();
        let is_entry_file = entry_path.is_file();

        if filter.contains(DirEntryFilter::FILE) && is_entry_file {
            if !callback(&entry_path, false) {
                callback_error = true;
                break;
            }
        }

        if filter.contains(DirEntryFilter::DIRECTORY) && is_entry_dir {
            if !callback(&entry_path, true) {
                callback_error = true;
                break;
            }
        }
    }

    if callback_error {
        error!(
            "Failed to visit all the directory entries of path={}",
            path_to_utf8_string(path)
        );
        return;
    }

    debug!(
        "Successfully visited all the directory entries of path={}",
        path_to_utf8_string(path)
    );
}

/// Iterates over the directory entries of a given directory and its sub-directories.
///
/// Maps to upstream `IterateDirEntriesRecursively`.
pub fn iterate_dir_entries_recursively(
    path: &Path,
    callback: &DirEntryCallable,
    filter: DirEntryFilter,
) {
    if !validate_path(path) {
        error!(
            "Input path is not valid, path={}",
            path_to_utf8_string(path)
        );
        return;
    }

    if !exists(path) {
        error!(
            "Filesystem object at path={} does not exist",
            path_to_utf8_string(path)
        );
        return;
    }

    if !is_dir(path) {
        error!(
            "Filesystem object at path={} is not a directory",
            path_to_utf8_string(path)
        );
        return;
    }

    let mut callback_error = false;

    let entries = match fs::read_dir(path) {
        Ok(entries) => entries,
        Err(e) => {
            error!(
                "Failed to visit all the directory entries of path={}, ec_message={}",
                path_to_utf8_string(path),
                e
            );
            return;
        }
    };

    for entry_result in entries {
        let entry = match entry_result {
            Ok(e) => e,
            Err(e) => {
                error!(
                    "Failed to visit all the directory entries of path={}, ec_message={}",
                    path_to_utf8_string(path),
                    e
                );
                return;
            }
        };

        let entry_path = entry.path();
        let is_entry_dir = entry_path.is_dir();
        let is_entry_file = entry_path.is_file();

        if filter.contains(DirEntryFilter::FILE) && is_entry_file {
            if !callback(&entry_path, false) {
                callback_error = true;
                break;
            }
        }

        if filter.contains(DirEntryFilter::DIRECTORY) && is_entry_dir {
            if !callback(&entry_path, true) {
                callback_error = true;
                break;
            }
        }

        // Recurse into subdirectories
        if is_entry_dir {
            iterate_dir_entries_recursively(&entry_path, callback, filter);
        }
    }

    if callback_error {
        error!(
            "Failed to visit all the directory entries of path={}",
            path_to_utf8_string(path)
        );
        return;
    }

    debug!(
        "Successfully visited all the directory entries of path={}",
        path_to_utf8_string(path)
    );
}

// =====================
// Generic Filesystem Operations
// =====================

/// Returns whether a filesystem object at path exists.
///
/// Maps to upstream `Exists`.
pub fn exists(path: &Path) -> bool {
    path.exists()
}

/// Returns whether a filesystem object at path is a regular file.
///
/// Maps to upstream `IsFile`.
pub fn is_file(path: &Path) -> bool {
    path.is_file()
}

/// Returns whether a filesystem object at path is a directory.
///
/// Maps to upstream `IsDir`.
pub fn is_dir(path: &Path) -> bool {
    path.is_dir()
}

/// Gets the current working directory.
/// Returns an empty `PathBuf` on failure.
///
/// Maps to upstream `GetCurrentDir`.
pub fn get_current_dir() -> PathBuf {
    match std::env::current_dir() {
        Ok(p) => p,
        Err(e) => {
            error!("Failed to get the current path, ec_message={}", e);
            PathBuf::new()
        }
    }
}

/// Sets the current working directory to path.
///
/// Maps to upstream `SetCurrentDir`.
pub fn set_current_dir(path: &Path) -> bool {
    match std::env::set_current_dir(path) {
        Ok(()) => true,
        Err(e) => {
            error!(
                "Failed to set the current path to path={}, ec_message={}",
                path_to_utf8_string(path),
                e
            );
            false
        }
    }
}

/// Entry type of a filesystem object.
///
/// Maps to upstream `std::filesystem::file_type`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryType {
    NotFound,
    Regular,
    Directory,
    Symlink,
    Other,
}

/// Gets the entry type of the filesystem object at path.
///
/// Maps to upstream `GetEntryType`.
pub fn get_entry_type(path: &Path) -> EntryType {
    match fs::symlink_metadata(path) {
        Ok(metadata) => {
            let ft = metadata.file_type();
            if ft.is_file() {
                EntryType::Regular
            } else if ft.is_dir() {
                EntryType::Directory
            } else if ft.is_symlink() {
                EntryType::Symlink
            } else {
                EntryType::Other
            }
        }
        Err(e) => {
            error!(
                "Failed to retrieve the entry type of path={}, ec_message={}",
                path_to_utf8_string(path),
                e
            );
            EntryType::NotFound
        }
    }
}

/// Gets the size of the filesystem object at path.
/// Returns 0 on failure.
///
/// Maps to upstream `GetSize`.
pub fn get_size(path: &Path) -> u64 {
    match fs::metadata(path) {
        Ok(metadata) => metadata.len(),
        Err(e) => {
            error!(
                "Failed to retrieve the file size of path={}, ec_message={}",
                path_to_utf8_string(path),
                e
            );
            0
        }
    }
}

/// Gets the free space size of the filesystem at path.
/// Returns 0 on failure.
///
/// Maps to upstream `GetFreeSpaceSize`.
///
/// Note: Uses platform-specific `statvfs` on Unix. On platforms where this
/// is not available, returns 0.
pub fn get_free_space_size(path: &Path) -> u64 {
    #[cfg(unix)]
    {
        use std::ffi::CString;
        use std::os::unix::ffi::OsStrExt;

        let c_path = match CString::new(path.as_os_str().as_bytes()) {
            Ok(p) => p,
            Err(_) => return 0,
        };

        unsafe {
            let mut stat: libc::statvfs = std::mem::zeroed();
            if libc::statvfs(c_path.as_ptr(), &mut stat) == 0 {
                stat.f_bfree as u64 * stat.f_frsize as u64
            } else {
                error!(
                    "Failed to retrieve the available free space of path={}",
                    path_to_utf8_string(path)
                );
                0
            }
        }
    }

    #[cfg(not(unix))]
    {
        error!(
            "Failed to retrieve the available free space of path={}",
            path_to_utf8_string(path)
        );
        0
    }
}

/// Gets the total capacity of the filesystem at path.
/// Returns 0 on failure.
///
/// Maps to upstream `GetTotalSpaceSize`.
pub fn get_total_space_size(path: &Path) -> u64 {
    #[cfg(unix)]
    {
        use std::ffi::CString;
        use std::os::unix::ffi::OsStrExt;

        let c_path = match CString::new(path.as_os_str().as_bytes()) {
            Ok(p) => p,
            Err(_) => return 0,
        };

        unsafe {
            let mut stat: libc::statvfs = std::mem::zeroed();
            if libc::statvfs(c_path.as_ptr(), &mut stat) == 0 {
                stat.f_blocks as u64 * stat.f_frsize as u64
            } else {
                error!(
                    "Failed to retrieve the total capacity of path={}",
                    path_to_utf8_string(path)
                );
                0
            }
        }
    }

    #[cfg(not(unix))]
    {
        error!(
            "Failed to retrieve the total capacity of path={}",
            path_to_utf8_string(path)
        );
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_dir() -> PathBuf {
        std::env::temp_dir().join("ruzu_test_fs")
    }

    #[test]
    fn test_create_and_remove_dir() {
        let dir = test_dir().join("test_create_remove");
        let _ = fs::create_dir_all(dir.parent().unwrap());

        assert!(create_dir(&dir));
        assert!(is_dir(&dir));
        assert!(remove_dir(&dir));
        assert!(!exists(&dir));
    }

    #[test]
    fn test_create_dirs() {
        let dir = test_dir().join("a/b/c");
        assert!(create_dirs(&dir));
        assert!(is_dir(&dir));
        let _ = fs::remove_dir_all(test_dir().join("a"));
    }

    #[test]
    fn test_new_file_and_remove() {
        let dir = test_dir();
        let _ = fs::create_dir_all(&dir);
        let path = dir.join("test_new_file.bin");
        let _ = fs::remove_file(&path);

        assert!(new_file(&path, 256));
        assert!(is_file(&path));
        assert_eq!(get_size(&path), 256);
        assert!(remove_file(&path));
        assert!(!exists(&path));
    }

    #[test]
    fn test_rename_file() {
        let dir = test_dir();
        let _ = fs::create_dir_all(&dir);
        let old = dir.join("rename_old.txt");
        let new = dir.join("rename_new.txt");
        let _ = fs::remove_file(&old);
        let _ = fs::remove_file(&new);

        fs::write(&old, "content").unwrap();
        assert!(rename_file(&old, &new));
        assert!(!exists(&old));
        assert!(is_file(&new));
        let _ = fs::remove_file(&new);
    }

    #[test]
    fn test_exists_and_is_queries() {
        let dir = test_dir();
        let _ = fs::create_dir_all(&dir);
        let path = dir.join("query_test.txt");
        fs::write(&path, "data").unwrap();

        assert!(exists(&path));
        assert!(is_file(&path));
        assert!(!is_dir(&path));
        assert!(is_dir(&dir));
        assert!(!is_file(&dir));

        let _ = fs::remove_file(&path);
    }
}
