// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/common/fs/path_util.h and zuyu/src/common/fs/path_util.cpp
//! Path manipulation functions and path management for ruzu directories.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};

use log::{debug, error, info};

use super::fs;
use super::fs_paths::*;
use super::fs_util::path_to_utf8_string;

/// Maximum path length.
/// On Windows this is the max number of UTF-16 code units (260).
/// On other OSes this is the max number of UTF-8 code units (1024).
#[cfg(windows)]
const MAX_PATH: usize = 260;
#[cfg(not(windows))]
const MAX_PATH: usize = 1024;

/// Well-known ruzu data directory paths.
///
/// Maps to upstream `YuzuPath`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuzuPath {
    /// Where ruzu stores its data.
    RuzuDir,
    /// Where Amiibo backups are stored.
    AmiiboDir,
    /// Where cached filesystem data is stored.
    CacheDir,
    /// Where config files are stored.
    ConfigDir,
    /// Where crash dumps are stored.
    CrashDumpsDir,
    /// Where dumped data is stored.
    DumpDir,
    /// Where key files are stored.
    KeysDir,
    /// Where cheat/mod files are stored.
    LoadDir,
    /// Where log files are stored.
    LogDir,
    /// Where the emulated NAND is stored.
    NANDDir,
    /// Where play time data is stored.
    PlayTimeDir,
    /// Where ruzu screenshots are stored.
    ScreenshotsDir,
    /// Where the emulated SDMC is stored.
    SDMCDir,
    /// Where shaders are stored.
    ShaderDir,
    /// Where TAS scripts are stored.
    TASDir,
    /// Where Icons for shortcuts are stored.
    IconsDir,
}

/// The PathManager manages the mapping of RuzuPath enums to real filesystem paths.
///
/// Maps to upstream `PathManagerImpl`.
struct PathManager {
    ruzu_paths: HashMap<RuzuPath, PathBuf>,
}

impl PathManager {
    fn new() -> Self {
        let mut manager = Self {
            ruzu_paths: HashMap::new(),
        };
        manager.reinitialize(None);
        manager
    }

    fn get_ruzu_path_impl(&self, ruzu_path: RuzuPath) -> &Path {
        self.ruzu_paths
            .get(&ruzu_path)
            .map(|p| p.as_path())
            .unwrap_or(Path::new(""))
    }

    fn set_ruzu_path_impl(&mut self, ruzu_path: RuzuPath, new_path: PathBuf) {
        self.ruzu_paths.insert(ruzu_path, new_path);
    }

    fn reinitialize(&mut self, ruzu_path_override: Option<PathBuf>) {
        let ruzu_path;
        let ruzu_path_cache;
        let ruzu_path_config;

        if let Some(override_path) = ruzu_path_override {
            ruzu_path = override_path;
            ruzu_path_cache = ruzu_path.join(CACHE_DIR);
            ruzu_path_config = ruzu_path.join(CONFIG_DIR);
        } else {
            // Use XDG directories on Unix, or a reasonable default
            let data_dir = get_data_directory("XDG_DATA_HOME");
            let candidate = data_dir.join(RUZU_DIR);

            if fs::exists(&candidate) && fs::is_dir(&candidate) {
                ruzu_path = candidate;
                ruzu_path_cache = ruzu_path.join(CACHE_DIR);
                ruzu_path_config = ruzu_path.join(CONFIG_DIR);
            } else {
                ruzu_path = get_data_directory("XDG_DATA_HOME").join(RUZU_DIR);
                ruzu_path_cache = get_data_directory("XDG_CACHE_HOME").join(RUZU_DIR);
                ruzu_path_config = get_data_directory("XDG_CONFIG_HOME").join(RUZU_DIR);
            }
        }

        self.generate_ruzu_path(RuzuPath::RuzuDir, &ruzu_path);
        self.generate_ruzu_path(RuzuPath::AmiiboDir, &ruzu_path.join(AMIIBO_DIR));
        self.generate_ruzu_path(RuzuPath::CacheDir, &ruzu_path_cache);
        self.generate_ruzu_path(RuzuPath::ConfigDir, &ruzu_path_config);
        self.generate_ruzu_path(RuzuPath::CrashDumpsDir, &ruzu_path.join(CRASH_DUMPS_DIR));
        self.generate_ruzu_path(RuzuPath::DumpDir, &ruzu_path.join(DUMP_DIR));
        self.generate_ruzu_path(RuzuPath::KeysDir, &ruzu_path.join(KEYS_DIR));
        self.generate_ruzu_path(RuzuPath::LoadDir, &ruzu_path.join(LOAD_DIR));
        self.generate_ruzu_path(RuzuPath::LogDir, &ruzu_path.join(LOG_DIR));
        self.generate_ruzu_path(RuzuPath::NANDDir, &ruzu_path.join(NAND_DIR));
        self.generate_ruzu_path(RuzuPath::PlayTimeDir, &ruzu_path.join(PLAY_TIME_DIR));
        self.generate_ruzu_path(RuzuPath::ScreenshotsDir, &ruzu_path.join(SCREENSHOTS_DIR));
        self.generate_ruzu_path(RuzuPath::SDMCDir, &ruzu_path.join(SDMC_DIR));
        self.generate_ruzu_path(RuzuPath::ShaderDir, &ruzu_path.join(SHADER_DIR));
        self.generate_ruzu_path(RuzuPath::TASDir, &ruzu_path.join(TAS_DIR));
        self.generate_ruzu_path(RuzuPath::IconsDir, &ruzu_path.join(ICONS_DIR));
    }

    fn generate_ruzu_path(&mut self, ruzu_path: RuzuPath, new_path: &Path) {
        let _ = fs::create_dir(new_path);
        self.set_ruzu_path_impl(ruzu_path, new_path.to_path_buf());
    }
}

fn path_manager() -> &'static Mutex<PathManager> {
    static INSTANCE: OnceLock<Mutex<PathManager>> = OnceLock::new();
    INSTANCE.get_or_init(|| Mutex::new(PathManager::new()))
}

// =====================
// Public path validation/manipulation functions
// =====================

/// Validates a given path.
/// A path is valid if it is not empty and not too long.
///
/// Maps to upstream `ValidatePath`.
pub fn validate_path(path: &Path) -> bool {
    let path_str = path_to_utf8_string(path);

    if path_str.is_empty() {
        error!("Input path is empty, path={}", path_str);
        return false;
    }

    if path_str.len() >= MAX_PATH {
        error!("Input path is too long, path={}", path_str);
        return false;
    }

    true
}

/// Concatenates two filesystem paths together.
///
/// This handles the case where the second path starts with a directory separator,
/// which would normally replace the first path in `std::path::Path::join`.
///
/// Maps to upstream `ConcatPath`.
pub fn concat_path(first: &Path, second: &Path) -> PathBuf {
    let second_str = second.to_string_lossy();

    if second_str.is_empty() {
        return first.to_path_buf();
    }

    let first_char = second_str.chars().next().unwrap();
    let has_dir_sep = is_dir_separator_char(first_char);

    let result = if !has_dir_sep {
        first.join(second)
    } else {
        // Append directly without treating the leading separator as absolute
        let mut concat = first.to_path_buf();
        let trimmed = second_str.trim_start_matches(|c| c == '/' || c == '\\');
        concat.push(trimmed);
        concat
    };

    // Normalize the path (lexically_normal equivalent)
    normalize_path(&result)
}

/// Safe variant of `concat_path` that ensures the result is sandboxed within the base path.
///
/// Maps to upstream `ConcatPathSafe`.
pub fn concat_path_safe(base: &Path, offset: &Path) -> PathBuf {
    let concatenated_path = concat_path(base, offset);

    if !is_path_sandboxed(base, &concatenated_path) {
        return base.to_path_buf();
    }

    concatenated_path
}

/// Checks whether a given path is sandboxed within a given base path.
///
/// Maps to upstream `IsPathSandboxed`.
pub fn is_path_sandboxed(base: &Path, path: &Path) -> bool {
    let base_string = path_to_utf8_string(&remove_trailing_separators(
        &normalize_path(base),
    ));
    let path_string = path_to_utf8_string(&remove_trailing_separators(
        &normalize_path(path),
    ));

    if path_string.len() < base_string.len() {
        return false;
    }

    path_string.starts_with(&base_string)
}

/// Checks if a character is a directory separator (either '/' or '\\').
///
/// Maps to upstream `IsDirSeparator`.
pub fn is_dir_separator(character: u8) -> bool {
    character == b'/' || character == b'\\'
}

/// Checks if a char is a directory separator.
///
/// Maps to upstream `IsDirSeparator` (char8_t overload).
pub fn is_dir_separator_char(character: char) -> bool {
    character == '/' || character == '\\'
}

/// Removes any trailing directory separators from the given path.
///
/// Maps to upstream `RemoveTrailingSeparators`.
pub fn remove_trailing_separators(path: &Path) -> PathBuf {
    let s = path_to_utf8_string(path);
    if s.is_empty() {
        return PathBuf::new();
    }
    let trimmed = s.trim_end_matches(|c| c == '/' || c == '\\');
    PathBuf::from(trimmed)
}

/// Sets the directory used for application storage.
///
/// Maps to upstream `SetAppDirectory`.
pub fn set_app_directory(app_directory: &str) {
    let mut manager = path_manager().lock().unwrap();
    manager.reinitialize(Some(PathBuf::from(app_directory)));
}

/// Gets the filesystem path associated with the RuzuPath enum.
///
/// Maps to upstream `GetYuzuPath`.
pub fn get_ruzu_path(ruzu_path: RuzuPath) -> PathBuf {
    let manager = path_manager().lock().unwrap();
    manager.get_ruzu_path_impl(ruzu_path).to_path_buf()
}

/// Gets the filesystem path associated with the RuzuPath enum as a UTF-8 string.
///
/// Maps to upstream `GetYuzuPathString`.
pub fn get_ruzu_path_string(ruzu_path: RuzuPath) -> String {
    path_to_utf8_string(&get_ruzu_path(ruzu_path))
}

/// Sets a new filesystem path associated with the RuzuPath enum.
/// If the filesystem object at new_path is not a directory, this does nothing.
///
/// Maps to upstream `SetYuzuPath`.
pub fn set_ruzu_path(ruzu_path: RuzuPath, new_path: &Path) {
    if !fs::is_dir(new_path) {
        error!(
            "Filesystem object at new_path={} is not a directory",
            path_to_utf8_string(new_path)
        );
        return;
    }

    let mut manager = path_manager().lock().unwrap();
    manager.set_ruzu_path_impl(ruzu_path, new_path.to_path_buf());
}

/// Gets the home directory of the current user.
///
/// Maps to upstream `GetHomeDirectory`.
#[cfg(unix)]
pub fn get_home_directory() -> PathBuf {
    if let Ok(home) = std::env::var("HOME") {
        return PathBuf::from(home);
    }

    info!(
        "$HOME is not defined in the environment variables, \
         attempting to query passwd to get the home path of the current user"
    );

    unsafe {
        let pw = libc::getpwuid(libc::getuid());
        if !pw.is_null() && !(*pw).pw_dir.is_null() {
            let c_str = std::ffi::CStr::from_ptr((*pw).pw_dir);
            if let Ok(s) = c_str.to_str() {
                return PathBuf::from(s);
            }
        }
    }

    error!("Failed to get the home path of the current user");
    PathBuf::new()
}

/// Gets the relevant paths for ruzu based on XDG environment variables.
///
/// Maps to upstream `GetDataDirectory`.
#[cfg(unix)]
pub fn get_data_directory(env_name: &str) -> PathBuf {
    if let Ok(val) = std::env::var(env_name) {
        return PathBuf::from(val);
    }

    let home = get_home_directory();

    match env_name {
        "XDG_DATA_HOME" => home.join(".local/share"),
        "XDG_CACHE_HOME" => home.join(".cache"),
        "XDG_CONFIG_HOME" => home.join(".config"),
        _ => PathBuf::new(),
    }
}

#[cfg(not(unix))]
pub fn get_home_directory() -> PathBuf {
    if let Ok(home) = std::env::var("USERPROFILE") {
        return PathBuf::from(home);
    }
    PathBuf::new()
}

#[cfg(not(unix))]
pub fn get_data_directory(env_name: &str) -> PathBuf {
    let home = get_home_directory();
    match env_name {
        "XDG_DATA_HOME" => home.join(".local/share"),
        "XDG_CACHE_HOME" => home.join(".cache"),
        "XDG_CONFIG_HOME" => home.join(".config"),
        _ => PathBuf::new(),
    }
}

// =====================
// Deprecated path utilities (kept for upstream parity)
// =====================

/// Directory separator style.
///
/// Maps to upstream `DirectorySeparator`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirectorySeparator {
    ForwardSlash,
    BackwardSlash,
    PlatformDefault,
}

/// Removes the final '/' or '\\' if one exists.
///
/// Maps to upstream `RemoveTrailingSlash` (deprecated).
pub fn remove_trailing_slash(path: &str) -> &str {
    if path.is_empty() {
        return path;
    }

    if path.ends_with('\\') || path.ends_with('/') {
        &path[..path.len() - 1]
    } else {
        path
    }
}

/// Splits the path on '/' or '\\' and returns the components.
///
/// Maps to upstream `SplitPathComponents`.
pub fn split_path_components(filename: &str) -> Vec<&str> {
    filename
        .split(|c| c == '/' || c == '\\')
        .filter(|s| !s.is_empty())
        .collect()
}

/// Splits the path on '/' or '\\' and returns owned components.
///
/// Maps to upstream `SplitPathComponentsCopy`.
pub fn split_path_components_copy(filename: &str) -> Vec<String> {
    filename
        .split(|c| c == '/' || c == '\\')
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect()
}

/// Removes trailing slash, normalizes separators, and removes duplicates.
///
/// Maps to upstream `SanitizePath`.
pub fn sanitize_path(path: &str, directory_separator: DirectorySeparator) -> String {
    let mut result = path.to_string();

    let (type1, type2) = match directory_separator {
        DirectorySeparator::BackwardSlash => ('/', '\\'),
        DirectorySeparator::ForwardSlash => ('\\', '/'),
        DirectorySeparator::PlatformDefault => {
            if cfg!(windows) {
                ('/', '\\')
            } else {
                ('\\', '/')
            }
        }
    };

    // Replace type1 with type2
    result = result.replace(type1, &type2.to_string());

    // Remove duplicate separators
    let type2_str = type2.to_string();
    let double = format!("{}{}", type2, type2);
    while result.contains(&double) {
        result = result.replace(&double, &type2_str);
    }

    // Remove trailing slash
    remove_trailing_slash(&result).to_string()
}

/// Gets all text up to the last '/' or '\\' in the path.
///
/// Maps to upstream `GetParentPath`.
pub fn get_parent_path(path: &str) -> String {
    if path.is_empty() {
        return String::new();
    }

    let name_bck_index = path.rfind('\\');
    let name_fwd_index = path.rfind('/');

    let name_index = match (name_bck_index, name_fwd_index) {
        (None, None) => return String::new(),
        (Some(a), None) => a,
        (None, Some(b)) => b,
        (Some(a), Some(b)) => a.max(b),
    };

    path[..name_index].to_string()
}

/// Gets all text after the first '/' or '\\' in the path.
///
/// Maps to upstream `GetPathWithoutTop`.
pub fn get_path_without_top(path: &str) -> &str {
    if path.is_empty() {
        return path;
    }

    let mut s = path;

    // Skip leading separators
    while !s.is_empty() && (s.starts_with('/') || s.starts_with('\\')) {
        s = &s[1..];
    }

    if s.is_empty() {
        return s;
    }

    let name_bck_index = s.find('\\').unwrap_or(usize::MAX);
    let name_fwd_index = s.find('/').unwrap_or(usize::MAX);
    let name_index = name_bck_index.min(name_fwd_index);

    if name_index == usize::MAX {
        return "";
    }

    &s[name_index + 1..]
}

/// Gets the filename from a path.
///
/// Maps to upstream `GetFilename`.
pub fn get_filename(path: &str) -> &str {
    match path.rfind(|c| c == '/' || c == '\\') {
        Some(index) => &path[index + 1..],
        None => "",
    }
}

/// Gets the extension from a filename.
///
/// Maps to upstream `GetExtensionFromFilename`.
pub fn get_extension_from_filename(name: &str) -> &str {
    match name.rfind('.') {
        Some(index) => &name[index + 1..],
        None => "",
    }
}

/// Normalize a path (basic equivalent of lexically_normal).
/// Resolves `.` and `..` components.
fn normalize_path(path: &Path) -> PathBuf {
    let mut components = Vec::new();

    for component in path.components() {
        match component {
            std::path::Component::ParentDir => {
                if !components.is_empty() {
                    components.pop();
                }
            }
            std::path::Component::CurDir => {}
            other => components.push(other),
        }
    }

    components.iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_path() {
        assert!(validate_path(Path::new("/some/path")));
        assert!(!validate_path(Path::new("")));
    }

    #[test]
    fn test_concat_path() {
        let first = Path::new("/first/path");
        let second = Path::new("second/path");
        let result = concat_path(first, second);
        assert_eq!(result, PathBuf::from("/first/path/second/path"));

        // With leading separator on second
        let second_sep = Path::new("/second/path");
        let result2 = concat_path(first, second_sep);
        assert_eq!(result2, PathBuf::from("/first/path/second/path"));
    }

    #[test]
    fn test_is_path_sandboxed() {
        assert!(is_path_sandboxed(
            Path::new("/base"),
            Path::new("/base/child")
        ));
        assert!(!is_path_sandboxed(
            Path::new("/base"),
            Path::new("/other/path")
        ));
        assert!(is_path_sandboxed(
            Path::new("/base"),
            Path::new("/base")
        ));
    }

    #[test]
    fn test_is_dir_separator() {
        assert!(is_dir_separator(b'/'));
        assert!(is_dir_separator(b'\\'));
        assert!(!is_dir_separator(b'a'));
    }

    #[test]
    fn test_remove_trailing_separators() {
        assert_eq!(
            remove_trailing_separators(Path::new("/path/to/dir/")),
            PathBuf::from("/path/to/dir")
        );
        assert_eq!(
            remove_trailing_separators(Path::new("/path")),
            PathBuf::from("/path")
        );
    }

    #[test]
    fn test_remove_trailing_slash() {
        assert_eq!(remove_trailing_slash("/path/"), "/path");
        assert_eq!(remove_trailing_slash("/path"), "/path");
        assert_eq!(remove_trailing_slash(""), "");
    }

    #[test]
    fn test_split_path_components() {
        let result = split_path_components("C:\\Users\\Ruzu\\Documents\\save.bin");
        assert_eq!(result, vec!["C:", "Users", "Ruzu", "Documents", "save.bin"]);
    }

    #[test]
    fn test_split_path_components_copy() {
        let result = split_path_components_copy("/home/user/file.txt");
        assert_eq!(
            result,
            vec![
                "home".to_string(),
                "user".to_string(),
                "file.txt".to_string()
            ]
        );
    }

    #[test]
    fn test_sanitize_path() {
        let result = sanitize_path("path//to///file/", DirectorySeparator::ForwardSlash);
        assert_eq!(result, "path/to/file");
    }

    #[test]
    fn test_get_parent_path() {
        assert_eq!(get_parent_path("/home/user/file.txt"), "/home/user");
        assert_eq!(get_parent_path("file.txt"), "");
    }

    #[test]
    fn test_get_path_without_top() {
        assert_eq!(get_path_without_top("/top/rest/of/path"), "rest/of/path");
    }

    #[test]
    fn test_get_filename() {
        assert_eq!(get_filename("/path/to/file.txt"), "file.txt");
        assert_eq!(get_filename("file.txt"), "");
    }

    #[test]
    fn test_get_extension_from_filename() {
        assert_eq!(get_extension_from_filename("file.txt"), "txt");
        assert_eq!(get_extension_from_filename("file"), "");
        assert_eq!(get_extension_from_filename("archive.tar.gz"), "gz");
    }
}
