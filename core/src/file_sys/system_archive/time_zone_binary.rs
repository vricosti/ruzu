// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/system_archive/time_zone_binary.h / .cpp
// Status: PARTIAL
//
// Synthesizes the TimeZoneBinary system archive. Upstream uses nx_tzdb to
// embed timezone data directly. Rust does not have that exact dependency, so
// this owner synthesizes the same archive shape from the host tzdb under
// /usr/share/zoneinfo when available.

use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};
use crate::file_sys::vfs::vfs_vector::{make_array_file, VectorVfsDirectory};

/// Known timezone region directories, matching upstream tzdb_zoneinfo_dirs.
const ZONEINFO_DIRS: &[&str] = &[
    "Africa",
    "America",
    "Antarctica",
    "Arctic",
    "Asia",
    "Atlantic",
    "Australia",
    "Brazil",
    "Canada",
    "Chile",
    "Etc",
    "Europe",
    "Indian",
    "Mexico",
    "Pacific",
    "US",
];

/// Known America subdirectories, matching upstream tzdb_america_dirs.
const AMERICA_SUB_DIRS: &[&str] = &["Argentina", "Indiana", "Kentucky", "North_Dakota"];

const HOST_ZONEINFO_ROOT: &str = "/usr/share/zoneinfo";
const EXCLUDED_ROOT_FILES: &[&str] = &[
    "iso3166.tab",
    "leap-seconds.list",
    "leapseconds",
    "localtime",
    "posixrules",
    "tzdata.zi",
    "zone.tab",
    "zone1970.tab",
    "zonenow.tab",
];

fn make_host_file(path: &Path, name: &str) -> Option<VirtualFile> {
    let data = fs::read(path).ok()?;
    Some(make_array_file(data, name.to_string(), None))
}

fn should_include_root_file(path: &Path) -> bool {
    let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
        return false;
    };
    if EXCLUDED_ROOT_FILES.contains(&name) {
        return false;
    }
    if name.contains('.') {
        return false;
    }
    path.is_file()
}

fn collect_time_zone_names(dir: &Path, prefix: &str, out_names: &mut Vec<String>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };

    let mut children: Vec<PathBuf> = entries
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .collect();
    children.sort();

    for child in children {
        let Some(name) = child.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        let child_name = if prefix.is_empty() {
            name.to_string()
        } else {
            format!("{prefix}/{name}")
        };

        if child.is_dir() {
            collect_time_zone_names(&child, &child_name, out_names);
        } else if child.is_file() {
            out_names.push(child_name);
        }
    }
}

fn build_directory_from_host(path: &Path, name: &str, allow_nested: bool) -> Option<VirtualDir> {
    let mut files: Vec<VirtualFile> = Vec::new();
    let mut dirs: Vec<VirtualDir> = Vec::new();

    let Ok(entries) = fs::read_dir(path) else {
        return None;
    };
    let mut children: Vec<PathBuf> = entries
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .collect();
    children.sort();

    for child in children {
        let Some(child_name) = child.file_name().and_then(|value| value.to_str()) else {
            continue;
        };
        if child.is_file() {
            if let Some(file) = make_host_file(&child, child_name) {
                files.push(file);
            }
            continue;
        }
        if allow_nested && child.is_dir() {
            if let Some(dir) = build_directory_from_host(&child, child_name, true) {
                dirs.push(dir);
            }
        }
    }

    Some(Arc::new(VectorVfsDirectory::new(
        files,
        dirs,
        name.to_string(),
        None,
    )))
}

fn read_tzdb_version() -> Vec<u8> {
    let tzdata = Path::new(HOST_ZONEINFO_ROOT).join("tzdata.zi");
    let Ok(data) = fs::read_to_string(tzdata) else {
        return b"unknown\0".to_vec();
    };

    for line in data.lines() {
        if let Some(version) = line.strip_prefix("# version ") {
            let mut bytes = version.as_bytes().to_vec();
            bytes.push(0);
            return bytes;
        }
    }

    b"unknown\0".to_vec()
}

/// Synthesize the TimeZoneBinary archive.
///
/// Creates a directory structure matching the upstream layout:
/// ```text
/// data/
///   zoneinfo/
///     Africa/
///     America/
///       Argentina/
///       Indiana/
///       Kentucky/
///       North_Dakota/
///     Antarctica/
///     ...etc
/// ```
///
pub fn time_zone_binary() -> Option<VirtualDir> {
    let host_root = Path::new(HOST_ZONEINFO_ROOT);
    if !host_root.is_dir() {
        return None;
    }

    let mut zoneinfo_files: Vec<VirtualFile> = Vec::new();
    let mut zoneinfo_sub_dirs: Vec<VirtualDir> = Vec::new();
    let mut binary_list_names: Vec<String> = Vec::new();

    let Ok(entries) = fs::read_dir(host_root) else {
        return None;
    };
    let mut children: Vec<PathBuf> = entries
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .collect();
    children.sort();

    for child in children {
        let Some(name) = child.file_name().and_then(|value| value.to_str()) else {
            continue;
        };

        if child.is_dir() && ZONEINFO_DIRS.contains(&name) {
            if let Some(dir) = build_directory_from_host(&child, name, true) {
                zoneinfo_sub_dirs.push(dir);
            }
            collect_time_zone_names(&child, name, &mut binary_list_names);
            continue;
        }

        if should_include_root_file(&child) {
            if let Some(file) = make_host_file(&child, name) {
                zoneinfo_files.push(file);
                binary_list_names.push(name.to_string());
            }
        }
    }

    binary_list_names.sort();
    binary_list_names.dedup();

    let binary_list = if binary_list_names.is_empty() {
        b"Etc/GMT\n".to_vec()
    } else {
        let mut data = binary_list_names.join("\n").into_bytes();
        data.push(b'\n');
        data
    };
    let version = read_tzdb_version();

    let root_files = vec![
        make_array_file(binary_list, "binaryList.txt".to_string(), None),
        make_array_file(version, "version.txt".to_string(), None),
    ];

    let zoneinfo_dir: VirtualDir = Arc::new(VectorVfsDirectory::new(
        zoneinfo_files,
        zoneinfo_sub_dirs,
        "zoneinfo".to_string(),
        None,
    ));

    Some(Arc::new(VectorVfsDirectory::new(
        root_files,
        vec![zoneinfo_dir],
        "data".to_string(),
        None,
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs::VfsDirectory;

    #[test]
    fn test_time_zone_binary_structure() {
        let dir = time_zone_binary().expect("time_zone_binary should return Some");
        assert_eq!(dir.get_name(), "data");

        let files = dir.get_files();
        let file_names: Vec<String> = files.iter().map(|f| f.get_name()).collect();
        assert!(file_names.contains(&"binaryList.txt".to_string()));
        assert!(file_names.contains(&"version.txt".to_string()));

        let subdirs = dir.get_subdirectories();
        assert_eq!(subdirs.len(), 1);
        assert_eq!(subdirs[0].get_name(), "zoneinfo");
    }

    #[test]
    fn test_zoneinfo_has_regions() {
        let dir = time_zone_binary().unwrap();
        let zoneinfo = &dir.get_subdirectories()[0];
        let regions = zoneinfo.get_subdirectories();
        assert_eq!(regions.len(), ZONEINFO_DIRS.len());

        let names: Vec<String> = regions.iter().map(|d| d.get_name()).collect();
        assert!(names.contains(&"Africa".to_string()));
        assert!(names.contains(&"America".to_string()));
        assert!(names.contains(&"Europe".to_string()));
        assert!(names.contains(&"US".to_string()));
    }

    #[test]
    fn test_america_has_subdirs() {
        let dir = time_zone_binary().unwrap();
        let zoneinfo = &dir.get_subdirectories()[0];
        let america = zoneinfo
            .get_subdirectories()
            .into_iter()
            .find(|d| d.get_name() == "America")
            .expect("America should exist");
        let sub = america.get_subdirectories();
        assert_eq!(sub.len(), AMERICA_SUB_DIRS.len());

        let names: Vec<String> = sub.iter().map(|d| d.get_name()).collect();
        assert!(names.contains(&"Argentina".to_string()));
        assert!(names.contains(&"Indiana".to_string()));
    }

    #[test]
    fn test_binary_list_contains_etc_gmt() {
        let dir = time_zone_binary().unwrap();
        let binary_list = dir
            .get_file_relative("/binaryList.txt")
            .expect("binaryList.txt should exist");
        let data = binary_list.read_all_bytes();
        let text = String::from_utf8(data).expect("binaryList.txt should be utf8");
        assert!(text.lines().any(|line| line == "Etc/GMT"));
    }
}
