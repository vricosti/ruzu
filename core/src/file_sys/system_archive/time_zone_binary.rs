// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/system_archive/time_zone_binary.h / .cpp
// Status: PARTIAL
//
// Synthesizes the TimeZoneBinary system archive. The upstream implementation
// uses the nx_tzdb library to populate timezone data files. Since we don't
// have that library available, we create the directory structure with empty
// placeholder files to satisfy the archive format.

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
/// NOTE: The actual timezone data files require the nx_tzdb library which is
/// not available in Rust. The directory structure is created but without the
/// individual timezone data files. This is sufficient for the archive format
/// but will not provide actual timezone conversion data.
pub fn time_zone_binary() -> Option<VirtualDir> {
    // Build America subdirectories.
    let america_sub_dirs: Vec<VirtualDir> = AMERICA_SUB_DIRS
        .iter()
        .map(|name| {
            Arc::new(VectorVfsDirectory::new(
                vec![],
                vec![],
                name.to_string(),
                None,
            )) as VirtualDir
        })
        .collect();

    // Build zoneinfo subdirectories.
    let mut zoneinfo_sub_dirs: Vec<VirtualDir> = Vec::with_capacity(ZONEINFO_DIRS.len());
    for &dir_name in ZONEINFO_DIRS {
        if dir_name == "America" {
            // America gets the extra subdirectories.
            zoneinfo_sub_dirs.push(Arc::new(VectorVfsDirectory::new(
                vec![],
                america_sub_dirs.clone(),
                dir_name.to_string(),
                None,
            )));
        } else {
            zoneinfo_sub_dirs.push(Arc::new(VectorVfsDirectory::new(
                vec![],
                vec![],
                dir_name.to_string(),
                None,
            )));
        }
    }

    // Build the zoneinfo directory.
    let zoneinfo_dir: VirtualDir = Arc::new(VectorVfsDirectory::new(
        vec![], // In upstream, GenerateZoneinfoFiles() populates base zoneinfo files
        zoneinfo_sub_dirs,
        "zoneinfo".to_string(),
        None,
    ));

    // Build the root "data" directory.
    // In upstream, NxTzdb::base files go here as root_files.
    Some(Arc::new(VectorVfsDirectory::new(
        vec![],
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
}
