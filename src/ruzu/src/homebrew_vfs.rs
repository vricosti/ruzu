// SPDX-License-Identifier: GPL-3.0-or-later

//! Per-launch SDMC view for standalone homebrew NROs.
//!
//! Eden's core VFS has no writable layered-directory counterpart: its
//! `LayeredVfsDirectory` deliberately rejects mutations. This frontend-owned
//! view therefore keeps the NRO's containing directory as the writable,
//! higher-priority layer and the configured SDMC as a fallback. It avoids
//! platform-specific host symbolic links and Windows junction points while
//! leaving the core content caches attached to the configured SDMC.

use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;
use std::sync::Arc;

use ruzu_core::file_sys::fs_filesystem::OpenMode;
use ruzu_core::file_sys::vfs::vfs::{VfsDirectory, VfsEntryType};
use ruzu_core::file_sys::vfs::vfs_real::{RealVfsDirectory, RealVfsFilesystem};
use ruzu_core::file_sys::vfs::vfs_types::{FileTimeStampRaw, VirtualDir, VirtualFile};
use ruzu_core::loader::loader::{FileType, FileTypeIdentifier};
use ruzu_core::loader::nro::AppLoaderNro;

/// A merged directory whose homebrew layer takes precedence over normal SDMC.
struct HomebrewSdmcDirectory {
    homebrew_root: VirtualDir,
    sdmc_root: VirtualDir,
    relative_components: Vec<String>,
}

impl HomebrewSdmcDirectory {
    fn root(homebrew_root: VirtualDir, sdmc_root: VirtualDir) -> Self {
        Self {
            homebrew_root,
            sdmc_root,
            relative_components: Vec::new(),
        }
    }

    fn with_components(&self, relative_components: Vec<String>) -> VirtualDir {
        Arc::new(Self {
            homebrew_root: Arc::clone(&self.homebrew_root),
            sdmc_root: Arc::clone(&self.sdmc_root),
            relative_components,
        })
    }

    fn child(&self, name: &str) -> VirtualDir {
        let mut components = self.relative_components.clone();
        components.push(name.to_string());
        self.with_components(components)
    }

    fn relative_path(&self) -> String {
        self.relative_components.join("/")
    }

    fn resolve(root: &VirtualDir, components: &[String]) -> Option<VirtualDir> {
        if components.is_empty() {
            return Some(Arc::clone(root));
        }
        root.get_directory_relative(&components.join("/"))
    }

    fn homebrew_directory(&self) -> Option<VirtualDir> {
        Self::resolve(&self.homebrew_root, &self.relative_components)
    }

    fn sdmc_directory(&self) -> Option<VirtualDir> {
        Self::resolve(&self.sdmc_root, &self.relative_components)
    }

    fn ensure_homebrew_directory(&self) -> Option<VirtualDir> {
        let mut directory = Arc::clone(&self.homebrew_root);
        for component in &self.relative_components {
            if directory.get_file(component).is_some() {
                return None;
            }
            directory = match directory.get_subdirectory(component) {
                Some(existing) => existing,
                None => directory.create_subdirectory(component)?,
            };
        }
        Some(directory)
    }

    fn homebrew_entries(&self) -> BTreeMap<String, VfsEntryType> {
        self.homebrew_directory()
            .map(|directory| directory.get_entries())
            .unwrap_or_default()
    }
}

impl VfsDirectory for HomebrewSdmcDirectory {
    fn get_files(&self) -> Vec<VirtualFile> {
        let homebrew_entries = self.homebrew_entries();
        let mut files = BTreeMap::new();

        if let Some(directory) = self.sdmc_directory() {
            for file in directory.get_files() {
                if !homebrew_entries.contains_key(&file.get_name()) {
                    files.insert(file.get_name(), file);
                }
            }
        }
        if let Some(directory) = self.homebrew_directory() {
            for file in directory.get_files() {
                files.insert(file.get_name(), file);
            }
        }

        files.into_values().collect()
    }

    fn get_file(&self, name: &str) -> Option<VirtualFile> {
        if let Some(directory) = self.homebrew_directory() {
            if let Some(file) = directory.get_file(name) {
                return Some(file);
            }
            if directory.get_subdirectory(name).is_some() {
                return None;
            }
        }
        self.sdmc_directory()?.get_file(name)
    }

    fn get_file_time_stamp(&self, path: &str) -> FileTimeStampRaw {
        if let Some(directory) = self.homebrew_directory() {
            if directory.get_file_relative(path).is_some()
                || directory.get_directory_relative(path).is_some()
            {
                return directory.get_file_time_stamp(path);
            }
        }
        self.sdmc_directory()
            .map(|directory| directory.get_file_time_stamp(path))
            .unwrap_or_default()
    }

    fn get_subdirectories(&self) -> Vec<VirtualDir> {
        let homebrew_entries = self.homebrew_entries();
        let mut names = BTreeSet::new();

        if let Some(directory) = self.sdmc_directory() {
            for child in directory.get_subdirectories() {
                if homebrew_entries.get(&child.get_name()) != Some(&VfsEntryType::File) {
                    names.insert(child.get_name());
                }
            }
        }
        if let Some(directory) = self.homebrew_directory() {
            for child in directory.get_subdirectories() {
                names.insert(child.get_name());
            }
        }

        names.into_iter().map(|name| self.child(&name)).collect()
    }

    fn get_subdirectory(&self, name: &str) -> Option<VirtualDir> {
        if let Some(directory) = self.homebrew_directory() {
            if directory.get_file(name).is_some() {
                return None;
            }
            if directory.get_subdirectory(name).is_some() {
                return Some(self.child(name));
            }
        }
        self.sdmc_directory()?
            .get_subdirectory(name)
            .map(|_| self.child(name))
    }

    fn is_writable(&self) -> bool {
        self.homebrew_root.is_writable()
    }

    fn is_readable(&self) -> bool {
        self.homebrew_root.is_readable() || self.sdmc_root.is_readable()
    }

    fn get_name(&self) -> String {
        self.relative_components
            .last()
            .cloned()
            .unwrap_or_else(|| "sdmc".to_string())
    }

    fn get_parent_directory(&self) -> Option<VirtualDir> {
        if self.relative_components.is_empty() {
            return None;
        }
        let mut parent = self.relative_components.clone();
        parent.pop();
        Some(self.with_components(parent))
    }

    fn create_subdirectory(&self, name: &str) -> Option<VirtualDir> {
        let directory = self.ensure_homebrew_directory()?;
        if directory.get_file(name).is_some() {
            return None;
        }
        directory
            .get_subdirectory(name)
            .or_else(|| directory.create_subdirectory(name))?;
        Some(self.child(name))
    }

    fn create_file(&self, name: &str) -> Option<VirtualFile> {
        let directory = self.ensure_homebrew_directory()?;
        if directory.get_subdirectory(name).is_some() {
            return None;
        }
        directory.create_file(name)
    }

    fn delete_subdirectory(&self, name: &str) -> bool {
        if let Some(directory) = self.homebrew_directory() {
            if directory.get_subdirectory(name).is_some() {
                return directory.delete_subdirectory(name);
            }
            if directory.get_file(name).is_some() {
                return false;
            }
        }
        self.sdmc_directory()
            .is_some_and(|directory| directory.delete_subdirectory(name))
    }

    fn delete_subdirectory_recursive(&self, name: &str) -> bool {
        if let Some(directory) = self.homebrew_directory() {
            if directory.get_subdirectory(name).is_some() {
                return directory.delete_subdirectory_recursive(name);
            }
            if directory.get_file(name).is_some() {
                return false;
            }
        }
        self.sdmc_directory()
            .is_some_and(|directory| directory.delete_subdirectory_recursive(name))
    }

    fn delete_file(&self, name: &str) -> bool {
        if let Some(directory) = self.homebrew_directory() {
            if directory.get_file(name).is_some() {
                return directory.delete_file(name);
            }
            if directory.get_subdirectory(name).is_some() {
                return false;
            }
        }
        self.sdmc_directory()
            .is_some_and(|directory| directory.delete_file(name))
    }

    fn rename(&self, name: &str) -> bool {
        if self.relative_components.is_empty() {
            return false;
        }
        if let Some(directory) = self.homebrew_directory() {
            return directory.rename(name);
        }
        self.sdmc_directory()
            .is_some_and(|directory| directory.rename(name))
    }

    fn get_full_path(&self) -> String {
        let relative = self.relative_path();
        if relative.is_empty() {
            "sdmc:/".to_string()
        } else {
            format!("sdmc:/{relative}")
        }
    }
}

/// Build a per-launch SDMC view when `executable_path` identifies as an NRO.
pub(crate) fn make_homebrew_sdmc_view(
    vfs: Arc<RealVfsFilesystem>,
    executable_path: &Path,
    sdmc_root: VirtualDir,
) -> Option<VirtualDir> {
    let executable_path = executable_path.canonicalize().ok()?;
    let executable = vfs.arc_open_file(&executable_path.to_string_lossy(), OpenMode::READ)?;
    if AppLoaderNro::identify_type(&executable) != FileType::NRO {
        return None;
    }

    let homebrew_path = executable_path.parent()?;
    if !homebrew_path.is_dir() {
        return None;
    }
    let homebrew_root: VirtualDir = Arc::new(RealVfsDirectory::new(
        vfs,
        homebrew_path.to_string_lossy().into_owned(),
        OpenMode::READ_WRITE,
    ));

    Some(Arc::new(HomebrewSdmcDirectory::root(
        homebrew_root,
        sdmc_root,
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn nro_bytes() -> Vec<u8> {
        let mut bytes = vec![0; 0x80];
        bytes[0x10..0x14].copy_from_slice(b"NRO0");
        bytes
    }

    fn real_directory(vfs: Arc<RealVfsFilesystem>, path: &Path) -> VirtualDir {
        Arc::new(RealVfsDirectory::new(
            vfs,
            path.to_string_lossy().into_owned(),
            OpenMode::READ_WRITE,
        ))
    }

    #[test]
    fn homebrew_files_take_priority_and_sdmc_remains_visible() {
        let temporary = tempfile::tempdir().unwrap();
        let homebrew = temporary.path().join("homebrew");
        let sdmc = temporary.path().join("sdmc");
        fs::create_dir_all(&homebrew).unwrap();
        fs::create_dir_all(&sdmc).unwrap();
        fs::write(homebrew.join("app.nro"), nro_bytes()).unwrap();
        fs::write(homebrew.join("default.cfg"), b"homebrew").unwrap();
        fs::write(sdmc.join("default.cfg"), b"sdmc").unwrap();
        fs::write(sdmc.join("fallback.dat"), b"fallback").unwrap();

        let vfs = RealVfsFilesystem::new();
        let view = make_homebrew_sdmc_view(
            Arc::clone(&vfs),
            &homebrew.join("app.nro"),
            real_directory(vfs, &sdmc),
        )
        .unwrap();

        assert_eq!(
            view.get_file("default.cfg").unwrap().read_all_bytes(),
            b"homebrew"
        );
        assert_eq!(
            view.get_file("fallback.dat").unwrap().read_all_bytes(),
            b"fallback"
        );
    }

    #[test]
    fn nested_creates_are_written_beside_the_nro() {
        let temporary = tempfile::tempdir().unwrap();
        let homebrew = temporary.path().join("homebrew");
        let sdmc = temporary.path().join("sdmc");
        fs::create_dir_all(&homebrew).unwrap();
        fs::create_dir_all(sdmc.join("switch")).unwrap();
        fs::write(homebrew.join("app.nro"), nro_bytes()).unwrap();
        fs::write(sdmc.join("switch/fallback.dat"), b"fallback").unwrap();

        let vfs = RealVfsFilesystem::new();
        let view = make_homebrew_sdmc_view(
            Arc::clone(&vfs),
            &homebrew.join("app.nro"),
            real_directory(vfs, &sdmc),
        )
        .unwrap();
        let created = view.create_file_relative("switch/app/save.dat").unwrap();
        assert_eq!(created.write_bytes(b"save", 0), 4);

        assert_eq!(
            fs::read(homebrew.join("switch/app/save.dat")).unwrap(),
            b"save"
        );
        assert!(!sdmc.join("switch/app/save.dat").exists());
        assert_eq!(
            view.get_file_relative("switch/fallback.dat")
                .unwrap()
                .read_all_bytes(),
            b"fallback"
        );
    }

    #[test]
    fn non_nro_executables_keep_the_normal_sdmc() {
        let temporary = tempfile::tempdir().unwrap();
        let homebrew = temporary.path().join("homebrew");
        let sdmc = temporary.path().join("sdmc");
        fs::create_dir_all(&homebrew).unwrap();
        fs::create_dir_all(&sdmc).unwrap();
        fs::write(homebrew.join("application.bin"), b"not an nro").unwrap();

        let vfs = RealVfsFilesystem::new();
        assert!(make_homebrew_sdmc_view(
            Arc::clone(&vfs),
            &homebrew.join("application.bin"),
            real_directory(vfs, &sdmc),
        )
        .is_none());
    }
}
