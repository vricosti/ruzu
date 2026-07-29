// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/input_profiles.cpp`.

use std::collections::HashMap;
use std::path::PathBuf;

use common::fs::path_util::{get_ruzu_path, RuzuPath};
use common::settings_input::PlayerInput;

use super::qt_config;

pub struct InputProfiles {
    map_profiles: HashMap<String, PathBuf>,
    input_profile_dir: PathBuf,
}

impl InputProfiles {
    pub fn new() -> Self {
        Self::from_directory(get_ruzu_path(RuzuPath::ConfigDir).join("input"))
    }

    fn from_directory(input_profile_dir: PathBuf) -> Self {
        let mut profiles = Self {
            map_profiles: HashMap::new(),
            input_profile_dir,
        };
        let Ok(entries) = std::fs::read_dir(&profiles.input_profile_dir) else {
            return profiles;
        };

        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_file() || path.extension().and_then(|ext| ext.to_str()) != Some("ini") {
                continue;
            }
            let Some(profile_name) = path.file_stem().and_then(|name| name.to_str()) else {
                continue;
            };
            if Self::is_profile_name_valid(profile_name) {
                profiles.map_profiles.insert(profile_name.to_string(), path);
            }
        }
        profiles
    }

    pub fn get_input_profile_names(&mut self) -> Vec<String> {
        self.map_profiles.retain(|_, path| path.exists());
        let mut names: Vec<_> = self.map_profiles.keys().cloned().collect();
        names.sort();
        names
    }

    pub fn is_profile_name_valid(profile_name: &str) -> bool {
        !profile_name.contains([
            '<', '>', ':', ';', '"', '/', '\\', '|', ',', '.', '!', '?', '*',
        ])
    }

    pub fn create_profile(&mut self, profile_name: &str, player: &PlayerInput) -> bool {
        if self.profile_exists_in_map(profile_name) {
            return false;
        }

        let path = self.profile_path(profile_name);
        self.map_profiles
            .insert(profile_name.to_string(), path.clone());
        self.save_profile(profile_name, player)
    }

    pub fn delete_profile(&mut self, profile_name: &str) -> bool {
        if !self.profile_exists_in_map(profile_name) {
            return false;
        }

        let path = self.map_profiles[profile_name].clone();
        if !path.exists() || std::fs::remove_file(&path).is_ok() {
            self.map_profiles.remove(profile_name);
        }

        !self.profile_exists_in_map(profile_name) && !path.exists()
    }

    pub fn load_profile(&mut self, profile_name: &str, player: &mut PlayerInput) -> bool {
        if !self.profile_exists_in_map(profile_name) {
            return false;
        }

        let path = self.map_profiles[profile_name].clone();
        let Ok(contents) = std::fs::read_to_string(&path) else {
            if !path.exists() {
                self.map_profiles.remove(profile_name);
            }
            return false;
        };

        log::info!("Loading input profile `{profile_name}`");
        qt_config::load_input_profile(&contents, player);
        true
    }

    pub fn save_profile(&self, profile_name: &str, player: &PlayerInput) -> bool {
        let Some(path) = self.map_profiles.get(profile_name) else {
            return false;
        };
        if let Some(parent) = path.parent() {
            if std::fs::create_dir_all(parent).is_err() {
                return false;
            }
        }
        std::fs::write(path, qt_config::serialize_input_profile(player)).is_ok()
    }

    fn profile_exists_in_map(&self, profile_name: &str) -> bool {
        self.map_profiles.contains_key(profile_name)
    }

    fn profile_path(&self, profile_name: &str) -> PathBuf {
        self.input_profile_dir.join(format!("{profile_name}.ini"))
    }
}

impl Default for InputProfiles {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::settings_input::ControllerType;

    struct TestDirectory(PathBuf);

    impl TestDirectory {
        fn new() -> Self {
            let path = std::env::temp_dir().join(format!(
                "ruzu-input-profiles-{}-{}",
                std::process::id(),
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_nanos()
            ));
            std::fs::create_dir_all(&path).unwrap();
            Self(path)
        }
    }

    impl Drop for TestDirectory {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn profile_name_validation_matches_upstream_forbidden_characters() {
        assert!(InputProfiles::is_profile_name_valid("Arcade Pad"));
        assert!(InputProfiles::is_profile_name_valid(""));
        for character in [
            '<', '>', ':', ';', '"', '/', '\\', '|', ',', '.', '!', '?', '*',
        ] {
            assert!(!InputProfiles::is_profile_name_valid(&format!(
                "profile{character}name"
            )));
        }
    }

    #[test]
    fn profiles_create_load_sort_and_delete() {
        let directory = TestDirectory::new();
        let mut profiles = InputProfiles::from_directory(directory.0.clone());
        let mut first = PlayerInput::default();
        first.controller_type = ControllerType::GameCube;
        first.buttons[0] = "engine:sdl,button:7".to_string();

        assert!(profiles.create_profile("zeta", &first));
        assert!(profiles.create_profile("alpha", &first));
        assert!(!profiles.create_profile("alpha", &first));
        assert_eq!(
            profiles.get_input_profile_names(),
            vec!["alpha".to_string(), "zeta".to_string()]
        );

        let mut loaded = PlayerInput::default();
        assert!(profiles.load_profile("zeta", &mut loaded));
        assert_eq!(loaded.controller_type, ControllerType::GameCube);
        assert_eq!(loaded.buttons[0], "engine:sdl,button:7");

        assert!(profiles.delete_profile("zeta"));
        assert!(!profiles.delete_profile("zeta"));
        assert_eq!(
            profiles.get_input_profile_names(),
            vec!["alpha".to_string()]
        );
    }

    #[test]
    fn stale_profile_files_are_removed_from_the_map() {
        let directory = TestDirectory::new();
        let path = directory.0.join("gone.ini");
        std::fs::write(&path, "[Controls]\n").unwrap();
        let mut profiles = InputProfiles::from_directory(directory.0.clone());
        std::fs::remove_file(path).unwrap();

        assert!(profiles.get_input_profile_names().is_empty());
    }
}
