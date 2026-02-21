// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use ini::Ini;
use log::{debug, info, warn};
use ruzu_common::settings::{RendererBackend, Settings, SystemLanguage, SystemRegion};
use std::path::PathBuf;

/// Locate the yuzu sdl2-config.ini file.
pub fn find_config_path() -> Option<PathBuf> {
    // Windows: %APPDATA%\yuzu\sdl2-config.ini
    if let Ok(appdata) = std::env::var("APPDATA") {
        let path = PathBuf::from(&appdata).join("yuzu").join("sdl2-config.ini");
        if path.exists() {
            return Some(path);
        }
    }

    // Linux: ~/.config/yuzu/sdl2-config.ini
    if let Ok(home) = std::env::var("HOME") {
        let path = PathBuf::from(&home)
            .join(".config")
            .join("yuzu")
            .join("sdl2-config.ini");
        if path.exists() {
            return Some(path);
        }
    }

    // XDG_CONFIG_HOME
    if let Ok(xdg) = std::env::var("XDG_CONFIG_HOME") {
        let path = PathBuf::from(&xdg).join("yuzu").join("sdl2-config.ini");
        if path.exists() {
            return Some(path);
        }
    }

    None
}

/// Load settings from yuzu's sdl2-config.ini file.
pub fn load_config(path: Option<&PathBuf>) -> Settings {
    let mut settings = Settings::default();

    let config_path = match path {
        Some(p) => {
            if !p.exists() {
                warn!("Config file not found: {}", p.display());
                return settings;
            }
            p.clone()
        }
        None => match find_config_path() {
            Some(p) => p,
            None => {
                info!("No yuzu config found, using defaults");
                return settings;
            }
        },
    };

    info!("Loading config from: {}", config_path.display());

    let conf = match Ini::load_from_file(&config_path) {
        Ok(c) => c,
        Err(e) => {
            warn!("Failed to parse config: {}", e);
            return settings;
        }
    };

    // [Renderer]
    if let Some(section) = conf.section(Some("Renderer")) {
        if let Some(backend) = section.get("renderer_backend") {
            settings.renderer_backend = RendererBackend::from_str_or_default(backend);
            debug!("Renderer backend: {:?}", settings.renderer_backend);
        }
        if let Some(vsync) = section.get("use_vsync") {
            settings.vsync_enabled = vsync.trim() == "true" || vsync.trim() == "1";
        }
        if let Some(fullscreen) = section.get("fullscreen_mode") {
            settings.fullscreen = fullscreen.trim() == "1" || fullscreen.trim() == "true";
        }
        if let Some(res) = section.get("resolution_setup") {
            settings.resolution_factor = res.trim().parse().unwrap_or(1);
        }
    }

    // [Core]
    if let Some(section) = conf.section(Some("Core")) {
        if let Some(multi) = section.get("use_multi_core") {
            settings.use_multi_core = multi.trim() == "true" || multi.trim() == "1";
        }
    }

    // [System]
    if let Some(section) = conf.section(Some("System")) {
        if let Some(lang) = section.get("language_index") {
            let idx: u32 = lang.trim().parse().unwrap_or(1);
            settings.language = SystemLanguage::from_index(idx);
            debug!("Language: {:?}", settings.language);
        }
        if let Some(region) = section.get("region_index") {
            let idx: u32 = region.trim().parse().unwrap_or(1);
            settings.region = SystemRegion::from_index(idx);
            debug!("Region: {:?}", settings.region);
        }
    }

    // [Debugging]
    if let Some(section) = conf.section(Some("Debugging")) {
        if let Some(debug_log) = section.get("use_debug_asserts") {
            settings.use_debug_logging =
                debug_log.trim() == "true" || debug_log.trim() == "1";
        }
        if let Some(args) = section.get("program_args") {
            settings.program_args = args.to_string();
        }
    }

    settings
}
