// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use ini::Ini;
use log::{debug, info, warn};
use ruzu_common::settings::{
    Language, Region, RendererBackend, Values,
};
use std::path::PathBuf;

// Re-export for backward compat
pub use ruzu_common::settings::Language as SystemLanguage;
pub use ruzu_common::settings::Region as SystemRegion;

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

/// Find the keys directory (containing prod.keys / title.keys).
pub fn find_keys_dir(explicit: Option<&PathBuf>) -> Option<PathBuf> {
    if let Some(path) = explicit {
        if path.exists() {
            return Some(path.clone());
        }
        warn!("Specified keys directory not found: {}", path.display());
    }

    if let Ok(appdata) = std::env::var("APPDATA") {
        let path = PathBuf::from(&appdata).join("yuzu").join("keys");
        if path.exists() {
            return Some(path);
        }
    }

    if let Ok(home) = std::env::var("HOME") {
        let path = PathBuf::from(&home)
            .join(".local")
            .join("share")
            .join("yuzu")
            .join("keys");
        if path.exists() {
            return Some(path);
        }
    }

    if let Ok(xdg) = std::env::var("XDG_DATA_HOME") {
        let path = PathBuf::from(&xdg).join("yuzu").join("keys");
        if path.exists() {
            return Some(path);
        }
    }

    None
}

/// Find the games directory.
pub fn find_games_dir(explicit: Option<&PathBuf>) -> Option<PathBuf> {
    if let Some(path) = explicit {
        if path.exists() {
            return Some(path.clone());
        }
        warn!("Specified games directory not found: {}", path.display());
    }

    if let Ok(home) = std::env::var("HOME") {
        let paths = [
            PathBuf::from(&home).join("Games").join("Switch"),
            PathBuf::from(&home).join("games").join("switch"),
            PathBuf::from(&home).join("roms").join("switch"),
        ];
        for path in &paths {
            if path.exists() {
                return Some(path.clone());
            }
        }
    }

    if let Ok(userprofile) = std::env::var("USERPROFILE") {
        let paths = [
            PathBuf::from(&userprofile).join("Games").join("Switch"),
            PathBuf::from(&userprofile).join("Documents").join("Switch"),
        ];
        for path in &paths {
            if path.exists() {
                return Some(path.clone());
            }
        }
    }

    None
}

/// Load settings from yuzu's sdl2-config.ini file.
pub fn load_config(path: Option<&PathBuf>) -> Values {
    let mut settings = Values::default();

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
            if let Some(rb) = RendererBackend::from_string(backend.trim()) {
                settings.renderer_backend.set_value(rb);
            }
            debug!("Renderer backend: {:?}", settings.renderer_backend.get_value());
        }
        if let Some(vsync) = section.get("use_vsync") {
            if let Some(mode) = ruzu_common::settings::VSyncMode::from_string(vsync.trim()) {
                settings.vsync_mode.set_value(mode);
            }
        }
        if let Some(res) = section.get("resolution_setup") {
            if let Some(setup) = ruzu_common::settings::ResolutionSetup::from_string(res.trim()) {
                settings.resolution_setup.set_value(setup);
            }
        }
    }

    // [Core]
    if let Some(section) = conf.section(Some("Core")) {
        if let Some(multi) = section.get("use_multi_core") {
            let val = multi.trim() == "true" || multi.trim() == "1";
            settings.use_multi_core.set_value(val);
        }
    }

    // [System]
    if let Some(section) = conf.section(Some("System")) {
        if let Some(lang) = section.get("language_index") {
            let idx: u32 = lang.trim().parse().unwrap_or(1);
            settings.language_index.set_value(Language::from_index(idx));
            debug!("Language: {:?}", settings.language_index.get_value());
        }
        if let Some(region) = section.get("region_index") {
            let idx: u32 = region.trim().parse().unwrap_or(1);
            settings.region_index.set_value(Region::from_index(idx));
            debug!("Region: {:?}", settings.region_index.get_value());
        }
    }

    // [Cpu] — matching zuyu's sdl2-config.ini format
    if let Some(section) = conf.section(Some("Cpu")) {
        // cpu_accuracy: numeric (0=Auto, 1=Accurate, 2=Unsafe, 3=Paranoid)
        if let Some(val) = section.get("cpu_accuracy") {
            let idx: u32 = val.trim().parse().unwrap_or(0);
            use ruzu_common::settings_enums::CpuAccuracy;
            let accuracy = match idx {
                0 => CpuAccuracy::Auto,
                1 => CpuAccuracy::Accurate,
                2 => CpuAccuracy::Unsafe,
                3 => CpuAccuracy::Paranoid,
                _ => CpuAccuracy::Auto,
            };
            settings.cpu_accuracy.set_value(accuracy);
            debug!("CPU accuracy: {:?}", accuracy);
        }
        if let Some(val) = section.get("cpu_debug_mode") {
            let v = val.trim() == "true" || val.trim() == "1";
            settings.cpu_debug_mode.set_value(v);
        }
    }

    // [CpuDebug] — individual optimization toggles (read from [Cpu] section
    // since zuyu stores them all together)
    {
        let section = conf.section(Some("Cpu"));
        let read_bool = |key: &str| -> Option<bool> {
            section?.get(key).map(|v| v.trim() == "true" || v.trim() == "1")
        };
        if let Some(v) = read_bool("cpuopt_page_tables") {
            settings.cpuopt_page_tables.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_block_linking") {
            settings.cpuopt_block_linking.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_return_stack_buffer") {
            settings.cpuopt_return_stack_buffer.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_fast_dispatcher") {
            settings.cpuopt_fast_dispatcher.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_context_elimination") {
            settings.cpuopt_context_elimination.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_const_prop") {
            settings.cpuopt_const_prop.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_misc_ir") {
            settings.cpuopt_misc_ir.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_reduce_misalign_checks") {
            settings.cpuopt_reduce_misalign_checks.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_unsafe_unfuse_fma") {
            settings.cpuopt_unsafe_unfuse_fma.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_unsafe_reduce_fp_error") {
            settings.cpuopt_unsafe_reduce_fp_error.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_unsafe_ignore_standard_fpcr") {
            settings.cpuopt_unsafe_ignore_standard_fpcr.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_unsafe_inaccurate_nan") {
            settings.cpuopt_unsafe_inaccurate_nan.set_value(v);
        }
        if let Some(v) = read_bool("cpuopt_unsafe_ignore_global_monitor") {
            settings.cpuopt_unsafe_ignore_global_monitor.set_value(v);
        }
    }

    // [Debugging]
    if let Some(section) = conf.section(Some("Debugging")) {
        if let Some(debug_asserts) = section.get("use_debug_asserts") {
            let val = debug_asserts.trim() == "true" || debug_asserts.trim() == "1";
            settings.use_debug_asserts.set_value(val);
        }
        if let Some(args) = section.get("program_args") {
            settings.program_args.set_value(args.to_string());
        }
    }

    settings
}
