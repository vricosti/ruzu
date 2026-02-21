// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

/// Global emulator settings, populated from config INI.
#[derive(Debug, Clone)]
pub struct Settings {
    // Renderer
    pub renderer_backend: RendererBackend,
    pub vsync_enabled: bool,
    pub fullscreen: bool,
    pub resolution_factor: u32,

    // Core
    pub use_multi_core: bool,

    // System
    pub language: SystemLanguage,
    pub region: SystemRegion,
    pub title_id: u64,

    // Debug
    pub use_debug_logging: bool,
    pub program_args: String,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            renderer_backend: RendererBackend::Vulkan,
            vsync_enabled: true,
            fullscreen: false,
            resolution_factor: 1,
            use_multi_core: false,
            language: SystemLanguage::AmericanEnglish,
            region: SystemRegion::Usa,
            title_id: 0,
            use_debug_logging: true,
            program_args: String::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RendererBackend {
    Vulkan,
    OpenGl,
    Null,
}

impl RendererBackend {
    pub fn from_str_or_default(s: &str) -> Self {
        match s.trim().to_lowercase().as_str() {
            "0" | "opengl" => Self::OpenGl,
            "1" | "vulkan" => Self::Vulkan,
            "2" | "null" => Self::Null,
            _ => Self::Vulkan,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SystemLanguage {
    Japanese = 0,
    AmericanEnglish = 1,
    French = 2,
    German = 3,
    Italian = 4,
    Spanish = 5,
    Chinese = 6,
    Korean = 7,
    Dutch = 8,
    Portuguese = 9,
    Russian = 10,
    Taiwanese = 11,
    BritishEnglish = 12,
    CanadianFrench = 13,
    LatinAmericanSpanish = 14,
    SimplifiedChinese = 15,
    TraditionalChinese = 16,
    BrazilianPortuguese = 17,
}

impl SystemLanguage {
    pub fn from_index(index: u32) -> Self {
        match index {
            0 => Self::Japanese,
            1 => Self::AmericanEnglish,
            2 => Self::French,
            3 => Self::German,
            4 => Self::Italian,
            5 => Self::Spanish,
            6 => Self::Chinese,
            7 => Self::Korean,
            8 => Self::Dutch,
            9 => Self::Portuguese,
            10 => Self::Russian,
            11 => Self::Taiwanese,
            12 => Self::BritishEnglish,
            13 => Self::CanadianFrench,
            14 => Self::LatinAmericanSpanish,
            15 => Self::SimplifiedChinese,
            16 => Self::TraditionalChinese,
            17 => Self::BrazilianPortuguese,
            _ => Self::AmericanEnglish,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SystemRegion {
    Japan = 0,
    Usa = 1,
    Europe = 2,
    Australia = 3,
    China = 4,
    Korea = 5,
    Taiwan = 6,
}

impl SystemRegion {
    pub fn from_index(index: u32) -> Self {
        match index {
            0 => Self::Japan,
            1 => Self::Usa,
            2 => Self::Europe,
            3 => Self::Australia,
            4 => Self::China,
            5 => Self::Korea,
            6 => Self::Taiwan,
            _ => Self::Usa,
        }
    }
}
