// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust counterpart of the combo-box label tables in
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/shared_translation.cpp`
// (`ConfigurationShared::ComboboxEnumeration`).
//
// Upstream maps each `Settings::` enum onto an ordered list of
// `(enum value, human label)` pairs, which the configuration pages feed into
// their `QComboBox`es. The enum's *canonical* name (used for serialization)
// deliberately differs from the label shown in the UI — e.g. `NvdecEmulation::Gpu`
// canonicalizes to "Gpu" but displays as "GPU Video Decoding (Default)".
//
// Each table below is `&[(variant, label)]`, in the same order as upstream's
// initializer list, because the combo-box row order is part of the UI contract.
// Pages select a row with `shared_widget::index_of` over the variant column.
//
// Divergence: upstream keys the map on a runtime `EnumMetadata<T>::Index()` so
// its generic widget builder can look tables up dynamically. The Rust port
// exposes one `const` table per enum instead — the call sites know their enum
// statically, so the runtime indirection buys nothing.

use common::settings_enums::{
    AnisotropyMode, AntiAliasing, AppletMode, AspectRatio, AstcDecodeMode, AstcRecompression,
    AudioMode, ConfirmStop, ConsoleMode, CpuAccuracy, CpuBackend, FullscreenMode, GpuAccuracy,
    Language, MemoryLayout, NvdecEmulation, Region, RendererBackend, ResolutionSetup,
    ScalingFilter, ShaderBackend, VramUsageMode,
};

/// Split a `&[(T, &str)]` table into its label column, for feeding a combo box.
pub fn labels<T>(table: &[(T, &'static str)]) -> Vec<&'static str> {
    table.iter().map(|(_, label)| *label).collect()
}

/// Index of `value` in `table`, or 0 when the stored value isn't listed.
pub fn index_of<T: PartialEq>(table: &[(T, &'static str)], value: &T) -> u32 {
    table
        .iter()
        .position(|(variant, _)| variant == value)
        .unwrap_or(0) as u32
}

/// Variant at combo-box row `index`, falling back to the first row.
pub fn value_at<T: Copy>(table: &[(T, &'static str)], index: u32) -> T {
    table
        .get(index as usize)
        .map(|(variant, _)| *variant)
        .unwrap_or(table[0].0)
}

pub const APPLET_MODE: &[(AppletMode, &str)] = &[
    (AppletMode::HLE, "Custom frontend"),
    (AppletMode::LLE, "Real applet"),
];

pub const ASTC_DECODE_MODE: &[(AstcDecodeMode, &str)] = &[
    (AstcDecodeMode::Cpu, "CPU"),
    (AstcDecodeMode::Gpu, "GPU"),
    (AstcDecodeMode::CpuAsynchronous, "CPU Asynchronous"),
];

pub const ASTC_RECOMPRESSION: &[(AstcRecompression, &str)] = &[
    (
        AstcRecompression::Uncompressed,
        "Uncompressed (Best quality)",
    ),
    (AstcRecompression::Bc1, "BC1 (Low quality)"),
    (AstcRecompression::Bc3, "BC3 (Medium quality)"),
];

pub const VRAM_USAGE_MODE: &[(VramUsageMode, &str)] = &[
    (VramUsageMode::Conservative, "Conservative"),
    (VramUsageMode::Aggressive, "Aggressive"),
];

pub const RENDERER_BACKEND: &[(RendererBackend, &str)] = &[
    (RendererBackend::OpenGL, "OpenGL"),
    (RendererBackend::Vulkan, "Vulkan"),
    (RendererBackend::Null, "Null"),
];

pub const SHADER_BACKEND: &[(ShaderBackend, &str)] = &[
    (ShaderBackend::Glsl, "GLSL"),
    (
        ShaderBackend::Glasm,
        "GLASM (Assembly Shaders, NVIDIA Only)",
    ),
    (ShaderBackend::SpirV, "SPIR-V (Experimental, AMD/Mesa Only)"),
];

pub const GPU_ACCURACY: &[(GpuAccuracy, &str)] = &[
    (GpuAccuracy::Normal, "Normal"),
    (GpuAccuracy::High, "High"),
    (GpuAccuracy::Extreme, "Extreme"),
];

pub const CPU_ACCURACY: &[(CpuAccuracy, &str)] = &[
    (CpuAccuracy::Auto, "Auto"),
    (CpuAccuracy::Accurate, "Accurate"),
    (CpuAccuracy::Unsafe, "Unsafe"),
    (
        CpuAccuracy::Paranoid,
        "Paranoid (disables most optimizations)",
    ),
];

/// Upstream's `configure_cpu.ui` only shows the "Backend:" row on targets where
/// NCE exists (ARM64 hosts), so the x86-64 dialog never renders it — but the
/// table is part of `ComboboxEnumeration` upstream, so it is kept here too.
#[allow(dead_code)]
pub const CPU_BACKEND: &[(CpuBackend, &str)] =
    &[(CpuBackend::Dynarmic, "Dynarmic"), (CpuBackend::Nce, "NCE")];

pub const FULLSCREEN_MODE: &[(FullscreenMode, &str)] = &[
    (FullscreenMode::Borderless, "Borderless Windowed"),
    (FullscreenMode::Exclusive, "Exclusive Fullscreen"),
];

pub const NVDEC_EMULATION: &[(NvdecEmulation, &str)] = &[
    (NvdecEmulation::Off, "No Video Output"),
    (NvdecEmulation::Cpu, "CPU Video Decoding"),
    (NvdecEmulation::Gpu, "GPU Video Decoding (Default)"),
];

pub const RESOLUTION_SETUP: &[(ResolutionSetup, &str)] = &[
    (ResolutionSetup::Res1_2X, "0.5X (360p/540p) [EXPERIMENTAL]"),
    (ResolutionSetup::Res3_4X, "0.75X (540p/810p) [EXPERIMENTAL]"),
    (ResolutionSetup::Res1X, "1X (720p/1080p)"),
    (
        ResolutionSetup::Res3_2X,
        "1.5X (1080p/1620p) [EXPERIMENTAL]",
    ),
    (ResolutionSetup::Res2X, "2X (1440p/2160p)"),
    (ResolutionSetup::Res3X, "3X (2160p/3240p)"),
    (ResolutionSetup::Res4X, "4X (2880p/4320p)"),
    (ResolutionSetup::Res5X, "5X (3600p/5400p)"),
    (ResolutionSetup::Res6X, "6X (4320p/6480p)"),
    (ResolutionSetup::Res7X, "7X (5040p/7560p)"),
    (ResolutionSetup::Res8X, "8X (5760p/8640p)"),
];

pub const SCALING_FILTER: &[(ScalingFilter, &str)] = &[
    (ScalingFilter::NearestNeighbor, "Nearest Neighbor"),
    (ScalingFilter::Bilinear, "Bilinear"),
    (ScalingFilter::Bicubic, "Bicubic"),
    (ScalingFilter::Gaussian, "Gaussian"),
    (ScalingFilter::ScaleForce, "ScaleForce"),
    (ScalingFilter::Fsr, "AMD FidelityFX™️ Super Resolution"),
];

pub const ANTI_ALIASING: &[(AntiAliasing, &str)] = &[
    (AntiAliasing::None, "None"),
    (AntiAliasing::Fxaa, "FXAA"),
    (AntiAliasing::Smaa, "SMAA"),
];

pub const ASPECT_RATIO: &[(AspectRatio, &str)] = &[
    (AspectRatio::R16_9, "Default (16:9)"),
    (AspectRatio::R4_3, "Force 4:3"),
    (AspectRatio::R21_9, "Force 21:9"),
    (AspectRatio::R16_10, "Force 16:10"),
    (AspectRatio::Stretch, "Stretch to Window"),
];

pub const ANISOTROPY_MODE: &[(AnisotropyMode, &str)] = &[
    (AnisotropyMode::Automatic, "Automatic"),
    (AnisotropyMode::Default, "Default"),
    (AnisotropyMode::X2, "2x"),
    (AnisotropyMode::X4, "4x"),
    (AnisotropyMode::X8, "8x"),
    (AnisotropyMode::X16, "16x"),
];

pub const LANGUAGE: &[(Language, &str)] = &[
    (Language::Japanese, "Japanese (日本語)"),
    (Language::EnglishAmerican, "American English"),
    (Language::French, "French (français)"),
    (Language::German, "German (Deutsch)"),
    (Language::Italian, "Italian (italiano)"),
    (Language::Spanish, "Spanish (español)"),
    (Language::Chinese, "Chinese"),
    (Language::Korean, "Korean (한국어)"),
    (Language::Dutch, "Dutch (Nederlands)"),
    (Language::Portuguese, "Portuguese (português)"),
    (Language::Russian, "Russian (Русский)"),
    (Language::Taiwanese, "Taiwanese"),
    (Language::EnglishBritish, "British English"),
    (Language::FrenchCanadian, "Canadian French"),
    (Language::SpanishLatin, "Latin American Spanish"),
    (Language::ChineseSimplified, "Simplified Chinese"),
    (
        Language::ChineseTraditional,
        "Traditional Chinese (正體中文)",
    ),
    (
        Language::PortugueseBrazilian,
        "Brazilian Portuguese (português do Brasil)",
    ),
];

pub const REGION: &[(Region, &str)] = &[
    (Region::Japan, "Japan"),
    (Region::Usa, "USA"),
    (Region::Europe, "Europe"),
    (Region::Australia, "Australia"),
    (Region::China, "China"),
    (Region::Korea, "Korea"),
    (Region::Taiwan, "Taiwan"),
];

pub const AUDIO_MODE: &[(AudioMode, &str)] = &[
    (AudioMode::Mono, "Mono"),
    (AudioMode::Stereo, "Stereo"),
    (AudioMode::Surround, "Surround"),
];

pub const MEMORY_LAYOUT: &[(MemoryLayout, &str)] = &[
    (MemoryLayout::Memory4Gb, "4GB DRAM (Default)"),
    (MemoryLayout::Memory6Gb, "6GB DRAM (Unsafe)"),
    (MemoryLayout::Memory8Gb, "8GB DRAM (Unsafe)"),
];

pub const CONSOLE_MODE: &[(ConsoleMode, &str)] = &[
    (ConsoleMode::Docked, "Docked"),
    (ConsoleMode::Handheld, "Handheld"),
];

pub const CONFIRM_STOP: &[(ConfirmStop, &str)] = &[
    (ConfirmStop::AskAlways, "Always ask (Default)"),
    (
        ConfirmStop::AskBasedOnGame,
        "Only if game specifies not to stop",
    ),
    (ConfirmStop::AskNever, "Never ask"),
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tables_are_ordered_like_upstream() {
        // Row order is part of the UI contract; upstream lists GPU last for
        // NVDEC and marks it "(Default)".
        assert_eq!(NVDEC_EMULATION[2].1, "GPU Video Decoding (Default)");
        assert_eq!(RESOLUTION_SETUP[2].1, "1X (720p/1080p)");
        assert_eq!(CONSOLE_MODE[0].1, "Docked");
    }

    #[test]
    fn index_of_round_trips_through_value_at() {
        let idx = index_of(ASPECT_RATIO, &AspectRatio::R21_9);
        assert_eq!(idx, 2);
        assert_eq!(value_at(ASPECT_RATIO, idx), AspectRatio::R21_9);
    }

    #[test]
    fn index_of_falls_back_to_first_row() {
        // `ScalingFilter::MaxEnum` is a sentinel, never a real UI row.
        assert_eq!(index_of(SCALING_FILTER, &ScalingFilter::MaxEnum), 0);
    }

    #[test]
    fn labels_extracts_the_display_column() {
        let l = labels(CPU_BACKEND);
        assert_eq!(l, vec!["Dynarmic", "NCE"]);
    }

    #[test]
    fn console_mode_table_order_differs_from_enum_order() {
        // Upstream lists Docked first in the UI even though the enum declares
        // Handheld first — a real ordering divergence worth pinning down.
        assert_eq!(CONSOLE_MODE[0].0, ConsoleMode::Docked);
        assert_eq!(CONSOLE_MODE[1].0, ConsoleMode::Handheld);
    }
}
