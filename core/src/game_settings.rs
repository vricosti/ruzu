// SPDX-FileCopyrightText: 2026 Eden Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `core/game_settings.{h,cpp}`.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Os {
    Windows,
    Linux,
    MacOs,
    Ios,
    Android,
    FireOs,
    HarmonyOs,
    FreeBsd,
    DragonFlyBsd,
    NetBsd,
    OpenBsd,
    HaikuOs,
    Aix,
    Managarm,
    RedoxOs,
    Solaris,
    #[default]
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum GpuVendor {
    Nvidia,
    Amd,
    Intel,
    Apple,
    Qualcomm,
    Arm,
    Imagination,
    Microsoft,
    #[default]
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EnvironmentInfo {
    pub os: Os,
    pub vendor: GpuVendor,
    pub vendor_string: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
pub enum TitleId {
    NinjaGaidenRagebound = 0x0100_7810_2071_0000,
}

fn get_gpu(gpu_vendor_string: &str) -> GpuVendor {
    const GPU_VENDORS: &[(&str, GpuVendor)] = &[
        ("NVIDIA", GpuVendor::Nvidia),
        ("Nouveau", GpuVendor::Nvidia),
        ("NVK", GpuVendor::Nvidia),
        ("Tegra", GpuVendor::Nvidia),
        ("AMD", GpuVendor::Amd),
        ("RadeonSI", GpuVendor::Amd),
        ("RADV", GpuVendor::Amd),
        ("AMDVLK", GpuVendor::Amd),
        ("R600", GpuVendor::Amd),
        ("Intel", GpuVendor::Intel),
        ("ANV", GpuVendor::Intel),
        ("i965", GpuVendor::Intel),
        ("i915", GpuVendor::Intel),
        ("OpenSWR", GpuVendor::Intel),
        ("Apple", GpuVendor::Apple),
        ("MoltenVK", GpuVendor::Apple),
        ("Qualcomm", GpuVendor::Qualcomm),
        ("Turnip", GpuVendor::Qualcomm),
        ("Mali", GpuVendor::Arm),
        ("PanVK", GpuVendor::Arm),
        ("PowerVR", GpuVendor::Imagination),
        ("PVR", GpuVendor::Imagination),
        ("D3D12", GpuVendor::Microsoft),
        ("Microsoft", GpuVendor::Microsoft),
        ("WARP", GpuVendor::Microsoft),
    ];

    if let Some((_, vendor)) = GPU_VENDORS
        .iter()
        .find(|(name, _)| gpu_vendor_string == *name)
    {
        return *vendor;
    }

    let gpu = gpu_vendor_string.to_ascii_lowercase();
    if gpu.contains("geforce") {
        return GpuVendor::Nvidia;
    }
    if gpu.contains("radeon") || gpu.contains("ati") {
        return GpuVendor::Amd;
    }
    GpuVendor::Unknown
}

fn detect_os() -> Os {
    if cfg!(target_os = "windows") {
        Os::Windows
    } else if cfg!(target_os = "linux") {
        Os::Linux
    } else if cfg!(target_os = "macos") {
        Os::MacOs
    } else if cfg!(target_os = "ios") {
        Os::Ios
    } else if cfg!(target_os = "android") {
        Os::Android
    } else if cfg!(target_os = "freebsd") {
        Os::FreeBsd
    } else if cfg!(target_os = "dragonfly") {
        Os::DragonFlyBsd
    } else if cfg!(target_os = "netbsd") {
        Os::NetBsd
    } else if cfg!(target_os = "openbsd") {
        Os::OpenBsd
    } else if cfg!(target_os = "redox") {
        Os::RedoxOs
    } else if cfg!(target_os = "solaris") {
        Os::Solaris
    } else {
        Os::Unknown
    }
}

pub fn detect_environment(gpu_vendor_string: String) -> EnvironmentInfo {
    EnvironmentInfo {
        os: detect_os(),
        vendor: get_gpu(&gpu_vendor_string),
        vendor_string: gpu_vendor_string,
    }
}

pub fn load_overrides(program_id: u64, gpu_vendor_string: String) {
    let env = detect_environment(gpu_vendor_string);

    if program_id == TitleId::NinjaGaidenRagebound as u64 {
        common::settings::values_mut().use_squashed_iterated_blend = true;
    }

    log::info!(
        "Applied game settings for title ID {:016X} on OS {:?}, GPU vendor {:?} ({})",
        program_id,
        env.os,
        env.vendor,
        env.vendor_string
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gpu_vendor_table_and_legacy_fallback_match_upstream() {
        assert_eq!(get_gpu("RADV"), GpuVendor::Amd);
        assert_eq!(get_gpu("MoltenVK"), GpuVendor::Apple);
        assert_eq!(get_gpu("GeForce RTX"), GpuVendor::Nvidia);
        assert_eq!(get_gpu("ATI Technologies"), GpuVendor::Amd);
        assert_eq!(get_gpu("unknown"), GpuVendor::Unknown);
    }

    #[test]
    fn ninja_gaiden_ragebound_enables_squashed_iterated_blend() {
        let previous = common::settings::values().use_squashed_iterated_blend;
        load_overrides(TitleId::NinjaGaidenRagebound as u64, "NVIDIA".to_string());
        assert!(common::settings::values().use_squashed_iterated_blend);
        common::settings::values_mut().use_squashed_iterated_blend = previous;
    }
}
