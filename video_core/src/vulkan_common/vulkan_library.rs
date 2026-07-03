// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `zuyu/src/video_core/vulkan_common/vulkan_library.h` and
//! `zuyu/src/video_core/vulkan_common/vulkan_library.cpp`.
//!
//! Loads the Vulkan library and returns an `ash::Entry`.
//! In C++ this uses `Common::DynamicLibrary` to dlopen libvulkan;
//! in Rust, `ash::Entry::linked()` or `ash::Entry::load()` handles this.

use super::vulkan_wrapper::VulkanError;
use ash::vk;

/// Opens the Vulkan library and returns an `ash::Entry`.
///
/// Port of `Vulkan::OpenLibrary`.
///
/// On Linux, this attempts to load `libvulkan.so.1` then `libvulkan.so`.
/// On macOS, this would look for MoltenVK paths.
/// On Android, the frontend provides the library.
///
/// Using `ash::Entry::linked()` requires linking against libvulkan at build time.
/// Using `ash::Entry::load()` dynamically loads it at runtime (matching the C++ approach).
pub fn open_library() -> Result<ash::Entry, VulkanError> {
    log::debug!("Looking for a Vulkan library");

    #[cfg(target_os = "macos")]
    {
        return open_library_macos();
    }

    #[cfg(not(target_os = "macos"))]
    {
        return open_library_default();
    }
}

#[cfg(not(target_os = "macos"))]
fn open_library_default() -> Result<ash::Entry, VulkanError> {
    // ash::Entry::load() will dlopen libvulkan.so / vulkan-1.dll / etc.
    // This matches the C++ behavior of dynamically loading the Vulkan library.
    match unsafe { ash::Entry::load() } {
        Ok(entry) => {
            log::debug!("Vulkan library loaded successfully");
            Ok(entry)
        }
        Err(e) => {
            log::error!("Failed to load Vulkan library: {}", e);
            Err(VulkanError::new(vk::Result::ERROR_INITIALIZATION_FAILED))
        }
    }
}

#[cfg(target_os = "macos")]
fn open_library_macos() -> Result<ash::Entry, VulkanError> {
    use std::path::PathBuf;

    let mut library_paths = Vec::<PathBuf>::new();

    // Upstream first allows an explicit Vulkan loader override.
    if let Some(path) = std::env::var_os("LIBVULKAN_PATH").filter(|path| !path.is_empty()) {
        library_paths.push(PathBuf::from(path));
    }

    // Upstream app bundle lookup:
    // Common::FS::GetBundleDirectory() / Contents/Frameworks/libvulkan.1.dylib
    // Common::FS::GetBundleDirectory() / Contents/Frameworks/libMoltenVK.dylib
    if let Some(bundle_dir) = bundle_directory() {
        library_paths.push(bundle_dir.join("Contents/Frameworks/libvulkan.1.dylib"));
        library_paths.push(bundle_dir.join("Contents/Frameworks/libMoltenVK.dylib"));
    }

    // Non-bundled ruzu-cmd development fallback on macOS. Prefer a Vulkan
    // loader from the SDK before loading MoltenVK directly; the loader owns
    // portability-enumeration/ICD discovery semantics.
    library_paths.extend(vulkan_sdk_paths());
    library_paths.extend(local_yuzu_bundle_paths());
    library_paths.extend([
        PathBuf::from("/opt/homebrew/lib/libvulkan.1.dylib"),
        PathBuf::from("/opt/homebrew/lib/libvulkan.dylib"),
        PathBuf::from("/opt/homebrew/lib/libMoltenVK.dylib"),
        PathBuf::from("/usr/local/lib/libvulkan.1.dylib"),
        PathBuf::from("/usr/local/lib/libvulkan.dylib"),
        PathBuf::from("/usr/local/lib/libMoltenVK.dylib"),
    ]);
    library_paths.extend(moltenvk_derived_data_paths());
    library_paths.extend(android_sdk_vulkan_paths());

    let mut errors = Vec::<String>::new();
    for library_path in library_paths {
        if !library_path.exists() {
            continue;
        }
        configure_macos_vulkan_icd(&library_path);
        log::debug!("Trying Vulkan library: {}", library_path.display());
        match unsafe { ash::Entry::load_from(library_path.as_os_str()) } {
            Ok(entry) => {
                log::info!("Vulkan library loaded from {}", library_path.display());
                return Ok(entry);
            }
            Err(err) => {
                errors.push(format!("{}: {}", library_path.display(), err));
            }
        }
    }

    match unsafe { ash::Entry::load() } {
        Ok(entry) => {
            log::debug!("Vulkan library loaded through ash default loader");
            Ok(entry)
        }
        Err(err) => {
            if errors.is_empty() {
                log::error!("Failed to load Vulkan library: {}", err);
            } else {
                log::error!(
                    "Failed to load Vulkan library: {}; explicit attempts: {}",
                    err,
                    errors.join("; ")
                );
            }
            Err(VulkanError::new(vk::Result::ERROR_INITIALIZATION_FAILED))
        }
    }
}

#[cfg(target_os = "macos")]
fn configure_macos_vulkan_icd(library_path: &std::path::Path) {
    if std::env::var_os("VK_ICD_FILENAMES")
        .filter(|path| !path.is_empty())
        .is_some()
    {
        return;
    }

    let Some(file_name) = library_path.file_name().and_then(|name| name.to_str()) else {
        return;
    };
    if !matches!(file_name, "libvulkan.1.dylib" | "libvulkan.dylib") {
        return;
    }

    let Some(lib_dir) = library_path.parent() else {
        return;
    };
    let Some(sdk_root) = lib_dir.parent() else {
        return;
    };
    let icd_path = sdk_root.join("share/vulkan/icd.d/MoltenVK_icd.json");
    if !icd_path.exists() {
        return;
    }

    log::info!(
        "Using Vulkan ICD manifest from {}",
        icd_path.display()
    );
    std::env::set_var("VK_ICD_FILENAMES", icd_path);
}

#[cfg(target_os = "macos")]
fn vulkan_sdk_paths() -> Vec<std::path::PathBuf> {
    let mut sdk_roots = Vec::<std::path::PathBuf>::new();
    if let Some(path) = std::env::var_os("VULKAN_SDK").filter(|path| !path.is_empty()) {
        sdk_roots.push(std::path::PathBuf::from(path));
    }
    if let Some(home) = std::env::var_os("HOME") {
        let dev_sdk_root = std::path::PathBuf::from(home).join("Dev/emulators/VulkanSDK");
        if let Ok(entries) = std::fs::read_dir(dev_sdk_root) {
            let mut versions = entries
                .flatten()
                .map(|entry| entry.path().join("macOS"))
                .filter(|path| path.is_dir())
                .collect::<Vec<_>>();
            versions.sort();
            versions.reverse();
            sdk_roots.extend(versions);
        }
    }

    let mut paths = Vec::new();
    for root in sdk_roots {
        let lib = root.join("lib");
        paths.extend([
            lib.join("libvulkan.1.dylib"),
            lib.join("libvulkan.dylib"),
            lib.join("libMoltenVK.dylib"),
        ]);
    }
    paths
}

#[cfg(target_os = "macos")]
fn local_yuzu_bundle_paths() -> Vec<std::path::PathBuf> {
    let Some(home) = std::env::var_os("HOME") else {
        return Vec::new();
    };
    let root = std::path::PathBuf::from(home).join("Dev/emulators/zuyu/build/bin/yuzu.app");
    vec![
        root.join("Contents/Frameworks/libvulkan.1.dylib"),
        root.join("Contents/Frameworks/libMoltenVK.dylib"),
    ]
}

#[cfg(target_os = "macos")]
fn bundle_directory() -> Option<std::path::PathBuf> {
    let exe_path = std::env::current_exe().ok()?;
    let mut path = exe_path.as_path();
    while let Some(parent) = path.parent() {
        if parent
            .extension()
            .is_some_and(|extension| extension == "app")
        {
            return Some(parent.to_path_buf());
        }
        path = parent;
    }
    None
}

#[cfg(target_os = "macos")]
fn moltenvk_derived_data_paths() -> Vec<std::path::PathBuf> {
    let Some(home) = std::env::var_os("HOME") else {
        return Vec::new();
    };
    let derived_data = std::path::PathBuf::from(home).join("Library/Developer/Xcode/DerivedData");
    let Ok(entries) = std::fs::read_dir(derived_data) else {
        return Vec::new();
    };
    entries
        .flatten()
        .map(|entry| entry.path())
        .filter(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name.starts_with("MoltenVKPackaging-"))
        })
        .map(|path| path.join("Build/Products/Release/libMoltenVK.dylib"))
        .collect()
}

#[cfg(target_os = "macos")]
fn android_sdk_vulkan_paths() -> Vec<std::path::PathBuf> {
    let sdk_root = std::env::var_os("ANDROID_HOME")
        .or_else(|| std::env::var_os("ANDROID_SDK_ROOT"))
        .map(std::path::PathBuf::from)
        .or_else(|| {
            std::env::var_os("HOME")
                .map(std::path::PathBuf::from)
                .map(|home| home.join("Library/Android/sdk"))
        });
    let Some(sdk_root) = sdk_root else {
        return Vec::new();
    };
    let vulkan_dir = sdk_root.join("emulator/lib64/vulkan");
    vec![
        vulkan_dir.join("libMoltenVK.dylib"),
        vulkan_dir.join("libvulkan.dylib"),
    ]
}

#[cfg(test)]
mod tests {
    // Note: These tests require a Vulkan runtime to be present on the system.
    // They are kept minimal to avoid CI failures on headless systems.

    #[test]
    fn test_open_library_does_not_panic() {
        // Just verify we don't panic; the actual result depends on the host.
        let _result = super::open_library();
    }
}
