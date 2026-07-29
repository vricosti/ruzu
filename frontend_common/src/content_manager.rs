// SPDX-FileCopyrightText: 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/frontend_common/content_manager.h
//!
//! Provides enums and helper functions for managing installed content
//! (DLC, updates, mods, NSP, NCA) and verifying game integrity.

use ruzu_core::crypto::key_manager::KeyManager;
use ruzu_core::file_sys::registered_cache::{ContentProvider, RegisteredCache};
use ruzu_core::hle::service::filesystem::filesystem::FileSystemController;
use ruzu_core::loader::loader::{AppLoader, ResultStatus};
use ruzu_core::loader::nca::AppLoaderNca;

// ---------------------------------------------------------------------------
// Enums
// ---------------------------------------------------------------------------

/// Result of an installation operation.
/// Maps to C++ `ContentManager::InstallResult`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InstallResult {
    Success,
    Overwrite,
    Failure,
    BaseInstallAttempted,
}

/// Result of a game verification operation.
/// Maps to C++ `ContentManager::GameVerificationResult`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GameVerificationResult {
    Success,
    Failed,
    NotImplemented,
}

// ---------------------------------------------------------------------------
// Content management functions
// ---------------------------------------------------------------------------

/// Removes a single installed DLC.
///
/// Maps to C++ `ContentManager::RemoveDLC`.
///
/// # Arguments
/// * `title_id` - Unique title ID representing the DLC which will be removed.
///
/// NOTE: Requires `Core::System` filesystem controller; stubbed.
pub fn remove_dlc(_title_id: u64) -> bool {
    // NOTE: Full implementation uses Core::System::GetFileSystemController to
    // find and remove the NCA for the given DLC title_id.
    log::warn!("remove_dlc: Core::System not integrated, returning false");
    false
}

/// Removes all DLC for a game.
///
/// Maps to C++ `ContentManager::RemoveAllDLC`.
///
/// # Arguments
/// * `program_id` - Program ID for the game that will have all of its DLC removed.
///
/// NOTE: Requires `Core::System`; stubbed.
pub fn remove_all_dlc(_program_id: u64) -> usize {
    // NOTE: Full implementation enumerates all DLC NCAs for program_id via
    // Core::System's filesystem controller and removes them, returning the count.
    log::warn!("remove_all_dlc: Core::System not integrated, returning 0");
    0
}

/// Removes the installed update for a game.
///
/// Maps to C++ `ContentManager::RemoveUpdate`.
///
/// # Arguments
/// * `program_id` - Program ID for the game that will have its installed update removed.
///
/// NOTE: Requires filesystem controller; stubbed.
pub fn remove_update(_program_id: u64) -> bool {
    // NOTE: Full implementation removes the update NCA for program_id via
    // Core::System's filesystem controller.
    log::warn!("remove_update: Core::System not integrated, returning false");
    false
}

/// Removes the base content for a game.
///
/// Maps to C++ `ContentManager::RemoveBaseContent`.
///
/// # Arguments
/// * `program_id` - Program ID for the game that will have its base content removed.
///
/// NOTE: Requires filesystem controller; stubbed.
pub fn remove_base_content(_program_id: u64) -> bool {
    // NOTE: Full implementation removes the base NCA for program_id via
    // Core::System's filesystem controller.
    log::warn!("remove_base_content: Core::System not integrated, returning false");
    false
}

/// Removes a mod for a game.
///
/// Maps to C++ `ContentManager::RemoveMod`.
///
/// # Arguments
/// * `program_id` - Program ID for the game where `mod_name` will be removed.
/// * `mod_name` - The name of a mod as given by `FileSys::PatchManager::GetPatches`.
///
/// NOTE: Requires filesystem controller; stubbed.
pub fn remove_mod(_program_id: u64, _mod_name: &str) -> bool {
    // NOTE: Full implementation removes a mod folder under the load directory
    // for program_id, matching mod_name from PatchManager::GetPatches.
    log::warn!("remove_mod: Core::System not integrated, returning false");
    false
}

/// Installs an NSP.
///
/// Maps to C++ `ContentManager::InstallNSP`.
///
/// # Arguments
/// * `filename` - Path to the NSP file.
/// * `callback` - Callback to report progress. Returns true to cancel.
///
/// NOTE: Requires `Core::System` and VFS; stubbed.
pub fn install_nsp(_filename: &str, _callback: &dyn Fn(usize, usize) -> bool) -> InstallResult {
    // NOTE: Full implementation opens the NSP as a VFS partition, iterates its
    // NCA files and installs them into the system registered cache via
    // Core::System's content manager.
    log::warn!("install_nsp: Core::System/VFS not integrated, returning Failure");
    InstallResult::Failure
}

/// Installs an NCA.
///
/// Maps to C++ `ContentManager::InstallNCA`.
///
/// # Arguments
/// * `filename` - Path to the NCA file.
/// * `callback` - Callback to report progress. Returns true to cancel.
///
/// NOTE: Requires VFS and RegisteredCache; stubbed.
pub fn install_nca(_filename: &str, _callback: &dyn Fn(usize, usize) -> bool) -> InstallResult {
    log::warn!("install_nca: VFS and RegisteredCache not integrated, returning Failure");
    InstallResult::Failure
}

/// Verifies the installed contents.
///
/// Maps to C++ `ContentManager::VerifyInstalledContents`.
///
/// Walks the System NAND registry (and the User NAND one unless `firmware_only`),
/// hashes every installed NCA through `AppLoader_NCA::VerifyIntegrity`, and
/// returns the names of the files that failed.
///
/// # Arguments
/// * `filesystem` - the controller owning the NAND registries.
/// * `callback` - progress reporter, `(total_bytes, processed_bytes)`; returning
///   `true` cancels, matching upstream's convention.
/// * `firmware_only` - only scan System NAND NCAs.
pub fn verify_installed_contents(
    filesystem: &FileSystemController,
    callback: &dyn Fn(usize, usize) -> bool,
    firmware_only: bool,
) -> Vec<String> {
    // Get content registries.
    let mut providers: Vec<&RegisteredCache> = Vec::new();
    if let Some(bis) = filesystem.get_system_nand_contents() {
        providers.push(bis);
    }
    if !firmware_only {
        if let Some(user) = filesystem.get_user_nand_contents() {
            providers.push(user);
        }
    }

    // Collect the associated NCA files, and the total size for progress.
    let mut nca_files = Vec::new();
    let mut total_size = 0usize;
    for provider in providers {
        for entry in provider.list_entries_filter(None, None, None) {
            let Some(nca_file) = provider.get_entry_raw(entry.title_id, entry.record_type) else {
                continue;
            };
            total_size += nca_file.get_size() as usize;
            nca_files.push(nca_file);
        }
    }

    log::info!(
        "Verifying {} installed NCA(s), {total_size} byte(s) total",
        nca_files.len()
    );

    let mut failed = Vec::new();
    let mut processed_size = 0usize;

    for nca_file in nca_files {
        let name = nca_file.get_name();
        let size = nca_file.get_size() as usize;

        // Upstream forwards the running total so the bar advances across files
        // rather than restarting for each one.
        let cancelled = std::cell::Cell::new(false);
        let nca_callback = |nca_total: usize, nca_processed: usize| {
            let _ = nca_total;
            cancelled.set(callback(total_size, processed_size + nca_processed));
            !cancelled.get()
        };

        let loader = AppLoaderNca::new(nca_file);
        match loader.verify_integrity(&nca_callback) {
            ResultStatus::Success => {}
            // A file the verifier cannot check is not a *failed* file; upstream
            // likewise only records genuine mismatches.
            ResultStatus::ErrorIntegrityVerificationNotImplemented => {
                log::debug!("Skipping integrity check for {name}");
            }
            status => {
                log::warn!("Integrity verification failed for {name}: {status:?}");
                failed.push(name);
            }
        }

        if cancelled.get() {
            break;
        }
        processed_size += size;
    }

    failed
}

/// Verifies the contents of a given game.
///
/// Maps to C++ `ContentManager::VerifyGameContents`.
///
/// # Arguments
/// * `game_path` - Path to the game file.
/// * `callback` - Callback to report progress. Returns true to cancel.
///
/// NOTE: Requires `Core::System`; stubbed.
pub fn verify_game_contents(
    _game_path: &str,
    _callback: &dyn Fn(usize, usize) -> bool,
) -> GameVerificationResult {
    log::warn!("verify_game_contents: Core::System not integrated, returning NotImplemented");
    GameVerificationResult::NotImplemented
}

/// Checks if the keys required for decrypting firmware and games are available.
///
/// Maps to C++ `ContentManager::AreKeysPresent`:
///
/// ```cpp
/// return !Core::Crypto::KeyManager::Instance().BaseDeriveNecessary();
/// ```
pub fn are_keys_present() -> bool {
    let keys = KeyManager::instance();
    let present = !keys.lock().unwrap().base_derive_necessary();
    log::debug!("are_keys_present: {present}");
    present
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_install_result_values() {
        assert_ne!(InstallResult::Success, InstallResult::Failure);
        assert_ne!(
            InstallResult::Overwrite,
            InstallResult::BaseInstallAttempted
        );
    }

    #[test]
    fn test_game_verification_result_values() {
        assert_ne!(
            GameVerificationResult::Success,
            GameVerificationResult::Failed
        );
        assert_ne!(
            GameVerificationResult::Failed,
            GameVerificationResult::NotImplemented
        );
    }
}
