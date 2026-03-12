// SPDX-FileCopyrightText: 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/frontend_common/content_manager.h
//!
//! Provides enums and helper functions for managing installed content
//! (DLC, updates, mods, NSP, NCA) and verifying game integrity.

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
    todo!("remove_dlc requires Core::System filesystem controller")
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
    todo!("remove_all_dlc requires Core::System")
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
    todo!("remove_update requires Core::System filesystem controller")
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
    todo!("remove_base_content requires Core::System filesystem controller")
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
    todo!("remove_mod requires Core::System filesystem controller")
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
pub fn install_nsp(
    _filename: &str,
    _callback: &dyn Fn(usize, usize) -> bool,
) -> InstallResult {
    todo!("install_nsp requires Core::System and VFS")
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
pub fn install_nca(
    _filename: &str,
    _callback: &dyn Fn(usize, usize) -> bool,
) -> InstallResult {
    todo!("install_nca requires VFS and RegisteredCache")
}

/// Verifies the installed contents.
///
/// Maps to C++ `ContentManager::VerifyInstalledContents`.
///
/// # Arguments
/// * `callback` - Callback to report progress. Returns true to cancel.
/// * `firmware_only` - Set to true to only scan system NAND NCAs.
///
/// NOTE: Requires `Core::System`; stubbed.
pub fn verify_installed_contents(
    _callback: &dyn Fn(usize, usize) -> bool,
    _firmware_only: bool,
) -> Vec<String> {
    todo!("verify_installed_contents requires Core::System")
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
    todo!("verify_game_contents requires Core::System")
}

/// Checks if the keys required for decrypting firmware and games are available.
///
/// Maps to C++ `ContentManager::AreKeysPresent`.
///
/// NOTE: Requires `Core::Crypto::KeyManager`; stubbed.
pub fn are_keys_present() -> bool {
    todo!("are_keys_present requires Core::Crypto::KeyManager")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_install_result_values() {
        assert_ne!(InstallResult::Success, InstallResult::Failure);
        assert_ne!(InstallResult::Overwrite, InstallResult::BaseInstallAttempted);
    }

    #[test]
    fn test_game_verification_result_values() {
        assert_ne!(GameVerificationResult::Success, GameVerificationResult::Failed);
        assert_ne!(
            GameVerificationResult::Failed,
            GameVerificationResult::NotImplemented
        );
    }
}
