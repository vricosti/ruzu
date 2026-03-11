// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_crypto_configuration.h / .cpp

use super::nca_file_system_driver::NcaCryptoConfiguration;

/// Get the global NCA crypto configuration.
/// Corresponds to upstream `GetCryptoConfiguration`.
pub fn get_crypto_configuration() -> NcaCryptoConfiguration {
    // TODO: populate with actual keys from KeyManager.
    NcaCryptoConfiguration::default()
}
