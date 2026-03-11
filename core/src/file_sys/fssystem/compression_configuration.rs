// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_compression_configuration.h / .cpp

use super::nca_file_system_driver::NcaCompressionConfiguration;

/// Get the global NCA compression configuration.
/// Corresponds to upstream `GetNcaCompressionConfiguration`.
pub fn get_nca_compression_configuration() -> NcaCompressionConfiguration {
    // TODO: wire up actual decompressor (LZ4, etc.)
    NcaCompressionConfiguration::default()
}
