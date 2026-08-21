// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssrv/fssrv_sf_path.h

use crate::file_sys::fs_directory::ENTRY_NAME_LENGTH_MAX;

/// SF path used in filesystem service IPC.
/// Corresponds to upstream `FileSys::Sf::Path`.
#[derive(Clone)]
#[repr(C)]
pub struct Path {
    pub str_buf: [u8; ENTRY_NAME_LENGTH_MAX + 1],
}

impl Path {
    /// Encode a string into a Path.
    /// Corresponds to upstream `Path::Encode`.
    pub fn encode(p: &str) -> Self {
        let mut path = Self {
            str_buf: [0u8; ENTRY_NAME_LENGTH_MAX + 1],
        };
        let bytes = p.as_bytes();
        let len = std::cmp::min(bytes.len(), ENTRY_NAME_LENGTH_MAX);
        path.str_buf[..len].copy_from_slice(&bytes[..len]);
        path
    }

    /// Get the length of the path string.
    /// Corresponds to upstream `Path::GetPathLength`.
    pub fn get_path_length(&self) -> usize {
        self.str_buf
            .iter()
            .position(|&b| b == 0)
            .unwrap_or(ENTRY_NAME_LENGTH_MAX)
    }

    /// Get the path as a string slice.
    pub fn as_str(&self) -> &str {
        let len = self.get_path_length();
        std::str::from_utf8(&self.str_buf[..len]).unwrap_or("")
    }
}

impl Default for Path {
    fn default() -> Self {
        Self {
            str_buf: [0u8; ENTRY_NAME_LENGTH_MAX + 1],
        }
    }
}

/// Type alias matching upstream `FspPath`.
pub type FspPath = Path;
