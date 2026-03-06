// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Null rendering backend — all draw calls are silently ignored.
//!
//! Used during early GPU bring-up when we can parse commands but
//! don't yet have a real renderer.

use super::GpuBackend;

pub struct NullBackend;

impl NullBackend {
    pub fn new() -> Self {
        Self
    }
}

impl Default for NullBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl GpuBackend for NullBackend {
    fn name(&self) -> &str {
        "null"
    }
}
