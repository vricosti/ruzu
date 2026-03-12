// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/ir/modifiers.h`
//!
//! IR instruction modifiers: floating-point control, texture info.
//! Note: FmzMode, FpRounding, FpControl, and TextureInstInfo are already
//! defined in `ir/types.rs`. This module re-exports them and adds the
//! upstream-faithful ImageFormat reference from shader_info.

// Re-export the existing types from types.rs which already match upstream
pub use super::types::{FmzMode, FpControl, FpRounding, TextureInstInfo};

// The upstream modifiers.h also references TextureType and ImageFormat from shader_info.h.
// Those are available in shader_info.rs.
