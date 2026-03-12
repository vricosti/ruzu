// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `backend/bindings.h`
//!
//! Descriptor binding counters used during SPIR-V emission to assign
//! sequential binding indices to uniform buffers, storage buffers,
//! textures, and images.

/// Descriptor binding counters for SPIR-V emission.
#[derive(Debug, Clone, Default)]
pub struct Bindings {
    /// Unified binding counter (for unified descriptor sets).
    pub unified: u32,
    /// Uniform buffer binding counter.
    pub uniform_buffer: u32,
    /// Storage buffer binding counter.
    pub storage_buffer: u32,
    /// Texture binding counter.
    pub texture: u32,
    /// Image binding counter.
    pub image: u32,
    /// Texture scaling index counter.
    pub texture_scaling_index: u32,
    /// Image scaling index counter.
    pub image_scaling_index: u32,
}
