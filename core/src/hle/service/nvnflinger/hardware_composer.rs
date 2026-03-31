// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/hardware_composer.h
//! Port of zuyu/src/core/hle/service/nvnflinger/hardware_composer.cpp
//!
//! The hardware composer is responsible for composing layers into
//! a final framebuffer for presentation. It collects HwcLayer data
//! from each visible layer and submits them to the GPU for rendering.

use super::hwc_layer::HwcLayer;

/// The hardware composer collects layers and presents them.
///
/// In upstream C++, this manages the composition pipeline including
/// fence management and framebuffer submission. The full implementation
/// depends on the GPU renderer infrastructure.
pub struct HardwareComposer {
    layers: Vec<HwcLayer>,
}

impl HardwareComposer {
    pub fn new() -> Self {
        Self { layers: Vec::new() }
    }

    /// Clear the layer list for a new composition pass.
    pub fn clear_layers(&mut self) {
        self.layers.clear();
    }

    /// Add a layer to the composition.
    pub fn add_layer(&mut self, layer: HwcLayer) {
        self.layers.push(layer);
    }

    /// Get the current layers.
    pub fn layers(&self) -> &[HwcLayer] {
        &self.layers
    }

    /// Sort layers by z-index for proper ordering.
    pub fn sort_layers(&mut self) {
        self.layers.sort_by_key(|l| l.z_index);
    }
}

impl Default for HardwareComposer {
    fn default() -> Self {
        Self::new()
    }
}
