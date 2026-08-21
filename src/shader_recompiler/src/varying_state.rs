// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `shader_recompiler/varying_state.h`
//!
//! Bitset tracking which shader varyings (attributes) are active.

/// Tracks which shader varyings are loaded/stored via a 512-bit bitmask.
///
/// Attribute indices map directly to `IR::Attribute` values.
#[derive(Debug, Clone, Default)]
pub struct VaryingState {
    /// 512-bit mask stored as 8 x u64.
    pub mask: [u64; 8],
}

impl VaryingState {
    /// Set the bit for the given attribute index.
    pub fn set(&mut self, attribute: usize, state: bool) {
        let word = attribute / 64;
        let bit = attribute % 64;
        if word < 8 {
            if state {
                self.mask[word] |= 1u64 << bit;
            } else {
                self.mask[word] &= !(1u64 << bit);
            }
        }
    }

    /// Get the state of the given attribute index.
    pub fn get(&self, attribute: usize) -> bool {
        let word = attribute / 64;
        let bit = attribute % 64;
        if word < 8 {
            (self.mask[word] >> bit) & 1 != 0
        } else {
            false
        }
    }

    /// Check if any of the 4 components of an attribute base are set.
    pub fn any_component(&self, base: usize) -> bool {
        self.get(base) || self.get(base + 1) || self.get(base + 2) || self.get(base + 3)
    }

    /// Check if all 4 components of an attribute base are set.
    pub fn all_components(&self, base: usize) -> bool {
        self.get(base) && self.get(base + 1) && self.get(base + 2) && self.get(base + 3)
    }

    /// Check if all or none of the 4 components are set (uniform state).
    pub fn is_uniform(&self, base: usize) -> bool {
        self.any_component(base) == self.all_components(base)
    }

    /// Check if a specific generic attribute component is set.
    /// `index` is the generic index (0..31), `component` is 0..3.
    pub fn generic(&self, index: usize, component: usize) -> bool {
        // Generic0X starts at attribute index 32
        self.get(32 + index * 4 + component)
    }

    /// Check if any component of a generic attribute is set.
    pub fn generic_any(&self, index: usize) -> bool {
        self.generic(index, 0)
            || self.generic(index, 1)
            || self.generic(index, 2)
            || self.generic(index, 3)
    }

    /// Check if any clip distance is set.
    pub fn clip_distances(&self) -> bool {
        // ClipDistance0 = 176
        self.any_component(176) || self.any_component(180)
    }

    /// Check if any legacy (fixed-function) varying is set.
    pub fn legacy(&self) -> bool {
        // ColorFrontDiffuseR = 160
        self.any_component(160)
            || self.any_component(164) // ColorFrontSpecular
            || self.any_component(168) // ColorBackDiffuse
            || self.any_component(172) // ColorBackSpecular
            || self.fixed_function_texture()
            || self.get(186) // FogCoordinate
    }

    /// Check if any fixed-function texture coordinate is set.
    pub fn fixed_function_texture(&self) -> bool {
        // FixedFncTexture0S = 192
        for index in 0..10 {
            if self.any_component(192 + index * 4) {
                return true;
            }
        }
        false
    }
}
