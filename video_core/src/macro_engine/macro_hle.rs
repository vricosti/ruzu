// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/macro/macro_hle.h` and `macro_hle.cpp`.
//!
//! High-Level Emulation (HLE) of known macro programs. When a macro's hash
//! matches a known program, the HLE implementation is used instead of
//! interpreting/JIT-compiling the macro code, providing significant speedup.

use std::collections::HashMap;

use super::macro_engine::CachedMacro;
use crate::engines::maxwell_3d::Maxwell3D;

// ── Known HLE program hashes ─────────────────────────────────────────────────

// These are the Common::HashValue(code) values of known upstream macro programs.
// Port of the hash constants from the `HLEMacro::HLEMacro` constructor.

const HASH_DRAW_ARRAYS_INDIRECT: u64 = 0x0D61FC9FAAC9FCAD;
const HASH_DRAW_ARRAYS_INDIRECT_EXT: u64 = 0x8A4D173EB99A8603;
const HASH_DRAW_INDEXED_INDIRECT: u64 = 0x771BB18C62444DA0;
const HASH_DRAW_INDEXED_INDIRECT_EXT: u64 = 0x0217920100488FF7;
const HASH_MULTI_DRAW_INDEXED_INDIRECT_COUNT: u64 = 0x3F5E74B9C9A50164;
const HASH_MULTI_LAYER_CLEAR: u64 = 0xEAD26C3E2109B06B;
const HASH_C713C83D8F63CCF3: u64 = 0xC713C83D8F63CCF3;
const HASH_D7333D26E0A93EDE: u64 = 0xD7333D26E0A93EDE;
const HASH_BIND_SHADER: u64 = 0xEB29B2A09AA06D38;
const HASH_SET_RASTER_BOUNDING_BOX: u64 = 0xDB1341DBEB4C8AF7;
const HASH_CLEAR_CONST_BUFFER_5F00: u64 = 0x6C97861D891EDF7E;
const HASH_CLEAR_CONST_BUFFER_7000: u64 = 0xD246FDDF3A6173D7;
const HASH_CLEAR_MEMORY: u64 = 0xEE4D0004BEC8ECF4;
const HASH_TRANSFORM_FEEDBACK_SETUP: u64 = 0xFC0CF27F5FFAA661;
const HASH_DRAW_INDIRECT_BYTE_COUNT: u64 = 0xB5F74EDB717278EC;

// ── HLE Macro Implementations ────────────────────────────────────────────────

/// Base for all HLE macro implementations.
///
/// Port of `HLEMacroImpl` from `macro_hle.cpp`.
/// In upstream, this holds a reference to Maxwell3D. That dependency
/// is carried via a raw pointer and set by `MacroEngine` before lookup.

#[derive(Clone, Copy)]
struct Maxwell3DPtr(*mut Maxwell3D);

unsafe impl Send for Maxwell3DPtr {}

impl Maxwell3DPtr {
    unsafe fn draw_arrays_indirect(self, extended: bool, parameters: &[u32]) {
        (&mut *self.0).hle_draw_arrays_indirect(extended, parameters);
    }

    unsafe fn draw_indexed_indirect(self, extended: bool, parameters: &[u32]) {
        (&mut *self.0).hle_draw_indexed_indirect(extended, parameters);
    }

    unsafe fn multi_draw_indexed_indirect_count(self, parameters: &[u32]) {
        (&mut *self.0).hle_multi_draw_indexed_indirect_count(parameters);
    }

    unsafe fn draw_indirect_byte_count(self, parameters: &[u32]) {
        (&mut *self.0).hle_draw_indirect_byte_count(parameters);
    }

    unsafe fn multi_layer_clear(self, parameters: &[u32]) {
        (&mut *self.0).hle_multi_layer_clear(parameters);
    }

    unsafe fn c713c83d8f63ccf3(self, parameters: &[u32]) {
        (&mut *self.0).hle_c713c83d8f63ccf3(parameters);
    }

    unsafe fn set_raster_bounding_box(self, parameters: &[u32]) {
        (&mut *self.0).hle_set_raster_bounding_box(parameters);
    }

    unsafe fn clear_const_buffer(self, base_size: usize, parameters: &[u32]) {
        (&mut *self.0).hle_clear_const_buffer(base_size, parameters);
    }

    unsafe fn clear_memory(self, parameters: &[u32], zero_memory: &mut Vec<u32>) {
        (&mut *self.0).hle_clear_memory(parameters, zero_memory);
    }

    unsafe fn transform_feedback_setup(self, parameters: &[u32]) {
        (&mut *self.0).hle_transform_feedback_setup(parameters);
    }

    unsafe fn d7333d26e0a93ede(self, parameters: &[u32]) {
        (&mut *self.0).hle_d7333d26e0a93ede(parameters);
    }

    unsafe fn bind_shader(self, parameters: &[u32]) {
        (&mut *self.0).hle_bind_shader(parameters);
    }
}

/// HLE: DrawArraysIndirect (non-extended).
///
/// Port of `HLE_DrawArraysIndirect<false>`.
struct HleDrawArraysIndirect {
    extended: bool,
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleDrawArraysIndirect {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!("HLE_DrawArraysIndirect: missing Maxwell3D owner");
            return;
        };
        unsafe { maxwell3d.draw_arrays_indirect(self.extended, parameters) };
    }
}

/// HLE: DrawIndexedIndirect.
///
/// Port of `HLE_DrawIndexedIndirect<extended>`.
struct HleDrawIndexedIndirect {
    extended: bool,
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleDrawIndexedIndirect {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!("HLE_DrawIndexedIndirect: missing Maxwell3D owner");
            return;
        };
        unsafe { maxwell3d.draw_indexed_indirect(self.extended, parameters) };
    }
}

/// HLE: MultiLayerClear.
///
/// Port of `HLE_MultiLayerClear`.
struct HleMultiLayerClear {
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleMultiLayerClear {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!("HLE_MultiLayerClear: missing Maxwell3D owner");
            return;
        };
        unsafe { maxwell3d.multi_layer_clear(parameters) };
    }
}

/// HLE: MultiDrawIndexedIndirectCount.
///
/// Port of `HLE_MultiDrawIndexedIndirectCount`.
struct HleMultiDrawIndexedIndirectCount {
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleMultiDrawIndexedIndirectCount {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!("HLE_MultiDrawIndexedIndirectCount: missing Maxwell3D owner");
            return;
        };
        unsafe { maxwell3d.multi_draw_indexed_indirect_count(parameters) };
    }
}

/// HLE: DrawIndirectByteCount.
///
/// Port of `HLE_DrawIndirectByteCount`.
struct HleDrawIndirectByteCount {
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleDrawIndirectByteCount {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!("HLE_DrawIndirectByteCount: missing Maxwell3D owner");
            return;
        };
        unsafe { maxwell3d.draw_indirect_byte_count(parameters) };
    }
}

/// HLE: C713C83D8F63CCF3 — const buffer setup.
///
/// Port of `HLE_C713C83D8F63CCF3`.
struct HleC713C83d8f63Ccf3 {
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleC713C83d8f63Ccf3 {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!("HLE_C713C83D8F63CCF3: missing Maxwell3D owner");
            return;
        };
        unsafe { maxwell3d.c713c83d8f63ccf3(parameters) };
    }
}

/// HLE: D7333D26E0A93EDE — const buffer address setup.
///
/// Port of `HLE_D7333D26E0A93EDE`.
struct HleD7333d26e0a93Ede {
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleD7333d26e0a93Ede {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!("HLE_D7333D26E0A93EDE: missing Maxwell3D owner");
            return;
        };
        unsafe { maxwell3d.d7333d26e0a93ede(parameters) };
    }
}

/// HLE: BindShader.
///
/// Port of `HLE_BindShader`.
struct HleBindShader {
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleBindShader {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!("HLE_BindShader: missing Maxwell3D owner");
            return;
        };
        unsafe { maxwell3d.bind_shader(parameters) };
    }
}

/// HLE: SetRasterBoundingBox.
///
/// Port of `HLE_SetRasterBoundingBox`.
struct HleSetRasterBoundingBox {
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleSetRasterBoundingBox {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!("HLE_SetRasterBoundingBox: missing Maxwell3D owner");
            return;
        };
        unsafe { maxwell3d.set_raster_bounding_box(parameters) };
    }
}

/// HLE: ClearConstBuffer.
///
/// Port of `HLE_ClearConstBuffer<base_size>`.
struct HleClearConstBuffer {
    base_size: usize,
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleClearConstBuffer {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!(
                "HLE_ClearConstBuffer(base_size=0x{:X}): missing Maxwell3D owner",
                self.base_size
            );
            return;
        };
        unsafe { maxwell3d.clear_const_buffer(self.base_size, parameters) };
    }
}

/// HLE: ClearMemory.
///
/// Port of `HLE_ClearMemory`.
struct HleClearMemory {
    zero_memory: Vec<u32>,
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleClearMemory {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!("HLE_ClearMemory: missing Maxwell3D owner");
            return;
        };
        unsafe { maxwell3d.clear_memory(parameters, &mut self.zero_memory) };
    }
}

/// HLE: TransformFeedbackSetup.
///
/// Port of `HLE_TransformFeedbackSetup`.
struct HleTransformFeedbackSetup {
    maxwell3d: Option<Maxwell3DPtr>,
}

impl CachedMacro for HleTransformFeedbackSetup {
    fn execute(&mut self, parameters: &[u32], _method: u32) {
        let Some(maxwell3d) = self.maxwell3d else {
            log::warn!("HLE_TransformFeedbackSetup: missing Maxwell3D owner");
            return;
        };
        unsafe { maxwell3d.transform_feedback_setup(parameters) };
    }
}

// ── HLE Macro Registry ──────────────────────────────────────────────────────

/// Builder function type for creating HLE macro instances.
type HleBuilder = fn(Option<Maxwell3DPtr>) -> Box<dyn CachedMacro>;

/// Registry of known HLE macro programs, keyed by hash.
///
/// Port of `Tegra::HLEMacro`.
pub struct HleMacro {
    builders: HashMap<u64, HleBuilder>,
    maxwell3d: Option<Maxwell3DPtr>,
}

impl HleMacro {
    /// Create a new HLE macro registry with all known program hashes.
    ///
    /// Port of `HLEMacro::HLEMacro(Maxwell3D&)`.
    pub fn new() -> Self {
        let mut builders: HashMap<u64, HleBuilder> = HashMap::new();

        builders.insert(HASH_DRAW_ARRAYS_INDIRECT, |maxwell3d| {
            Box::new(HleDrawArraysIndirect {
                extended: false,
                maxwell3d,
            })
        });
        builders.insert(HASH_DRAW_ARRAYS_INDIRECT_EXT, |maxwell3d| {
            Box::new(HleDrawArraysIndirect {
                extended: true,
                maxwell3d,
            })
        });
        builders.insert(HASH_DRAW_INDEXED_INDIRECT, |maxwell3d| {
            Box::new(HleDrawIndexedIndirect {
                extended: false,
                maxwell3d,
            })
        });
        builders.insert(HASH_DRAW_INDEXED_INDIRECT_EXT, |maxwell3d| {
            Box::new(HleDrawIndexedIndirect {
                extended: true,
                maxwell3d,
            })
        });
        builders.insert(HASH_MULTI_DRAW_INDEXED_INDIRECT_COUNT, |maxwell3d| {
            Box::new(HleMultiDrawIndexedIndirectCount { maxwell3d })
        });
        builders.insert(HASH_MULTI_LAYER_CLEAR, |maxwell3d| {
            Box::new(HleMultiLayerClear { maxwell3d })
        });
        builders.insert(HASH_C713C83D8F63CCF3, |maxwell3d| {
            Box::new(HleC713C83d8f63Ccf3 { maxwell3d })
        });
        builders.insert(HASH_D7333D26E0A93EDE, |maxwell3d| {
            Box::new(HleD7333d26e0a93Ede { maxwell3d })
        });
        builders.insert(HASH_BIND_SHADER, |maxwell3d| {
            Box::new(HleBindShader { maxwell3d })
        });
        builders.insert(HASH_SET_RASTER_BOUNDING_BOX, |maxwell3d| {
            Box::new(HleSetRasterBoundingBox { maxwell3d })
        });
        builders.insert(HASH_CLEAR_CONST_BUFFER_5F00, |maxwell3d| {
            Box::new(HleClearConstBuffer {
                base_size: 0x5F00,
                maxwell3d,
            })
        });
        builders.insert(HASH_CLEAR_CONST_BUFFER_7000, |maxwell3d| {
            Box::new(HleClearConstBuffer {
                base_size: 0x7000,
                maxwell3d,
            })
        });
        builders.insert(HASH_CLEAR_MEMORY, |maxwell3d| {
            Box::new(HleClearMemory {
                zero_memory: Vec::new(),
                maxwell3d,
            })
        });
        builders.insert(HASH_TRANSFORM_FEEDBACK_SETUP, |maxwell3d| {
            Box::new(HleTransformFeedbackSetup { maxwell3d })
        });
        builders.insert(HASH_DRAW_INDIRECT_BYTE_COUNT, |maxwell3d| {
            Box::new(HleDrawIndirectByteCount { maxwell3d })
        });

        Self {
            builders,
            maxwell3d: None,
        }
    }

    pub fn set_maxwell_3d(&mut self, maxwell3d: *mut Maxwell3D) {
        self.maxwell3d = Some(Maxwell3DPtr(maxwell3d));
    }

    /// Look up and instantiate an HLE program by its hash.
    ///
    /// Port of `HLEMacro::GetHLEProgram`.
    ///
    /// Returns `None` if the hash is not recognized.
    pub fn get_hle_program(&self, hash: u64) -> Option<Box<dyn CachedMacro>> {
        let hit = self.builders.contains_key(&hash);
        log::info!("HleMacro::get_hle_program hash=0x{:016X} hit={}", hash, hit);
        self.builders
            .get(&hash)
            .map(|builder| builder(self.maxwell3d))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hle_macro_registry_populated() {
        let hle = HleMacro::new();
        // Should have 15 known programs
        assert_eq!(hle.builders.len(), 15);
    }

    #[test]
    fn hle_macro_known_hash_found() {
        let hle = HleMacro::new();
        assert!(hle.get_hle_program(HASH_DRAW_ARRAYS_INDIRECT).is_some());
        assert!(hle.get_hle_program(HASH_BIND_SHADER).is_some());
    }

    #[test]
    fn hle_macro_unknown_hash_returns_none() {
        let hle = HleMacro::new();
        assert!(hle.get_hle_program(0xDEADBEEF).is_none());
    }
}
