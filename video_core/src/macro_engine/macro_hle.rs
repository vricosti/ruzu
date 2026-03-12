// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/macro/macro_hle.h` and `macro_hle.cpp`.
//!
//! High-Level Emulation (HLE) of known macro programs. When a macro's hash
//! matches a known program, the HLE implementation is used instead of
//! interpreting/JIT-compiling the macro code, providing significant speedup.

use std::collections::HashMap;

use super::macro_engine::CachedMacro;

// ── Known HLE program hashes ─────────────────────────────────────────────────

// These are the CityHash64 values of known upstream macro programs.
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
/// will be wired when the engine integration is complete.

/// HLE: DrawArraysIndirect (non-extended).
///
/// Port of `HLE_DrawArraysIndirect<false>`.
struct HleDrawArraysIndirect {
    extended: bool,
}

impl CachedMacro for HleDrawArraysIndirect {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        // TODO: Wire to Maxwell3D draw_manager when engines are integrated
        todo!("HLE_DrawArraysIndirect::Execute")
    }
}

/// HLE: DrawIndexedIndirect.
///
/// Port of `HLE_DrawIndexedIndirect<extended>`.
struct HleDrawIndexedIndirect {
    extended: bool,
}

impl CachedMacro for HleDrawIndexedIndirect {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        todo!("HLE_DrawIndexedIndirect::Execute")
    }
}

/// HLE: MultiLayerClear.
///
/// Port of `HLE_MultiLayerClear`.
struct HleMultiLayerClear;

impl CachedMacro for HleMultiLayerClear {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        todo!("HLE_MultiLayerClear::Execute")
    }
}

/// HLE: MultiDrawIndexedIndirectCount.
///
/// Port of `HLE_MultiDrawIndexedIndirectCount`.
struct HleMultiDrawIndexedIndirectCount;

impl CachedMacro for HleMultiDrawIndexedIndirectCount {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        todo!("HLE_MultiDrawIndexedIndirectCount::Execute")
    }
}

/// HLE: DrawIndirectByteCount.
///
/// Port of `HLE_DrawIndirectByteCount`.
struct HleDrawIndirectByteCount;

impl CachedMacro for HleDrawIndirectByteCount {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        todo!("HLE_DrawIndirectByteCount::Execute")
    }
}

/// HLE: C713C83D8F63CCF3 — const buffer setup.
///
/// Port of `HLE_C713C83D8F63CCF3`.
struct HleC713C83d8f63Ccf3;

impl CachedMacro for HleC713C83d8f63Ccf3 {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        todo!("HLE_C713C83D8F63CCF3::Execute")
    }
}

/// HLE: D7333D26E0A93EDE — const buffer address setup.
///
/// Port of `HLE_D7333D26E0A93EDE`.
struct HleD7333d26e0a93Ede;

impl CachedMacro for HleD7333d26e0a93Ede {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        todo!("HLE_D7333D26E0A93EDE::Execute")
    }
}

/// HLE: BindShader.
///
/// Port of `HLE_BindShader`.
struct HleBindShader;

impl CachedMacro for HleBindShader {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        todo!("HLE_BindShader::Execute")
    }
}

/// HLE: SetRasterBoundingBox.
///
/// Port of `HLE_SetRasterBoundingBox`.
struct HleSetRasterBoundingBox;

impl CachedMacro for HleSetRasterBoundingBox {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        todo!("HLE_SetRasterBoundingBox::Execute")
    }
}

/// HLE: ClearConstBuffer.
///
/// Port of `HLE_ClearConstBuffer<base_size>`.
struct HleClearConstBuffer {
    base_size: usize,
}

impl CachedMacro for HleClearConstBuffer {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        todo!("HLE_ClearConstBuffer::Execute")
    }
}

/// HLE: ClearMemory.
///
/// Port of `HLE_ClearMemory`.
struct HleClearMemory {
    #[allow(dead_code)]
    zero_memory: Vec<u32>,
}

impl CachedMacro for HleClearMemory {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        todo!("HLE_ClearMemory::Execute")
    }
}

/// HLE: TransformFeedbackSetup.
///
/// Port of `HLE_TransformFeedbackSetup`.
struct HleTransformFeedbackSetup;

impl CachedMacro for HleTransformFeedbackSetup {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        todo!("HLE_TransformFeedbackSetup::Execute")
    }
}

// ── HLE Macro Registry ──────────────────────────────────────────────────────

/// Builder function type for creating HLE macro instances.
type HleBuilder = fn() -> Box<dyn CachedMacro>;

/// Registry of known HLE macro programs, keyed by hash.
///
/// Port of `Tegra::HLEMacro`.
pub struct HleMacro {
    builders: HashMap<u64, HleBuilder>,
}

impl HleMacro {
    /// Create a new HLE macro registry with all known program hashes.
    ///
    /// Port of `HLEMacro::HLEMacro(Maxwell3D&)`.
    pub fn new() -> Self {
        let mut builders: HashMap<u64, HleBuilder> = HashMap::new();

        builders.insert(HASH_DRAW_ARRAYS_INDIRECT, || {
            Box::new(HleDrawArraysIndirect { extended: false })
        });
        builders.insert(HASH_DRAW_ARRAYS_INDIRECT_EXT, || {
            Box::new(HleDrawArraysIndirect { extended: true })
        });
        builders.insert(HASH_DRAW_INDEXED_INDIRECT, || {
            Box::new(HleDrawIndexedIndirect { extended: false })
        });
        builders.insert(HASH_DRAW_INDEXED_INDIRECT_EXT, || {
            Box::new(HleDrawIndexedIndirect { extended: true })
        });
        builders.insert(HASH_MULTI_DRAW_INDEXED_INDIRECT_COUNT, || {
            Box::new(HleMultiDrawIndexedIndirectCount)
        });
        builders.insert(HASH_MULTI_LAYER_CLEAR, || Box::new(HleMultiLayerClear));
        builders.insert(HASH_C713C83D8F63CCF3, || Box::new(HleC713C83d8f63Ccf3));
        builders.insert(HASH_D7333D26E0A93EDE, || Box::new(HleD7333d26e0a93Ede));
        builders.insert(HASH_BIND_SHADER, || Box::new(HleBindShader));
        builders.insert(HASH_SET_RASTER_BOUNDING_BOX, || {
            Box::new(HleSetRasterBoundingBox)
        });
        builders.insert(HASH_CLEAR_CONST_BUFFER_5F00, || {
            Box::new(HleClearConstBuffer { base_size: 0x5F00 })
        });
        builders.insert(HASH_CLEAR_CONST_BUFFER_7000, || {
            Box::new(HleClearConstBuffer { base_size: 0x7000 })
        });
        builders.insert(HASH_CLEAR_MEMORY, || {
            Box::new(HleClearMemory {
                zero_memory: Vec::new(),
            })
        });
        builders.insert(HASH_TRANSFORM_FEEDBACK_SETUP, || {
            Box::new(HleTransformFeedbackSetup)
        });
        builders.insert(HASH_DRAW_INDIRECT_BYTE_COUNT, || {
            Box::new(HleDrawIndirectByteCount)
        });

        Self { builders }
    }

    /// Look up and instantiate an HLE program by its hash.
    ///
    /// Port of `HLEMacro::GetHLEProgram`.
    ///
    /// Returns `None` if the hash is not recognized.
    pub fn get_hle_program(&self, hash: u64) -> Option<Box<dyn CachedMacro>> {
        self.builders.get(&hash).map(|builder| builder())
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
