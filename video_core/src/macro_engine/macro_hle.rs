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
    unsafe fn clear_const_buffer(self, base_size: usize, parameters: &[u32]) {
        (&mut *self.0).hle_clear_const_buffer(base_size, parameters);
    }

    unsafe fn clear_memory(self, parameters: &[u32], zero_memory: &mut Vec<u32>) {
        (&mut *self.0).hle_clear_memory(parameters, zero_memory);
    }
}

/// HLE: DrawArraysIndirect (non-extended).
///
/// Port of `HLE_DrawArraysIndirect<false>`.
struct HleDrawArraysIndirect {
    extended: bool,
}

impl CachedMacro for HleDrawArraysIndirect {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        // Stubbed — requires Maxwell3D reference to call draw_manager->DrawArrayIndirect()
        // and set indirect draw parameters.
        // Upstream: HLE_DrawArraysIndirect<extended>::Execute() in video_core/macro/macro_hle.cpp
        log::warn!(
            "HLE_DrawArraysIndirect(extended={}): not yet implemented (requires Maxwell3D integration)",
            self.extended
        );
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
        // Stubbed — requires Maxwell3D reference to call draw_manager->DrawIndexedIndirect()
        // and set element_base/base_instance registers.
        // Upstream: HLE_DrawIndexedIndirect<extended>::Execute() in video_core/macro/macro_hle.cpp
        log::warn!(
            "HLE_DrawIndexedIndirect(extended={}): not yet implemented (requires Maxwell3D integration)",
            self.extended
        );
    }
}

/// HLE: MultiLayerClear.
///
/// Port of `HLE_MultiLayerClear`.
struct HleMultiLayerClear;

impl CachedMacro for HleMultiLayerClear {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        // Stubbed — requires Maxwell3D reference to decode ClearSurface params and
        // call draw_manager->Clear(num_layers).
        // Upstream: HLE_MultiLayerClear::Execute() in video_core/macro/macro_hle.cpp
        log::warn!("HLE_MultiLayerClear: not yet implemented (requires Maxwell3D integration)");
    }
}

/// HLE: MultiDrawIndexedIndirectCount.
///
/// Port of `HLE_MultiDrawIndexedIndirectCount`.
struct HleMultiDrawIndexedIndirectCount;

impl CachedMacro for HleMultiDrawIndexedIndirectCount {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        // Stubbed — requires Maxwell3D reference to set indirect draw parameters and call
        // draw_manager->DrawIndexedIndirect() with count/stride from parameters.
        // Upstream: HLE_MultiDrawIndexedIndirectCount::Execute() in video_core/macro/macro_hle.cpp
        log::warn!("HLE_MultiDrawIndexedIndirectCount: not yet implemented (requires Maxwell3D integration)");
    }
}

/// HLE: DrawIndirectByteCount.
///
/// Port of `HLE_DrawIndirectByteCount`.
struct HleDrawIndirectByteCount;

impl CachedMacro for HleDrawIndirectByteCount {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        // Stubbed — requires Maxwell3D reference to set draw_auto_stride/byte_count registers
        // and call draw_manager->DrawArrayIndirect(topology) with is_byte_count=true.
        // Upstream: HLE_DrawIndirectByteCount::Execute() in video_core/macro/macro_hle.cpp
        log::warn!(
            "HLE_DrawIndirectByteCount: not yet implemented (requires Maxwell3D integration)"
        );
    }
}

/// HLE: C713C83D8F63CCF3 — const buffer setup.
///
/// Port of `HLE_C713C83D8F63CCF3`.
struct HleC713C83d8f63Ccf3;

impl CachedMacro for HleC713C83d8f63Ccf3 {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        // Stubbed — requires Maxwell3D register access to read shadow_scratch[24] and
        // write const_buffer.{size, address_high, address_low, offset}.
        // Upstream: HLE_C713C83D8F63CCF3::Execute() in video_core/macro/macro_hle.cpp:
        //   offset = (params[0] & 0x3FFFFFFF) << 2
        //   address = regs.shadow_scratch[24]
        //   const_buffer.size = 0x7000
        //   const_buffer.address_high = (address >> 24) & 0xFF
        //   const_buffer.address_low = address << 8
        //   const_buffer.offset = offset
        log::warn!("HLE_C713C83D8F63CCF3: not yet implemented (requires Maxwell3D integration)");
    }
}

/// HLE: D7333D26E0A93EDE — const buffer address setup.
///
/// Port of `HLE_D7333D26E0A93EDE`.
struct HleD7333d26e0a93Ede;

impl CachedMacro for HleD7333d26e0a93Ede {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        // Stubbed — requires Maxwell3D register access to read shadow_scratch[42+index] and
        // shadow_scratch[47+index] and write const_buffer.{size, address_high, address_low}.
        // Upstream: HLE_D7333D26E0A93EDE::Execute() in video_core/macro/macro_hle.cpp:
        //   index = params[0]
        //   address = regs.shadow_scratch[42 + index]
        //   size = regs.shadow_scratch[47 + index]
        //   const_buffer.size = size
        //   const_buffer.address_high = (address >> 24) & 0xFF
        //   const_buffer.address_low = address << 8
        log::warn!("HLE_D7333D26E0A93EDE: not yet implemented (requires Maxwell3D integration)");
    }
}

/// HLE: BindShader.
///
/// Port of `HLE_BindShader`.
struct HleBindShader;

impl CachedMacro for HleBindShader {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        // Stubbed — requires Maxwell3D register access to update pipelines[index].offset,
        // shadow_scratch entries, const_buffer address fields, and call ProcessCBBind.
        // Upstream: HLE_BindShader::Execute() in video_core/macro/macro_hle.cpp:
        //   index = params[0]; if params[1] == shadow_scratch[28+index] => early return
        //   regs.pipelines[index & 0xF].offset = params[2]
        //   dirty.flags[Shaders] = true
        //   shadow_scratch[28+index] = params[1]; shadow_scratch[34+index] = params[2]
        //   address = params[4]
        //   const_buffer.{size=0x10000, address_high, address_low, offset=0}
        //   bind_group[params[3] & 0x7F].raw_config = 0x11; ProcessCBBind(bind_group_id)
        log::warn!("HLE_BindShader: not yet implemented (requires Maxwell3D integration)");
    }
}

/// HLE: SetRasterBoundingBox.
///
/// Port of `HLE_SetRasterBoundingBox`.
struct HleSetRasterBoundingBox;

impl CachedMacro for HleSetRasterBoundingBox {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        // Stubbed — requires Maxwell3D register access to read conservative_raster_enable
        // and shadow_scratch[52], then write raster_bounding_box.
        // Upstream: HLE_SetRasterBoundingBox::Execute() in video_core/macro/macro_hle.cpp:
        //   raster_mode = params[0]
        //   raster_enabled = regs.conservative_raster_enable
        //   scratch_data = regs.shadow_scratch[52]
        //   regs.raster_bounding_box.raw = raster_mode & 0xFFFFF00F
        //   regs.raster_bounding_box.pad = scratch_data & raster_enabled
        log::warn!(
            "HLE_SetRasterBoundingBox: not yet implemented (requires Maxwell3D integration)"
        );
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
struct HleTransformFeedbackSetup;

impl CachedMacro for HleTransformFeedbackSetup {
    fn execute(&mut self, _parameters: &[u32], _method: u32) {
        // Stubbed — requires Maxwell3D register access to set transform_feedback_enabled,
        // clear buffer start_offsets, set upload dest address, call CallMethod(launch_dma)
        // and CallMethod(inline_data) with the TF stride, then RegisterTransformFeedback.
        // Upstream: HLE_TransformFeedbackSetup::Execute() in video_core/macro/macro_hle.cpp:
        //   regs.transform_feedback_enabled = 1
        //   regs.transform_feedback.buffers[0..3].start_offset = 0
        //   regs.upload.{line_length_in=4, line_count=1, dest.{address_high=params[0], address_low=params[1]}}
        //   CallMethod(launch_dma, 0x1011, true)
        //   CallMethod(inline_data, tf.controls[0].stride, true)
        //   Rasterizer().RegisterTransformFeedback(upload.dest.Address())
        log::warn!(
            "HLE_TransformFeedbackSetup: not yet implemented (requires Maxwell3D integration)"
        );
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

        builders.insert(HASH_DRAW_ARRAYS_INDIRECT, |_| {
            Box::new(HleDrawArraysIndirect { extended: false })
        });
        builders.insert(HASH_DRAW_ARRAYS_INDIRECT_EXT, |_| {
            Box::new(HleDrawArraysIndirect { extended: true })
        });
        builders.insert(HASH_DRAW_INDEXED_INDIRECT, |_| {
            Box::new(HleDrawIndexedIndirect { extended: false })
        });
        builders.insert(HASH_DRAW_INDEXED_INDIRECT_EXT, |_| {
            Box::new(HleDrawIndexedIndirect { extended: true })
        });
        builders.insert(HASH_MULTI_DRAW_INDEXED_INDIRECT_COUNT, |_| {
            Box::new(HleMultiDrawIndexedIndirectCount)
        });
        builders.insert(HASH_MULTI_LAYER_CLEAR, |_| Box::new(HleMultiLayerClear));
        builders.insert(HASH_C713C83D8F63CCF3, |_| Box::new(HleC713C83d8f63Ccf3));
        builders.insert(HASH_D7333D26E0A93EDE, |_| Box::new(HleD7333d26e0a93Ede));
        builders.insert(HASH_BIND_SHADER, |_| Box::new(HleBindShader));
        builders.insert(HASH_SET_RASTER_BOUNDING_BOX, |_| {
            Box::new(HleSetRasterBoundingBox)
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
        builders.insert(HASH_TRANSFORM_FEEDBACK_SETUP, |_| {
            Box::new(HleTransformFeedbackSetup)
        });
        builders.insert(HASH_DRAW_INDIRECT_BYTE_COUNT, |_| {
            Box::new(HleDrawIndirectByteCount)
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
