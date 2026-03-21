// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/engines/draw_manager.h and draw_manager.cpp
//!
//! The DrawManager handles draw call dispatch for the Maxwell 3D engine.
//! It processes method writes for begin/end draws, inline index buffers,
//! instanced draws, and draw textures.

use crate::dirty_flags::flags as Dirty;
use crate::rasterizer_interface::RasterizerInterface;

/// GPU virtual address type.
pub type GPUVAddr = u64;

// ── Type aliases matching upstream using declarations ────────────────────────

// In upstream these are `using PrimitiveTopologyControl = Maxwell3D::Regs::...`.
// We use re-exports / newtypes. For now, define the needed types locally to
// preserve file-level self-containment, matching const_buffer_info.h pattern.

/// Primitive topology control mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum PrimitiveTopologyControl {
    #[default]
    UseInBeginMethods = 0,
    UseSeparateState = 1,
}

/// Primitive topology (draw mode).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum PrimitiveTopology {
    #[default]
    Points = 0x0001,
    Lines = 0x0002,
    LineLoop = 0x0003,
    LineStrip = 0x0004,
    Triangles = 0x0005,
    TriangleStrip = 0x0006,
    TriangleFan = 0x0007,
    Quads = 0x0008,
    QuadStrip = 0x0009,
    Polygon = 0x000A,
    LinesAdjacency = 0x000B,
    LineStripAdjacency = 0x000C,
    TrianglesAdjacency = 0x000D,
    TriangleStripAdjacency = 0x000E,
    Patches = 0x000F,
}

impl PrimitiveTopology {
    /// Convert a raw u32 value to a PrimitiveTopology.
    /// Unknown values default to Points.
    pub fn from_raw(value: u32) -> Self {
        match value {
            0x0001 => Self::Points,
            0x0002 => Self::Lines,
            0x0003 => Self::LineLoop,
            0x0004 => Self::LineStrip,
            0x0005 => Self::Triangles,
            0x0006 => Self::TriangleStrip,
            0x0007 => Self::TriangleFan,
            0x0008 => Self::Quads,
            0x0009 => Self::QuadStrip,
            0x000A => Self::Polygon,
            0x000B => Self::LinesAdjacency,
            0x000C => Self::LineStripAdjacency,
            0x000D => Self::TrianglesAdjacency,
            0x000E => Self::TriangleStripAdjacency,
            0x000F => Self::Patches,
            _ => Self::Points,
        }
    }
}

/// Topology override values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum PrimitiveTopologyOverride {
    #[default]
    None = 0,
    Points = 1,
    Lines = 2,
    LineStrip = 3,
}

/// Index buffer format.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum IndexFormat {
    #[default]
    UnsignedByte = 0,
    UnsignedShort = 1,
    UnsignedInt = 2,
}

/// Vertex buffer register state.
#[derive(Debug, Clone, Copy, Default)]
pub struct VertexBuffer {
    pub first: u32,
    pub count: u32,
}

/// Index buffer register state.
#[derive(Debug, Clone, Copy, Default)]
pub struct IndexBuffer {
    pub first: u32,
    pub count: u32,
    pub format: IndexFormat,
}

/// Small (packed) index buffer parameters, decoded from a single u32.
///
/// Upstream `Maxwell3D::Regs::IndexBufferSmall`:
///   bits [0:15]  = first
///   bits [16:27] = count
///   bits [28:31] = topology (PrimitiveTopology)
#[derive(Debug, Clone, Copy)]
pub struct IndexBufferSmall {
    pub first: u32,
    pub count: u32,
    pub topology: PrimitiveTopology,
}

impl IndexBufferSmall {
    /// Decode from a raw u32 argument (upstream `IndexBufferSmall{argument}`).
    ///
    /// Bit layout from upstream maxwell_3d.h:
    ///   BitField<0, 16, u32> first
    ///   BitField<16, 12, u32> count
    ///   BitField<28, 4, PrimitiveTopology> topology
    pub fn from_raw(argument: u32) -> Self {
        Self {
            first: argument & 0xFFFF,
            count: (argument >> 16) & 0xFFF,
            topology: PrimitiveTopology::from_raw((argument >> 28) & 0xF),
        }
    }
}

// ── Maxwell3D register access trait ─────────────────────────────────────────

/// Trait providing access to Maxwell3D state needed by DrawManager.
///
/// Upstream DrawManager holds a raw `Maxwell3D*` pointer and accesses
/// `maxwell3d->regs`, `maxwell3d->dirty.flags`, `maxwell3d->rasterizer`,
/// and `maxwell3d->ShouldExecute()`. In Rust, we use a trait to avoid
/// circular dependencies.
pub trait Maxwell3DAccess {
    /// Whether conditional rendering allows execution.
    /// Upstream: `maxwell3d->ShouldExecute()`.
    fn should_execute(&self) -> bool;

    /// Read the global_base_instance_index register.
    fn global_base_instance_index(&self) -> u32;

    /// Read the global_base_vertex_index register.
    fn global_base_vertex_index(&self) -> u32;

    /// Read the index_buffer register state.
    fn index_buffer(&self) -> IndexBuffer;

    /// Read the vertex_buffer register state.
    fn vertex_buffer(&self) -> VertexBuffer;

    /// Read the primitive_topology_control register.
    fn primitive_topology_control(&self) -> PrimitiveTopologyControl;

    /// Read the topology_override register.
    fn topology_override(&self) -> PrimitiveTopologyOverride;

    /// Read the raw topology_override value for the default match arm.
    fn topology_override_raw(&self) -> u32;

    /// Read the draw.topology register (for DrawBegin).
    fn draw_topology(&self) -> PrimitiveTopology;

    /// Read the draw.instance_id register: returns (is_first, is_subsequent).
    fn draw_instance_id(&self) -> (bool, bool);

    /// Set a dirty flag. Upstream: `maxwell3d->dirty.flags[index] = true`.
    fn set_dirty_flag(&mut self, index: u8);

    /// Get a mutable reference to the rasterizer, if bound.
    fn rasterizer_mut(&mut self) -> Option<&mut dyn RasterizerInterface>;
}

// ── DrawManager ─────────────────────────────────────────────────────────────

/// Draw mode for the current draw state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum DrawMode {
    #[default]
    General = 0,
    Instance = 1,
    InlineIndex = 2,
}

/// Core draw state tracked across begin/end sequences.
#[derive(Debug, Clone, Default)]
pub struct DrawState {
    pub topology: PrimitiveTopology,
    pub draw_mode: DrawMode,
    pub draw_indexed: bool,
    pub base_index: u32,
    pub vertex_buffer: VertexBuffer,
    pub index_buffer: IndexBuffer,
    pub base_instance: u32,
    pub instance_count: u32,
    pub inline_index_draw_indexes: Vec<u8>,
}

/// State for draw-texture operations.
#[derive(Debug, Clone, Copy, Default)]
pub struct DrawTextureState {
    pub dst_x0: f32,
    pub dst_y0: f32,
    pub dst_x1: f32,
    pub dst_y1: f32,
    pub src_x0: f32,
    pub src_y0: f32,
    pub src_x1: f32,
    pub src_y1: f32,
    pub src_sampler: u32,
    pub src_texture: u32,
}

/// Parameters for indirect draw calls.
#[derive(Debug, Clone, Copy, Default)]
pub struct IndirectParams {
    pub is_byte_count: bool,
    pub is_indexed: bool,
    pub include_count: bool,
    pub count_start_address: GPUVAddr,
    pub indirect_start_address: GPUVAddr,
    pub buffer_size: usize,
    pub max_draw_counts: usize,
    pub stride: usize,
}

/// Manages draw call processing for the Maxwell 3D engine.
///
/// Corresponds to the C++ `DrawManager` class. In the C++ code this holds a
/// raw pointer to `Maxwell3D`; in Rust we use the `Maxwell3DAccess` trait
/// for the methods that need register/rasterizer access, while keeping
/// draw state here.
pub struct DrawManager {
    pub draw_state: DrawState,
    pub draw_texture_state: DrawTextureState,
    pub indirect_state: IndirectParams,
}

impl DrawManager {
    /// Create a new DrawManager.
    pub fn new() -> Self {
        Self {
            draw_state: DrawState::default(),
            draw_texture_state: DrawTextureState::default(),
            indirect_state: IndirectParams::default(),
        }
    }

    /// Get a reference to the current draw state.
    pub fn get_draw_state(&self) -> &DrawState {
        &self.draw_state
    }

    /// Get a reference to the draw texture state.
    pub fn get_draw_texture_state(&self) -> &DrawTextureState {
        &self.draw_texture_state
    }

    /// Get a mutable reference to indirect draw parameters.
    pub fn get_indirect_params_mut(&mut self) -> &mut IndirectParams {
        &mut self.indirect_state
    }

    /// Get an immutable reference to indirect draw parameters.
    pub fn get_indirect_params(&self) -> &IndirectParams {
        &self.indirect_state
    }

    /// Process a method call that may trigger draw operations.
    ///
    /// Corresponds to `DrawManager::ProcessMethodCall`.
    /// Stubbed — full implementation requires access to Maxwell3D registers to decode
    /// MAXWELL3D_REG_INDEX values (clear_surface, draw.begin/end, index_buffer32_*, etc.)
    /// Upstream: DrawManager::ProcessMethodCall() in video_core/engines/draw_manager.cpp
    pub fn process_method_call(&mut self, _method: u32, _argument: u32) {
        log::warn!("DrawManager::process_method_call: not yet implemented (requires Maxwell3D register index constants)");
    }

    /// Execute a clear operation.
    ///
    /// Corresponds to `DrawManager::Clear`.
    /// Upstream: `maxwell3d->rasterizer->Clear(layer_count)` if ShouldExecute.
    pub fn clear(&mut self, layer_count: u32, maxwell3d: &mut dyn Maxwell3DAccess) {
        if maxwell3d.should_execute() {
            if let Some(rasterizer) = maxwell3d.rasterizer_mut() {
                rasterizer.clear(layer_count);
            }
        }
    }

    /// Flush any deferred instanced draw calls.
    ///
    /// Corresponds to `DrawManager::DrawDeferred`.
    pub fn draw_deferred(&mut self, maxwell3d: &mut dyn Maxwell3DAccess) {
        if self.draw_state.draw_mode != DrawMode::Instance
            || self.draw_state.instance_count == 0
        {
            return;
        }
        let instance_count = self.draw_state.instance_count + 1;
        self.draw_end(instance_count, true, maxwell3d);
        self.draw_state.instance_count = 0;
    }

    /// Issue a non-indexed draw call.
    ///
    /// Corresponds to `DrawManager::DrawArray`.
    pub fn draw_array(
        &mut self,
        topology: PrimitiveTopology,
        vertex_first: u32,
        vertex_count: u32,
        base_instance: u32,
        num_instances: u32,
        maxwell3d: &mut dyn Maxwell3DAccess,
    ) {
        self.draw_state.topology = topology;
        self.draw_state.vertex_buffer.first = vertex_first;
        self.draw_state.vertex_buffer.count = vertex_count;
        self.draw_state.base_instance = base_instance;
        self.process_draw(false, num_instances, maxwell3d);
    }

    /// Issue an instanced non-indexed draw call.
    ///
    /// Corresponds to `DrawManager::DrawArrayInstanced`.
    pub fn draw_array_instanced(
        &mut self,
        topology: PrimitiveTopology,
        vertex_first: u32,
        vertex_count: u32,
        subsequent: bool,
        maxwell3d: &mut dyn Maxwell3DAccess,
    ) {
        self.draw_state.topology = topology;
        self.draw_state.vertex_buffer.first = vertex_first;
        self.draw_state.vertex_buffer.count = vertex_count;

        if !subsequent {
            self.draw_state.instance_count = 1;
        }

        self.draw_state.base_instance = self.draw_state.instance_count - 1;
        self.draw_state.draw_mode = DrawMode::Instance;
        self.draw_state.instance_count += 1;
        self.process_draw(false, 1, maxwell3d);
    }

    /// Issue an indexed draw call.
    ///
    /// Corresponds to `DrawManager::DrawIndex`.
    pub fn draw_index(
        &mut self,
        topology: PrimitiveTopology,
        index_first: u32,
        index_count: u32,
        base_index: u32,
        base_instance: u32,
        num_instances: u32,
        maxwell3d: &mut dyn Maxwell3DAccess,
    ) {
        self.draw_state.topology = topology;
        // Upstream: draw_state.index_buffer = regs.index_buffer;
        self.draw_state.index_buffer = maxwell3d.index_buffer();
        self.draw_state.index_buffer.first = index_first;
        self.draw_state.index_buffer.count = index_count;
        self.draw_state.base_index = base_index;
        self.draw_state.base_instance = base_instance;
        self.process_draw(true, num_instances, maxwell3d);
    }

    /// Issue an indirect non-indexed draw call.
    ///
    /// Corresponds to `DrawManager::DrawArrayIndirect`.
    pub fn draw_array_indirect(
        &mut self,
        topology: PrimitiveTopology,
        maxwell3d: &mut dyn Maxwell3DAccess,
    ) {
        self.draw_state.topology = topology;
        self.process_draw_indirect(maxwell3d);
    }

    /// Issue an indirect indexed draw call.
    ///
    /// Corresponds to `DrawManager::DrawIndexedIndirect`.
    pub fn draw_indexed_indirect(
        &mut self,
        topology: PrimitiveTopology,
        index_first: u32,
        index_count: u32,
        maxwell3d: &mut dyn Maxwell3DAccess,
    ) {
        self.draw_state.topology = topology;
        // Upstream: draw_state.index_buffer = regs.index_buffer;
        self.draw_state.index_buffer = maxwell3d.index_buffer();
        self.draw_state.index_buffer.first = index_first;
        self.draw_state.index_buffer.count = index_count;
        self.process_draw_indirect(maxwell3d);
    }

    // ── Private helpers ─────────────────────────────────────────────────

    /// Push 4 bytes (one u32 in LE) into the inline index buffer.
    ///
    /// Corresponds to `DrawManager::SetInlineIndexBuffer`.
    pub fn set_inline_index_buffer(&mut self, index: u32) {
        self.draw_state
            .inline_index_draw_indexes
            .push((index & 0x000000FF) as u8);
        self.draw_state
            .inline_index_draw_indexes
            .push(((index & 0x0000FF00) >> 8) as u8);
        self.draw_state
            .inline_index_draw_indexes
            .push(((index & 0x00FF0000) >> 16) as u8);
        self.draw_state
            .inline_index_draw_indexes
            .push(((index & 0xFF000000) >> 24) as u8);
        self.draw_state.draw_mode = DrawMode::InlineIndex;
    }

    /// Handle draw-begin register write.
    ///
    /// Corresponds to `DrawManager::DrawBegin`.
    /// Upstream reads `regs.draw.instance_id` and `regs.draw.topology` from Maxwell3D.
    pub fn draw_begin(&mut self, maxwell3d: &mut dyn Maxwell3DAccess) {
        let (is_first, is_subsequent) = maxwell3d.draw_instance_id();
        if is_first {
            self.draw_deferred(maxwell3d);
            self.draw_state.instance_count = 0;
            self.draw_state.draw_mode = DrawMode::General;
        } else if is_subsequent {
            self.draw_state.instance_count += 1;
            self.draw_state.draw_mode = DrawMode::Instance;
        }

        self.draw_state.topology = maxwell3d.draw_topology();
    }

    /// Handle draw-end, flushing the current draw call.
    ///
    /// Corresponds to `DrawManager::DrawEnd`.
    /// Upstream reads `regs.global_base_instance_index`, `regs.global_base_vertex_index`,
    /// `regs.index_buffer`, `regs.vertex_buffer` from Maxwell3D.
    fn draw_end(
        &mut self,
        instance_count: u32,
        force_draw: bool,
        maxwell3d: &mut dyn Maxwell3DAccess,
    ) {
        match self.draw_state.draw_mode {
            DrawMode::Instance => {
                if !force_draw {
                    return;
                }
                // fallthrough to General (matching upstream [[fallthrough]])
                self.draw_state.base_instance = maxwell3d.global_base_instance_index();
                self.draw_state.base_index = maxwell3d.global_base_vertex_index();
                if self.draw_state.draw_indexed {
                    self.draw_state.index_buffer = maxwell3d.index_buffer();
                    self.process_draw(true, instance_count, maxwell3d);
                } else {
                    self.draw_state.vertex_buffer = maxwell3d.vertex_buffer();
                    self.process_draw(false, instance_count, maxwell3d);
                }
                self.draw_state.draw_indexed = false;
            }
            DrawMode::General => {
                self.draw_state.base_instance = maxwell3d.global_base_instance_index();
                self.draw_state.base_index = maxwell3d.global_base_vertex_index();
                if self.draw_state.draw_indexed {
                    self.draw_state.index_buffer = maxwell3d.index_buffer();
                    self.process_draw(true, instance_count, maxwell3d);
                } else {
                    self.draw_state.vertex_buffer = maxwell3d.vertex_buffer();
                    self.process_draw(false, instance_count, maxwell3d);
                }
                self.draw_state.draw_indexed = false;
            }
            DrawMode::InlineIndex => {
                self.draw_state.base_instance = maxwell3d.global_base_instance_index();
                self.draw_state.base_index = maxwell3d.global_base_vertex_index();
                self.draw_state.index_buffer = maxwell3d.index_buffer();
                self.draw_state.index_buffer.count =
                    (self.draw_state.inline_index_draw_indexes.len() / 4) as u32;
                self.draw_state.index_buffer.format = IndexFormat::UnsignedInt;
                // Upstream: maxwell3d->dirty.flags[VideoCommon::Dirty::IndexBuffer] = true;
                maxwell3d.set_dirty_flag(Dirty::INDEX_BUFFER);
                self.process_draw(true, instance_count, maxwell3d);
                self.draw_state.inline_index_draw_indexes.clear();
            }
        }
    }

    /// Handle small (packed) index draw.
    ///
    /// Corresponds to `DrawManager::DrawIndexSmall`.
    pub fn draw_index_small(&mut self, argument: u32, maxwell3d: &mut dyn Maxwell3DAccess) {
        let index_small = IndexBufferSmall::from_raw(argument);
        self.draw_state.base_instance = maxwell3d.global_base_instance_index();
        self.draw_state.base_index = maxwell3d.global_base_vertex_index();
        self.draw_state.index_buffer = maxwell3d.index_buffer();
        self.draw_state.index_buffer.first = index_small.first;
        self.draw_state.index_buffer.count = index_small.count;
        self.draw_state.topology = index_small.topology;
        // Upstream: maxwell3d->dirty.flags[VideoCommon::Dirty::IndexBuffer] = true;
        maxwell3d.set_dirty_flag(Dirty::INDEX_BUFFER);
        self.process_draw(true, 1, maxwell3d);
    }

    /// Handle draw-texture trigger.
    ///
    /// Corresponds to `DrawManager::DrawTexture`.
    /// Stubbed — requires access to Maxwell3D draw_texture and surface_clip registers to
    /// compute dst/src coordinates, then calls rasterizer->DrawTexture().
    /// Upstream: DrawManager::DrawTexture() in video_core/engines/draw_manager.cpp
    pub fn draw_texture(&mut self) {
        // Full implementation requires reading regs.draw_texture (dst_x0, dst_y0, dst_width,
        // dst_height, src_x0, src_y0, dx_du, dy_dv, src_sampler, src_texture),
        // regs.window_origin.mode, and regs.surface_clip.height from Maxwell3D.
        // These register types are not yet defined in the Maxwell3DAccess trait.
        // Once they are, this should match upstream DrawTexture() exactly.
        log::warn!("DrawManager::draw_texture: not yet implemented (requires draw_texture register types in Maxwell3DAccess)");
    }

    /// Update topology based on topology control mode and override.
    ///
    /// Corresponds to `DrawManager::UpdateTopology`.
    /// Upstream reads `regs.primitive_topology_control` and `regs.topology_override`.
    fn update_topology(&mut self, maxwell3d: &dyn Maxwell3DAccess) {
        match maxwell3d.primitive_topology_control() {
            PrimitiveTopologyControl::UseInBeginMethods => {}
            PrimitiveTopologyControl::UseSeparateState => {
                match maxwell3d.topology_override() {
                    PrimitiveTopologyOverride::None => {}
                    PrimitiveTopologyOverride::Points => {
                        self.draw_state.topology = PrimitiveTopology::Points;
                    }
                    PrimitiveTopologyOverride::Lines => {
                        self.draw_state.topology = PrimitiveTopology::Lines;
                    }
                    PrimitiveTopologyOverride::LineStrip => {
                        self.draw_state.topology = PrimitiveTopology::LineStrip;
                    }
                }
                // Upstream default case: cast topology_override raw value to PrimitiveTopology.
                // The match above covers all defined PrimitiveTopologyOverride variants.
                // For unknown values, upstream does:
                //   draw_state.topology = static_cast<PrimitiveTopology>(regs.topology_override);
                // We handle this by checking if no branch matched:
                let topo_override = maxwell3d.topology_override();
                if topo_override != PrimitiveTopologyOverride::None
                    && topo_override != PrimitiveTopologyOverride::Points
                    && topo_override != PrimitiveTopologyOverride::Lines
                    && topo_override != PrimitiveTopologyOverride::LineStrip
                {
                    self.draw_state.topology =
                        PrimitiveTopology::from_raw(maxwell3d.topology_override_raw());
                }
            }
        }
    }

    /// Core draw dispatch.
    ///
    /// Corresponds to `DrawManager::ProcessDraw`.
    /// Calls UpdateTopology, then rasterizer->Draw if ShouldExecute.
    fn process_draw(
        &mut self,
        draw_indexed: bool,
        instance_count: u32,
        maxwell3d: &mut dyn Maxwell3DAccess,
    ) {
        log::trace!(
            "DrawManager::process_draw: topology={:?}, count={}",
            self.draw_state.topology,
            if draw_indexed {
                self.draw_state.index_buffer.count
            } else {
                self.draw_state.vertex_buffer.count
            }
        );

        self.update_topology(maxwell3d);

        if maxwell3d.should_execute() {
            if let Some(rasterizer) = maxwell3d.rasterizer_mut() {
                rasterizer.draw(draw_indexed, instance_count);
            }
        }
    }

    /// Core indirect draw dispatch.
    ///
    /// Corresponds to `DrawManager::ProcessDrawIndirect`.
    /// Calls UpdateTopology, then rasterizer->DrawIndirect if ShouldExecute.
    fn process_draw_indirect(&mut self, maxwell3d: &mut dyn Maxwell3DAccess) {
        log::trace!(
            "DrawManager::process_draw_indirect: topology={:?}, is_indexed={}, buffer_size={}",
            self.draw_state.topology,
            self.indirect_state.is_indexed,
            self.indirect_state.buffer_size,
        );

        self.update_topology(maxwell3d);

        if maxwell3d.should_execute() {
            if let Some(rasterizer) = maxwell3d.rasterizer_mut() {
                rasterizer.draw_indirect();
            }
        }
    }
}

impl Default for DrawManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_buffer_small_from_raw() {
        // Topology = Triangles (0x5), count = 100, first = 0
        let raw = (0x5u32 << 28) | (100u32 << 16) | 0;
        let ibs = IndexBufferSmall::from_raw(raw);
        assert_eq!(ibs.first, 0);
        assert_eq!(ibs.count, 100);
        assert_eq!(ibs.topology, PrimitiveTopology::Triangles);

        // Topology = Lines (0x2), count = 50, first = 1234
        let raw2 = (0x2u32 << 28) | (50u32 << 16) | 1234;
        let ibs2 = IndexBufferSmall::from_raw(raw2);
        assert_eq!(ibs2.first, 1234);
        assert_eq!(ibs2.count, 50);
        assert_eq!(ibs2.topology, PrimitiveTopology::Lines);
    }

    #[test]
    fn test_index_buffer_small_max_values() {
        // Max first (16 bits) = 0xFFFF, max count (12 bits) = 0xFFF, max topo (4 bits) = 0xF
        let raw = 0xFFFF_FFFF;
        let ibs = IndexBufferSmall::from_raw(raw);
        assert_eq!(ibs.first, 0xFFFF);
        assert_eq!(ibs.count, 0xFFF);
        assert_eq!(ibs.topology, PrimitiveTopology::Patches); // 0xF
    }
}
