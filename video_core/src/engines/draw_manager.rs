// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/engines/draw_manager.h and draw_manager.cpp
//!
//! The DrawManager handles draw call dispatch for the Maxwell 3D engine.
//! It processes method writes for begin/end draws, inline index buffers,
//! instanced draws, and draw textures.

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
#[derive(Debug, Clone, Copy)]
pub struct IndexBufferSmall {
    pub first: u32,
    pub count: u32,
    pub topology: PrimitiveTopology,
}

impl IndexBufferSmall {
    /// Decode from a raw u32 argument (upstream `IndexBufferSmall{argument}`).
    pub fn from_raw(argument: u32) -> Self {
        // Upstream encoding: topology in bits [28:31], count in bits [16:27], first in bits [0:15]
        // Actual bit layout may differ; this is a placeholder matching typical NV encoding.
        Self {
            first: argument & 0xFFFF,
            count: (argument >> 16) & 0xFFF,
            topology: PrimitiveTopology::Points, // TODO: decode from argument bits
        }
    }
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
/// raw pointer to `Maxwell3D`; in Rust we keep draw state here and let the
/// parent engine invoke methods as needed.
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
        log::warn!("DrawManager::process_method_call: not yet implemented (requires Maxwell3D register access)");
    }

    /// Execute a clear operation.
    ///
    /// Corresponds to `DrawManager::Clear`.
    /// Stubbed — requires rasterizer access to call rasterizer->Clear(layer_count).
    /// Upstream: DrawManager::Clear() in video_core/engines/draw_manager.cpp
    pub fn clear(&mut self, _layer_count: u32) {
        log::warn!("DrawManager::clear: not yet implemented (requires rasterizer access)");
    }

    /// Flush any deferred instanced draw calls.
    ///
    /// Corresponds to `DrawManager::DrawDeferred`.
    pub fn draw_deferred(&mut self) {
        if self.draw_state.draw_mode != DrawMode::Instance
            || self.draw_state.instance_count == 0
        {
            return;
        }
        self.draw_end(self.draw_state.instance_count + 1, true);
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
    ) {
        self.draw_state.topology = topology;
        self.draw_state.vertex_buffer.first = vertex_first;
        self.draw_state.vertex_buffer.count = vertex_count;
        self.draw_state.base_instance = base_instance;
        self.process_draw(false, num_instances);
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
        self.process_draw(false, 1);
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
    ) {
        self.draw_state.topology = topology;
        self.draw_state.index_buffer.first = index_first;
        self.draw_state.index_buffer.count = index_count;
        self.draw_state.base_index = base_index;
        self.draw_state.base_instance = base_instance;
        self.process_draw(true, num_instances);
    }

    /// Issue an indirect non-indexed draw call.
    ///
    /// Corresponds to `DrawManager::DrawArrayIndirect`.
    pub fn draw_array_indirect(&mut self, topology: PrimitiveTopology) {
        self.draw_state.topology = topology;
        self.process_draw_indirect();
    }

    /// Issue an indirect indexed draw call.
    ///
    /// Corresponds to `DrawManager::DrawIndexedIndirect`.
    pub fn draw_indexed_indirect(
        &mut self,
        topology: PrimitiveTopology,
        index_first: u32,
        index_count: u32,
    ) {
        self.draw_state.topology = topology;
        self.draw_state.index_buffer.first = index_first;
        self.draw_state.index_buffer.count = index_count;
        self.process_draw_indirect();
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
    /// TODO: requires access to Maxwell3D regs.draw fields.
    pub fn draw_begin(&mut self, topology: PrimitiveTopology, is_first: bool, is_subsequent: bool) {
        if is_first {
            self.draw_deferred();
            self.draw_state.instance_count = 0;
            self.draw_state.draw_mode = DrawMode::General;
        } else if is_subsequent {
            self.draw_state.instance_count += 1;
            self.draw_state.draw_mode = DrawMode::Instance;
        }

        self.draw_state.topology = topology;
    }

    /// Handle draw-end, flushing the current draw call.
    ///
    /// Corresponds to `DrawManager::DrawEnd`.
    /// TODO: full implementation requires Maxwell3D register access for
    /// global_base_instance_index, global_base_vertex_index, index_buffer, etc.
    fn draw_end(&mut self, instance_count: u32, force_draw: bool) {
        match self.draw_state.draw_mode {
            DrawMode::Instance => {
                if !force_draw {
                    return;
                }
                // fallthrough to General
                self.process_draw(self.draw_state.draw_indexed, instance_count);
                self.draw_state.draw_indexed = false;
            }
            DrawMode::General => {
                self.process_draw(self.draw_state.draw_indexed, instance_count);
                self.draw_state.draw_indexed = false;
            }
            DrawMode::InlineIndex => {
                self.draw_state.index_buffer.count =
                    (self.draw_state.inline_index_draw_indexes.len() / 4) as u32;
                self.draw_state.index_buffer.format = IndexFormat::UnsignedInt;
                // TODO: set dirty flag for IndexBuffer
                self.process_draw(true, instance_count);
                self.draw_state.inline_index_draw_indexes.clear();
            }
        }
    }

    /// Handle small (packed) index draw.
    ///
    /// Corresponds to `DrawManager::DrawIndexSmall`.
    pub fn draw_index_small(&mut self, argument: u32) {
        let index_small = IndexBufferSmall::from_raw(argument);
        self.draw_state.index_buffer.first = index_small.first;
        self.draw_state.index_buffer.count = index_small.count;
        self.draw_state.topology = index_small.topology;
        // TODO: set dirty flag for IndexBuffer
        self.process_draw(true, 1);
    }

    /// Handle draw-texture trigger.
    ///
    /// Corresponds to `DrawManager::DrawTexture`.
    /// Stubbed — requires access to Maxwell3D draw_texture and surface_clip registers to
    /// compute dst/src coordinates, then calls rasterizer->DrawTexture().
    /// Upstream: DrawManager::DrawTexture() in video_core/engines/draw_manager.cpp
    pub fn draw_texture(&mut self) {
        log::warn!("DrawManager::draw_texture: not yet implemented (requires Maxwell3D register access)");
    }

    /// Update topology based on topology control mode and override.
    ///
    /// Corresponds to `DrawManager::UpdateTopology`.
    pub fn update_topology(
        &mut self,
        control: PrimitiveTopologyControl,
        topology_override: PrimitiveTopologyOverride,
    ) {
        match control {
            PrimitiveTopologyControl::UseInBeginMethods => {}
            PrimitiveTopologyControl::UseSeparateState => match topology_override {
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
            },
        }
    }

    /// Core draw dispatch.
    ///
    /// Corresponds to `DrawManager::ProcessDraw`.
    /// TODO: requires rasterizer access to call `rasterizer->Draw(...)`.
    fn process_draw(&mut self, _draw_indexed: bool, _instance_count: u32) {
        // TODO: call UpdateTopology, then rasterizer->Draw if ShouldExecute
        log::trace!(
            "DrawManager::process_draw: topology={:?}, count={}",
            self.draw_state.topology,
            if _draw_indexed {
                self.draw_state.index_buffer.count
            } else {
                self.draw_state.vertex_buffer.count
            }
        );
    }

    /// Core indirect draw dispatch.
    ///
    /// Corresponds to `DrawManager::ProcessDrawIndirect`.
    /// TODO: requires rasterizer access to call `rasterizer->DrawIndirect(...)`.
    fn process_draw_indirect(&mut self) {
        // TODO: call UpdateTopology, then rasterizer->DrawIndirect if ShouldExecute
        log::trace!(
            "DrawManager::process_draw_indirect: topology={:?}, is_indexed={}, buffer_size={}",
            self.draw_state.topology,
            self.indirect_state.is_indexed,
            self.indirect_state.buffer_size,
        );
    }
}

impl Default for DrawManager {
    fn default() -> Self {
        Self::new()
    }
}
