// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/engines/draw_manager.h and draw_manager.cpp
//!
//! The DrawManager handles draw call dispatch for the Maxwell 3D engine.
//! It processes method writes for begin/end draws, inline index buffers,
//! instanced draws, and draw textures.

use crate::dirty_flags::flags as Dirty;
use crate::engines::maxwell_3d::{
    ConstBufferBinding, DrawCall, RenderTargetInfo, RtControlInfo, CLEAR_SURFACE, DRAW_BEGIN,
    DRAW_END, DRAW_INLINE_INDEX, DRAW_TEXTURE_SRC_Y0, IB_BASE, IB_OFF_COUNT, IB_OFF_FIRST,
    INDEX_BUFFER16_FIRST, INDEX_BUFFER16_SUBSEQUENT, INDEX_BUFFER32_FIRST,
    INDEX_BUFFER32_SUBSEQUENT, INDEX_BUFFER8_FIRST, INDEX_BUFFER8_SUBSEQUENT,
    INLINE_INDEX_2X16_EVEN, INLINE_INDEX_4X8_INDEX0, MAX_CB_SLOTS, NUM_SHADER_PROGRAMS,
    NUM_SHADER_STAGES, NUM_VERTEX_ATTRIBS, RT_FORMAT_A8B8G8R8_SRGB, RT_FORMAT_A8B8G8R8_UNORM,
    RT_FORMAT_B5G6R5_UNORM, RT_FORMAT_R8_UNORM, TOPOLOGY_OVERRIDE, VB_COUNT, VB_FIRST,
    VERTEX_ARRAY_INSTANCE_FIRST, VERTEX_ARRAY_INSTANCE_SUBSEQUENT,
};
use crate::engines::Framebuffer;
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

impl PrimitiveTopologyControl {
    pub fn from_raw(raw: u32) -> Self {
        match raw {
            1 => Self::UseSeparateState,
            _ => Self::UseInBeginMethods,
        }
    }
}

// `PrimitiveTopology` is the upstream-faithful enum from
// `engines::maxwell_3d` (matching `Maxwell3D::Regs::PrimitiveTopology`).
// Re-exported here so existing imports of
// `engines::draw_manager::PrimitiveTopology` continue to resolve.
pub use crate::engines::maxwell_3d::PrimitiveTopology;

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

impl PrimitiveTopologyOverride {
    pub fn from_raw(raw: u32) -> Self {
        match raw {
            1 => Self::Points,
            2 => Self::Lines,
            3 => Self::LineStrip,
            _ => Self::None,
        }
    }
}

/// Port of `DrawManager::UpdateTopology()` as a pure owner-local helper.
///
/// This keeps the topology-resolution logic in the matching upstream owner
/// file even when other owners need the same resolved draw-state topology.
pub fn resolve_draw_topology(
    mut draw_topology: PrimitiveTopology,
    primitive_topology_control: PrimitiveTopologyControl,
    topology_override: PrimitiveTopologyOverride,
    topology_override_raw: u32,
) -> PrimitiveTopology {
    match primitive_topology_control {
        PrimitiveTopologyControl::UseInBeginMethods => {}
        PrimitiveTopologyControl::UseSeparateState => match topology_override {
            PrimitiveTopologyOverride::None => {}
            PrimitiveTopologyOverride::Points => {
                draw_topology = PrimitiveTopology::Points;
            }
            PrimitiveTopologyOverride::Lines => {
                draw_topology = PrimitiveTopology::Lines;
            }
            PrimitiveTopologyOverride::LineStrip => {
                draw_topology = PrimitiveTopology::LineStrip;
            }
        },
    }

    if primitive_topology_control == PrimitiveTopologyControl::UseSeparateState
        && topology_override != PrimitiveTopologyOverride::None
        && topology_override != PrimitiveTopologyOverride::Points
        && topology_override != PrimitiveTopologyOverride::Lines
        && topology_override != PrimitiveTopologyOverride::LineStrip
    {
        draw_topology = PrimitiveTopology::from_raw(topology_override_raw);
    }

    draw_topology
}

fn format_clear_color(
    format: u32,
    color: [f32; 4],
    clear_r: bool,
    clear_g: bool,
    clear_b: bool,
    clear_a: bool,
) -> [u8; 4] {
    let r = if clear_r {
        (color[0].clamp(0.0, 1.0) * 255.0) as u8
    } else {
        0
    };
    let g = if clear_g {
        (color[1].clamp(0.0, 1.0) * 255.0) as u8
    } else {
        0
    };
    let b = if clear_b {
        (color[2].clamp(0.0, 1.0) * 255.0) as u8
    } else {
        0
    };
    let a = if clear_a {
        (color[3].clamp(0.0, 1.0) * 255.0) as u8
    } else {
        0
    };

    match format {
        RT_FORMAT_A8B8G8R8_UNORM | RT_FORMAT_A8B8G8R8_SRGB => [r, g, b, a],
        RT_FORMAT_R8_UNORM => [r, 0, 0, 255],
        RT_FORMAT_B5G6R5_UNORM => [r, g, b, 255],
        _ => {
            log::trace!(
                "DrawManager::clear RT format 0x{:X}, using RGBA8 layout for local framebuffer",
                format
            );
            [r, g, b, a]
        }
    }
}

// `IndexFormat` is the upstream-faithful enum from
// `engines::maxwell_3d` (matching `Maxwell3D::Regs::IndexFormat`).
pub use crate::engines::maxwell_3d::IndexFormat;

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

    /// Decode the current packed `inline_index_2x16` register into its two
    /// 16-bit values, matching upstream register-field access.
    fn inline_index_2x16_values(&self) -> (u32, u32);

    /// Decode the current packed `inline_index_4x8` register into its four
    /// byte values, matching upstream register-field access.
    fn inline_index_4x8_values(&self) -> [u32; 4];

    /// Decode `vertex_array_instance_first` from the written argument.
    fn vertex_array_instance_first_params(&self, argument: u32) -> (PrimitiveTopology, u32, u32);

    /// Decode `vertex_array_instance_subsequent` from the written argument.
    fn vertex_array_instance_subsequent_params(
        &self,
        argument: u32,
    ) -> (PrimitiveTopology, u32, u32);

    /// Set a dirty flag. Upstream: `maxwell3d->dirty.flags[index] = true`.
    fn set_dirty_flag(&mut self, index: u8);

    /// Read dirty flags for draw-time rasterizer state synchronization,
    /// matching upstream rasterizer reads of `maxwell3d->dirty.flags`.
    fn dirty_flags(&self) -> [bool; 256];

    /// Clear one dirty flag after the backend consumes it.
    fn clear_dirty_flag(&mut self, index: u8);

    /// Run a closure with the bound rasterizer, if present.
    ///
    /// Upstream stores `RasterizerInterface*` on `Maxwell3D` and all draw
    /// paths dispatch through that owner. Keeping the callback on
    /// `Maxwell3DAccess` avoids leaking a mutable rasterizer reference into
    /// `DrawManager` while remaining object-safe for the trait boundary.
    fn with_rasterizer_mut(&mut self, f: &mut dyn FnMut(&mut dyn RasterizerInterface)) -> bool;

    /// Dispatch a draw through the bound rasterizer using the draw-time
    /// Maxwell3D view. Upstream `DrawManager::ProcessDraw` calls
    /// `maxwell3d->rasterizer->Draw(...)`; keeping this as a Maxwell3D-owned
    /// operation avoids spreading rasterizer ownership through DrawManager.
    fn draw_rasterizer(&mut self, draw_state: &DrawState, instance_count: u32) -> bool {
        let mut dispatched = false;
        self.with_rasterizer_mut(&mut |rasterizer| {
            rasterizer.draw(Maxwell3DDrawView::new(draw_state), instance_count);
            dispatched = true;
        });
        dispatched
    }

    /// Dispatch a clear through the bound rasterizer using the clear-time
    /// Maxwell3D view. Upstream `DrawManager::Clear` calls
    /// `maxwell3d->rasterizer->Clear(layer_count)` and the backend reads
    /// Maxwell3D registers directly; the concrete Maxwell3D implementation
    /// passes a live view while the default trait fallback keeps test fakes
    /// working with an empty snapshot.
    fn clear_rasterizer(&mut self, layer_count: u32) -> bool {
        let mut dispatched = false;
        self.with_rasterizer_mut(&mut |rasterizer| {
            rasterizer.clear(Maxwell3DClearView::default(), layer_count);
            dispatched = true;
        });
        dispatched
    }

    /// Dispatch an indirect draw through the bound rasterizer.
    fn draw_indirect_rasterizer(
        &mut self,
        draw_state: &DrawState,
        indirect_params: &IndirectParams,
    ) -> bool {
        let mut dispatched = false;
        self.with_rasterizer_mut(&mut |rasterizer| {
            rasterizer.draw_indirect(Maxwell3DIndirectView::new(draw_state, indirect_params));
            dispatched = true;
        });
        dispatched
    }

    /// Dispatch a draw-texture operation through the bound rasterizer.
    fn draw_texture_rasterizer(&mut self) -> bool {
        let mut dispatched = false;
        self.with_rasterizer_mut(&mut |rasterizer| {
            rasterizer.draw_texture();
            dispatched = true;
        });
        dispatched
    }

    /// Snapshot per-stage shader program GPU virtual addresses.
    /// Disabled stages report `0`. Default impl returns all zeros for tests
    /// and stub access types that do not yet plumb shader-program registers.
    fn shader_program_addresses(&self) -> [u64; 6] {
        [0; 6]
    }

    /// Read the live `regs.draw_texture` payload.
    fn draw_texture_params(&self) -> DrawTextureParams;

    /// Read whether `regs.window_origin.mode != UpperLeft`.
    fn window_origin_lower_left(&self) -> bool;

    /// Read whether `regs.window_origin.flip_y != 0`.
    fn window_origin_flip_y(&self) -> bool;

    /// Read `regs.viewport_transform[index].scale_y`.
    fn viewport_transform_scale_y(&self, index: u32) -> f32;

    /// Read raw `regs.viewport_transform[index]`.
    fn viewport_transform_info(
        &self,
        index: u32,
    ) -> crate::engines::maxwell_3d::ViewportTransformInfo;

    /// Read whether `regs.viewport_scale_offset_enabled != 0`.
    fn viewport_scale_offset_enabled(&self) -> bool;

    /// Read raw `regs.surface_clip`.
    fn surface_clip_info(&self) -> crate::engines::maxwell_3d::SurfaceClipInfo;

    /// Read `regs.framebuffer_srgb`.
    fn framebuffer_srgb(&self) -> bool {
        false
    }

    /// Read `regs.surface_clip.height`.
    fn surface_clip_height(&self) -> u32;

    /// Read the current clear-surface flags register payload.
    fn clear_surface_flags(&self) -> u32;

    /// Read render-target address.
    fn rt_address(&self, index: usize) -> u64;

    /// Read render-target width.
    fn rt_width(&self, index: usize) -> u32;

    /// Read render-target height.
    fn rt_height(&self, index: usize) -> u32;

    /// Read render-target format.
    fn rt_format(&self, index: usize) -> u32;

    /// Read the full render-target config snapshot.
    fn rt_info(&self, index: usize) -> crate::engines::maxwell_3d::RenderTargetInfo {
        crate::engines::maxwell_3d::RenderTargetInfo {
            address: self.rt_address(index),
            width: self.rt_width(index),
            height: self.rt_height(index),
            format: self.rt_format(index),
            ..Default::default()
        }
    }

    /// Read clear color as RGBA floats.
    fn clear_color_rgba(&self) -> [f32; 4];

    /// Read clear depth value.
    fn clear_depth(&self) -> f32 {
        0.0
    }

    /// Read clear stencil value.
    fn clear_stencil(&self) -> i32 {
        0
    }

    /// Publish a pending framebuffer.
    fn set_pending_framebuffer(&mut self, framebuffer: Framebuffer);

    /// Read index buffer GPU virtual address.
    fn index_buffer_addr(&self) -> u64;

    /// Read index buffer GPU virtual end address.
    fn index_buffer_addr_end(&self) -> u64 {
        let index_buffer = self.index_buffer();
        self.index_buffer_addr()
            + index_buffer.count as u64 * index_buffer.format.size_bytes() as u64
    }

    /// Read vertex stream info for one stream slot.
    fn vertex_stream_info(&self, index: u32) -> crate::engines::maxwell_3d::VertexStreamInfo;

    /// Read vertex stream instancing enable for one stream slot.
    fn vertex_stream_instance(&self, index: u32) -> u32;

    /// Read vertex stream limit info for one stream slot.
    fn vertex_stream_limit(&self, index: u32) -> VertexStreamLimit;

    /// Read viewport info for one viewport slot.
    fn viewport_info(&self, index: u32) -> crate::engines::maxwell_3d::ViewportInfo;

    /// Read scissor info for one scissor slot.
    fn scissor_info(&self, index: u32) -> crate::engines::maxwell_3d::ScissorInfo;

    /// Read effective blend state for one render target.
    fn effective_blend_info(&self, rt: usize) -> crate::engines::maxwell_3d::BlendInfo;

    /// Read blend constant color.
    fn blend_color_info(&self) -> crate::engines::maxwell_3d::BlendColorInfo;

    /// Read combined depth/stencil state.
    fn depth_stencil_info(&self) -> crate::engines::maxwell_3d::DepthStencilInfo;

    /// Read rasterizer state.
    fn rasterizer_info(&self) -> crate::engines::maxwell_3d::RasterizerInfo;

    /// Read shader program region base address.
    fn program_base_address(&self) -> u64;

    /// Read one constant-buffer binding.
    fn const_buffer_binding(
        &self,
        stage: usize,
        slot: usize,
    ) -> crate::engines::maxwell_3d::ConstBufferBinding;

    /// Read vertex attribute info for one attribute slot.
    fn vertex_attrib_info(&self, index: u32) -> crate::engines::maxwell_3d::VertexAttribInfo;

    /// Read shader stage info for one pipeline slot.
    fn shader_stage_info(&self, index: u32) -> crate::engines::maxwell_3d::ShaderStageInfo;

    /// Read color write mask for one render target.
    fn color_mask_info(&self, rt: usize) -> crate::engines::maxwell_3d::ColorMaskInfo;

    /// Read render target control info.
    fn rt_control_info(&self) -> crate::engines::maxwell_3d::RtControlInfo;

    /// Read texture header pool base address.
    fn tex_header_pool_address(&self) -> u64;

    /// Read texture header pool limit.
    fn tex_header_pool_limit(&self) -> u32;

    /// Read texture sampler pool base address.
    fn tex_sampler_pool_address(&self) -> u64;

    /// Read texture sampler pool limit.
    fn tex_sampler_pool_limit(&self) -> u32;

    /// Read sampler binding mode.
    fn sampler_binding(&self) -> crate::engines::maxwell_3d::SamplerBinding;
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
#[derive(Debug, Clone)]
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

/// Render-target register snapshot used by draw and clear views.
#[derive(Debug, Clone, Copy, Default)]
pub struct Maxwell3DRenderTargets {
    pub rt_control: RtControlInfo,
    pub render_targets: [RenderTargetInfo; 8],
}

#[derive(Debug, Clone)]
pub struct Maxwell3DDrawRegisters {
    pub shader_program_addresses: [u64; 6],
    pub index_buffer_gpu_addr: u64,
    pub index_buffer_gpu_addr_end: u64,
    pub render_targets: Maxwell3DRenderTargets,
    pub cb_bindings: [[ConstBufferBinding; MAX_CB_SLOTS]; NUM_SHADER_STAGES],
    pub vertex_streams: [crate::engines::maxwell_3d::VertexStreamInfo; 32],
    pub vertex_stream_instances: [u32; 32],
    pub vertex_stream_limits: [VertexStreamLimit; 32],
    pub vertex_attribs: [crate::engines::maxwell_3d::VertexAttribInfo; 32],
    pub scissors:
        [crate::engines::maxwell_3d::ScissorInfo; crate::engines::maxwell_3d::NUM_VIEWPORTS],
    pub blend: [crate::engines::maxwell_3d::BlendInfo; 8],
    pub blend_color: crate::engines::maxwell_3d::BlendColorInfo,
    pub depth_stencil: crate::engines::maxwell_3d::DepthStencilInfo,
    pub rasterizer: crate::engines::maxwell_3d::RasterizerInfo,
    pub descriptor_sync_regs: crate::texture_cache::texture_cache_base::DescriptorSyncRegs,
    pub window_origin_lower_left: bool,
    pub window_origin_flip_y: bool,
    pub viewport0_scale_y: f32,
    pub viewport_transforms:
        [crate::engines::maxwell_3d::ViewportTransformInfo; crate::engines::maxwell_3d::NUM_VIEWPORTS],
    pub viewport_scale_offset_enabled: bool,
    pub surface_clip: crate::engines::maxwell_3d::SurfaceClipInfo,
    pub framebuffer_srgb: bool,
    pub depth_mode: crate::engines::maxwell_3d::DepthMode,
    pub dirty_flags: [bool; 256],
    pub color_masks: [crate::engines::maxwell_3d::ColorMaskInfo; 8],
}

impl Default for Maxwell3DDrawRegisters {
    fn default() -> Self {
        Self {
            shader_program_addresses: [0; 6],
            index_buffer_gpu_addr: 0,
            index_buffer_gpu_addr_end: 0,
            render_targets: Default::default(),
            cb_bindings: Default::default(),
            vertex_streams: Default::default(),
            vertex_stream_instances: [0; 32],
            vertex_stream_limits: Default::default(),
            vertex_attribs: Default::default(),
            scissors: Default::default(),
            blend: Default::default(),
            blend_color: Default::default(),
            depth_stencil: Default::default(),
            rasterizer: Default::default(),
            descriptor_sync_regs: Default::default(),
            window_origin_lower_left: false,
            window_origin_flip_y: false,
            viewport0_scale_y: 0.0,
            viewport_transforms: Default::default(),
            viewport_scale_offset_enabled: false,
            surface_clip: Default::default(),
            framebuffer_srgb: false,
            depth_mode: crate::engines::maxwell_3d::DepthMode::ZeroToOne,
            dirty_flags: [false; 256],
            color_masks: Default::default(),
        }
    }
}

impl Maxwell3DDrawRegisters {
    pub fn from_maxwell3d(maxwell3d: &dyn Maxwell3DAccess) -> Self {
        Self {
            shader_program_addresses: maxwell3d.shader_program_addresses(),
            index_buffer_gpu_addr: maxwell3d.index_buffer_addr(),
            index_buffer_gpu_addr_end: maxwell3d.index_buffer_addr_end(),
            render_targets: Maxwell3DRenderTargets {
                rt_control: maxwell3d.rt_control_info(),
                render_targets: std::array::from_fn(|i| maxwell3d.rt_info(i)),
            },
            cb_bindings: std::array::from_fn(|stage| {
                std::array::from_fn(|slot| maxwell3d.const_buffer_binding(stage, slot))
            }),
            vertex_streams: std::array::from_fn(|i| maxwell3d.vertex_stream_info(i as u32)),
            vertex_stream_instances: std::array::from_fn(|i| {
                maxwell3d.vertex_stream_instance(i as u32)
            }),
            vertex_stream_limits: std::array::from_fn(|i| maxwell3d.vertex_stream_limit(i as u32)),
            vertex_attribs: std::array::from_fn(|i| maxwell3d.vertex_attrib_info(i as u32)),
            scissors: std::array::from_fn(|i| maxwell3d.scissor_info(i as u32)),
            blend: std::array::from_fn(|i| maxwell3d.effective_blend_info(i)),
            blend_color: maxwell3d.blend_color_info(),
            depth_stencil: maxwell3d.depth_stencil_info(),
            rasterizer: maxwell3d.rasterizer_info(),
            descriptor_sync_regs: crate::texture_cache::texture_cache_base::DescriptorSyncRegs {
                sampler_binding_via_header: matches!(
                    maxwell3d.sampler_binding(),
                    crate::engines::maxwell_3d::SamplerBinding::ViaHeaderBinding
                ),
                tex_header_addr: maxwell3d.tex_header_pool_address(),
                tex_header_limit: maxwell3d.tex_header_pool_limit(),
                tex_sampler_addr: maxwell3d.tex_sampler_pool_address(),
                tex_sampler_limit: maxwell3d.tex_sampler_pool_limit(),
            },
            window_origin_lower_left: maxwell3d.window_origin_lower_left(),
            window_origin_flip_y: maxwell3d.window_origin_flip_y(),
            viewport0_scale_y: maxwell3d.viewport_transform_scale_y(0),
            viewport_transforms: std::array::from_fn(|i| {
                maxwell3d.viewport_transform_info(i as u32)
            }),
            viewport_scale_offset_enabled: maxwell3d.viewport_scale_offset_enabled(),
            surface_clip: maxwell3d.surface_clip_info(),
            framebuffer_srgb: maxwell3d.framebuffer_srgb(),
            depth_mode: maxwell3d.depth_stencil_info().depth_mode,
            dirty_flags: maxwell3d.dirty_flags(),
            color_masks: std::array::from_fn(|i| maxwell3d.color_mask_info(i)),
        }
    }
}

#[derive(Clone)]
enum Maxwell3DDrawSource<'a> {
    Live(&'a dyn Maxwell3DAccess),
    Snapshot(Maxwell3DDrawRegisters),
}

/// Draw-time Maxwell3D view passed to rasterizers.
///
/// Upstream rasterizers keep a `Maxwell3D*` and read register/dirty state
/// through it while drawing. This wrapper keeps the backend entry point on a
/// Maxwell view instead of a raw `DrawState`; the live path reads through
/// `Maxwell3DAccess` so Maxwell register-derived values do not live in
/// `DrawState`.
#[derive(Clone)]
pub struct Maxwell3DDrawView<'a> {
    draw_state: &'a DrawState,
    source: Maxwell3DDrawSource<'a>,
}

impl<'a> Maxwell3DDrawView<'a> {
    pub fn new(draw_state: &'a DrawState) -> Self {
        Self {
            draw_state,
            source: Maxwell3DDrawSource::Snapshot(Maxwell3DDrawRegisters::default()),
        }
    }

    pub fn live(draw_state: &'a DrawState, maxwell3d: &'a dyn Maxwell3DAccess) -> Self {
        Self {
            draw_state,
            source: Maxwell3DDrawSource::Live(maxwell3d),
        }
    }

    pub fn with_register_snapshot(
        draw_state: &'a DrawState,
        registers: Maxwell3DDrawRegisters,
    ) -> Self {
        Self {
            draw_state,
            source: Maxwell3DDrawSource::Snapshot(registers),
        }
    }

    pub fn draw_state(&self) -> &'a DrawState {
        self.draw_state
    }

    pub fn registers(&self) -> Maxwell3DDrawRegisters {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => {
                Maxwell3DDrawRegisters::from_maxwell3d(*maxwell3d)
            }
            Maxwell3DDrawSource::Snapshot(registers) => registers.clone(),
        }
    }

    /// Per-stage shader program GPU virtual addresses.
    ///
    /// Upstream computes these as
    /// `maxwell3d->regs.program_region.Address() + maxwell3d->regs.pipelines[i].offset`.
    /// The live view reads these from Maxwell3D directly; snapshot mode is
    /// retained only for tests and default trait fallbacks.
    pub fn shader_program_addresses(&self) -> [u64; 6] {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.shader_program_addresses(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.shader_program_addresses,
        }
    }

    pub fn index_buffer_gpu_addr(&self) -> u64 {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.index_buffer_addr(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.index_buffer_gpu_addr,
        }
    }

    pub fn index_buffer_gpu_addr_end(&self) -> u64 {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.index_buffer_addr_end(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.index_buffer_gpu_addr_end,
        }
    }

    pub fn render_targets(&self) -> Maxwell3DRenderTargets {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => Maxwell3DRenderTargets {
                rt_control: maxwell3d.rt_control_info(),
                render_targets: std::array::from_fn(|i| maxwell3d.rt_info(i)),
            },
            Maxwell3DDrawSource::Snapshot(registers) => registers.render_targets,
        }
    }

    pub fn cb_bindings(&self) -> [[ConstBufferBinding; MAX_CB_SLOTS]; NUM_SHADER_STAGES] {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => std::array::from_fn(|stage| {
                std::array::from_fn(|slot| maxwell3d.const_buffer_binding(stage, slot))
            }),
            Maxwell3DDrawSource::Snapshot(registers) => registers.cb_bindings,
        }
    }

    pub fn vertex_streams(&self) -> [crate::engines::maxwell_3d::VertexStreamInfo; 32] {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => {
                std::array::from_fn(|i| maxwell3d.vertex_stream_info(i as u32))
            }
            Maxwell3DDrawSource::Snapshot(registers) => registers.vertex_streams,
        }
    }

    pub fn vertex_stream_instances(&self) -> [u32; 32] {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => {
                std::array::from_fn(|i| maxwell3d.vertex_stream_instance(i as u32))
            }
            Maxwell3DDrawSource::Snapshot(registers) => registers.vertex_stream_instances,
        }
    }

    pub fn vertex_stream_limits(&self) -> [VertexStreamLimit; 32] {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => {
                std::array::from_fn(|i| maxwell3d.vertex_stream_limit(i as u32))
            }
            Maxwell3DDrawSource::Snapshot(registers) => registers.vertex_stream_limits,
        }
    }

    pub fn vertex_attribs(&self) -> [crate::engines::maxwell_3d::VertexAttribInfo; 32] {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => {
                std::array::from_fn(|i| maxwell3d.vertex_attrib_info(i as u32))
            }
            Maxwell3DDrawSource::Snapshot(registers) => registers.vertex_attribs,
        }
    }

    pub fn scissors(
        &self,
    ) -> [crate::engines::maxwell_3d::ScissorInfo; crate::engines::maxwell_3d::NUM_VIEWPORTS]
    {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => {
                std::array::from_fn(|i| maxwell3d.scissor_info(i as u32))
            }
            Maxwell3DDrawSource::Snapshot(registers) => registers.scissors,
        }
    }

    pub fn blend(&self) -> [crate::engines::maxwell_3d::BlendInfo; 8] {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => {
                std::array::from_fn(|i| maxwell3d.effective_blend_info(i))
            }
            Maxwell3DDrawSource::Snapshot(registers) => registers.blend,
        }
    }

    pub fn blend_color(&self) -> crate::engines::maxwell_3d::BlendColorInfo {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.blend_color_info(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.blend_color,
        }
    }

    pub fn depth_stencil(&self) -> crate::engines::maxwell_3d::DepthStencilInfo {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.depth_stencil_info(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.depth_stencil.clone(),
        }
    }

    pub fn rasterizer(&self) -> crate::engines::maxwell_3d::RasterizerInfo {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.rasterizer_info(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.rasterizer.clone(),
        }
    }

    pub fn descriptor_sync_regs(
        &self,
    ) -> crate::texture_cache::texture_cache_base::DescriptorSyncRegs {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => {
                crate::texture_cache::texture_cache_base::DescriptorSyncRegs {
                    sampler_binding_via_header: matches!(
                        maxwell3d.sampler_binding(),
                        crate::engines::maxwell_3d::SamplerBinding::ViaHeaderBinding
                    ),
                    tex_header_addr: maxwell3d.tex_header_pool_address(),
                    tex_header_limit: maxwell3d.tex_header_pool_limit(),
                    tex_sampler_addr: maxwell3d.tex_sampler_pool_address(),
                    tex_sampler_limit: maxwell3d.tex_sampler_pool_limit(),
                }
            }
            Maxwell3DDrawSource::Snapshot(registers) => registers.descriptor_sync_regs,
        }
    }

    pub fn window_origin_lower_left(&self) -> bool {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.window_origin_lower_left(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.window_origin_lower_left,
        }
    }

    pub fn window_origin_flip_y(&self) -> bool {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.window_origin_flip_y(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.window_origin_flip_y,
        }
    }

    pub fn viewport0_scale_y(&self) -> f32 {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.viewport_transform_scale_y(0),
            Maxwell3DDrawSource::Snapshot(registers) => registers.viewport0_scale_y,
        }
    }

    pub fn viewport_transforms(
        &self,
    ) -> [crate::engines::maxwell_3d::ViewportTransformInfo; crate::engines::maxwell_3d::NUM_VIEWPORTS]
    {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => {
                std::array::from_fn(|i| maxwell3d.viewport_transform_info(i as u32))
            }
            Maxwell3DDrawSource::Snapshot(registers) => registers.viewport_transforms,
        }
    }

    pub fn viewport_scale_offset_enabled(&self) -> bool {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.viewport_scale_offset_enabled(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.viewport_scale_offset_enabled,
        }
    }

    pub fn surface_clip(&self) -> crate::engines::maxwell_3d::SurfaceClipInfo {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.surface_clip_info(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.surface_clip,
        }
    }

    pub fn framebuffer_srgb(&self) -> bool {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.framebuffer_srgb(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.framebuffer_srgb,
        }
    }

    pub fn depth_mode(&self) -> crate::engines::maxwell_3d::DepthMode {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.depth_stencil_info().depth_mode,
            Maxwell3DDrawSource::Snapshot(registers) => registers.depth_mode,
        }
    }

    pub fn dirty_flags(&self) -> [bool; 256] {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => maxwell3d.dirty_flags(),
            Maxwell3DDrawSource::Snapshot(registers) => registers.dirty_flags,
        }
    }

    pub fn color_masks(&self) -> [crate::engines::maxwell_3d::ColorMaskInfo; 8] {
        match &self.source {
            Maxwell3DDrawSource::Live(maxwell3d) => {
                std::array::from_fn(|i| maxwell3d.color_mask_info(i))
            }
            Maxwell3DDrawSource::Snapshot(registers) => registers.color_masks,
        }
    }
}

#[derive(Clone, Copy)]
enum Maxwell3DClearSource<'a> {
    Live(&'a dyn Maxwell3DAccess),
    Snapshot {
        clear_state: ClearState,
        render_targets: Maxwell3DRenderTargets,
    },
}

#[derive(Clone, Copy)]
pub struct Maxwell3DClearView<'a> {
    source: Maxwell3DClearSource<'a>,
}

impl<'a> Maxwell3DClearView<'a> {
    pub fn new(clear_state: ClearState, render_targets: Maxwell3DRenderTargets) -> Self {
        Self {
            source: Maxwell3DClearSource::Snapshot {
                clear_state,
                render_targets,
            },
        }
    }

    pub fn live(maxwell3d: &'a dyn Maxwell3DAccess) -> Self {
        Self {
            source: Maxwell3DClearSource::Live(maxwell3d),
        }
    }

    pub fn clear_state(&self) -> ClearState {
        match self.source {
            Maxwell3DClearSource::Live(maxwell3d) => ClearState {
                flags: maxwell3d.clear_surface_flags(),
                color: maxwell3d.clear_color_rgba(),
                depth: maxwell3d.clear_depth(),
                stencil: maxwell3d.clear_stencil(),
            },
            Maxwell3DClearSource::Snapshot { clear_state, .. } => clear_state,
        }
    }

    pub fn render_targets(&self) -> Maxwell3DRenderTargets {
        match self.source {
            Maxwell3DClearSource::Live(maxwell3d) => Maxwell3DRenderTargets {
                rt_control: maxwell3d.rt_control_info(),
                render_targets: std::array::from_fn(|i| maxwell3d.rt_info(i)),
            },
            Maxwell3DClearSource::Snapshot { render_targets, .. } => render_targets,
        }
    }
}

impl Default for Maxwell3DClearView<'static> {
    fn default() -> Self {
        Self::new(ClearState::default(), Maxwell3DRenderTargets::default())
    }
}

/// View passed to `RasterizerOpenGL::DrawIndirect`.
///
/// Upstream reaches this through `maxwell3d->draw_manager->GetIndirectParams()`
/// and `maxwell3d->regs` from the rasterizer's `Maxwell3D*`.
pub struct Maxwell3DIndirectView<'a> {
    draw_state: &'a DrawState,
    indirect_params: &'a IndirectParams,
}

impl<'a> Maxwell3DIndirectView<'a> {
    pub fn new(draw_state: &'a DrawState, indirect_params: &'a IndirectParams) -> Self {
        Self {
            draw_state,
            indirect_params,
        }
    }

    pub fn draw_state(&self) -> &'a DrawState {
        self.draw_state
    }

    pub fn params(&self) -> &'a IndirectParams {
        self.indirect_params
    }
}

impl Default for DrawState {
    fn default() -> Self {
        Self {
            topology: PrimitiveTopology::default(),
            draw_mode: DrawMode::default(),
            draw_indexed: false,
            base_index: 0,
            vertex_buffer: VertexBuffer::default(),
            index_buffer: IndexBuffer::default(),
            base_instance: 0,
            instance_count: 0,
            inline_index_draw_indexes: Vec::new(),
        }
    }
}

/// Vertex stream limit state captured from Maxwell3D registers.
#[derive(Debug, Clone, Copy, Default)]
pub struct VertexStreamLimit {
    pub address: u64,
}

/// Clear state captured from Maxwell3D registers.
#[derive(Debug, Clone, Copy, Default)]
pub struct ClearState {
    pub flags: u32,
    pub color: [f32; 4],
    pub depth: f32,
    pub stencil: i32,
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

/// Raw draw-texture register payload from `Maxwell3D`.
///
/// Corresponds to `Maxwell3D::Regs::DrawTexture`.
#[derive(Debug, Clone, Copy, Default)]
pub struct DrawTextureParams {
    pub dst_x0: i32,
    pub dst_y0: i32,
    pub dst_width: i32,
    pub dst_height: i32,
    pub dx_du: i64,
    pub dy_dv: i64,
    pub src_sampler: u32,
    pub src_texture: u32,
    pub src_x0: i32,
    pub src_y0: i32,
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
    compat_draw_calls: Vec<DrawCall>,
}

impl DrawManager {
    /// Create a new DrawManager.
    pub fn new() -> Self {
        Self {
            draw_state: DrawState::default(),
            draw_texture_state: DrawTextureState::default(),
            indirect_state: IndirectParams::default(),
            compat_draw_calls: Vec::new(),
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

    /// Drain the temporary software-facing compatibility queue.
    pub fn take_compat_draw_calls(&mut self) -> Vec<DrawCall> {
        std::mem::take(&mut self.compat_draw_calls)
    }

    /// Push a temporary software-facing compatibility draw snapshot.
    pub fn push_compat_draw_call(&mut self, draw_call: DrawCall) {
        self.compat_draw_calls.push(draw_call);
    }

    fn build_compat_draw_call(
        &self,
        draw_state: &DrawState,
        draw_indexed: bool,
        instance_count: u32,
        maxwell3d: &dyn Maxwell3DAccess,
    ) -> DrawCall {
        let mut vertex_streams = Vec::new();
        for i in 0..32u32 {
            let info = maxwell3d.vertex_stream_info(i);
            if info.enabled {
                vertex_streams.push(info);
            }
        }

        let mut vertex_attribs = Vec::new();
        for i in 0..NUM_VERTEX_ATTRIBS {
            let info = maxwell3d.vertex_attrib_info(i);
            if info.buffer_index != 0
                || info.constant
                || info.offset != 0
                || info.size as u32 != 0
                || info.attrib_type as u32 != 0
                || info.bgra
            {
                vertex_attribs.push(info);
            }
        }

        let mut shader_stages =
            [crate::engines::maxwell_3d::ShaderStageInfo::default(); NUM_SHADER_PROGRAMS];
        for (i, stage) in shader_stages.iter_mut().enumerate() {
            *stage = maxwell3d.shader_stage_info(i as u32);
        }

        let mut color_masks = [crate::engines::maxwell_3d::ColorMaskInfo::default(); 8];
        for (i, mask) in color_masks.iter_mut().enumerate() {
            *mask = maxwell3d.color_mask_info(i);
        }

        let render_targets = std::array::from_fn(|i| maxwell3d.rt_info(i));

        let mut blend = [crate::engines::maxwell_3d::BlendInfo::default(); 8];
        for (i, item) in blend.iter_mut().enumerate() {
            *item = maxwell3d.effective_blend_info(i);
        }

        let cb_bindings = std::array::from_fn(|stage| {
            std::array::from_fn(|slot| maxwell3d.const_buffer_binding(stage, slot))
        });

        DrawCall {
            topology: draw_state.topology,
            vertex_first: draw_state.vertex_buffer.first,
            vertex_count: draw_state.vertex_buffer.count,
            indexed: draw_indexed,
            index_buffer_addr: maxwell3d.index_buffer_addr(),
            index_buffer_count: draw_state.index_buffer.count,
            index_buffer_first: draw_state.index_buffer.first,
            index_format: draw_state.index_buffer.format,
            vertex_streams,
            viewports: std::array::from_fn(|i| maxwell3d.viewport_info(i as u32)),
            scissors: std::array::from_fn(|i| maxwell3d.scissor_info(i as u32)),
            blend,
            blend_color: maxwell3d.blend_color_info(),
            depth_stencil: maxwell3d.depth_stencil_info(),
            rasterizer: maxwell3d.rasterizer_info(),
            program_base_address: maxwell3d.program_base_address(),
            cb_bindings,
            vertex_attribs,
            shader_stages,
            color_masks,
            rt_control: maxwell3d.rt_control_info(),
            tex_header_pool_addr: maxwell3d.tex_header_pool_address(),
            tex_header_pool_limit: maxwell3d.tex_header_pool_limit(),
            tex_sampler_pool_addr: maxwell3d.tex_sampler_pool_address(),
            tex_sampler_pool_limit: maxwell3d.tex_sampler_pool_limit(),
            instance_count,
            base_instance: draw_state.base_instance,
            base_vertex: draw_state.base_index as i32,
            inline_index_data: draw_state.inline_index_draw_indexes.clone(),
            sampler_binding: maxwell3d.sampler_binding(),
            render_targets,
        }
    }

    /// Process a method call that may trigger draw operations.
    ///
    /// Corresponds to `DrawManager::ProcessMethodCall`.
    /// Stubbed — full implementation requires access to Maxwell3D registers to decode
    /// MAXWELL3D_REG_INDEX values (clear_surface, draw.begin/end, index_buffer32_*, etc.)
    /// Upstream: DrawManager::ProcessMethodCall() in video_core/engines/draw_manager.cpp
    pub fn process_method_call(
        &mut self,
        method: u32,
        argument: u32,
        maxwell3d: &mut dyn Maxwell3DAccess,
    ) {
        match method {
            CLEAR_SURFACE => self.clear(1, maxwell3d),
            DRAW_BEGIN => self.draw_begin(maxwell3d),
            DRAW_END => self.draw_end(1, false, maxwell3d),
            VB_FIRST | VB_COUNT => {}
            m if m == IB_BASE + IB_OFF_FIRST => {}
            m if m == IB_BASE + IB_OFF_COUNT => {
                self.draw_state.draw_indexed = true;
            }
            INDEX_BUFFER32_SUBSEQUENT | INDEX_BUFFER16_SUBSEQUENT | INDEX_BUFFER8_SUBSEQUENT => {
                self.draw_state.instance_count += 1;
                self.draw_index_small(argument, maxwell3d);
            }
            INDEX_BUFFER32_FIRST | INDEX_BUFFER16_FIRST | INDEX_BUFFER8_FIRST => {
                self.draw_index_small(argument, maxwell3d);
            }
            DRAW_INLINE_INDEX => {
                self.set_inline_index_buffer(argument);
            }
            INLINE_INDEX_2X16_EVEN => {
                let (even, odd) = maxwell3d.inline_index_2x16_values();
                self.set_inline_index_buffer(even);
                self.set_inline_index_buffer(odd);
            }
            INLINE_INDEX_4X8_INDEX0 => {
                for index in maxwell3d.inline_index_4x8_values() {
                    self.set_inline_index_buffer(index);
                }
            }
            VERTEX_ARRAY_INSTANCE_FIRST => {
                let (topology, start, count) =
                    maxwell3d.vertex_array_instance_first_params(argument);
                self.draw_array_instanced(topology, start, count, false, maxwell3d);
            }
            VERTEX_ARRAY_INSTANCE_SUBSEQUENT => {
                let (topology, start, count) =
                    maxwell3d.vertex_array_instance_subsequent_params(argument);
                self.draw_array_instanced(topology, start, count, true, maxwell3d);
            }
            DRAW_TEXTURE_SRC_Y0 => {
                self.draw_texture(maxwell3d);
            }
            TOPOLOGY_OVERRIDE => {}
            _ => {}
        }
    }

    /// Execute a clear operation.
    ///
    /// Corresponds to `DrawManager::Clear`.
    /// Upstream: `maxwell3d->rasterizer->Clear(layer_count)` if ShouldExecute.
    pub fn clear(&mut self, layer_count: u32, maxwell3d: &mut dyn Maxwell3DAccess) {
        if maxwell3d.should_execute() {
            maxwell3d.clear_rasterizer(layer_count);
            self.produce_clear_framebuffer(maxwell3d);
        }
    }

    fn produce_clear_framebuffer(&mut self, maxwell3d: &mut dyn Maxwell3DAccess) {
        let flags = maxwell3d.clear_surface_flags();
        let rt_index = ((flags >> 6) & 0xF) as usize;
        let clear_r = flags & (1 << 2) != 0;
        let clear_g = flags & (1 << 3) != 0;
        let clear_b = flags & (1 << 4) != 0;
        let clear_a = flags & (1 << 5) != 0;

        if !clear_r && !clear_g && !clear_b && !clear_a {
            log::trace!("DrawManager::clear depth/stencil only, skipping local framebuffer");
            return;
        }

        if rt_index >= 8 {
            log::warn!("DrawManager::clear invalid RT index {}", rt_index);
            return;
        }

        let gpu_va = maxwell3d.rt_address(rt_index);
        let width = maxwell3d.rt_width(rt_index);
        let height = maxwell3d.rt_height(rt_index);
        let format = maxwell3d.rt_format(rt_index);

        if width == 0 || height == 0 || gpu_va == 0 {
            log::trace!(
                "DrawManager::clear skipped local framebuffer (width={}, height={}, va=0x{:X})",
                width,
                height,
                gpu_va
            );
            return;
        }

        let color = maxwell3d.clear_color_rgba();
        let pixel = format_clear_color(format, color, clear_r, clear_g, clear_b, clear_a);

        let pixel_count = width as usize * height as usize;
        let mut pixels = vec![0u8; pixel_count * 4];
        for i in 0..pixel_count {
            let off = i * 4;
            pixels[off] = pixel[0];
            pixels[off + 1] = pixel[1];
            pixels[off + 2] = pixel[2];
            pixels[off + 3] = pixel[3];
        }

        maxwell3d.set_pending_framebuffer(Framebuffer {
            gpu_va,
            width,
            height,
            pixels,
        });
    }

    /// Flush any deferred instanced draw calls.
    ///
    /// Corresponds to `DrawManager::DrawDeferred`.
    pub fn draw_deferred(&mut self, maxwell3d: &mut dyn Maxwell3DAccess) {
        if self.draw_state.draw_mode != DrawMode::Instance || self.draw_state.instance_count == 0 {
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
    pub(crate) fn draw_end(
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
    /// Upstream: DrawManager::DrawTexture() in video_core/engines/draw_manager.cpp
    pub fn draw_texture(&mut self, maxwell3d: &mut dyn Maxwell3DAccess) {
        let regs = maxwell3d.draw_texture_params();
        self.draw_texture_state.dst_x0 = regs.dst_x0 as f32 / 4096.0;
        self.draw_texture_state.dst_y0 = regs.dst_y0 as f32 / 4096.0;
        let dst_width = regs.dst_width as f32 / 4096.0;
        let dst_height = regs.dst_height as f32 / 4096.0;
        if maxwell3d.window_origin_lower_left() {
            self.draw_texture_state.dst_y0 =
                maxwell3d.surface_clip_height() as f32 - self.draw_texture_state.dst_y0;
        }
        self.draw_texture_state.dst_x1 = self.draw_texture_state.dst_x0 + dst_width;
        self.draw_texture_state.dst_y1 = self.draw_texture_state.dst_y0 + dst_height;
        self.draw_texture_state.src_x0 = regs.src_x0 as f32 / 4096.0;
        self.draw_texture_state.src_y0 = regs.src_y0 as f32 / 4096.0;
        self.draw_texture_state.src_x1 =
            (regs.dx_du as f32 / 4_294_967_296.0) * dst_width + self.draw_texture_state.src_x0;
        self.draw_texture_state.src_y1 =
            (regs.dy_dv as f32 / 4_294_967_296.0) * dst_height + self.draw_texture_state.src_y0;
        self.draw_texture_state.src_sampler = regs.src_sampler;
        self.draw_texture_state.src_texture = regs.src_texture;
        maxwell3d.draw_texture_rasterizer();
    }

    /// Update topology based on topology control mode and override.
    ///
    /// Corresponds to `DrawManager::UpdateTopology`.
    /// Upstream reads `regs.primitive_topology_control` and `regs.topology_override`.
    fn update_topology(&mut self, maxwell3d: &dyn Maxwell3DAccess) {
        self.draw_state.topology = resolve_draw_topology(
            self.draw_state.topology,
            maxwell3d.primitive_topology_control(),
            maxwell3d.topology_override(),
            maxwell3d.topology_override_raw(),
        );
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

        let draw_call =
            self.build_compat_draw_call(&self.draw_state, draw_indexed, instance_count, maxwell3d);
        self.compat_draw_calls.push(draw_call);

        let should_execute = maxwell3d.should_execute();
        let trace_process_draw = std::env::var_os("RUZU_TRACE_PROCESS_DRAW").is_some();
        if trace_process_draw {
            eprintln!(
                "[PROCESS_DRAW] indexed={} instances={} should_execute={} ib_count={} vb_count={} shader_addrs={:X?}",
                draw_indexed,
                instance_count,
                should_execute,
                self.draw_state.index_buffer.count,
                self.draw_state.vertex_buffer.count,
                maxwell3d.shader_program_addresses(),
            );
        }

        if should_execute {
            // `draw_state.draw_indexed` is already kept in sync with the
            // Maxwell3D register file by the caller chains that lead into
            // `process_draw` (draw_index_small, draw_index_array, etc.).
            // We ensure it matches the argument before handing the state
            // reference to the rasterizer so the rasterizer never sees a
            // stale value.
            self.draw_state.draw_indexed = draw_indexed;
            let rasterizer_present = maxwell3d.draw_rasterizer(&self.draw_state, instance_count);
            if trace_process_draw {
                eprintln!(
                    "[PROCESS_DRAW] rasterizer_present={}",
                    rasterizer_present
                );
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
            maxwell3d.draw_indirect_rasterizer(&self.draw_state, &self.indirect_state);
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

    #[test]
    fn resolve_draw_topology_uses_separate_state_override() {
        let resolved = resolve_draw_topology(
            PrimitiveTopology::TriangleStrip,
            PrimitiveTopologyControl::UseSeparateState,
            PrimitiveTopologyOverride::Lines,
            PrimitiveTopologyOverride::Lines as u32,
        );
        assert_eq!(resolved, PrimitiveTopology::Lines);
    }

    #[test]
    fn maxwell_draw_view_exposes_draw_boundary_state() {
        let draw_state = DrawState::default();
        let mut registers = Maxwell3DDrawRegisters::default();
        registers.shader_program_addresses = [0x1000, 0x2000, 0, 0, 0, 0];
        registers.descriptor_sync_regs.tex_header_addr = 0x3000;
        registers.window_origin_lower_left = true;
        registers.window_origin_flip_y = true;
        registers.viewport0_scale_y = -1.0;
        registers.dirty_flags[crate::renderer_opengl::gl_state_tracker::dirty::VIEWPORTS as usize] =
            true;

        let view = Maxwell3DDrawView::with_register_snapshot(&draw_state, registers);

        assert!(std::ptr::eq(view.draw_state(), &draw_state));
        assert_eq!(view.shader_program_addresses(), [0x1000, 0x2000, 0, 0, 0, 0]);
        assert_eq!(view.descriptor_sync_regs().tex_header_addr, 0x3000);
        assert!(view.window_origin_lower_left());
        assert!(view.window_origin_flip_y());
        assert_eq!(view.viewport0_scale_y(), -1.0);
        assert!(
            view.dirty_flags()[crate::renderer_opengl::gl_state_tracker::dirty::VIEWPORTS as usize]
        );
    }

    #[test]
    fn maxwell_draw_view_exposes_raw_viewport_register_snapshot() {
        let draw_state = DrawState::default();
        let mut registers = Maxwell3DDrawRegisters::default();
        registers.viewport_transforms[0] =
            crate::engines::maxwell_3d::ViewportTransformInfo {
                scale_x: 10.0,
                scale_y: -20.0,
                scale_z: 0.25,
                translate_x: 30.0,
                translate_y: 40.0,
                translate_z: 0.5,
                swizzle: 0x3210,
                snap_grid_precision: 0x0504,
            };
        registers.viewport_scale_offset_enabled = true;
        registers.surface_clip = crate::engines::maxwell_3d::SurfaceClipInfo {
            x: 1,
            y: 2,
            width: 1280,
            height: 720,
        };
        registers.depth_mode = crate::engines::maxwell_3d::DepthMode::MinusOneToOne;

        let view = Maxwell3DDrawView::with_register_snapshot(&draw_state, registers);

        assert!(view.viewport_scale_offset_enabled());
        assert_eq!(view.viewport_transforms()[0].scale_y, -20.0);
        assert_eq!(view.viewport_transforms()[0].swizzle, 0x3210);
        assert_eq!(view.surface_clip().width, 1280);
        assert_eq!(
            view.depth_mode(),
            crate::engines::maxwell_3d::DepthMode::MinusOneToOne
        );
    }
}
