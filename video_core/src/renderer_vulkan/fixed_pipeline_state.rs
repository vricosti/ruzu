// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `fixed_pipeline_state.h` / `fixed_pipeline_state.cpp`.
//!
//! Hashable, bit-packed representation of non-dynamic graphics pipeline state.
//! Used as a key in the graphics pipeline cache to avoid re-creating VkPipelines.

use std::hash::{Hash, Hasher};

use crate::engines::maxwell_3d::{
    BlendEquation, BlendFactor, BlendInfo, ComparisonOp, CullFace, DrawCall, FrontFace,
    PolygonMode, PrimitiveTopology, StencilOp,
};

// ---------------------------------------------------------------------------
// Constants — port of anonymous namespace in fixed_pipeline_state.cpp
// ---------------------------------------------------------------------------

const POINT: usize = 0;
const LINE: usize = 1;
const POLYGON: usize = 2;

/// Lookup table mapping `PrimitiveTopology` to polygon offset mode index.
///
/// Port of `POLYGON_OFFSET_ENABLE_LUT` from `fixed_pipeline_state.cpp`.
const POLYGON_OFFSET_ENABLE_LUT: [usize; 15] = [
    POINT,   // Points
    LINE,    // Lines
    LINE,    // LineLoop
    LINE,    // LineStrip
    POLYGON, // Triangles
    POLYGON, // TriangleStrip
    POLYGON, // TriangleFan
    POLYGON, // Quads
    POLYGON, // QuadStrip
    POLYGON, // Polygon
    LINE,    // LinesAdjacency
    LINE,    // LineStripAdjacency
    POLYGON, // TrianglesAdjacency
    POLYGON, // TriangleStripAdjacency
    POLYGON, // Patches
];

/// Number of render targets.
const NUM_RENDER_TARGETS: usize = 8;

/// Number of vertex attributes.
const NUM_VERTEX_ATTRIBUTES: usize = 32;

/// Number of vertex arrays/streams.
const NUM_VERTEX_ARRAYS: usize = 32;

/// Number of viewports.
const NUM_VIEWPORTS: usize = 16;

// ---------------------------------------------------------------------------
// DynamicFeatures — port of DynamicFeatures struct
// ---------------------------------------------------------------------------

/// Dynamic state feature flags from the Vulkan device.
///
/// Port of `DynamicFeatures` struct from `fixed_pipeline_state.h`.
#[derive(Debug, Clone, Copy, Default)]
pub struct DynamicFeatures {
    pub has_extended_dynamic_state: bool,
    pub has_extended_dynamic_state_2: bool,
    pub has_extended_dynamic_state_2_extra: bool,
    pub has_extended_dynamic_state_3_blend: bool,
    pub has_extended_dynamic_state_3_enables: bool,
    pub has_dynamic_vertex_input: bool,
}

// ---------------------------------------------------------------------------
// Pack/Unpack functions — port of FixedPipelineState static methods
// ---------------------------------------------------------------------------

/// Port of `FixedPipelineState::PackComparisonOp`.
///
/// OpenGL enums go from 0x200 to 0x207 and the D3D ones from 1 to 8.
/// Subtracting 0x200 from GL enums and 1 from D3D gives a 0-7 range.
pub fn pack_comparison_op(op: ComparisonOp) -> u32 {
    // Our Rust enum already normalizes D3D/GL, so just use ordinal
    op as u32
}

/// Port of `FixedPipelineState::UnpackComparisonOp`.
pub fn unpack_comparison_op(packed: u32) -> ComparisonOp {
    match packed {
        0 => ComparisonOp::Never,
        1 => ComparisonOp::Less,
        2 => ComparisonOp::Equal,
        3 => ComparisonOp::LessEqual,
        4 => ComparisonOp::Greater,
        5 => ComparisonOp::NotEqual,
        6 => ComparisonOp::GreaterEqual,
        7 => ComparisonOp::Always,
        _ => ComparisonOp::Always,
    }
}

/// Port of `FixedPipelineState::PackStencilOp`.
pub fn pack_stencil_op(op: StencilOp) -> u32 {
    match op {
        StencilOp::Keep => 0,
        StencilOp::Zero => 1,
        StencilOp::Replace => 2,
        StencilOp::IncrSat => 3,
        StencilOp::DecrSat => 4,
        StencilOp::Invert => 5,
        StencilOp::Incr => 6,
        StencilOp::Decr => 7,
    }
}

/// Port of `FixedPipelineState::UnpackStencilOp`.
pub fn unpack_stencil_op(packed: u32) -> StencilOp {
    const LUT: [StencilOp; 8] = [
        StencilOp::Keep,
        StencilOp::Zero,
        StencilOp::Replace,
        StencilOp::IncrSat,
        StencilOp::DecrSat,
        StencilOp::Invert,
        StencilOp::Incr,
        StencilOp::Decr,
    ];
    LUT[packed as usize % LUT.len()]
}

/// Port of `FixedPipelineState::PackCullFace`.
///
/// FrontAndBack is 0x408, Front is 0x404, Back is 0x405.
pub fn pack_cull_face(cull: CullFace) -> u32 {
    match cull {
        CullFace::Front => 0,
        CullFace::Back => 1,
        CullFace::FrontAndBack => 2,
    }
}

/// Port of `FixedPipelineState::UnpackCullFace`.
pub fn unpack_cull_face(packed: u32) -> CullFace {
    const LUT: [CullFace; 3] = [CullFace::Front, CullFace::Back, CullFace::FrontAndBack];
    LUT[packed as usize % LUT.len()]
}

/// Port of `FixedPipelineState::PackFrontFace`.
pub fn pack_front_face(face: FrontFace) -> u32 {
    match face {
        FrontFace::CW => 0,
        FrontFace::CCW => 1,
    }
}

/// Port of `FixedPipelineState::UnpackFrontFace`.
pub fn unpack_front_face(packed: u32) -> FrontFace {
    if packed == 0 {
        FrontFace::CW
    } else {
        FrontFace::CCW
    }
}

/// Port of `FixedPipelineState::PackPolygonMode`.
pub fn pack_polygon_mode(mode: PolygonMode) -> u32 {
    match mode {
        PolygonMode::Point => 0,
        PolygonMode::Line => 1,
        PolygonMode::Fill => 2,
    }
}

/// Port of `FixedPipelineState::UnpackPolygonMode`.
pub fn unpack_polygon_mode(packed: u32) -> PolygonMode {
    match packed {
        0 => PolygonMode::Point,
        1 => PolygonMode::Line,
        _ => PolygonMode::Fill,
    }
}

/// Port of `FixedPipelineState::PackLogicOp`.
///
/// Logic ops are GL-encoded starting at 0x1500.
pub fn pack_logic_op(op: u32) -> u32 {
    if op >= 0x1500 {
        op - 0x1500
    } else {
        op
    }
}

/// Port of `FixedPipelineState::UnpackLogicOp`.
pub fn unpack_logic_op(packed: u32) -> u32 {
    packed + 0x1500
}

/// Port of `FixedPipelineState::PackBlendEquation`.
pub fn pack_blend_equation(eq: BlendEquation) -> u32 {
    match eq {
        BlendEquation::Add => 0,
        BlendEquation::Subtract => 1,
        BlendEquation::ReverseSubtract => 2,
        BlendEquation::Min => 3,
        BlendEquation::Max => 4,
    }
}

/// Port of `FixedPipelineState::UnpackBlendEquation`.
pub fn unpack_blend_equation(packed: u32) -> BlendEquation {
    const LUT: [BlendEquation; 5] = [
        BlendEquation::Add,
        BlendEquation::Subtract,
        BlendEquation::ReverseSubtract,
        BlendEquation::Min,
        BlendEquation::Max,
    ];
    LUT[packed as usize % LUT.len()]
}

/// Port of `FixedPipelineState::PackBlendFactor`.
pub fn pack_blend_factor(factor: BlendFactor) -> u32 {
    match factor {
        BlendFactor::Zero => 0,
        BlendFactor::One => 1,
        BlendFactor::SrcColor => 2,
        BlendFactor::OneMinusSrcColor => 3,
        BlendFactor::SrcAlpha => 4,
        BlendFactor::OneMinusSrcAlpha => 5,
        BlendFactor::DstAlpha => 6,
        BlendFactor::OneMinusDstAlpha => 7,
        BlendFactor::DstColor => 8,
        BlendFactor::OneMinusDstColor => 9,
        BlendFactor::SrcAlphaSaturate => 10,
        BlendFactor::Src1Color => 11,
        BlendFactor::OneMinusSrc1Color => 12,
        BlendFactor::Src1Alpha => 13,
        BlendFactor::OneMinusSrc1Alpha => 14,
        BlendFactor::ConstantColor => 15,
        BlendFactor::OneMinusConstantColor => 16,
        BlendFactor::ConstantAlpha => 17,
        BlendFactor::OneMinusConstantAlpha => 18,
    }
}

/// Port of `FixedPipelineState::UnpackBlendFactor`.
pub fn unpack_blend_factor(packed: u32) -> BlendFactor {
    const LUT: [BlendFactor; 19] = [
        BlendFactor::Zero,
        BlendFactor::One,
        BlendFactor::SrcColor,
        BlendFactor::OneMinusSrcColor,
        BlendFactor::SrcAlpha,
        BlendFactor::OneMinusSrcAlpha,
        BlendFactor::DstAlpha,
        BlendFactor::OneMinusDstAlpha,
        BlendFactor::DstColor,
        BlendFactor::OneMinusDstColor,
        BlendFactor::SrcAlphaSaturate,
        BlendFactor::Src1Color,
        BlendFactor::OneMinusSrc1Color,
        BlendFactor::Src1Alpha,
        BlendFactor::OneMinusSrc1Alpha,
        BlendFactor::ConstantColor,
        BlendFactor::OneMinusConstantColor,
        BlendFactor::ConstantAlpha,
        BlendFactor::OneMinusConstantAlpha,
    ];
    debug_assert!((packed as usize) < LUT.len());
    LUT[packed as usize % LUT.len()]
}

// ---------------------------------------------------------------------------
// BlendingAttachment — port of FixedPipelineState::BlendingAttachment
// ---------------------------------------------------------------------------

/// Bit-packed blend state for a single render target attachment.
///
/// Port of `FixedPipelineState::BlendingAttachment`.
///
/// Bit layout (matches upstream):
/// - bits  0..0  : mask_r
/// - bits  1..1  : mask_g
/// - bits  2..2  : mask_b
/// - bits  3..3  : mask_a
/// - bits  4..6  : equation_rgb (3 bits)
/// - bits  7..9  : equation_a   (3 bits)
/// - bits 10..14 : factor_source_rgb (5 bits)
/// - bits 15..19 : factor_dest_rgb   (5 bits)
/// - bits 20..24 : factor_source_a   (5 bits)
/// - bits 25..29 : factor_dest_a     (5 bits)
/// - bit  30     : enable
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BlendingAttachment {
    pub raw: u32,
}

impl Default for BlendingAttachment {
    fn default() -> Self {
        Self { raw: 0 }
    }
}

impl BlendingAttachment {
    /// Port of `BlendingAttachment::Mask`.
    pub fn mask(&self) -> [bool; 4] {
        [
            (self.raw & (1 << 0)) != 0,
            (self.raw & (1 << 1)) != 0,
            (self.raw & (1 << 2)) != 0,
            (self.raw & (1 << 3)) != 0,
        ]
    }

    /// Port of `BlendingAttachment::EquationRGB`.
    pub fn equation_rgb(&self) -> BlendEquation {
        unpack_blend_equation((self.raw >> 4) & 0x7)
    }

    /// Port of `BlendingAttachment::EquationAlpha`.
    pub fn equation_alpha(&self) -> BlendEquation {
        unpack_blend_equation((self.raw >> 7) & 0x7)
    }

    /// Port of `BlendingAttachment::SourceRGBFactor`.
    pub fn source_rgb_factor(&self) -> BlendFactor {
        unpack_blend_factor((self.raw >> 10) & 0x1F)
    }

    /// Port of `BlendingAttachment::DestRGBFactor`.
    pub fn dest_rgb_factor(&self) -> BlendFactor {
        unpack_blend_factor((self.raw >> 15) & 0x1F)
    }

    /// Port of `BlendingAttachment::SourceAlphaFactor`.
    pub fn source_alpha_factor(&self) -> BlendFactor {
        unpack_blend_factor((self.raw >> 20) & 0x1F)
    }

    /// Port of `BlendingAttachment::DestAlphaFactor`.
    pub fn dest_alpha_factor(&self) -> BlendFactor {
        unpack_blend_factor((self.raw >> 25) & 0x1F)
    }

    /// Whether blending is enabled for this attachment.
    pub fn is_enabled(&self) -> bool {
        (self.raw & (1 << 30)) != 0
    }

    /// Set the mask values.
    pub fn set_mask(&mut self, r: bool, g: bool, b: bool, a: bool) {
        self.raw = (self.raw & !0xF)
            | ((r as u32) << 0)
            | ((g as u32) << 1)
            | ((b as u32) << 2)
            | ((a as u32) << 3);
    }

    /// Set equation RGB (3 bits at position 4).
    pub fn set_equation_rgb(&mut self, eq: BlendEquation) {
        let v = pack_blend_equation(eq);
        self.raw = (self.raw & !(0x7 << 4)) | ((v & 0x7) << 4);
    }

    /// Set equation Alpha (3 bits at position 7).
    pub fn set_equation_alpha(&mut self, eq: BlendEquation) {
        let v = pack_blend_equation(eq);
        self.raw = (self.raw & !(0x7 << 7)) | ((v & 0x7) << 7);
    }

    /// Set source RGB factor (5 bits at position 10).
    pub fn set_source_rgb_factor(&mut self, f: BlendFactor) {
        let v = pack_blend_factor(f);
        self.raw = (self.raw & !(0x1F << 10)) | ((v & 0x1F) << 10);
    }

    /// Set dest RGB factor (5 bits at position 15).
    pub fn set_dest_rgb_factor(&mut self, f: BlendFactor) {
        let v = pack_blend_factor(f);
        self.raw = (self.raw & !(0x1F << 15)) | ((v & 0x1F) << 15);
    }

    /// Set source alpha factor (5 bits at position 20).
    pub fn set_source_alpha_factor(&mut self, f: BlendFactor) {
        let v = pack_blend_factor(f);
        self.raw = (self.raw & !(0x1F << 20)) | ((v & 0x1F) << 20);
    }

    /// Set dest alpha factor (5 bits at position 25).
    pub fn set_dest_alpha_factor(&mut self, f: BlendFactor) {
        let v = pack_blend_factor(f);
        self.raw = (self.raw & !(0x1F << 25)) | ((v & 0x1F) << 25);
    }

    /// Set enable bit (bit 30).
    pub fn set_enabled(&mut self, enabled: bool) {
        if enabled {
            self.raw |= 1 << 30;
        } else {
            self.raw &= !(1 << 30);
        }
    }
}

// ---------------------------------------------------------------------------
// VertexAttribute — port of FixedPipelineState::VertexAttribute
// ---------------------------------------------------------------------------

/// Bit-packed vertex attribute descriptor.
///
/// Port of `FixedPipelineState::VertexAttribute`.
///
/// Bit layout:
/// - bit  0      : enabled
/// - bits 1..5   : buffer (5 bits)
/// - bits 6..19  : offset (14 bits)
/// - bits 20..22 : type   (3 bits)
/// - bits 23..28 : size   (6 bits)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct VertexAttribute {
    pub raw: u32,
}

impl Default for VertexAttribute {
    fn default() -> Self {
        Self { raw: 0 }
    }
}

impl VertexAttribute {
    /// Whether this attribute is enabled.
    pub fn is_enabled(&self) -> bool {
        (self.raw & 1) != 0
    }

    /// Buffer index (5 bits).
    pub fn buffer(&self) -> u32 {
        (self.raw >> 1) & 0x1F
    }

    /// Offset within buffer (14 bits).
    pub fn offset(&self) -> u32 {
        (self.raw >> 6) & 0x3FFF
    }

    /// Attribute type (3 bits) — maps to `Maxwell::VertexAttribute::Type`.
    pub fn attrib_type(&self) -> u32 {
        (self.raw >> 20) & 0x7
    }

    /// Attribute size (6 bits) — maps to `Maxwell::VertexAttribute::Size`.
    pub fn attrib_size(&self) -> u32 {
        (self.raw >> 23) & 0x3F
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.raw = (self.raw & !1) | (enabled as u32);
    }

    pub fn set_buffer(&mut self, buffer: u32) {
        self.raw = (self.raw & !(0x1F << 1)) | ((buffer & 0x1F) << 1);
    }

    pub fn set_offset(&mut self, offset: u32) {
        self.raw = (self.raw & !(0x3FFF << 6)) | ((offset & 0x3FFF) << 6);
    }

    pub fn set_type(&mut self, ty: u32) {
        self.raw = (self.raw & !(0x7 << 20)) | ((ty & 0x7) << 20);
    }

    pub fn set_size(&mut self, size: u32) {
        self.raw = (self.raw & !(0x3F << 23)) | ((size & 0x3F) << 23);
    }
}

// ---------------------------------------------------------------------------
// StencilFace — port of FixedPipelineState::StencilFace<Position>
// ---------------------------------------------------------------------------

/// Packed stencil face operations within a u32, at a given bit position.
///
/// Port of `FixedPipelineState::StencilFace<Position>`.
///
/// Layout (relative to position):
/// - bits 0..2  : action_stencil_fail (3 bits)
/// - bits 3..5  : action_depth_fail   (3 bits)
/// - bits 6..8  : action_depth_pass   (3 bits)
/// - bits 9..11 : test_func           (3 bits)
#[derive(Debug, Clone, Copy)]
pub struct StencilFace {
    pub position: u32,
}

impl StencilFace {
    /// Extract action_stencil_fail from the packed u32.
    pub fn action_stencil_fail(&self, raw: u32) -> StencilOp {
        unpack_stencil_op((raw >> self.position) & 0x7)
    }

    /// Extract action_depth_fail.
    pub fn action_depth_fail(&self, raw: u32) -> StencilOp {
        unpack_stencil_op((raw >> (self.position + 3)) & 0x7)
    }

    /// Extract action_depth_pass.
    pub fn action_depth_pass(&self, raw: u32) -> StencilOp {
        unpack_stencil_op((raw >> (self.position + 6)) & 0x7)
    }

    /// Extract test_func.
    pub fn test_func(&self, raw: u32) -> ComparisonOp {
        unpack_comparison_op((raw >> (self.position + 9)) & 0x7)
    }
}

/// Front stencil face (position 0 within raw2).
pub const STENCIL_FRONT: StencilFace = StencilFace { position: 0 };
/// Back stencil face (position 12 within raw2).
pub const STENCIL_BACK: StencilFace = StencilFace { position: 12 };

// ---------------------------------------------------------------------------
// DynamicState — port of FixedPipelineState::DynamicState
// ---------------------------------------------------------------------------

/// Bit-packed dynamic pipeline state.
///
/// Port of `FixedPipelineState::DynamicState`.
///
/// raw1 layout:
/// - bits 0..1   : cull_face (2 bits)
/// - bit  2      : cull_enable
/// - bit  3      : primitive_restart_enable
/// - bit  4      : depth_bias_enable
/// - bit  5      : rasterize_enable
/// - bits 6..9   : logic_op (4 bits)
/// - bit  10     : logic_op_enable
/// - bit  11     : depth_clamp_disabled
///
/// raw2 layout:
/// - bits 0..11  : front stencil face (12 bits)
/// - bits 12..23 : back stencil face  (12 bits)
/// - bit  24     : stencil_enable
/// - bit  25     : depth_write_enable
/// - bit  26     : depth_bounds_enable
/// - bit  27     : depth_test_enable
/// - bit  28     : front_face (1 bit)
/// - bits 29..31 : depth_test_func (3 bits)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DynamicState {
    pub raw1: u32,
    pub raw2: u32,
}

impl Default for DynamicState {
    fn default() -> Self {
        Self { raw1: 0, raw2: 0 }
    }
}

impl DynamicState {
    // --- raw1 field accessors ---

    pub fn cull_face(&self) -> CullFace {
        unpack_cull_face(self.raw1 & 0x3)
    }

    pub fn cull_enable(&self) -> bool {
        (self.raw1 & (1 << 2)) != 0
    }

    pub fn primitive_restart_enable(&self) -> bool {
        (self.raw1 & (1 << 3)) != 0
    }

    pub fn depth_bias_enable(&self) -> bool {
        (self.raw1 & (1 << 4)) != 0
    }

    pub fn rasterize_enable(&self) -> bool {
        (self.raw1 & (1 << 5)) != 0
    }

    pub fn logic_op(&self) -> u32 {
        unpack_logic_op((self.raw1 >> 6) & 0xF)
    }

    pub fn logic_op_enable(&self) -> bool {
        (self.raw1 & (1 << 10)) != 0
    }

    pub fn depth_clamp_disabled(&self) -> bool {
        (self.raw1 & (1 << 11)) != 0
    }

    // --- raw1 field setters ---

    pub fn set_cull_face(&mut self, cull: CullFace) {
        let v = pack_cull_face(cull);
        self.raw1 = (self.raw1 & !0x3) | (v & 0x3);
    }

    pub fn set_cull_enable(&mut self, enable: bool) {
        if enable {
            self.raw1 |= 1 << 2;
        } else {
            self.raw1 &= !(1 << 2);
        }
    }

    pub fn set_primitive_restart_enable(&mut self, enable: bool) {
        if enable {
            self.raw1 |= 1 << 3;
        } else {
            self.raw1 &= !(1 << 3);
        }
    }

    pub fn set_depth_bias_enable(&mut self, enable: bool) {
        if enable {
            self.raw1 |= 1 << 4;
        } else {
            self.raw1 &= !(1 << 4);
        }
    }

    pub fn set_rasterize_enable(&mut self, enable: bool) {
        if enable {
            self.raw1 |= 1 << 5;
        } else {
            self.raw1 &= !(1 << 5);
        }
    }

    pub fn set_logic_op(&mut self, op: u32) {
        let v = pack_logic_op(op);
        self.raw1 = (self.raw1 & !(0xF << 6)) | ((v & 0xF) << 6);
    }

    pub fn set_logic_op_enable(&mut self, enable: bool) {
        if enable {
            self.raw1 |= 1 << 10;
        } else {
            self.raw1 &= !(1 << 10);
        }
    }

    pub fn set_depth_clamp_disabled(&mut self, disabled: bool) {
        if disabled {
            self.raw1 |= 1 << 11;
        } else {
            self.raw1 &= !(1 << 11);
        }
    }

    // --- raw2 field accessors ---

    pub fn stencil_enable(&self) -> bool {
        (self.raw2 & (1 << 24)) != 0
    }

    pub fn depth_write_enable(&self) -> bool {
        (self.raw2 & (1 << 25)) != 0
    }

    pub fn depth_bounds_enable(&self) -> bool {
        (self.raw2 & (1 << 26)) != 0
    }

    pub fn depth_test_enable(&self) -> bool {
        (self.raw2 & (1 << 27)) != 0
    }

    pub fn front_face(&self) -> FrontFace {
        unpack_front_face((self.raw2 >> 28) & 0x1)
    }

    pub fn depth_test_func(&self) -> ComparisonOp {
        unpack_comparison_op((self.raw2 >> 29) & 0x7)
    }

    /// Get front stencil face operations from raw2.
    pub fn front_stencil(&self) -> &StencilFace {
        &STENCIL_FRONT
    }

    /// Get back stencil face operations from raw2.
    pub fn back_stencil(&self) -> &StencilFace {
        &STENCIL_BACK
    }

    // --- raw2 field setters ---

    pub fn set_stencil_enable(&mut self, enable: bool) {
        if enable {
            self.raw2 |= 1 << 24;
        } else {
            self.raw2 &= !(1 << 24);
        }
    }

    pub fn set_depth_write_enable(&mut self, enable: bool) {
        if enable {
            self.raw2 |= 1 << 25;
        } else {
            self.raw2 &= !(1 << 25);
        }
    }

    pub fn set_depth_bounds_enable(&mut self, enable: bool) {
        if enable {
            self.raw2 |= 1 << 26;
        } else {
            self.raw2 &= !(1 << 26);
        }
    }

    pub fn set_depth_test_enable(&mut self, enable: bool) {
        if enable {
            self.raw2 |= 1 << 27;
        } else {
            self.raw2 &= !(1 << 27);
        }
    }

    pub fn set_front_face(&mut self, face: FrontFace) {
        let v = pack_front_face(face);
        self.raw2 = (self.raw2 & !(1 << 28)) | ((v & 0x1) << 28);
    }

    pub fn set_depth_test_func(&mut self, func: ComparisonOp) {
        let v = pack_comparison_op(func);
        self.raw2 = (self.raw2 & !(0x7 << 29)) | ((v & 0x7) << 29);
    }

    /// Set a stencil face field (12 bits) at the given position.
    pub fn set_stencil_face(
        &mut self,
        position: u32,
        stencil_fail: StencilOp,
        depth_fail: StencilOp,
        depth_pass: StencilOp,
        test_func: ComparisonOp,
    ) {
        let packed = (pack_stencil_op(stencil_fail) & 0x7)
            | ((pack_stencil_op(depth_fail) & 0x7) << 3)
            | ((pack_stencil_op(depth_pass) & 0x7) << 6)
            | ((pack_comparison_op(test_func) & 0x7) << 9);
        let mask = 0xFFF << position;
        self.raw2 = (self.raw2 & !mask) | ((packed << position) & mask);
    }
}

// ---------------------------------------------------------------------------
// FixedPipelineState — port of the main struct
// ---------------------------------------------------------------------------

/// Hashable representation of all non-dynamic graphics pipeline state.
///
/// Port of `FixedPipelineState` from `fixed_pipeline_state.h`.
///
/// The upstream struct uses anonymous unions with bitfields for compact hashing.
/// We replicate the same bit layout using explicit raw u32 fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FixedPipelineState {
    /// Packed flags word 1: dynamic state features, topology, polygon mode, etc.
    ///
    /// Bit layout (matches upstream raw1):
    /// - bit  0     : extended_dynamic_state
    /// - bit  1     : extended_dynamic_state_2
    /// - bit  2     : extended_dynamic_state_2_extra
    /// - bit  3     : extended_dynamic_state_3_blend
    /// - bit  4     : extended_dynamic_state_3_enables
    /// - bit  5     : dynamic_vertex_input
    /// - bit  6     : xfb_enabled
    /// - bit  7     : ndc_minus_one_to_one
    /// - bits 8..9  : polygon_mode (2 bits)
    /// - bits 10..11: tessellation_primitive (2 bits)
    /// - bits 12..13: tessellation_spacing (2 bits)
    /// - bit  14    : tessellation_clockwise
    /// - bits 15..19: patch_control_points_minus_one (5 bits)
    /// - bits 24..27: topology (4 bits)
    /// - bits 28..31: msaa_mode (4 bits)
    pub raw1: u32,

    /// Packed flags word 2: alpha test, depth format, etc.
    ///
    /// Bit layout (matches upstream raw2):
    /// - bits 1..3  : alpha_test_func (3 bits)
    /// - bit  4     : early_z
    /// - bit  5     : depth_enabled
    /// - bits 6..10 : depth_format (5 bits)
    /// - bit  11    : y_negate
    /// - bit  12    : provoking_vertex_last
    /// - bit  13    : conservative_raster_enable
    /// - bit  14    : smooth_lines
    /// - bit  15    : alpha_to_coverage_enabled
    /// - bit  16    : alpha_to_one_enabled
    /// - bits 17..19: app_stage (3 bits)
    pub raw2: u32,

    pub color_formats: [u8; NUM_RENDER_TARGETS],

    pub alpha_test_ref: u32,
    pub point_size: u32,
    pub viewport_swizzles: [u16; NUM_VIEWPORTS],

    /// Used with VK_EXT_vertex_input_dynamic_state as attribute_types,
    /// or as enabled_divisors otherwise (overlapping union).
    pub attribute_types_or_enabled_divisors: u64,

    pub dynamic_state: DynamicState,
    pub attachments: [BlendingAttachment; NUM_RENDER_TARGETS],
    pub attributes: [VertexAttribute; NUM_VERTEX_ATTRIBUTES],
    pub binding_divisors: [u32; NUM_VERTEX_ARRAYS],
    /// Vertex stride is a 12-bit value, we have 4 bits to spare per element.
    pub vertex_strides: [u16; NUM_VERTEX_ARRAYS],
    // TransformFeedbackState xfb_state omitted for now (large struct, rarely used)
}

impl Default for FixedPipelineState {
    fn default() -> Self {
        Self {
            raw1: 0,
            raw2: 0,
            color_formats: [0; NUM_RENDER_TARGETS],
            alpha_test_ref: 0,
            point_size: 0,
            viewport_swizzles: [0; NUM_VIEWPORTS],
            attribute_types_or_enabled_divisors: 0,
            dynamic_state: DynamicState::default(),
            attachments: [BlendingAttachment::default(); NUM_RENDER_TARGETS],
            attributes: [VertexAttribute::default(); NUM_VERTEX_ARRAYS],
            binding_divisors: [0; NUM_VERTEX_ARRAYS],
            vertex_strides: [0; NUM_VERTEX_ARRAYS],
        }
    }
}

impl Hash for FixedPipelineState {
    /// Port of `FixedPipelineState::Hash`.
    ///
    /// Upstream uses CityHash64 over a byte range whose size depends on
    /// dynamic state features. We hash the same fields for parity.
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw1.hash(state);
        self.raw2.hash(state);
        self.color_formats.hash(state);
        self.alpha_test_ref.hash(state);
        self.point_size.hash(state);
        self.viewport_swizzles.hash(state);
        self.attribute_types_or_enabled_divisors.hash(state);

        // Only include dynamic/vertex state if not managed by dynamic state extensions
        if !self.extended_dynamic_state() || !self.dynamic_vertex_input() {
            self.dynamic_state.hash(state);
            self.attachments.hash(state);
            self.attributes.hash(state);
            self.binding_divisors.hash(state);
            self.vertex_strides.hash(state);
        }
    }
}

impl FixedPipelineState {
    // --- raw1 accessors ---

    pub fn extended_dynamic_state(&self) -> bool {
        (self.raw1 & (1 << 0)) != 0
    }

    pub fn extended_dynamic_state_2(&self) -> bool {
        (self.raw1 & (1 << 1)) != 0
    }

    pub fn extended_dynamic_state_2_extra(&self) -> bool {
        (self.raw1 & (1 << 2)) != 0
    }

    pub fn extended_dynamic_state_3_blend(&self) -> bool {
        (self.raw1 & (1 << 3)) != 0
    }

    pub fn extended_dynamic_state_3_enables(&self) -> bool {
        (self.raw1 & (1 << 4)) != 0
    }

    pub fn dynamic_vertex_input(&self) -> bool {
        (self.raw1 & (1 << 5)) != 0
    }

    pub fn xfb_enabled(&self) -> bool {
        (self.raw1 & (1 << 6)) != 0
    }

    pub fn ndc_minus_one_to_one(&self) -> bool {
        (self.raw1 & (1 << 7)) != 0
    }

    pub fn polygon_mode(&self) -> PolygonMode {
        unpack_polygon_mode((self.raw1 >> 8) & 0x3)
    }

    pub fn topology(&self) -> PrimitiveTopology {
        PrimitiveTopology::from_raw(((self.raw1 >> 24) & 0xF) as u32)
    }

    pub fn patch_control_points_minus_one(&self) -> u32 {
        (self.raw1 >> 15) & 0x1F
    }

    pub fn patch_control_points(&self) -> u32 {
        self.patch_control_points_minus_one() + 1
    }

    // --- raw1 setters ---

    pub fn set_extended_dynamic_state(&mut self, v: bool) {
        self.set_bit_raw1(0, v);
    }
    pub fn set_extended_dynamic_state_2(&mut self, v: bool) {
        self.set_bit_raw1(1, v);
    }
    pub fn set_extended_dynamic_state_2_extra(&mut self, v: bool) {
        self.set_bit_raw1(2, v);
    }
    pub fn set_extended_dynamic_state_3_blend(&mut self, v: bool) {
        self.set_bit_raw1(3, v);
    }
    pub fn set_extended_dynamic_state_3_enables(&mut self, v: bool) {
        self.set_bit_raw1(4, v);
    }
    pub fn set_dynamic_vertex_input(&mut self, v: bool) {
        self.set_bit_raw1(5, v);
    }
    pub fn set_xfb_enabled(&mut self, v: bool) {
        self.set_bit_raw1(6, v);
    }
    pub fn set_ndc_minus_one_to_one(&mut self, v: bool) {
        self.set_bit_raw1(7, v);
    }

    pub fn set_polygon_mode(&mut self, mode: PolygonMode) {
        let v = pack_polygon_mode(mode);
        self.raw1 = (self.raw1 & !(0x3 << 8)) | ((v & 0x3) << 8);
    }

    pub fn set_topology(&mut self, topology: PrimitiveTopology) {
        let v = topology as u32;
        self.raw1 = (self.raw1 & !(0xF << 24)) | ((v & 0xF) << 24);
    }

    pub fn set_patch_control_points_minus_one(&mut self, v: u32) {
        self.raw1 = (self.raw1 & !(0x1F << 15)) | ((v & 0x1F) << 15);
    }

    fn set_bit_raw1(&mut self, bit: u32, v: bool) {
        if v {
            self.raw1 |= 1 << bit;
        } else {
            self.raw1 &= !(1 << bit);
        }
    }

    // --- raw2 accessors ---

    pub fn alpha_test_func(&self) -> ComparisonOp {
        unpack_comparison_op((self.raw2 >> 1) & 0x7)
    }

    pub fn early_z(&self) -> bool {
        (self.raw2 & (1 << 4)) != 0
    }

    pub fn depth_enabled(&self) -> bool {
        (self.raw2 & (1 << 5)) != 0
    }

    pub fn depth_format(&self) -> u32 {
        (self.raw2 >> 6) & 0x1F
    }

    pub fn y_negate(&self) -> bool {
        (self.raw2 & (1 << 11)) != 0
    }

    pub fn provoking_vertex_last(&self) -> bool {
        (self.raw2 & (1 << 12)) != 0
    }

    pub fn conservative_raster_enable(&self) -> bool {
        (self.raw2 & (1 << 13)) != 0
    }

    pub fn smooth_lines(&self) -> bool {
        (self.raw2 & (1 << 14)) != 0
    }

    pub fn alpha_to_coverage_enabled(&self) -> bool {
        (self.raw2 & (1 << 15)) != 0
    }

    pub fn alpha_to_one_enabled(&self) -> bool {
        (self.raw2 & (1 << 16)) != 0
    }

    // --- raw2 setters ---

    pub fn set_alpha_test_func(&mut self, func: ComparisonOp) {
        let v = pack_comparison_op(func);
        self.raw2 = (self.raw2 & !(0x7 << 1)) | ((v & 0x7) << 1);
    }

    pub fn set_early_z(&mut self, v: bool) {
        self.set_bit_raw2(4, v);
    }
    pub fn set_depth_enabled(&mut self, v: bool) {
        self.set_bit_raw2(5, v);
    }

    pub fn set_depth_format(&mut self, format: u32) {
        self.raw2 = (self.raw2 & !(0x1F << 6)) | ((format & 0x1F) << 6);
    }

    pub fn set_y_negate(&mut self, v: bool) {
        self.set_bit_raw2(11, v);
    }
    pub fn set_provoking_vertex_last(&mut self, v: bool) {
        self.set_bit_raw2(12, v);
    }
    pub fn set_conservative_raster_enable(&mut self, v: bool) {
        self.set_bit_raw2(13, v);
    }
    pub fn set_smooth_lines(&mut self, v: bool) {
        self.set_bit_raw2(14, v);
    }
    pub fn set_alpha_to_coverage_enabled(&mut self, v: bool) {
        self.set_bit_raw2(15, v);
    }
    pub fn set_alpha_to_one_enabled(&mut self, v: bool) {
        self.set_bit_raw2(16, v);
    }

    fn set_bit_raw2(&mut self, bit: u32, v: bool) {
        if v {
            self.raw2 |= 1 << bit;
        } else {
            self.raw2 &= !(1 << bit);
        }
    }

    /// Port of `FixedPipelineState::DynamicAttributeType`.
    pub fn dynamic_attribute_type(&self, index: usize) -> u32 {
        ((self.attribute_types_or_enabled_divisors >> (index * 2)) & 0b11) as u32
    }

    /// Populate from a Maxwell3D DrawCall state snapshot.
    ///
    /// This bridges the existing DrawCall-based API to the new bit-packed layout.
    /// A full port of `FixedPipelineState::Refresh(Maxwell3D&, DynamicFeatures&)`
    /// requires direct register access; this intermediate form captures the
    /// essential state from the DrawCall abstraction.
    pub fn refresh(&mut self, draw: &DrawCall) {
        self.raw1 = 0;
        self.raw2 = 0;

        self.set_topology(draw.topology);
        self.dynamic_state
            .set_cull_enable(draw.rasterizer.cull_enable);
        self.dynamic_state.set_cull_face(draw.rasterizer.cull_face);
        self.dynamic_state
            .set_front_face(draw.rasterizer.front_face);
        self.dynamic_state
            .set_depth_test_enable(draw.depth_stencil.depth_test_enable);
        self.dynamic_state
            .set_depth_write_enable(draw.depth_stencil.depth_write_enable);
        self.dynamic_state
            .set_depth_test_func(draw.depth_stencil.depth_func);
        self.dynamic_state
            .set_stencil_enable(draw.depth_stencil.stencil_enable);
        self.dynamic_state
            .set_depth_bias_enable(draw.rasterizer.depth_bias != 0.0);

        // Populate color formats from render targets
        for (i, rt) in draw.render_targets.iter().enumerate() {
            self.color_formats[i] = rt.format as u8;
        }

        // Populate blend attachments
        for (i, blend) in draw.blend.iter().enumerate() {
            let mut att = BlendingAttachment::default();
            if blend.enabled {
                att.set_enabled(true);
                att.set_equation_rgb(blend.color_op);
                att.set_equation_alpha(blend.alpha_op);
                att.set_source_rgb_factor(blend.color_src);
                att.set_dest_rgb_factor(blend.color_dst);
                att.set_source_alpha_factor(blend.alpha_src);
                att.set_dest_alpha_factor(blend.alpha_dst);
            }
            self.attachments[i] = att;
        }

        // Populate vertex attributes
        for (i, attrib) in draw.vertex_attribs.iter().enumerate() {
            if i >= NUM_VERTEX_ATTRIBUTES {
                break;
            }
            let mut va = VertexAttribute::default();
            va.set_enabled(!attrib.constant);
            va.set_buffer(attrib.buffer_index as u32);
            va.set_offset(attrib.offset as u32);
            va.set_type(attrib.attrib_type as u32);
            va.set_size(attrib.size as u32);
            self.attributes[i] = va;
        }

        // Populate vertex strides
        for (i, stream) in draw.vertex_streams.iter().enumerate() {
            if i >= NUM_VERTEX_ARRAYS {
                break;
            }
            self.vertex_strides[i] = stream.stride as u16;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::hash_map::DefaultHasher;

    fn hash_state(state: &FixedPipelineState) -> u64 {
        let mut hasher = DefaultHasher::new();
        state.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn test_default_state_is_consistent() {
        let a = FixedPipelineState::default();
        let b = FixedPipelineState::default();
        assert_eq!(a, b);
        assert_eq!(hash_state(&a), hash_state(&b));
    }

    #[test]
    fn test_different_topology_gives_different_hash() {
        let mut a = FixedPipelineState::default();
        let mut b = FixedPipelineState::default();
        a.set_topology(PrimitiveTopology::Triangles);
        b.set_topology(PrimitiveTopology::Lines);
        assert_ne!(a, b);
        assert_ne!(hash_state(&a), hash_state(&b));
    }

    #[test]
    fn test_pack_unpack_comparison_op() {
        for i in 0..8u32 {
            let op = unpack_comparison_op(i);
            assert_eq!(pack_comparison_op(op), i);
        }
    }

    #[test]
    fn test_pack_unpack_stencil_op() {
        for i in 0..8u32 {
            let op = unpack_stencil_op(i);
            assert_eq!(pack_stencil_op(op), i);
        }
    }

    #[test]
    fn test_pack_unpack_blend_equation() {
        for i in 0..5u32 {
            let eq = unpack_blend_equation(i);
            assert_eq!(pack_blend_equation(eq), i);
        }
    }

    #[test]
    fn test_pack_unpack_blend_factor() {
        for i in 0..19u32 {
            let f = unpack_blend_factor(i);
            assert_eq!(pack_blend_factor(f), i);
        }
    }

    #[test]
    fn test_pack_unpack_cull_face() {
        for i in 0..3u32 {
            let c = unpack_cull_face(i);
            assert_eq!(pack_cull_face(c), i);
        }
    }

    #[test]
    fn test_pack_unpack_front_face() {
        assert_eq!(pack_front_face(unpack_front_face(0)), 0);
        assert_eq!(pack_front_face(unpack_front_face(1)), 1);
    }

    #[test]
    fn test_pack_unpack_polygon_mode() {
        for i in 0..3u32 {
            let m = unpack_polygon_mode(i);
            assert_eq!(pack_polygon_mode(m), i);
        }
    }

    #[test]
    fn test_blending_attachment_bitfields() {
        let mut att = BlendingAttachment::default();
        att.set_mask(true, false, true, false);
        att.set_equation_rgb(BlendEquation::Subtract);
        att.set_source_rgb_factor(BlendFactor::SrcAlpha);
        att.set_dest_rgb_factor(BlendFactor::OneMinusSrcAlpha);
        att.set_enabled(true);

        let mask = att.mask();
        assert!(mask[0]);
        assert!(!mask[1]);
        assert!(mask[2]);
        assert!(!mask[3]);
        assert_eq!(att.equation_rgb(), BlendEquation::Subtract);
        assert_eq!(att.source_rgb_factor(), BlendFactor::SrcAlpha);
        assert_eq!(att.dest_rgb_factor(), BlendFactor::OneMinusSrcAlpha);
        assert!(att.is_enabled());
    }

    #[test]
    fn test_vertex_attribute_bitfields() {
        let mut attr = VertexAttribute::default();
        attr.set_enabled(true);
        attr.set_buffer(5);
        attr.set_offset(128);
        attr.set_type(3);
        attr.set_size(12);

        assert!(attr.is_enabled());
        assert_eq!(attr.buffer(), 5);
        assert_eq!(attr.offset(), 128);
        assert_eq!(attr.attrib_type(), 3);
        assert_eq!(attr.attrib_size(), 12);
    }

    #[test]
    fn test_dynamic_state_bitfields() {
        let mut ds = DynamicState::default();
        ds.set_cull_enable(true);
        ds.set_cull_face(CullFace::Back);
        ds.set_depth_test_enable(true);
        ds.set_depth_test_func(ComparisonOp::Less);
        ds.set_front_face(FrontFace::CCW);
        ds.set_stencil_enable(true);

        assert!(ds.cull_enable());
        assert_eq!(ds.cull_face(), CullFace::Back);
        assert!(ds.depth_test_enable());
        assert_eq!(ds.depth_test_func(), ComparisonOp::Less);
        assert_eq!(ds.front_face(), FrontFace::CCW);
        assert!(ds.stencil_enable());
    }

    #[test]
    fn test_polygon_offset_lut() {
        assert_eq!(
            POLYGON_OFFSET_ENABLE_LUT[PrimitiveTopology::Points as usize],
            POINT
        );
        assert_eq!(
            POLYGON_OFFSET_ENABLE_LUT[PrimitiveTopology::Lines as usize],
            LINE
        );
        assert_eq!(
            POLYGON_OFFSET_ENABLE_LUT[PrimitiveTopology::Triangles as usize],
            POLYGON
        );
        assert_eq!(
            POLYGON_OFFSET_ENABLE_LUT[PrimitiveTopology::Patches as usize],
            POLYGON
        );
    }
}
