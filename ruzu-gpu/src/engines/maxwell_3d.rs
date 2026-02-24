// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell 3D engine — structured state tracking, clear operations, and draw
//! call recording.
//!
//! This is the main 3D rendering engine (NV class B197). It handles render
//! target configuration, clear operations, and draw call state tracking.
//! Register writes are stored in a flat array and side-effect methods (clear,
//! draw begin/end) are triggered on specific register writes.

use super::{ClassId, Engine, Framebuffer, ENGINE_REG_COUNT};

// ── Register offset constants (method addresses) ────────────────────────────

/// Render target array base. 8 targets, 0x10 words (0x40 bytes) each.
const RT_BASE: u32 = 0x0800;
/// Words per render target.
const RT_STRIDE: u32 = 0x10;

// Offsets within each render target (relative to RT_BASE + i*RT_STRIDE):
const RT_OFF_ADDRESS_HIGH: u32 = 0x00;
const RT_OFF_ADDRESS_LOW: u32 = 0x01;
const RT_OFF_WIDTH: u32 = 0x02;
const RT_OFF_HEIGHT: u32 = 0x03;
const RT_OFF_FORMAT: u32 = 0x04;

/// Clear color RGBA: 4 consecutive f32-as-u32 registers.
const CLEAR_COLOR_BASE: u32 = 0x0D80;
/// Clear depth value (f32 as u32 bits).
#[allow(dead_code)]
const CLEAR_DEPTH: u32 = 0x0D90;
/// Clear stencil value.
#[allow(dead_code)]
const CLEAR_STENCIL: u32 = 0x0DA0;
/// Clear surface trigger register.
const CLEAR_SURFACE: u32 = 0x19D0;

// ── Viewport registers ──────────────────────────────────────────────────────

/// Viewport transform base. 16 viewports, 8 words each.
/// Words: scale_x, scale_y, scale_z, translate_x, translate_y, translate_z, swizzle, snap.
const VP_TRANSFORM_BASE: u32 = 0x0A00;
const VP_TRANSFORM_STRIDE: u32 = 8;

// ── Scissor registers ───────────────────────────────────────────────────────

/// Scissor base. 16 scissors, 4 words each.
/// Words: enable, min_x|max_x(packed), min_y|max_y(packed), pad.
const SCISSOR_BASE: u32 = 0x0E00;
const SCISSOR_STRIDE: u32 = 4;

// ── Vertex buffer registers ─────────────────────────────────────────────────

/// Vertex buffer first vertex.
const VB_FIRST: u32 = 0x0D74;
/// Vertex buffer vertex count.
const VB_COUNT: u32 = 0x0D75;

/// Vertex stream array base. 32 streams, 4 words each.
/// Words: stride|enable, addr_high, addr_low, frequency.
const VERTEX_STREAM_BASE: u32 = 0x1C00;
const VERTEX_STREAM_STRIDE: u32 = 4;

/// Vertex stream limit array base. 32 streams, 2 words each.
#[allow(dead_code)]
const VERTEX_STREAM_LIMIT_BASE: u32 = 0x1F00;

// ── Index buffer registers ──────────────────────────────────────────────────

/// Index buffer base (7 words).
/// Words: addr_high, addr_low, limit_high, limit_low, format, first, count.
const IB_BASE: u32 = 0x17C8;
const IB_OFF_ADDR_HIGH: u32 = 0;
const IB_OFF_ADDR_LOW: u32 = 1;
#[allow(dead_code)]
const IB_OFF_LIMIT_HIGH: u32 = 2;
#[allow(dead_code)]
const IB_OFF_LIMIT_LOW: u32 = 3;
const IB_OFF_FORMAT: u32 = 4;
const IB_OFF_FIRST: u32 = 5;
const IB_OFF_COUNT: u32 = 6;

// ── Draw registers ──────────────────────────────────────────────────────────

/// Draw end trigger (previously DRAW_REG).
const DRAW_END: u32 = 0x1614;
/// Draw begin: sets topology.
const DRAW_BEGIN: u32 = 0x1615;

// ── Depth/Stencil registers ─────────────────────────────────────────────────

const DEPTH_MODE: u32 = 0x0D7C;
const DEPTH_TEST_ENABLE: u32 = 0x12CC;
const DEPTH_WRITE_ENABLE: u32 = 0x12E8;
const DEPTH_TEST_FUNC: u32 = 0x130C;

const STENCIL_ENABLE: u32 = 0x1380;
const STENCIL_FRONT_OP_BASE: u32 = 0x1384;
const STENCIL_FRONT_REF: u32 = 0x1394;
const STENCIL_FRONT_FUNC_MASK: u32 = 0x1398;
const STENCIL_FRONT_MASK: u32 = 0x139C;

const STENCIL_TWO_SIDE_ENABLE: u32 = 0x1594;
const STENCIL_BACK_OP_BASE: u32 = 0x1598;
const STENCIL_BACK_REF: u32 = 0x0F54;
const STENCIL_BACK_MASK: u32 = 0x0F58;
const STENCIL_BACK_FUNC_MASK: u32 = 0x0F5C;

// ── Blend registers ─────────────────────────────────────────────────────────

/// 4 consecutive f32 registers: R, G, B, A blend constant color.
const BLEND_COLOR_BASE: u32 = 0x131C;

/// Global blend struct base.
/// +0 separate_alpha, +1 color_op, +2 color_src, +3 color_dst,
/// +4 alpha_op, +5 alpha_src, +6 (color_key), +7 alpha_dst,
/// +8 single_rop_ctrl, +9..+16 enable[0..7]
const BLEND_BASE: u32 = 0x133C;

/// Whether per-target blend overrides are active.
const BLEND_PER_TARGET_ENABLED: u32 = 0x12E4;

/// Per-target blend base. 8 entries, stride 8.
/// +0 sep_alpha, +1 color_op, +2 color_src, +3 color_dst,
/// +4 alpha_op, +5 alpha_src, +6 alpha_dst
const BLEND_PER_TARGET_BASE: u32 = 0x1E00;
const BLEND_PER_TARGET_STRIDE: u32 = 8;

// ── Rasterizer registers ────────────────────────────────────────────────────

const POLYGON_MODE_FRONT: u32 = 0x0DAC;
const POLYGON_MODE_BACK: u32 = 0x0DB0;
const LINE_WIDTH_SMOOTH: u32 = 0x13B0;
const LINE_WIDTH_ALIASED: u32 = 0x13B4;
const SLOPE_SCALE_DEPTH_BIAS: u32 = 0x156C;
const DEPTH_BIAS: u32 = 0x15BC;
const DEPTH_BIAS_CLAMP: u32 = 0x187C;
const CULL_TEST_ENABLE: u32 = 0x1918;
const FRONT_FACE: u32 = 0x191C;
const CULL_FACE: u32 = 0x1920;

// ── Shader program registers ────────────────────────────────────────────────

/// Program region base: addr_high at +0, addr_low at +1.
const PROGRAM_REGION_BASE: u32 = 0x1608;

// ── Vertex attribute registers ────────────────────────────────────────────

/// Vertex attribute array base. 32 entries, 1 word each.
/// Per entry: bits[4:0]=buffer, bit[6]=constant, bits[20:7]=offset,
///            bits[26:21]=size, bits[29:27]=type, bit[31]=bgra.
const VERTEX_ATTRIB_BASE: u32 = 0x1160;
const NUM_VERTEX_ATTRIBS: u32 = 32;

// ── Shader pipeline registers ─────────────────────────────────────────────

/// Shader pipeline base. 6 stages, 0x10 words each.
/// Per stage: +0 packed(enable|type), +1 offset, +3 register_count, +4 binding_group.
const PIPELINE_BASE: u32 = 0x2000;
const PIPELINE_STRIDE: u32 = 0x10;
const NUM_SHADER_PROGRAMS: usize = 6;

// ── Color write mask registers ────────────────────────────────────────────

/// If nonzero, all RTs share color_mask[0].
const COLOR_MASK_COMMON: u32 = 0x0F90;
/// Per-RT color write mask array. 8 entries, 1 word each.
/// Per RT: R=bit[0], G=bit[4], B=bit[8], A=bit[12].
const COLOR_MASK_BASE: u32 = 0x1A00;

// ── Render target control register ────────────────────────────────────────

/// RT control: count in bits[3:0], target map in bits[6:4],[9:7],... (3 bits each).
const RT_CONTROL: u32 = 0x121C;

// ── Constant buffer registers ───────────────────────────────────────────────

/// CB config: +0 size, +1 addr_high, +2 addr_low, +3 offset.
const CB_CONFIG_BASE: u32 = 0x2380;

/// CB data: 16 words of inline push (0x2384..0x2393).
const CB_DATA_BASE: u32 = 0x2384;
const CB_DATA_END: u32 = 0x2394; // exclusive

/// CB bind base. 5 stages, stride 8, trigger at +4.
const CB_BIND_BASE: u32 = 0x2400;
const CB_BIND_STRIDE: u32 = 8;
/// CB bind trigger registers (one per shader stage).
const CB_BIND_TRIGGER_0: u32 = 0x2404;
const CB_BIND_TRIGGER_1: u32 = 0x240C;
const CB_BIND_TRIGGER_2: u32 = 0x2414;
const CB_BIND_TRIGGER_3: u32 = 0x241C;
const CB_BIND_TRIGGER_4: u32 = 0x2424;

/// Number of shader stages (vertex, tess ctrl, tess eval, geometry, fragment).
const NUM_SHADER_STAGES: usize = 5;
/// Maximum constant buffer slots per stage.
const MAX_CB_SLOTS: usize = 18;

// ── Common render target formats ────────────────────────────────────────────

const RT_FORMAT_A8B8G8R8_UNORM: u32 = 0xD5;
const RT_FORMAT_A8B8G8R8_SRGB: u32 = 0xD6;
#[allow(dead_code)]
const RT_FORMAT_A8R8G8B8_UNORM: u32 = 0xCF;

// ── Draw state types ────────────────────────────────────────────────────────

/// GPU primitive topology.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum PrimitiveTopology {
    Points = 0,
    Lines = 1,
    LineLoop = 2,
    LineStrip = 3,
    Triangles = 4,
    TriangleStrip = 5,
    TriangleFan = 6,
    Quads = 7,
    QuadStrip = 8,
    Polygon = 9,
    LinesAdjacency = 10,
    LineStripAdjacency = 11,
    TrianglesAdjacency = 12,
    TriangleStripAdjacency = 13,
    Patches = 14,
}

impl PrimitiveTopology {
    pub fn from_raw(value: u32) -> Self {
        match value & 0xFFFF {
            0 => Self::Points,
            1 => Self::Lines,
            2 => Self::LineLoop,
            3 => Self::LineStrip,
            4 => Self::Triangles,
            5 => Self::TriangleStrip,
            6 => Self::TriangleFan,
            7 => Self::Quads,
            8 => Self::QuadStrip,
            9 => Self::Polygon,
            10 => Self::LinesAdjacency,
            11 => Self::LineStripAdjacency,
            12 => Self::TrianglesAdjacency,
            13 => Self::TriangleStripAdjacency,
            14 => Self::Patches,
            _ => {
                log::warn!("Maxwell3D: unknown topology {}, defaulting to Triangles", value & 0xFFFF);
                Self::Triangles
            }
        }
    }
}

/// Index buffer element format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum IndexFormat {
    UnsignedByte = 0,
    UnsignedShort = 1,
    UnsignedInt = 2,
}

impl IndexFormat {
    pub fn from_raw(value: u32) -> Self {
        match value {
            0 => Self::UnsignedByte,
            1 => Self::UnsignedShort,
            2 => Self::UnsignedInt,
            _ => {
                log::warn!("Maxwell3D: unknown index format {}, defaulting to UnsignedInt", value);
                Self::UnsignedInt
            }
        }
    }

    pub fn size_bytes(&self) -> u32 {
        match self {
            Self::UnsignedByte => 1,
            Self::UnsignedShort => 2,
            Self::UnsignedInt => 4,
        }
    }
}

// ── Pipeline state enums ────────────────────────────────────────────────────

/// Depth/stencil comparison function. Supports both D3D (1-8) and GL (0x200-0x207) encodings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp {
    Never,
    Less,
    Equal,
    LessEqual,
    Greater,
    NotEqual,
    GreaterEqual,
    Always,
}

impl ComparisonOp {
    pub fn from_raw(value: u32) -> Self {
        match value {
            1 | 0x200 => Self::Never,
            2 | 0x201 => Self::Less,
            3 | 0x202 => Self::Equal,
            4 | 0x203 => Self::LessEqual,
            5 | 0x204 => Self::Greater,
            6 | 0x205 => Self::NotEqual,
            7 | 0x206 => Self::GreaterEqual,
            8 | 0x207 => Self::Always,
            _ => {
                log::warn!("Maxwell3D: unknown ComparisonOp 0x{:X}, defaulting to Always", value);
                Self::Always
            }
        }
    }
}

/// Blend equation. Supports both D3D (1-5) and GL (0x8006-0x800B) encodings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlendEquation {
    Add,
    Subtract,
    ReverseSubtract,
    Min,
    Max,
}

impl BlendEquation {
    pub fn from_raw(value: u32) -> Self {
        match value {
            1 | 0x8006 => Self::Add,
            2 | 0x800A => Self::Subtract,
            3 | 0x800B => Self::ReverseSubtract,
            4 | 0x8007 => Self::Min,
            5 | 0x8008 => Self::Max,
            _ => {
                log::warn!("Maxwell3D: unknown BlendEquation 0x{:X}, defaulting to Add", value);
                Self::Add
            }
        }
    }
}

/// Blend factor. Supports both D3D (0x1-0x13) and GL (0x4000+) encodings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlendFactor {
    Zero,
    One,
    SrcColor,
    OneMinusSrcColor,
    SrcAlpha,
    OneMinusSrcAlpha,
    DstAlpha,
    OneMinusDstAlpha,
    DstColor,
    OneMinusDstColor,
    SrcAlphaSaturate,
    Src1Color,
    OneMinusSrc1Color,
    Src1Alpha,
    OneMinusSrc1Alpha,
    ConstantColor,
    OneMinusConstantColor,
    ConstantAlpha,
    OneMinusConstantAlpha,
}

impl BlendFactor {
    pub fn from_raw(value: u32) -> Self {
        match value {
            0x01 | 0x4000 => Self::Zero,
            0x02 | 0x4001 => Self::One,
            0x03 | 0x4300 => Self::SrcColor,
            0x04 | 0x4301 => Self::OneMinusSrcColor,
            0x05 | 0x4302 => Self::SrcAlpha,
            0x06 | 0x4303 => Self::OneMinusSrcAlpha,
            0x07 | 0x4304 => Self::DstAlpha,
            0x08 | 0x4305 => Self::OneMinusDstAlpha,
            0x09 | 0x4306 => Self::DstColor,
            0x0A | 0x4307 => Self::OneMinusDstColor,
            0x0B | 0x4308 => Self::SrcAlphaSaturate,
            0x0D | 0xC900 => Self::Src1Color,
            0x0E | 0xC901 => Self::OneMinusSrc1Color,
            0x0F | 0xC902 => Self::Src1Alpha,
            0x10 | 0xC903 => Self::OneMinusSrc1Alpha,
            0x11 | 0xC001 => Self::ConstantColor,
            0x12 | 0xC002 => Self::OneMinusConstantColor,
            0x13 | 0xC003 => Self::ConstantAlpha,
            0x14 | 0xC004 => Self::OneMinusConstantAlpha,
            _ => {
                log::warn!("Maxwell3D: unknown BlendFactor 0x{:X}, defaulting to One", value);
                Self::One
            }
        }
    }
}

/// Stencil operation. Supports both D3D (1-8) and GL (0x0-0x8508) encodings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StencilOp {
    Keep,
    Zero,
    Replace,
    IncrSat,
    DecrSat,
    Invert,
    Incr,
    Decr,
}

impl StencilOp {
    pub fn from_raw(value: u32) -> Self {
        match value {
            1 | 0x1E00 => Self::Keep,
            2 | 0x0000 => Self::Zero,
            3 | 0x1E01 => Self::Replace,
            4 | 0x1E02 => Self::IncrSat,
            5 | 0x1E03 => Self::DecrSat,
            6 | 0x150A => Self::Invert,
            7 | 0x8507 => Self::Incr,
            8 | 0x8508 => Self::Decr,
            _ => {
                log::warn!("Maxwell3D: unknown StencilOp 0x{:X}, defaulting to Keep", value);
                Self::Keep
            }
        }
    }
}

/// Cull face mode (GL encoding).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CullFace {
    Front,
    Back,
    FrontAndBack,
}

impl CullFace {
    pub fn from_raw(value: u32) -> Self {
        match value {
            0x0404 => Self::Front,
            0x0405 => Self::Back,
            0x0408 => Self::FrontAndBack,
            _ => {
                log::warn!("Maxwell3D: unknown CullFace 0x{:X}, defaulting to Back", value);
                Self::Back
            }
        }
    }
}

/// Front face winding order (GL encoding).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrontFace {
    CW,
    CCW,
}

impl FrontFace {
    pub fn from_raw(value: u32) -> Self {
        match value {
            0x0900 => Self::CW,
            0x0901 => Self::CCW,
            _ => {
                log::warn!("Maxwell3D: unknown FrontFace 0x{:X}, defaulting to CCW", value);
                Self::CCW
            }
        }
    }
}

/// Polygon rasterization mode (GL encoding).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PolygonMode {
    Point,
    Line,
    Fill,
}

impl PolygonMode {
    pub fn from_raw(value: u32) -> Self {
        match value {
            0x1B00 => Self::Point,
            0x1B01 => Self::Line,
            0x1B02 => Self::Fill,
            _ => {
                log::warn!("Maxwell3D: unknown PolygonMode 0x{:X}, defaulting to Fill", value);
                Self::Fill
            }
        }
    }
}

/// Depth range mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DepthMode {
    MinusOneToOne,
    ZeroToOne,
}

impl DepthMode {
    pub fn from_raw(value: u32) -> Self {
        match value {
            0 => Self::MinusOneToOne,
            1 => Self::ZeroToOne,
            _ => {
                log::warn!("Maxwell3D: unknown DepthMode {}, defaulting to ZeroToOne", value);
                Self::ZeroToOne
            }
        }
    }
}

// ── Vertex attribute enums ────────────────────────────────────────────────

/// Vertex attribute component size/layout.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VertexAttribSize {
    R32G32B32A32,
    R32G32B32,
    R16G16B16A16,
    R32G32,
    R16G16B16,
    R8G8B8A8,
    R16G16,
    R32,
    R8G8B8,
    R8G8,
    R16,
    R8,
    A2B10G10R10,
    B10G11R11,
    G8R8,
    X8B8G8R8,
    A8,
    Invalid,
}

impl VertexAttribSize {
    pub fn from_raw(value: u32) -> Self {
        match value {
            0x01 => Self::R32G32B32A32,
            0x02 => Self::R32G32B32,
            0x03 => Self::R16G16B16A16,
            0x04 => Self::R32G32,
            0x05 => Self::R16G16B16,
            0x0A => Self::R8G8B8A8,
            0x0F => Self::R16G16,
            0x12 => Self::R32,
            0x13 => Self::R8G8B8,
            0x18 => Self::R8G8,
            0x1B => Self::R16,
            0x1D => Self::R8,
            0x30 => Self::A2B10G10R10,
            0x31 => Self::B10G11R11,
            0x32 => Self::G8R8,
            0x33 => Self::X8B8G8R8,
            0x34 => Self::A8,
            _ => Self::Invalid,
        }
    }

    /// Size in bytes of one vertex attribute element.
    pub fn size_bytes(&self) -> u32 {
        match self {
            Self::R32G32B32A32 => 16,
            Self::R32G32B32 => 12,
            Self::R16G16B16A16 => 8,
            Self::R32G32 => 8,
            Self::R16G16B16 => 6,
            Self::R8G8B8A8 => 4,
            Self::R16G16 => 4,
            Self::R32 => 4,
            Self::R8G8B8 => 3,
            Self::R8G8 => 2,
            Self::R16 => 2,
            Self::R8 => 1,
            Self::A2B10G10R10 => 4,
            Self::B10G11R11 => 4,
            Self::G8R8 => 2,
            Self::X8B8G8R8 => 4,
            Self::A8 => 1,
            Self::Invalid => 0,
        }
    }

    /// Number of components.
    pub fn component_count(&self) -> u32 {
        match self {
            Self::R32G32B32A32 | Self::R16G16B16A16 | Self::R8G8B8A8
            | Self::A2B10G10R10 | Self::X8B8G8R8 => 4,
            Self::R32G32B32 | Self::R16G16B16 | Self::R8G8B8
            | Self::B10G11R11 => 3,
            Self::R32G32 | Self::R16G16 | Self::R8G8 | Self::G8R8 => 2,
            Self::R32 | Self::R16 | Self::R8 | Self::A8 => 1,
            Self::Invalid => 0,
        }
    }
}

/// Vertex attribute numeric type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VertexAttribType {
    SNorm,
    UNorm,
    SInt,
    UInt,
    UScaled,
    SScaled,
    Float,
    Invalid,
}

impl VertexAttribType {
    pub fn from_raw(value: u32) -> Self {
        match value {
            1 => Self::SNorm,
            2 => Self::UNorm,
            3 => Self::SInt,
            4 => Self::UInt,
            5 => Self::UScaled,
            6 => Self::SScaled,
            7 => Self::Float,
            _ => Self::Invalid,
        }
    }
}

// ── Shader stage enum ─────────────────────────────────────────────────────

/// Shader stage type in the pipeline program array.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShaderStageType {
    VertexA,
    VertexB,
    TessInit,
    Tessellation,
    Geometry,
    Fragment,
    Invalid,
}

impl ShaderStageType {
    pub fn from_raw(value: u32) -> Self {
        match value {
            0 => Self::VertexA,
            1 => Self::VertexB,
            2 => Self::TessInit,
            3 => Self::Tessellation,
            4 => Self::Geometry,
            5 => Self::Fragment,
            _ => Self::Invalid,
        }
    }
}

// ── Info structs ────────────────────────────────────────────────────────────

/// Information about an active vertex stream.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VertexStreamInfo {
    pub index: u32,
    pub address: u64,
    pub stride: u32,
    pub enabled: bool,
}

/// Viewport computed from scale/translate registers.
#[derive(Debug, Clone, PartialEq)]
pub struct ViewportInfo {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
    pub depth_near: f32,
    pub depth_far: f32,
}

/// Scissor rectangle.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScissorInfo {
    pub enabled: bool,
    pub min_x: u32,
    pub max_x: u32,
    pub min_y: u32,
    pub max_y: u32,
}

/// Blend state for a single render target.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlendInfo {
    pub enabled: bool,
    pub separate_alpha: bool,
    pub color_op: BlendEquation,
    pub color_src: BlendFactor,
    pub color_dst: BlendFactor,
    pub alpha_op: BlendEquation,
    pub alpha_src: BlendFactor,
    pub alpha_dst: BlendFactor,
}

impl Default for BlendInfo {
    fn default() -> Self {
        Self {
            enabled: false,
            separate_alpha: false,
            color_op: BlendEquation::Add,
            color_src: BlendFactor::One,
            color_dst: BlendFactor::Zero,
            alpha_op: BlendEquation::Add,
            alpha_src: BlendFactor::One,
            alpha_dst: BlendFactor::Zero,
        }
    }
}

/// Blend constant color.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BlendColorInfo {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}

/// Stencil state for one face.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StencilFaceInfo {
    pub fail_op: StencilOp,
    pub zfail_op: StencilOp,
    pub zpass_op: StencilOp,
    pub func: ComparisonOp,
    pub ref_value: u32,
    pub func_mask: u32,
    pub write_mask: u32,
}

impl Default for StencilFaceInfo {
    fn default() -> Self {
        Self {
            fail_op: StencilOp::Keep,
            zfail_op: StencilOp::Keep,
            zpass_op: StencilOp::Keep,
            func: ComparisonOp::Always,
            ref_value: 0,
            func_mask: 0xFFFF_FFFF,
            write_mask: 0xFFFF_FFFF,
        }
    }
}

/// Combined depth and stencil state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DepthStencilInfo {
    pub depth_test_enable: bool,
    pub depth_write_enable: bool,
    pub depth_func: ComparisonOp,
    pub depth_mode: DepthMode,
    pub stencil_enable: bool,
    pub stencil_two_side: bool,
    pub front: StencilFaceInfo,
    pub back: StencilFaceInfo,
}

/// Vertex attribute info unpacked from a single register word.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VertexAttribInfo {
    pub buffer_index: u32,
    pub constant: bool,
    pub offset: u32,
    pub size: VertexAttribSize,
    pub attrib_type: VertexAttribType,
    pub bgra: bool,
}

/// Shader stage info for one of the 6 pipeline program slots.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ShaderStageInfo {
    pub enabled: bool,
    pub program_type: ShaderStageType,
    pub offset: u32,
    pub register_count: u32,
    pub binding_group: u32,
}

impl Default for ShaderStageInfo {
    fn default() -> Self {
        Self {
            enabled: false,
            program_type: ShaderStageType::VertexA,
            offset: 0,
            register_count: 0,
            binding_group: 0,
        }
    }
}

/// Per-render-target color write mask.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ColorMaskInfo {
    pub r: bool,
    pub g: bool,
    pub b: bool,
    pub a: bool,
}

impl Default for ColorMaskInfo {
    fn default() -> Self {
        Self {
            r: true,
            g: true,
            b: true,
            a: true,
        }
    }
}

/// Render target control: count and target mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RtControlInfo {
    pub count: u32,
    pub map: [u32; 8],
}

impl Default for RtControlInfo {
    fn default() -> Self {
        Self {
            count: 1,
            map: [0, 1, 2, 3, 4, 5, 6, 7],
        }
    }
}

/// Rasterizer state.
#[derive(Debug, Clone, PartialEq)]
pub struct RasterizerInfo {
    pub cull_enable: bool,
    pub front_face: FrontFace,
    pub cull_face: CullFace,
    pub polygon_mode_front: PolygonMode,
    pub polygon_mode_back: PolygonMode,
    pub line_width_smooth: f32,
    pub line_width_aliased: f32,
    pub depth_bias: f32,
    pub slope_scale_depth_bias: f32,
    pub depth_bias_clamp: f32,
}

/// A constant buffer binding for one slot of one shader stage.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConstBufferBinding {
    pub enabled: bool,
    pub address: u64,
    pub size: u32,
}

impl Default for ConstBufferBinding {
    fn default() -> Self {
        Self {
            enabled: false,
            address: 0,
            size: 0,
        }
    }
}

/// A recorded draw call with all relevant state at the time of DRAW_END.
#[derive(Debug, Clone)]
pub struct DrawCall {
    pub topology: PrimitiveTopology,
    pub vertex_first: u32,
    pub vertex_count: u32,
    pub indexed: bool,
    pub index_buffer_addr: u64,
    pub index_buffer_count: u32,
    pub index_buffer_first: u32,
    pub index_format: IndexFormat,
    pub vertex_streams: Vec<VertexStreamInfo>,
    pub viewport: ViewportInfo,
    pub scissor: ScissorInfo,
    pub blend: [BlendInfo; 8],
    pub blend_color: BlendColorInfo,
    pub depth_stencil: DepthStencilInfo,
    pub rasterizer: RasterizerInfo,
    pub program_base_address: u64,
    pub cb_bindings: [[ConstBufferBinding; MAX_CB_SLOTS]; NUM_SHADER_STAGES],
    pub vertex_attribs: Vec<VertexAttribInfo>,
    pub shader_stages: [ShaderStageInfo; NUM_SHADER_PROGRAMS],
    pub color_masks: [ColorMaskInfo; 8],
    pub rt_control: RtControlInfo,
}

// ── Engine struct ───────────────────────────────────────────────────────────

pub struct Maxwell3D {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
    /// Pending framebuffer output from clear operations.
    pending_framebuffer: Option<Framebuffer>,
    /// Accumulated draw call records.
    draw_calls: Vec<DrawCall>,
    /// Current primitive topology (set on DRAW_BEGIN).
    current_topology: PrimitiveTopology,
    /// Whether the current draw is indexed (set when IB count is written).
    draw_indexed: bool,
    /// Constant buffer bindings: 5 shader stages x 18 slots.
    cb_bindings: [[ConstBufferBinding; MAX_CB_SLOTS]; NUM_SHADER_STAGES],
}

impl Maxwell3D {
    pub fn new() -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
            pending_framebuffer: None,
            draw_calls: Vec::new(),
            current_topology: PrimitiveTopology::Triangles,
            draw_indexed: false,
            cb_bindings: [[ConstBufferBinding::default(); MAX_CB_SLOTS]; NUM_SHADER_STAGES],
        }
    }

    // ── Render target accessors ──────────────────────────────────────────

    /// GPU virtual address of render target `index` (0..7).
    fn rt_address(&self, index: usize) -> u64 {
        let base = (RT_BASE + index as u32 * RT_STRIDE) as usize;
        let high = self.regs[base + RT_OFF_ADDRESS_HIGH as usize] as u64;
        let low = self.regs[base + RT_OFF_ADDRESS_LOW as usize] as u64;
        (high << 32) | low
    }

    /// Width of render target `index`.
    fn rt_width(&self, index: usize) -> u32 {
        let base = (RT_BASE + index as u32 * RT_STRIDE) as usize;
        self.regs[base + RT_OFF_WIDTH as usize]
    }

    /// Height of render target `index`.
    fn rt_height(&self, index: usize) -> u32 {
        let base = (RT_BASE + index as u32 * RT_STRIDE) as usize;
        self.regs[base + RT_OFF_HEIGHT as usize]
    }

    /// Format of render target `index`.
    fn rt_format(&self, index: usize) -> u32 {
        let base = (RT_BASE + index as u32 * RT_STRIDE) as usize;
        self.regs[base + RT_OFF_FORMAT as usize]
    }

    /// Read the 4-component clear color as f32 values.
    fn clear_color_rgba(&self) -> [f32; 4] {
        let base = CLEAR_COLOR_BASE as usize;
        [
            f32::from_bits(self.regs[base]),
            f32::from_bits(self.regs[base + 1]),
            f32::from_bits(self.regs[base + 2]),
            f32::from_bits(self.regs[base + 3]),
        ]
    }

    // ── Vertex stream accessors ──────────────────────────────────────────

    /// Read vertex stream `index` (0..31) info from registers.
    pub fn vertex_stream_info(&self, index: u32) -> VertexStreamInfo {
        let base = (VERTEX_STREAM_BASE + index * VERTEX_STREAM_STRIDE) as usize;
        let word0 = self.regs[base]; // stride in bits[11:0], enable in bit 12
        let addr_high = self.regs[base + 1] as u64;
        let addr_low = self.regs[base + 2] as u64;
        VertexStreamInfo {
            index,
            address: (addr_high << 32) | addr_low,
            stride: word0 & 0xFFF,
            enabled: (word0 & (1 << 12)) != 0,
        }
    }

    // ── Index buffer accessors ───────────────────────────────────────────

    /// Index buffer GPU address.
    pub fn index_buffer_addr(&self) -> u64 {
        let base = IB_BASE as usize;
        let high = self.regs[base + IB_OFF_ADDR_HIGH as usize] as u64;
        let low = self.regs[base + IB_OFF_ADDR_LOW as usize] as u64;
        (high << 32) | low
    }

    /// Index buffer element format.
    pub fn index_buffer_format(&self) -> IndexFormat {
        IndexFormat::from_raw(self.regs[(IB_BASE + IB_OFF_FORMAT) as usize])
    }

    /// Index buffer first index.
    pub fn index_buffer_first(&self) -> u32 {
        self.regs[(IB_BASE + IB_OFF_FIRST) as usize]
    }

    /// Index buffer element count.
    pub fn index_buffer_count(&self) -> u32 {
        self.regs[(IB_BASE + IB_OFF_COUNT) as usize]
    }

    // ── Viewport accessors ───────────────────────────────────────────────

    /// Compute viewport info for viewport `index` (0..15) from scale/translate.
    pub fn viewport_info(&self, index: u32) -> ViewportInfo {
        let base = (VP_TRANSFORM_BASE + index * VP_TRANSFORM_STRIDE) as usize;
        let scale_x = f32::from_bits(self.regs[base]);
        let scale_y = f32::from_bits(self.regs[base + 1]);
        let scale_z = f32::from_bits(self.regs[base + 2]);
        let translate_x = f32::from_bits(self.regs[base + 3]);
        let translate_y = f32::from_bits(self.regs[base + 4]);
        let translate_z = f32::from_bits(self.regs[base + 5]);

        // Viewport transform: x = translate - |scale|, width = 2*|scale|
        let width = scale_x.abs() * 2.0;
        let height = scale_y.abs() * 2.0;
        ViewportInfo {
            x: translate_x - scale_x.abs(),
            y: translate_y - scale_y.abs(),
            width,
            height,
            depth_near: translate_z - scale_z.abs(),
            depth_far: translate_z + scale_z.abs(),
        }
    }

    // ── Scissor accessors ────────────────────────────────────────────────

    /// Read scissor info for scissor `index` (0..15).
    pub fn scissor_info(&self, index: u32) -> ScissorInfo {
        let base = (SCISSOR_BASE + index * SCISSOR_STRIDE) as usize;
        let enable = self.regs[base];
        let x_packed = self.regs[base + 1]; // min_x[15:0] | max_x[31:16]
        let y_packed = self.regs[base + 2]; // min_y[15:0] | max_y[31:16]
        ScissorInfo {
            enabled: (enable & 1) != 0,
            min_x: x_packed & 0xFFFF,
            max_x: (x_packed >> 16) & 0xFFFF,
            min_y: y_packed & 0xFFFF,
            max_y: (y_packed >> 16) & 0xFFFF,
        }
    }

    // ── Blend accessors ──────────────────────────────────────────────────

    /// Read blend constant color.
    pub fn blend_color_info(&self) -> BlendColorInfo {
        let base = BLEND_COLOR_BASE as usize;
        BlendColorInfo {
            r: f32::from_bits(self.regs[base]),
            g: f32::from_bits(self.regs[base + 1]),
            b: f32::from_bits(self.regs[base + 2]),
            a: f32::from_bits(self.regs[base + 3]),
        }
    }

    /// Whether blend is enabled for render target `rt` (0..7).
    pub fn blend_enable(&self, rt: usize) -> bool {
        self.regs[(BLEND_BASE + 9 + rt as u32) as usize] != 0
    }

    /// Read global (non-per-target) blend info for render target `rt`.
    pub fn global_blend_info(&self, rt: usize) -> BlendInfo {
        let base = BLEND_BASE as usize;
        BlendInfo {
            enabled: self.blend_enable(rt),
            separate_alpha: self.regs[base] != 0,
            color_op: BlendEquation::from_raw(self.regs[base + 1]),
            color_src: BlendFactor::from_raw(self.regs[base + 2]),
            color_dst: BlendFactor::from_raw(self.regs[base + 3]),
            alpha_op: BlendEquation::from_raw(self.regs[base + 4]),
            alpha_src: BlendFactor::from_raw(self.regs[base + 5]),
            alpha_dst: BlendFactor::from_raw(self.regs[base + 7]),
        }
    }

    /// Read per-target blend info for render target `rt` (0..7).
    pub fn blend_per_target_info(&self, rt: usize) -> BlendInfo {
        let base = (BLEND_PER_TARGET_BASE + rt as u32 * BLEND_PER_TARGET_STRIDE) as usize;
        BlendInfo {
            enabled: self.blend_enable(rt),
            separate_alpha: self.regs[base] != 0,
            color_op: BlendEquation::from_raw(self.regs[base + 1]),
            color_src: BlendFactor::from_raw(self.regs[base + 2]),
            color_dst: BlendFactor::from_raw(self.regs[base + 3]),
            alpha_op: BlendEquation::from_raw(self.regs[base + 4]),
            alpha_src: BlendFactor::from_raw(self.regs[base + 5]),
            alpha_dst: BlendFactor::from_raw(self.regs[base + 6]),
        }
    }

    /// Effective blend info: per-target if enabled, otherwise global.
    pub fn effective_blend_info(&self, rt: usize) -> BlendInfo {
        if self.regs[BLEND_PER_TARGET_ENABLED as usize] != 0 {
            self.blend_per_target_info(rt)
        } else {
            self.global_blend_info(rt)
        }
    }

    // ── Depth/Stencil accessors ──────────────────────────────────────────

    /// Read combined depth and stencil state.
    pub fn depth_stencil_info(&self) -> DepthStencilInfo {
        let front_base = STENCIL_FRONT_OP_BASE as usize;
        let back_base = STENCIL_BACK_OP_BASE as usize;

        let front = StencilFaceInfo {
            fail_op: StencilOp::from_raw(self.regs[front_base]),
            zfail_op: StencilOp::from_raw(self.regs[front_base + 1]),
            zpass_op: StencilOp::from_raw(self.regs[front_base + 2]),
            func: ComparisonOp::from_raw(self.regs[front_base + 3]),
            ref_value: self.regs[STENCIL_FRONT_REF as usize],
            func_mask: self.regs[STENCIL_FRONT_FUNC_MASK as usize],
            write_mask: self.regs[STENCIL_FRONT_MASK as usize],
        };

        let back = StencilFaceInfo {
            fail_op: StencilOp::from_raw(self.regs[back_base]),
            zfail_op: StencilOp::from_raw(self.regs[back_base + 1]),
            zpass_op: StencilOp::from_raw(self.regs[back_base + 2]),
            func: ComparisonOp::from_raw(self.regs[back_base + 3]),
            ref_value: self.regs[STENCIL_BACK_REF as usize],
            func_mask: self.regs[STENCIL_BACK_FUNC_MASK as usize],
            write_mask: self.regs[STENCIL_BACK_MASK as usize],
        };

        DepthStencilInfo {
            depth_test_enable: self.regs[DEPTH_TEST_ENABLE as usize] != 0,
            depth_write_enable: self.regs[DEPTH_WRITE_ENABLE as usize] != 0,
            depth_func: ComparisonOp::from_raw(self.regs[DEPTH_TEST_FUNC as usize]),
            depth_mode: DepthMode::from_raw(self.regs[DEPTH_MODE as usize]),
            stencil_enable: self.regs[STENCIL_ENABLE as usize] != 0,
            stencil_two_side: self.regs[STENCIL_TWO_SIDE_ENABLE as usize] != 0,
            front,
            back,
        }
    }

    // ── Rasterizer accessors ─────────────────────────────────────────────

    /// Read rasterizer state.
    pub fn rasterizer_info(&self) -> RasterizerInfo {
        RasterizerInfo {
            cull_enable: self.regs[CULL_TEST_ENABLE as usize] != 0,
            front_face: FrontFace::from_raw(self.regs[FRONT_FACE as usize]),
            cull_face: CullFace::from_raw(self.regs[CULL_FACE as usize]),
            polygon_mode_front: PolygonMode::from_raw(
                self.regs[POLYGON_MODE_FRONT as usize],
            ),
            polygon_mode_back: PolygonMode::from_raw(
                self.regs[POLYGON_MODE_BACK as usize],
            ),
            line_width_smooth: f32::from_bits(self.regs[LINE_WIDTH_SMOOTH as usize]),
            line_width_aliased: f32::from_bits(self.regs[LINE_WIDTH_ALIASED as usize]),
            depth_bias: f32::from_bits(self.regs[DEPTH_BIAS as usize]),
            slope_scale_depth_bias: f32::from_bits(
                self.regs[SLOPE_SCALE_DEPTH_BIAS as usize],
            ),
            depth_bias_clamp: f32::from_bits(self.regs[DEPTH_BIAS_CLAMP as usize]),
        }
    }

    // ── Shader program accessors ─────────────────────────────────────────

    /// Read the shader program region base address.
    pub fn program_base_address(&self) -> u64 {
        let base = PROGRAM_REGION_BASE as usize;
        let high = self.regs[base] as u64;
        let low = self.regs[base + 1] as u64;
        (high << 32) | low
    }

    // ── Vertex attribute accessors ────────────────────────────────────────

    /// Read vertex attribute info for `index` (0..31).
    pub fn vertex_attrib_info(&self, index: u32) -> VertexAttribInfo {
        let raw = self.regs[(VERTEX_ATTRIB_BASE + index) as usize];
        VertexAttribInfo {
            buffer_index: raw & 0x1F,
            constant: (raw & (1 << 6)) != 0,
            offset: (raw >> 7) & 0x3FFF,
            size: VertexAttribSize::from_raw((raw >> 21) & 0x3F),
            attrib_type: VertexAttribType::from_raw((raw >> 27) & 0x7),
            bgra: (raw & (1 << 31)) != 0,
        }
    }

    // ── Shader pipeline accessors ────────────────────────────────────────

    /// Read shader stage info for pipeline slot `index` (0..5).
    pub fn shader_stage_info(&self, index: u32) -> ShaderStageInfo {
        let base = (PIPELINE_BASE + index * PIPELINE_STRIDE) as usize;
        let word0 = self.regs[base];
        let enabled = (word0 & 1) != 0;
        let program_type = ShaderStageType::from_raw((word0 >> 4) & 0xF);
        ShaderStageInfo {
            enabled,
            program_type,
            offset: self.regs[base + 1],
            register_count: self.regs[base + 3],
            binding_group: self.regs[base + 4],
        }
    }

    /// Whether shader stage `index` is enabled.
    /// VertexB (index 1) always returns true — the GPU requires it.
    pub fn is_shader_stage_enabled(&self, index: u32) -> bool {
        if index == 1 {
            return true;
        }
        let base = (PIPELINE_BASE + index * PIPELINE_STRIDE) as usize;
        (self.regs[base] & 1) != 0
    }

    // ── Color mask accessors ─────────────────────────────────────────────

    /// Read color write mask for render target `rt` (0..7).
    /// If COLOR_MASK_COMMON is set, all RTs share mask[0].
    pub fn color_mask_info(&self, rt: usize) -> ColorMaskInfo {
        let effective_rt = if self.regs[COLOR_MASK_COMMON as usize] != 0 {
            0
        } else {
            rt
        };
        let raw = self.regs[(COLOR_MASK_BASE + effective_rt as u32) as usize];
        ColorMaskInfo {
            r: (raw & (1 << 0)) != 0,
            g: (raw & (1 << 4)) != 0,
            b: (raw & (1 << 8)) != 0,
            a: (raw & (1 << 12)) != 0,
        }
    }

    // ── Render target control accessors ──────────────────────────────────

    /// Read render target control: count and per-RT target mapping.
    pub fn rt_control_info(&self) -> RtControlInfo {
        let raw = self.regs[RT_CONTROL as usize];
        let count = raw & 0xF;
        let mut map = [0u32; 8];
        for i in 0..8 {
            map[i] = (raw >> (4 + i * 3)) & 0x7;
        }
        RtControlInfo { count, map }
    }

    // ── Constant buffer accessors ────────────────────────────────────────

    /// Read constant buffer bindings for a shader stage (0..4).
    pub fn const_buffer_bindings(
        &self,
        stage: usize,
    ) -> &[ConstBufferBinding; MAX_CB_SLOTS] {
        &self.cb_bindings[stage]
    }

    // ── Draw call accessors ──────────────────────────────────────────────

    /// Drain accumulated draw call records.
    pub fn take_draw_calls(&mut self) -> Vec<DrawCall> {
        std::mem::take(&mut self.draw_calls)
    }

    // ── Side-effect handlers ─────────────────────────────────────────────

    /// Handle clear_surface trigger.
    ///
    /// Bitfield:
    /// - bit 0: clear Z (depth)
    /// - bit 1: clear S (stencil)
    /// - bit 2: clear R
    /// - bit 3: clear G
    /// - bit 4: clear B
    /// - bit 5: clear A
    /// - bits 6..9: RT index
    fn handle_clear_surface(&mut self, flags: u32) {
        let rt_index = ((flags >> 6) & 0xF) as usize;
        let clear_r = flags & (1 << 2) != 0;
        let clear_g = flags & (1 << 3) != 0;
        let clear_b = flags & (1 << 4) != 0;
        let clear_a = flags & (1 << 5) != 0;

        if !clear_r && !clear_g && !clear_b && !clear_a {
            // No color channels to clear (depth/stencil only) — skip for now.
            log::trace!("Maxwell3D: clear_surface depth/stencil only, skipping");
            return;
        }

        if rt_index >= 8 {
            log::warn!("Maxwell3D: clear_surface invalid RT index {}", rt_index);
            return;
        }

        let gpu_va = self.rt_address(rt_index);
        let width = self.rt_width(rt_index);
        let height = self.rt_height(rt_index);
        let format = self.rt_format(rt_index);

        if width == 0 || height == 0 || gpu_va == 0 {
            log::trace!(
                "Maxwell3D: clear_surface skipped (width={}, height={}, va=0x{:X})",
                width,
                height,
                gpu_va
            );
            return;
        }

        let color = self.clear_color_rgba();

        log::debug!(
            "Maxwell3D: clear RT{} {}x{} fmt=0x{:X} color=[{:.3}, {:.3}, {:.3}, {:.3}] va=0x{:X}",
            rt_index,
            width,
            height,
            format,
            color[0],
            color[1],
            color[2],
            color[3],
            gpu_va,
        );

        // Convert clear color to RGBA8 bytes based on format.
        let pixel = format_clear_color(format, color, clear_r, clear_g, clear_b, clear_a);

        // Fill framebuffer.
        let pixel_count = (width as usize) * (height as usize);
        let mut pixels = vec![0u8; pixel_count * 4];
        for i in 0..pixel_count {
            let off = i * 4;
            pixels[off] = pixel[0];
            pixels[off + 1] = pixel[1];
            pixels[off + 2] = pixel[2];
            pixels[off + 3] = pixel[3];
        }

        self.pending_framebuffer = Some(Framebuffer {
            gpu_va,
            width,
            height,
            pixels,
        });
    }

    /// Handle DRAW_BEGIN: captures topology from bits[15:0].
    fn handle_draw_begin(&mut self, value: u32) {
        self.current_topology = PrimitiveTopology::from_raw(value);
        log::debug!(
            "Maxwell3D: DRAW_BEGIN topology={:?}",
            self.current_topology
        );
    }

    /// Handle DRAW_END: builds DrawCall record from current register state.
    fn handle_draw_end(&mut self) {
        // Collect active vertex streams (scan all 32 slots).
        let mut vertex_streams = Vec::new();
        for i in 0..32 {
            let info = self.vertex_stream_info(i);
            if info.enabled {
                vertex_streams.push(info);
            }
        }

        // Collect active vertex attributes (non-zero raw word only).
        let mut vertex_attribs = Vec::new();
        for i in 0..NUM_VERTEX_ATTRIBS {
            let raw = self.regs[(VERTEX_ATTRIB_BASE + i) as usize];
            if raw != 0 {
                vertex_attribs.push(self.vertex_attrib_info(i));
            }
        }

        // Collect all 6 shader stages.
        let mut shader_stages = [ShaderStageInfo::default(); NUM_SHADER_PROGRAMS];
        for (i, stage) in shader_stages.iter_mut().enumerate() {
            *stage = self.shader_stage_info(i as u32);
        }

        // Collect color masks for all 8 render targets.
        let mut color_masks = [ColorMaskInfo::default(); 8];
        for (i, mask) in color_masks.iter_mut().enumerate() {
            *mask = self.color_mask_info(i);
        }

        // Collect render target control.
        let rt_control = self.rt_control_info();

        // Collect blend info for all 8 render targets.
        let mut blend = [BlendInfo::default(); 8];
        for (i, b) in blend.iter_mut().enumerate() {
            *b = self.effective_blend_info(i);
        }

        let draw = DrawCall {
            topology: self.current_topology,
            vertex_first: self.regs[VB_FIRST as usize],
            vertex_count: self.regs[VB_COUNT as usize],
            indexed: self.draw_indexed,
            index_buffer_addr: self.index_buffer_addr(),
            index_buffer_count: self.index_buffer_count(),
            index_buffer_first: self.index_buffer_first(),
            index_format: self.index_buffer_format(),
            vertex_streams,
            viewport: self.viewport_info(0),
            scissor: self.scissor_info(0),
            blend,
            blend_color: self.blend_color_info(),
            depth_stencil: self.depth_stencil_info(),
            rasterizer: self.rasterizer_info(),
            program_base_address: self.program_base_address(),
            cb_bindings: self.cb_bindings,
            vertex_attribs,
            shader_stages,
            color_masks,
            rt_control,
        };

        log::debug!(
            "Maxwell3D: DRAW_END {:?} verts={}/{} indexed={} streams={}",
            draw.topology,
            draw.vertex_first,
            draw.vertex_count,
            draw.indexed,
            draw.vertex_streams.len(),
        );

        self.draw_calls.push(draw);
    }

    /// Handle CB_DATA write: auto-increment CB offset by 4.
    fn handle_cb_data(&mut self, _value: u32) {
        let offset_reg = (CB_CONFIG_BASE + 3) as usize;
        self.regs[offset_reg] = self.regs[offset_reg].wrapping_add(4);
    }

    /// Handle CB_BIND trigger for a shader stage.
    fn handle_cb_bind(&mut self, stage: usize) {
        let bind_base = (CB_BIND_BASE + stage as u32 * CB_BIND_STRIDE) as usize;
        let raw_config = self.regs[bind_base + 4];

        let valid = (raw_config & 1) != 0;
        let slot = ((raw_config >> 4) & 0x1F) as usize;

        if slot >= MAX_CB_SLOTS {
            log::warn!(
                "Maxwell3D: CB_BIND stage {} slot {} out of range",
                stage,
                slot
            );
            return;
        }

        if valid {
            let cb_base = CB_CONFIG_BASE as usize;
            let size = self.regs[cb_base];
            let addr_high = self.regs[cb_base + 1] as u64;
            let addr_low = self.regs[cb_base + 2] as u64;
            let address = (addr_high << 32) | addr_low;

            self.cb_bindings[stage][slot] = ConstBufferBinding {
                enabled: true,
                address,
                size,
            };
            log::trace!(
                "Maxwell3D: CB_BIND stage={} slot={} addr=0x{:X} size={}",
                stage,
                slot,
                address,
                size
            );
        } else {
            self.cb_bindings[stage][slot] = ConstBufferBinding::default();
            log::trace!(
                "Maxwell3D: CB_BIND stage={} slot={} disabled",
                stage,
                slot
            );
        }
    }
}

/// Convert clear color f32s to 4 bytes based on render target format.
///
/// For formats where only certain channels are being cleared, uncleared
/// channels default to 0 (since we're generating a full framebuffer fill,
/// the partial clear only matters if we were compositing with existing data).
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
        // A8B8G8R8: memory layout is [R, G, B, A] when read as bytes in LE.
        // The Switch GPU stores pixels as ABGR in u32 LE, which means
        // byte[0]=R, byte[1]=G, byte[2]=B, byte[3]=A in memory.
        RT_FORMAT_A8B8G8R8_UNORM | RT_FORMAT_A8B8G8R8_SRGB => [r, g, b, a],
        // Unknown format: default to RGBA layout.
        _ => {
            log::trace!(
                "Maxwell3D: unknown RT format 0x{:X}, using RGBA8 layout",
                format
            );
            [r, g, b, a]
        }
    }
}

impl Default for Maxwell3D {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine for Maxwell3D {
    fn class_id(&self) -> ClassId {
        ClassId::Threed
    }

    fn write_reg(&mut self, method: u32, value: u32) {
        let idx = method as usize;
        if idx < ENGINE_REG_COUNT {
            self.regs[idx] = value;
        }

        // Track draw_indexed flag when IB count register is written.
        if method == IB_BASE + IB_OFF_COUNT && value > 0 {
            self.draw_indexed = true;
        }

        // Detect side-effect triggers.
        match method {
            CLEAR_SURFACE => self.handle_clear_surface(value),
            DRAW_BEGIN => self.handle_draw_begin(value),
            DRAW_END => self.handle_draw_end(),
            CB_DATA_BASE..CB_DATA_END => self.handle_cb_data(value),
            CB_BIND_TRIGGER_0 => self.handle_cb_bind(0),
            CB_BIND_TRIGGER_1 => self.handle_cb_bind(1),
            CB_BIND_TRIGGER_2 => self.handle_cb_bind(2),
            CB_BIND_TRIGGER_3 => self.handle_cb_bind(3),
            CB_BIND_TRIGGER_4 => self.handle_cb_bind(4),
            _ => {}
        }

        log::trace!("Maxwell3D: reg[0x{:X}] = 0x{:X}", method, value);
    }

    fn take_framebuffer(&mut self) -> Option<Framebuffer> {
        self.pending_framebuffer.take()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── Existing tests ───────────────────────────────────────────────────

    #[test]
    fn test_write_reg() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(0x100, 0xDEAD);
        assert_eq!(engine.regs[0x100], 0xDEAD);
    }

    #[test]
    fn test_write_reg_high_method() {
        // Regression: methods above 0xFFF were silently dropped with old 0x1000 size.
        let mut engine = Maxwell3D::new();
        engine.write_reg(0x19D0, 0x1234);
        assert_eq!(engine.regs[0x19D0], 0x1234);
    }

    #[test]
    fn test_class_id() {
        let engine = Maxwell3D::new();
        assert_eq!(engine.class_id(), ClassId::Threed);
    }

    #[test]
    fn test_rt_accessors() {
        let mut engine = Maxwell3D::new();
        let rt0_base = RT_BASE as usize;

        // Set RT0: address = 0x0001_0000_2000, width=1280, height=720, format=0xD5
        engine.regs[rt0_base + RT_OFF_ADDRESS_HIGH as usize] = 0x0001;
        engine.regs[rt0_base + RT_OFF_ADDRESS_LOW as usize] = 0x0000_2000;
        engine.regs[rt0_base + RT_OFF_WIDTH as usize] = 1280;
        engine.regs[rt0_base + RT_OFF_HEIGHT as usize] = 720;
        engine.regs[rt0_base + RT_OFF_FORMAT as usize] = 0xD5;

        assert_eq!(engine.rt_address(0), 0x0001_0000_2000);
        assert_eq!(engine.rt_width(0), 1280);
        assert_eq!(engine.rt_height(0), 720);
        assert_eq!(engine.rt_format(0), 0xD5);
    }

    #[test]
    fn test_clear_color_accessor() {
        let mut engine = Maxwell3D::new();
        let base = CLEAR_COLOR_BASE as usize;

        engine.regs[base] = f32::to_bits(1.0); // R
        engine.regs[base + 1] = f32::to_bits(0.5); // G
        engine.regs[base + 2] = f32::to_bits(0.0); // B
        engine.regs[base + 3] = f32::to_bits(0.75); // A

        let color = engine.clear_color_rgba();
        assert_eq!(color[0], 1.0);
        assert_eq!(color[1], 0.5);
        assert_eq!(color[2], 0.0);
        assert_eq!(color[3], 0.75);
    }

    #[test]
    fn test_handle_clear_produces_framebuffer() {
        let mut engine = Maxwell3D::new();
        let rt0_base = RT_BASE as usize;

        // Configure RT0: 4x2 pixels, A8B8G8R8_UNORM, address 0x1000.
        engine.regs[rt0_base + RT_OFF_ADDRESS_HIGH as usize] = 0;
        engine.regs[rt0_base + RT_OFF_ADDRESS_LOW as usize] = 0x1000;
        engine.regs[rt0_base + RT_OFF_WIDTH as usize] = 4;
        engine.regs[rt0_base + RT_OFF_HEIGHT as usize] = 2;
        engine.regs[rt0_base + RT_OFF_FORMAT as usize] = RT_FORMAT_A8B8G8R8_UNORM;

        // Set clear color to solid red (R=1, G=0, B=0, A=1).
        let base = CLEAR_COLOR_BASE as usize;
        engine.regs[base] = f32::to_bits(1.0);
        engine.regs[base + 1] = f32::to_bits(0.0);
        engine.regs[base + 2] = f32::to_bits(0.0);
        engine.regs[base + 3] = f32::to_bits(1.0);

        // Trigger clear: clear RGBA on RT0 (bits 2-5 set, RT index 0).
        let flags = (1 << 2) | (1 << 3) | (1 << 4) | (1 << 5); // R|G|B|A
        engine.handle_clear_surface(flags);

        let fb = engine.take_framebuffer().expect("Should produce framebuffer");
        assert_eq!(fb.gpu_va, 0x1000);
        assert_eq!(fb.width, 4);
        assert_eq!(fb.height, 2);
        assert_eq!(fb.pixels.len(), 4 * 2 * 4); // 32 bytes

        // Every pixel should be [255, 0, 0, 255] (red).
        for chunk in fb.pixels.chunks_exact(4) {
            assert_eq!(chunk, &[255, 0, 0, 255]);
        }
    }

    #[test]
    fn test_clear_with_zero_dimensions_skipped() {
        let mut engine = Maxwell3D::new();
        // RT0 has zero width/height — clear should not produce framebuffer.
        let flags = (1 << 2) | (1 << 3) | (1 << 4) | (1 << 5);
        engine.handle_clear_surface(flags);
        assert!(engine.take_framebuffer().is_none());
    }

    #[test]
    fn test_clear_depth_only_skipped() {
        let mut engine = Maxwell3D::new();
        let rt0_base = RT_BASE as usize;
        engine.regs[rt0_base + RT_OFF_ADDRESS_LOW as usize] = 0x1000;
        engine.regs[rt0_base + RT_OFF_WIDTH as usize] = 4;
        engine.regs[rt0_base + RT_OFF_HEIGHT as usize] = 2;
        engine.regs[rt0_base + RT_OFF_FORMAT as usize] = RT_FORMAT_A8B8G8R8_UNORM;

        // Only depth clear (bit 0), no color channels.
        engine.handle_clear_surface(1);
        assert!(engine.take_framebuffer().is_none());
    }

    #[test]
    fn test_draw_logs_without_crash() {
        let mut engine = Maxwell3D::new();
        // Just ensure draw begin/end doesn't panic.
        engine.handle_draw_begin(0x0004); // Triangles
        engine.handle_draw_end();
        assert!(engine.take_framebuffer().is_none());
        assert_eq!(engine.draw_calls.len(), 1);
    }

    #[test]
    fn test_take_framebuffer_returns_none_after_take() {
        let mut engine = Maxwell3D::new();
        let rt0_base = RT_BASE as usize;
        engine.regs[rt0_base + RT_OFF_ADDRESS_LOW as usize] = 0x1000;
        engine.regs[rt0_base + RT_OFF_WIDTH as usize] = 2;
        engine.regs[rt0_base + RT_OFF_HEIGHT as usize] = 2;
        engine.regs[rt0_base + RT_OFF_FORMAT as usize] = RT_FORMAT_A8B8G8R8_UNORM;

        let base = CLEAR_COLOR_BASE as usize;
        engine.regs[base] = f32::to_bits(0.0);
        engine.regs[base + 1] = f32::to_bits(1.0);
        engine.regs[base + 2] = f32::to_bits(0.0);
        engine.regs[base + 3] = f32::to_bits(1.0);

        let flags = (1 << 2) | (1 << 3) | (1 << 4) | (1 << 5);
        engine.handle_clear_surface(flags);

        assert!(engine.take_framebuffer().is_some());
        // Second take should return None.
        assert!(engine.take_framebuffer().is_none());
    }

    #[test]
    fn test_clear_via_write_reg() {
        let mut engine = Maxwell3D::new();
        let rt0_base = RT_BASE;

        // Write RT0 config via write_reg (as the command processor would).
        engine.write_reg(rt0_base + RT_OFF_ADDRESS_LOW, 0x5000);
        engine.write_reg(rt0_base + RT_OFF_WIDTH, 8);
        engine.write_reg(rt0_base + RT_OFF_HEIGHT, 4);
        engine.write_reg(rt0_base + RT_OFF_FORMAT, RT_FORMAT_A8B8G8R8_UNORM);

        // Set clear color to blue.
        engine.write_reg(CLEAR_COLOR_BASE, f32::to_bits(0.0));
        engine.write_reg(CLEAR_COLOR_BASE + 1, f32::to_bits(0.0));
        engine.write_reg(CLEAR_COLOR_BASE + 2, f32::to_bits(1.0));
        engine.write_reg(CLEAR_COLOR_BASE + 3, f32::to_bits(1.0));

        // Trigger clear via write_reg (this is the actual path).
        let flags = (1 << 2) | (1 << 3) | (1 << 4) | (1 << 5);
        engine.write_reg(CLEAR_SURFACE, flags);

        let fb = engine.take_framebuffer().expect("Should produce framebuffer");
        assert_eq!(fb.gpu_va, 0x5000);
        assert_eq!(fb.width, 8);
        assert_eq!(fb.height, 4);

        // Every pixel: [0, 0, 255, 255] (blue).
        for chunk in fb.pixels.chunks_exact(4) {
            assert_eq!(chunk, &[0, 0, 255, 255]);
        }
    }

    // ── Draw state tracking tests ────────────────────────────────────────

    #[test]
    fn test_vertex_stream_accessors() {
        let mut engine = Maxwell3D::new();
        let base = VERTEX_STREAM_BASE as usize;

        // Stream 0: stride=64, enabled, addr=0x0000_1000_2000.
        engine.regs[base] = 64 | (1 << 12); // stride=64, enable bit 12
        engine.regs[base + 1] = 0x0000_1000; // addr_high
        engine.regs[base + 2] = 0x0000_2000; // addr_low

        let info = engine.vertex_stream_info(0);
        assert_eq!(info.index, 0);
        assert_eq!(info.stride, 64);
        assert!(info.enabled);
        assert_eq!(info.address, 0x0000_1000_0000_2000);
    }

    #[test]
    fn test_vertex_stream_disabled() {
        let mut engine = Maxwell3D::new();
        let base = VERTEX_STREAM_BASE as usize;

        // Stream 0: stride=32, NOT enabled (bit 12 clear).
        engine.regs[base] = 32;
        engine.regs[base + 1] = 0;
        engine.regs[base + 2] = 0x5000;

        let info = engine.vertex_stream_info(0);
        assert_eq!(info.stride, 32);
        assert!(!info.enabled);
    }

    #[test]
    fn test_index_buffer_accessors() {
        let mut engine = Maxwell3D::new();
        let base = IB_BASE as usize;

        engine.regs[base + IB_OFF_ADDR_HIGH as usize] = 0x0000_00AB;
        engine.regs[base + IB_OFF_ADDR_LOW as usize] = 0xCDEF_0000;
        engine.regs[base + IB_OFF_FORMAT as usize] = 1; // UnsignedShort
        engine.regs[base + IB_OFF_FIRST as usize] = 10;
        engine.regs[base + IB_OFF_COUNT as usize] = 500;

        assert_eq!(engine.index_buffer_addr(), 0xAB_CDEF_0000);
        assert_eq!(engine.index_buffer_format(), IndexFormat::UnsignedShort);
        assert_eq!(engine.index_buffer_format().size_bytes(), 2);
        assert_eq!(engine.index_buffer_first(), 10);
        assert_eq!(engine.index_buffer_count(), 500);
    }

    #[test]
    fn test_viewport_info() {
        let mut engine = Maxwell3D::new();
        let base = VP_TRANSFORM_BASE as usize;

        // VP0: scale=(640, -360, 0.5), translate=(640, 360, 0.5)
        // => x=0, y=0, width=1280, height=720, near=0, far=1
        engine.regs[base] = f32::to_bits(640.0); // scale_x
        engine.regs[base + 1] = f32::to_bits(-360.0); // scale_y
        engine.regs[base + 2] = f32::to_bits(0.5); // scale_z
        engine.regs[base + 3] = f32::to_bits(640.0); // translate_x
        engine.regs[base + 4] = f32::to_bits(360.0); // translate_y
        engine.regs[base + 5] = f32::to_bits(0.5); // translate_z

        let vp = engine.viewport_info(0);
        assert_eq!(vp.x, 0.0);
        assert_eq!(vp.y, 0.0);
        assert_eq!(vp.width, 1280.0);
        assert_eq!(vp.height, 720.0);
        assert_eq!(vp.depth_near, 0.0);
        assert_eq!(vp.depth_far, 1.0);
    }

    #[test]
    fn test_scissor_info() {
        let mut engine = Maxwell3D::new();
        let base = SCISSOR_BASE as usize;

        // Scissor 0: enabled, min_x=10, max_x=1270, min_y=20, max_y=700.
        engine.regs[base] = 1; // enabled
        engine.regs[base + 1] = 10 | (1270 << 16); // min_x | max_x
        engine.regs[base + 2] = 20 | (700 << 16); // min_y | max_y

        let sc = engine.scissor_info(0);
        assert!(sc.enabled);
        assert_eq!(sc.min_x, 10);
        assert_eq!(sc.max_x, 1270);
        assert_eq!(sc.min_y, 20);
        assert_eq!(sc.max_y, 700);
    }

    #[test]
    fn test_draw_begin_sets_topology() {
        let mut engine = Maxwell3D::new();
        engine.handle_draw_begin(4); // Triangles
        assert_eq!(engine.current_topology, PrimitiveTopology::Triangles);

        engine.handle_draw_begin(1); // Lines
        assert_eq!(engine.current_topology, PrimitiveTopology::Lines);
    }

    #[test]
    fn test_draw_end_creates_draw_call() {
        let mut engine = Maxwell3D::new();

        // Set up vertex stream 0.
        let vs_base = VERTEX_STREAM_BASE;
        engine.write_reg(vs_base, 32 | (1 << 12)); // stride=32, enabled
        engine.write_reg(vs_base + 1, 0); // addr_high
        engine.write_reg(vs_base + 2, 0x10000); // addr_low

        // Set vertex buffer first/count.
        engine.write_reg(VB_FIRST, 0);
        engine.write_reg(VB_COUNT, 36);

        // Set viewport 0.
        let vp_base = VP_TRANSFORM_BASE;
        engine.write_reg(vp_base, f32::to_bits(640.0));
        engine.write_reg(vp_base + 1, f32::to_bits(-360.0));
        engine.write_reg(vp_base + 2, f32::to_bits(0.5));
        engine.write_reg(vp_base + 3, f32::to_bits(640.0));
        engine.write_reg(vp_base + 4, f32::to_bits(360.0));
        engine.write_reg(vp_base + 5, f32::to_bits(0.5));

        // Draw: begin(Triangles) + end.
        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        let d = &draws[0];
        assert_eq!(d.topology, PrimitiveTopology::Triangles);
        assert_eq!(d.vertex_first, 0);
        assert_eq!(d.vertex_count, 36);
        assert!(!d.indexed);
        assert_eq!(d.vertex_streams.len(), 1);
        assert_eq!(d.vertex_streams[0].stride, 32);
        assert_eq!(d.viewport.width, 1280.0);
    }

    #[test]
    fn test_multiple_draw_calls() {
        let mut engine = Maxwell3D::new();

        engine.write_reg(DRAW_BEGIN, 4); // Triangles
        engine.write_reg(DRAW_END, 0);
        engine.write_reg(DRAW_BEGIN, 1); // Lines
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 2);
        assert_eq!(draws[0].topology, PrimitiveTopology::Triangles);
        assert_eq!(draws[1].topology, PrimitiveTopology::Lines);

        // After take, should be empty.
        let draws2 = engine.take_draw_calls();
        assert!(draws2.is_empty());
    }

    #[test]
    fn test_draw_indexed_flag() {
        let mut engine = Maxwell3D::new();

        // Write IB count > 0 → sets draw_indexed.
        engine.write_reg(IB_BASE + IB_OFF_COUNT, 100);
        engine.write_reg(IB_BASE + IB_OFF_FORMAT, 2); // UnsignedInt

        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        assert!(draws[0].indexed);
        assert_eq!(draws[0].index_format, IndexFormat::UnsignedInt);
        assert_eq!(draws[0].index_buffer_count, 100);
    }

    // ── Enum encoding tests ──────────────────────────────────────────────

    #[test]
    fn test_comparison_op_gl_encoding() {
        // D3D encoding
        assert_eq!(ComparisonOp::from_raw(1), ComparisonOp::Never);
        assert_eq!(ComparisonOp::from_raw(2), ComparisonOp::Less);
        assert_eq!(ComparisonOp::from_raw(8), ComparisonOp::Always);
        // GL encoding
        assert_eq!(ComparisonOp::from_raw(0x200), ComparisonOp::Never);
        assert_eq!(ComparisonOp::from_raw(0x201), ComparisonOp::Less);
        assert_eq!(ComparisonOp::from_raw(0x207), ComparisonOp::Always);
        // Unknown defaults to Always
        assert_eq!(ComparisonOp::from_raw(0xFFFF), ComparisonOp::Always);
    }

    #[test]
    fn test_stencil_op_gl_encoding() {
        // D3D encoding
        assert_eq!(StencilOp::from_raw(1), StencilOp::Keep);
        assert_eq!(StencilOp::from_raw(3), StencilOp::Replace);
        assert_eq!(StencilOp::from_raw(6), StencilOp::Invert);
        // GL encoding
        assert_eq!(StencilOp::from_raw(0x1E00), StencilOp::Keep);
        assert_eq!(StencilOp::from_raw(0x1E01), StencilOp::Replace);
        assert_eq!(StencilOp::from_raw(0x150A), StencilOp::Invert);
        assert_eq!(StencilOp::from_raw(0x8507), StencilOp::Incr);
        assert_eq!(StencilOp::from_raw(0x8508), StencilOp::Decr);
    }

    #[test]
    fn test_blend_equation_gl_encoding() {
        // D3D encoding
        assert_eq!(BlendEquation::from_raw(1), BlendEquation::Add);
        assert_eq!(BlendEquation::from_raw(2), BlendEquation::Subtract);
        assert_eq!(BlendEquation::from_raw(5), BlendEquation::Max);
        // GL encoding
        assert_eq!(BlendEquation::from_raw(0x8006), BlendEquation::Add);
        assert_eq!(BlendEquation::from_raw(0x800A), BlendEquation::Subtract);
        assert_eq!(BlendEquation::from_raw(0x800B), BlendEquation::ReverseSubtract);
        assert_eq!(BlendEquation::from_raw(0x8007), BlendEquation::Min);
        assert_eq!(BlendEquation::from_raw(0x8008), BlendEquation::Max);
    }

    #[test]
    fn test_blend_factor_gl_encoding() {
        // D3D encoding
        assert_eq!(BlendFactor::from_raw(0x01), BlendFactor::Zero);
        assert_eq!(BlendFactor::from_raw(0x02), BlendFactor::One);
        assert_eq!(BlendFactor::from_raw(0x05), BlendFactor::SrcAlpha);
        assert_eq!(BlendFactor::from_raw(0x06), BlendFactor::OneMinusSrcAlpha);
        // GL encoding
        assert_eq!(BlendFactor::from_raw(0x4000), BlendFactor::Zero);
        assert_eq!(BlendFactor::from_raw(0x4001), BlendFactor::One);
        assert_eq!(BlendFactor::from_raw(0x4302), BlendFactor::SrcAlpha);
        assert_eq!(BlendFactor::from_raw(0xC001), BlendFactor::ConstantColor);
        assert_eq!(BlendFactor::from_raw(0xC903), BlendFactor::OneMinusSrc1Alpha);
    }

    #[test]
    fn test_cull_face_values() {
        assert_eq!(CullFace::from_raw(0x0404), CullFace::Front);
        assert_eq!(CullFace::from_raw(0x0405), CullFace::Back);
        assert_eq!(CullFace::from_raw(0x0408), CullFace::FrontAndBack);
        // Unknown defaults to Back
        assert_eq!(CullFace::from_raw(0x0000), CullFace::Back);
    }

    #[test]
    fn test_polygon_mode_values() {
        assert_eq!(PolygonMode::from_raw(0x1B00), PolygonMode::Point);
        assert_eq!(PolygonMode::from_raw(0x1B01), PolygonMode::Line);
        assert_eq!(PolygonMode::from_raw(0x1B02), PolygonMode::Fill);
        // Unknown defaults to Fill
        assert_eq!(PolygonMode::from_raw(0x0000), PolygonMode::Fill);
    }

    // ── Blend tests ──────────────────────────────────────────────────────

    #[test]
    fn test_blend_color_accessor() {
        let mut engine = Maxwell3D::new();
        let base = BLEND_COLOR_BASE as usize;
        engine.regs[base] = f32::to_bits(0.2);
        engine.regs[base + 1] = f32::to_bits(0.4);
        engine.regs[base + 2] = f32::to_bits(0.6);
        engine.regs[base + 3] = f32::to_bits(0.8);

        let bc = engine.blend_color_info();
        assert_eq!(bc.r, 0.2);
        assert_eq!(bc.g, 0.4);
        assert_eq!(bc.b, 0.6);
        assert_eq!(bc.a, 0.8);
    }

    #[test]
    fn test_global_blend_info() {
        let mut engine = Maxwell3D::new();
        let base = BLEND_BASE as usize;

        // Enable blend for RT0.
        engine.regs[base + 9] = 1; // enable[0]
        // Set separate alpha, color Add SrcAlpha/OneMinusSrcAlpha, alpha Add One/Zero.
        engine.regs[base] = 1; // separate_alpha
        engine.regs[base + 1] = 1; // color_op = Add (D3D)
        engine.regs[base + 2] = 0x05; // color_src = SrcAlpha (D3D)
        engine.regs[base + 3] = 0x06; // color_dst = OneMinusSrcAlpha (D3D)
        engine.regs[base + 4] = 1; // alpha_op = Add
        engine.regs[base + 5] = 0x02; // alpha_src = One
        engine.regs[base + 7] = 0x01; // alpha_dst = Zero

        let bi = engine.global_blend_info(0);
        assert!(bi.enabled);
        assert!(bi.separate_alpha);
        assert_eq!(bi.color_op, BlendEquation::Add);
        assert_eq!(bi.color_src, BlendFactor::SrcAlpha);
        assert_eq!(bi.color_dst, BlendFactor::OneMinusSrcAlpha);
        assert_eq!(bi.alpha_op, BlendEquation::Add);
        assert_eq!(bi.alpha_src, BlendFactor::One);
        assert_eq!(bi.alpha_dst, BlendFactor::Zero);
    }

    #[test]
    fn test_blend_enable_per_rt() {
        let mut engine = Maxwell3D::new();
        let base = BLEND_BASE as usize;

        // Enable RT0, RT3, RT7.
        engine.regs[base + 9] = 1; // RT0
        engine.regs[base + 12] = 1; // RT3
        engine.regs[base + 16] = 1; // RT7

        assert!(engine.blend_enable(0));
        assert!(!engine.blend_enable(1));
        assert!(!engine.blend_enable(2));
        assert!(engine.blend_enable(3));
        assert!(!engine.blend_enable(4));
        assert!(!engine.blend_enable(5));
        assert!(!engine.blend_enable(6));
        assert!(engine.blend_enable(7));
    }

    #[test]
    fn test_blend_per_target_info() {
        let mut engine = Maxwell3D::new();

        // Enable per-target blend override.
        engine.regs[BLEND_PER_TARGET_ENABLED as usize] = 1;

        // Set per-target blend for RT2.
        let rt2_base = (BLEND_PER_TARGET_BASE + 2 * BLEND_PER_TARGET_STRIDE) as usize;
        engine.regs[rt2_base] = 0; // no separate_alpha
        engine.regs[rt2_base + 1] = 2; // color_op = Subtract
        engine.regs[rt2_base + 2] = 0x09; // color_src = DstColor
        engine.regs[rt2_base + 3] = 0x01; // color_dst = Zero
        engine.regs[rt2_base + 4] = 1; // alpha_op = Add
        engine.regs[rt2_base + 5] = 0x02; // alpha_src = One
        engine.regs[rt2_base + 6] = 0x02; // alpha_dst = One

        // Enable RT2 blend.
        engine.regs[(BLEND_BASE + 11) as usize] = 1; // enable[2]

        let bi = engine.effective_blend_info(2);
        assert!(bi.enabled);
        assert!(!bi.separate_alpha);
        assert_eq!(bi.color_op, BlendEquation::Subtract);
        assert_eq!(bi.color_src, BlendFactor::DstColor);
        assert_eq!(bi.color_dst, BlendFactor::Zero);
    }

    // ── Depth/Stencil tests ──────────────────────────────────────────────

    #[test]
    fn test_depth_state() {
        let mut engine = Maxwell3D::new();
        engine.regs[DEPTH_TEST_ENABLE as usize] = 1;
        engine.regs[DEPTH_WRITE_ENABLE as usize] = 1;
        engine.regs[DEPTH_TEST_FUNC as usize] = 2; // Less (D3D)
        engine.regs[DEPTH_MODE as usize] = 1; // ZeroToOne

        let ds = engine.depth_stencil_info();
        assert!(ds.depth_test_enable);
        assert!(ds.depth_write_enable);
        assert_eq!(ds.depth_func, ComparisonOp::Less);
        assert_eq!(ds.depth_mode, DepthMode::ZeroToOne);
    }

    #[test]
    fn test_stencil_front_state() {
        let mut engine = Maxwell3D::new();
        engine.regs[STENCIL_ENABLE as usize] = 1;

        let front_base = STENCIL_FRONT_OP_BASE as usize;
        engine.regs[front_base] = 1; // fail = Keep (D3D)
        engine.regs[front_base + 1] = 1; // zfail = Keep
        engine.regs[front_base + 2] = 3; // zpass = Replace
        engine.regs[front_base + 3] = 8; // func = Always
        engine.regs[STENCIL_FRONT_REF as usize] = 0xFF;
        engine.regs[STENCIL_FRONT_FUNC_MASK as usize] = 0xFF;
        engine.regs[STENCIL_FRONT_MASK as usize] = 0xFF;

        let ds = engine.depth_stencil_info();
        assert!(ds.stencil_enable);
        assert_eq!(ds.front.fail_op, StencilOp::Keep);
        assert_eq!(ds.front.zpass_op, StencilOp::Replace);
        assert_eq!(ds.front.func, ComparisonOp::Always);
        assert_eq!(ds.front.ref_value, 0xFF);
        assert_eq!(ds.front.func_mask, 0xFF);
        assert_eq!(ds.front.write_mask, 0xFF);
    }

    #[test]
    fn test_stencil_two_side() {
        let mut engine = Maxwell3D::new();
        engine.regs[STENCIL_ENABLE as usize] = 1;
        engine.regs[STENCIL_TWO_SIDE_ENABLE as usize] = 1;

        // Front: Replace on pass.
        let front_base = STENCIL_FRONT_OP_BASE as usize;
        engine.regs[front_base + 2] = 3; // zpass = Replace
        engine.regs[front_base + 3] = 8; // func = Always

        // Back: Invert on pass.
        let back_base = STENCIL_BACK_OP_BASE as usize;
        engine.regs[back_base + 2] = 6; // zpass = Invert
        engine.regs[back_base + 3] = 2; // func = Less
        engine.regs[STENCIL_BACK_REF as usize] = 0x80;

        let ds = engine.depth_stencil_info();
        assert!(ds.stencil_two_side);
        assert_eq!(ds.front.zpass_op, StencilOp::Replace);
        assert_eq!(ds.back.zpass_op, StencilOp::Invert);
        assert_eq!(ds.back.func, ComparisonOp::Less);
        assert_eq!(ds.back.ref_value, 0x80);
    }

    // ── Rasterizer tests ─────────────────────────────────────────────────

    #[test]
    fn test_rasterizer_state() {
        let mut engine = Maxwell3D::new();
        engine.regs[CULL_TEST_ENABLE as usize] = 1;
        engine.regs[FRONT_FACE as usize] = 0x0901; // CCW
        engine.regs[CULL_FACE as usize] = 0x0405; // Back
        engine.regs[POLYGON_MODE_FRONT as usize] = 0x1B02; // Fill
        engine.regs[POLYGON_MODE_BACK as usize] = 0x1B02; // Fill
        engine.regs[LINE_WIDTH_SMOOTH as usize] = f32::to_bits(1.0);
        engine.regs[LINE_WIDTH_ALIASED as usize] = f32::to_bits(1.0);

        let ri = engine.rasterizer_info();
        assert!(ri.cull_enable);
        assert_eq!(ri.front_face, FrontFace::CCW);
        assert_eq!(ri.cull_face, CullFace::Back);
        assert_eq!(ri.polygon_mode_front, PolygonMode::Fill);
        assert_eq!(ri.polygon_mode_back, PolygonMode::Fill);
        assert_eq!(ri.line_width_smooth, 1.0);
    }

    #[test]
    fn test_rasterizer_wireframe() {
        let mut engine = Maxwell3D::new();
        engine.regs[POLYGON_MODE_FRONT as usize] = 0x1B01; // Line
        engine.regs[POLYGON_MODE_BACK as usize] = 0x1B01; // Line
        engine.regs[DEPTH_BIAS as usize] = f32::to_bits(0.5);
        engine.regs[SLOPE_SCALE_DEPTH_BIAS as usize] = f32::to_bits(1.5);
        engine.regs[DEPTH_BIAS_CLAMP as usize] = f32::to_bits(0.01);

        let ri = engine.rasterizer_info();
        assert_eq!(ri.polygon_mode_front, PolygonMode::Line);
        assert_eq!(ri.polygon_mode_back, PolygonMode::Line);
        assert_eq!(ri.depth_bias, 0.5);
        assert_eq!(ri.slope_scale_depth_bias, 1.5);
        assert_eq!(ri.depth_bias_clamp, 0.01);
    }

    // ── Constant buffer tests ────────────────────────────────────────────

    #[test]
    fn test_cb_bind() {
        let mut engine = Maxwell3D::new();

        // Set CB config: size=0x10000, addr=0x0000_0001_0000_0000.
        engine.write_reg(CB_CONFIG_BASE, 0x10000); // size
        engine.write_reg(CB_CONFIG_BASE + 1, 0x0001); // addr_high
        engine.write_reg(CB_CONFIG_BASE + 2, 0x0000_0000); // addr_low

        // Bind to stage 0 (vertex), slot 3: raw_config = valid | (3 << 4).
        let raw_config = 1 | (3 << 4);
        engine.write_reg(CB_BIND_TRIGGER_0, raw_config);

        let bindings = engine.const_buffer_bindings(0);
        assert!(bindings[3].enabled);
        assert_eq!(bindings[3].address, 0x0001_0000_0000);
        assert_eq!(bindings[3].size, 0x10000);
        // Other slots should be disabled.
        assert!(!bindings[0].enabled);
        assert!(!bindings[1].enabled);
    }

    #[test]
    fn test_cb_data_increments_offset() {
        let mut engine = Maxwell3D::new();

        // Set initial offset.
        engine.write_reg(CB_CONFIG_BASE + 3, 0x100); // offset = 0x100

        // Write CB_DATA — should auto-increment offset by 4 each time.
        engine.write_reg(CB_DATA_BASE, 0xAAAA);
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 3) as usize], 0x104);

        engine.write_reg(CB_DATA_BASE, 0xBBBB);
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 3) as usize], 0x108);
    }

    #[test]
    fn test_cb_bind_multiple_stages() {
        let mut engine = Maxwell3D::new();

        // Bind CB to stage 0 slot 0.
        engine.write_reg(CB_CONFIG_BASE, 256);
        engine.write_reg(CB_CONFIG_BASE + 1, 0);
        engine.write_reg(CB_CONFIG_BASE + 2, 0x1000);
        engine.write_reg(CB_BIND_TRIGGER_0, 1 | (0 << 4));

        // Bind CB to stage 4 (fragment) slot 5.
        engine.write_reg(CB_CONFIG_BASE, 512);
        engine.write_reg(CB_CONFIG_BASE + 1, 0);
        engine.write_reg(CB_CONFIG_BASE + 2, 0x2000);
        engine.write_reg(CB_BIND_TRIGGER_4, 1 | (5 << 4));

        assert!(engine.const_buffer_bindings(0)[0].enabled);
        assert_eq!(engine.const_buffer_bindings(0)[0].address, 0x1000);
        assert_eq!(engine.const_buffer_bindings(0)[0].size, 256);

        assert!(engine.const_buffer_bindings(4)[5].enabled);
        assert_eq!(engine.const_buffer_bindings(4)[5].address, 0x2000);
        assert_eq!(engine.const_buffer_bindings(4)[5].size, 512);

        // Other stages should be unaffected.
        assert!(!engine.const_buffer_bindings(1)[0].enabled);
        assert!(!engine.const_buffer_bindings(2)[0].enabled);
    }

    // ── Shader program test ──────────────────────────────────────────────

    #[test]
    fn test_program_base_address() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(PROGRAM_REGION_BASE, 0x0002); // addr_high
        engine.write_reg(PROGRAM_REGION_BASE + 1, 0xABCD_0000); // addr_low

        assert_eq!(engine.program_base_address(), 0x0002_ABCD_0000);
    }

    // ── Draw integration tests ───────────────────────────────────────────

    #[test]
    fn test_draw_captures_depth_stencil() {
        let mut engine = Maxwell3D::new();

        // Set depth state.
        engine.write_reg(DEPTH_TEST_ENABLE, 1);
        engine.write_reg(DEPTH_WRITE_ENABLE, 1);
        engine.write_reg(DEPTH_TEST_FUNC, 0x201); // Less (GL)
        engine.write_reg(DEPTH_MODE, 1); // ZeroToOne

        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        let ds = &draws[0].depth_stencil;
        assert!(ds.depth_test_enable);
        assert!(ds.depth_write_enable);
        assert_eq!(ds.depth_func, ComparisonOp::Less);
        assert_eq!(ds.depth_mode, DepthMode::ZeroToOne);
    }

    #[test]
    fn test_draw_captures_blend() {
        let mut engine = Maxwell3D::new();
        let base = BLEND_BASE as usize;

        // Enable blend for RT0 with SrcAlpha/OneMinusSrcAlpha.
        engine.regs[base + 9] = 1; // enable[0]
        engine.regs[base + 1] = 1; // color_op = Add
        engine.regs[base + 2] = 0x05; // color_src = SrcAlpha
        engine.regs[base + 3] = 0x06; // color_dst = OneMinusSrcAlpha
        engine.regs[base + 4] = 1; // alpha_op = Add
        engine.regs[base + 5] = 0x02; // alpha_src = One
        engine.regs[base + 7] = 0x01; // alpha_dst = Zero

        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        assert!(draws[0].blend[0].enabled);
        assert_eq!(draws[0].blend[0].color_src, BlendFactor::SrcAlpha);
        assert_eq!(draws[0].blend[0].color_dst, BlendFactor::OneMinusSrcAlpha);
        // RT1 should not be enabled.
        assert!(!draws[0].blend[1].enabled);
    }

    // ── Vertex attribute enum tests ─────────────────────────────────────

    #[test]
    fn test_vertex_attrib_size_values() {
        assert_eq!(VertexAttribSize::from_raw(0x01), VertexAttribSize::R32G32B32A32);
        assert_eq!(VertexAttribSize::from_raw(0x02), VertexAttribSize::R32G32B32);
        assert_eq!(VertexAttribSize::from_raw(0x03), VertexAttribSize::R16G16B16A16);
        assert_eq!(VertexAttribSize::from_raw(0x04), VertexAttribSize::R32G32);
        assert_eq!(VertexAttribSize::from_raw(0x0A), VertexAttribSize::R8G8B8A8);
        assert_eq!(VertexAttribSize::from_raw(0x12), VertexAttribSize::R32);
        assert_eq!(VertexAttribSize::from_raw(0x1D), VertexAttribSize::R8);
        assert_eq!(VertexAttribSize::from_raw(0x30), VertexAttribSize::A2B10G10R10);
        assert_eq!(VertexAttribSize::from_raw(0x31), VertexAttribSize::B10G11R11);
        assert_eq!(VertexAttribSize::from_raw(0x34), VertexAttribSize::A8);
        assert_eq!(VertexAttribSize::from_raw(0xFF), VertexAttribSize::Invalid);
    }

    #[test]
    fn test_vertex_attrib_size_bytes() {
        assert_eq!(VertexAttribSize::R32G32B32A32.size_bytes(), 16);
        assert_eq!(VertexAttribSize::R32G32B32.size_bytes(), 12);
        assert_eq!(VertexAttribSize::R16G16B16A16.size_bytes(), 8);
        assert_eq!(VertexAttribSize::R32G32.size_bytes(), 8);
        assert_eq!(VertexAttribSize::R8G8B8A8.size_bytes(), 4);
        assert_eq!(VertexAttribSize::R32.size_bytes(), 4);
        assert_eq!(VertexAttribSize::R8.size_bytes(), 1);
        assert_eq!(VertexAttribSize::A2B10G10R10.size_bytes(), 4);
        assert_eq!(VertexAttribSize::Invalid.size_bytes(), 0);
    }

    #[test]
    fn test_vertex_attrib_size_component_count() {
        assert_eq!(VertexAttribSize::R32G32B32A32.component_count(), 4);
        assert_eq!(VertexAttribSize::R32G32B32.component_count(), 3);
        assert_eq!(VertexAttribSize::R32G32.component_count(), 2);
        assert_eq!(VertexAttribSize::R32.component_count(), 1);
        assert_eq!(VertexAttribSize::R8G8B8A8.component_count(), 4);
        assert_eq!(VertexAttribSize::B10G11R11.component_count(), 3);
        assert_eq!(VertexAttribSize::G8R8.component_count(), 2);
        assert_eq!(VertexAttribSize::A8.component_count(), 1);
        assert_eq!(VertexAttribSize::Invalid.component_count(), 0);
    }

    #[test]
    fn test_vertex_attrib_type_values() {
        assert_eq!(VertexAttribType::from_raw(1), VertexAttribType::SNorm);
        assert_eq!(VertexAttribType::from_raw(2), VertexAttribType::UNorm);
        assert_eq!(VertexAttribType::from_raw(3), VertexAttribType::SInt);
        assert_eq!(VertexAttribType::from_raw(4), VertexAttribType::UInt);
        assert_eq!(VertexAttribType::from_raw(5), VertexAttribType::UScaled);
        assert_eq!(VertexAttribType::from_raw(6), VertexAttribType::SScaled);
        assert_eq!(VertexAttribType::from_raw(7), VertexAttribType::Float);
        assert_eq!(VertexAttribType::from_raw(0), VertexAttribType::Invalid);
        assert_eq!(VertexAttribType::from_raw(99), VertexAttribType::Invalid);
    }

    #[test]
    fn test_shader_stage_type_values() {
        assert_eq!(ShaderStageType::from_raw(0), ShaderStageType::VertexA);
        assert_eq!(ShaderStageType::from_raw(1), ShaderStageType::VertexB);
        assert_eq!(ShaderStageType::from_raw(2), ShaderStageType::TessInit);
        assert_eq!(ShaderStageType::from_raw(3), ShaderStageType::Tessellation);
        assert_eq!(ShaderStageType::from_raw(4), ShaderStageType::Geometry);
        assert_eq!(ShaderStageType::from_raw(5), ShaderStageType::Fragment);
        assert_eq!(ShaderStageType::from_raw(99), ShaderStageType::Invalid);
    }

    // ── Vertex attribute accessor tests ─────────────────────────────────

    #[test]
    fn test_vertex_attrib_info() {
        let mut engine = Maxwell3D::new();

        // Attrib 0: buffer=3, not constant, offset=16, size=R32G32B32A32(0x01),
        // type=Float(7), no bgra.
        // bits[4:0]=3, bit[6]=0, bits[20:7]=16, bits[26:21]=0x01, bits[29:27]=7, bit[31]=0
        let raw = 3u32
            | (16 << 7)
            | (0x01 << 21)
            | (7 << 27);
        engine.regs[VERTEX_ATTRIB_BASE as usize] = raw;

        let info = engine.vertex_attrib_info(0);
        assert_eq!(info.buffer_index, 3);
        assert!(!info.constant);
        assert_eq!(info.offset, 16);
        assert_eq!(info.size, VertexAttribSize::R32G32B32A32);
        assert_eq!(info.attrib_type, VertexAttribType::Float);
        assert!(!info.bgra);
    }

    #[test]
    fn test_vertex_attrib_constant_bgra() {
        let mut engine = Maxwell3D::new();

        // Attrib 5: buffer=0, constant=true, offset=0, size=R8G8B8A8(0x0A),
        // type=UNorm(2), bgra=true.
        let raw = 0u32
            | (1 << 6)       // constant
            | (0x0A << 21)   // R8G8B8A8
            | (2 << 27)      // UNorm
            | (1 << 31);     // bgra
        engine.regs[(VERTEX_ATTRIB_BASE + 5) as usize] = raw;

        let info = engine.vertex_attrib_info(5);
        assert_eq!(info.buffer_index, 0);
        assert!(info.constant);
        assert_eq!(info.offset, 0);
        assert_eq!(info.size, VertexAttribSize::R8G8B8A8);
        assert_eq!(info.attrib_type, VertexAttribType::UNorm);
        assert!(info.bgra);
    }

    // ── Shader stage accessor tests ─────────────────────────────────────

    #[test]
    fn test_shader_stage_info() {
        let mut engine = Maxwell3D::new();
        let base = (PIPELINE_BASE + 1 * PIPELINE_STRIDE) as usize; // VertexB slot

        // word0: enabled=1, type=VertexB(1) at bits[7:4]
        engine.regs[base] = 1 | (1 << 4);
        engine.regs[base + 1] = 0x100; // offset
        engine.regs[base + 3] = 64;    // register_count
        engine.regs[base + 4] = 0;     // binding_group

        let info = engine.shader_stage_info(1);
        assert!(info.enabled);
        assert_eq!(info.program_type, ShaderStageType::VertexB);
        assert_eq!(info.offset, 0x100);
        assert_eq!(info.register_count, 64);
        assert_eq!(info.binding_group, 0);
    }

    #[test]
    fn test_shader_stage_fragment() {
        let mut engine = Maxwell3D::new();
        let base = (PIPELINE_BASE + 5 * PIPELINE_STRIDE) as usize; // Fragment slot

        engine.regs[base] = 1 | (5 << 4); // enabled, Fragment
        engine.regs[base + 1] = 0x500;
        engine.regs[base + 3] = 32;
        engine.regs[base + 4] = 2;

        let info = engine.shader_stage_info(5);
        assert!(info.enabled);
        assert_eq!(info.program_type, ShaderStageType::Fragment);
        assert_eq!(info.offset, 0x500);
        assert_eq!(info.register_count, 32);
        assert_eq!(info.binding_group, 2);
    }

    #[test]
    fn test_shader_stage_vertexb_always_enabled() {
        let engine = Maxwell3D::new();
        // VertexB (index 1) always returns enabled even with zero registers.
        assert!(engine.is_shader_stage_enabled(1));
        // Other stages default to disabled.
        assert!(!engine.is_shader_stage_enabled(0));
        assert!(!engine.is_shader_stage_enabled(2));
        assert!(!engine.is_shader_stage_enabled(5));
    }

    // ── Color mask tests ────────────────────────────────────────────────

    #[test]
    fn test_color_mask_info() {
        let mut engine = Maxwell3D::new();
        // RT0: R and A only. R=bit[0], G=bit[4], B=bit[8], A=bit[12].
        engine.regs[COLOR_MASK_BASE as usize] = (1 << 0) | (1 << 12);

        let mask = engine.color_mask_info(0);
        assert!(mask.r);
        assert!(!mask.g);
        assert!(!mask.b);
        assert!(mask.a);
    }

    #[test]
    fn test_color_mask_common() {
        let mut engine = Maxwell3D::new();
        // Enable common mask mode.
        engine.regs[COLOR_MASK_COMMON as usize] = 1;
        // Set mask[0] to G+B only.
        engine.regs[COLOR_MASK_BASE as usize] = (1 << 4) | (1 << 8);
        // Set mask[3] differently (should be ignored in common mode).
        engine.regs[(COLOR_MASK_BASE + 3) as usize] = 0xFFFF;

        let mask3 = engine.color_mask_info(3);
        // Should use mask[0], not mask[3].
        assert!(!mask3.r);
        assert!(mask3.g);
        assert!(mask3.b);
        assert!(!mask3.a);
    }

    #[test]
    fn test_color_mask_per_rt() {
        let mut engine = Maxwell3D::new();
        // Common mode off (default).
        // RT0: all channels.
        engine.regs[COLOR_MASK_BASE as usize] = (1 << 0) | (1 << 4) | (1 << 8) | (1 << 12);
        // RT2: R only.
        engine.regs[(COLOR_MASK_BASE + 2) as usize] = 1 << 0;

        let mask0 = engine.color_mask_info(0);
        assert!(mask0.r && mask0.g && mask0.b && mask0.a);

        let mask2 = engine.color_mask_info(2);
        assert!(mask2.r);
        assert!(!mask2.g);
        assert!(!mask2.b);
        assert!(!mask2.a);
    }

    // ── RT control tests ────────────────────────────────────────────────

    #[test]
    fn test_rt_control_info() {
        let mut engine = Maxwell3D::new();
        // count=2, map[0]=0, map[1]=1 (identity).
        // bits[3:0]=2, bits[6:4]=0, bits[9:7]=1
        engine.regs[RT_CONTROL as usize] = 2 | (0 << 4) | (1 << 7);

        let rtc = engine.rt_control_info();
        assert_eq!(rtc.count, 2);
        assert_eq!(rtc.map[0], 0);
        assert_eq!(rtc.map[1], 1);
    }

    #[test]
    fn test_rt_control_swizzled() {
        let mut engine = Maxwell3D::new();
        // count=3, map[0]=2, map[1]=0, map[2]=1 (swizzled).
        // bits[3:0]=3, bits[6:4]=2, bits[9:7]=0, bits[12:10]=1
        engine.regs[RT_CONTROL as usize] = 3 | (2 << 4) | (0 << 7) | (1 << 10);

        let rtc = engine.rt_control_info();
        assert_eq!(rtc.count, 3);
        assert_eq!(rtc.map[0], 2);
        assert_eq!(rtc.map[1], 0);
        assert_eq!(rtc.map[2], 1);
    }

    // ── Draw integration tests for new state ────────────────────────────

    #[test]
    fn test_draw_captures_vertex_attribs() {
        let mut engine = Maxwell3D::new();

        // Set attrib 0: buffer=0, offset=0, R32G32B32(0x02), Float(7).
        let raw0 = 0u32 | (0x02 << 21) | (7 << 27);
        engine.write_reg(VERTEX_ATTRIB_BASE, raw0);

        // Set attrib 1: buffer=0, offset=12, R8G8B8A8(0x0A), UNorm(2).
        let raw1 = 0u32 | (12 << 7) | (0x0A << 21) | (2 << 27);
        engine.write_reg(VERTEX_ATTRIB_BASE + 1, raw1);

        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws[0].vertex_attribs.len(), 2);
        assert_eq!(draws[0].vertex_attribs[0].size, VertexAttribSize::R32G32B32);
        assert_eq!(draws[0].vertex_attribs[0].attrib_type, VertexAttribType::Float);
        assert_eq!(draws[0].vertex_attribs[1].offset, 12);
        assert_eq!(draws[0].vertex_attribs[1].size, VertexAttribSize::R8G8B8A8);
    }

    #[test]
    fn test_draw_captures_shader_stages() {
        let mut engine = Maxwell3D::new();

        // Enable VertexB (slot 1) and Fragment (slot 5).
        let vb_base = PIPELINE_BASE + 1 * PIPELINE_STRIDE;
        engine.write_reg(vb_base, 1 | (1 << 4));
        engine.write_reg(vb_base + 1, 0x100);
        engine.write_reg(vb_base + 3, 64);

        let frag_base = PIPELINE_BASE + 5 * PIPELINE_STRIDE;
        engine.write_reg(frag_base, 1 | (5 << 4));
        engine.write_reg(frag_base + 1, 0x500);
        engine.write_reg(frag_base + 3, 32);

        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert!(draws[0].shader_stages[1].enabled);
        assert_eq!(draws[0].shader_stages[1].program_type, ShaderStageType::VertexB);
        assert_eq!(draws[0].shader_stages[1].offset, 0x100);
        assert!(draws[0].shader_stages[5].enabled);
        assert_eq!(draws[0].shader_stages[5].program_type, ShaderStageType::Fragment);
        // Slot 0 should be disabled.
        assert!(!draws[0].shader_stages[0].enabled);
    }

    #[test]
    fn test_draw_captures_color_masks_and_rt_control() {
        let mut engine = Maxwell3D::new();

        // Set RT0 mask: R+G only.
        engine.write_reg(COLOR_MASK_BASE, (1 << 0) | (1 << 4));
        // Set RT control: count=1, map[0]=0.
        engine.write_reg(RT_CONTROL, 1 | (0 << 4));

        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert!(draws[0].color_masks[0].r);
        assert!(draws[0].color_masks[0].g);
        assert!(!draws[0].color_masks[0].b);
        assert!(!draws[0].color_masks[0].a);
        assert_eq!(draws[0].rt_control.count, 1);
        assert_eq!(draws[0].rt_control.map[0], 0);
    }
}
