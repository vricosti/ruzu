// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell 3D engine — structured state tracking, clear operations, and draw
//! call recording.
//!
//! This is the main 3D rendering engine (NV class B197). It handles render
//! target configuration, clear operations, and draw call state tracking.
//! Register writes are stored in a flat array and side-effect methods (clear,
//! draw begin/end) are triggered on specific register writes.

use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

use parking_lot::Mutex;

use super::engine_interface::{EngineInterface, EngineInterfaceState};
use super::engine_upload;
use super::{ClassId, Engine, Framebuffer, PendingWrite, ENGINE_REG_COUNT};
use crate::descriptor_table::{TicTable, TscTable};
use crate::macro_engine::macro_engine::{get_macro_engine, MacroEngine};
use crate::macro_engine::macro_interpreter::MacroInterpreterImpl;
use crate::memory_manager::MemoryManager;
use crate::query_cache::types::QueryPropertiesFlags;
use crate::rasterizer_interface::RasterizerInterface;

#[derive(Clone, Copy)]
struct Maxwell3DPtr(*mut Maxwell3D);

unsafe impl Send for Maxwell3DPtr {}

static MAXWELL3D_TRACE_COUNT: AtomicU32 = AtomicU32::new(0);

fn should_trace_dma_flow() -> bool {
    std::env::var_os("RUZU_TRACE_DMA_FLOW").is_some()
}

impl Maxwell3DPtr {
    unsafe fn call_method(self, address: u32, value: u32) {
        (&mut *self.0).call_method(address, value, true);
    }

    unsafe fn read_reg(self, method: u32) -> u32 {
        let gpu = &*self.0;
        let idx = method as usize;
        if idx < ENGINE_REG_COUNT {
            gpu.regs[idx]
        } else {
            0
        }
    }
}

// ── Register offset constants (method addresses) ────────────────────────────

/// Render target array base. 8 targets, 0x10 words (0x40 bytes) each.
/// Convert upstream byte offset (from ASSERT_REG_POSITION) to word index.
/// Matches upstream `MAXWELL3D_REG_INDEX(field) = offsetof(Regs, field) / sizeof(u32)`.
macro_rules! reg_index {
    ($byte_offset:expr) => {
        $byte_offset / 4
    };
    ($base:expr, +$field:expr) => {
        $base / 4 + $field
    };
}
const RT_BASE: u32 = reg_index!(0x0800);
/// Words per render target.
const RT_STRIDE: u32 = 0x10;

// Offsets within each render target (relative to RT_BASE + i*RT_STRIDE):
const RT_OFF_ADDRESS_HIGH: u32 = 0x00;
const RT_OFF_ADDRESS_LOW: u32 = 0x01;
const RT_OFF_WIDTH: u32 = 0x02;
const RT_OFF_HEIGHT: u32 = 0x03;
const RT_OFF_FORMAT: u32 = 0x04;

/// Clear color RGBA: 4 consecutive f32-as-u32 registers.
const CLEAR_COLOR_BASE: u32 = reg_index!(0x0D80);
/// Clear depth value (f32 as u32 bits).
#[allow(dead_code)]
const CLEAR_DEPTH: u32 = reg_index!(0x0D90);
/// Clear stencil value.
#[allow(dead_code)]
const CLEAR_STENCIL: u32 = reg_index!(0x0DA0);
/// Clear surface trigger register.
const CLEAR_SURFACE: u32 = reg_index!(0x19D0);
const RASTERIZE_ENABLE: u32 = reg_index!(0x037C);

// ── Viewport registers ──────────────────────────────────────────────────────

/// Viewport transform base. 16 viewports, 8 words each.
/// Words: scale_x, scale_y, scale_z, translate_x, translate_y, translate_z, swizzle, snap.
const VP_TRANSFORM_BASE: u32 = reg_index!(0x0A00);
const VP_TRANSFORM_STRIDE: u32 = 8;

// ── Scissor registers ───────────────────────────────────────────────────────

/// Scissor base. 16 scissors, 4 words each.
/// Words: enable, min_x|max_x(packed), min_y|max_y(packed), pad.
const SCISSOR_BASE: u32 = reg_index!(0x0E00);
const SCISSOR_STRIDE: u32 = 4;

// ── Vertex buffer registers ─────────────────────────────────────────────────

/// Vertex buffer first vertex.
const VB_FIRST: u32 = reg_index!(0x0D74); // upstream byte 0x0D74
/// Vertex buffer vertex count.
const VB_COUNT: u32 = reg_index!(0x0D78); // upstream byte 0x0D78

/// Vertex stream array base. 32 streams, 4 words each.
/// Words: stride|enable, addr_high, addr_low, frequency.
const VERTEX_STREAM_BASE: u32 = reg_index!(0x1C00);
const VERTEX_STREAM_STRIDE: u32 = 4;

/// Vertex stream limit array base. 32 streams, 2 words each.
#[allow(dead_code)]
const VERTEX_STREAM_LIMIT_BASE: u32 = reg_index!(0x1F00);

// ── Index buffer registers ──────────────────────────────────────────────────

/// Index buffer base (7 words).
/// Words: addr_high, addr_low, limit_high, limit_low, format, first, count.
const IB_BASE: u32 = reg_index!(0x17C8);
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
const DRAW_END: u32 = reg_index!(0x1614); // upstream byte 0x1614
/// Draw begin: sets topology and instance mode.
const DRAW_BEGIN: u32 = reg_index!(0x1618); // upstream byte 0x1618

/// Signed base vertex offset for indexed draws (i32).
const GLOBAL_BASE_VERTEX_INDEX: u32 = reg_index!(0x1434);
/// Base instance offset for instanced draws.
const GLOBAL_BASE_INSTANCE_INDEX: u32 = reg_index!(0x1438);
/// Each write pushes 4 LE bytes of inline index data.
const DRAW_INLINE_INDEX: u32 = reg_index!(0x15E8);

// ── Report semaphore registers ────────────────────────────────────────────────

/// Report semaphore block: 4 words (addr_high, addr_low, payload, query).
/// Writing to REPORT_SEMAPHORE_BASE + 3 triggers the operation.
const REPORT_SEMAPHORE_BASE: u32 = reg_index!(0x06C0);
/// Trigger register for report semaphore (writing here fires the operation).
const REPORT_SEMAPHORE_TRIGGER: u32 = REPORT_SEMAPHORE_BASE + 3;

// ── Depth/Stencil registers ─────────────────────────────────────────────────

const DEPTH_MODE: u32 = reg_index!(0x0D7C);
const DEPTH_TEST_ENABLE: u32 = reg_index!(0x12CC);
const DEPTH_WRITE_ENABLE: u32 = reg_index!(0x12E8);
const DEPTH_TEST_FUNC: u32 = reg_index!(0x130C);
const POINT_SIZE: u32 = reg_index!(0x1518);

const STENCIL_ENABLE: u32 = reg_index!(0x1380);
const STENCIL_FRONT_OP_BASE: u32 = reg_index!(0x1384);
const STENCIL_FRONT_REF: u32 = reg_index!(0x1394);
const STENCIL_FRONT_FUNC_MASK: u32 = reg_index!(0x1398);
const STENCIL_FRONT_MASK: u32 = reg_index!(0x139C);

const STENCIL_TWO_SIDE_ENABLE: u32 = reg_index!(0x1594);
const STENCIL_BACK_OP_BASE: u32 = reg_index!(0x1598);
const STENCIL_BACK_REF: u32 = reg_index!(0x0F54);
const STENCIL_BACK_MASK: u32 = reg_index!(0x0F58);
const STENCIL_BACK_FUNC_MASK: u32 = reg_index!(0x0F5C);

// ── Blend registers ─────────────────────────────────────────────────────────

/// 4 consecutive f32 registers: R, G, B, A blend constant color.
const BLEND_COLOR_BASE: u32 = reg_index!(0x131C);

/// Global blend struct base.
/// +0 separate_alpha, +1 color_op, +2 color_src, +3 color_dst,
/// +4 alpha_op, +5 alpha_src, +6 (color_key), +7 alpha_dst,
/// +8 single_rop_ctrl, +9..+16 enable[0..7]
const BLEND_BASE: u32 = reg_index!(0x133C);

/// Whether per-target blend overrides are active.
const BLEND_PER_TARGET_ENABLED: u32 = reg_index!(0x12E4);

/// Per-target blend base. 8 entries, stride 8.
/// +0 sep_alpha, +1 color_op, +2 color_src, +3 color_dst,
/// +4 alpha_op, +5 alpha_src, +6 alpha_dst
const BLEND_PER_TARGET_BASE: u32 = reg_index!(0x1E00);
const BLEND_PER_TARGET_STRIDE: u32 = 8;

// ── Rasterizer registers ────────────────────────────────────────────────────

const POLYGON_MODE_FRONT: u32 = reg_index!(0x0DAC);
const POLYGON_MODE_BACK: u32 = reg_index!(0x0DB0);
const LINE_WIDTH_SMOOTH: u32 = reg_index!(0x13B0);
const LINE_WIDTH_ALIASED: u32 = reg_index!(0x13B4);
const SLOPE_SCALE_DEPTH_BIAS: u32 = reg_index!(0x156C);
const DEPTH_BIAS: u32 = reg_index!(0x15BC);
const DEPTH_BIAS_CLAMP: u32 = reg_index!(0x187C);
const CULL_TEST_ENABLE: u32 = reg_index!(0x1918);
const FRONT_FACE: u32 = reg_index!(0x191C);
const CULL_FACE: u32 = reg_index!(0x1920);
const FRAMEBUFFER_SRGB: u32 = reg_index!(0x15B8);

// ── Shader program registers ────────────────────────────────────────────────

/// Program region base: addr_high at +0, addr_low at +1.
const PROGRAM_REGION_BASE: u32 = reg_index!(0x1608);

// ── Vertex attribute registers ────────────────────────────────────────────

/// Vertex attribute array base. 32 entries, 1 word each.
/// Per entry: bits[4:0]=buffer, bit[6]=constant, bits[20:7]=offset,
///            bits[26:21]=size, bits[29:27]=type, bit[31]=bgra.
const VERTEX_ATTRIB_BASE: u32 = reg_index!(0x1160);
const NUM_VERTEX_ATTRIBS: u32 = 32;

// ── Shader pipeline registers ─────────────────────────────────────────────

/// Shader pipeline base. 6 stages, 0x10 words each.
/// Per stage: +0 packed(enable|type), +1 offset, +3 register_count, +4 binding_group.
const PIPELINE_BASE: u32 = reg_index!(0x2000);
const PIPELINE_STRIDE: u32 = 0x10;
const NUM_SHADER_PROGRAMS: usize = 6;

/// Shader program region: GPU virtual base of the shared shader code region.
/// Two consecutive u32 registers — high then low — at byte offset 0x1608.
/// Upstream: `Maxwell3D::Regs::ProgramRegion` (`address_high`, `address_low`).
const PROGRAM_REGION_HIGH: u32 = reg_index!(0x1608);
const PROGRAM_REGION_LOW: u32 = reg_index!(0x160C);

// ── Color write mask registers ────────────────────────────────────────────

/// If nonzero, all RTs share color_mask[0].
const COLOR_MASK_COMMON: u32 = reg_index!(0x0F90);
const COLOR_TARGET_MRT_ENABLE: u32 = reg_index!(0x0FAC);
/// Per-RT color write mask array. 8 entries, 1 word each.
/// Per RT: R=bit[0], G=bit[4], B=bit[8], A=bit[12].
const COLOR_MASK_BASE: u32 = reg_index!(0x1A00);

// ── Render target control register ────────────────────────────────────────

/// RT control: count in bits[3:0], target map in bits[6:4],[9:7],... (3 bits each).
const RT_CONTROL: u32 = reg_index!(0x121C);

// ── Constant buffer registers ───────────────────────────────────────────────

/// CB config: +0 size, +1 addr_high, +2 addr_low, +3 offset.
const CB_CONFIG_BASE: u32 = reg_index!(0x2380);

/// CB data: 16 words of inline push at `const_buffer.buffer`.
/// Upstream `Regs::ConstantBuffer` has 4 header words
/// `(size, address_high, address_low, offset)` before `buffer[16]`.
const CB_DATA_BASE: u32 = reg_index!(0x2390);
const CB_DATA_END: u32 = reg_index!(0x23D0); // exclusive

/// CB bind base. 5 stages, stride 8, trigger at +4.
const CB_BIND_BASE: u32 = reg_index!(0x2400);
const CB_BIND_STRIDE: u32 = 8;
/// CB bind trigger registers (one per shader stage).
const CB_BIND_TRIGGER_0: u32 = reg_index!(0x2404);
const CB_BIND_TRIGGER_1: u32 = reg_index!(0x240C);
const CB_BIND_TRIGGER_2: u32 = reg_index!(0x2414);
const CB_BIND_TRIGGER_3: u32 = reg_index!(0x241C);
const CB_BIND_TRIGGER_4: u32 = reg_index!(0x2424);

/// Number of shader stages (vertex, tess ctrl, tess eval, geometry, fragment).
const NUM_SHADER_STAGES: usize = 5;
/// Maximum constant buffer slots per stage.
const MAX_CB_SLOTS: usize = 18;

// ── Texture/Sampler pool registers ──────────────────────────────────────────

/// Texture sampler pool base: +0 addr_high, +1 addr_low, +2 limit.
const TEX_SAMPLER_POOL_BASE: u32 = reg_index!(0x155C);

/// Texture header pool base: +0 addr_high, +1 addr_low, +2 limit.
const TEX_HEADER_POOL_BASE: u32 = reg_index!(0x1574);

/// Sampler binding mode register.
/// 0 = Independently (tic_id and tsc_id are separate in texture handle)
/// 1 = ViaHeaderBinding (tic_id == tsc_id, linked)
const SAMPLER_BINDING: u32 = reg_index!(0x1234);

// ── MME (Macro Method Executor) registers ──────────────────────────────────

/// Pointer into macro code upload buffer (auto-increments on instruction write).
/// Upstream byte offset 0x0114, word index 0x45.
const LOAD_MME_INSTRUCTION_PTR: u32 = reg_index!(0x0114);
/// Code word to upload at the current pointer.
/// Upstream byte offset 0x0118, word index 0x46.
const LOAD_MME_INSTRUCTION: u32 = reg_index!(0x0118);
/// Pointer into 128-slot position table (auto-increments on bind).
/// Upstream byte offset 0x011C, word index 0x47.
const LOAD_MME_START_ADDR_PTR: u32 = reg_index!(0x011C);
/// Start offset for the current macro slot.
/// Upstream byte offset 0x0120, word index 0x48.
const LOAD_MME_START_ADDR: u32 = reg_index!(0x0120);
/// First macro method register. Methods 0xE00..0xFFF invoke macros.
/// Now uses word indices matching upstream (ENGINE_REG_COUNT = 0xE00).
const MACRO_METHODS_START: u32 = reg_index!(0x3800);
/// Exclusive end of macro method range (0x1000 * 4).
const MACRO_METHODS_END: u32 = reg_index!(0x4000);

// ── Additional register offsets (upstream ASSERT_REG_POSITION values) ──────

/// Wait-for-idle register. Writing triggers a rasterizer idle wait.
const WAIT_FOR_IDLE: u32 = reg_index!(0x0110);
/// Shadow RAM control register.
const SHADOW_RAM_CONTROL: u32 = reg_index!(0x0124);
/// Launch DMA register (triggers inline upload execution).
const LAUNCH_DMA: u32 = reg_index!(0x01B0);
/// Inline data register (data words for DMA upload).
const INLINE_DATA: u32 = reg_index!(0x01B4);
const UPLOAD_REGS_BASE: usize = reg_index!(0x0180) as usize;
/// Sync info register (triggers sync point signaling).
const SYNC_INFO: u32 = reg_index!(0x02C8);
/// Fragment barrier register.
const FRAGMENT_BARRIER: u32 = reg_index!(0x0DE0);
/// Draw texture trigger (writing src_y0 triggers the draw).
const DRAW_TEXTURE_SRC_Y0: u32 = reg_index!(0x1088);
/// Vertex array instance first (triggers instanced array draw).
const VERTEX_ARRAY_INSTANCE_FIRST: u32 = reg_index!(0x1214);
/// Vertex array instance subsequent (triggers subsequent instance draw).
const VERTEX_ARRAY_INSTANCE_SUBSEQUENT: u32 = reg_index!(0x1218);
/// Inline index 4x8 (index0 triggers 4-byte inline index push).
const INLINE_INDEX_4X8_INDEX0: u32 = reg_index!(0x1300);
/// Invalidate texture data cache register.
const INVALIDATE_TEXTURE_DATA_CACHE: u32 = reg_index!(0x0F74);
/// Tiled cache barrier register.
const TILED_CACHE_BARRIER: u32 = reg_index!(0x0F7C);
/// Clear report value register (triggers counter reset).
const CLEAR_REPORT_VALUE: u32 = reg_index!(0x1530);
/// Render enable block base: +0 addr_high, +1 addr_low, +2 mode.
const RENDER_ENABLE_BASE: u32 = reg_index!(0x1550);
/// Render enable mode register (triggers query condition evaluation).
const RENDER_ENABLE_MODE: u32 = reg_index!(0x1554);
/// Render enable override register.
const RENDER_ENABLE_OVERRIDE: u32 = reg_index!(0x1944);
/// Inline index 2x16 even (triggers 2-short inline index push).
const INLINE_INDEX_2X16_EVEN: u32 = reg_index!(0x15EC);
/// Topology override register.
const TOPOLOGY_OVERRIDE: u32 = reg_index!(0x1970);
/// Index buffer 32-bit first register.
const INDEX_BUFFER32_FIRST: u32 = reg_index!(0x17E4);
/// Index buffer 16-bit first register.
const INDEX_BUFFER16_FIRST: u32 = reg_index!(0x17E8);
/// Index buffer 8-bit first register.
const INDEX_BUFFER8_FIRST: u32 = reg_index!(0x17EC);
/// Index buffer 32-bit subsequent register.
const INDEX_BUFFER32_SUBSEQUENT: u32 = reg_index!(0x17F0);
/// Index buffer 16-bit subsequent register.
const INDEX_BUFFER16_SUBSEQUENT: u32 = reg_index!(0x17F4);
/// Index buffer 8-bit subsequent register.
const INDEX_BUFFER8_SUBSEQUENT: u32 = reg_index!(0x17F8);
/// Report semaphore query trigger (writing here fires the semaphore query).
// Matches upstream MAXWELL3D_REG_INDEX(report_semaphore.query).
const REPORT_SEMAPHORE_QUERY: u32 = REPORT_SEMAPHORE_TRIGGER;
/// Falcon register array element used by `ProcessFirmwareCall4`.
/// Upstream owner is `MAXWELL3D_REG_INDEX(falcon[4])`, i.e. byte offset
/// `0x2300 + 4 * sizeof(u32) = 0x2310`.
const FALCON4: u32 = reg_index!(0x2310);
/// Shadow scratch memory base (0x3400). Used by firmware stubs.
const SHADOW_SCRATCH_BASE: u32 = reg_index!(0x3400);

/// Macro registers start offset (method index space, not byte offset).
/// Methods >= 0xE00 are macro triggers.
const MACRO_REGISTERS_START: u32 = reg_index!(0x3800);

// ── Common render target formats ────────────────────────────────────────────

const RT_FORMAT_A8B8G8R8_UNORM: u32 = reg_index!(0x0354);
const RT_FORMAT_A8B8G8R8_SRGB: u32 = reg_index!(0x0358);
#[allow(dead_code)]
const RT_FORMAT_A8R8G8B8_UNORM: u32 = reg_index!(0x00CC);
#[allow(dead_code)]
const RT_FORMAT_R16G16B16A16_FLOAT: u32 = reg_index!(0x00C8);
#[allow(dead_code)]
const RT_FORMAT_R32_FLOAT: u32 = reg_index!(0x00E4);
#[allow(dead_code)]
const RT_FORMAT_R16G16_FLOAT: u32 = reg_index!(0x00DC);
#[allow(dead_code)]
const RT_FORMAT_R32G32_FLOAT: u32 = reg_index!(0x00C8);
#[allow(dead_code)]
const RT_FORMAT_R16_FLOAT: u32 = reg_index!(0x00F0);
#[allow(dead_code)]
const RT_FORMAT_R8_UNORM: u32 = reg_index!(0x00F0);
#[allow(dead_code)]
const RT_FORMAT_R16G16_UNORM: u32 = reg_index!(0x00D8);
#[allow(dead_code)]
const RT_FORMAT_B5G6R5_UNORM: u32 = reg_index!(0x00E8);
#[allow(dead_code)]
const RT_FORMAT_A2B10G10R10_UNORM: u32 = reg_index!(0x00D0);
#[allow(dead_code)]
const RT_FORMAT_R11G11B10_FLOAT: u32 = reg_index!(0x00E0);

// ── Draw state types ────────────────────────────────────────────────────────

/// GPU primitive topology.
///
/// Port of upstream `Maxwell3D::Regs::PrimitiveTopology`
/// (`maxwell_3d.h:871`). The discriminant values match upstream exactly
/// (Points = 0x0, …, Patches = 0xE) — they are the raw register values
/// the GPU writes into the topology field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum PrimitiveTopology {
    #[default]
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
                log::warn!(
                    "Maxwell3D: unknown topology {}, defaulting to Triangles",
                    value & 0xFFFF
                );
                Self::Triangles
            }
        }
    }
}

/// Index buffer element format.
///
/// Port of upstream `Maxwell3D::Regs::IndexFormat` (`maxwell_3d.h:932`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u32)]
pub enum IndexFormat {
    #[default]
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
                log::warn!(
                    "Maxwell3D: unknown index format {}, defaulting to UnsignedInt",
                    value
                );
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

// ── Shadow RAM control ─────────────────────────────────────────────────────

/// Shadow RAM control mode — how register writes interact with shadow state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ShadowRamControl {
    /// Normal operation — no shadowing.
    MethodTrack = 0,
    /// Track writes into shadow state.
    Track = 1,
    /// Track with filter.
    TrackWithFilter = 2,
    /// Replay from shadow state instead of using written values.
    Replay = 3,
}

impl ShadowRamControl {
    pub fn from_raw(value: u32) -> Self {
        match value {
            1 => Self::Track,
            2 => Self::TrackWithFilter,
            3 => Self::Replay,
            _ => Self::MethodTrack,
        }
    }
}

/// Clear report value types for counter reset.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ClearReport {
    ZPassPixelCount = 0x01,
    StreamingPrimitivesSucceeded = 0x03,
    PrimitivesGenerated = 0x12,
    VtgPrimitivesOut = 0x15,
}

// ── Draw mode types ─────────────────────────────────────────────────────────

/// Instance mode extracted from DRAW_BEGIN bits[27:26].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstanceId {
    First = 0,
    Subsequent = 1,
    Unchanged = 2,
}

impl InstanceId {
    pub fn from_raw(value: u32) -> Self {
        match (value >> 26) & 0x3 {
            0 => Self::First,
            1 => Self::Subsequent,
            _ => Self::Unchanged,
        }
    }
}

/// Internal draw mode state machine.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DrawMode {
    General,
    Instance,
    InlineIndex,
}

#[derive(Debug, Clone, Copy)]
struct VertexArrayParams {
    start: u32,
    count: u32,
    topology: PrimitiveTopology,
}

impl VertexArrayParams {
    fn from_raw(raw: u32) -> Self {
        Self {
            start: raw & 0xFFFF,
            count: (raw >> 16) & 0xFFF,
            topology: PrimitiveTopology::from_raw((raw >> 28) & 0x7),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct IndexBufferSmallParams {
    first: u32,
    count: u32,
    topology: PrimitiveTopology,
}

impl IndexBufferSmallParams {
    fn from_raw(raw: u32) -> Self {
        Self {
            first: raw & 0xFFFF,
            count: (raw >> 16) & 0xFFF,
            topology: PrimitiveTopology::from_raw((raw >> 28) & 0xF),
        }
    }
}

/// Sampler binding mode — how texture handles encode TIC/TSC indices.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SamplerBinding {
    /// TIC and TSC indices are packed independently in the texture handle.
    /// Handle: bits[19:0] = tic_id, bits[31:20] = tsc_id.
    Independently = 0,
    /// TIC and TSC share the same index (linked binding).
    ViaHeaderBinding = 1,
}

/// Report semaphore operation type from query word bits[1:0].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReportOperation {
    Release = 0,
    Acquire = 1,
    ReportOnly = 2,
    Trap = 3,
}

impl ReportOperation {
    pub fn from_raw(value: u32) -> Self {
        match value & 0x3 {
            0 => Self::Release,
            1 => Self::Acquire,
            2 => Self::ReportOnly,
            _ => Self::Trap,
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
                log::warn!(
                    "Maxwell3D: unknown ComparisonOp 0x{:X}, defaulting to Always",
                    value
                );
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
                log::warn!(
                    "Maxwell3D: unknown BlendEquation 0x{:X}, defaulting to Add",
                    value
                );
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
                log::warn!(
                    "Maxwell3D: unknown BlendFactor 0x{:X}, defaulting to One",
                    value
                );
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
                log::warn!(
                    "Maxwell3D: unknown StencilOp 0x{:X}, defaulting to Keep",
                    value
                );
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
                log::warn!(
                    "Maxwell3D: unknown CullFace 0x{:X}, defaulting to Back",
                    value
                );
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
                log::warn!(
                    "Maxwell3D: unknown FrontFace 0x{:X}, defaulting to CCW",
                    value
                );
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
                log::warn!(
                    "Maxwell3D: unknown PolygonMode 0x{:X}, defaulting to Fill",
                    value
                );
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
                log::warn!(
                    "Maxwell3D: unknown DepthMode {}, defaulting to ZeroToOne",
                    value
                );
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
            Self::R32G32B32A32
            | Self::R16G16B16A16
            | Self::R8G8B8A8
            | Self::A2B10G10R10
            | Self::X8B8G8R8 => 4,
            Self::R32G32B32 | Self::R16G16B16 | Self::R8G8B8 | Self::B10G11R11 => 3,
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

// ── Texture/Sampler descriptor enums ─────────────────────────────────────────

/// Texture image format (7-bit field from TIC word 0).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextureFormat {
    R32G32B32A32,
    R32G32B32,
    R16G16B16A16,
    R32G32,
    R32B24G8,
    R16G16,
    R32,
    B5G6R5,
    A1R5G5B5,
    R8G8,
    R16,
    R8,
    A8B8G8R8,
    A2B10G10R10,
    R16G16B16X16,
    R32G32B32X32,
    B10G11R11,
    G8R24,
    R32G8X24,
    R8G8B8A8,
    Bc1Rgba,
    Bc2,
    Bc3,
    Bc4,
    Bc5,
    Bc7,
    Bc6HSf16,
    Bc6HUf16,
    Etc2Rgb,
    Etc2RgbA1,
    Etc2RgbA8,
    Eac,
    EacX2,
    Astc2d4x4,
    Astc2d5x4,
    Astc2d5x5,
    Astc2d6x5,
    Astc2d6x6,
    Astc2d8x5,
    Astc2d8x6,
    Astc2d8x8,
    Astc2d10x5,
    Astc2d10x6,
    Astc2d10x8,
    Astc2d10x10,
    Astc2d12x10,
    Astc2d12x12,
    Invalid,
}

impl TextureFormat {
    pub fn from_raw(value: u32) -> Self {
        match value & 0x7F {
            0x01 => Self::R32G32B32A32,
            0x02 => Self::R32G32B32,
            0x03 => Self::R16G16B16A16,
            0x04 => Self::R32G32,
            0x05 => Self::R32B24G8,
            0x08 => Self::R16G16,
            0x09 => Self::R32,
            0x0E => Self::B5G6R5,
            0x0F => Self::A1R5G5B5,
            0x10 => Self::R8G8,
            0x11 => Self::R16,
            0x12 => Self::R8,
            0x1D => Self::A8B8G8R8,
            0x1E => Self::A2B10G10R10,
            0x1F => Self::R16G16B16X16,
            0x20 => Self::R32G32B32X32,
            0x21 => Self::B10G11R11,
            0x22 => Self::G8R24,
            0x23 => Self::R32G8X24,
            0x24 => Self::R8G8B8A8,
            0x25 => Self::Bc1Rgba,
            0x26 => Self::Bc2,
            0x27 => Self::Bc3,
            0x28 => Self::Bc4,
            0x29 => Self::Bc5,
            0x2A => Self::Bc7,
            0x2B => Self::Bc6HSf16,
            0x2C => Self::Bc6HUf16,
            0x2D => Self::Etc2Rgb,
            0x2E => Self::Etc2RgbA1,
            0x2F => Self::Etc2RgbA8,
            0x30 => Self::Eac,
            0x31 => Self::EacX2,
            0x40 => Self::Astc2d4x4,
            0x41 => Self::Astc2d5x4,
            0x42 => Self::Astc2d5x5,
            0x43 => Self::Astc2d6x5,
            0x44 => Self::Astc2d6x6,
            0x45 => Self::Astc2d8x5,
            0x46 => Self::Astc2d8x6,
            0x47 => Self::Astc2d8x8,
            0x48 => Self::Astc2d10x5,
            0x49 => Self::Astc2d10x6,
            0x4A => Self::Astc2d10x8,
            0x4B => Self::Astc2d10x10,
            0x4C => Self::Astc2d12x10,
            0x4D => Self::Astc2d12x12,
            _ => Self::Invalid,
        }
    }
}

/// Texture type (4-bit field from TIC word 4).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextureType {
    Texture1D,
    Texture2D,
    Texture3D,
    Cubemap,
    Array1D,
    Array2D,
    Buffer1D,
    Texture2DNoMip,
    CubemapArray,
    Invalid,
}

impl TextureType {
    pub fn from_raw(value: u32) -> Self {
        match value & 0xF {
            0 => Self::Texture1D,
            1 => Self::Texture2D,
            2 => Self::Texture3D,
            3 => Self::Cubemap,
            4 => Self::Array1D,
            5 => Self::Array2D,
            6 => Self::Buffer1D,
            7 => Self::Texture2DNoMip,
            8 => Self::CubemapArray,
            _ => Self::Invalid,
        }
    }
}

/// Texture component type (3-bit field, per-channel in TIC word 0).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComponentType {
    SNorm,
    UNorm,
    SInt,
    UInt,
    SNormForceFp16,
    UNormForceFp16,
    Float,
    Invalid,
}

impl ComponentType {
    pub fn from_raw(value: u32) -> Self {
        match value & 0x7 {
            1 => Self::SNorm,
            2 => Self::UNorm,
            3 => Self::SInt,
            4 => Self::UInt,
            5 => Self::SNormForceFp16,
            6 => Self::UNormForceFp16,
            7 => Self::Float,
            _ => Self::Invalid,
        }
    }
}

/// Texture swizzle source (3-bit field, XYZW in TIC word 0).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwizzleSource {
    Zero,
    R,
    G,
    B,
    A,
    OneInt,
    OneFloat,
    Invalid,
}

impl SwizzleSource {
    pub fn from_raw(value: u32) -> Self {
        match value & 0x7 {
            0 => Self::Zero,
            2 => Self::R,
            3 => Self::G,
            4 => Self::B,
            5 => Self::A,
            6 => Self::OneInt,
            7 => Self::OneFloat,
            _ => Self::Invalid,
        }
    }
}

/// TIC header version (3-bit field from TIC word 2).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TicHeaderVersion {
    OneDBuffer,
    PitchColorKey,
    Pitch,
    BlockLinear,
    BlockLinearColorKey,
    Invalid,
}

impl TicHeaderVersion {
    pub fn from_raw(value: u32) -> Self {
        match value & 0x7 {
            0 => Self::OneDBuffer,
            1 => Self::PitchColorKey,
            2 => Self::Pitch,
            3 => Self::BlockLinear,
            4 => Self::BlockLinearColorKey,
            _ => Self::Invalid,
        }
    }
}

/// Texture wrap/address mode (3-bit field in TSC word 0).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WrapMode {
    Wrap,
    Mirror,
    ClampToEdge,
    Border,
    Clamp,
    MirrorOnceClampToEdge,
    MirrorOnceBorder,
    MirrorOnceClampOgl,
    Invalid,
}

impl WrapMode {
    pub fn from_raw(value: u32) -> Self {
        match value & 0x7 {
            0 => Self::Wrap,
            1 => Self::Mirror,
            2 => Self::ClampToEdge,
            3 => Self::Border,
            4 => Self::Clamp,
            5 => Self::MirrorOnceClampToEdge,
            6 => Self::MirrorOnceBorder,
            7 => Self::MirrorOnceClampOgl,
            _ => Self::Invalid,
        }
    }
}

/// Texture magnification/minification filter (2-bit field in TSC word 1).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextureFilter {
    Nearest,
    Linear,
    Invalid,
}

impl TextureFilter {
    pub fn from_raw(value: u32) -> Self {
        match value & 0x3 {
            1 => Self::Nearest,
            2 => Self::Linear,
            _ => Self::Invalid,
        }
    }
}

/// Mipmap filter mode (2-bit field in TSC word 1).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MipmapFilter {
    None,
    Nearest,
    Linear,
    Invalid,
}

impl MipmapFilter {
    pub fn from_raw(value: u32) -> Self {
        match value & 0x3 {
            1 => Self::None,
            2 => Self::Nearest,
            3 => Self::Linear,
            _ => Self::Invalid,
        }
    }
}

/// Depth compare function for sampler (3-bit field in TSC word 0).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DepthCompareFunc {
    Never,
    Less,
    Equal,
    LessEqual,
    Greater,
    NotEqual,
    GreaterEqual,
    Always,
}

impl DepthCompareFunc {
    pub fn from_raw(value: u32) -> Self {
        match value & 0x7 {
            0 => Self::Never,
            1 => Self::Less,
            2 => Self::Equal,
            3 => Self::LessEqual,
            4 => Self::Greater,
            5 => Self::NotEqual,
            6 => Self::GreaterEqual,
            7 => Self::Always,
            _ => unreachable!(),
        }
    }
}

// ── Texture/Sampler descriptor structs ──────────────────────────────────────

/// Parsed texture image control descriptor (TIC, 8 words = 32 bytes).
#[derive(Debug, Clone, PartialEq)]
pub struct TextureDescriptor {
    pub format: TextureFormat,
    pub r_type: ComponentType,
    pub g_type: ComponentType,
    pub b_type: ComponentType,
    pub a_type: ComponentType,
    pub x_source: SwizzleSource,
    pub y_source: SwizzleSource,
    pub z_source: SwizzleSource,
    pub w_source: SwizzleSource,
    pub address: u64,
    pub header_version: TicHeaderVersion,
    pub texture_type: TextureType,
    pub width: u32,
    pub height: u32,
    pub depth: u32,
    pub max_mip_level: u32,
    pub block_height: u32,
    pub block_depth: u32,
    pub srgb_conversion: bool,
    pub normalized_coords: bool,
}

impl TextureDescriptor {
    /// Parse a TIC descriptor from 8 raw u32 words.
    pub fn from_words(words: &[u32; 8]) -> Self {
        let word0 = words[0];
        let word1 = words[1];
        let word2 = words[2];
        let word3 = words[3];
        let word4 = words[4];
        let word5 = words[5];

        let addr_low = word1 as u64;
        let addr_high = (word2 & 0xFFFF) as u64;

        Self {
            format: TextureFormat::from_raw(word0 & 0x7F),
            r_type: ComponentType::from_raw((word0 >> 7) & 0x7),
            g_type: ComponentType::from_raw((word0 >> 10) & 0x7),
            b_type: ComponentType::from_raw((word0 >> 13) & 0x7),
            a_type: ComponentType::from_raw((word0 >> 16) & 0x7),
            x_source: SwizzleSource::from_raw((word0 >> 19) & 0x7),
            y_source: SwizzleSource::from_raw((word0 >> 22) & 0x7),
            z_source: SwizzleSource::from_raw((word0 >> 25) & 0x7),
            w_source: SwizzleSource::from_raw((word0 >> 28) & 0x7),
            address: (addr_high << 32) | addr_low,
            header_version: TicHeaderVersion::from_raw((word2 >> 21) & 0x7),
            texture_type: TextureType::from_raw((word4 >> 23) & 0xF),
            width: (word4 & 0xFFFF) + 1,
            height: (word5 & 0xFFFF) + 1,
            depth: ((word5 >> 16) & 0x3FFF) + 1,
            max_mip_level: (word3 >> 28) & 0xF,
            block_height: (word3 >> 3) & 0x7,
            block_depth: (word3 >> 6) & 0x7,
            srgb_conversion: (word4 & (1 << 22)) != 0,
            normalized_coords: (word5 & (1 << 31)) != 0,
        }
    }
}

/// Parsed texture sampler control descriptor (TSC, 8 words = 32 bytes).
#[derive(Debug, Clone, PartialEq)]
pub struct SamplerDescriptor {
    pub wrap_u: WrapMode,
    pub wrap_v: WrapMode,
    pub wrap_p: WrapMode,
    pub depth_compare_enabled: bool,
    pub depth_compare_func: DepthCompareFunc,
    pub max_anisotropy: u32,
    pub mag_filter: TextureFilter,
    pub min_filter: TextureFilter,
    pub mipmap_filter: MipmapFilter,
    pub min_lod: f32,
    pub max_lod: f32,
    pub mip_lod_bias: f32,
    pub border_color: [f32; 4],
}

impl SamplerDescriptor {
    /// Parse a TSC descriptor from 8 raw u32 words.
    pub fn from_words(words: &[u32; 8]) -> Self {
        let word0 = words[0];
        let word1 = words[1];
        let word2 = words[2];

        // mip_lod_bias is a 13-bit sign-extended fixed-point value at word1[24:12].
        let raw_bias = (word1 >> 12) & 0x1FFF;
        let bias_signed = if raw_bias & 0x1000 != 0 {
            // Sign-extend from 13 bits.
            (raw_bias | 0xFFFF_E000) as i32
        } else {
            raw_bias as i32
        };

        Self {
            wrap_u: WrapMode::from_raw(word0 & 0x7),
            wrap_v: WrapMode::from_raw((word0 >> 3) & 0x7),
            wrap_p: WrapMode::from_raw((word0 >> 6) & 0x7),
            depth_compare_enabled: (word0 & (1 << 9)) != 0,
            depth_compare_func: DepthCompareFunc::from_raw((word0 >> 10) & 0x7),
            max_anisotropy: (word0 >> 20) & 0x7,
            mag_filter: TextureFilter::from_raw(word1 & 0x3),
            min_filter: TextureFilter::from_raw((word1 >> 4) & 0x3),
            mipmap_filter: MipmapFilter::from_raw((word1 >> 6) & 0x3),
            min_lod: (word2 & 0xFFF) as f32 / 256.0,
            max_lod: ((word2 >> 12) & 0xFFF) as f32 / 256.0,
            mip_lod_bias: bias_signed as f32 / 256.0,
            border_color: [
                f32::from_bits(words[4]),
                f32::from_bits(words[5]),
                f32::from_bits(words[6]),
                f32::from_bits(words[7]),
            ],
        }
    }

    /// Convert the raw 3-bit max_anisotropy value to a multiplier (1/2/4/8/16).
    pub fn anisotropy_multiplier(&self) -> u32 {
        match self.max_anisotropy {
            0 => 1,
            1 => 2,
            2 => 4,
            3 => 8,
            4 => 16,
            _ => 16, // clamp to max
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

/// Number of hardware viewports/scissors.
pub const NUM_VIEWPORTS: usize = 16;

/// Viewport computed from scale/translate registers.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct ViewportInfo {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
    pub depth_near: f32,
    pub depth_far: f32,
}

/// Scissor rectangle.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
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

/// Render target configuration for one color target.
#[derive(Debug, Clone, Copy, Default)]
pub struct RenderTargetInfo {
    pub address: u64,
    pub width: u32,
    pub height: u32,
    pub format: u32,
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
    pub viewports: [ViewportInfo; NUM_VIEWPORTS],
    pub scissors: [ScissorInfo; NUM_VIEWPORTS],
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
    pub tex_header_pool_addr: u64,
    pub tex_header_pool_limit: u32,
    pub tex_sampler_pool_addr: u64,
    pub tex_sampler_pool_limit: u32,
    /// Instance count (1 for non-instanced, N for instanced batches).
    pub instance_count: u32,
    /// Base instance offset from GLOBAL_BASE_INSTANCE_INDEX.
    pub base_instance: u32,
    /// Base vertex offset from GLOBAL_BASE_VERTEX_INDEX (signed).
    pub base_vertex: i32,
    /// Non-empty only for InlineIndex draws.
    pub inline_index_data: Vec<u8>,
    /// Sampler binding mode for this draw call.
    pub sampler_binding: SamplerBinding,
    /// Render target configurations for up to 8 color targets.
    pub render_targets: [RenderTargetInfo; 8],
}

// ── Engine struct ───────────────────────────────────────────────────────────

pub struct Maxwell3D {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
    /// Shadow copy of registers for shadow RAM tracking.
    shadow_state: Box<[u32; ENGINE_REG_COUNT]>,
    /// Engine interface state: execution mask, method sink, dirty tracking.
    pub interface_state: EngineInterfaceState,
    /// Whether conditional rendering is active.
    execute_on: bool,
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
    /// Draw mode state machine.
    draw_mode: DrawMode,
    /// Accumulated instance count in Instance mode.
    instance_count: u32,
    /// Accumulated inline index data bytes.
    inline_index_data: Vec<u8>,
    /// Pending semaphore writes to be returned by execute_pending.
    pending_semaphore_writes: Vec<PendingWrite>,
    /// Bound rasterizer backend.
    rasterizer: Option<[usize; 2]>,
    /// Start offsets of each macro in macro memory.
    macro_positions: [u32; 0x80],
    /// MME macro engine for uploaded programmable macro execution.
    macro_engine: MacroEngine,
    /// Upstream owner `Upload::State upload_state`.
    upload_state: engine_upload::State,
    /// Upstream owner `MemoryManager& memory_manager`.
    memory_manager: Option<Arc<Mutex<MemoryManager>>>,
    /// Method of the macro currently being fed parameters (0 = none).
    executing_macro: u32,
    /// Accumulated parameters for the current macro call.
    macro_params: Vec<u32>,
    /// GPU addresses for macro parameter words.
    macro_addresses: Vec<u64>,
    /// (segment_addr, word_count) pairs for macro parameter memory segments.
    macro_segments: Vec<(u64, u32)>,
    /// Whether the current macro has dirty memory.
    current_macro_dirty: bool,
    /// Rust adaptation for the upstream `Core::System& system` dependency used
    /// here only to read guest memory through the active GPU owner.
    guest_memory_reader: Option<Arc<dyn Fn(u64, &mut [u8]) + Send + Sync>>,
    /// Owner-local bridge for guest memory writes needed by inline upload paths.
    guest_memory_writer: Option<Arc<dyn Fn(u64, &[u8]) + Send + Sync>>,
    /// Rust owner-local bridge for upstream `system.GPU().GetTicks()` query timestamp writes.
    gpu_ticks_getter: Option<Arc<dyn Fn() -> u64 + Send + Sync>>,
    /// Graphics TIC descriptor table (texture image descriptors).
    tic_table: TicTable,
    /// Graphics TSC descriptor table (texture sampler descriptors).
    tsc_table: TscTable,
}

impl Maxwell3D {
    pub fn new() -> Self {
        // Build execution mask: mark which methods trigger immediate execution.
        let mut execution_mask = vec![false; u16::MAX as usize];
        for i in 0..execution_mask.len() {
            execution_mask[i] = Self::is_method_executable(i as u32);
        }

        let mut engine = Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
            shadow_state: Box::new([0u32; ENGINE_REG_COUNT]),
            interface_state: EngineInterfaceState {
                execution_mask,
                method_sink: Vec::new(),
                current_dirty: false,
                current_dma_segment: 0,
            },
            execute_on: true,
            pending_framebuffer: None,
            draw_calls: Vec::new(),
            current_topology: PrimitiveTopology::Triangles,
            draw_indexed: false,
            cb_bindings: [[ConstBufferBinding::default(); MAX_CB_SLOTS]; NUM_SHADER_STAGES],
            draw_mode: DrawMode::General,
            instance_count: 0,
            inline_index_data: Vec::new(),
            pending_semaphore_writes: Vec::new(),
            rasterizer: None,
            macro_positions: [0u32; 0x80],
            macro_engine: get_macro_engine(),
            upload_state: engine_upload::State::new(),
            memory_manager: None,
            executing_macro: 0,
            macro_params: Vec::new(),
            macro_addresses: Vec::new(),
            macro_segments: Vec::new(),
            current_macro_dirty: false,
            guest_memory_reader: None,
            guest_memory_writer: None,
            gpu_ticks_getter: None,
            tic_table: TicTable::new(),
            tsc_table: TscTable::new(),
        };
        engine.initialize_register_defaults();
        engine
    }

    fn initialize_register_defaults(&mut self) {
        for viewport in 0..16usize {
            let base = VP_TRANSFORM_BASE as usize + viewport * VP_TRANSFORM_STRIDE as usize;
            self.regs[base + 6] = 0x688;
        }

        self.regs[BLEND_BASE as usize + 1] = 1;
        self.regs[BLEND_BASE as usize + 2] = 0x02;
        self.regs[BLEND_BASE as usize + 3] = 0x01;
        self.regs[BLEND_BASE as usize + 4] = 1;
        self.regs[BLEND_BASE as usize + 5] = 0x02;
        self.regs[BLEND_BASE as usize + 7] = 0x01;

        for rt in 0..8usize {
            let base = BLEND_PER_TARGET_BASE as usize + rt * BLEND_PER_TARGET_STRIDE as usize;
            self.regs[base + 1] = 1;
            self.regs[base + 2] = 0x02;
            self.regs[base + 3] = 0x01;
            self.regs[base + 4] = 1;
            self.regs[base + 5] = 0x02;
            self.regs[base + 6] = 0x01;
        }

        let front_base = STENCIL_FRONT_OP_BASE as usize;
        self.regs[front_base] = 1;
        self.regs[front_base + 1] = 1;
        self.regs[front_base + 2] = 1;
        self.regs[front_base + 3] = 0x207;
        self.regs[STENCIL_FRONT_FUNC_MASK as usize] = 0xFFFF_FFFF;
        self.regs[STENCIL_FRONT_MASK as usize] = 0xFFFF_FFFF;
        self.regs[STENCIL_TWO_SIDE_ENABLE as usize] = 1;

        let back_base = STENCIL_BACK_OP_BASE as usize;
        self.regs[back_base] = 1;
        self.regs[back_base + 1] = 1;
        self.regs[back_base + 2] = 1;
        self.regs[back_base + 3] = 0x207;
        self.regs[STENCIL_BACK_FUNC_MASK as usize] = 0xFFFF_FFFF;
        self.regs[STENCIL_BACK_MASK as usize] = 0xFFFF_FFFF;

        self.regs[DEPTH_TEST_FUNC as usize] = 0x207;
        self.regs[FRONT_FACE as usize] = 0x0900;
        self.regs[CULL_FACE as usize] = 0x0405;
        self.regs[POINT_SIZE as usize] = f32::to_bits(1.0);

        for rt in 0..8usize {
            self.regs[COLOR_MASK_BASE as usize + rt] = 0x1111;
        }

        for attrib in 0..NUM_VERTEX_ATTRIBS as usize {
            self.regs[VERTEX_ATTRIB_BASE as usize + attrib] |= 1 << 6;
        }

        self.regs[RASTERIZE_ENABLE as usize] = 1;
        self.regs[COLOR_TARGET_MRT_ENABLE as usize] = 1;
        self.regs[FRAMEBUFFER_SRGB as usize] = 1;
        self.regs[LINE_WIDTH_ALIASED as usize] = f32::to_bits(1.0);
        self.regs[LINE_WIDTH_SMOOTH as usize] = f32::to_bits(1.0);
        self.regs[POLYGON_MODE_BACK as usize] = 0x1B02;
        self.regs[POLYGON_MODE_FRONT as usize] = 0x1B02;

        self.shadow_state.copy_from_slice(&self.regs[..]);
    }

    /// Whether conditional rendering allows execution.
    pub fn should_execute(&self) -> bool {
        self.execute_on
    }

    pub fn bind_rasterizer(&mut self, rasterizer: &dyn RasterizerInterface) {
        if std::env::var_os("RUZU_TRACE_RASTERIZER_BIND").is_some() {
            let ptr = rasterizer as *const dyn RasterizerInterface;
            log::info!("Maxwell3D::bind_rasterizer rasterizer_ptr={:p}", ptr);
        }
        self.rasterizer = Some(unsafe {
            std::mem::transmute::<*const dyn RasterizerInterface, [usize; 2]>(rasterizer)
        });
    }

    pub fn set_memory_manager(&mut self, memory_manager: Arc<Mutex<MemoryManager>>) {
        self.memory_manager = Some(memory_manager);
    }

    pub fn set_guest_memory_reader(
        &mut self,
        guest_memory_reader: Arc<dyn Fn(u64, &mut [u8]) + Send + Sync>,
    ) {
        self.guest_memory_reader = Some(guest_memory_reader);
    }

    pub fn set_guest_memory_writer(
        &mut self,
        guest_memory_writer: Arc<dyn Fn(u64, &[u8]) + Send + Sync>,
    ) {
        self.guest_memory_writer = Some(guest_memory_writer);
    }

    pub fn set_gpu_ticks_getter(&mut self, gpu_ticks_getter: Arc<dyn Fn() -> u64 + Send + Sync>) {
        self.gpu_ticks_getter = Some(gpu_ticks_getter);
    }

    fn read_gpu_block(&self, addr: u64, output: &mut [u8]) -> bool {
        let Some(memory_manager) = self.memory_manager.as_ref().cloned() else {
            return false;
        };
        let Some(guest_memory_reader) = self.guest_memory_reader.as_ref().cloned() else {
            return false;
        };
        memory_manager
            .lock()
            .read_block(addr, output, &*guest_memory_reader);
        true
    }

    fn with_rasterizer_mut<R>(
        &mut self,
        f: impl FnOnce(&mut dyn RasterizerInterface) -> R,
    ) -> Option<R> {
        let raw = self.rasterizer?;
        let rasterizer =
            unsafe { &mut *std::mem::transmute::<[usize; 2], *mut dyn RasterizerInterface>(raw) };
        Some(f(rasterizer))
    }

    pub fn get_macro_address(&self, index: usize) -> u64 {
        self.macro_addresses[index]
    }

    pub fn refresh_parameters(&mut self) {
        if !self.current_macro_dirty {
            return;
        }
        self.refresh_parameters_impl();
    }

    pub fn any_parameters_dirty(&self) -> bool {
        self.current_macro_dirty
    }

    fn refresh_parameters_impl(&mut self) {
        let Some(memory_manager) = self.memory_manager.as_ref().cloned() else {
            return;
        };
        let Some(guest_memory_reader) = self.guest_memory_reader.as_ref().cloned() else {
            return;
        };

        let mut current_index = 0usize;
        for &(segment_addr, word_count) in &self.macro_segments {
            let word_count = word_count as usize;
            if segment_addr == 0 {
                current_index += word_count;
                continue;
            }

            let byte_count = word_count * std::mem::size_of::<u32>();
            let mut bytes = vec![0u8; byte_count];
            memory_manager
                .lock()
                .read_block(segment_addr, &mut bytes, &*guest_memory_reader);
            for (word_index, chunk) in bytes.chunks_exact(4).enumerate() {
                self.macro_params[current_index + word_index] =
                    u32::from_le_bytes(chunk.try_into().expect("4-byte chunk"));
            }
            current_index += word_count;
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

    fn upload_registers(&self) -> engine_upload::Registers {
        engine_upload::Registers {
            line_length_in: self.regs[UPLOAD_REGS_BASE],
            line_count: self.regs[UPLOAD_REGS_BASE + 1],
            dest: engine_upload::DestRegisters {
                address_high: self.regs[UPLOAD_REGS_BASE + 2],
                address_low: self.regs[UPLOAD_REGS_BASE + 3],
                pitch: self.regs[UPLOAD_REGS_BASE + 4],
                block_dims: self.regs[UPLOAD_REGS_BASE + 5],
                width: self.regs[UPLOAD_REGS_BASE + 6],
                height: self.regs[UPLOAD_REGS_BASE + 7],
                depth: self.regs[UPLOAD_REGS_BASE + 8],
                layer: self.regs[UPLOAD_REGS_BASE + 9],
                x: self.regs[UPLOAD_REGS_BASE + 10],
                y: self.regs[UPLOAD_REGS_BASE + 11],
            },
        }
    }

    fn launch_dma_is_linear(&self) -> bool {
        (self.regs[LAUNCH_DMA as usize] & 0x1) != 0
    }

    fn process_inline_upload_word(&mut self, data: u32, is_last_call: bool) {
        if should_trace_dma_flow() {
            let trace_idx = MAXWELL3D_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if trace_idx < 160 {
                log::info!(
                    "Maxwell3D::process_inline_upload_word data=0x{:X} last={} target=0x{:X} size={} linear={}",
                    data,
                    is_last_call,
                    self.upload_registers().dest.address(),
                    self.upload_registers().line_length_in * self.upload_registers().line_count,
                    self.launch_dma_is_linear()
                );
            }
        }
        self.upload_state.regs = self.upload_registers();
        let Some(memory_manager) = self.memory_manager.as_ref().cloned() else {
            return;
        };
        let rasterizer_raw = self.rasterizer.map(|raw| unsafe {
            std::mem::transmute::<[usize; 2], *mut dyn RasterizerInterface>(raw)
        });
        let writer = self.guest_memory_writer.as_ref().cloned();
        let mut write_cpu = move |addr: u64, bytes: &[u8]| {
            if let Some(writer) = writer.as_ref() {
                writer(addr, bytes);
            }
        };
        let mut rasterizer = rasterizer_raw.map(|ptr| unsafe { &mut *ptr });
        let mut ctx = engine_upload::FlushContext {
            rasterizer: rasterizer.as_deref_mut(),
            memory_manager,
            write_cpu_mem: &mut write_cpu,
        };
        self.upload_state
            .process_data_word_with_ctx(data, is_last_call, &mut ctx);
    }

    fn process_inline_upload_multi(&mut self, data: &[u32]) {
        if should_trace_dma_flow() {
            let trace_idx = MAXWELL3D_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if trace_idx < 160 {
                log::info!(
                    "Maxwell3D::process_inline_upload_multi words={} first=0x{:X} target=0x{:X} size={} linear={}",
                    data.len(),
                    data.first().copied().unwrap_or(0),
                    self.upload_registers().dest.address(),
                    self.upload_registers().line_length_in * self.upload_registers().line_count,
                    self.launch_dma_is_linear()
                );
            }
        }
        self.upload_state.regs = self.upload_registers();
        let Some(memory_manager) = self.memory_manager.as_ref().cloned() else {
            return;
        };
        let rasterizer_raw = self.rasterizer.map(|raw| unsafe {
            std::mem::transmute::<[usize; 2], *mut dyn RasterizerInterface>(raw)
        });
        if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
            log::info!(
                "Maxwell3D::process_inline_upload_multi rasterizer_present={} rasterizer_ptr={:?} target=0x{:X} words={}",
                rasterizer_raw.is_some(),
                rasterizer_raw.map(|ptr| ptr as *mut ()),
                self.upload_registers().dest.address(),
                data.len()
            );
        }
        let writer = self.guest_memory_writer.as_ref().cloned();
        let mut write_cpu = move |addr: u64, bytes: &[u8]| {
            if let Some(writer) = writer.as_ref() {
                writer(addr, bytes);
            }
        };
        let mut rasterizer = rasterizer_raw.map(|ptr| unsafe { &mut *ptr });
        let mut ctx = engine_upload::FlushContext {
            rasterizer: rasterizer.as_deref_mut(),
            memory_manager,
            write_cpu_mem: &mut write_cpu,
        };
        self.upload_state
            .process_data_multi_with_ctx(data, &mut ctx);
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
            polygon_mode_front: PolygonMode::from_raw(self.regs[POLYGON_MODE_FRONT as usize]),
            polygon_mode_back: PolygonMode::from_raw(self.regs[POLYGON_MODE_BACK as usize]),
            line_width_smooth: f32::from_bits(self.regs[LINE_WIDTH_SMOOTH as usize]),
            line_width_aliased: f32::from_bits(self.regs[LINE_WIDTH_ALIASED as usize]),
            depth_bias: f32::from_bits(self.regs[DEPTH_BIAS as usize]),
            slope_scale_depth_bias: f32::from_bits(self.regs[SLOPE_SCALE_DEPTH_BIAS as usize]),
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

    // ── Texture/Sampler pool accessors ────────────────────────────────────

    /// GPU address of the texture header (TIC) pool.
    pub fn tex_header_pool_address(&self) -> u64 {
        let base = TEX_HEADER_POOL_BASE as usize;
        let high = self.regs[base] as u64;
        let low = self.regs[base + 1] as u64;
        (high << 32) | low
    }

    /// Maximum descriptor index in the texture header pool.
    pub fn tex_header_pool_limit(&self) -> u32 {
        self.regs[(TEX_HEADER_POOL_BASE + 2) as usize]
    }

    /// GPU address of the texture sampler (TSC) pool.
    pub fn tex_sampler_pool_address(&self) -> u64 {
        let base = TEX_SAMPLER_POOL_BASE as usize;
        let high = self.regs[base] as u64;
        let low = self.regs[base + 1] as u64;
        (high << 32) | low
    }

    /// Maximum descriptor index in the texture sampler pool.
    pub fn tex_sampler_pool_limit(&self) -> u32 {
        self.regs[(TEX_SAMPLER_POOL_BASE + 2) as usize]
    }

    // ── Descriptor table methods ─────────────────────────────────────────

    /// Synchronize descriptor tables with current register state.
    /// Call before accessing descriptors during draw dispatch.
    pub fn sync_descriptor_tables(&mut self) {
        let tic_addr = self.tex_header_pool_address();
        let tic_limit = self.regs[(TEX_HEADER_POOL_BASE + 2) as usize];
        self.tic_table.synchronize(tic_addr, tic_limit);

        let linked = self.regs[SAMPLER_BINDING as usize] == SamplerBinding::ViaHeaderBinding as u32;
        let tsc_addr = self.tex_sampler_pool_address();
        let tsc_limit = if linked {
            tic_limit
        } else {
            self.regs[(TEX_SAMPLER_POOL_BASE + 2) as usize]
        };
        self.tsc_table.synchronize(tsc_addr, tsc_limit);
    }

    /// Read a TIC entry by index from the texture header pool.
    /// Returns `(TextureDescriptor, changed)`.
    pub fn get_tic_entry(
        &mut self,
        index: u32,
        gpu_read: &dyn Fn(u64, &mut [u8]),
    ) -> (TextureDescriptor, bool) {
        let (raw, changed) = self.tic_table.read(index, gpu_read);
        let words = words_from_bytes(&raw);
        (TextureDescriptor::from_words(&words), changed)
    }

    /// Read a TSC entry by index from the texture sampler pool.
    /// Returns `(SamplerDescriptor, changed)`.
    pub fn get_tsc_entry(
        &mut self,
        index: u32,
        gpu_read: &dyn Fn(u64, &mut [u8]),
    ) -> (SamplerDescriptor, bool) {
        let (raw, changed) = self.tsc_table.read(index, gpu_read);
        let words = words_from_bytes(&raw);
        (SamplerDescriptor::from_words(&words), changed)
    }

    /// Decode a texture handle into `(tic_id, tsc_id)` based on sampler
    /// binding mode.
    pub fn decode_texture_handle(&self, handle: u32) -> (u32, u32) {
        let linked = self.regs[SAMPLER_BINDING as usize] == SamplerBinding::ViaHeaderBinding as u32;
        if linked {
            // Same index for both TIC and TSC.
            (handle, handle)
        } else {
            // Independent: bits[19:0] = tic_id, bits[31:20] = tsc_id.
            let tic_id = handle & 0xF_FFFF; // 20 bits
            let tsc_id = (handle >> 20) & 0xFFF; // 12 bits
            (tic_id, tsc_id)
        }
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

    /// GPU virtual base address of the shader program region.
    ///
    /// Upstream: `Maxwell3D::Regs::ProgramRegion::Address()` —
    /// `(address_high << 32) | address_low`.
    pub fn program_region_address(&self) -> u64 {
        let high = self.regs[PROGRAM_REGION_HIGH as usize] as u64;
        let low = self.regs[PROGRAM_REGION_LOW as usize] as u64;
        (high << 32) | low
    }

    /// Snapshot the GPU virtual address of the entry point for each of the
    /// 6 Maxwell shader stages. Disabled stages report `0`.
    ///
    /// Upstream rasterizers compute these inline as
    /// `regs.program_region.Address() + regs.pipelines[i].offset`. The Rust
    /// port snapshots them into `DrawState` so the rasterizer doesn't need
    /// a Maxwell3D back-reference.
    pub fn shader_program_addresses(&self) -> [u64; 6] {
        let base = self.program_region_address();
        let mut out = [0u64; 6];
        for i in 0..6u32 {
            if !self.is_shader_stage_enabled(i) {
                continue;
            }
            let info = self.shader_stage_info(i);
            out[i as usize] = base + info.offset as u64;
        }
        out
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
    pub fn const_buffer_bindings(&self, stage: usize) -> &[ConstBufferBinding; MAX_CB_SLOTS] {
        &self.cb_bindings[stage]
    }

    // ── Draw call accessors ──────────────────────────────────────────────

    /// Drain accumulated draw call records.
    pub fn take_draw_calls(&mut self) -> Vec<DrawCall> {
        std::mem::take(&mut self.draw_calls)
    }

    // ── Instance / base vertex accessors ─────────────────────────────────

    /// Base vertex index (signed) from GLOBAL_BASE_VERTEX_INDEX.
    pub fn base_vertex(&self) -> i32 {
        self.regs[GLOBAL_BASE_VERTEX_INDEX as usize] as i32
    }

    /// Base instance index from GLOBAL_BASE_INSTANCE_INDEX.
    pub fn base_instance(&self) -> u32 {
        self.regs[GLOBAL_BASE_INSTANCE_INDEX as usize]
    }

    // ── Report semaphore accessors ───────────────────────────────────────

    /// Report semaphore GPU virtual address (high << 32 | low).
    pub fn report_semaphore_address(&self) -> u64 {
        let high = self.regs[REPORT_SEMAPHORE_BASE as usize] as u64;
        let low = self.regs[(REPORT_SEMAPHORE_BASE + 1) as usize] as u64;
        (high << 32) | low
    }

    /// Report semaphore payload value.
    pub fn report_semaphore_payload(&self) -> u32 {
        self.regs[(REPORT_SEMAPHORE_BASE + 2) as usize]
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

    /// Handle DRAW_BEGIN: captures topology and instance mode from value.
    fn handle_draw_begin(&mut self, value: u32) {
        self.current_topology = PrimitiveTopology::from_raw(value);
        let instance_id = InstanceId::from_raw(value);

        log::debug!(
            "Maxwell3D: DRAW_BEGIN topology={:?} instance_id={:?}",
            self.current_topology,
            instance_id,
        );

        match instance_id {
            InstanceId::First => {
                // Flush any pending instanced draw before resetting.
                if self.draw_mode == DrawMode::Instance && self.instance_count > 0 {
                    self.flush_deferred_draw();
                }
                self.instance_count = 0;
                self.draw_mode = DrawMode::General;
            }
            InstanceId::Subsequent => {
                self.instance_count += 1;
                self.draw_mode = DrawMode::Instance;
            }
            InstanceId::Unchanged => {}
        }
    }

    /// Handle DRAW_END: behaviour depends on current draw mode.
    fn handle_draw_end(&mut self) {
        match self.draw_mode {
            DrawMode::General => {
                let draw = self.build_draw_call(1, Vec::new());
                log::debug!(
                    "Maxwell3D: DRAW_END {:?} verts={}/{} indexed={} streams={}",
                    draw.topology,
                    draw.vertex_first,
                    draw.vertex_count,
                    draw.indexed,
                    draw.vertex_streams.len(),
                );
                self.draw_calls.push(draw);
                self.dispatch_draw_to_rasterizer(false, 1);
            }
            DrawMode::Instance => {
                // Upstream DrawManager::DrawEnd does nothing for instance mode unless forced
                // through DrawDeferred().
            }
            DrawMode::InlineIndex => {
                let inline_data = std::mem::take(&mut self.inline_index_data);
                let mut draw = self.build_draw_call(1, inline_data);
                // Override index fields for inline index draws.
                draw.indexed = true;
                draw.index_format = IndexFormat::UnsignedInt;
                draw.index_buffer_count = draw.inline_index_data.len() as u32 / 4;
                log::debug!(
                    "Maxwell3D: DRAW_END InlineIndex {:?} indices={}",
                    draw.topology,
                    draw.index_buffer_count,
                );
                self.draw_calls.push(draw);
                self.dispatch_draw_to_rasterizer(true, 1);
                self.draw_mode = DrawMode::General;
            }
        }
    }

    /// Build a `DrawState` snapshot from the current Maxwell3D registers
    /// and forward it to the bound rasterizer via the upstream-parity
    /// `RasterizerInterface::draw` entry point.
    ///
    /// Mirrors the `DrawManager::ProcessDraw → rasterizer->Draw` path that
    /// upstream takes inside `Maxwell3D` when a draw method is hit. The
    /// legacy `draw_calls` queue is preserved alongside this call so the
    /// software rasterizer (still consumed by `GpuContext::flush_entries`)
    /// keeps working until the OpenGL pipeline cache produces real GL
    /// programs (gaps 4–5).
    fn dispatch_draw_to_rasterizer(&mut self, is_indexed: bool, instance_count: u32) {
        let draw_state = self.build_draw_state(is_indexed, instance_count);
        let _ = self.with_rasterizer_mut(|rasterizer| {
            rasterizer.draw(&draw_state, instance_count);
        });
    }

    /// Build a `DrawState` reflecting the current Maxwell3D register file.
    fn build_draw_state(
        &self,
        is_indexed: bool,
        instance_count: u32,
    ) -> crate::engines::draw_manager::DrawState {
        use crate::engines::draw_manager as dm;
        crate::engines::draw_manager::DrawState {
            topology: self.current_topology,
            draw_mode: match self.draw_mode {
                DrawMode::General => dm::DrawMode::General,
                DrawMode::Instance => dm::DrawMode::Instance,
                DrawMode::InlineIndex => dm::DrawMode::InlineIndex,
            },
            draw_indexed: is_indexed,
            base_index: self.base_vertex() as u32,
            vertex_buffer: dm::VertexBuffer {
                first: self.regs[VB_FIRST as usize],
                count: self.regs[VB_COUNT as usize],
            },
            index_buffer: dm::IndexBuffer {
                first: self.index_buffer_first(),
                count: self.index_buffer_count(),
                format: self.index_buffer_format(),
            },
            base_instance: self.base_instance(),
            instance_count,
            inline_index_draw_indexes: Vec::new(),
            shader_program_addresses: self.shader_program_addresses(),
            index_buffer_gpu_addr: self.index_buffer_addr(),
            index_buffer_gpu_addr_end: {
                let base = IB_BASE as usize;
                let high = self.regs[base + IB_OFF_LIMIT_HIGH as usize] as u64;
                let low = self.regs[base + IB_OFF_LIMIT_LOW as usize] as u64;
                (high << 32) | low
            },
        }
    }

    /// Flush a deferred instanced draw batch. Called when a new First
    /// instance_id is seen or when take_draw_calls needs to finalize.
    fn flush_deferred_draw(&mut self) {
        let count = self.instance_count + 1;
        let draw = self.build_draw_call(count, Vec::new());
        log::debug!(
            "Maxwell3D: flush_deferred_draw {:?} instance_count={}",
            draw.topology,
            count,
        );
        self.draw_calls.push(draw);
        self.dispatch_draw_to_rasterizer(false, count);
        self.instance_count = 0;
        self.draw_mode = DrawMode::General;
    }

    /// Build a DrawCall from current register state with the given instance
    /// count and optional inline index data.
    fn build_draw_call(&self, instance_count: u32, inline_index_data: Vec<u8>) -> DrawCall {
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

        // Collect render target configurations.
        let render_targets: [RenderTargetInfo; 8] = std::array::from_fn(|i| RenderTargetInfo {
            address: self.rt_address(i),
            width: self.rt_width(i),
            height: self.rt_height(i),
            format: self.rt_format(i),
        });

        // Collect blend info for all 8 render targets.
        let mut blend = [BlendInfo::default(); 8];
        for (i, b) in blend.iter_mut().enumerate() {
            *b = self.effective_blend_info(i);
        }

        DrawCall {
            topology: self.current_topology,
            vertex_first: self.regs[VB_FIRST as usize],
            vertex_count: self.regs[VB_COUNT as usize],
            indexed: self.draw_indexed,
            index_buffer_addr: self.index_buffer_addr(),
            index_buffer_count: self.index_buffer_count(),
            index_buffer_first: self.index_buffer_first(),
            index_format: self.index_buffer_format(),
            vertex_streams,
            viewports: std::array::from_fn(|i| self.viewport_info(i as u32)),
            scissors: std::array::from_fn(|i| self.scissor_info(i as u32)),
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
            tex_header_pool_addr: self.tex_header_pool_address(),
            tex_header_pool_limit: self.tex_header_pool_limit(),
            tex_sampler_pool_addr: self.tex_sampler_pool_address(),
            tex_sampler_pool_limit: self.tex_sampler_pool_limit(),
            instance_count,
            base_instance: self.base_instance(),
            base_vertex: self.base_vertex(),
            inline_index_data,
            sampler_binding: if self.regs[SAMPLER_BINDING as usize] == 1 {
                SamplerBinding::ViaHeaderBinding
            } else {
                SamplerBinding::Independently
            },
            render_targets,
        }
    }

    /// Handle report semaphore trigger (write to REPORT_SEMAPHORE_BASE + 3).
    fn handle_report_semaphore(&mut self, value: u32) {
        let _ = value;
        self.process_query_get();
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
            log::trace!("Maxwell3D: CB_BIND stage={} slot={} disabled", stage, slot);
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
        RT_FORMAT_A8B8G8R8_UNORM | RT_FORMAT_A8B8G8R8_SRGB => [r, g, b, a],
        // R8: single-channel 8-bit
        RT_FORMAT_R8_UNORM => [r, 0, 0, 255],
        // B5G6R5: 16-bit packed, convert to RGBA8
        RT_FORMAT_B5G6R5_UNORM => [r, g, b, 255],
        // All other formats: default to RGBA8 layout (float/HDR formats are
        // quantized to 8-bit for the internal framebuffer representation).
        _ => {
            log::trace!(
                "Maxwell3D: RT format 0x{:X}, using RGBA8 layout for clear",
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

impl Maxwell3D {
    // ── Upstream-matching execution mask ─────────────────────────────────

    /// Determine whether a method triggers immediate execution (matching
    /// upstream `Maxwell3D::IsMethodExecutable`).
    fn is_method_executable(method: u32) -> bool {
        if method >= MACRO_REGISTERS_START {
            return true;
        }
        match method {
            DRAW_END | DRAW_BEGIN | VB_FIRST | VB_COUNT => true,
            m if m == IB_BASE + IB_OFF_FIRST => true,
            m if m == IB_BASE + IB_OFF_COUNT => true,
            DRAW_INLINE_INDEX => true,
            INDEX_BUFFER32_SUBSEQUENT | INDEX_BUFFER16_SUBSEQUENT | INDEX_BUFFER8_SUBSEQUENT => {
                true
            }
            INDEX_BUFFER32_FIRST | INDEX_BUFFER16_FIRST | INDEX_BUFFER8_FIRST => true,
            INLINE_INDEX_2X16_EVEN | INLINE_INDEX_4X8_INDEX0 => true,
            VERTEX_ARRAY_INSTANCE_FIRST | VERTEX_ARRAY_INSTANCE_SUBSEQUENT => true,
            DRAW_TEXTURE_SRC_Y0 => true,
            WAIT_FOR_IDLE | SHADOW_RAM_CONTROL => true,
            LOAD_MME_INSTRUCTION_PTR | LOAD_MME_INSTRUCTION | LOAD_MME_START_ADDR => true,
            FALCON4 => true,
            m if m >= CB_DATA_BASE && m < CB_DATA_END => true,
            CB_BIND_TRIGGER_0 | CB_BIND_TRIGGER_1 | CB_BIND_TRIGGER_2 | CB_BIND_TRIGGER_3
            | CB_BIND_TRIGGER_4 => true,
            TOPOLOGY_OVERRIDE | CLEAR_SURFACE => true,
            REPORT_SEMAPHORE_QUERY => true,
            RENDER_ENABLE_MODE | CLEAR_REPORT_VALUE | SYNC_INFO => true,
            LAUNCH_DMA | INLINE_DATA => true,
            FRAGMENT_BARRIER | INVALIDATE_TEXTURE_DATA_CACHE | TILED_CACHE_BARRIER => true,
            _ => false,
        }
    }

    // ── Shadow RAM processing (matching upstream ProcessShadowRam) ──────

    /// Process shadow RAM for a register write, returning the effective
    /// argument value. Matches upstream `Maxwell3D::ProcessShadowRam`.
    fn process_shadow_ram(&mut self, method: u32, argument: u32) -> u32 {
        let control = ShadowRamControl::from_raw(self.shadow_state[SHADOW_RAM_CONTROL as usize]);
        match control {
            ShadowRamControl::Track | ShadowRamControl::TrackWithFilter => {
                self.shadow_state[method as usize] = argument;
                argument
            }
            ShadowRamControl::Replay => self.shadow_state[method as usize],
            ShadowRamControl::MethodTrack => argument,
        }
    }

    // ── Dirty register tracking (matching upstream ProcessDirtyRegisters) ─

    /// Update the register value if changed. Matches upstream
    /// `Maxwell3D::ProcessDirtyRegisters`.
    fn process_dirty_registers(&mut self, method: u32, argument: u32) {
        let idx = method as usize;
        if idx < ENGINE_REG_COUNT {
            if self.regs[idx] == argument {
                return;
            }
            self.regs[idx] = argument;
        }
        // In the full implementation, this would update dirty flag tables.
        // For now, all registers are implicitly dirty on change.
    }

    // ── Method call dispatch (matching upstream ProcessMethodCall) ───────

    /// Dispatch a method call with side effects. Matches upstream
    /// `Maxwell3D::ProcessMethodCall`.
    fn process_method_call(
        &mut self,
        method: u32,
        argument: u32,
        nonshadow_argument: u32,
        is_last_call: bool,
    ) {
        if should_trace_dma_flow() {
            match method {
                WAIT_FOR_IDLE
                | REPORT_SEMAPHORE_QUERY
                | SYNC_INFO
                | LAUNCH_DMA
                | FRAGMENT_BARRIER
                | INVALIDATE_TEXTURE_DATA_CACHE
                | TILED_CACHE_BARRIER => {
                    log::info!(
                        "Maxwell3D::process_method_call method=0x{:X} arg=0x{:X} nonshadow=0x{:X} last={}",
                        method,
                        argument,
                        nonshadow_argument,
                        is_last_call
                    );
                }
                _ => {}
            }
        }
        match method {
            WAIT_FOR_IDLE => {
                let _ = self.with_rasterizer_mut(|rasterizer| rasterizer.wait_for_idle());
            }
            SHADOW_RAM_CONTROL => {
                self.shadow_state[SHADOW_RAM_CONTROL as usize] = nonshadow_argument;
            }
            LOAD_MME_INSTRUCTION_PTR => {
                let ptr = self.regs[LOAD_MME_INSTRUCTION_PTR as usize];
                self.macro_engine.clear_code(ptr);
            }
            LOAD_MME_INSTRUCTION => {
                let ptr = self.regs[LOAD_MME_INSTRUCTION_PTR as usize];
                self.macro_engine.add_code(ptr, argument);
            }
            LOAD_MME_START_ADDR => {
                self.process_macro_bind(argument);
            }
            FALCON4 => {
                self.process_firmware_call4();
            }
            m if m >= CB_DATA_BASE && m < CB_DATA_END => {
                self.process_cb_data(argument);
            }
            CB_BIND_TRIGGER_0 => self.process_cb_bind(0),
            CB_BIND_TRIGGER_1 => self.process_cb_bind(1),
            CB_BIND_TRIGGER_2 => self.process_cb_bind(2),
            CB_BIND_TRIGGER_3 => self.process_cb_bind(3),
            CB_BIND_TRIGGER_4 => self.process_cb_bind(4),
            REPORT_SEMAPHORE_QUERY => {
                self.process_query_get();
            }
            RENDER_ENABLE_MODE => {
                self.process_query_condition();
            }
            CLEAR_REPORT_VALUE => {
                self.process_counter_reset();
            }
            SYNC_INFO => {
                self.process_sync_point();
            }
            LAUNCH_DMA => {
                self.upload_state.regs = self.upload_registers();
                self.upload_state.process_exec(self.launch_dma_is_linear());
            }
            INLINE_DATA => {
                self.process_inline_upload_word(argument, is_last_call);
            }
            FRAGMENT_BARRIER => {
                let _ = self.with_rasterizer_mut(|rasterizer| rasterizer.fragment_barrier());
            }
            INVALIDATE_TEXTURE_DATA_CACHE => {
                let _ = self.with_rasterizer_mut(|rasterizer| {
                    rasterizer.invalidate_gpu_cache();
                    rasterizer.wait_for_idle();
                });
            }
            TILED_CACHE_BARRIER => {
                let _ = self.with_rasterizer_mut(|rasterizer| rasterizer.tiled_cache_barrier());
            }
            _ => {
                // Delegate to draw manager equivalent.
                self.process_draw_method_call(method, argument);
            }
        }
    }

    /// Handle draw-related method calls. Matches the upstream
    /// `DrawManager::ProcessMethodCall` dispatch.
    fn process_draw_method_call(&mut self, method: u32, argument: u32) {
        match method {
            CLEAR_SURFACE => {
                self.handle_clear_surface(argument);
            }
            DRAW_BEGIN => {
                self.handle_draw_begin(argument);
            }
            DRAW_END => {
                self.handle_draw_end();
            }
            VB_FIRST | VB_COUNT => {
                // Values already written to regs by process_dirty_registers.
            }
            m if m == IB_BASE + IB_OFF_FIRST => {
                // Value already written to regs.
            }
            m if m == IB_BASE + IB_OFF_COUNT => {
                self.draw_indexed = true;
            }
            INDEX_BUFFER32_SUBSEQUENT | INDEX_BUFFER16_SUBSEQUENT | INDEX_BUFFER8_SUBSEQUENT => {
                self.instance_count += 1;
                self.draw_index_small(argument);
            }
            INDEX_BUFFER32_FIRST | INDEX_BUFFER16_FIRST | INDEX_BUFFER8_FIRST => {
                self.draw_index_small(argument);
            }
            DRAW_INLINE_INDEX => {
                self.inline_index_data
                    .extend_from_slice(&argument.to_le_bytes());
                self.draw_mode = DrawMode::InlineIndex;
            }
            INLINE_INDEX_2X16_EVEN => {
                let even = self.regs[INLINE_INDEX_2X16_EVEN as usize] & 0xFFFF;
                let odd = (self.regs[INLINE_INDEX_2X16_EVEN as usize] >> 16) & 0xFFFF;
                self.inline_index_data
                    .extend_from_slice(&even.to_le_bytes());
                self.inline_index_data.extend_from_slice(&odd.to_le_bytes());
                self.draw_mode = DrawMode::InlineIndex;
            }
            INLINE_INDEX_4X8_INDEX0 => {
                let raw = self.regs[INLINE_INDEX_4X8_INDEX0 as usize];
                for shift in [0, 8, 16, 24] {
                    let idx = (raw >> shift) & 0xFF;
                    self.inline_index_data.extend_from_slice(&idx.to_le_bytes());
                }
                self.draw_mode = DrawMode::InlineIndex;
            }
            VERTEX_ARRAY_INSTANCE_FIRST => {
                self.draw_array_instanced(argument, false);
            }
            VERTEX_ARRAY_INSTANCE_SUBSEQUENT => {
                self.draw_array_instanced(argument, true);
            }
            DRAW_TEXTURE_SRC_Y0 => {
                self.draw_texture();
            }
            TOPOLOGY_OVERRIDE => {
                // Value written to regs; topology override handled during draw.
            }
            _ => {}
        }
    }

    /// Handle small indexed draw (index_buffer{32,16,8}_{first,subsequent}).
    fn draw_index_small(&mut self, argument: u32) {
        let params = IndexBufferSmallParams::from_raw(argument);
        let mut draw = self.build_draw_call(1, Vec::new());
        draw.topology = params.topology;
        draw.indexed = true;
        draw.index_buffer_first = params.first;
        draw.index_buffer_count = params.count;
        self.draw_calls.push(draw);
        self.dispatch_draw_to_rasterizer(true, 1);
    }

    /// Handle `vertex_array_instance_{first,subsequent}` immediate instanced draws.
    fn draw_array_instanced(&mut self, argument: u32, subsequent: bool) {
        let params = VertexArrayParams::from_raw(argument);
        self.current_topology = params.topology;
        self.draw_mode = DrawMode::Instance;
        if !subsequent {
            self.instance_count = 1;
        }

        let mut draw = self.build_draw_call(1, Vec::new());
        draw.topology = params.topology;
        draw.indexed = false;
        draw.vertex_first = params.start;
        draw.vertex_count = params.count;
        draw.base_instance = self.instance_count.saturating_sub(1);
        self.instance_count += 1;
        self.draw_calls.push(draw);
        // Note: instance_count was just incremented; the dispatch reflects
        // the *current* (post-increment) instance count, mirroring how
        // upstream `DrawManager::DrawIndex` reads the live register state.
        self.dispatch_draw_to_rasterizer(false, self.instance_count);
    }

    /// Handle draw-texture trigger.
    fn draw_texture(&mut self) {
        let _ = self.with_rasterizer_mut(|rasterizer| rasterizer.draw_texture());
    }

    // ── Upstream ProcessCBData / ProcessCBMultiData / ProcessCBBind ──────

    /// Handle CB_DATA write: write to const buffer at current offset and
    /// auto-increment offset by 4. Matches upstream `ProcessCBData`.
    fn process_cb_data(&mut self, value: u32) {
        self.process_cb_multi_data(&[value]);
    }

    /// Batch write to const buffer. Matches upstream `ProcessCBMultiData`.
    fn process_cb_multi_data(&mut self, data: &[u32]) {
        let cb_base = CB_CONFIG_BASE as usize;
        let addr_high = self.regs[cb_base + 1] as u64;
        let addr_low = self.regs[cb_base + 2] as u64;
        let buffer_address = (addr_high << 32) | addr_low;

        if buffer_address == 0 {
            log::warn!("Maxwell3D: ProcessCBMultiData with null buffer address");
            return;
        }

        let offset = self.regs[cb_base + 3];
        let size = self.regs[cb_base];
        if offset > size {
            log::warn!(
                "Maxwell3D: ProcessCBMultiData offset 0x{:X} > size 0x{:X}",
                offset,
                size
            );
            return;
        }

        let copy_size = data.len() as u32 * 4;
        let address = buffer_address.wrapping_add(offset as u64);
        if let Some(memory_manager) = self.memory_manager.as_ref().cloned() {
            let writer = self.guest_memory_writer.as_ref().cloned();
            let mut write_cpu = move |addr: u64, bytes: &[u8]| {
                if let Some(writer) = writer.as_ref() {
                    writer(addr, bytes);
                }
            };
            let mut bytes = Vec::with_capacity(copy_size as usize);
            for value in data {
                bytes.extend_from_slice(&value.to_le_bytes());
            }
            memory_manager
                .lock()
                .write_block_cached(address, &bytes, &mut write_cpu);
        }

        // Increment the current buffer position.
        self.regs[cb_base + 3] = offset.wrapping_add(copy_size);
    }

    /// Handle CB_BIND trigger for a shader stage. Matches upstream `ProcessCBBind`.
    fn process_cb_bind(&mut self, stage_index: usize) {
        let bind_base = (CB_BIND_BASE + stage_index as u32 * CB_BIND_STRIDE) as usize;
        let raw_config = self.regs[bind_base + 4];

        let valid = (raw_config & 1) != 0;
        let slot = ((raw_config >> 4) & 0x1F) as usize;

        if slot >= MAX_CB_SLOTS {
            log::warn!(
                "Maxwell3D: CB_BIND stage {} slot {} out of range",
                stage_index,
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

            self.cb_bindings[stage_index][slot] = ConstBufferBinding {
                enabled: true,
                address,
                size,
            };
            log::trace!(
                "Maxwell3D: CB_BIND stage={} slot={} addr=0x{:X} size={}",
                stage_index,
                slot,
                address,
                size
            );
            let _ = self.with_rasterizer_mut(|rasterizer| {
                rasterizer.bind_graphics_uniform_buffer(stage_index, slot as u32, address, size)
            });
        } else {
            self.cb_bindings[stage_index][slot] = ConstBufferBinding::default();
            log::trace!(
                "Maxwell3D: CB_BIND stage={} slot={} disabled",
                stage_index,
                slot
            );
            let _ = self.with_rasterizer_mut(|rasterizer| {
                rasterizer.disable_graphics_uniform_buffer(stage_index, slot as u32)
            });
        }
    }

    // ── Upstream ProcessMacroUpload / ProcessMacroBind / ProcessFirmwareCall4 ──

    /// Upload a macro code word. Matches upstream `ProcessMacroUpload`.
    fn process_macro_upload(&mut self, data: u32) {
        let ptr = self.regs[LOAD_MME_INSTRUCTION_PTR as usize];
        self.macro_engine.add_code(ptr, data);
        self.regs[LOAD_MME_INSTRUCTION_PTR as usize] = ptr.wrapping_add(1);
    }

    /// Bind a macro start address. Matches upstream `ProcessMacroBind`.
    fn process_macro_bind(&mut self, data: u32) {
        let ptr = self.regs[LOAD_MME_START_ADDR_PTR as usize];
        self.macro_positions[ptr as usize] = data;
        self.regs[LOAD_MME_START_ADDR_PTR as usize] = ptr + 1;
        log::info!(
            "Maxwell3D::process_macro_bind slot={} start=0x{:X}",
            ptr,
            data
        );
    }

    /// Handle firmware call 4. Matches upstream `ProcessFirmwareCall4`.
    fn process_firmware_call4(&mut self) {
        // Firmware call 4 changes some registers depending on its parameters.
        // These registers don't affect emulation, so set shadow_scratch[0] = 1.
        self.regs[SHADOW_SCRATCH_BASE as usize] = 1;
    }

    pub(crate) fn hle_clear_const_buffer(&mut self, base_size: usize, parameters: &[u32]) {
        self.refresh_parameters();
        if parameters.len() < 3 {
            return;
        }
        self.regs[CB_CONFIG_BASE as usize] = base_size as u32;
        self.regs[CB_CONFIG_BASE as usize + 1] = parameters[0];
        self.regs[CB_CONFIG_BASE as usize + 2] = parameters[1];
        self.regs[CB_CONFIG_BASE as usize + 3] = 0;
        let zeroes = vec![0u32; parameters[2] as usize];
        self.process_cb_multi_data(&zeroes);
    }

    pub(crate) fn hle_clear_memory(&mut self, parameters: &[u32], zero_memory: &mut Vec<u32>) {
        self.refresh_parameters();
        if parameters.len() < 3 {
            return;
        }
        let needed_memory = (parameters[2] / std::mem::size_of::<u32>() as u32) as usize;
        if needed_memory > zero_memory.len() {
            zero_memory.resize(needed_memory, 0);
        }
        self.regs[UPLOAD_REGS_BASE] = parameters[2];
        self.regs[UPLOAD_REGS_BASE + 1] = 1;
        self.regs[UPLOAD_REGS_BASE + 2] = parameters[0];
        self.regs[UPLOAD_REGS_BASE + 3] = parameters[1];
        <Self as EngineInterface>::call_method(self, LAUNCH_DMA, 0x1011, true);
        <Self as EngineInterface>::call_multi_method(
            self,
            INLINE_DATA,
            &zero_memory[..needed_memory],
            needed_memory as u32,
            needed_memory as u32,
        );
    }

    // ── Upstream ProcessQueryGet / ProcessQueryCondition / etc ───────────

    /// Handle report semaphore query. Matches upstream `ProcessQueryGet`.
    fn process_query_get(&mut self) {
        let query_word = self.regs[(REPORT_SEMAPHORE_BASE + 3) as usize];
        let operation = ReportOperation::from_raw(query_word);
        match operation {
            ReportOperation::Release | ReportOperation::ReportOnly => {
                let gpu_va = self.report_semaphore_address();
                let payload = self.report_semaphore_payload();
                let short_query = (query_word >> 28) & 1 != 0;
                let query_type = (query_word >> 24) & 0xF;
                let subreport = (query_word >> 16) & 0x3F;
                let mut flags = QueryPropertiesFlags::empty();
                if !short_query {
                    flags |= QueryPropertiesFlags::HAS_TIMEOUT;
                }
                if short_query && operation == ReportOperation::Release {
                    flags |= QueryPropertiesFlags::IS_A_FENCE;
                }
                let mut queried = false;
                let gpu_write: Arc<dyn Fn(u64, &[u8]) + Send + Sync> = {
                    let memory_manager = self.memory_manager.as_ref().cloned();
                    let guest_memory_writer = self.guest_memory_writer.as_ref().cloned();
                    Arc::new(move |gpu_addr: u64, bytes: &[u8]| {
                        let Some(memory_manager) = memory_manager.as_ref() else {
                            return;
                        };
                        let Some(guest_memory_writer) = guest_memory_writer.as_ref() else {
                            return;
                        };
                        memory_manager.lock().write_block_unsafe(
                            gpu_addr,
                            bytes,
                            &mut |cpu_addr, data| guest_memory_writer(cpu_addr, data),
                        );
                    })
                };

                log::debug!(
                    "Maxwell3D: query_get {:?} va=0x{:X} payload=0x{:X} short={} type={} subreport={}",
                    operation,
                    gpu_va,
                    payload,
                    short_query,
                    query_type,
                    subreport,
                );

                let gpu_ticks = self
                    .gpu_ticks_getter
                    .as_ref()
                    .map(|getter| getter())
                    .unwrap_or(0);
                let _ = self.with_rasterizer_mut(|rasterizer| {
                    queried = true;
                    rasterizer.query(
                        gpu_va,
                        query_type,
                        flags,
                        gpu_ticks,
                        payload,
                        subreport,
                        Arc::clone(&gpu_write),
                    );
                });

                if !queried {
                    let gpu_ticks = self
                        .gpu_ticks_getter
                        .as_ref()
                        .map(|getter| getter())
                        .unwrap_or(0);
                    let data = if short_query {
                        payload.to_le_bytes().to_vec()
                    } else {
                        let mut buf = Vec::with_capacity(16);
                        buf.extend_from_slice(&(payload as u64).to_le_bytes());
                        buf.extend_from_slice(&gpu_ticks.to_le_bytes());
                        buf
                    };
                    self.pending_semaphore_writes
                        .push(PendingWrite { gpu_va, data });
                }
            }
            ReportOperation::Acquire => {
                log::debug!("Maxwell3D: query_get Acquire (unimplemented)");
            }
            ReportOperation::Trap => {
                log::debug!("Maxwell3D: query_get Trap (unimplemented)");
            }
        }
    }

    /// Handle render enable / query condition. Matches upstream `ProcessQueryCondition`.
    fn process_query_condition(&mut self) {
        let accelerated = self
            .with_rasterizer_mut(|rasterizer| rasterizer.accelerate_conditional_rendering())
            .unwrap_or(false);
        if accelerated {
            self.execute_on = true;
            return;
        }

        let override_val = self.regs[RENDER_ENABLE_OVERRIDE as usize];
        match override_val {
            0 => {
                let mode = self.regs[RENDER_ENABLE_MODE as usize];
                match mode {
                    0 => self.execute_on = false,
                    1 => self.execute_on = true,
                    2..=4 => {
                        let condition_address = ((self.regs[RENDER_ENABLE_BASE as usize] as u64)
                            << 32)
                            | self.regs[(RENDER_ENABLE_BASE + 1) as usize] as u64;
                        let mut compare_bytes = [0u8; 24];
                        if !self.read_gpu_block(condition_address, &mut compare_bytes) {
                            self.execute_on = true;
                            return;
                        }
                        let read_word = |index: usize| -> u32 {
                            let start = index * 4;
                            u32::from_le_bytes(compare_bytes[start..start + 4].try_into().unwrap())
                        };
                        let initial_sequence = read_word(0);
                        let initial_mode = read_word(1);
                        let current_sequence = read_word(4);
                        let current_mode = read_word(5);
                        self.execute_on = match mode {
                            2 => initial_sequence != 0 && initial_mode != 0,
                            3 => {
                                initial_sequence == current_sequence && initial_mode == current_mode
                            }
                            4 => {
                                initial_sequence != current_sequence || initial_mode != current_mode
                            }
                            _ => unreachable!(),
                        };
                    }
                    _ => {
                        log::warn!("Maxwell3D: unknown render_enable mode {}", mode);
                        self.execute_on = true;
                    }
                }
            }
            1 => {
                self.execute_on = true;
            }
            2 => {
                self.execute_on = false;
            }
            _ => {
                log::warn!("Maxwell3D: unknown render_enable override {}", override_val);
                self.execute_on = true;
            }
        }
    }

    /// Handle counter reset. Matches upstream `ProcessCounterReset`.
    fn process_counter_reset(&mut self) {
        let clear_report = self.regs[CLEAR_REPORT_VALUE as usize];
        log::debug!("Maxwell3D: counter_reset report=0x{:X}", clear_report);
        let query_type = match clear_report {
            1 => 2,
            2 => 6,
            3 => 1,
            4 => 7,
            _ => 0,
        };
        let _ = self.with_rasterizer_mut(|rasterizer| rasterizer.reset_counter(query_type));
    }

    /// Handle sync point. Matches upstream `ProcessSyncPoint`.
    fn process_sync_point(&mut self) {
        let sync_point = self.regs[SYNC_INFO as usize] & 0xFFFF;
        if std::env::var_os("RUZU_TRACE_GPU_SUBMIT").is_some() {
            log::info!("Maxwell3D::process_sync_point sync_point={}", sync_point);
        }
        log::debug!("Maxwell3D: sync_point {}", sync_point);
        let _ = self.with_rasterizer_mut(|rasterizer| rasterizer.signal_sync_point(sync_point));
    }

    // ── Macro processing (matching upstream ProcessMacro / CallMacroMethod) ─

    /// Process a macro method call. Matches upstream `Maxwell3D::ProcessMacro`.
    fn process_macro(&mut self, method: u32, base_start: &[u32], is_last_call: bool) {
        if self.executing_macro == 0 {
            // A macro call must begin by writing the macro method's register.
            assert!(
                (method % 2) == 0,
                "Can't start macro execution by writing to the ARGS register"
            );
            self.executing_macro = method;
        }

        self.macro_params.extend_from_slice(base_start);
        for i in 0..base_start.len() {
            self.macro_addresses
                .push(self.interface_state.current_dma_segment + i as u64 * 4);
        }
        self.macro_segments.push((
            self.interface_state.current_dma_segment,
            base_start.len() as u32,
        ));
        self.current_macro_dirty |= self.interface_state.current_dirty;
        self.interface_state.current_dirty = false;

        // Call the macro when there are no more parameters in the command buffer.
        if is_last_call {
            self.consume_sink();
            self.call_macro_method(self.executing_macro);
        }
    }

    /// Execute a macro. Matches upstream `Maxwell3D::CallMacroMethod`.
    fn call_macro_method(&mut self, method: u32) {
        self.executing_macro = 0;

        let entry = ((method - MACRO_REGISTERS_START) >> 1) % 128;
        let params = std::mem::take(&mut self.macro_params);
        let macro_method = self.macro_positions[entry as usize];
        if should_trace_dma_flow() {
            let trace_idx = MAXWELL3D_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if trace_idx < 128 {
                log::info!(
                    "Maxwell3D::call_macro_method entry={} macro_start=0x{:X} param_count={} params={:08X?} dirty={}",
                    entry,
                    macro_method,
                    params.len(),
                    params,
                    self.current_macro_dirty
                );
            }
        } else if macro_method == 0x14F {
            log::info!(
                "Maxwell3D::call_macro_method trace entry={} macro_start=0x{:X} params={:08X?} addrs={:X?} segments={:X?} dirty={}",
                entry,
                macro_method,
                params,
                self.macro_addresses,
                self.macro_segments,
                self.current_macro_dirty
            );
        }
        let self_raw = std::ptr::from_mut(self);
        let self_ptr = Maxwell3DPtr(self_raw);
        self.macro_engine.set_maxwell_3d(self_raw);
        let compile = |code: &[u32]| -> Box<dyn crate::macro_engine::macro_engine::CachedMacro> {
            let mut program = MacroInterpreterImpl::new(code.to_vec());
            let writer_ptr = self_ptr;
            program.set_method_writer(move |address, value, _is_last_call| unsafe {
                writer_ptr.call_method(address, value);
            });
            let reader_ptr = self_ptr;
            program.set_method_reader(move |method| unsafe { reader_ptr.read_reg(method) });
            Box::new(program)
        };
        self.macro_engine.execute(
            macro_method,
            &params,
            move || unsafe { (&mut *self_ptr.0).refresh_parameters() },
            compile,
        );

        // Upstream calls draw_manager->DrawDeferred() here.
        // Flush any deferred instanced draw.
        if self.draw_mode == DrawMode::Instance && self.instance_count > 0 {
            self.flush_deferred_draw();
        }

        self.macro_params.clear();
        self.macro_addresses.clear();
        self.macro_segments.clear();
        self.current_macro_dirty = false;
    }

    /// Consume the method sink. Matches upstream `ConsumeSink`.
    pub fn consume_sink(&mut self) {
        if self.interface_state.method_sink.is_empty() {
            return;
        }
        self.consume_sink_inner();
    }

    /// Internal sink consumption matching upstream `ConsumeSinkImpl`.
    fn consume_sink_inner(&mut self) {
        let control = ShadowRamControl::from_raw(self.shadow_state[SHADOW_RAM_CONTROL as usize]);
        let sink = std::mem::take(&mut self.interface_state.method_sink);
        match control {
            ShadowRamControl::Track | ShadowRamControl::TrackWithFilter => {
                for (method, value) in &sink {
                    self.shadow_state[*method as usize] = *value;
                    self.process_dirty_registers(*method, *value);
                }
            }
            ShadowRamControl::Replay => {
                for (method, _value) in &sink {
                    let shadow_val = self.shadow_state[*method as usize];
                    self.process_dirty_registers(*method, shadow_val);
                }
            }
            ShadowRamControl::MethodTrack => {
                for (method, value) in &sink {
                    self.process_dirty_registers(*method, *value);
                }
            }
        }
    }

    // ── Legacy process_method (used by MacroProcessor::macro_write) ─────

    /// Process a method write: store to register array and handle side effects.
    /// This is the shared implementation used by `MacroProcessor::macro_write`.
    fn process_method(&mut self, method: u32, value: u32) {
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
            DRAW_INLINE_INDEX => {
                self.inline_index_data
                    .extend_from_slice(&value.to_le_bytes());
                self.draw_mode = DrawMode::InlineIndex;
            }
            REPORT_SEMAPHORE_TRIGGER => self.handle_report_semaphore(value),
            m if m >= CB_DATA_BASE && m < CB_DATA_END => self.handle_cb_data(value),
            CB_BIND_TRIGGER_0 => self.handle_cb_bind(0),
            CB_BIND_TRIGGER_1 => self.handle_cb_bind(1),
            CB_BIND_TRIGGER_2 => self.handle_cb_bind(2),
            CB_BIND_TRIGGER_3 => self.handle_cb_bind(3),
            CB_BIND_TRIGGER_4 => self.handle_cb_bind(4),
            _ => {}
        }

        log::trace!("Maxwell3D: reg[0x{:X}] = 0x{:X}", method, value);
    }

    /// Handle a write to a macro method register (used by Engine::write_reg
    /// for backward compatibility).
    fn handle_macro_method(&mut self, method: u32, value: u32) {
        // Even method = start of new macro call; odd = additional parameter.
        if self.executing_macro == 0 || (method & 1) == 0 {
            if self.executing_macro != 0 {
                self.flush_macro();
            }
            self.executing_macro = method;
            self.macro_params.clear();
        }
        self.macro_params.push(value);
    }

    /// Execute the pending macro (if any) and reset state.
    pub fn flush_macro(&mut self) {
        if self.executing_macro == 0 || self.macro_params.is_empty() {
            return;
        }
        let method = self.executing_macro;
        let params = std::mem::take(&mut self.macro_params);
        let entry = ((method - MACRO_METHODS_START) >> 1) % 128;
        let macro_method = self.macro_positions[entry as usize];
        let self_raw = std::ptr::from_mut(self);
        let self_ptr = Maxwell3DPtr(self_raw);
        self.macro_engine.set_maxwell_3d(self_raw);
        self.macro_engine.execute(
            macro_method,
            &params,
            move || unsafe { (&mut *self_ptr.0).refresh_parameters() },
            |code| {
                let mut program = MacroInterpreterImpl::new(code.to_vec());
                let writer_ptr = self_ptr;
                program.set_method_writer(move |address, value, _is_last_call| unsafe {
                    writer_ptr.call_method(address, value);
                });
                let reader_ptr = self_ptr;
                program.set_method_reader(move |method| unsafe { reader_ptr.read_reg(method) });
                Box::new(program)
            },
        );
        self.executing_macro = 0;
    }
}

// ── EngineInterface implementation (upstream CallMethod / CallMultiMethod) ──

impl EngineInterface for Maxwell3D {
    /// Write a single value to the register identified by `method`.
    /// Matches upstream `Maxwell3D::CallMethod`.
    fn call_method(&mut self, method: u32, method_argument: u32, is_last_call: bool) {
        if should_trace_dma_flow() {
            let trace_idx = MAXWELL3D_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if trace_idx < 96 {
                log::info!(
                    "Maxwell3D::call_method method=0x{:X} arg=0x{:X} last={}",
                    method,
                    method_argument,
                    is_last_call
                );
            }
        }
        // It is an error to write to a register other than the current macro's
        // ARG register before it has finished execution.
        if self.executing_macro != 0 {
            debug_assert!(
                method == self.executing_macro + 1,
                "Writing to method 0x{:X} while macro 0x{:X} is executing",
                method,
                self.executing_macro
            );
        }

        // Methods >= 0xE00 are macro triggers.
        if method >= MACRO_REGISTERS_START {
            self.process_macro(method, &[method_argument], is_last_call);
            return;
        }

        assert!(
            (method as usize) < ENGINE_REG_COUNT,
            "Invalid Maxwell3D register 0x{:X}, increase ENGINE_REG_COUNT",
            method
        );

        let argument = self.process_shadow_ram(method, method_argument);
        self.process_dirty_registers(method, argument);
        self.process_method_call(method, argument, method_argument, is_last_call);
    }

    /// Write multiple values to the register identified by `method`.
    /// Matches upstream `Maxwell3D::CallMultiMethod`.
    fn call_multi_method(
        &mut self,
        method: u32,
        base_start: &[u32],
        amount: u32,
        methods_pending: u32,
    ) {
        let amount = amount as usize;

        // Methods >= 0xE00 are macro triggers.
        if method >= MACRO_REGISTERS_START {
            self.process_macro(
                method,
                &base_start[..amount],
                amount as u32 == methods_pending,
            );
            return;
        }

        match method {
            m if m >= CB_DATA_BASE && m < CB_DATA_END => {
                self.process_cb_multi_data(&base_start[..amount]);
            }
            INLINE_DATA => {
                assert!(methods_pending == amount as u32);
                self.process_inline_upload_multi(&base_start[..amount]);
            }
            _ => {
                for i in 0..amount {
                    let is_last = methods_pending.wrapping_sub(i as u32) <= 1;
                    self.call_method(method, base_start[i], is_last);
                }
            }
        }
    }

    fn consume_sink_impl(&mut self) {
        // Call the inherent method (not the trait method) to avoid infinite recursion.
        self.consume_sink_inner();
    }

    fn execution_mask(&self) -> &[bool] {
        &self.interface_state.execution_mask
    }

    fn push_method_sink(&mut self, method: u32, value: u32) {
        self.interface_state.method_sink.push((method, value));
    }

    fn set_current_dma_segment(&mut self, segment: u64) {
        self.interface_state.current_dma_segment = segment;
    }

    fn current_dirty(&self) -> bool {
        self.interface_state.current_dirty
    }

    fn set_current_dirty(&mut self, dirty: bool) {
        self.interface_state.current_dirty = dirty;
    }
}

/// Convert 32 raw bytes to 8 u32 words (little-endian).
fn words_from_bytes(bytes: &[u8; 32]) -> [u32; 8] {
    let mut words = [0u32; 8];
    for i in 0..8 {
        words[i] = u32::from_le_bytes([
            bytes[i * 4],
            bytes[i * 4 + 1],
            bytes[i * 4 + 2],
            bytes[i * 4 + 3],
        ]);
    }
    words
}

impl Engine for Maxwell3D {
    fn class_id(&self) -> ClassId {
        ClassId::Threed
    }

    fn write_reg(&mut self, method: u32, value: u32) {
        <Self as EngineInterface>::call_method(self, method, value, true);
    }

    fn take_framebuffer(&mut self) -> Option<Framebuffer> {
        self.pending_framebuffer.take()
    }

    fn execute_pending(&mut self, _read_gpu: &dyn Fn(u64, &mut [u8])) -> Vec<PendingWrite> {
        std::mem::take(&mut self.pending_semaphore_writes)
    }

    fn take_draw_calls(&mut self) -> Vec<DrawCall> {
        self.take_draw_calls()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rasterizer_interface::{RasterizerDownloadArea, RasterizerInterface};
    use std::sync::{Arc, Mutex};

    #[derive(Default, Clone)]
    struct RasterizerCalls {
        wait_for_idle: u32,
        draw_texture: u32,
        signal_sync_point: Vec<u32>,
        reset_counter: Vec<u32>,
        query_writes: Vec<(u64, Vec<u8>)>,
        bound_uniforms: Vec<(usize, u32, u64, u32)>,
        disabled_uniforms: Vec<(usize, u32)>,
        accelerate_conditional_rendering: bool,
        inline_to_memory: Vec<(u64, usize, Vec<u8>)>,
        /// (instance_count, draw_indexed, shader_program_addresses) per `draw` call.
        draws: Vec<(u32, bool, [u64; 6])>,
    }

    struct TestRasterizer {
        calls: Arc<Mutex<RasterizerCalls>>,
    }

    impl TestRasterizer {
        fn new(calls: Arc<Mutex<RasterizerCalls>>) -> Self {
            Self { calls }
        }
    }

    impl RasterizerInterface for TestRasterizer {
        fn draw(
            &mut self,
            draw_state: &crate::engines::draw_manager::DrawState,
            instance_count: u32,
        ) {
            self.calls.lock().unwrap().draws.push((
                instance_count,
                draw_state.draw_indexed,
                draw_state.shader_program_addresses,
            ));
        }
        fn draw_texture(&mut self) {
            self.calls.lock().unwrap().draw_texture += 1;
        }
        fn clear(&mut self, _layer_count: u32) {}
        fn dispatch_compute(&mut self) {}
        fn reset_counter(&mut self, query_type: u32) {
            self.calls.lock().unwrap().reset_counter.push(query_type);
        }
        fn query(
            &mut self,
            gpu_addr: u64,
            _query_type: u32,
            flags: QueryPropertiesFlags,
            gpu_ticks: u64,
            payload: u32,
            _subreport: u32,
            gpu_write: Arc<dyn Fn(u64, &[u8]) + Send + Sync>,
        ) {
            let bytes = if flags.contains(QueryPropertiesFlags::HAS_TIMEOUT) {
                let mut buf = Vec::with_capacity(16);
                buf.extend_from_slice(&(payload as u64).to_le_bytes());
                buf.extend_from_slice(&gpu_ticks.to_le_bytes());
                buf
            } else {
                payload.to_le_bytes().to_vec()
            };
            gpu_write(gpu_addr, &bytes);
            self.calls
                .lock()
                .unwrap()
                .query_writes
                .push((gpu_addr, bytes));
        }
        fn bind_graphics_uniform_buffer(
            &mut self,
            stage: usize,
            index: u32,
            gpu_addr: u64,
            size: u32,
        ) {
            self.calls
                .lock()
                .unwrap()
                .bound_uniforms
                .push((stage, index, gpu_addr, size));
        }
        fn disable_graphics_uniform_buffer(&mut self, stage: usize, index: u32) {
            self.calls
                .lock()
                .unwrap()
                .disabled_uniforms
                .push((stage, index));
        }
        fn signal_fence(&mut self, _func: Box<dyn FnOnce() + Send>) {}
        fn sync_operation(&mut self, _func: Box<dyn FnOnce() + Send>) {}
        fn signal_sync_point(&mut self, value: u32) {
            self.calls.lock().unwrap().signal_sync_point.push(value);
        }
        fn signal_reference(&mut self) {}
        fn release_fences(&mut self, _force: bool) {}
        fn flush_all(&mut self) {}
        fn flush_region(&mut self, _addr: u64, _size: u64) {}
        fn must_flush_region(&self, _addr: u64, _size: u64) -> bool {
            false
        }
        fn get_flush_area(&self, addr: u64, size: u64) -> RasterizerDownloadArea {
            RasterizerDownloadArea {
                start_address: addr,
                end_address: addr + size,
                preemptive: false,
            }
        }
        fn invalidate_region(&mut self, _addr: u64, _size: u64) {}
        fn on_cache_invalidation(&mut self, _addr: u64, _size: u64) {}
        fn on_cpu_write(&mut self, _addr: u64, _size: u64) -> bool {
            false
        }
        fn invalidate_gpu_cache(&mut self) {}
        fn unmap_memory(&mut self, _addr: u64, _size: u64) {}
        fn modify_gpu_memory(&mut self, _as_id: usize, _addr: u64, _size: u64) {}
        fn flush_and_invalidate_region(&mut self, _addr: u64, _size: u64) {}
        fn wait_for_idle(&mut self) {
            self.calls.lock().unwrap().wait_for_idle += 1;
        }
        fn fragment_barrier(&mut self) {}
        fn tiled_cache_barrier(&mut self) {}
        fn flush_commands(&mut self) {}
        fn tick_frame(&mut self) {}
        fn accelerate_conditional_rendering(&mut self) -> bool {
            self.calls.lock().unwrap().accelerate_conditional_rendering
        }
        fn accelerate_inline_to_memory(&mut self, address: u64, copy_size: usize, memory: &[u8]) {
            self.calls
                .lock()
                .unwrap()
                .inline_to_memory
                .push((address, copy_size, memory.to_vec()));
        }
    }

    // ── Existing tests ───────────────────────────────────────────────────

    #[test]
    fn test_write_reg() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(0x100, 0xDEAD);
        assert_eq!(engine.regs[0x100], 0xDEAD);
    }

    #[test]
    fn test_write_reg_high_method() {
        // Regression: methods above 0xFFF byte offset were silently dropped.
        // The register file stores upstream word indices, not raw byte offsets.
        let mut engine = Maxwell3D::new();
        engine.write_reg(CLEAR_SURFACE, 0x1234);
        assert_eq!(engine.regs[CLEAR_SURFACE as usize], 0x1234);
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

        let fb = engine
            .take_framebuffer()
            .expect("Should produce framebuffer");
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

        let fb = engine
            .take_framebuffer()
            .expect("Should produce framebuffer");
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
        assert_eq!(d.viewports[0].width, 1280.0);
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
        assert_eq!(
            BlendEquation::from_raw(0x800B),
            BlendEquation::ReverseSubtract
        );
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
        assert_eq!(
            BlendFactor::from_raw(0xC903),
            BlendFactor::OneMinusSrc1Alpha
        );
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
        assert_eq!(
            VertexAttribSize::from_raw(0x01),
            VertexAttribSize::R32G32B32A32
        );
        assert_eq!(
            VertexAttribSize::from_raw(0x02),
            VertexAttribSize::R32G32B32
        );
        assert_eq!(
            VertexAttribSize::from_raw(0x03),
            VertexAttribSize::R16G16B16A16
        );
        assert_eq!(VertexAttribSize::from_raw(0x04), VertexAttribSize::R32G32);
        assert_eq!(VertexAttribSize::from_raw(0x0A), VertexAttribSize::R8G8B8A8);
        assert_eq!(VertexAttribSize::from_raw(0x12), VertexAttribSize::R32);
        assert_eq!(VertexAttribSize::from_raw(0x1D), VertexAttribSize::R8);
        assert_eq!(
            VertexAttribSize::from_raw(0x30),
            VertexAttribSize::A2B10G10R10
        );
        assert_eq!(
            VertexAttribSize::from_raw(0x31),
            VertexAttribSize::B10G11R11
        );
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
        let raw = 3u32 | (16 << 7) | (0x01 << 21) | (7 << 27);
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
            | (1 << 31); // bgra
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
        engine.regs[base + 3] = 64; // register_count
        engine.regs[base + 4] = 0; // binding_group

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
        assert_eq!(
            draws[0].vertex_attribs[0].attrib_type,
            VertexAttribType::Float
        );
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
        assert_eq!(
            draws[0].shader_stages[1].program_type,
            ShaderStageType::VertexB
        );
        assert_eq!(draws[0].shader_stages[1].offset, 0x100);
        assert!(draws[0].shader_stages[5].enabled);
        assert_eq!(
            draws[0].shader_stages[5].program_type,
            ShaderStageType::Fragment
        );
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

    // ── Texture/Sampler descriptor tests ─────────────────────────────────

    #[test]
    fn test_texture_format_values() {
        assert_eq!(TextureFormat::from_raw(0x01), TextureFormat::R32G32B32A32);
        assert_eq!(TextureFormat::from_raw(0x09), TextureFormat::R32);
        assert_eq!(TextureFormat::from_raw(0x1D), TextureFormat::A8B8G8R8);
        assert_eq!(TextureFormat::from_raw(0x24), TextureFormat::R8G8B8A8);
        assert_eq!(TextureFormat::from_raw(0x12), TextureFormat::R8);
        assert_eq!(TextureFormat::from_raw(0x7F), TextureFormat::Invalid);
    }

    #[test]
    fn test_texture_format_compressed() {
        assert_eq!(TextureFormat::from_raw(0x25), TextureFormat::Bc1Rgba);
        assert_eq!(TextureFormat::from_raw(0x27), TextureFormat::Bc3);
        assert_eq!(TextureFormat::from_raw(0x2A), TextureFormat::Bc7);
        assert_eq!(TextureFormat::from_raw(0x40), TextureFormat::Astc2d4x4);
        assert_eq!(TextureFormat::from_raw(0x4D), TextureFormat::Astc2d12x12);
    }

    #[test]
    fn test_texture_type_values() {
        assert_eq!(TextureType::from_raw(0), TextureType::Texture1D);
        assert_eq!(TextureType::from_raw(1), TextureType::Texture2D);
        assert_eq!(TextureType::from_raw(2), TextureType::Texture3D);
        assert_eq!(TextureType::from_raw(3), TextureType::Cubemap);
        assert_eq!(TextureType::from_raw(5), TextureType::Array2D);
        assert_eq!(TextureType::from_raw(6), TextureType::Buffer1D);
        assert_eq!(TextureType::from_raw(8), TextureType::CubemapArray);
        assert_eq!(TextureType::from_raw(9), TextureType::Invalid);
    }

    #[test]
    fn test_component_type_values() {
        assert_eq!(ComponentType::from_raw(0), ComponentType::Invalid);
        assert_eq!(ComponentType::from_raw(1), ComponentType::SNorm);
        assert_eq!(ComponentType::from_raw(2), ComponentType::UNorm);
        assert_eq!(ComponentType::from_raw(3), ComponentType::SInt);
        assert_eq!(ComponentType::from_raw(4), ComponentType::UInt);
        assert_eq!(ComponentType::from_raw(7), ComponentType::Float);
    }

    #[test]
    fn test_swizzle_source_values() {
        assert_eq!(SwizzleSource::from_raw(0), SwizzleSource::Zero);
        assert_eq!(SwizzleSource::from_raw(2), SwizzleSource::R);
        assert_eq!(SwizzleSource::from_raw(3), SwizzleSource::G);
        assert_eq!(SwizzleSource::from_raw(4), SwizzleSource::B);
        assert_eq!(SwizzleSource::from_raw(5), SwizzleSource::A);
        assert_eq!(SwizzleSource::from_raw(7), SwizzleSource::OneFloat);
        assert_eq!(SwizzleSource::from_raw(1), SwizzleSource::Invalid);
    }

    #[test]
    fn test_tic_header_version_values() {
        assert_eq!(TicHeaderVersion::from_raw(0), TicHeaderVersion::OneDBuffer);
        assert_eq!(TicHeaderVersion::from_raw(2), TicHeaderVersion::Pitch);
        assert_eq!(TicHeaderVersion::from_raw(3), TicHeaderVersion::BlockLinear);
        assert_eq!(TicHeaderVersion::from_raw(5), TicHeaderVersion::Invalid);
    }

    #[test]
    fn test_wrap_mode_values() {
        assert_eq!(WrapMode::from_raw(0), WrapMode::Wrap);
        assert_eq!(WrapMode::from_raw(1), WrapMode::Mirror);
        assert_eq!(WrapMode::from_raw(2), WrapMode::ClampToEdge);
        assert_eq!(WrapMode::from_raw(3), WrapMode::Border);
        assert_eq!(WrapMode::from_raw(4), WrapMode::Clamp);
        assert_eq!(WrapMode::from_raw(7), WrapMode::MirrorOnceClampOgl);
    }

    #[test]
    fn test_texture_filter_values() {
        assert_eq!(TextureFilter::from_raw(0), TextureFilter::Invalid);
        assert_eq!(TextureFilter::from_raw(1), TextureFilter::Nearest);
        assert_eq!(TextureFilter::from_raw(2), TextureFilter::Linear);
        assert_eq!(TextureFilter::from_raw(3), TextureFilter::Invalid);
    }

    #[test]
    fn test_mipmap_filter_values() {
        assert_eq!(MipmapFilter::from_raw(0), MipmapFilter::Invalid);
        assert_eq!(MipmapFilter::from_raw(1), MipmapFilter::None);
        assert_eq!(MipmapFilter::from_raw(2), MipmapFilter::Nearest);
        assert_eq!(MipmapFilter::from_raw(3), MipmapFilter::Linear);
    }

    #[test]
    fn test_depth_compare_func_values() {
        assert_eq!(DepthCompareFunc::from_raw(0), DepthCompareFunc::Never);
        assert_eq!(DepthCompareFunc::from_raw(1), DepthCompareFunc::Less);
        assert_eq!(DepthCompareFunc::from_raw(3), DepthCompareFunc::LessEqual);
        assert_eq!(DepthCompareFunc::from_raw(7), DepthCompareFunc::Always);
    }

    #[test]
    fn test_texture_descriptor_basic() {
        // Build a basic 2D RGBA8 texture descriptor.
        let mut words = [0u32; 8];
        // word0: format=0x1D(A8B8G8R8), r_type=UNorm(2), g_type=UNorm(2), b_type=UNorm(2),
        //        a_type=UNorm(2), xyzw swizzle = R(2),G(3),B(4),A(5)
        words[0] = 0x1D
            | (2 << 7)   // r_type = UNorm
            | (2 << 10)  // g_type = UNorm
            | (2 << 13)  // b_type = UNorm
            | (2 << 16)  // a_type = UNorm
            | (2 << 19)  // x_source = R
            | (3 << 22)  // y_source = G
            | (4 << 25)  // z_source = B
            | (5 << 28); // w_source = A
                         // word1: addr_low
        words[1] = 0x0010_0000;
        // word2: addr_high[15:0]=0x0001, header_version=BlockLinear(3) at bits[23:21]
        words[2] = 0x0001 | (3 << 21);
        // word3: max_mip_level=5 at bits[31:28]
        words[3] = 5 << 28;
        // word4: width=1279(+1=1280) at [15:0], texture_type=Texture2D(1) at [26:23]
        words[4] = 1279 | (1 << 23);
        // word5: height=719(+1=720) at [15:0], depth=0(+1=1) at [29:16], normalized=1 at [31]
        words[5] = 719 | (1 << 31);

        let desc = TextureDescriptor::from_words(&words);
        assert_eq!(desc.format, TextureFormat::A8B8G8R8);
        assert_eq!(desc.r_type, ComponentType::UNorm);
        assert_eq!(desc.g_type, ComponentType::UNorm);
        assert_eq!(desc.x_source, SwizzleSource::R);
        assert_eq!(desc.w_source, SwizzleSource::A);
        assert_eq!(desc.address, 0x0001_0010_0000);
        assert_eq!(desc.header_version, TicHeaderVersion::BlockLinear);
        assert_eq!(desc.texture_type, TextureType::Texture2D);
        assert_eq!(desc.width, 1280);
        assert_eq!(desc.height, 720);
        assert_eq!(desc.depth, 1);
        assert_eq!(desc.max_mip_level, 5);
        assert_eq!(desc.block_height, 0);
        assert_eq!(desc.block_depth, 0);
        assert!(!desc.srgb_conversion);
        assert!(desc.normalized_coords);
    }

    #[test]
    fn test_texture_descriptor_block_height_depth() {
        let mut words = [0u32; 8];
        words[0] = 0x1D; // A8B8G8R8
                         // word3: block_height=3 at bits[5:3], block_depth=2 at bits[8:6], max_mip=0
        words[3] = (3 << 3) | (2 << 6);
        let desc = TextureDescriptor::from_words(&words);
        assert_eq!(desc.block_height, 3);
        assert_eq!(desc.block_depth, 2);
    }

    #[test]
    fn test_texture_descriptor_srgb_3d() {
        let mut words = [0u32; 8];
        words[0] = 0x1D; // A8B8G8R8, all other fields zero
                         // word4: srgb_conversion=1 at bit[22], texture_type=Texture3D(2) at [26:23], width=63(+1=64)
        words[4] = 63 | (1 << 22) | (2 << 23);
        // word5: height=63(+1=64), depth=31(+1=32) at [29:16]
        words[5] = 63 | (31 << 16);

        let desc = TextureDescriptor::from_words(&words);
        assert!(desc.srgb_conversion);
        assert_eq!(desc.texture_type, TextureType::Texture3D);
        assert_eq!(desc.width, 64);
        assert_eq!(desc.height, 64);
        assert_eq!(desc.depth, 32);
        assert!(!desc.normalized_coords);
    }

    #[test]
    fn test_texture_descriptor_buffer() {
        let mut words = [0u32; 8];
        words[0] = 0x09; // R32
                         // word2: header_version=OneDBuffer(0) — already zero
                         // word4: texture_type=Buffer1D(6) at [26:23], width=255(+1=256)
        words[4] = 255 | (6 << 23);
        words[5] = 0; // height=0+1=1, depth=0+1=1

        let desc = TextureDescriptor::from_words(&words);
        assert_eq!(desc.format, TextureFormat::R32);
        assert_eq!(desc.header_version, TicHeaderVersion::OneDBuffer);
        assert_eq!(desc.texture_type, TextureType::Buffer1D);
        assert_eq!(desc.width, 256);
        assert_eq!(desc.height, 1);
        assert_eq!(desc.depth, 1);
    }

    #[test]
    fn test_sampler_descriptor_basic() {
        let mut words = [0u32; 8];
        // word0: wrap_u=Wrap(0), wrap_v=ClampToEdge(2) at [5:3], wrap_p=Mirror(1) at [8:6]
        words[0] = 0 | (2 << 3) | (1 << 6);
        // word1: mag=Linear(2) at [1:0], min=Linear(2) at [5:4], mipmap=Linear(3) at [7:6]
        //        mip_lod_bias=0 at [24:12]
        words[1] = 2 | (2 << 4) | (3 << 6);
        // word2: min_lod=0, max_lod=3072 (=12.0*256) at [23:12]
        words[2] = 0 | (3072 << 12);
        // border color = [1.0, 0.5, 0.0, 1.0]
        words[4] = f32::to_bits(1.0);
        words[5] = f32::to_bits(0.5);
        words[6] = f32::to_bits(0.0);
        words[7] = f32::to_bits(1.0);

        let desc = SamplerDescriptor::from_words(&words);
        assert_eq!(desc.wrap_u, WrapMode::Wrap);
        assert_eq!(desc.wrap_v, WrapMode::ClampToEdge);
        assert_eq!(desc.wrap_p, WrapMode::Mirror);
        assert!(!desc.depth_compare_enabled);
        assert_eq!(desc.mag_filter, TextureFilter::Linear);
        assert_eq!(desc.min_filter, TextureFilter::Linear);
        assert_eq!(desc.mipmap_filter, MipmapFilter::Linear);
        assert!((desc.min_lod - 0.0).abs() < f32::EPSILON);
        assert!((desc.max_lod - 12.0).abs() < 0.01);
        assert!((desc.mip_lod_bias - 0.0).abs() < f32::EPSILON);
        assert_eq!(desc.border_color[0], 1.0);
        assert_eq!(desc.border_color[1], 0.5);
    }

    #[test]
    fn test_sampler_descriptor_depth_compare() {
        let mut words = [0u32; 8];
        // word0: wrap_u=Border(3), depth_compare_enabled=1 at bit[9],
        //        depth_compare_func=Less(1) at [12:10], max_anisotropy=3 at [22:20]
        words[0] = 3 | (1 << 9) | (1 << 10) | (3 << 20);
        // word1: mag=Nearest(1), min=Nearest(1) at [5:4], mipmap=None(1) at [7:6],
        //        mip_lod_bias: -1.0 → -256 as 13-bit signed → 0x1F00 at [24:12]
        let bias_raw = ((-256i32) as u32) & 0x1FFF; // 13-bit
        words[1] = 1 | (1 << 4) | (1 << 6) | (bias_raw << 12);

        let desc = SamplerDescriptor::from_words(&words);
        assert!(desc.depth_compare_enabled);
        assert_eq!(desc.depth_compare_func, DepthCompareFunc::Less);
        assert_eq!(desc.max_anisotropy, 3);
        assert_eq!(desc.mag_filter, TextureFilter::Nearest);
        assert_eq!(desc.min_filter, TextureFilter::Nearest);
        assert_eq!(desc.mipmap_filter, MipmapFilter::None);
        assert!((desc.mip_lod_bias - (-1.0)).abs() < 0.01);
    }

    #[test]
    fn test_sampler_anisotropy_multiplier() {
        let mut words = [0u32; 8];

        // anisotropy = 0 → 1x
        words[0] = 0;
        assert_eq!(
            SamplerDescriptor::from_words(&words).anisotropy_multiplier(),
            1
        );

        // anisotropy = 1 → 2x
        words[0] = 1 << 20;
        assert_eq!(
            SamplerDescriptor::from_words(&words).anisotropy_multiplier(),
            2
        );

        // anisotropy = 2 → 4x
        words[0] = 2 << 20;
        assert_eq!(
            SamplerDescriptor::from_words(&words).anisotropy_multiplier(),
            4
        );

        // anisotropy = 3 → 8x
        words[0] = 3 << 20;
        assert_eq!(
            SamplerDescriptor::from_words(&words).anisotropy_multiplier(),
            8
        );

        // anisotropy = 4 → 16x
        words[0] = 4 << 20;
        assert_eq!(
            SamplerDescriptor::from_words(&words).anisotropy_multiplier(),
            16
        );
    }

    #[test]
    fn test_tex_header_pool_address() {
        let mut engine = Maxwell3D::new();
        let base = TEX_HEADER_POOL_BASE as usize;
        engine.regs[base] = 0x0002; // addr_high
        engine.regs[base + 1] = 0x4000; // addr_low
        engine.regs[base + 2] = 1024; // limit

        assert_eq!(engine.tex_header_pool_address(), 0x0002_0000_4000);
        assert_eq!(engine.tex_header_pool_limit(), 1024);
    }

    #[test]
    fn test_tex_sampler_pool_address() {
        let mut engine = Maxwell3D::new();
        let base = TEX_SAMPLER_POOL_BASE as usize;
        engine.regs[base] = 0x0003; // addr_high
        engine.regs[base + 1] = 0x8000; // addr_low
        engine.regs[base + 2] = 512; // limit

        assert_eq!(engine.tex_sampler_pool_address(), 0x0003_0000_8000);
        assert_eq!(engine.tex_sampler_pool_limit(), 512);
    }

    #[test]
    fn test_draw_captures_tex_pools() {
        let mut engine = Maxwell3D::new();

        // Set up TIC pool.
        let tic = TEX_HEADER_POOL_BASE as usize;
        engine.regs[tic] = 0x0001;
        engine.regs[tic + 1] = 0x2000;
        engine.regs[tic + 2] = 256;

        // Set up TSC pool.
        let tsc = TEX_SAMPLER_POOL_BASE as usize;
        engine.regs[tsc] = 0x0001;
        engine.regs[tsc + 1] = 0x3000;
        engine.regs[tsc + 2] = 128;

        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        assert_eq!(draws[0].tex_header_pool_addr, 0x0001_0000_2000);
        assert_eq!(draws[0].tex_header_pool_limit, 256);
        assert_eq!(draws[0].tex_sampler_pool_addr, 0x0001_0000_3000);
        assert_eq!(draws[0].tex_sampler_pool_limit, 128);
    }

    // ── Multi-viewport / multi-scissor tests ────────────────────────────

    #[test]
    fn test_viewport_default() {
        let vp = ViewportInfo::default();
        assert_eq!(vp.x, 0.0);
        assert_eq!(vp.y, 0.0);
        assert_eq!(vp.width, 0.0);
        assert_eq!(vp.height, 0.0);
        assert_eq!(vp.depth_near, 0.0);
        assert_eq!(vp.depth_far, 0.0);
    }

    #[test]
    fn test_scissor_default() {
        let sc = ScissorInfo::default();
        assert!(!sc.enabled);
        assert_eq!(sc.min_x, 0);
        assert_eq!(sc.max_x, 0);
        assert_eq!(sc.min_y, 0);
        assert_eq!(sc.max_y, 0);
    }

    #[test]
    fn test_viewport_info_nonzero_index() {
        let mut engine = Maxwell3D::new();

        // Set viewport 3 only.
        let vp3_base = VP_TRANSFORM_BASE + 3 * VP_TRANSFORM_STRIDE;
        engine.write_reg(vp3_base, f32::to_bits(400.0)); // scale_x
        engine.write_reg(vp3_base + 1, f32::to_bits(-300.0)); // scale_y
        engine.write_reg(vp3_base + 2, f32::to_bits(0.5)); // scale_z
        engine.write_reg(vp3_base + 3, f32::to_bits(400.0)); // translate_x
        engine.write_reg(vp3_base + 4, f32::to_bits(300.0)); // translate_y
        engine.write_reg(vp3_base + 5, f32::to_bits(0.5)); // translate_z

        let vp3 = engine.viewport_info(3);
        assert_eq!(vp3.width, 800.0);
        assert_eq!(vp3.height, 600.0);

        // Viewport 0 should still be all zeros.
        let vp0 = engine.viewport_info(0);
        assert_eq!(vp0.width, 0.0);
        assert_eq!(vp0.height, 0.0);
    }

    #[test]
    fn test_scissor_info_nonzero_index() {
        let mut engine = Maxwell3D::new();

        // Set scissor 5 only.
        let sc5_base = SCISSOR_BASE + 5 * SCISSOR_STRIDE;
        engine.write_reg(sc5_base, 1); // enable
        engine.write_reg(sc5_base + 1, 100 | (500 << 16)); // min_x=100, max_x=500
        engine.write_reg(sc5_base + 2, 50 | (400 << 16)); // min_y=50, max_y=400

        let sc5 = engine.scissor_info(5);
        assert!(sc5.enabled);
        assert_eq!(sc5.min_x, 100);
        assert_eq!(sc5.max_x, 500);
        assert_eq!(sc5.min_y, 50);
        assert_eq!(sc5.max_y, 400);
    }

    #[test]
    fn test_draw_captures_all_viewports() {
        let mut engine = Maxwell3D::new();

        // Set viewport 0.
        let vp0_base = VP_TRANSFORM_BASE;
        engine.write_reg(vp0_base, f32::to_bits(640.0));
        engine.write_reg(vp0_base + 1, f32::to_bits(-360.0));
        engine.write_reg(vp0_base + 2, f32::to_bits(0.5));
        engine.write_reg(vp0_base + 3, f32::to_bits(640.0));
        engine.write_reg(vp0_base + 4, f32::to_bits(360.0));
        engine.write_reg(vp0_base + 5, f32::to_bits(0.5));

        // Set viewport 5.
        let vp5_base = VP_TRANSFORM_BASE + 5 * VP_TRANSFORM_STRIDE;
        engine.write_reg(vp5_base, f32::to_bits(200.0));
        engine.write_reg(vp5_base + 1, f32::to_bits(-100.0));
        engine.write_reg(vp5_base + 2, f32::to_bits(1.0));
        engine.write_reg(vp5_base + 3, f32::to_bits(200.0));
        engine.write_reg(vp5_base + 4, f32::to_bits(100.0));
        engine.write_reg(vp5_base + 5, f32::to_bits(1.0));

        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws[0].viewports[0].width, 1280.0);
        assert_eq!(draws[0].viewports[0].height, 720.0);
        assert_eq!(draws[0].viewports[5].width, 400.0);
        assert_eq!(draws[0].viewports[5].height, 200.0);
        // Unset viewport should be zero.
        assert_eq!(draws[0].viewports[10].width, 0.0);
    }

    #[test]
    fn test_draw_captures_all_scissors() {
        let mut engine = Maxwell3D::new();

        // Enable scissor 0.
        let sc0_base = SCISSOR_BASE;
        engine.write_reg(sc0_base, 1);
        engine.write_reg(sc0_base + 1, 0 | (1920 << 16));
        engine.write_reg(sc0_base + 2, 0 | (1080 << 16));

        // Enable scissor 7.
        let sc7_base = SCISSOR_BASE + 7 * SCISSOR_STRIDE;
        engine.write_reg(sc7_base, 1);
        engine.write_reg(sc7_base + 1, 10 | (200 << 16));
        engine.write_reg(sc7_base + 2, 20 | (300 << 16));

        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert!(draws[0].scissors[0].enabled);
        assert_eq!(draws[0].scissors[0].max_x, 1920);
        assert!(draws[0].scissors[7].enabled);
        assert_eq!(draws[0].scissors[7].min_x, 10);
        // Unset scissor should be disabled.
        assert!(!draws[0].scissors[3].enabled);
    }

    #[test]
    fn test_draw_viewport_array_all_indices() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);
        let draws = engine.take_draw_calls();
        // All 16 viewports should be accessible.
        assert_eq!(draws[0].viewports.len(), NUM_VIEWPORTS);
        for vp in &draws[0].viewports {
            assert_eq!(vp.width, 0.0);
        }
    }

    #[test]
    fn test_draw_scissor_array_all_indices() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);
        let draws = engine.take_draw_calls();
        // All 16 scissors should be accessible.
        assert_eq!(draws[0].scissors.len(), NUM_VIEWPORTS);
        for sc in &draws[0].scissors {
            assert!(!sc.enabled);
        }
    }

    // ── Instance / DrawMode tests ────────────────────────────────────────

    #[test]
    fn test_instance_id_from_raw() {
        // bits[27:26] = 0 → First
        assert_eq!(InstanceId::from_raw(0x0000_0000), InstanceId::First);
        // bits[27:26] = 1 → Subsequent
        assert_eq!(InstanceId::from_raw(0x0400_0000), InstanceId::Subsequent);
        // bits[27:26] = 2 → Unchanged
        assert_eq!(InstanceId::from_raw(0x0800_0000), InstanceId::Unchanged);
        // bits[27:26] = 3 → Unchanged (fallback)
        assert_eq!(InstanceId::from_raw(0x0C00_0000), InstanceId::Unchanged);
    }

    #[test]
    fn test_draw_begin_parses_instance_id() {
        let mut engine = Maxwell3D::new();
        // Topology = TriangleStrip(5), instance_id = Subsequent (bits[27:26]=1).
        let value = 5 | (1 << 26);
        engine.handle_draw_begin(value);
        assert_eq!(engine.current_topology, PrimitiveTopology::TriangleStrip);
        assert_eq!(engine.draw_mode, DrawMode::Instance);
    }

    #[test]
    fn test_general_draw_has_instance_count_one() {
        let mut engine = Maxwell3D::new();
        // Plain draw: topology=Triangles(4), instance_id=First (bits[27:26]=0).
        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        assert_eq!(draws[0].instance_count, 1);
    }

    #[test]
    fn test_instanced_draw_accumulates() {
        let mut engine = Maxwell3D::new();
        let subsequent = 4 | (1 << 26); // Triangles + Subsequent

        // 3 × Subsequent BEGIN+END → no DrawCalls yet.
        for _ in 0..3 {
            engine.write_reg(DRAW_BEGIN, subsequent);
            engine.write_reg(DRAW_END, 0);
        }

        let draws = engine.take_draw_calls();
        assert!(draws.is_empty());
        assert_eq!(engine.instance_count, 3);
    }

    #[test]
    fn test_instanced_draw_flushes_on_first() {
        let mut engine = Maxwell3D::new();
        let subsequent = 4 | (1 << 26);

        // 3 Subsequent draws.
        for _ in 0..3 {
            engine.write_reg(DRAW_BEGIN, subsequent);
            engine.write_reg(DRAW_END, 0);
        }
        assert!(engine.take_draw_calls().is_empty());

        // BEGIN(First) flushes the previous batch.
        engine.write_reg(DRAW_BEGIN, 4); // First (bits[27:26]=0)
        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        assert_eq!(draws[0].instance_count, 4);
    }

    #[test]
    fn test_instance_count_resets_after_flush() {
        let mut engine = Maxwell3D::new();
        let subsequent = 4 | (1 << 26);

        // Accumulate 2 instances.
        for _ in 0..2 {
            engine.write_reg(DRAW_BEGIN, subsequent);
            engine.write_reg(DRAW_END, 0);
        }
        assert_eq!(engine.instance_count, 2);

        // Flush via First.
        engine.write_reg(DRAW_BEGIN, 4);
        engine.take_draw_calls(); // discard flush

        // Now a General draw should have instance_count=1.
        engine.write_reg(DRAW_END, 0);
        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        assert_eq!(draws[0].instance_count, 1);
    }

    #[test]
    fn test_draw_captures_base_instance() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(GLOBAL_BASE_INSTANCE_INDEX, 42);
        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws[0].base_instance, 42);
    }

    #[test]
    fn test_draw_captures_base_vertex() {
        let mut engine = Maxwell3D::new();
        // Write a negative base vertex (-10 as u32).
        engine.write_reg(GLOBAL_BASE_VERTEX_INDEX, (-10i32) as u32);
        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws[0].base_vertex, -10);
    }

    #[test]
    fn test_inline_index_accumulates() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(DRAW_BEGIN, 4);

        // Push two inline index values.
        engine.write_reg(DRAW_INLINE_INDEX, 0x0000_0001);
        engine.write_reg(DRAW_INLINE_INDEX, 0x0000_0002);

        assert_eq!(engine.inline_index_data.len(), 8);
        assert_eq!(engine.draw_mode, DrawMode::InlineIndex);
    }

    #[test]
    fn test_inline_index_draw_end() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_INLINE_INDEX, 0x0000_0000);
        engine.write_reg(DRAW_INLINE_INDEX, 0x0000_0001);
        engine.write_reg(DRAW_INLINE_INDEX, 0x0000_0002);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        assert!(draws[0].indexed);
        assert_eq!(draws[0].index_format, IndexFormat::UnsignedInt);
        assert_eq!(draws[0].index_buffer_count, 3);
        assert_eq!(draws[0].inline_index_data.len(), 12);
    }

    #[test]
    fn test_inline_index_clears_after_draw() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_INLINE_INDEX, 0x0000_0001);
        engine.write_reg(DRAW_END, 0);

        // After draw, inline buffer should be empty.
        assert!(engine.inline_index_data.is_empty());
    }

    #[test]
    fn test_inline_index_resets_to_general() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_INLINE_INDEX, 0x0000_0001);
        engine.write_reg(DRAW_END, 0);

        assert_eq!(engine.draw_mode, DrawMode::General);
    }

    // ── Report Semaphore tests ───────────────────────────────────────────

    #[test]
    fn test_report_operation_from_raw() {
        assert_eq!(ReportOperation::from_raw(0), ReportOperation::Release);
        assert_eq!(ReportOperation::from_raw(1), ReportOperation::Acquire);
        assert_eq!(ReportOperation::from_raw(2), ReportOperation::ReportOnly);
        assert_eq!(ReportOperation::from_raw(3), ReportOperation::Trap);
        // Bits above [1:0] are ignored for operation extraction.
        assert_eq!(
            ReportOperation::from_raw(0xFFFF_FF00),
            ReportOperation::Release
        );
    }

    #[test]
    fn test_report_semaphore_address() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(REPORT_SEMAPHORE_BASE, 0x0000_0001); // addr_high
        engine.write_reg(REPORT_SEMAPHORE_BASE + 1, 0xABCD_0000); // addr_low

        assert_eq!(engine.report_semaphore_address(), 0x0001_ABCD_0000);
    }

    #[test]
    fn test_report_semaphore_short_query() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(REPORT_SEMAPHORE_BASE, 0); // addr_high
        engine.write_reg(REPORT_SEMAPHORE_BASE + 1, 0x1000); // addr_low
        engine.write_reg(REPORT_SEMAPHORE_BASE + 2, 0xDEAD_BEEF); // payload

        // Trigger: Release(0) + short_query=1 (bit 28).
        let query = 0 | (1 << 28);
        engine.write_reg(REPORT_SEMAPHORE_TRIGGER, query);

        assert_eq!(engine.pending_semaphore_writes.len(), 1);
        let pw = &engine.pending_semaphore_writes[0];
        assert_eq!(pw.gpu_va, 0x1000);
        assert_eq!(pw.data.len(), 4);
        assert_eq!(pw.data, 0xDEAD_BEEFu32.to_le_bytes());
    }

    #[test]
    fn test_report_semaphore_long_query() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(REPORT_SEMAPHORE_BASE, 0);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 1, 0x2000);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 2, 0x42);

        // Trigger: Release(0) + short_query=0.
        engine.write_reg(REPORT_SEMAPHORE_TRIGGER, 0);

        assert_eq!(engine.pending_semaphore_writes.len(), 1);
        let pw = &engine.pending_semaphore_writes[0];
        assert_eq!(pw.gpu_va, 0x2000);
        assert_eq!(pw.data.len(), 16);
        // First 8 bytes: payload as u64.
        assert_eq!(&pw.data[0..8], &(0x42u64).to_le_bytes());
        // Last 8 bytes: zero timestamp when no GPU tick getter is installed.
        assert_eq!(&pw.data[8..16], &0u64.to_le_bytes());
    }

    #[test]
    fn test_report_semaphore_long_query_uses_gpu_ticks_in_fallback() {
        let mut engine = Maxwell3D::new();
        engine.set_gpu_ticks_getter(Arc::new(|| 0x1122_3344_5566_7788));
        engine.write_reg(REPORT_SEMAPHORE_BASE, 0);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 1, 0x2100);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 2, 0x7B);
        engine.write_reg(REPORT_SEMAPHORE_TRIGGER, 0);

        let pw = &engine.pending_semaphore_writes[0];
        assert_eq!(&pw.data[0..8], &(0x7Bu64).to_le_bytes());
        assert_eq!(&pw.data[8..16], &0x1122_3344_5566_7788u64.to_le_bytes());
    }

    #[test]
    fn test_report_semaphore_payload_value() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(REPORT_SEMAPHORE_BASE, 0);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 1, 0x3000);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 2, 0x1234_5678);

        // Short query Release.
        engine.write_reg(REPORT_SEMAPHORE_TRIGGER, 1 << 28);

        let pw = &engine.pending_semaphore_writes[0];
        let payload = u32::from_le_bytes(pw.data[0..4].try_into().unwrap());
        assert_eq!(payload, 0x1234_5678);
    }

    #[test]
    fn test_report_semaphore_acquire_no_write() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(REPORT_SEMAPHORE_BASE, 0);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 1, 0x4000);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 2, 0xFF);

        // Acquire = operation 1.
        engine.write_reg(REPORT_SEMAPHORE_TRIGGER, 1);

        assert!(engine.pending_semaphore_writes.is_empty());
    }

    #[test]
    fn test_report_semaphore_no_trigger_no_write() {
        let mut engine = Maxwell3D::new();
        // Write addr and payload but NOT the trigger word.
        engine.write_reg(REPORT_SEMAPHORE_BASE, 0);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 1, 0x5000);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 2, 0xFF);

        assert!(engine.pending_semaphore_writes.is_empty());
    }

    #[test]
    fn test_report_semaphore_drains_on_execute() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(REPORT_SEMAPHORE_BASE, 0);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 1, 0x6000);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 2, 0xAA);

        // Two short-query releases.
        engine.write_reg(REPORT_SEMAPHORE_TRIGGER, 1 << 28);
        engine.write_reg(REPORT_SEMAPHORE_TRIGGER, 1 << 28);
        assert_eq!(engine.pending_semaphore_writes.len(), 2);

        let noop_reader = |_addr: u64, _buf: &mut [u8]| {};
        let writes = engine.execute_pending(&noop_reader);
        assert_eq!(writes.len(), 2);

        // Second call should be empty.
        let writes2 = engine.execute_pending(&noop_reader);
        assert!(writes2.is_empty());
    }

    #[test]
    fn test_report_semaphore_query_writes_through_rasterizer_callback() {
        let calls = Arc::new(Mutex::new(RasterizerCalls::default()));
        let mut rasterizer = TestRasterizer::new(Arc::clone(&calls));
        let memory_manager = Arc::new(parking_lot::Mutex::new(
            crate::memory_manager::MemoryManager::default(),
        ));
        memory_manager.lock().map(0x7000, 0x8000, 0x1000, 0, false);

        let writes = Arc::new(Mutex::new(Vec::<(u64, Vec<u8>)>::new()));
        let writes_cb = Arc::clone(&writes);

        let mut engine = Maxwell3D::new();
        engine.bind_rasterizer(&rasterizer);
        engine.set_memory_manager(Arc::clone(&memory_manager));
        engine.set_guest_memory_writer(Arc::new(move |cpu_addr, data| {
            writes_cb.lock().unwrap().push((cpu_addr, data.to_vec()));
        }));

        engine.write_reg(REPORT_SEMAPHORE_BASE, 0);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 1, 0x7000);
        engine.write_reg(REPORT_SEMAPHORE_BASE + 2, 0x1122_3344);
        engine.write_reg(REPORT_SEMAPHORE_TRIGGER, 1 << 28);

        let calls = calls.lock().unwrap();
        assert_eq!(calls.query_writes.len(), 1);
        assert_eq!(calls.query_writes[0].0, 0x7000);
        assert_eq!(
            calls.query_writes[0].1,
            0x1122_3344u32.to_le_bytes().to_vec()
        );
        drop(calls);

        let writes = writes.lock().unwrap();
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].0, 0x8000);
        assert_eq!(writes[0].1, 0x1122_3344u32.to_le_bytes().to_vec());
        assert!(engine.pending_semaphore_writes.is_empty());

        let _ = &mut rasterizer;
    }

    // ── MME macro integration tests ─────────────────────────────────────

    /// Helper: encode an AddImmediate macro opcode.
    /// operation=1, bits[31:14]=imm, bits[13:11]=src_a, bits[10:8]=dst,
    /// bit[7]=exit, bits[6:4]=result_op.
    fn macro_add_imm(result_op: u32, is_exit: bool, dst: u32, src_a: u32, imm: i32) -> u32 {
        1u32 | ((result_op & 0x7) << 4)
            | ((is_exit as u32) << 7)
            | ((dst & 0x7) << 8)
            | ((src_a & 0x7) << 11)
            | (((imm as u32) & 0x3FFFF) << 14)
    }

    #[test]
    fn test_load_mme_upload() {
        let mut engine = Maxwell3D::new();

        // Set upload pointer to offset 5.
        engine.write_reg(LOAD_MME_INSTRUCTION_PTR, 5);
        assert_eq!(engine.regs[LOAD_MME_INSTRUCTION_PTR as usize], 5);

        // Upload two code words. Upstream's ProcessMethodCall path keeps using the
        // current instruction_ptr as the code key for each write.
        engine.write_reg(LOAD_MME_INSTRUCTION, 0xAAAA);
        assert_eq!(engine.regs[LOAD_MME_INSTRUCTION_PTR as usize], 5);

        engine.write_reg(LOAD_MME_INSTRUCTION, 0xBBBB);
        assert_eq!(engine.regs[LOAD_MME_INSTRUCTION_PTR as usize], 5);
    }

    #[test]
    fn test_refresh_parameters_updates_dirty_macro_segments() {
        let gpu = crate::gpu::Gpu::new(false, false);
        gpu.set_guest_memory_reader(std::sync::Arc::new(|addr, output| {
            let backing = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
            let start = (addr - 0x2000) as usize;
            output.copy_from_slice(&backing[start..start + output.len()]);
        }));

        let memory_manager = std::sync::Arc::new(parking_lot::Mutex::new(
            crate::memory_manager::MemoryManager::default(),
        ));
        memory_manager.lock().map(0x1000, 0x2000, 8, 0, false);

        let mut engine = Maxwell3D::new();
        engine.set_memory_manager(std::sync::Arc::clone(&memory_manager));
        let gpu_ptr = &gpu as *const crate::gpu::Gpu as usize;
        engine.set_guest_memory_reader(std::sync::Arc::new(move |addr, output| unsafe {
            let gpu = &*(gpu_ptr as *const crate::gpu::Gpu);
            let _ = gpu.read_guest_memory(addr, output);
        }));
        engine.macro_params = vec![0, 0];
        engine.macro_segments.push((0x1000, 2));
        engine.current_macro_dirty = true;

        engine.refresh_parameters();

        assert_eq!(engine.macro_params, vec![0x4433_2211, 0x8877_6655]);
        assert!(engine.any_parameters_dirty());
    }

    #[test]
    fn test_load_mme_bind() {
        let mut engine = Maxwell3D::new();

        // Set bind pointer to slot 0.
        engine.write_reg(LOAD_MME_START_ADDR_PTR, 0);

        // Bind slot 0 → start offset 10, slot 1 → start offset 20.
        engine.write_reg(LOAD_MME_START_ADDR, 10);
        assert_eq!(engine.regs[LOAD_MME_START_ADDR_PTR as usize], 1);

        engine.write_reg(LOAD_MME_START_ADDR, 20);
        assert_eq!(engine.regs[LOAD_MME_START_ADDR_PTR as usize], 2);
    }

    #[test]
    fn test_initialize_register_defaults_matches_upstream_boot_values() {
        let engine = Maxwell3D::new();

        assert_eq!(engine.regs[DEPTH_TEST_FUNC as usize], 0x207);
        assert_eq!(engine.regs[STENCIL_TWO_SIDE_ENABLE as usize], 1);
        assert_eq!(engine.regs[STENCIL_FRONT_FUNC_MASK as usize], 0xFFFF_FFFF);
        assert_eq!(engine.regs[STENCIL_BACK_MASK as usize], 0xFFFF_FFFF);
        assert_eq!(engine.regs[POINT_SIZE as usize], f32::to_bits(1.0));
        assert_eq!(engine.regs[COLOR_MASK_BASE as usize], 0x1111);
        assert_eq!(engine.regs[VERTEX_ATTRIB_BASE as usize] & (1 << 6), 1 << 6);
        assert_eq!(engine.regs[RASTERIZE_ENABLE as usize], 1);
        assert_eq!(engine.regs[COLOR_TARGET_MRT_ENABLE as usize], 1);
        assert_eq!(engine.regs[FRAMEBUFFER_SRGB as usize], 1);
        assert_eq!(engine.regs[LINE_WIDTH_ALIASED as usize], f32::to_bits(1.0));
        assert_eq!(engine.regs[LINE_WIDTH_SMOOTH as usize], f32::to_bits(1.0));
        assert_eq!(engine.regs[POLYGON_MODE_FRONT as usize], 0x1B02);
        assert_eq!(engine.regs[POLYGON_MODE_BACK as usize], 0x1B02);
        assert_eq!(engine.shadow_state[DEPTH_TEST_FUNC as usize], 0x207);
        assert_eq!(engine.shadow_state[COLOR_MASK_BASE as usize], 0x1111);
    }

    #[test]
    fn test_macro_call_triggers_execution() {
        let mut engine = Maxwell3D::new();

        // Upload a macro that writes r1 (param[0]) to method 0x100.
        // Code: MoveAndSetMethod r2=0x100, then MoveAndSend r3=r1, exit.
        let method_raw = 0x100u32; // addr=0x100, incr=0
        let code = [
            macro_add_imm(2, false, 2, 0, method_raw as i32), // MoveAndSetMethod
            macro_add_imm(4, true, 3, 1, 0),                  // MoveAndSend r1, exit
        ];

        // Upload code at offset 0.
        engine.write_reg(LOAD_MME_INSTRUCTION_PTR, 0);
        for &word in &code {
            engine.write_reg(LOAD_MME_INSTRUCTION, word);
        }

        // Bind slot 0 → offset 0.
        engine.write_reg(LOAD_MME_START_ADDR_PTR, 0);
        engine.write_reg(LOAD_MME_START_ADDR, 0);

        // Invoke macro at slot 0 (method MACRO_METHODS_START) with param 0xDEAD.
        engine.write_reg(MACRO_METHODS_START, 0xDEAD);
        engine.flush_macro();

        // The macro should have written 0xDEAD to method 0x100.
        assert_eq!(engine.regs[0x100], 0xDEAD);
    }

    #[test]
    fn test_macro_writes_registers() {
        let mut engine = Maxwell3D::new();

        // Macro: set method=0x200 (incr=1), send param[0], send param[1].
        let method_raw = 0x200 | (1 << 12); // addr=0x200, incr=1
        let code = [
            macro_add_imm(2, false, 2, 0, method_raw as i32), // MoveAndSetMethod
            macro_add_imm(4, false, 3, 1, 0),                 // MoveAndSend r1
            macro_add_imm(0, true, 4, 0, 0), // IgnoreAndFetch(exit), fetch param[1] into r4
        ];
        // But we need to also send param[1]. Let me simplify:
        // Macro: MoveAndSetMethod, then FetchAndSend (fetch param[1], send r1),
        //        then exit.
        let code = [
            macro_add_imm(2, false, 2, 0, method_raw as i32), // MoveAndSetMethod r2=method
            macro_add_imm(3, false, 3, 1, 0), // FetchAndSend: fetch param[1]→r3, send r1
            macro_add_imm(4, true, 4, 3, 0),  // MoveAndSend r4=r3, send r3, exit
        ];

        engine.write_reg(LOAD_MME_INSTRUCTION_PTR, 0);
        for &word in &code {
            engine.write_reg(LOAD_MME_INSTRUCTION, word);
        }
        engine.write_reg(LOAD_MME_START_ADDR_PTR, 0);
        engine.write_reg(LOAD_MME_START_ADDR, 0);

        // Call with params [0xAA, 0xBB].
        engine.write_reg(MACRO_METHODS_START, 0xAA); // First param (even method = new macro).
        engine.write_reg(MACRO_METHODS_START + 1, 0xBB); // Second param (odd = append).
        engine.flush_macro();

        // First send: r1=0xAA → method 0x200.
        assert_eq!(engine.regs[0x200], 0xAA);
        // Second send: r3=0xBB (fetched param[1]) → method 0x201.
        assert_eq!(engine.regs[0x201], 0xBB);
    }

    #[test]
    fn test_macro_slot_calculation() {
        let mut engine = Maxwell3D::new();

        // Upload a simple "move imm to r2, exit" macro.
        let code = [macro_add_imm(1, true, 2, 0, 42)];
        engine.write_reg(LOAD_MME_INSTRUCTION_PTR, 0);
        for &word in &code {
            engine.write_reg(LOAD_MME_INSTRUCTION, word);
        }

        // Bind slot 5 → offset 0.
        engine.write_reg(LOAD_MME_START_ADDR_PTR, 5);
        engine.write_reg(LOAD_MME_START_ADDR, 0);

        // Method for slot 5 = MACRO_METHODS_START + 5*2 = 0x380A.
        engine.write_reg(MACRO_METHODS_START + 5 * 2, 0);
        engine.flush_macro();

        // Slot = ((0x380A - 0x3800) >> 1) % 128 = (0xA >> 1) % 128 = 5.
        // Macro should have executed slot 5.
        // (We can't directly check which slot ran, but the macro writes r2=42.)
        // Since we can't inspect interpreter registers from here, just verify
        // no panic occurred. The macro has no send, so no register writes.
    }

    // ── Descriptor table integration tests ───────────────────────────────

    #[test]
    fn test_sampler_binding_register() {
        let mut engine = Maxwell3D::new();
        assert_eq!(engine.regs[SAMPLER_BINDING as usize], 0);

        engine.write_reg(SAMPLER_BINDING, 1);
        assert_eq!(engine.regs[SAMPLER_BINDING as usize], 1);

        engine.write_reg(SAMPLER_BINDING, 0);
        assert_eq!(engine.regs[SAMPLER_BINDING as usize], 0);
    }

    #[test]
    fn test_tex_header_pool_address_reconstruction() {
        let mut engine = Maxwell3D::new();
        let base = TEX_HEADER_POOL_BASE as usize;
        engine.regs[base] = 0x0005;
        engine.regs[base + 1] = 0xABCD_0000;
        assert_eq!(engine.tex_header_pool_address(), 0x0005_ABCD_0000);
    }

    #[test]
    fn test_tex_sampler_pool_address_reconstruction() {
        let mut engine = Maxwell3D::new();
        let base = TEX_SAMPLER_POOL_BASE as usize;
        engine.regs[base] = 0x0003;
        engine.regs[base + 1] = 0x1234_0000;
        assert_eq!(engine.tex_sampler_pool_address(), 0x0003_1234_0000);
    }

    #[test]
    fn test_decode_texture_handle_independent() {
        let mut engine = Maxwell3D::new();
        // Default is Independently (0).
        let handle: u32 = (0x0AB << 20) | 0x1_2345;
        let (tic_id, tsc_id) = engine.decode_texture_handle(handle);
        assert_eq!(tic_id, 0x1_2345); // 20-bit
        assert_eq!(tsc_id, 0x0AB); // 12-bit
    }

    #[test]
    fn test_decode_texture_handle_linked() {
        let mut engine = Maxwell3D::new();
        engine.regs[SAMPLER_BINDING as usize] = SamplerBinding::ViaHeaderBinding as u32;

        let handle = 42u32;
        let (tic_id, tsc_id) = engine.decode_texture_handle(handle);
        assert_eq!(tic_id, 42);
        assert_eq!(tsc_id, 42);
    }

    #[test]
    fn test_get_tic_entry() {
        let mut engine = Maxwell3D::new();

        // Set up TIC pool: address = 0x1_0000, limit = 10.
        let base = TEX_HEADER_POOL_BASE as usize;
        engine.regs[base] = 0;
        engine.regs[base + 1] = 0x1_0000;
        engine.regs[base + 2] = 10;
        engine.sync_descriptor_tables();

        // Build a TIC entry for A8B8G8R8 UNorm at index 3.
        // word0: format=0x1D(A8B8G8R8), component types UNorm(2), swizzle RGBA.
        let mut raw = [0u8; 32];
        let word0: u32 = 0x1D
            | (2 << 7)   // r_type = UNorm
            | (2 << 10)  // g_type = UNorm
            | (2 << 13)  // b_type = UNorm
            | (2 << 16)  // a_type = UNorm
            | (2 << 19)  // x_source = R
            | (3 << 22)  // y_source = G
            | (4 << 25)  // z_source = B
            | (5 << 28); // w_source = A
        raw[0..4].copy_from_slice(&word0.to_le_bytes());

        let reader = move |addr: u64, buf: &mut [u8]| {
            let expected_addr = 0x1_0000u64 + 3 * 32;
            if addr == expected_addr {
                buf.copy_from_slice(&raw);
            } else {
                buf.fill(0);
            }
        };

        let (desc, changed) = engine.get_tic_entry(3, &reader);
        assert!(changed);
        assert_eq!(desc.format, TextureFormat::A8B8G8R8);
    }

    #[test]
    fn test_get_tsc_entry() {
        let mut engine = Maxwell3D::new();

        // Set up TSC pool: address = 0x2_0000, limit = 5.
        let base = TEX_SAMPLER_POOL_BASE as usize;
        engine.regs[base] = 0;
        engine.regs[base + 1] = 0x2_0000;
        engine.regs[base + 2] = 5;
        engine.sync_descriptor_tables();

        // Build a TSC entry at index 1.
        // word0: wrap_u=Wrap(0), wrap_v=ClampToEdge(2), wrap_p=Mirror(1).
        // word1: mag=Linear(2), min=Linear(2).
        let mut raw = [0u8; 32];
        let word0: u32 = 0 | (2 << 3) | (1 << 6);
        raw[0..4].copy_from_slice(&word0.to_le_bytes());
        let word1: u32 = 2 | (2 << 4); // mag=Linear(2), min=Linear(2)
        raw[4..8].copy_from_slice(&word1.to_le_bytes());

        let reader = move |addr: u64, buf: &mut [u8]| {
            let expected_addr = 0x2_0000u64 + 1 * 32;
            if addr == expected_addr {
                buf.copy_from_slice(&raw);
            } else {
                buf.fill(0);
            }
        };

        let (desc, changed) = engine.get_tsc_entry(1, &reader);
        assert!(changed);
        assert_eq!(desc.wrap_u, WrapMode::Wrap);
        assert_eq!(desc.wrap_v, WrapMode::ClampToEdge);
        assert_eq!(desc.wrap_p, WrapMode::Mirror);
    }

    #[test]
    fn test_draw_call_captures_sampler_binding() {
        let mut engine = Maxwell3D::new();

        // Default → Independently.
        engine.write_reg(DRAW_BEGIN, 0); // Topology = Points.
        engine.write_reg(DRAW_END, 0);
        let draws = engine.take_draw_calls();
        assert_eq!(draws[0].sampler_binding, SamplerBinding::Independently);

        // Set ViaHeaderBinding.
        engine.write_reg(SAMPLER_BINDING, 1);
        engine.write_reg(DRAW_BEGIN, 0);
        engine.write_reg(DRAW_END, 0);
        let draws = engine.take_draw_calls();
        assert_eq!(draws[0].sampler_binding, SamplerBinding::ViaHeaderBinding);
    }

    // ── EngineInterface (call_method / call_multi_method) tests ──────────

    #[test]
    fn test_call_method_writes_register() {
        let mut engine = Maxwell3D::new();
        engine.call_method(0x100, 0xBEEF, true);
        assert_eq!(engine.regs[0x100], 0xBEEF);
    }

    #[test]
    fn test_call_method_cb_data_increments_offset() {
        let mut engine = Maxwell3D::new();
        // Set up const buffer config: address and offset.
        engine.call_method(CB_CONFIG_BASE, 0x1000, true); // size
        engine.call_method(CB_CONFIG_BASE + 1, 0, true); // addr_high
        engine.call_method(CB_CONFIG_BASE + 2, 0x8000, true); // addr_low
        engine.call_method(CB_CONFIG_BASE + 3, 0, true); // offset = 0

        // Write to CB_DATA — should increment offset by 4 each time.
        engine.call_method(CB_DATA_BASE, 0x1111, true);
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 3) as usize], 4);
        engine.call_method(CB_DATA_BASE, 0x2222, true);
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 3) as usize], 8);
    }

    #[test]
    fn test_call_multi_method_cb_data_batch() {
        let mut engine = Maxwell3D::new();
        // Set up const buffer config.
        engine.call_method(CB_CONFIG_BASE, 0x1000, true);
        engine.call_method(CB_CONFIG_BASE + 1, 0, true);
        engine.call_method(CB_CONFIG_BASE + 2, 0x8000, true);
        engine.call_method(CB_CONFIG_BASE + 3, 0, true);

        // Multi-write 4 words to CB_DATA.
        let data = [0x1111u32, 0x2222, 0x3333, 0x4444];
        engine.call_multi_method(CB_DATA_BASE, &data, 4, 4);
        // Offset should advance by 4*4 = 16 bytes.
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 3) as usize], 16);
    }

    #[test]
    fn test_cb_header_writes_do_not_trigger_cb_data_path() {
        let mut engine = Maxwell3D::new();

        engine.call_method(CB_CONFIG_BASE, 0x200, true);
        engine.call_method(CB_CONFIG_BASE + 1, 0x1234, true);
        engine.call_method(CB_CONFIG_BASE + 2, 0x5678, true);
        engine.call_method(CB_CONFIG_BASE + 3, 0x40, true);

        assert_eq!(engine.regs[CB_CONFIG_BASE as usize], 0x200);
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 1) as usize], 0x1234);
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 2) as usize], 0x5678);
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 3) as usize], 0x40);
    }

    #[test]
    fn test_hle_clear_const_buffer_sets_cb_and_resets_offset() {
        let mut engine = Maxwell3D::new();
        engine.hle_clear_const_buffer(0x5F00, &[0x12, 0x3456, 4]);

        assert_eq!(engine.regs[CB_CONFIG_BASE as usize], 0x5F00);
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 1) as usize], 0x12);
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 2) as usize], 0x3456);
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 3) as usize], 16);
    }

    #[test]
    fn test_process_cb_multi_data_writes_through_memory_manager() {
        let mut engine = Maxwell3D::new();
        let memory_manager = Arc::new(parking_lot::Mutex::new(
            crate::memory_manager::MemoryManager::new_with_geometry(1, 32, 0x1_0000_0000, 16, 12),
        ));
        memory_manager
            .lock()
            .map(0x10000, 0x9000_0000, 0x1000, 0, false);
        let writes = Arc::new(Mutex::new(Vec::<(u64, Vec<u8>)>::new()));
        let writes_clone = Arc::clone(&writes);
        engine.set_memory_manager(Arc::clone(&memory_manager));
        engine.set_guest_memory_writer(Arc::new(move |addr, bytes| {
            writes_clone.lock().unwrap().push((addr, bytes.to_vec()));
        }));

        engine.call_method(CB_CONFIG_BASE, 0x100, true);
        engine.call_method(CB_CONFIG_BASE + 1, 0, true);
        engine.call_method(CB_CONFIG_BASE + 2, 0x10000, true);
        engine.call_method(CB_CONFIG_BASE + 3, 0, true);
        engine.call_multi_method(CB_DATA_BASE, &[0x11223344, 0x55667788], 2, 2);

        let writes = writes.lock().unwrap();
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].0, 0x9000_0000);
        assert_eq!(
            writes[0].1,
            vec![0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55]
        );
        assert_eq!(engine.regs[(CB_CONFIG_BASE + 3) as usize], 8);
    }

    #[test]
    fn test_hle_clear_memory_sets_upload_regs_and_launches_dma() {
        let mut engine = Maxwell3D::new();
        let mut zero_memory = Vec::new();
        engine.hle_clear_memory(&[0x44, 0x5566, 0x20], &mut zero_memory);

        assert_eq!(engine.regs[UPLOAD_REGS_BASE], 0x20);
        assert_eq!(engine.regs[UPLOAD_REGS_BASE + 1], 1);
        assert_eq!(engine.regs[UPLOAD_REGS_BASE + 2], 0x44);
        assert_eq!(engine.regs[UPLOAD_REGS_BASE + 3], 0x5566);
        assert_eq!(engine.regs[LAUNCH_DMA as usize], 0x1011);
        assert_eq!(zero_memory.len(), 8);
    }

    #[test]
    fn test_process_inline_upload_multi_calls_bound_rasterizer() {
        let calls = Arc::new(Mutex::new(RasterizerCalls::default()));
        let rasterizer = TestRasterizer::new(Arc::clone(&calls));
        let memory_manager = Arc::new(parking_lot::Mutex::new(
            crate::memory_manager::MemoryManager::new_with_geometry(1, 32, 0x1_0000_0000, 16, 12),
        ));

        let mut engine = Maxwell3D::new();
        engine.set_memory_manager(Arc::clone(&memory_manager));
        engine.bind_rasterizer(&rasterizer);
        engine.regs[UPLOAD_REGS_BASE as usize] = 8;
        engine.regs[(UPLOAD_REGS_BASE + 1) as usize] = 1;
        engine.regs[(UPLOAD_REGS_BASE + 2) as usize] = 0;
        engine.regs[(UPLOAD_REGS_BASE + 3) as usize] = 0x2000;
        engine.regs[(UPLOAD_REGS_BASE + 4) as usize] = 8;
        engine.regs[LAUNCH_DMA as usize] = 0x1011;
        engine.upload_state.regs = engine.upload_registers();
        engine
            .upload_state
            .process_exec(engine.launch_dma_is_linear());

        engine.process_inline_upload_multi(&[0x1122_3344, 0x5566_7788]);

        assert_eq!(
            calls.lock().unwrap().inline_to_memory,
            vec![(
                0x2000,
                8,
                vec![0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55]
            )]
        );
    }

    #[test]
    fn test_call_multi_method_default_iterates() {
        let mut engine = Maxwell3D::new();
        // Writing to a generic register (not CB_DATA) via call_multi_method
        // should iterate call_method for each value.
        let data = [0xAAAAu32, 0xBBBB, 0xCCCC];
        // Use a non-special register.
        engine.call_multi_method(0x100, &data, 3, 3);
        // Last value written wins since all go to same register.
        assert_eq!(engine.regs[0x100], 0xCCCC);
    }

    #[test]
    fn test_call_method_firmware_call4() {
        let mut engine = Maxwell3D::new();
        assert_eq!(FALCON4, reg_index!(0x2310));
        // Writing to falcon[4] should set shadow_scratch[0] to 1.
        engine.call_method(FALCON4, 0, true);
        assert_eq!(engine.regs[SHADOW_SCRATCH_BASE as usize], 1);
    }

    #[test]
    fn test_call_method_shadow_ram_track() {
        let mut engine = Maxwell3D::new();
        // Enable shadow RAM tracking.
        engine.call_method(SHADOW_RAM_CONTROL, 1, true); // Track
                                                         // Write a value through call_method.
        engine.call_method(0x200, 0xDEAD, true);
        // Shadow state should have the tracked value.
        assert_eq!(engine.shadow_state[0x200], 0xDEAD);
        // Regs should also have the value.
        assert_eq!(engine.regs[0x200], 0xDEAD);
    }

    #[test]
    fn test_call_method_shadow_ram_replay() {
        let mut engine = Maxwell3D::new();
        // First, track a value.
        engine.call_method(SHADOW_RAM_CONTROL, 1, true); // Track
        engine.call_method(0x200, 0xAAAA, true);
        assert_eq!(engine.shadow_state[0x200], 0xAAAA);

        // Switch to Replay mode.
        engine.call_method(SHADOW_RAM_CONTROL, 3, true); // Replay
                                                         // Write a different value — should use shadow state instead.
        engine.call_method(0x200, 0xBBBB, true);
        // Regs should have the shadow value (0xAAAA), not the written value.
        assert_eq!(engine.regs[0x200], 0xAAAA);
    }

    #[test]
    fn test_execution_mask_covers_key_methods() {
        let engine = Maxwell3D::new();
        // Key methods should be marked as executable.
        assert!(engine.interface_state.execution_mask[DRAW_END as usize]);
        assert!(engine.interface_state.execution_mask[DRAW_BEGIN as usize]);
        assert!(engine.interface_state.execution_mask[CLEAR_SURFACE as usize]);
        assert!(engine.interface_state.execution_mask[CB_DATA_BASE as usize]);
        assert!(engine.interface_state.execution_mask[CB_BIND_TRIGGER_0 as usize]);
        assert!(engine.interface_state.execution_mask[SYNC_INFO as usize]);
        assert!(engine.interface_state.execution_mask[LAUNCH_DMA as usize]);
        // Generic register should NOT be executable.
        assert!(!engine.interface_state.execution_mask[0x100]);
    }

    #[test]
    fn test_method_sink_deferred_writes() {
        let mut engine = Maxwell3D::new();
        // Push method sink entries directly (simulating DmaPusher deferral).
        engine.interface_state.method_sink.push((0x200, 0x1111));
        engine.interface_state.method_sink.push((0x201, 0x2222));

        // Consume sink should apply the writes.
        engine.consume_sink();
        assert_eq!(engine.regs[0x200], 0x1111);
        assert_eq!(engine.regs[0x201], 0x2222);
        assert!(engine.interface_state.method_sink.is_empty());
    }

    #[test]
    fn test_call_method_query_condition_always_render() {
        let mut engine = Maxwell3D::new();
        // Set render_enable_override to AlwaysRender (1).
        engine.call_method(RENDER_ENABLE_OVERRIDE, 1, true);
        // Trigger query condition evaluation.
        engine.call_method(RENDER_ENABLE_MODE, 0, true);
        assert!(engine.should_execute());
    }

    #[test]
    fn test_call_method_query_condition_never_render() {
        let mut engine = Maxwell3D::new();
        // Set render_enable_override to NeverRender (2).
        engine.call_method(RENDER_ENABLE_OVERRIDE, 2, true);
        engine.call_method(RENDER_ENABLE_MODE, 0, true);
        assert!(!engine.should_execute());
    }

    #[test]
    fn test_call_method_query_condition_uses_rasterizer_acceleration() {
        let calls = Arc::new(Mutex::new(RasterizerCalls {
            accelerate_conditional_rendering: true,
            ..Default::default()
        }));
        let mut engine = Maxwell3D::new();
        let rasterizer = TestRasterizer::new(Arc::clone(&calls));
        engine.bind_rasterizer(&rasterizer);

        engine.call_method(RENDER_ENABLE_OVERRIDE, 2, true);
        engine.call_method(RENDER_ENABLE_MODE, 0, true);

        assert!(engine.should_execute());
    }

    #[test]
    fn test_call_method_query_condition_if_equal_reads_compare_block() {
        let gpu = crate::gpu::Gpu::new(false, false);
        gpu.set_guest_memory_reader(std::sync::Arc::new(|addr, output| {
            let mut backing = [0u8; 24];
            backing[0..4].copy_from_slice(&5u32.to_le_bytes());
            backing[4..8].copy_from_slice(&7u32.to_le_bytes());
            backing[16..20].copy_from_slice(&5u32.to_le_bytes());
            backing[20..24].copy_from_slice(&7u32.to_le_bytes());
            let start = (addr - 0x3000) as usize;
            output.copy_from_slice(&backing[start..start + output.len()]);
        }));

        let memory_manager = std::sync::Arc::new(parking_lot::Mutex::new(
            crate::memory_manager::MemoryManager::default(),
        ));
        memory_manager.lock().map(0x2000, 0x3000, 24, 0, false);

        let mut engine = Maxwell3D::new();
        engine.set_memory_manager(std::sync::Arc::clone(&memory_manager));
        let gpu_ptr = &gpu as *const crate::gpu::Gpu as usize;
        engine.set_guest_memory_reader(std::sync::Arc::new(move |addr, output| unsafe {
            let gpu = &*(gpu_ptr as *const crate::gpu::Gpu);
            let _ = gpu.read_guest_memory(addr, output);
        }));

        engine.call_method(RENDER_ENABLE_BASE, 0, true);
        engine.call_method(RENDER_ENABLE_BASE + 1, 0x2000, true);
        engine.call_method(RENDER_ENABLE_OVERRIDE, 0, true);
        engine.call_method(RENDER_ENABLE_MODE, 3, true);

        assert!(engine.should_execute());
    }

    #[test]
    fn test_call_method_cb_bind() {
        let mut engine = Maxwell3D::new();
        // Set up const buffer config.
        engine.call_method(CB_CONFIG_BASE, 0x800, true); // size
        engine.call_method(CB_CONFIG_BASE + 1, 0x1, true); // addr_high
        engine.call_method(CB_CONFIG_BASE + 2, 0x2000, true); // addr_low

        // Trigger CB_BIND for stage 0, slot 2, valid.
        // raw_config: valid=1, slot=2 => (2 << 4) | 1 = 0x21
        let bind_base = (CB_BIND_BASE + 0 * CB_BIND_STRIDE) as usize;
        engine.regs[bind_base + 4] = 0x21;
        engine.call_method(CB_BIND_TRIGGER_0, 0x21, true);

        let binding = engine.cb_bindings[0][2];
        assert!(binding.enabled);
        assert_eq!(binding.address, 0x1_0000_2000);
        assert_eq!(binding.size, 0x800);
    }

    #[test]
    fn test_call_method_sync_point() {
        let mut engine = Maxwell3D::new();
        let calls = Arc::new(Mutex::new(RasterizerCalls::default()));
        let rasterizer = TestRasterizer::new(calls.clone());
        engine.bind_rasterizer(&rasterizer);
        engine.call_method(SYNC_INFO, 42, true);
        assert_eq!(engine.regs[SYNC_INFO as usize], 42);
        assert_eq!(calls.lock().unwrap().signal_sync_point, vec![42]);
    }

    #[test]
    fn test_wait_for_idle_calls_rasterizer() {
        let mut engine = Maxwell3D::new();
        let calls = Arc::new(Mutex::new(RasterizerCalls::default()));
        let rasterizer = TestRasterizer::new(calls.clone());
        engine.bind_rasterizer(&rasterizer);
        engine.call_method(WAIT_FOR_IDLE, 0, true);
        assert_eq!(calls.lock().unwrap().wait_for_idle, 1);
    }

    #[test]
    fn test_draw_texture_trigger_calls_rasterizer() {
        let mut engine = Maxwell3D::new();
        let calls = Arc::new(Mutex::new(RasterizerCalls::default()));
        let rasterizer = TestRasterizer::new(calls.clone());
        engine.bind_rasterizer(&rasterizer);

        engine.call_method(DRAW_TEXTURE_SRC_Y0, 0x1234, true);

        assert_eq!(calls.lock().unwrap().draw_texture, 1);
    }

    #[test]
    fn test_draw_end_dispatches_draw_state_to_rasterizer_with_program_addresses() {
        let mut engine = Maxwell3D::new();
        let calls = Arc::new(Mutex::new(RasterizerCalls::default()));
        let rasterizer = TestRasterizer::new(calls.clone());
        engine.bind_rasterizer(&rasterizer);

        // Configure shader_program_region = 0x1_0000_0000 and enable
        // VertexB (slot 1) at offset 0x100 plus Fragment (slot 5) at 0x500.
        engine.write_reg(PROGRAM_REGION_HIGH, 1); // high = 1 → base = 0x1_0000_0000
        engine.write_reg(PROGRAM_REGION_LOW, 0);

        let vb_base = PIPELINE_BASE + 1 * PIPELINE_STRIDE;
        engine.write_reg(vb_base, 1 | (1 << 4));
        engine.write_reg(vb_base + 1, 0x100);

        let frag_base = PIPELINE_BASE + 5 * PIPELINE_STRIDE;
        engine.write_reg(frag_base, 1 | (5 << 4));
        engine.write_reg(frag_base + 1, 0x500);

        // Trigger a non-indexed draw via DRAW_BEGIN/DRAW_END.
        engine.write_reg(DRAW_BEGIN, 4); // Triangles
        engine.write_reg(DRAW_END, 0);

        let calls = calls.lock().unwrap();
        assert_eq!(
            calls.draws.len(),
            1,
            "DRAW_END should trigger exactly one rasterizer.draw call"
        );
        let (instance_count, indexed, addrs) = calls.draws[0];
        assert_eq!(instance_count, 1);
        assert!(!indexed);
        // Slots 0/2/3/4 disabled (zero); 1 = VertexB at base+0x100; 5 = Fragment at base+0x500.
        assert_eq!(addrs[0], 0);
        assert_eq!(addrs[1], 0x1_0000_0100);
        assert_eq!(addrs[2], 0);
        assert_eq!(addrs[3], 0);
        assert_eq!(addrs[4], 0);
        assert_eq!(addrs[5], 0x1_0000_0500);
    }

    #[test]
    fn test_cb_bind_forwards_to_rasterizer() {
        let mut engine = Maxwell3D::new();
        let calls = Arc::new(Mutex::new(RasterizerCalls::default()));
        let rasterizer = TestRasterizer::new(calls.clone());
        engine.bind_rasterizer(&rasterizer);

        engine.call_method(CB_CONFIG_BASE, 0x800, true);
        engine.call_method(CB_CONFIG_BASE + 1, 0x1, true);
        engine.call_method(CB_CONFIG_BASE + 2, 0x2000, true);
        let bind_base = (CB_BIND_BASE + 0 * CB_BIND_STRIDE) as usize;
        engine.regs[bind_base + 4] = 0x21;
        engine.call_method(CB_BIND_TRIGGER_0, 0x21, true);

        assert_eq!(
            calls.lock().unwrap().bound_uniforms,
            vec![(0, 2, 0x1_0000_2000, 0x800)]
        );
    }

    #[test]
    fn test_draw_index_small_matches_upstream_immediate_indexed_draw() {
        let mut engine = Maxwell3D::new();
        engine.regs[(IB_BASE + IB_OFF_FORMAT) as usize] = 2;
        engine.regs[GLOBAL_BASE_VERTEX_INDEX as usize] = 7;
        engine.regs[GLOBAL_BASE_INSTANCE_INDEX as usize] = 3;

        let argument = (4u32 << 28) | (100u32 << 16) | 12u32;
        engine.call_method(INDEX_BUFFER32_FIRST, argument, true);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        let draw = &draws[0];
        assert!(draw.indexed);
        assert_eq!(draw.topology, PrimitiveTopology::Triangles);
        assert_eq!(draw.index_buffer_first, 12);
        assert_eq!(draw.index_buffer_count, 100);
        assert_eq!(draw.base_vertex, 7);
        assert_eq!(draw.base_instance, 3);
        assert_eq!(draw.instance_count, 1);
    }

    #[test]
    fn test_vertex_array_instance_methods_match_upstream_base_instance_progression() {
        let mut engine = Maxwell3D::new();

        let encoded = (4u32 << 28) | (20u32 << 16) | 10u32;
        engine.call_method(VERTEX_ARRAY_INSTANCE_FIRST, encoded, true);
        engine.call_method(VERTEX_ARRAY_INSTANCE_SUBSEQUENT, encoded, true);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 2);
        assert_eq!(draws[0].topology, PrimitiveTopology::Triangles);
        assert_eq!(draws[0].vertex_first, 10);
        assert_eq!(draws[0].vertex_count, 20);
        assert_eq!(draws[0].indexed, false);
        assert_eq!(draws[0].base_instance, 0);
        assert_eq!(draws[0].instance_count, 1);
        assert_eq!(draws[1].base_instance, 1);
        assert_eq!(draws[1].instance_count, 1);
    }

    #[test]
    fn test_call_method_report_semaphore() {
        let mut engine = Maxwell3D::new();
        // Set up report semaphore: address, payload.
        engine.call_method(REPORT_SEMAPHORE_BASE, 0, true); // addr_high
        engine.call_method(REPORT_SEMAPHORE_BASE + 1, 0x5000, true); // addr_low
        engine.call_method(REPORT_SEMAPHORE_BASE + 2, 0xCAFE, true); // payload
                                                                     // Trigger: Release (0), short query (bit 28 set).
        let query_val = (1 << 28) | 0; // short_query=1, operation=Release
        engine.call_method(REPORT_SEMAPHORE_QUERY, query_val, true);

        let writes = engine.execute_pending(&|_, _| {});
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].gpu_va, 0x5000);
        // Short query: 4 bytes (u32 payload).
        assert_eq!(writes[0].data.len(), 4);
        let payload = u32::from_le_bytes(writes[0].data[..4].try_into().unwrap());
        assert_eq!(payload, 0xCAFE);
    }
}
