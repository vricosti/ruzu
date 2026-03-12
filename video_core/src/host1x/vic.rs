// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/vic.h` and `vic.cpp`.
//!
//! Video Image Composer (VIC) — reads decoded frames from the frame queue,
//! performs color conversion / compositing, and writes output surfaces.
//!
//! The VIC implementation is heavily FFmpeg/SIMD-dependent in upstream; method
//! bodies that require FFmpeg frame data or SIMD (SSE4.1) are stubbed.

use std::sync::Arc;

use log::info;

use crate::host1x::host1x::FrameQueue;

// --------------------------------------------------------------------------
// Pixel type
// --------------------------------------------------------------------------

/// RGBX pixel with 16-bit components.
///
/// Port of `Tegra::Host1x::Pixel`.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Pixel {
    pub r: u16,
    pub g: u16,
    pub b: u16,
    pub a: u16,
}

// --------------------------------------------------------------------------
// VideoPixelFormat
// --------------------------------------------------------------------------

/// Video pixel formats used by VIC surfaces.
///
/// Port of `Tegra::Host1x::VideoPixelFormat`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VideoPixelFormat {
    A8 = 0,
    L8 = 1,
    A4L4 = 2,
    L4A4 = 3,
    R8 = 4,
    A8L8 = 5,
    L8A8 = 6,
    R8G8 = 7,
    G8R8 = 8,
    B5G6R5 = 9,
    R5G6B5 = 10,
    B6G5R5 = 11,
    R5G5B6 = 12,
    A1B5G5R5 = 13,
    A1R5G5B5 = 14,
    B5G5R5A1 = 15,
    R5G5B5A1 = 16,
    A5B5G5R1 = 17,
    A5R1G5B5 = 18,
    B5G5R1A5 = 19,
    R1G5B5A5 = 20,
    X1B5G5R5 = 21,
    X1R5G5B5 = 22,
    B5G5R5X1 = 23,
    R5G5B5X1 = 24,
    A4B4G5R4 = 25,
    A4R4G4B4 = 26,
    B4G4R4A4 = 27,
    R4G4B4A4 = 28,
    B8G8R8 = 29,
    R8G8B8 = 30,
    A8B8G8R8 = 31,
    A8R8G8B8 = 32,
    B8G8R8A8 = 33,
    R8G8B8A8 = 34,
    X8B8G8R8 = 35,
    X8R8G8B8 = 36,
    B8G8R8X8 = 37,
    R8G8B8X8 = 38,
    A8B10G10R10 = 39,
    A2R10G10B10 = 40,
    B10G10R10A2 = 41,
    R10G10B10A2 = 42,
    A4P4 = 43,
    P4A4 = 44,
    P8A8 = 45,
    A8P8 = 46,
    P8 = 47,
    P1 = 48,
    U8V8 = 49,
    V8U8 = 50,
    A8Y8U8V8 = 51,
    V8U8Y8A8 = 52,
    Y8U8V8 = 53,
    Y8V8U8 = 54,
    U8V8Y8 = 55,
    V8U8Y8 = 56,
    Y8U8Y8V8 = 57,
    Y8V8Y8U8 = 58,
    U8Y8V8Y8 = 59,
    V8Y8U8Y8 = 60,
    Y8UvN444 = 61,
    Y8VuN444 = 62,
    Y8UvN422 = 63,
    Y8VuN422 = 64,
    Y8UvN422R = 65,
    Y8VuN422R = 66,
    Y8UvN420 = 67,
    Y8VuN420 = 68,
    Y8U8V8N444 = 69,
    Y8U8V8N422 = 70,
    Y8U8V8N422R = 71,
    Y8U8V8N420 = 72,
    U8 = 73,
    V8 = 74,
}

// --------------------------------------------------------------------------
// Offset (32-bit VIC variant)
// --------------------------------------------------------------------------

/// 32-bit offset used in VIC register structures.
///
/// Port of `Tegra::Host1x::Offset` in vic.h.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct VicOffset {
    offset: u32,
}

impl VicOffset {
    pub fn address(&self) -> u32 {
        self.offset << 8
    }
}

const _: () = assert!(std::mem::size_of::<VicOffset>() == 0x4);

// --------------------------------------------------------------------------
// PlaneOffsets
// --------------------------------------------------------------------------

/// Luma/chroma plane offsets for a surface.
///
/// Port of `Tegra::Host1x::PlaneOffsets`.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct PlaneOffsets {
    pub luma: VicOffset,
    pub chroma_u: VicOffset,
    pub chroma_v: VicOffset,
}

const _: () = assert!(std::mem::size_of::<PlaneOffsets>() == 0xC);

// --------------------------------------------------------------------------
// SurfaceIndex
// --------------------------------------------------------------------------

/// Surface indices for VIC slot surfaces.
///
/// Port of `Tegra::Host1x::SurfaceIndex`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SurfaceIndex {
    Current = 0,
    Previous = 1,
    Next = 2,
    NextNoiseReduced = 3,
    CurrentMotion = 4,
    PreviousMotion = 5,
    PreviousPreviousMotion = 6,
    CombinedMotion = 7,
}

// --------------------------------------------------------------------------
// DXVAHD enums
// --------------------------------------------------------------------------

/// Port of `Tegra::Host1x::DXVAHD_ALPHA_FILL_MODE`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DxvahdAlphaFillMode {
    Opaque = 0,
    Background = 1,
    Destination = 2,
    SourceStream = 3,
    Composited = 4,
    SourceAlpha = 5,
}

/// Port of `Tegra::Host1x::DXVAHD_FRAME_FORMAT`.
#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DxvahdFrameFormat {
    Progressive = 0,
    InterlacedTopFieldFirst = 1,
    InterlacedBottomFieldFirst = 2,
    TopField = 3,
    BottomField = 4,
    SubpicProgressive = 5,
    SubpicInterlacedTopFieldFirst = 6,
    SubpicInterlacedBottomFieldFirst = 7,
    SubpicTopField = 8,
    SubpicBottomField = 9,
    TopFieldChromaBottom = 10,
    BottomFieldChromaTop = 11,
    SubpicTopFieldChromaBottom = 12,
    SubpicBottomFieldChromaTop = 13,
}

/// Port of `Tegra::Host1x::DXVAHD_DEINTERLACE_MODE_PRIVATE`.
#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DxvahdDeinterlaceModePrivate {
    Weave = 0,
    BobField = 1,
    Bob = 2,
    Newbob = 3,
    Disi1 = 4,
    WeaveLumaBobFieldChroma = 5,
    Max = 0xF,
}

/// Port of `Tegra::Host1x::BLK_KIND`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlkKind {
    Pitch = 0,
    Generic16Bx2 = 1,
    BlNaive = 2,
    BlKeplerXbarRaw = 3,
    Vp2Tiled = 15,
}

// --------------------------------------------------------------------------
// Blend factor enums
// --------------------------------------------------------------------------

/// Port of `Tegra::Host1x::BLEND_SRCFACTC`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlendSrcFactC {
    K1 = 0,
    K1TimesDst = 1,
    NegK1TimesDst = 2,
    K1TimesSrc = 3,
    Zero = 4,
}

/// Port of `Tegra::Host1x::BLEND_DSTFACTC`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlendDstFactC {
    K1 = 0,
    K2 = 1,
    K1TimesDst = 2,
    NegK1TimesDst = 3,
    NegK1TimesSrc = 4,
    Zero = 5,
    One = 6,
}

/// Port of `Tegra::Host1x::BLEND_SRCFACTA`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlendSrcFactA {
    K1 = 0,
    K2 = 1,
    NegK1TimesDst = 2,
    Zero = 3,
    Max = 7,
}

/// Port of `Tegra::Host1x::BLEND_DSTFACTA`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlendDstFactA {
    K2 = 0,
    NegK1TimesSrc = 1,
    Zero = 2,
    One = 3,
    Max = 7,
}

// --------------------------------------------------------------------------
// Config structs (bitfield structs represented as raw u32/u64 with accessors)
// --------------------------------------------------------------------------

/// Port of `Tegra::Host1x::PipeConfig` — 0x10 bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct PipeConfig {
    pub downsample_raw: u32,
    pub reserved2: u32,
    pub reserved3: u32,
    pub reserved4: u32,
}

const _: () = assert!(std::mem::size_of::<PipeConfig>() == 0x10);

/// Port of `Tegra::Host1x::OutputConfig` — 0x10 bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct OutputConfig {
    pub flags0: u64,
    pub target_rect_lr: u32,
    pub target_rect_tb: u32,
}

const _: () = assert!(std::mem::size_of::<OutputConfig>() == 0x10);

/// Port of `Tegra::Host1x::OutputSurfaceConfig` — 0x10 bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct OutputSurfaceConfig {
    pub format_flags: u32,
    pub surface_dims: u32,
    pub luma_dims: u32,
    pub chroma_dims: u32,
}

const _: () = assert!(std::mem::size_of::<OutputSurfaceConfig>() == 0x10);

/// Port of `Tegra::Host1x::MatrixStruct` — 0x20 bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct MatrixStruct {
    pub row0: u64,
    pub row1: u64,
    pub row2: u64,
    pub row3: u64,
}

const _: () = assert!(std::mem::size_of::<MatrixStruct>() == 0x20);

/// Port of `Tegra::Host1x::ClearRectStruct` — 0x10 bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct ClearRectStruct {
    pub rect0: u32,
    pub rect0_tb: u32,
    pub rect1: u32,
    pub rect1_tb: u32,
}

const _: () = assert!(std::mem::size_of::<ClearRectStruct>() == 0x10);

/// Port of `Tegra::Host1x::SlotConfig` — 0x40 bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SlotConfig {
    pub data: [u64; 7],
    pub reserved22: u32,
    pub reserved23: u32,
}

const _: () = assert!(std::mem::size_of::<SlotConfig>() == 0x40);

/// Port of `Tegra::Host1x::SlotSurfaceConfig` — 0x10 bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SlotSurfaceConfig {
    pub format_flags: u32,
    pub surface_dims: u32,
    pub luma_dims: u32,
    pub chroma_dims: u32,
}

const _: () = assert!(std::mem::size_of::<SlotSurfaceConfig>() == 0x10);

/// Port of `Tegra::Host1x::LumaKeyStruct` — 0x10 bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct LumaKeyStruct {
    pub data0: u64,
    pub data1: u64,
}

const _: () = assert!(std::mem::size_of::<LumaKeyStruct>() == 0x10);

/// Port of `Tegra::Host1x::BlendingSlotStruct` — 0x10 bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct BlendingSlotStruct {
    pub alpha_k: u32,
    pub factors: u32,
    pub overrides: u32,
    pub override_a_masks: u32,
}

const _: () = assert!(std::mem::size_of::<BlendingSlotStruct>() == 0x10);

/// Port of `Tegra::Host1x::SlotStruct` — 0xB0 bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct SlotStruct {
    pub config: SlotConfig,
    pub surface_config: SlotSurfaceConfig,
    pub luma_key: LumaKeyStruct,
    pub color_matrix: MatrixStruct,
    pub gamut_matrix: MatrixStruct,
    pub blending: BlendingSlotStruct,
}

const _: () = assert!(std::mem::size_of::<SlotStruct>() == 0xB0);

/// Port of `Tegra::Host1x::ConfigStruct` — 0x610 bytes.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct ConfigStruct {
    pub pipe_config: PipeConfig,
    pub output_config: OutputConfig,
    pub output_surface_config: OutputSurfaceConfig,
    pub out_color_matrix: MatrixStruct,
    pub clear_rects: [ClearRectStruct; 4],
    pub slot_structs: [SlotStruct; 8],
}

const _: () = assert!(std::mem::size_of::<ConfigStruct>() == 0x610);

impl Default for ConfigStruct {
    fn default() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

// --------------------------------------------------------------------------
// VicRegisters — 0x1118 bytes
// --------------------------------------------------------------------------

/// VIC register file.
///
/// Port of `Tegra::Host1x::VicRegisters`.
pub const VIC_NUM_REGS: usize = 0x446;

#[repr(C)]
#[derive(Clone)]
pub struct VicRegisters {
    pub reg_array: [u32; VIC_NUM_REGS],
}

const _: () = assert!(std::mem::size_of::<VicRegisters>() == 0x1118);

impl Default for VicRegisters {
    fn default() -> Self {
        Self {
            reg_array: [0u32; VIC_NUM_REGS],
        }
    }
}

// VIC register offsets (byte offsets matching upstream offsetof values).
pub const VIC_REG_EXECUTE: usize = 0x300 / 4;
pub const VIC_REG_SURFACES: usize = 0x400 / 4;
pub const VIC_REG_PICTURE_INDEX: usize = 0x700 / 4;
pub const VIC_REG_CONTROL_PARAMS: usize = 0x704 / 4;
pub const VIC_REG_CONFIG_STRUCT_OFFSET: usize = 0x708 / 4;
pub const VIC_REG_OUTPUT_SURFACE_LUMA: usize = 0x720 / 4;
pub const VIC_REG_OUTPUT_SURFACE_CHROMA_U: usize = 0x724 / 4;
pub const VIC_REG_OUTPUT_SURFACE_CHROMA_V: usize = 0x728 / 4;

/// VIC method enum for process_method dispatch.
///
/// Port of `Tegra::Host1x::Vic::Method`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VicMethod {
    Execute = 0x300,
    SetControlParams = 0x704,
    SetConfigStructOffset = 0x708,
    SetOutputSurfaceLumaOffset = 0x720,
    SetOutputSurfaceChromaOffset = 0x724,
    SetOutputSurfaceChromaUnusedOffset = 0x728,
}

impl VicMethod {
    pub fn from_u32(value: u32) -> Option<Self> {
        match value {
            0x300 => Some(Self::Execute),
            0x704 => Some(Self::SetControlParams),
            0x708 => Some(Self::SetConfigStructOffset),
            0x720 => Some(Self::SetOutputSurfaceLumaOffset),
            0x724 => Some(Self::SetOutputSurfaceChromaOffset),
            0x728 => Some(Self::SetOutputSurfaceChromaUnusedOffset),
            _ => None,
        }
    }
}

// --------------------------------------------------------------------------
// Vic
// --------------------------------------------------------------------------

/// Video Image Composer device.
///
/// Port of `Tegra::Host1x::Vic`.
pub struct Vic {
    id: i32,
    nvdec_id: i32,
    syncpoint: u32,
    regs: VicRegisters,
    frame_queue: Arc<FrameQueue>,
    output_surface: Vec<Pixel>,
    slot_surface: Vec<Pixel>,
    luma_scratch: Vec<u8>,
    chroma_scratch: Vec<u8>,
    swizzle_scratch: Vec<u8>,
}

impl Vic {
    pub fn new(id: i32, syncpt: u32, frame_queue: Arc<FrameQueue>) -> Self {
        info!("Created VIC {}", id);
        Self {
            id,
            nvdec_id: -1,
            syncpoint: syncpt,
            regs: VicRegisters::default(),
            frame_queue,
            output_surface: Vec::new(),
            slot_surface: Vec::new(),
            luma_scratch: Vec::new(),
            chroma_scratch: Vec::new(),
            swizzle_scratch: Vec::new(),
        }
    }

    /// Write to the device state.
    ///
    /// Port of `Vic::ProcessMethod`.
    pub fn process_method(&mut self, method: u32, arg: u32) {
        let word_offset = (method / 4) as usize;
        if word_offset < VIC_NUM_REGS {
            self.regs.reg_array[word_offset] = arg;
        }

        if method == VicMethod::Execute as u32 {
            self.execute();
        }
    }

    /// Execute the VIC pipeline.
    ///
    /// Port of `Vic::Execute`.
    fn execute(&mut self) {
        // TODO: Read ConfigStruct from memory at config_struct_offset.
        // TODO: For each enabled slot, read the decoded frame from frame_queue,
        //       perform color conversion and blending, write output surface.
        // Stubbed — requires memory manager and FFmpeg frame integration.
        todo!("Vic::execute — requires memory manager and frame integration")
    }
}

impl Drop for Vic {
    fn drop(&mut self) {
        info!("Destroying VIC {}", self.id);
    }
}
