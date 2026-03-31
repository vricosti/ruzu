// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/maxwell_to_gl.h
//!
//! Maxwell GPU register values to OpenGL enum translation tables.

/// A GL format tuple (internal_format, format, type).
///
/// Corresponds to `OpenGL::MaxwellToGL::FormatTuple`.
#[derive(Clone, Copy, Debug)]
pub struct FormatTuple {
    pub internal_format: u32,
    pub format: u32,
    pub gl_type: u32,
}

impl FormatTuple {
    pub const fn new(internal_format: u32, format: u32, gl_type: u32) -> Self {
        Self {
            internal_format,
            format,
            gl_type,
        }
    }

    pub const fn compressed(internal_format: u32) -> Self {
        Self {
            internal_format,
            format: gl::NONE,
            gl_type: gl::NONE,
        }
    }
}

// S3TC/DXT compressed format constants (EXT_texture_compression_s3tc).
// These are not always in the base gl crate.
const GL_COMPRESSED_RGBA_S3TC_DXT1_EXT: u32 = 0x83F1;
const GL_COMPRESSED_RGBA_S3TC_DXT3_EXT: u32 = 0x83F2;
const GL_COMPRESSED_RGBA_S3TC_DXT5_EXT: u32 = 0x83F3;

/// Format table mapping PixelFormat enum to GL format tuples.
///
/// Corresponds to `OpenGL::MaxwellToGL::FORMAT_TABLE`.
/// NOTE: Only a representative subset is included here. The full table has MaxPixelFormat entries.
pub static FORMAT_TABLE: &[FormatTuple] = &[
    FormatTuple::new(gl::RGBA8, gl::RGBA, gl::UNSIGNED_INT_8_8_8_8_REV), // A8B8G8R8_UNORM
    FormatTuple::new(gl::RGBA8_SNORM, gl::RGBA, gl::BYTE),               // A8B8G8R8_SNORM
    FormatTuple::new(gl::RGBA8I, gl::RGBA_INTEGER, gl::BYTE),            // A8B8G8R8_SINT
    FormatTuple::new(gl::RGBA8UI, gl::RGBA_INTEGER, gl::UNSIGNED_BYTE),  // A8B8G8R8_UINT
    FormatTuple::new(gl::RGB565, gl::RGB, gl::UNSIGNED_SHORT_5_6_5),     // R5G6B5_UNORM
    FormatTuple::new(gl::RGB565, gl::RGB, gl::UNSIGNED_SHORT_5_6_5_REV), // B5G6R5_UNORM
    FormatTuple::new(gl::RGB5_A1, gl::BGRA, gl::UNSIGNED_SHORT_1_5_5_5_REV), // A1R5G5B5_UNORM
    FormatTuple::new(gl::RGB10_A2, gl::RGBA, gl::UNSIGNED_INT_2_10_10_10_REV), // A2B10G10R10_UNORM
    FormatTuple::new(
        gl::RGB10_A2UI,
        gl::RGBA_INTEGER,
        gl::UNSIGNED_INT_2_10_10_10_REV,
    ), // A2B10G10R10_UINT
    FormatTuple::new(gl::R8, gl::RED, gl::UNSIGNED_BYTE),                // R8_UNORM
    FormatTuple::new(gl::R8_SNORM, gl::RED, gl::BYTE),                   // R8_SNORM
    FormatTuple::new(gl::R8I, gl::RED_INTEGER, gl::BYTE),                // R8_SINT
    FormatTuple::new(gl::R8UI, gl::RED_INTEGER, gl::UNSIGNED_BYTE),      // R8_UINT
    FormatTuple::new(gl::RGBA16F, gl::RGBA, gl::HALF_FLOAT),             // R16G16B16A16_FLOAT
    FormatTuple::new(gl::RGBA16, gl::RGBA, gl::UNSIGNED_SHORT),          // R16G16B16A16_UNORM
    FormatTuple::new(
        gl::R11F_G11F_B10F,
        gl::RGB,
        gl::UNSIGNED_INT_10F_11F_11F_REV,
    ), // B10G11R11_FLOAT
    FormatTuple::new(gl::RGBA32UI, gl::RGBA_INTEGER, gl::UNSIGNED_INT),  // R32G32B32A32_UINT
    FormatTuple::compressed(GL_COMPRESSED_RGBA_S3TC_DXT1_EXT),           // BC1_RGBA_UNORM
    FormatTuple::compressed(GL_COMPRESSED_RGBA_S3TC_DXT3_EXT),           // BC2_UNORM
    FormatTuple::compressed(GL_COMPRESSED_RGBA_S3TC_DXT5_EXT),           // BC3_UNORM
    FormatTuple::compressed(gl::COMPRESSED_RED_RGTC1),                   // BC4_UNORM
    FormatTuple::compressed(gl::COMPRESSED_SIGNED_RED_RGTC1),            // BC4_SNORM
    FormatTuple::compressed(gl::COMPRESSED_RG_RGTC2),                    // BC5_UNORM
    FormatTuple::compressed(gl::COMPRESSED_SIGNED_RG_RGTC2),             // BC5_SNORM
    FormatTuple::compressed(gl::COMPRESSED_RGBA_BPTC_UNORM),             // BC7_UNORM
    FormatTuple::compressed(gl::COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT),     // BC6H_UFLOAT
    FormatTuple::compressed(gl::COMPRESSED_RGB_BPTC_SIGNED_FLOAT),       // BC6H_SFLOAT
    FormatTuple::new(gl::RGBA32F, gl::RGBA, gl::FLOAT),                  // R32G32B32A32_FLOAT
    FormatTuple::new(gl::RGBA32I, gl::RGBA_INTEGER, gl::INT),            // R32G32B32A32_SINT
    FormatTuple::new(gl::RG32F, gl::RG, gl::FLOAT),                      // R32G32_FLOAT
    FormatTuple::new(gl::R32F, gl::RED, gl::FLOAT),                      // R32_FLOAT
    FormatTuple::new(gl::R16F, gl::RED, gl::HALF_FLOAT),                 // R16_FLOAT
    FormatTuple::new(gl::R16, gl::RED, gl::UNSIGNED_SHORT),              // R16_UNORM
    FormatTuple::new(gl::R16UI, gl::RED_INTEGER, gl::UNSIGNED_SHORT),    // R16_UINT
    FormatTuple::new(gl::R16I, gl::RED_INTEGER, gl::SHORT),              // R16_SINT
    FormatTuple::new(gl::RG16F, gl::RG, gl::HALF_FLOAT),                 // R16G16_FLOAT
    FormatTuple::new(gl::RG16UI, gl::RG_INTEGER, gl::UNSIGNED_SHORT),    // R16G16_UINT
    FormatTuple::new(gl::SRGB8_ALPHA8, gl::RGBA, gl::UNSIGNED_INT_8_8_8_8_REV), // A8B8G8R8_SRGB
    FormatTuple::new(gl::RG8, gl::RG, gl::UNSIGNED_BYTE),                // R8G8_UNORM
    FormatTuple::new(gl::RG8UI, gl::RG_INTEGER, gl::UNSIGNED_BYTE),      // R8G8_UINT
    FormatTuple::new(gl::RG32UI, gl::RG_INTEGER, gl::UNSIGNED_INT),      // R32G32_UINT
    FormatTuple::new(gl::R32UI, gl::RED_INTEGER, gl::UNSIGNED_INT),      // R32_UINT
    FormatTuple::new(gl::R32I, gl::RED_INTEGER, gl::INT),                // R32_SINT
    FormatTuple::new(gl::DEPTH_COMPONENT32F, gl::DEPTH_COMPONENT, gl::FLOAT), // D32_FLOAT
    FormatTuple::new(
        gl::DEPTH_COMPONENT16,
        gl::DEPTH_COMPONENT,
        gl::UNSIGNED_SHORT,
    ), // D16_UNORM
    FormatTuple::new(gl::STENCIL_INDEX8, gl::STENCIL, gl::UNSIGNED_BYTE), // S8_UINT
    FormatTuple::new(
        gl::DEPTH24_STENCIL8,
        gl::DEPTH_STENCIL,
        gl::UNSIGNED_INT_24_8,
    ), // D24_UNORM_S8_UINT
    FormatTuple::new(
        gl::DEPTH32F_STENCIL8,
        gl::DEPTH_STENCIL,
        gl::FLOAT_32_UNSIGNED_INT_24_8_REV,
    ), // D32_FLOAT_S8_UINT
];

/// Look up the format tuple for a given pixel format index.
///
/// Corresponds to `OpenGL::MaxwellToGL::GetFormatTuple()`.
pub fn get_format_tuple(pixel_format: usize) -> &'static FormatTuple {
    assert!(pixel_format < FORMAT_TABLE.len());
    &FORMAT_TABLE[pixel_format]
}

/// Map a Maxwell index format to GL index type.
///
/// Corresponds to `OpenGL::MaxwellToGL::IndexFormat()`.
pub fn index_format(format: u32) -> u32 {
    match format {
        0 => gl::UNSIGNED_BYTE,
        1 => gl::UNSIGNED_SHORT,
        2 => gl::UNSIGNED_INT,
        _ => {
            debug_assert!(false, "Invalid index format: {}", format);
            gl::UNSIGNED_INT
        }
    }
}

/// Map a Maxwell primitive topology to GL primitive mode.
///
/// Corresponds to `OpenGL::MaxwellToGL::PrimitiveTopology()`.
pub fn primitive_topology(topology: u32) -> u32 {
    match topology {
        0x0001 => gl::POINTS,
        0x0002 => gl::LINES,
        0x0003 => gl::LINE_LOOP,
        0x0004 => gl::LINE_STRIP,
        0x0005 => gl::TRIANGLES,
        0x0006 => gl::TRIANGLE_STRIP,
        0x0007 => gl::TRIANGLE_FAN,
        0x0008 => gl::QUADS,
        0x000A => gl::LINES_ADJACENCY,
        0x000B => gl::LINE_STRIP_ADJACENCY,
        0x000C => gl::TRIANGLES_ADJACENCY,
        0x000D => gl::TRIANGLE_STRIP_ADJACENCY,
        0x000E => gl::PATCHES,
        _ => {
            log::warn!("Unknown primitive topology: {:#x}", topology);
            gl::POINTS
        }
    }
}

/// Map a Maxwell blend equation to GL blend equation.
///
/// Corresponds to `OpenGL::MaxwellToGL::BlendEquation()`.
pub fn blend_equation(equation: u32) -> u32 {
    match equation {
        1 | 0x8006 => gl::FUNC_ADD,
        2 | 0x800A => gl::FUNC_SUBTRACT,
        3 | 0x800B => gl::FUNC_REVERSE_SUBTRACT,
        4 | 0x8007 => gl::MIN,
        5 | 0x8008 => gl::MAX,
        _ => {
            log::warn!("Unimplemented blend equation: {}", equation);
            gl::FUNC_ADD
        }
    }
}

/// Map a Maxwell comparison op to GL comparison function.
///
/// Corresponds to `OpenGL::MaxwellToGL::ComparisonOp()`.
pub fn comparison_op(comparison: u32) -> u32 {
    match comparison {
        1 | 0x0200 => gl::NEVER,
        2 | 0x0201 => gl::LESS,
        3 | 0x0202 => gl::EQUAL,
        4 | 0x0203 => gl::LEQUAL,
        5 | 0x0204 => gl::GREATER,
        6 | 0x0205 => gl::NOTEQUAL,
        7 | 0x0206 => gl::GEQUAL,
        8 | 0x0207 => gl::ALWAYS,
        _ => {
            log::warn!("Unimplemented comparison op: {}", comparison);
            gl::ALWAYS
        }
    }
}

/// Map a Maxwell front face to GL front face.
///
/// Corresponds to `OpenGL::MaxwellToGL::FrontFace()`.
pub fn front_face(face: u32) -> u32 {
    match face {
        1 => gl::CW,
        2 => gl::CCW,
        _ => {
            log::warn!("Unimplemented front face: {}", face);
            gl::CCW
        }
    }
}

/// Map a Maxwell cull face to GL cull face.
///
/// Corresponds to `OpenGL::MaxwellToGL::CullFace()`.
pub fn cull_face(face: u32) -> u32 {
    match face {
        1 => gl::FRONT,
        2 => gl::BACK,
        3 => gl::FRONT_AND_BACK,
        _ => {
            log::warn!("Unimplemented cull face: {}", face);
            gl::BACK
        }
    }
}

/// Map a Maxwell polygon mode to GL polygon mode.
///
/// Corresponds to `OpenGL::MaxwellToGL::PolygonMode()`.
pub fn polygon_mode(mode: u32) -> u32 {
    match mode {
        0x1B00 => gl::POINT,
        0x1B01 => gl::LINE,
        0x1B02 => gl::FILL,
        _ => {
            log::warn!("Invalid polygon mode: {:#x}", mode);
            gl::FILL
        }
    }
}

/// Map a Maxwell stencil operation to GL stencil operation.
///
/// Corresponds to `OpenGL::MaxwellToGL::StencilOp()`.
pub fn stencil_op(op: u32) -> u32 {
    match op {
        // D3D / GL pairs
        1 | 0x1E00 => gl::KEEP,
        2 | 0x0000 => gl::ZERO,
        3 | 0x1E01 => gl::REPLACE,
        4 | 0x1E02 => gl::INCR,
        5 | 0x1E03 => gl::DECR,
        6 | 0x150A => gl::INVERT,
        7 | 0x8507 => gl::INCR_WRAP,
        8 | 0x8508 => gl::DECR_WRAP,
        _ => {
            log::warn!("Unimplemented stencil op: {:#x}", op);
            gl::KEEP
        }
    }
}

/// Map a Maxwell blend factor to GL blend factor.
///
/// Corresponds to `OpenGL::MaxwellToGL::BlendFunc()`.
pub fn blend_func(factor: u32) -> u32 {
    match factor {
        0x01 | 0x4000 => gl::ZERO,
        0x02 | 0x4001 => gl::ONE,
        0x03 | 0x4300 => gl::SRC_COLOR,
        0x04 | 0x4301 => gl::ONE_MINUS_SRC_COLOR,
        0x05 | 0x4302 => gl::SRC_ALPHA,
        0x06 | 0x4303 => gl::ONE_MINUS_SRC_ALPHA,
        0x07 | 0x4304 => gl::DST_ALPHA,
        0x08 | 0x4305 => gl::ONE_MINUS_DST_ALPHA,
        0x09 | 0x4306 => gl::DST_COLOR,
        0x0A | 0x4307 => gl::ONE_MINUS_DST_COLOR,
        0x0B | 0x4308 => gl::SRC_ALPHA_SATURATE,
        0x0D | 0xC900 => gl::SRC1_COLOR,
        0x0E | 0xC901 => gl::ONE_MINUS_SRC1_COLOR,
        0x0F | 0xC902 => gl::SRC1_ALPHA,
        0x10 | 0xC903 => gl::ONE_MINUS_SRC1_ALPHA,
        0x11 | 0xC001 => gl::CONSTANT_COLOR,
        0x12 | 0xC002 => gl::ONE_MINUS_CONSTANT_COLOR,
        0x13 | 0xC003 => gl::CONSTANT_ALPHA,
        0x14 | 0xC004 => gl::ONE_MINUS_CONSTANT_ALPHA,
        _ => {
            log::warn!("Unimplemented blend factor: {:#x}", factor);
            gl::ZERO
        }
    }
}

/// Map texture filter + mipmap filter to a combined GL filter mode.
///
/// Corresponds to `OpenGL::MaxwellToGL::TextureFilterMode()`.
pub fn texture_filter_mode(filter: u32, mipmap_filter: u32) -> u32 {
    match filter {
        // Nearest
        1 => match mipmap_filter {
            1 => gl::NEAREST, // None
            2 => gl::NEAREST_MIPMAP_NEAREST,
            3 => gl::NEAREST_MIPMAP_LINEAR,
            _ => {
                log::warn!("Invalid mipmap filter mode: {}", mipmap_filter);
                gl::NEAREST
            }
        },
        // Linear
        2 => match mipmap_filter {
            1 => gl::LINEAR, // None
            2 => gl::LINEAR_MIPMAP_NEAREST,
            3 => gl::LINEAR_MIPMAP_LINEAR,
            _ => {
                log::warn!("Invalid mipmap filter mode: {}", mipmap_filter);
                gl::LINEAR
            }
        },
        _ => {
            log::warn!("Invalid texture filter mode: {}", filter);
            gl::NEAREST
        }
    }
}

/// Map a Maxwell wrap mode to GL wrap mode.
///
/// Corresponds to `OpenGL::MaxwellToGL::WrapMode()`.
pub fn wrap_mode(mode: u32) -> u32 {
    match mode {
        0 => gl::REPEAT,
        1 => gl::MIRRORED_REPEAT,
        2 => gl::CLAMP_TO_EDGE,
        3 => gl::CLAMP_TO_BORDER,
        4 => gl::CLAMP_TO_EDGE, // GL_CLAMP (deprecated) — fallback
        5 => gl::MIRROR_CLAMP_TO_EDGE,
        6 => gl::MIRROR_CLAMP_TO_EDGE, // MirrorOnceBorder — fallback
        7 => gl::MIRROR_CLAMP_TO_EDGE, // MirrorOnceClampOgl — fallback
        _ => {
            log::warn!("Unimplemented texture wrap mode: {}", mode);
            gl::REPEAT
        }
    }
}

/// Map a depth compare function to GL compare function.
///
/// Corresponds to `OpenGL::MaxwellToGL::DepthCompareFunc()`.
pub fn depth_compare_func(func: u32) -> u32 {
    match func {
        0 => gl::NEVER,
        1 => gl::LESS,
        2 => gl::EQUAL,
        3 => gl::LEQUAL,
        4 => gl::GREATER,
        5 => gl::NOTEQUAL,
        6 => gl::GEQUAL,
        7 => gl::ALWAYS,
        _ => {
            log::warn!("Unimplemented depth compare func: {}", func);
            gl::GREATER
        }
    }
}

/// Map a Maxwell vertex attribute type + size to GL type.
///
/// Corresponds to `OpenGL::MaxwellToGL::VertexFormat()`.
/// Returns the GL type constant (e.g. GL_FLOAT, GL_UNSIGNED_BYTE).
pub fn vertex_format(attrib_type: u32, size: u32) -> u32 {
    match attrib_type {
        // UNorm, UScaled, UInt
        2 | 5 | 4 => match size {
            0x01 | 0x02 | 0x03 | 0x05 | 0x0A | 0x13 | 0x18 | 0x1D | 0x32 | 0x33 | 0x34 => {
                gl::UNSIGNED_BYTE
            }
            0x0F | 0x1B => gl::UNSIGNED_SHORT,
            0x04 | 0x12 => gl::UNSIGNED_INT,
            0x30 => gl::UNSIGNED_INT_2_10_10_10_REV,
            _ => {
                log::warn!("Unknown unsigned vertex size: {:#x}", size);
                gl::UNSIGNED_BYTE
            }
        },
        // SNorm, SScaled, SInt
        1 | 6 | 3 => match size {
            0x01 | 0x02 | 0x03 | 0x05 | 0x0A | 0x13 | 0x18 | 0x1D | 0x32 | 0x33 | 0x34 => gl::BYTE,
            0x0F | 0x1B => gl::SHORT,
            0x04 | 0x12 => gl::INT,
            0x30 => gl::INT_2_10_10_10_REV,
            _ => {
                log::warn!("Unknown signed vertex size: {:#x}", size);
                gl::BYTE
            }
        },
        // Float
        7 => match size {
            0x0F | 0x03 | 0x1B => gl::HALF_FLOAT,
            0x01 | 0x02 | 0x04 | 0x12 => gl::FLOAT,
            0x31 => gl::UNSIGNED_INT_10F_11F_11F_REV,
            _ => {
                log::warn!("Unknown float vertex size: {:#x}", size);
                gl::FLOAT
            }
        },
        _ => {
            log::warn!("Invalid vertex attribute type: {}", attrib_type);
            gl::FLOAT
        }
    }
}

/// Map a reduction filter mode to GL reduction mode.
///
/// Corresponds to `OpenGL::MaxwellToGL::ReductionFilter()`.
const GL_WEIGHTED_AVERAGE_ARB: u32 = 0x9367;

pub fn reduction_filter(filter: u32) -> u32 {
    match filter {
        0 => GL_WEIGHTED_AVERAGE_ARB,
        1 => gl::MIN,
        2 => gl::MAX,
        _ => {
            log::warn!("Invalid reduction filter: {}", filter);
            GL_WEIGHTED_AVERAGE_ARB
        }
    }
}

/// Map a viewport swizzle to GL viewport swizzle (NV extension).
///
/// Corresponds to `OpenGL::MaxwellToGL::ViewportSwizzle()`.
const GL_VIEWPORT_SWIZZLE_POSITIVE_X_NV: u32 = 0x9350;

pub fn viewport_swizzle(swizzle: u32) -> u32 {
    GL_VIEWPORT_SWIZZLE_POSITIVE_X_NV + swizzle
}

/// Map a logic operation to GL logic op.
///
/// Corresponds to `OpenGL::MaxwellToGL::LogicOp()`.
pub fn logic_op(op: u32) -> u32 {
    match op {
        0x1500 => gl::CLEAR,
        0x1501 => gl::AND,
        0x1502 => gl::AND_REVERSE,
        0x1503 => gl::COPY,
        0x1504 => gl::AND_INVERTED,
        0x1505 => gl::NOOP,
        0x1506 => gl::XOR,
        0x1507 => gl::OR,
        0x1508 => gl::NOR,
        0x1509 => gl::EQUIV,
        0x150A => gl::INVERT,
        0x150B => gl::OR_REVERSE,
        0x150C => gl::COPY_INVERTED,
        0x150D => gl::OR_INVERTED,
        0x150E => gl::NAND,
        0x150F => gl::SET,
        _ => {
            log::warn!("Unimplemented logic op: {:#x}", op);
            gl::COPY
        }
    }
}
