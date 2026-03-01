// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell GPU shader interpreter.
//!
//! Provides loading, decoding, and execution of Maxwell shader bytecode for
//! vertex and fragment shader stages. The instruction stream uses 32-byte
//! bundles: `[sched_ctrl 8B][insn0 8B][insn1 8B][insn2 8B]`. The first 0x50
//! bytes of each shader program is a Shader Program Header (SPH).

pub mod decoder;
pub mod interpreter;

use std::collections::HashMap;

/// Pixel interpolation mode for fragment shader inputs (from SPH).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PixelImap {
    Unused = 0,
    Constant = 1,
    Perspective = 2,
    ScreenLinear = 3,
}

impl PixelImap {
    pub fn from_bits(val: u8) -> Self {
        match val & 3 {
            0 => Self::Unused,
            1 => Self::Constant,
            2 => Self::Perspective,
            3 => Self::ScreenLinear,
            _ => unreachable!(),
        }
    }
}

/// Parsed Shader Program Header (SPH) — 0x50 bytes.
#[derive(Debug, Clone)]
pub struct ShaderProgramHeader {
    /// Shader type from common0 bits [10:13].
    pub shader_type: u32,
    /// Whether the shader kills pixels (bit 15 of common0).
    pub kills_pixels: bool,

    // ── Vertex/Tess/Geom (VTG) fields ────────────────────────────────
    /// omap_systemb byte — bit 4..7 = position x/y/z/w enable.
    pub omap_systemb: u8,
    /// omap_generic_vector[16] — 4 bits per generic output (packed 2 per byte).
    pub omap_generic_vector: [u8; 16],

    // ── Fragment (PS) fields ─────────────────────────────────────────
    /// imap_generic_vector[32] — 2 bits per component (PixelImap), raw bytes.
    pub ps_imap_generic: [u8; 32],
    /// omap.target — 4 bits per render target (RGBA enables).
    pub ps_omap_target: u32,
    /// omap.sample_mask (bit 0) and omap.depth (bit 1).
    pub ps_omap_misc: u32,
}

impl ShaderProgramHeader {
    /// Check if a VTG generic output attribute `index` (0..31) has component `comp` (0..3) enabled.
    pub fn vtg_output_generic(&self, index: usize, comp: usize) -> bool {
        if index >= 32 || comp >= 4 {
            return false;
        }
        let byte = self.omap_generic_vector[index >> 1];
        let nibble = if index & 1 == 0 {
            byte & 0xF
        } else {
            (byte >> 4) & 0xF
        };
        (nibble >> comp) & 1 != 0
    }

    /// Check if VTG position component `comp` (0=x,1=y,2=z,3=w) is output.
    pub fn vtg_output_position(&self, comp: usize) -> bool {
        if comp >= 4 {
            return false;
        }
        (self.omap_systemb >> (4 + comp)) & 1 != 0
    }

    /// Get the PixelImap for fragment shader input generic `attr` component `comp`.
    pub fn ps_input_generic_imap(&self, attr: usize, comp: usize) -> PixelImap {
        if attr >= 32 || comp >= 4 {
            return PixelImap::Unused;
        }
        let byte = self.ps_imap_generic[attr];
        let shift = comp * 2;
        PixelImap::from_bits((byte >> shift) & 3)
    }

    /// Check if PS render target `rt` has output component `comp` enabled.
    pub fn ps_output_enabled(&self, rt: usize, comp: usize) -> bool {
        if rt >= 8 || comp >= 4 {
            return false;
        }
        (self.ps_omap_target >> (rt * 4 + comp)) & 1 != 0
    }

    /// Whether this is a fragment shader (type 5).
    pub fn is_fragment(&self) -> bool {
        self.shader_type == 5
    }
}

/// Size of the Shader Program Header in bytes.
pub const SPH_SIZE: usize = 0x50;

/// Parse the 0x50-byte Shader Program Header from raw bytes.
pub fn parse_sph(data: &[u8; SPH_SIZE]) -> ShaderProgramHeader {
    let common0 = u32::from_le_bytes([data[0], data[1], data[2], data[3]]);
    let shader_type = (common0 >> 10) & 0xF;
    let kills_pixels = (common0 >> 15) & 1 != 0;

    // VTG layout:
    // Offset 0x00: common0..common4 (5 × u32 = 20 bytes)
    // Offset 0x14: ImapSystemValuesA (3 bytes) + imap_systemb (1 byte) = 4 bytes
    // Offset 0x18: imap_generic_vector[16] (16 bytes)
    // Offset 0x28: ImapColor (2 bytes)
    // Offset 0x2A: clip_distances etc (2 bytes)
    // Offset 0x2C: ImapFixedFncTexture (5 bytes) + ImapReserved (1 byte) = 6 bytes
    // Offset 0x32: padding to 0x33
    // Offset 0x33: OmapSystemValuesA (3 bytes) + omap_systemb (1 byte)
    // Actually the union starts at offset 0x14 for vtg fields.
    // common0..common4 occupy offsets 0x00..0x13 (5 × 4 = 20 bytes)
    // VTG union starts at offset 0x14:
    //   +0x00 (0x14): ImapSystemValuesA [3 bytes]
    //   +0x03 (0x17): imap_systemb [1 byte]
    //   +0x04 (0x18): imap_generic_vector [16 bytes]
    //   +0x14 (0x28): ImapColor [2 bytes]
    //   +0x16 (0x2A): clip_distances/etc [2 bytes]
    //   +0x18 (0x2C): ImapFixedFncTexture [5 bytes]
    //   +0x1D (0x31): ImapReserved [1 byte]
    //   +0x1E (0x32): OmapSystemValuesA [3 bytes]
    //   +0x21 (0x35): omap_systemb [1 byte]
    //   +0x22 (0x36): omap_generic_vector [16 bytes]

    let omap_systemb = data[0x35];
    let mut omap_generic_vector = [0u8; 16];
    omap_generic_vector.copy_from_slice(&data[0x36..0x46]);

    // PS layout (same union, different interpretation):
    // Union starts at 0x14:
    //   +0x00 (0x14): ImapSystemValuesA [3 bytes]
    //   +0x03 (0x17): imap_systemb [1 byte]
    //   +0x04 (0x18): imap_generic_vector[32] [32 bytes]
    //   +0x24 (0x38): ImapColor [2 bytes]
    //   +0x26 (0x3A): ImapSystemValuesC [2 bytes]
    //   +0x28 (0x3C): ImapFixedFncTexture [10 bytes]
    //   +0x32 (0x46): ImapReserved [2 bytes]
    //   +0x34 (0x48): omap.target [4 bytes]
    //   +0x38 (0x4C): omap.misc [4 bytes]
    let mut ps_imap_generic = [0u8; 32];
    ps_imap_generic.copy_from_slice(&data[0x18..0x38]);
    let ps_omap_target = u32::from_le_bytes([data[0x48], data[0x49], data[0x4A], data[0x4B]]);
    let ps_omap_misc = u32::from_le_bytes([data[0x4C], data[0x4D], data[0x4E], data[0x4F]]);

    ShaderProgramHeader {
        shader_type,
        kills_pixels,
        omap_systemb,
        omap_generic_vector,
        ps_imap_generic,
        ps_omap_target,
        ps_omap_misc,
    }
}

/// A loaded shader program ready for interpretation.
#[derive(Debug, Clone)]
pub struct ShaderProgram {
    /// Decoded SPH.
    pub header: ShaderProgramHeader,
    /// Instruction words (scheduling control words stripped out).
    pub instructions: Vec<u64>,
}

/// Result of executing a vertex shader for one vertex.
#[derive(Debug, Clone)]
pub struct VertexShaderOutput {
    /// gl_Position equivalent (clip-space).
    pub position: [f32; 4],
    /// Generic output attributes indexed by attribute slot.
    pub generics: HashMap<u32, [f32; 4]>,
}

/// Result of executing a fragment shader for one pixel.
#[derive(Debug, Clone)]
pub struct FragmentShaderOutput {
    /// Output color for render target 0.
    pub color: [f32; 4],
    /// Optional output depth override.
    pub depth: Option<f32>,
    /// Whether the fragment was killed (discard).
    pub killed: bool,
}

/// Load a shader program from GPU memory.
///
/// Reads the SPH header at `shader_addr`, then instruction words starting at
/// `shader_addr + 0x50`. Instruction words are read in 32-byte bundles — every
/// 4th u64 (the first in each bundle) is a scheduling control word and is
/// skipped. Stops when an EXIT instruction is found or `max_insns` instructions
/// have been read.
pub fn load_shader_program(
    shader_addr: u64,
    read_gpu: &dyn Fn(u64, &mut [u8]),
    max_insns: usize,
) -> ShaderProgram {
    // Read SPH header.
    let mut sph_bytes = [0u8; SPH_SIZE];
    read_gpu(shader_addr, &mut sph_bytes);
    let header = parse_sph(&sph_bytes);

    // Read instruction stream starting after the SPH.
    let code_base = shader_addr + SPH_SIZE as u64;
    let mut instructions = Vec::with_capacity(256);
    let mut bundle_idx = 0u64; // Index in u64 units from code_base

    loop {
        if instructions.len() >= max_insns {
            break;
        }

        // In a 32-byte bundle (4 × u64), index 0 is scheduling control.
        let pos_in_bundle = bundle_idx % 4;
        if pos_in_bundle == 0 {
            // Skip the scheduling control word.
            bundle_idx += 1;
            continue;
        }

        // Read the instruction word.
        let byte_offset = bundle_idx * 8;
        let mut buf = [0u8; 8];
        read_gpu(code_base + byte_offset, &mut buf);
        let word = u64::from_le_bytes(buf);

        instructions.push(word);

        // Check for EXIT instruction: top bits match "1110 0011 0000 ----"
        // = bits [63:48] masked. EXIT pattern: 0xE300_xxxx_xxxx_xxxx
        let top16 = (word >> 48) as u16;
        if top16 & 0xFFF0 == 0xE300 {
            break;
        }

        bundle_idx += 1;
    }

    ShaderProgram {
        header,
        instructions,
    }
}

/// Load raw Maxwell instruction words from GPU memory for the shader recompiler.
///
/// This is similar to `load_shader_program` but returns just the instruction
/// words (scheduling control words stripped), suitable for passing directly to
/// `shader_recompiler::compile_shader()`.
///
/// Returns `(instructions, header)` where instructions are the u64 words.
pub fn load_shader_code(
    shader_addr: u64,
    read_gpu: &dyn Fn(u64, &mut [u8]),
    max_insns: usize,
) -> (Vec<u64>, ShaderProgramHeader) {
    let prog = load_shader_program(shader_addr, read_gpu, max_insns);
    (prog.instructions, prog.header)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_sph_basic() {
        let mut data = [0u8; SPH_SIZE];
        // Set shader_type = 1 (VertexB) in common0 bits [10:13]
        let common0: u32 = 1 << 10;
        data[0..4].copy_from_slice(&common0.to_le_bytes());

        // Set omap_systemb: position xyzw all enabled (bits 4-7)
        data[0x35] = 0xF0;

        let sph = parse_sph(&data);
        assert_eq!(sph.shader_type, 1);
        assert!(!sph.kills_pixels);
        assert!(sph.vtg_output_position(0)); // x
        assert!(sph.vtg_output_position(1)); // y
        assert!(sph.vtg_output_position(2)); // z
        assert!(sph.vtg_output_position(3)); // w
    }

    #[test]
    fn test_parse_sph_fragment() {
        let mut data = [0u8; SPH_SIZE];
        // shader_type = 5 (Fragment) + kills_pixels
        let common0: u32 = (5 << 10) | (1 << 15);
        data[0..4].copy_from_slice(&common0.to_le_bytes());

        // Set PS omap.target: RT0 has RGBA (0xF)
        let target: u32 = 0xF;
        data[0x48..0x4C].copy_from_slice(&target.to_le_bytes());

        // Set PS imap_generic[0]: perspective for all 4 components (0x2 each = 0xAA)
        data[0x18] = 0xAA; // x=2, y=2, z=2, w=2

        let sph = parse_sph(&data);
        assert!(sph.is_fragment());
        assert!(sph.kills_pixels);
        assert!(sph.ps_output_enabled(0, 0)); // RT0.R
        assert!(sph.ps_output_enabled(0, 1)); // RT0.G
        assert!(sph.ps_output_enabled(0, 2)); // RT0.B
        assert!(sph.ps_output_enabled(0, 3)); // RT0.A
        assert!(!sph.ps_output_enabled(1, 0)); // RT1 disabled
        assert_eq!(
            sph.ps_input_generic_imap(0, 0),
            PixelImap::Perspective
        );
    }

    #[test]
    fn test_vtg_output_generic() {
        let mut data = [0u8; SPH_SIZE];
        // Generic 0: all 4 components enabled = 0xF in low nibble of byte 0
        data[0x36] = 0x0F;
        // Generic 1: x and z enabled = 0x5 in high nibble of byte 0
        data[0x36] |= 0x50;

        let sph = parse_sph(&data);
        assert!(sph.vtg_output_generic(0, 0));
        assert!(sph.vtg_output_generic(0, 1));
        assert!(sph.vtg_output_generic(0, 2));
        assert!(sph.vtg_output_generic(0, 3));
        assert!(sph.vtg_output_generic(1, 0));
        assert!(!sph.vtg_output_generic(1, 1));
        assert!(sph.vtg_output_generic(1, 2));
        assert!(!sph.vtg_output_generic(1, 3));
    }

    #[test]
    fn test_load_shader_skips_sched_ctrl() {
        // Build a minimal shader in memory: SPH + 1 bundle with EXIT.
        // Bundle layout: [sched_ctrl(8B)][insn0(8B)][insn1(8B)][insn2(8B)]
        let mut gpu_mem = vec![0u8; SPH_SIZE + 32];

        // SPH: shader_type=1
        let common0: u32 = 1 << 10;
        gpu_mem[0..4].copy_from_slice(&common0.to_le_bytes());

        // Sched ctrl at offset 0x50
        let sched_ctrl: u64 = 0xDEADBEEF_12345678;
        gpu_mem[SPH_SIZE..SPH_SIZE + 8].copy_from_slice(&sched_ctrl.to_le_bytes());

        // Instruction 0: NOP (0x50B0_xxxx_xxxx_xxxx)
        let nop: u64 = 0x50B0_0000_0000_0000;
        gpu_mem[SPH_SIZE + 8..SPH_SIZE + 16].copy_from_slice(&nop.to_le_bytes());

        // Instruction 1: EXIT (0xE300_xxxx_xxxx_xxxx)
        let exit: u64 = 0xE300_0000_0000_0000;
        gpu_mem[SPH_SIZE + 16..SPH_SIZE + 24].copy_from_slice(&exit.to_le_bytes());

        let read_gpu = |addr: u64, buf: &mut [u8]| {
            let start = addr as usize;
            let end = start + buf.len();
            if end <= gpu_mem.len() {
                buf.copy_from_slice(&gpu_mem[start..end]);
            }
        };

        let program = load_shader_program(0, &read_gpu, 4096);
        // Should have 2 instructions: NOP + EXIT (sched ctrl skipped)
        assert_eq!(program.instructions.len(), 2);
        assert_eq!(program.instructions[0], nop);
        assert_eq!(program.instructions[1], exit);
    }
}
