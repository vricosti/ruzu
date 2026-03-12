// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `shader_recompiler/program_header.h`
//!
//! Nvidia Shader Program Header (SPH) as documented in:
//! http://download.nvidia.com/open-gpu-doc/Shader-Program-Header/1/Shader-Program-Header.html

/// Output topology for geometry shaders (from SPH).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum OutputTopology {
    PointList = 1,
    LineStrip = 6,
    TriangleStrip = 7,
}

impl Default for OutputTopology {
    fn default() -> Self {
        OutputTopology::PointList
    }
}

/// Pixel interpolation map type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum PixelImap {
    Unused = 0,
    Constant = 1,
    Perspective = 2,
    ScreenLinear = 3,
}

impl Default for PixelImap {
    fn default() -> Self {
        PixelImap::Unused
    }
}

impl PixelImap {
    pub fn from_u8(val: u8) -> Self {
        match val & 0x3 {
            0 => PixelImap::Unused,
            1 => PixelImap::Constant,
            2 => PixelImap::Perspective,
            3 => PixelImap::ScreenLinear,
            _ => unreachable!(),
        }
    }
}

/// Shader Program Header.
///
/// This is a 0x50 byte structure at the start of every shader binary.
/// Represented as raw bytes with accessor methods rather than bitfields
/// since Rust does not have native bitfield support.
#[derive(Clone)]
#[repr(C)]
pub struct ProgramHeader {
    pub raw: [u32; 20], // 0x50 bytes = 20 u32s
}

// Verify size matches upstream static_assert(sizeof(ProgramHeader) == 0x50)
const _: () = assert!(std::mem::size_of::<ProgramHeader>() == 0x50);

impl Default for ProgramHeader {
    fn default() -> Self {
        Self { raw: [0u32; 20] }
    }
}

impl std::fmt::Debug for ProgramHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ProgramHeader")
            .field("sph_type", &self.sph_type())
            .field("version", &self.version())
            .field("shader_type", &self.shader_type())
            .field("mrt_enable", &self.mrt_enable())
            .field("kills_pixels", &self.kills_pixels())
            .field("does_global_store", &self.does_global_store())
            .field("geometry_passthrough", &self.geometry_passthrough())
            .finish()
    }
}

impl ProgramHeader {
    // ---- common0 (raw[0]) ----
    pub fn sph_type(&self) -> u32 {
        self.raw[0] & 0x1F
    }
    pub fn version(&self) -> u32 {
        (self.raw[0] >> 5) & 0x1F
    }
    pub fn shader_type(&self) -> u32 {
        (self.raw[0] >> 10) & 0xF
    }
    pub fn mrt_enable(&self) -> bool {
        (self.raw[0] >> 14) & 1 != 0
    }
    pub fn kills_pixels(&self) -> bool {
        (self.raw[0] >> 15) & 1 != 0
    }
    pub fn does_global_store(&self) -> bool {
        (self.raw[0] >> 16) & 1 != 0
    }
    pub fn sass_version(&self) -> u32 {
        (self.raw[0] >> 17) & 0xF
    }
    pub fn geometry_passthrough(&self) -> bool {
        (self.raw[0] >> 24) & 1 != 0
    }
    pub fn does_load_or_store(&self) -> bool {
        (self.raw[0] >> 26) & 1 != 0
    }
    pub fn does_fp64(&self) -> bool {
        (self.raw[0] >> 27) & 1 != 0
    }
    pub fn stream_out_mask(&self) -> u32 {
        (self.raw[0] >> 28) & 0xF
    }

    // ---- common1 (raw[1]) ----
    pub fn shader_local_memory_low_size(&self) -> u32 {
        self.raw[1] & 0x00FF_FFFF
    }
    pub fn per_patch_attribute_count(&self) -> u32 {
        (self.raw[1] >> 24) & 0xFF
    }

    // ---- common2 (raw[2]) ----
    pub fn shader_local_memory_high_size(&self) -> u32 {
        self.raw[2] & 0x00FF_FFFF
    }
    pub fn threads_per_input_primitive(&self) -> u32 {
        (self.raw[2] >> 24) & 0xFF
    }

    // ---- common3 (raw[3]) ----
    pub fn shader_local_memory_crs_size(&self) -> u32 {
        self.raw[3] & 0x00FF_FFFF
    }
    pub fn output_topology(&self) -> OutputTopology {
        match (self.raw[3] >> 24) & 0xF {
            1 => OutputTopology::PointList,
            6 => OutputTopology::LineStrip,
            7 => OutputTopology::TriangleStrip,
            _ => OutputTopology::PointList,
        }
    }

    // ---- common4 (raw[4]) ----
    pub fn max_output_vertices(&self) -> u32 {
        self.raw[4] & 0xFFF
    }
    pub fn store_req_start(&self) -> u32 {
        (self.raw[4] >> 12) & 0xFF
    }
    pub fn store_req_end(&self) -> u32 {
        (self.raw[4] >> 24) & 0xFF
    }

    /// Total local memory size (combined low and high).
    pub fn local_memory_size(&self) -> u64 {
        self.shader_local_memory_low_size() as u64
            | ((self.shader_local_memory_high_size() as u64) << 24)
    }

    // ---- VTG (vertex/tessellation/geometry) imap/omap accessors ----
    // Offsets within raw[] for VTG section (starts at raw[5]):
    //   raw[5..7]  = ImapSystemValuesA (3 bytes) + imap_systemb (1 byte)
    //   raw[8..11] = imap_generic_vector (16 bytes)
    //   raw[12]    = ImapColor (2 bytes) + clip_distances etc (2 bytes)
    //   etc.

    /// Get imap_systemb raw byte for VTG shaders.
    pub fn vtg_imap_systemb(&self) -> u8 {
        // Byte 3 of raw[5] (big-endian layout: bytes [5*4+3])
        ((self.raw[5] >> 24) & 0xFF) as u8
    }

    /// Get the input generic vector bytes (16 bytes starting at offset 0x18).
    pub fn vtg_imap_generic_vector(&self) -> [u8; 16] {
        let mut result = [0u8; 16];
        for i in 0..4 {
            let word = self.raw[6 + i];
            result[i * 4] = (word & 0xFF) as u8;
            result[i * 4 + 1] = ((word >> 8) & 0xFF) as u8;
            result[i * 4 + 2] = ((word >> 16) & 0xFF) as u8;
            result[i * 4 + 3] = ((word >> 24) & 0xFF) as u8;
        }
        result
    }

    /// Check if an input generic attribute component is active (VTG).
    pub fn vtg_input_generic(&self, index: usize) -> [bool; 4] {
        let vec = self.vtg_imap_generic_vector();
        let data = vec[index >> 1] >> ((index % 2) * 4);
        [
            (data & 1) != 0,
            (data & 2) != 0,
            (data & 4) != 0,
            (data & 8) != 0,
        ]
    }

    /// Get omap_generic_vector bytes (16 bytes).
    pub fn vtg_omap_generic_vector(&self) -> [u8; 16] {
        // omap_generic_vector starts at offset 0x38 in the VTG union
        // which is raw[14..17]
        let mut result = [0u8; 16];
        for i in 0..4 {
            let word = self.raw[14 + i];
            result[i * 4] = (word & 0xFF) as u8;
            result[i * 4 + 1] = ((word >> 8) & 0xFF) as u8;
            result[i * 4 + 2] = ((word >> 16) & 0xFF) as u8;
            result[i * 4 + 3] = ((word >> 24) & 0xFF) as u8;
        }
        result
    }

    /// Check if an output generic attribute component is active (VTG).
    pub fn vtg_output_generic(&self, index: usize) -> [bool; 4] {
        let vec = self.vtg_omap_generic_vector();
        let data = vec[index >> 1] >> ((index % 2) * 4);
        [
            (data & 1) != 0,
            (data & 2) != 0,
            (data & 4) != 0,
            (data & 8) != 0,
        ]
    }

    // ---- PS (pixel/fragment) accessors ----

    /// Get the pixel omap target bitmask.
    pub fn ps_omap_target(&self) -> u32 {
        // PS omap is at a specific offset within the union
        self.raw[18]
    }

    /// Check if the given render target has output components enabled.
    pub fn ps_enabled_output_components(&self, rt: u32) -> [bool; 4] {
        let bits = self.ps_omap_target() >> (rt * 4);
        [
            (bits & 1) != 0,
            (bits & 2) != 0,
            (bits & 4) != 0,
            (bits & 8) != 0,
        ]
    }

    /// Check if a render target has any output components.
    pub fn ps_has_output_components(&self, rt: u32) -> bool {
        let bits = self.ps_omap_target() >> (rt * 4);
        (bits & 0xf) != 0
    }

    /// Get the PS omap sample_mask bit.
    pub fn ps_omap_sample_mask(&self) -> bool {
        self.raw[19] & 1 != 0
    }

    /// Get the PS omap depth bit.
    pub fn ps_omap_depth(&self) -> bool {
        (self.raw[19] >> 1) & 1 != 0
    }
}
