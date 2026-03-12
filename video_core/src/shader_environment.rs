// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/shader_environment.h and video_core/shader_environment.cpp
//!
//! Shader execution environment for reading shader instructions and metadata
//! from GPU memory during shader compilation.

use std::collections::HashMap;
use std::path::Path;

/// GPU virtual address type.
pub type GPUVAddr = u64;

/// Magic number for pipeline cache files.
pub const MAGIC_NUMBER: [u8; 8] = *b"yuzucach";

/// Instruction size in bytes.
const INST_SIZE: usize = 8;

/// Shader stage enumeration (matching shader_recompiler).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShaderStage {
    VertexA,
    VertexB,
    TessellationControl,
    TessellationEval,
    Geometry,
    Fragment,
    Compute,
}

/// Texture type enumeration (matching shader_recompiler).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TextureType {
    Color1D,
    Color2D,
    Color2DRect,
    Color3D,
    ColorCube,
    ColorArray1D,
    ColorArray2D,
    Buffer,
    ColorArrayCube,
}

/// Texture pixel format (matching shader_recompiler).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TexturePixelFormat(pub u32);

/// Replace constant types for HLE macro state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReplaceConstant {
    BaseVertex,
    BaseInstance,
    DrawId,
}

/// Make a constant buffer key from index and offset.
pub fn make_cbuf_key(index: u32, offset: u32) -> u64 {
    ((index as u64) << 32) | (offset as u64)
}

/// Generic shader environment base.
///
/// Provides instruction reading, constant buffer access, and texture info
/// lookup from GPU memory.
pub struct GenericEnvironment {
    pub program_base: GPUVAddr,
    pub start_address: u32,
    pub stage: ShaderStage,

    pub code: Vec<u64>,
    pub texture_types: HashMap<u32, TextureType>,
    pub texture_pixel_formats: HashMap<u32, TexturePixelFormat>,
    pub cbuf_values: HashMap<u64, u32>,
    pub cbuf_replacements: HashMap<u64, ReplaceConstant>,

    pub local_memory_size: u32,
    pub texture_bound: u32,
    pub shared_memory_size: u32,
    pub workgroup_size: [u32; 3],

    pub read_lowest: u32,
    pub read_highest: u32,
    pub cached_lowest: u32,
    pub cached_highest: u32,
    pub initial_offset: u32,
    pub viewport_transform_state: u32,

    pub has_unbound_instructions: bool,
    pub has_hle_engine_state: bool,
    pub is_proprietary_driver: bool,

    // In the full port: gpu_memory: &MemoryManager,
}

impl GenericEnvironment {
    pub fn new() -> Self {
        Self {
            program_base: 0,
            start_address: 0,
            stage: ShaderStage::VertexB,
            code: Vec::new(),
            texture_types: HashMap::new(),
            texture_pixel_formats: HashMap::new(),
            cbuf_values: HashMap::new(),
            cbuf_replacements: HashMap::new(),
            local_memory_size: 0,
            texture_bound: 0,
            shared_memory_size: 0,
            workgroup_size: [0; 3],
            read_lowest: u32::MAX,
            read_highest: 0,
            cached_lowest: u32::MAX,
            cached_highest: 0,
            initial_offset: 0,
            viewport_transform_state: 1,
            has_unbound_instructions: false,
            has_hle_engine_state: false,
            is_proprietary_driver: false,
        }
    }

    pub fn texture_bound_buffer(&self) -> u32 {
        self.texture_bound
    }

    pub fn local_memory_size(&self) -> u32 {
        self.local_memory_size
    }

    pub fn shared_memory_size(&self) -> u32 {
        self.shared_memory_size
    }

    pub fn workgroup_size(&self) -> [u32; 3] {
        self.workgroup_size
    }

    /// Read an instruction at the given address.
    pub fn read_instruction(&mut self, address: u32) -> u64 {
        self.read_lowest = self.read_lowest.min(address);
        self.read_highest = self.read_highest.max(address);

        if address >= self.cached_lowest && address < self.cached_highest {
            return self.code[((address - self.cached_lowest) / INST_SIZE as u32) as usize];
        }
        self.has_unbound_instructions = true;
        // In full port: gpu_memory.read::<u64>(program_base + address)
        todo!("read_instruction requires GPU memory access");
    }

    /// Try to analyze the shader and return its hash.
    pub fn analyze(&mut self) -> Option<u64> {
        todo!("analyze requires TryFindSize and CityHash");
    }

    /// Set the cached code size.
    pub fn set_cached_size(&mut self, size_bytes: usize) {
        self.cached_lowest = self.start_address;
        self.cached_highest = self.start_address + size_bytes as u32;
        self.code.resize(self.cached_size_words(), 0);
        // In full port: read code block from GPU memory
    }

    pub fn cached_size_words(&self) -> usize {
        self.cached_size_bytes() / INST_SIZE
    }

    pub fn cached_size_bytes(&self) -> usize {
        (self.cached_highest as usize) - (self.cached_lowest as usize) + INST_SIZE
    }

    pub fn read_size_bytes(&self) -> usize {
        (self.read_highest as usize) - (self.read_lowest as usize) + INST_SIZE
    }

    pub fn can_be_serialized(&self) -> bool {
        !self.has_unbound_instructions
    }

    pub fn calculate_hash(&self) -> u64 {
        todo!("calculate_hash requires CityHash and GPU memory");
    }

    pub fn has_hle_macro_state(&self) -> bool {
        self.has_hle_engine_state
    }
}

impl Default for GenericEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

/// Graphics shader environment.
pub struct GraphicsEnvironment {
    pub base: GenericEnvironment,
    pub stage_index: usize,
    // maxwell3d: &Maxwell3D,
}

impl GraphicsEnvironment {
    pub fn new() -> Self {
        Self {
            base: GenericEnvironment::new(),
            stage_index: 0,
        }
    }
}

impl Default for GraphicsEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

/// Compute shader environment.
pub struct ComputeEnvironment {
    pub base: GenericEnvironment,
    // kepler_compute: &KeplerCompute,
}

impl ComputeEnvironment {
    pub fn new() -> Self {
        let mut base = GenericEnvironment::new();
        base.stage = ShaderStage::Compute;
        Self { base }
    }
}

impl Default for ComputeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

/// File-based shader environment for loading from pipeline cache.
pub struct FileEnvironment {
    pub code: Vec<u64>,
    pub texture_types: HashMap<u32, TextureType>,
    pub texture_pixel_formats: HashMap<u32, TexturePixelFormat>,
    pub cbuf_values: HashMap<u64, u32>,
    pub cbuf_replacements: HashMap<u64, ReplaceConstant>,
    pub workgroup_size: [u32; 3],
    pub local_memory_size: u32,
    pub shared_memory_size: u32,
    pub texture_bound: u32,
    pub read_lowest: u32,
    pub read_highest: u32,
    pub initial_offset: u32,
    pub viewport_transform_state: u32,
    pub stage: ShaderStage,
    pub start_address: u32,
    pub is_proprietary_driver: bool,
}

impl FileEnvironment {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            texture_types: HashMap::new(),
            texture_pixel_formats: HashMap::new(),
            cbuf_values: HashMap::new(),
            cbuf_replacements: HashMap::new(),
            workgroup_size: [0; 3],
            local_memory_size: 0,
            shared_memory_size: 0,
            texture_bound: 0,
            read_lowest: 0,
            read_highest: 0,
            initial_offset: 0,
            viewport_transform_state: 1,
            stage: ShaderStage::VertexB,
            start_address: 0,
            is_proprietary_driver: false,
        }
    }

    /// Deserialize from a pipeline cache file.
    pub fn deserialize(&mut self, _file: &mut std::fs::File) {
        todo!("deserialize requires binary file reading");
    }

    pub fn read_instruction(&self, address: u32) -> u64 {
        if address < self.read_lowest || address > self.read_highest {
            panic!("Out of bounds address {}", address);
        }
        self.code[((address - self.read_lowest) / 8) as usize]
    }

    pub fn shader_stage(&self) -> ShaderStage {
        self.stage
    }

    pub fn has_hle_macro_state(&self) -> bool {
        !self.cbuf_replacements.is_empty()
    }
}

impl Default for FileEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

/// Serialize a pipeline to the cache file.
pub fn serialize_pipeline(
    _key: &[u8],
    _envs: &[&GenericEnvironment],
    _filename: &Path,
    _cache_version: u32,
) {
    todo!("serialize_pipeline requires file I/O and serialization");
}

/// Load pipelines from a cache file.
pub fn load_pipelines(
    _filename: &Path,
    _expected_cache_version: u32,
    _load_compute: Box<dyn FnMut(&mut std::fs::File, FileEnvironment)>,
    _load_graphics: Box<dyn FnMut(&mut std::fs::File, Vec<FileEnvironment>)>,
) {
    todo!("load_pipelines requires file I/O and deserialization");
}
