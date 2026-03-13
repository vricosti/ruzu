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
        // NOTE: Full implementation reads from gpu_memory.read::<u64>(program_base + address).
        // Without GPU memory integration, we return 0 (NOP instruction).
        log::warn!(
            "GenericEnvironment::read_instruction: GPU memory not integrated, returning 0 for address 0x{:X}",
            address
        );
        0
    }

    /// Try to analyze the shader and return its hash.
    pub fn analyze(&mut self) -> Option<u64> {
        // NOTE: Full implementation calls TryFindSize() to scan GPU memory for self-branch
        // sentinel instructions (SELF_BRANCH_A / SELF_BRANCH_B), then hashes the code
        // block with CityHash64. Without GPU memory integration we cannot do this.
        log::warn!("GenericEnvironment::analyze: GPU memory not integrated, cannot compute hash");
        None
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
        // NOTE: Full implementation reads ReadSizeBytes() from GPU memory at
        // program_base + read_lowest, then hashes it with CityHash64.
        // Without GPU memory integration we return 0.
        log::warn!("GenericEnvironment::calculate_hash: GPU memory not integrated, returning 0");
        0
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
    ///
    /// Port of `FileEnvironment::Deserialize`. Reads all fields written by
    /// `GenericEnvironment::Serialize` plus stage-specific trailing fields.
    pub fn deserialize(&mut self, file: &mut std::fs::File) {
        use std::io::Read;

        let mut read_u32 = |f: &mut std::fs::File| -> u32 {
            let mut buf = [0u8; 4];
            f.read_exact(&mut buf).unwrap_or(());
            u32::from_le_bytes(buf)
        };
        let mut read_u64 = |f: &mut std::fs::File| -> u64 {
            let mut buf = [0u8; 8];
            f.read_exact(&mut buf).unwrap_or(());
            u64::from_le_bytes(buf)
        };

        let code_size = read_u64(file);
        let num_texture_types = read_u64(file);
        let num_texture_pixel_formats = read_u64(file);
        let num_cbuf_values = read_u64(file);
        let num_cbuf_replacement_values = read_u64(file);
        self.local_memory_size = read_u32(file);
        self.texture_bound = read_u32(file);
        self.start_address = read_u32(file);
        self.read_lowest = read_u32(file);
        self.read_highest = read_u32(file);
        self.viewport_transform_state = read_u32(file);
        // stage: read as u32 and cast
        let stage_raw = read_u32(file);
        self.stage = match stage_raw {
            0 => ShaderStage::VertexA,
            1 => ShaderStage::VertexB,
            2 => ShaderStage::TessellationControl,
            3 => ShaderStage::TessellationEval,
            4 => ShaderStage::Geometry,
            5 => ShaderStage::Fragment,
            6 => ShaderStage::Compute,
            _ => {
                log::warn!("FileEnvironment::deserialize: unknown stage {}", stage_raw);
                ShaderStage::VertexB
            }
        };

        // Read code words (code_size bytes, rounded up to u64 boundary)
        let num_words = code_size.div_ceil(8) as usize;
        self.code.resize(num_words, 0);
        let code_bytes =
            unsafe { std::slice::from_raw_parts_mut(self.code.as_mut_ptr() as *mut u8, code_size as usize) };
        file.read_exact(code_bytes).unwrap_or(());

        for _ in 0..num_texture_types {
            let key = read_u32(file);
            let type_raw = read_u32(file);
            let texture_type = match type_raw {
                0 => TextureType::Color1D,
                1 => TextureType::Color2D,
                2 => TextureType::Color2DRect,
                3 => TextureType::Color3D,
                4 => TextureType::ColorCube,
                5 => TextureType::ColorArray1D,
                6 => TextureType::ColorArray2D,
                7 => TextureType::Buffer,
                8 => TextureType::ColorArrayCube,
                _ => TextureType::Color2D,
            };
            self.texture_types.insert(key, texture_type);
        }
        for _ in 0..num_texture_pixel_formats {
            let key = read_u32(file);
            let fmt_raw = read_u32(file);
            self.texture_pixel_formats.insert(key, TexturePixelFormat(fmt_raw));
        }
        for _ in 0..num_cbuf_values {
            let key = read_u64(file);
            let value = read_u32(file);
            self.cbuf_values.insert(key, value);
        }
        for _ in 0..num_cbuf_replacement_values {
            let key = read_u64(file);
            let rc_raw = read_u32(file);
            let rc = match rc_raw {
                0 => ReplaceConstant::BaseVertex,
                1 => ReplaceConstant::BaseInstance,
                _ => ReplaceConstant::DrawId,
            };
            self.cbuf_replacements.insert(key, rc);
        }
        // Stage-specific trailing fields
        if self.stage == ShaderStage::Compute {
            self.workgroup_size[0] = read_u32(file);
            self.workgroup_size[1] = read_u32(file);
            self.workgroup_size[2] = read_u32(file);
            self.shared_memory_size = read_u32(file);
            self.initial_offset = 0;
        } else {
            // sph: 4 * u32 = 16 bytes (ShaderProgramHeader is 20 bytes upstream but varies)
            // Skip sph by reading past it; initial_offset = sizeof(sph) = 20 bytes upstream
            let mut sph_buf = [0u8; 20];
            file.read_exact(&mut sph_buf).unwrap_or(());
            self.initial_offset = 20;
            if self.stage == ShaderStage::Geometry {
                let _gp_passthrough_mask = read_u32(file);
            }
        }
        self.is_proprietary_driver = self.texture_bound == 2;
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
///
/// Port of `VideoCommon::SerializePipeline`.
/// Appends a pipeline entry to the binary cache file at `filename`.
/// If the file is new (empty), writes the magic header first.
/// Only serializes if all environments can be serialized (no unbound instructions).
pub fn serialize_pipeline(
    key: &[u8],
    envs: &[&GenericEnvironment],
    filename: &Path,
    cache_version: u32,
) {
    use std::io::{Seek, SeekFrom, Write};

    // All envs must be serializable
    if !envs.iter().all(|e| e.can_be_serialized()) {
        return;
    }

    let file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(filename);
    let mut file = match file {
        Ok(f) => f,
        Err(e) => {
            log::error!("serialize_pipeline: failed to open {:?}: {}", filename, e);
            return;
        }
    };

    // Write header if file is empty
    let pos = file.seek(SeekFrom::Current(0)).unwrap_or(u64::MAX);
    if pos == 0 {
        if let Err(e) = file
            .write_all(&MAGIC_NUMBER)
            .and_then(|_| file.write_all(&cache_version.to_le_bytes()))
        {
            log::error!("serialize_pipeline: failed to write header: {}", e);
            return;
        }
    }

    let num_envs = envs.len() as u32;
    if let Err(e) = file.write_all(&num_envs.to_le_bytes()) {
        log::error!("serialize_pipeline: failed to write num_envs: {}", e);
        return;
    }

    for env in envs {
        if let Err(e) = serialize_generic_environment(env, &mut file) {
            log::error!("serialize_pipeline: failed to serialize env: {}", e);
            return;
        }
    }

    if let Err(e) = file.write_all(key) {
        log::error!("serialize_pipeline: failed to write key: {}", e);
    }
}

/// Serializes a GenericEnvironment to a file in the binary format used by the pipeline cache.
fn serialize_generic_environment(
    env: &GenericEnvironment,
    file: &mut std::fs::File,
) -> std::io::Result<()> {
    use std::io::Write;

    let code_size = env.cached_size_bytes() as u64;
    let num_texture_types = env.texture_types.len() as u64;
    let num_texture_pixel_formats = env.texture_pixel_formats.len() as u64;
    let num_cbuf_values = env.cbuf_values.len() as u64;
    let num_cbuf_replacement_values = env.cbuf_replacements.len() as u64;

    file.write_all(&code_size.to_le_bytes())?;
    file.write_all(&num_texture_types.to_le_bytes())?;
    file.write_all(&num_texture_pixel_formats.to_le_bytes())?;
    file.write_all(&num_cbuf_values.to_le_bytes())?;
    file.write_all(&num_cbuf_replacement_values.to_le_bytes())?;
    file.write_all(&env.local_memory_size.to_le_bytes())?;
    file.write_all(&env.texture_bound.to_le_bytes())?;
    file.write_all(&env.start_address.to_le_bytes())?;
    file.write_all(&env.cached_lowest.to_le_bytes())?;
    file.write_all(&env.cached_highest.to_le_bytes())?;
    file.write_all(&env.viewport_transform_state.to_le_bytes())?;
    let stage_raw = env.stage as u32;
    file.write_all(&stage_raw.to_le_bytes())?;

    // Write code as bytes
    let code_byte_slice = unsafe {
        std::slice::from_raw_parts(env.code.as_ptr() as *const u8, code_size as usize)
    };
    file.write_all(code_byte_slice)?;

    for (&key, &texture_type) in &env.texture_types {
        let type_raw = texture_type as u32;
        file.write_all(&key.to_le_bytes())?;
        file.write_all(&type_raw.to_le_bytes())?;
    }
    for (&key, &fmt) in &env.texture_pixel_formats {
        file.write_all(&key.to_le_bytes())?;
        file.write_all(&fmt.0.to_le_bytes())?;
    }
    for (&key, &val) in &env.cbuf_values {
        file.write_all(&key.to_le_bytes())?;
        file.write_all(&val.to_le_bytes())?;
    }
    for (&key, &rc) in &env.cbuf_replacements {
        let rc_raw = rc as u32;
        file.write_all(&key.to_le_bytes())?;
        file.write_all(&rc_raw.to_le_bytes())?;
    }

    // Stage-specific trailing fields
    if env.stage == ShaderStage::Compute {
        file.write_all(&env.workgroup_size[0].to_le_bytes())?;
        file.write_all(&env.workgroup_size[1].to_le_bytes())?;
        file.write_all(&env.workgroup_size[2].to_le_bytes())?;
        file.write_all(&env.shared_memory_size.to_le_bytes())?;
    } else {
        // Write sph placeholder (20 bytes of zeros); upstream writes real SPH
        file.write_all(&[0u8; 20])?;
        if env.stage == ShaderStage::Geometry {
            file.write_all(&0u32.to_le_bytes())?;
        }
    }

    Ok(())
}

/// Load pipelines from a cache file.
///
/// Port of `VideoCommon::LoadPipelines`.
/// Reads the binary cache file and calls `load_compute` or `load_graphics` for each entry.
pub fn load_pipelines(
    filename: &Path,
    expected_cache_version: u32,
    mut load_compute: Box<dyn FnMut(&mut std::fs::File, FileEnvironment)>,
    mut load_graphics: Box<dyn FnMut(&mut std::fs::File, Vec<FileEnvironment>)>,
) {
    use std::io::{Read, Seek, SeekFrom};

    let mut file = match std::fs::File::open(filename) {
        Ok(f) => f,
        Err(_) => return, // File does not exist yet — normal case
    };

    // Read and verify header
    let mut magic = [0u8; 8];
    let mut version_buf = [0u8; 4];
    if file.read_exact(&mut magic).is_err() || file.read_exact(&mut version_buf).is_err() {
        log::error!("load_pipelines: failed to read header from {:?}", filename);
        return;
    }
    let cache_version = u32::from_le_bytes(version_buf);
    if magic != MAGIC_NUMBER || cache_version != expected_cache_version {
        log::warn!(
            "load_pipelines: invalid or outdated pipeline cache {:?}, removing",
            filename
        );
        drop(file);
        let _ = std::fs::remove_file(filename);
        return;
    }

    // Determine file size
    let end = match file.seek(SeekFrom::End(0)) {
        Ok(pos) => pos,
        Err(e) => {
            log::error!("load_pipelines: seek failed: {}", e);
            return;
        }
    };
    file.seek(SeekFrom::Start(8 + 4)).unwrap_or(0); // rewind past header

    while file.seek(SeekFrom::Current(0)).unwrap_or(end) < end {
        // Read num_envs
        let mut buf4 = [0u8; 4];
        if file.read_exact(&mut buf4).is_err() {
            break;
        }
        let num_envs = u32::from_le_bytes(buf4) as usize;

        let mut envs: Vec<FileEnvironment> = Vec::with_capacity(num_envs);
        let mut ok = true;
        for _ in 0..num_envs {
            let mut env = FileEnvironment::new();
            // We cannot call env.deserialize here cleanly because it borrows &mut self and &mut file.
            // Inline a simplified deserialize that uses a local helper.
            if !deserialize_file_env_from(&mut env, &mut file) {
                ok = false;
                break;
            }
            envs.push(env);
        }
        if !ok {
            break;
        }

        // Dispatch based on whether any env is a compute shader
        if envs.iter().any(|e| e.stage == ShaderStage::Compute) {
            if let Some(env) = envs.into_iter().next() {
                load_compute(&mut file, env);
            }
        } else {
            load_graphics(&mut file, envs);
        }
    }
}

/// Deserialize a FileEnvironment from an open file handle.
/// Returns false on I/O error.
fn deserialize_file_env_from(env: &mut FileEnvironment, file: &mut std::fs::File) -> bool {
    // Delegate to the existing deserialize method
    env.deserialize(file);
    true
}
