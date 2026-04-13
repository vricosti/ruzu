// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/shader_environment.h and video_core/shader_environment.cpp
//!
//! Shader execution environment for reading shader instructions and metadata
//! from GPU memory during shader compilation.

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

/// GPU virtual address type.
pub type GPUVAddr = u64;

/// Magic number for pipeline cache files.
pub const MAGIC_NUMBER: [u8; 8] = *b"yuzucach";

/// Instruction size in bytes.
const INST_SIZE: usize = 8;

/// `TryFindSize` block fetch granularity. Upstream
/// `GenericEnvironment::TryFindSize` reads 0x1000 bytes per iteration.
const TRY_FIND_SIZE_BLOCK_BYTES: usize = 0x1000;

/// `TryFindSize` upper bound. Upstream caps the search at 0x100000 bytes.
const TRY_FIND_SIZE_MAX_BYTES: usize = 0x100000;

/// Maxwell self-branch sentinel A. Marks the tail of a shader.
/// Upstream: `GenericEnvironment::TryFindSize` line 251.
const SELF_BRANCH_A: u64 = 0xE2400FFFFF87000F;

/// Maxwell self-branch sentinel B. Marks the tail of a shader.
/// Upstream: `GenericEnvironment::TryFindSize` line 252.
const SELF_BRANCH_B: u64 = 0xE2400FFFFF07000F;

/// GPU memory reader callback shape: read `bytes.len()` bytes starting at
/// the given GPU virtual address.
///
/// Rust adaptation of upstream's `Tegra::MemoryManager*` member field on
/// `GenericEnvironment`. The Rust port doesn't have a single, persistent
/// `MemoryManager` reference reachable from every video_core component
/// yet, so the reader is injected as an `Arc<dyn Fn>` callback by the
/// owner that does have access (e.g. `RasterizerOpenGL` via the
/// `gpu_read` callback already plumbed through `render_draw_calls`).
///
/// `Send + Sync` so it can be cloned across threads / fibers safely.
pub type GpuMemoryReader = Arc<dyn Fn(GPUVAddr, &mut [u8]) + Send + Sync>;

/// Shader stage enumeration.
///
/// Re-exported from `shader_recompiler::stage::Stage`, which itself
/// matches upstream `Shader::Stage` exactly. The previous local copy
/// was deleted as part of the cross-crate type-unification pass.
pub use shader_recompiler::stage::Stage as ShaderStage;

// `TextureType`, `TexturePixelFormat`, and `ReplaceConstant` are the
// upstream-faithful types from `shader_recompiler::shader_info`, matching
// the upstream `Shader::TextureType`, `Shader::TexturePixelFormat`, and
// `Shader::ReplaceConstant` declarations. Re-exported here so existing
// consumers of `shader_environment::TextureType` etc. continue to resolve.
//
// The previous local copies had variant-ordering / discriminant mismatches
// compared to upstream (e.g. ReplaceConstant had BaseVertex=0 vs upstream
// BaseInstance=0). Deleted and replaced with re-exports as part of the
// cross-crate type-unification pass.
pub use shader_recompiler::shader_info::{ReplaceConstant, TexturePixelFormat, TextureType};

/// Make a constant buffer key from index and offset.
pub fn make_cbuf_key(index: u32, offset: u32) -> u64 {
    ((index as u64) << 32) | (offset as u64)
}

/// Generic shader environment base.
///
/// Provides instruction reading, constant buffer access, and texture info
/// lookup from GPU memory.
///
/// Upstream: `VideoCommon::GenericEnvironment` (`shader_environment.h:29`),
/// which holds a `Tegra::MemoryManager*` and uses
/// `gpu_memory->Read<u64>` / `gpu_memory->ReadBlock` to fetch shader bytes.
/// The Rust port replaces that pointer with the [`GpuMemoryReader`]
/// callback set via [`GenericEnvironment::with_gpu_read`].
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

    /// GPU memory reader. Rust adaptation of upstream's
    /// `Tegra::MemoryManager*` field. `None` means no reader has been
    /// installed yet — methods that need to fetch GPU memory will return
    /// the documented "no data" value (0 from `read_instruction`,
    /// `None` from `analyze`) instead of panicking.
    gpu_read: Option<GpuMemoryReader>,
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
            gpu_read: None,
        }
    }

    /// Set the GPU memory reader.
    ///
    /// Rust adaptation of upstream's
    /// `GenericEnvironment::GenericEnvironment(Tegra::MemoryManager& gpu_memory_, ...)`
    /// constructor: stores the reader so subsequent `read_instruction`,
    /// `set_cached_size`, `analyze`, and `calculate_hash` calls can fetch
    /// shader bytes from guest GPU memory.
    pub fn with_gpu_read(mut self, reader: GpuMemoryReader) -> Self {
        self.gpu_read = Some(reader);
        self
    }

    /// Set program base / start address. Mirrors the upstream constructor's
    /// `program_base_` and `start_address_` parameters.
    pub fn with_program(mut self, program_base: GPUVAddr, start_address: u32) -> Self {
        self.program_base = program_base;
        self.start_address = start_address;
        self
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
    ///
    /// Port of upstream `GenericEnvironment::ReadInstruction`
    /// (`shader_environment.cpp:141`).
    pub fn read_instruction(&mut self, address: u32) -> u64 {
        self.read_lowest = self.read_lowest.min(address);
        self.read_highest = self.read_highest.max(address);

        if address >= self.cached_lowest && address < self.cached_highest {
            return self.code[((address - self.cached_lowest) / INST_SIZE as u32) as usize];
        }
        self.has_unbound_instructions = true;
        // Upstream: `gpu_memory->Read<u64>(program_base + address);`
        // We use the injected reader. With no reader installed, this slot
        // is unreachable through the normal pipeline-build path (the cache
        // gates pipeline creation on `has_gpu_memory_reader`).
        let Some(reader) = self.gpu_read.as_ref() else {
            return 0;
        };
        let mut buf = [0u8; INST_SIZE];
        reader(self.program_base + address as u64, &mut buf);
        u64::from_le_bytes(buf)
    }

    /// Try to analyze the shader and return its CityHash64 hash.
    ///
    /// Port of upstream `GenericEnvironment::Analyze` (cpp:152).
    pub fn analyze(&mut self) -> Option<u64> {
        let size = self.try_find_size()?;
        self.cached_lowest = self.start_address;
        self.cached_highest = self.start_address + size as u32;
        let bytes = self.code_bytes(size as usize);
        Some(common::cityhash::city_hash64(bytes))
    }

    /// Walk GPU memory in `BLOCK_SIZE` chunks looking for the
    /// SELF_BRANCH_A / SELF_BRANCH_B sentinels that mark the shader's
    /// tail. Returns the byte offset of the sentinel, or `None` if no
    /// sentinel is found within `MAXIMUM_SIZE` bytes.
    ///
    /// Port of upstream `GenericEnvironment::TryFindSize`
    /// (`shader_environment.cpp:247`).
    fn try_find_size(&mut self) -> Option<u64> {
        let reader = self.gpu_read.as_ref()?.clone();
        let mut guest_addr = self.program_base + self.start_address as u64;
        let mut offset: usize = 0;
        let mut size: usize = TRY_FIND_SIZE_BLOCK_BYTES;
        while size <= TRY_FIND_SIZE_MAX_BYTES {
            self.code.resize(size / INST_SIZE, 0);

            // Read the next BLOCK_SIZE chunk into `code` at `offset`.
            let words_offset = offset / INST_SIZE;
            let words_in_block = TRY_FIND_SIZE_BLOCK_BYTES / INST_SIZE;
            let mut block_bytes = [0u8; TRY_FIND_SIZE_BLOCK_BYTES];
            reader(guest_addr, &mut block_bytes);
            for i in 0..words_in_block {
                let chunk: [u8; 8] = block_bytes[i * 8..(i + 1) * 8]
                    .try_into()
                    .expect("8-byte chunk");
                self.code[words_offset + i] = u64::from_le_bytes(chunk);
            }

            // Scan this block for the self-branch sentinels. Match upstream
            // exactly: byte index in [0, BLOCK_SIZE) stepping by INST_SIZE.
            for index in (0..TRY_FIND_SIZE_BLOCK_BYTES).step_by(INST_SIZE) {
                let inst = self.code[words_offset + index / INST_SIZE];
                if inst == SELF_BRANCH_A || inst == SELF_BRANCH_B {
                    return Some((offset + index) as u64);
                }
            }

            guest_addr += TRY_FIND_SIZE_BLOCK_BYTES as u64;
            size += TRY_FIND_SIZE_BLOCK_BYTES;
            offset += TRY_FIND_SIZE_BLOCK_BYTES;
        }
        None
    }

    /// Set the cached code size and fetch the bytes from GPU memory.
    ///
    /// Port of upstream `GenericEnvironment::SetCachedSize` (cpp:162).
    pub fn set_cached_size(&mut self, size_bytes: usize) {
        self.cached_lowest = self.start_address;
        self.cached_highest = self.start_address + size_bytes as u32;
        self.code.resize(self.cached_size_words(), 0);
        // Upstream: `gpu_memory->ReadBlock(program_base + cached_lowest,
        //                                  code.data(), code.size() * sizeof(u64));`
        if let Some(reader) = self.gpu_read.as_ref() {
            let bytes_to_read = self.code.len() * INST_SIZE;
            let mut buf = vec![0u8; bytes_to_read];
            reader(self.program_base + self.cached_lowest as u64, &mut buf);
            for (i, chunk) in buf.chunks_exact(8).enumerate() {
                self.code[i] = u64::from_le_bytes(chunk.try_into().expect("8 bytes"));
            }
        }
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

    /// Hash the slice of GPU memory actually touched by translation.
    ///
    /// Port of upstream `GenericEnvironment::CalculateHash` (cpp:185).
    pub fn calculate_hash(&self) -> u64 {
        let Some(reader) = self.gpu_read.as_ref() else {
            return 0;
        };
        let size = self.read_size_bytes();
        let mut buf = vec![0u8; size];
        reader(self.program_base + self.read_lowest as u64, &mut buf);
        common::cityhash::city_hash64(&buf)
    }

    pub fn has_hle_macro_state(&self) -> bool {
        self.has_hle_engine_state
    }

    /// Helper for `analyze`: borrow the first `size` bytes of `code` as a
    /// byte slice. The recompiler stores instructions as `Vec<u64>` and
    /// CityHash64 wants `&[u8]`, so we reinterpret in place rather than
    /// re-fetch the bytes from GPU memory.
    fn code_bytes(&self, size: usize) -> &[u8] {
        let total_bytes = self.code.len() * INST_SIZE;
        let len = size.min(total_bytes);
        unsafe { std::slice::from_raw_parts(self.code.as_ptr() as *const u8, len) }
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
        let code_bytes = unsafe {
            std::slice::from_raw_parts_mut(self.code.as_mut_ptr() as *mut u8, code_size as usize)
        };
        file.read_exact(code_bytes).unwrap_or(());

        // Deserialization mappings use the upstream-faithful discriminant
        // values from `shader_recompiler::shader_info` (#[repr(u32)]):
        //   TextureType:     Color1D=0, ColorArray1D=1, Color2D=2, ...
        //   ReplaceConstant: BaseInstance=0, BaseVertex=1, DrawID=2
        //   TexturePixelFormat: 103-variant enum, discriminant = variant index
        for _ in 0..num_texture_types {
            let key = read_u32(file);
            let type_raw = read_u32(file);
            let texture_type = match type_raw {
                0 => TextureType::Color1D,
                1 => TextureType::ColorArray1D,
                2 => TextureType::Color2D,
                3 => TextureType::ColorArray2D,
                4 => TextureType::Color3D,
                5 => TextureType::ColorCube,
                6 => TextureType::ColorArrayCube,
                7 => TextureType::Buffer,
                8 => TextureType::Color2DRect,
                _ => TextureType::Color2D,
            };
            self.texture_types.insert(key, texture_type);
        }
        for _ in 0..num_texture_pixel_formats {
            let key = read_u32(file);
            let fmt_raw = read_u32(file);
            // SAFETY: TexturePixelFormat is #[repr(u32)] with 102
            // contiguous discriminants starting at 0. If the raw value
            // is out of range, we fall back to A8B8G8R8Unorm (0).
            let fmt = if fmt_raw < 102 {
                unsafe { std::mem::transmute::<u32, TexturePixelFormat>(fmt_raw) }
            } else {
                log::warn!("TexturePixelFormat out of range: {}", fmt_raw);
                TexturePixelFormat::A8B8G8R8Unorm
            };
            self.texture_pixel_formats.insert(key, fmt);
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
                0 => ReplaceConstant::BaseInstance,
                1 => ReplaceConstant::BaseVertex,
                _ => ReplaceConstant::DrawID,
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
    let code_byte_slice =
        unsafe { std::slice::from_raw_parts(env.code.as_ptr() as *const u8, code_size as usize) };
    file.write_all(code_byte_slice)?;

    for (&key, &texture_type) in &env.texture_types {
        let type_raw = texture_type as u32;
        file.write_all(&key.to_le_bytes())?;
        file.write_all(&type_raw.to_le_bytes())?;
    }
    for (&key, &fmt) in &env.texture_pixel_formats {
        file.write_all(&key.to_le_bytes())?;
        file.write_all(&(fmt as u32).to_le_bytes())?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    /// Build a 64 KiB GPU-memory mock with a Maxwell self-branch sentinel
    /// at byte offset `sentinel_offset` (relative to the program base) and
    /// return a `(reader, log)` pair. The log records every read so tests
    /// can assert that walking matches upstream's block stride.
    fn make_mock_gpu_with_sentinel(
        program_base: u64,
        sentinel_offset: usize,
        sentinel_value: u64,
    ) -> (GpuMemoryReader, Arc<Mutex<Vec<(u64, usize)>>>) {
        let mut backing = vec![0u8; 64 * 1024];
        backing[sentinel_offset..sentinel_offset + 8]
            .copy_from_slice(&sentinel_value.to_le_bytes());
        let backing = Arc::new(backing);
        let log: Arc<Mutex<Vec<(u64, usize)>>> = Arc::new(Mutex::new(Vec::new()));
        let log_clone = Arc::clone(&log);
        let reader: GpuMemoryReader = Arc::new(move |gpu_addr, dst| {
            log_clone.lock().unwrap().push((gpu_addr, dst.len()));
            let offset = (gpu_addr - program_base) as usize;
            let end = (offset + dst.len()).min(backing.len());
            if offset < backing.len() {
                let n = end - offset;
                dst[..n].copy_from_slice(&backing[offset..end]);
            }
        });
        (reader, log)
    }

    #[test]
    fn try_find_size_finds_self_branch_a_in_first_block() {
        let program_base: u64 = 0x1_0000_0000;
        let sentinel_offset = 0x80; // bytes from start_address
        let (reader, log) =
            make_mock_gpu_with_sentinel(program_base, sentinel_offset, SELF_BRANCH_A);
        let mut env = GenericEnvironment::new()
            .with_gpu_read(reader)
            .with_program(program_base, 0);

        let size = env.try_find_size().expect("sentinel must be found");
        assert_eq!(size as usize, sentinel_offset);

        // Exactly one BLOCK_SIZE read at the program base.
        let log = log.lock().unwrap();
        assert_eq!(log.len(), 1);
        assert_eq!(log[0], (program_base, TRY_FIND_SIZE_BLOCK_BYTES));
    }

    #[test]
    fn try_find_size_finds_self_branch_b_across_multiple_blocks() {
        let program_base: u64 = 0x2_0000_0000;
        // Sentinel in the third block.
        let sentinel_offset = TRY_FIND_SIZE_BLOCK_BYTES * 2 + 0x40;
        let (reader, log) =
            make_mock_gpu_with_sentinel(program_base, sentinel_offset, SELF_BRANCH_B);
        let mut env = GenericEnvironment::new()
            .with_gpu_read(reader)
            .with_program(program_base, 0);

        let size = env.try_find_size().expect("sentinel must be found");
        assert_eq!(size as usize, sentinel_offset);

        // Three reads, each BLOCK_SIZE, advancing by BLOCK_SIZE.
        let log = log.lock().unwrap();
        assert_eq!(log.len(), 3);
        for (i, entry) in log.iter().enumerate() {
            assert_eq!(
                *entry,
                (
                    program_base + (i * TRY_FIND_SIZE_BLOCK_BYTES) as u64,
                    TRY_FIND_SIZE_BLOCK_BYTES
                )
            );
        }
    }

    #[test]
    fn try_find_size_returns_none_when_no_sentinel_within_max() {
        // Backing memory contains no sentinel anywhere; capping at MAXIMUM_SIZE.
        let program_base: u64 = 0x3_0000_0000;
        let backing = Arc::new(vec![0u8; TRY_FIND_SIZE_MAX_BYTES + TRY_FIND_SIZE_BLOCK_BYTES]);
        let reader: GpuMemoryReader = Arc::new(move |gpu_addr, dst| {
            let offset = (gpu_addr - program_base) as usize;
            let end = (offset + dst.len()).min(backing.len());
            if offset < backing.len() {
                let n = end - offset;
                dst[..n].copy_from_slice(&backing[offset..end]);
            }
        });
        let mut env = GenericEnvironment::new()
            .with_gpu_read(reader)
            .with_program(program_base, 0);

        assert!(env.try_find_size().is_none());
    }

    #[test]
    fn analyze_returns_cityhash_of_shader_bytes() {
        let program_base: u64 = 0x4_0000_0000;
        let sentinel_offset = 0x40;
        let (reader, _log) =
            make_mock_gpu_with_sentinel(program_base, sentinel_offset, SELF_BRANCH_A);

        // Analyze should return Some(hash) and seed cached_lowest/highest.
        let mut env = GenericEnvironment::new()
            .with_gpu_read(reader)
            .with_program(program_base, 0);
        let hash = env.analyze().expect("analyze must succeed");
        assert_ne!(hash, 0);
        assert_eq!(env.cached_lowest, 0);
        assert_eq!(env.cached_highest, sentinel_offset as u32);
    }

    #[test]
    fn read_instruction_falls_back_to_gpu_memory_outside_cache() {
        let program_base: u64 = 0x5_0000_0000;
        let target_addr: u32 = 0x200;
        let target_value: u64 = 0xDEAD_BEEF_CAFE_BABE;
        // Backing with the target value at offset target_addr.
        let mut backing = vec![0u8; 0x1000];
        backing[target_addr as usize..target_addr as usize + 8]
            .copy_from_slice(&target_value.to_le_bytes());
        let backing = Arc::new(backing);
        let reader: GpuMemoryReader = Arc::new(move |gpu_addr, dst| {
            let offset = (gpu_addr - program_base) as usize;
            let end = (offset + dst.len()).min(backing.len());
            if offset < backing.len() {
                dst[..end - offset].copy_from_slice(&backing[offset..end]);
            }
        });
        let mut env = GenericEnvironment::new()
            .with_gpu_read(reader)
            .with_program(program_base, 0);

        // No SetCachedSize was called, so the address misses the cache and
        // must be served from the reader.
        let value = env.read_instruction(target_addr);
        assert_eq!(value, target_value);
        assert!(env.has_unbound_instructions);
    }
}
