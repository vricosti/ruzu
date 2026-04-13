// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Shader pipeline cache: Maxwell binary → compiled SPIR-V.
//!
//! Caches compiled shaders by hashing the Maxwell binary + runtime state.
//! The cache is device-independent — it stores SPIR-V words that can be
//! loaded into VkShaderModule by the Vulkan backend.
//!
//! Matches zuyu's `vk_pipeline_cache.cpp` concept.

use std::collections::HashMap;

use super::backend;
use super::frontend::control_flow;
use super::frontend::maxwell_opcodes;
use super::frontend::structured_control_flow;
use super::frontend::translate::TranslatorVisitor;
use super::ir::basic_block::Block;
use super::ir::program::{Program, ShaderInfo};
use super::ir::types::ShaderStage;
use super::ir_opt;
use super::profile::Profile;
use super::runtime_info::RuntimeInfo;

/// Key for looking up a cached shader.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ShaderKey {
    /// Hash of the Maxwell binary code.
    pub code_hash: u64,
    /// Shader stage.
    pub stage: ShaderStage,
}

/// A compiled shader ready for Vulkan consumption.
#[derive(Debug, Clone)]
pub struct CompiledShader {
    /// SPIR-V words ready for VkShaderModule creation.
    pub spirv_words: Vec<u32>,
    /// Shader resource usage information.
    pub info: ShaderInfo,
    /// Shader stage.
    pub stage: ShaderStage,
}

/// A compiled shader emitted as GLSL source for the OpenGL backend.
#[derive(Debug, Clone)]
pub struct CompiledGlslShader {
    /// GLSL source ready for `glShaderSource` / `glCompileShader`.
    pub source: String,
    /// Shader resource usage information.
    pub info: ShaderInfo,
    /// Shader stage.
    pub stage: ShaderStage,
}

/// Pipeline cache — maps shader keys to compiled SPIR-V.
pub struct PipelineCache {
    /// Cached compiled shaders.
    cache: HashMap<ShaderKey, CompiledShader>,
    /// GPU/driver profile for SPIR-V emission.
    profile: Profile,
}

impl PipelineCache {
    /// Create a new pipeline cache with the given GPU profile.
    pub fn new(profile: Profile) -> Self {
        Self {
            cache: HashMap::new(),
            profile,
        }
    }

    /// Create a pipeline cache with default profile.
    pub fn with_default_profile() -> Self {
        Self::new(Profile::default())
    }

    /// Get or compile a shader from Maxwell binary code.
    ///
    /// `code` is a slice of Maxwell instructions (each instruction is 8 bytes / u64).
    /// `stage` is the shader stage (Vertex, Fragment, etc.).
    ///
    /// Returns the compiled shader with SPIR-V words and shader info.
    pub fn get_or_compile(
        &mut self,
        code: &[u64],
        stage: ShaderStage,
        runtime_info: &RuntimeInfo,
    ) -> &CompiledShader {
        let key = ShaderKey {
            code_hash: hash_code(code),
            stage,
        };

        if !self.cache.contains_key(&key) {
            let compiled = compile_shader(code, stage, &self.profile, runtime_info);
            self.cache.insert(key, compiled);
        }

        self.cache.get(&key).unwrap()
    }

    /// Check if a shader is already cached.
    pub fn contains(&self, code: &[u64], stage: ShaderStage) -> bool {
        let key = ShaderKey {
            code_hash: hash_code(code),
            stage,
        };
        self.cache.contains_key(&key)
    }

    /// Number of cached shaders.
    pub fn len(&self) -> usize {
        self.cache.len()
    }

    /// Whether the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }

    /// Clear all cached shaders.
    pub fn clear(&mut self) {
        self.cache.clear();
    }
}

/// Compile a Maxwell shader binary to SPIR-V.
///
/// This is the main entry point for the shader recompiler pipeline:
/// Maxwell binary → decode opcodes → build CFG → structured CF
/// → translate to IR → optimize → emit SPIR-V.
pub fn compile_shader(
    code: &[u64],
    stage: ShaderStage,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
) -> CompiledShader {
    log::debug!("Compiling {:?} shader ({} instructions)", stage, code.len());

    // Step 1: Build control flow graph from Maxwell instructions.
    let cfg_blocks = control_flow::build_cfg(code);
    log::trace!("  CFG: {} blocks", cfg_blocks.len());

    // Step 2: Convert flat CFG to structured control flow (if/loop/break/return).
    let syntax_list = structured_control_flow::structure_cfg(&cfg_blocks);
    log::trace!("  Syntax nodes: {}", syntax_list.len());

    // Step 3: Translate Maxwell instructions to IR.
    let mut program = Program::new(stage);
    program.syntax_list = syntax_list;

    // Create initial block for the entry point
    program.blocks.push(Block::new());

    {
        let mut tv = TranslatorVisitor::new(&mut program, 0);

        // Translate each instruction
        for &insn in code {
            tv.translate_instruction(insn);
        }
    }

    // Step 4: Run optimization passes.
    ir_opt::optimize(&mut program);

    // Step 5: Emit SPIR-V.
    let spirv_words = backend::emit_spirv(&program, profile, runtime_info);
    log::debug!(
        "  SPIR-V: {} words, {} cbuf descriptors, {} tex descriptors",
        spirv_words.len(),
        program.info.constant_buffer_descriptors.len(),
        program.info.texture_descriptors.len(),
    );

    CompiledShader {
        spirv_words,
        info: program.info,
        stage,
    }
}

/// Compile a Maxwell shader binary to GLSL source for the OpenGL backend.
///
/// Mirrors [`compile_shader`] but invokes the GLSL emitter instead of the
/// SPIR-V emitter, returning a [`CompiledGlslShader`] whose `source` field
/// can be fed directly into `glShaderSource` / `glCompileShader`.
pub fn compile_shader_glsl(
    code: &[u64],
    stage: ShaderStage,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
) -> CompiledGlslShader {
    log::debug!(
        "Compiling {:?} shader to GLSL ({} instructions)",
        stage,
        code.len()
    );

    let cfg_blocks = control_flow::build_cfg(code);
    let syntax_list = structured_control_flow::structure_cfg(&cfg_blocks);

    let mut program = Program::new(stage);
    program.syntax_list = syntax_list;
    program.blocks.push(Block::new());
    {
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        for &insn in code {
            tv.translate_instruction(insn);
        }
    }

    ir_opt::optimize(&mut program);

    let mut bindings = backend::bindings::Bindings::default();
    let source = backend::glsl::emit_glsl(profile, runtime_info, &program, &mut bindings);
    log::debug!("  GLSL: {} bytes", source.len());

    CompiledGlslShader {
        source,
        info: program.info,
        stage,
    }
}

/// Hash Maxwell instruction code for cache lookup.
fn hash_code(code: &[u64]) -> u64 {
    // FNV-1a hash
    let mut hash: u64 = 0xcbf29ce484222325;
    for &insn in code {
        let bytes = insn.to_le_bytes();
        for &byte in &bytes {
            hash ^= byte as u64;
            hash = hash.wrapping_mul(0x100000001b3);
        }
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hash_code_deterministic() {
        let code = vec![0x1234_5678_9abc_def0u64, 0xfedcba9876543210];
        let h1 = hash_code(&code);
        let h2 = hash_code(&code);
        assert_eq!(h1, h2);
    }

    #[test]
    fn test_hash_code_different_inputs() {
        let code1 = vec![0x0000_0000_0000_0001u64];
        let code2 = vec![0x0000_0000_0000_0002u64];
        assert_ne!(hash_code(&code1), hash_code(&code2));
    }

    #[test]
    fn test_pipeline_cache_empty() {
        let cache = PipelineCache::with_default_profile();
        assert!(cache.is_empty());
        assert_eq!(cache.len(), 0);
    }

    #[test]
    fn test_compile_empty_shader() {
        let profile = Profile::default();
        let code: Vec<u64> = vec![];
        let runtime_info = RuntimeInfo::default();
        let compiled = compile_shader(&code, ShaderStage::VertexB, &profile, &runtime_info);
        assert!(!compiled.spirv_words.is_empty()); // Should produce valid SPIR-V header at minimum
        assert_eq!(compiled.stage, ShaderStage::VertexB);
    }

    #[test]
    fn test_compile_shader_glsl_emits_source() {
        let profile = Profile::default();
        let code: Vec<u64> = vec![];
        let runtime_info = RuntimeInfo::default();
        let compiled = compile_shader_glsl(&code, ShaderStage::VertexB, &profile, &runtime_info);
        assert!(
            !compiled.source.is_empty(),
            "GLSL emitter should produce a non-empty source string for an empty shader"
        );
        assert_eq!(compiled.stage, ShaderStage::VertexB);
    }

    #[test]
    fn test_pipeline_cache_get_or_compile() {
        let mut cache = PipelineCache::with_default_profile();
        let code: Vec<u64> = vec![0x0000_0000_0000_0000]; // NOP-like instruction
        assert!(!cache.contains(&code, ShaderStage::Fragment));

        let runtime_info = RuntimeInfo::default();
        let _compiled = cache.get_or_compile(&code, ShaderStage::Fragment, &runtime_info);
        assert!(cache.contains(&code, ShaderStage::Fragment));
        assert_eq!(cache.len(), 1);

        // Second lookup should hit cache
        let _compiled2 = cache.get_or_compile(&code, ShaderStage::Fragment, &runtime_info);
        assert_eq!(cache.len(), 1);
    }
}
