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
use super::frontend::structured_control_flow;
use super::frontend::translate::TranslatorVisitor;
use super::frontend::translate_program::{
    collect_interpolation_info, convert_legacy_to_generic, merge_dual_vertex_programs,
};
use super::ir::basic_block::Block;
use super::ir::post_order::post_order;
use super::ir::program::{Program, ShaderInfo};
use super::ir::types::ShaderStage;
use super::ir_opt;
use super::profile::Profile;
use super::program_header::ProgramHeader;
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

fn translate_cfg_to_program(
    code: &[u64],
    stage: ShaderStage,
    base_offset: u32,
    cfg_blocks: &[control_flow::CfgBlock],
    sph: Option<&ProgramHeader>,
) -> Program {
    let syntax_list = structured_control_flow::structure_cfg(cfg_blocks);
    let mut program = Program::new(stage);
    program.syntax_list = syntax_list;
    program.blocks = (0..cfg_blocks.len()).map(|_| Block::new()).collect();

    for (idx, cfg_block) in cfg_blocks.iter().enumerate() {
        if let Some(target) = cfg_block.branch_true {
            if target < cfg_blocks.len() {
                program.block_mut(idx as u32).add_successor(target as u32);
                program.block_mut(target as u32).add_predecessor(idx as u32);
            }
        }
        if let Some(target) = cfg_block.branch_false {
            if target < cfg_blocks.len() {
                program.block_mut(idx as u32).add_successor(target as u32);
                program.block_mut(target as u32).add_predecessor(idx as u32);
            }
        }
    }

    for (idx, cfg_block) in cfg_blocks.iter().enumerate() {
        program.block_mut(idx as u32).order = idx as u32;
        let mut tv = TranslatorVisitor::new_with_sph(&mut program, idx as u32, sph.cloned());
        for i in cfg_block.begin as usize..cfg_block.end as usize {
            if i >= code.len() {
                break;
            }
            // Skip Maxwell sched-control words. Each 32-byte SASS bundle is one
            // 8-byte sched word followed by three 8-byte instructions, so every
            // absolute word whose byte offset is 32-byte aligned is control metadata.
            if is_sched_control_word(base_offset, i) {
                continue;
            }
            if std::env::var_os("RUZU_TRACE_SHADER_WORDS").is_some()
                && maxwell_opcode_is_unknown(code[i])
            {
                eprintln!(
                    "[SHADER_WORD_UNKNOWN] stage={:?} abs=0x{:X} index={} word=0x{:016X}",
                    stage,
                    base_offset + (i as u32 * 8),
                    i,
                    code[i]
                );
            }
            tv.translate_instruction(code[i]);
        }
    }

    if !program.blocks.is_empty() {
        program.post_order_blocks = post_order(&program.blocks, 0);
        clear_unreachable_blocks(&mut program);
    }

    program
}

/// Rust adaptation of upstream `RemoveUnreachableBlocks`.
///
/// Upstream erases unreachable `IR::Block*` entries after computing
/// `post_order_blocks`. Rust `Value::Inst` stores block indices rather than
/// pointers, so erasing from `program.blocks` would shift indices and corrupt
/// instruction references. Clearing unreachable blocks preserves indices while
/// preventing stale instructions from reaching optimization/emission.
fn clear_unreachable_blocks(program: &mut Program) {
    if program.blocks.len() == program.post_order_blocks.len() {
        return;
    }
    let reachable: std::collections::BTreeSet<u32> =
        program.post_order_blocks.iter().copied().collect();
    for (index, block) in program.blocks.iter_mut().enumerate().skip(1) {
        if reachable.contains(&(index as u32)) {
            continue;
        }
        for inst in &mut block.instructions {
            *inst = None;
        }
        block.imm_predecessors.clear();
        block.imm_successors.clear();
    }
}

fn is_sched_control_word(base_offset: u32, word_index: usize) -> bool {
    ((base_offset as usize / 8) + word_index) % 4 == 0
}

fn maxwell_opcode_is_unknown(word: u64) -> bool {
    super::frontend::maxwell_opcodes::decode_opcode(word).is_none()
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

    // Step 2/3: Convert flat CFG to structured control flow and translate
    // Maxwell instructions into matching IR blocks.
    let mut program = translate_cfg_to_program(code, stage, 0, &cfg_blocks, None);
    log::trace!("  Syntax nodes: {}", program.syntax_list.len());

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
    let mut program = translate_cfg_to_program(code, stage, 0, &cfg_blocks, None);

    ir_opt::optimize(&mut program);

    let mut bindings = backend::bindings::Bindings::default();
    convert_legacy_to_generic(&mut program, runtime_info);
    let source = backend::glsl::emit_glsl(profile, runtime_info, &mut program, &mut bindings);
    log::debug!("  GLSL: {} bytes", source.len());
    if std::env::var_os("RUZU_DUMP_GLSL").is_some() {
        eprintln!("[RUZU_DUMP_GLSL {:?}]\n{}", stage, source);
    }

    CompiledGlslShader {
        source,
        info: program.info,
        stage,
    }
}

/// Compile a Maxwell shader whose first word corresponds to an absolute
/// shader-program byte offset. This preserves upstream `Location` ownership
/// for sched-control skipping when the cached slice does not start at offset 0.
pub fn compile_shader_glsl_at_offset(
    code: &[u64],
    stage: ShaderStage,
    base_offset: u32,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
) -> CompiledGlslShader {
    let mut bindings = backend::bindings::Bindings::default();
    emit_glsl_program_at_offset(
        code,
        stage,
        base_offset,
        profile,
        runtime_info,
        &mut bindings,
        None,
        None,
    )
}

/// Same as [`compile_shader_glsl_at_offset`], but reuses the caller-owned
/// GLSL binding allocator across stages. Upstream `gl_shader_cache.cpp`
/// keeps one `Shader::Backend::Bindings binding` for the whole graphics
/// pipeline so vertex/fragment UBOs and textures receive distinct GL binding
/// points.
pub fn compile_shader_glsl_at_offset_with_bindings(
    code: &[u64],
    stage: ShaderStage,
    base_offset: u32,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    bindings: &mut backend::bindings::Bindings,
) -> CompiledGlslShader {
    emit_glsl_program_at_offset(
        code,
        stage,
        base_offset,
        profile,
        runtime_info,
        bindings,
        None,
        None,
    )
}

/// OpenGL graphics path variant that mirrors upstream's
/// `TexturePass(env, program, host_info)` for currently ported bound
/// texture instructions.
pub fn compile_shader_glsl_at_offset_with_bindings_and_texture_bound(
    code: &[u64],
    stage: ShaderStage,
    base_offset: u32,
    texture_bound_buffer: u32,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    bindings: &mut backend::bindings::Bindings,
) -> CompiledGlslShader {
    emit_glsl_program_at_offset(
        code,
        stage,
        base_offset,
        profile,
        runtime_info,
        bindings,
        Some(texture_bound_buffer),
        None,
    )
}

/// Same as [`compile_shader_glsl_at_offset_with_bindings_and_texture_bound`],
/// but preserves the upstream environment-owned SPH for fragment interpolation
/// and IPA perspective handling.
pub fn compile_shader_glsl_at_offset_with_bindings_and_texture_bound_and_sph(
    code: &[u64],
    stage: ShaderStage,
    base_offset: u32,
    texture_bound_buffer: u32,
    sph: &ProgramHeader,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    bindings: &mut backend::bindings::Bindings,
) -> CompiledGlslShader {
    emit_glsl_program_at_offset(
        code,
        stage,
        base_offset,
        profile,
        runtime_info,
        bindings,
        Some(texture_bound_buffer),
        Some(sph),
    )
}

fn emit_glsl_program_at_offset(
    code: &[u64],
    stage: ShaderStage,
    base_offset: u32,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    bindings: &mut backend::bindings::Bindings,
    texture_bound_buffer: Option<u32>,
    sph: Option<&ProgramHeader>,
) -> CompiledGlslShader {
    log::debug!(
        "Compiling {:?} shader to GLSL ({} instructions, base_offset=0x{:X})",
        stage,
        code.len(),
        base_offset
    );
    if std::env::var_os("RUZU_TRACE_SHADER_WORDS").is_some() {
        trace_shader_words(stage, base_offset, code);
    }

    let cfg_blocks = control_flow::build_cfg(code);
    let mut program = translate_cfg_to_program(code, stage, base_offset, &cfg_blocks, sph);

    if let Some(texture_bound_buffer) = texture_bound_buffer {
        ir_opt::optimize_with_bound_textures(&mut program, texture_bound_buffer);
    } else {
        ir_opt::optimize(&mut program);
    }

    convert_legacy_to_generic(&mut program, runtime_info);
    if let Some(sph) = sph {
        collect_interpolation_info(sph, &mut program);
    }
    let source = backend::glsl::emit_glsl(profile, runtime_info, &mut program, bindings);
    if std::env::var_os("RUZU_DUMP_GLSL").is_some() {
        eprintln!("[RUZU_DUMP_GLSL {:?}]\n{}", stage, source);
    }

    CompiledGlslShader {
        source,
        info: program.info,
        stage,
    }
}

/// Compile a Maxwell VertexA + VertexB pair into one merged VertexB GLSL
/// program, matching upstream `MergeDualVertexPrograms` ownership.
pub fn compile_dual_vertex_shader_glsl_at_offset(
    vertex_a_code: &[u64],
    vertex_a_base_offset: u32,
    vertex_b_code: &[u64],
    vertex_b_base_offset: u32,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
) -> CompiledGlslShader {
    let mut bindings = backend::bindings::Bindings::default();
    compile_dual_vertex_shader_glsl_at_offset_with_bindings(
        vertex_a_code,
        vertex_a_base_offset,
        vertex_b_code,
        vertex_b_base_offset,
        profile,
        runtime_info,
        &mut bindings,
    )
}

pub fn compile_dual_vertex_shader_glsl_at_offset_with_bindings(
    vertex_a_code: &[u64],
    vertex_a_base_offset: u32,
    vertex_b_code: &[u64],
    vertex_b_base_offset: u32,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    bindings: &mut backend::bindings::Bindings,
) -> CompiledGlslShader {
    log::debug!(
        "Compiling dual vertex shader to GLSL (va={} words @0x{:X}, vb={} words @0x{:X})",
        vertex_a_code.len(),
        vertex_a_base_offset,
        vertex_b_code.len(),
        vertex_b_base_offset
    );
    if std::env::var_os("RUZU_TRACE_SHADER_WORDS").is_some() {
        trace_shader_words(ShaderStage::VertexA, vertex_a_base_offset, vertex_a_code);
        trace_shader_words(ShaderStage::VertexB, vertex_b_base_offset, vertex_b_code);
    }

    let mut vertex_a =
        translate_and_optimize(vertex_a_code, ShaderStage::VertexA, vertex_a_base_offset);
    let mut vertex_b =
        translate_and_optimize(vertex_b_code, ShaderStage::VertexB, vertex_b_base_offset);
    let mut program = merge_dual_vertex_programs(&mut vertex_a, &mut vertex_b);

    convert_legacy_to_generic(&mut program, runtime_info);
    let source = backend::glsl::emit_glsl(profile, runtime_info, &mut program, bindings);
    if std::env::var_os("RUZU_DUMP_GLSL").is_some() {
        eprintln!("[RUZU_DUMP_GLSL DualVertex]\n{}", source);
    }

    CompiledGlslShader {
        source,
        info: program.info,
        stage: ShaderStage::VertexB,
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

fn translate_and_optimize(code: &[u64], stage: ShaderStage, base_offset: u32) -> Program {
    let cfg_blocks = control_flow::build_cfg(code);
    let mut program = translate_cfg_to_program(code, stage, base_offset, &cfg_blocks, None);
    ir_opt::optimize(&mut program);
    program
}

fn trace_shader_words(stage: ShaderStage, base_offset: u32, code: &[u64]) {
    eprintln!(
        "[SHADER_CODE] stage={:?} base=0x{:X} words={}",
        stage,
        base_offset,
        code.len()
    );
    for (i, &word) in code.iter().take(32).enumerate() {
        eprintln!(
            "[SHADER_CODE_WORD] stage={:?} abs=0x{:X} index={} sched={} word=0x{:016X}",
            stage,
            base_offset + (i as u32 * 8),
            i,
            is_sched_control_word(base_offset, i),
            word
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::control_flow::{CfgBlock, Condition, EndClass};
    use crate::ir::program::SyntaxNode;

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
    fn cfg_translation_creates_matching_ir_blocks_and_edges() {
        let cfg_blocks = vec![
            CfgBlock {
                begin: 0,
                end: 1,
                end_class: EndClass::Branch,
                branch_true: Some(1),
                branch_false: None,
                cond: Condition::always(),
                stack_depth: 0,
            },
            CfgBlock {
                begin: 1,
                end: 2,
                end_class: EndClass::Branch,
                branch_true: None,
                branch_false: None,
                cond: Condition::always(),
                stack_depth: 0,
            },
        ];

        let program = translate_cfg_to_program(
            &[0, 0],
            ShaderStage::VertexB,
            0,
            cfg_blocks.as_slice(),
            None,
        );

        assert_eq!(program.blocks.len(), 2);
        assert_eq!(program.block(0).imm_successors, vec![1]);
        assert_eq!(program.block(1).imm_predecessors, vec![0]);
        assert_eq!(program.post_order_blocks, vec![1, 0]);
        assert!(
            program
                .syntax_list
                .iter()
                .any(|node| matches!(node, SyntaxNode::Block(1))),
            "syntax block indices must have matching IR blocks"
        );
    }

    #[test]
    fn clear_unreachable_blocks_preserves_indices_but_drops_stale_instructions() {
        use crate::ir::basic_block::Block;
        use crate::ir::instruction::Inst;
        use crate::ir::opcodes::Opcode;
        use crate::ir::value::{Reg, Value};

        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.blocks.push(Block::new());
        program
            .block_mut(1)
            .append_inst(Inst::new(Opcode::GetRegister, vec![Value::Reg(Reg(3))]));
        program.post_order_blocks = vec![0];

        clear_unreachable_blocks(&mut program);

        assert_eq!(program.blocks.len(), 2);
        assert!(program.block(1).is_empty());
        assert_eq!(program.post_order_blocks, vec![0]);
    }

    #[test]
    fn sched_control_skip_uses_absolute_shader_offset() {
        assert!(is_sched_control_word(0, 0));
        assert!(!is_sched_control_word(0, 1));
        assert!(is_sched_control_word(8, 3));
        assert!(!is_sched_control_word(8, 0));
        assert!(is_sched_control_word(0x50, 2));
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
