// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Shader pipeline cache: Maxwell binary → compiled SPIR-V.
//!
//! Caches compiled shaders by hashing the Maxwell binary + runtime state.
//! The cache is device-independent — it stores SPIR-V words that can be
//! loaded into VkShaderModule by the Vulkan backend.
//!
//! Matches zuyu's `vk_pipeline_cache.cpp` concept.

use std::collections::{hash_map::DefaultHasher, BTreeMap, HashMap};
use std::hash::{Hash, Hasher};

use super::backend;
use super::environment::Environment;
use super::frontend::control_flow;
use super::frontend::structured_control_flow::{self, Expr, StructuredAction};
use super::frontend::translate::TranslatorVisitor;
use super::frontend::translate_program::{
    collect_interpolation_info, convert_legacy_to_generic, merge_dual_vertex_programs,
};
use super::ir::basic_block::Block;
use super::ir::emitter::Emitter;
use super::ir::instruction::Inst;
use super::ir::opcodes::Opcode;
use super::ir::post_order::post_order;
use super::ir::program::{Program, ShaderInfo, SyntaxNode};
use super::ir::types::{OutputTopology, ShaderStage};
use super::ir::value::{InstRef, Pred, Value};
use super::ir_opt;
use super::profile::Profile;
use super::program_header::ProgramHeader;
use super::runtime_info::RuntimeInfo;

/// Key for looking up a cached shader.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ShaderKey {
    /// Hash of the Maxwell binary code.
    pub code_hash: u64,
    /// Hash of pipeline runtime state that affects translation/emission.
    pub runtime_hash: u64,
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
    let mut structured = if std::env::var_os("RUZU_SHADER_FORCE_LINEAR_SYNTAX").is_some() {
        structured_control_flow::StructuredSyntax {
            syntax: linear_syntax_list(cfg_blocks),
            actions: Vec::new(),
            block_count: cfg_blocks.len(),
        }
    } else {
        structured_control_flow::structure_cfg_detailed(cfg_blocks)
    };
    remove_pre_end_if_merge_blocks(&mut structured);
    let mut program = Program::new(stage);
    program.syntax_list = structured.syntax;
    program.blocks = (0..structured.block_count).map(|_| Block::new()).collect();
    materialize_return_epilogues(&mut program);

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
    for idx in cfg_blocks.len()..program.blocks.len() {
        program.block_mut(idx as u32).order = idx as u32;
    }
    materialize_structured_actions(&mut program, &structured.actions);
    rebuild_syntax_successors(&mut program);

    if !program.blocks.is_empty() {
        program.post_order_blocks = post_order(&program.blocks, 0);
        clear_unreachable_blocks(&mut program);
    }

    program
}

/// Upstream `structured_control_flow.cpp` creates a dedicated return block and
/// immediately emits `IR::IREmitter{*return_block}.Epilogue()`. Ruzu builds the
/// syntax list before allocating the Rust `Program` blocks, so materialize the
/// same IR instruction after the block vector exists.
fn materialize_return_epilogues(program: &mut Program) {
    let return_blocks: Vec<u32> = program
        .syntax_list
        .windows(2)
        .filter_map(|nodes| match (&nodes[0], &nodes[1]) {
            (SyntaxNode::Block(block), SyntaxNode::Return) => Some(*block),
            _ => None,
        })
        .collect();

    for block in return_blocks {
        Emitter::new(program, block).epilogue();
    }
}

fn linear_syntax_list(cfg_blocks: &[control_flow::CfgBlock]) -> Vec<SyntaxNode> {
    let mut syntax = Vec::with_capacity(cfg_blocks.len() + 1);
    for block in 0..cfg_blocks.len() {
        syntax.push(SyntaxNode::Block(block as u32));
    }
    if !cfg_blocks.is_empty() {
        syntax.push(SyntaxNode::Return);
    }
    syntax
}

fn materialize_condition(
    program: &mut Program,
    block: u32,
    cond: control_flow::Condition,
) -> Value {
    let pred = Pred(cond.pred);
    if pred.is_true() {
        return Value::ImmU1(!cond.negated);
    }

    let get_pred = append_inst(
        program,
        block,
        Inst::new(Opcode::GetPred, vec![Value::Pred(pred)]),
    );
    let value = if cond.negated {
        append_inst(
            program,
            block,
            Inst::new(Opcode::LogicalNot, vec![get_pred]),
        )
    } else {
        get_pred
    };
    append_inst(program, block, Inst::new(Opcode::ConditionRef, vec![value]))
}

fn materialize_structured_actions(program: &mut Program, actions: &[StructuredAction]) {
    for action in actions {
        match action {
            StructuredAction::SetVariable { block, id, expr } => {
                let value = materialize_expr(program, *block, expr);
                append_inst(
                    program,
                    *block,
                    Inst::new(Opcode::SetGotoVariable, vec![Value::ImmU32(*id), value]),
                );
            }
            StructuredAction::SetIndirectBranchVariable {
                block,
                branch_reg,
                branch_offset,
            } => {
                let reg = append_inst(
                    program,
                    *block,
                    Inst::new(
                        Opcode::GetRegister,
                        vec![Value::Reg(super::ir::value::Reg(*branch_reg as u8))],
                    ),
                );
                let address = append_inst(
                    program,
                    *block,
                    Inst::new(
                        Opcode::IAdd32,
                        vec![reg, Value::ImmU32(*branch_offset as u32)],
                    ),
                );
                append_inst(
                    program,
                    *block,
                    Inst::new(Opcode::SetIndirectBranchVariable, vec![address]),
                );
            }
            StructuredAction::Condition {
                syntax_index,
                block,
                expr,
            } => {
                let value = materialize_expr(program, *block, expr);
                let cond_ref = append_inst(
                    program,
                    *block,
                    Inst::new(Opcode::ConditionRef, vec![value]),
                );
                match &mut program.syntax_list[*syntax_index] {
                    SyntaxNode::If { cond, .. }
                    | SyntaxNode::Repeat { cond, .. }
                    | SyntaxNode::Break { cond, .. } => *cond = cond_ref,
                    _ => {}
                }
            }
        }
    }
}

fn remove_pre_end_if_merge_blocks(structured: &mut structured_control_flow::StructuredSyntax) {
    let mut index = 1usize;
    while index < structured.syntax.len() {
        let SyntaxNode::EndIf { merge } = structured.syntax[index] else {
            index += 1;
            continue;
        };
        if matches!(structured.syntax[index - 1], SyntaxNode::Block(block) if block == merge) {
            let removed_index = index - 1;
            structured.syntax.remove(removed_index);
            for action in &mut structured.actions {
                if let StructuredAction::Condition { syntax_index, .. } = action {
                    if *syntax_index > removed_index {
                        *syntax_index -= 1;
                    }
                }
            }
        } else {
            index += 1;
        }
    }
}

fn materialize_expr(program: &mut Program, block: u32, expr: &Expr) -> Value {
    match expr {
        Expr::Identity(cond) => materialize_condition(program, block, *cond),
        Expr::Not(expr) => {
            let value = materialize_expr(program, block, expr);
            append_inst(program, block, Inst::new(Opcode::LogicalNot, vec![value]))
        }
        Expr::Or(lhs, rhs) => {
            let lhs = materialize_expr(program, block, lhs);
            let rhs = materialize_expr(program, block, rhs);
            append_inst(program, block, Inst::new(Opcode::LogicalOr, vec![lhs, rhs]))
        }
        Expr::Variable(id) => append_inst(
            program,
            block,
            Inst::new(Opcode::GetGotoVariable, vec![Value::ImmU32(*id)]),
        ),
        Expr::IndirectBranchCond(location) => {
            let branch = append_inst(
                program,
                block,
                Inst::new(Opcode::GetIndirectBranchVariable, vec![]),
            );
            append_inst(
                program,
                block,
                Inst::new(Opcode::IEqual, vec![branch, Value::ImmU32(*location)]),
            )
        }
    }
}

fn add_syntax_edge(program: &mut Program, from: u32, to: u32) {
    if from as usize >= program.blocks.len() || to as usize >= program.blocks.len() {
        return;
    }
    if !program.block(from).imm_successors.contains(&to) {
        program.block_mut(from).add_successor(to);
    }
    if !program.block(to).imm_predecessors.contains(&from) {
        program.block_mut(to).add_predecessor(from);
    }
}

fn rebuild_syntax_successors(program: &mut Program) {
    for block in &mut program.blocks {
        block.imm_successors.clear();
        block.imm_predecessors.clear();
    }

    let mut current_block = None;
    let syntax = program.syntax_list.clone();
    for node in syntax {
        match node {
            SyntaxNode::Block(block) => {
                if let Some(current) = current_block {
                    if current != block {
                        add_syntax_edge(program, current, block);
                    }
                }
                current_block = Some(block);
            }
            SyntaxNode::If { body, merge, .. }
            | SyntaxNode::Break {
                merge, skip: body, ..
            } => {
                if let Some(current) = current_block {
                    add_syntax_edge(program, current, body);
                    add_syntax_edge(program, current, merge);
                }
                current_block = None;
            }
            SyntaxNode::Loop {
                body,
                continue_block,
                merge,
            } => {
                if let Some(current) = current_block {
                    add_syntax_edge(program, current, body);
                }
                add_syntax_edge(program, continue_block, body);
                add_syntax_edge(program, continue_block, merge);
                current_block = None;
            }
            SyntaxNode::Repeat {
                loop_header, merge, ..
            } => {
                if let Some(current) = current_block {
                    add_syntax_edge(program, current, loop_header);
                    add_syntax_edge(program, current, merge);
                }
                current_block = None;
            }
            SyntaxNode::EndIf { merge } => {
                if let Some(current) = current_block {
                    add_syntax_edge(program, current, merge);
                }
                current_block = None;
            }
            SyntaxNode::Return | SyntaxNode::Unreachable => {
                current_block = None;
            }
        }
    }
}

fn append_inst(program: &mut Program, block: u32, inst: Inst) -> Value {
    let inst_index = program.block_mut(block).append_inst(inst);
    Value::Inst(InstRef {
        block,
        inst: inst_index,
    })
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
    for node in &mut program.syntax_list {
        match node {
            SyntaxNode::If { cond, .. }
            | SyntaxNode::Repeat { cond, .. }
            | SyntaxNode::Break { cond, .. } => {
                if let Value::Inst(inst_ref) = cond {
                    if !reachable.contains(&inst_ref.block) {
                        *cond = Value::ImmU1(false);
                    }
                }
            }
            _ => {}
        }
    }
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
            runtime_hash: hash_runtime_info(runtime_info),
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
            runtime_hash: hash_runtime_info(&RuntimeInfo::default()),
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

/// Translate a Maxwell shader binary into IR and run the currently ported
/// optimization driver.
///
/// This is the Rust counterpart used by `frontend::translate_program` until
/// the full upstream `TranslateProgram(env, cfg, host_info)` signature is
/// ported. It shares the same CFG, structured-control-flow, translation, and
/// host-info-aware pass sequence as the GLSL/Vulkan compile paths instead of
/// returning an empty placeholder program.
pub fn translate_program_at_offset_with_host_info(
    code: &[u64],
    stage: ShaderStage,
    base_offset: u32,
    host_info: &crate::host_translate_info::HostTranslateInfo,
) -> Program {
    translate_and_optimize_with_host_info(code, stage, base_offset, host_info)
}

/// Translate a Maxwell shader binary into IR using default host capabilities.
pub fn translate_program_at_offset(code: &[u64], stage: ShaderStage, base_offset: u32) -> Program {
    translate_program_at_offset_with_host_info(
        code,
        stage,
        base_offset,
        &crate::host_translate_info::HostTranslateInfo::default(),
    )
}

/// Translate a Maxwell shader binary using the upstream-shaped environment
/// owner for stage metadata and environment-dependent optimization passes.
///
/// This is an incremental Rust counterpart of upstream
/// `TranslateProgram(inst_pool, block_pool, env, cfg, host_info)`: the CFG is
/// built through the upstream-owned environment path so instruction locations
/// are byte-addressed Maxwell `Location`s instead of slice-local indices.
pub fn translate_program_from_env_with_host_info(
    code: &[u64],
    base_offset: u32,
    env: &mut dyn Environment,
    host_info: &crate::host_translate_info::HostTranslateInfo,
) -> Program {
    let cfg_blocks = control_flow::build_cfg_from_env(env, base_offset, code.len());
    let sph = env.sph().clone();
    let mut program = translate_cfg_to_program(
        code,
        env.shader_stage(),
        base_offset,
        &cfg_blocks,
        Some(&sph),
    );
    apply_environment_program_metadata(&mut program, env, host_info);
    optimize_program_with_env(&mut program, env, host_info, Some(&sph));
    collect_interpolation_info(&sph, &mut program);
    program
}

/// Translate a Maxwell shader binary using default host capabilities.
pub fn translate_program_from_env(
    code: &[u64],
    base_offset: u32,
    env: &mut dyn Environment,
) -> Program {
    translate_program_from_env_with_host_info(
        code,
        base_offset,
        env,
        &crate::host_translate_info::HostTranslateInfo::default(),
    )
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
    let host_info = host_info_from_profile(profile);
    emit_glsl_program_at_offset(
        code,
        stage,
        base_offset,
        profile,
        runtime_info,
        &mut bindings,
        None,
        None,
        &host_info,
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
    let host_info = host_info_from_profile(profile);
    emit_glsl_program_at_offset(
        code,
        stage,
        base_offset,
        profile,
        runtime_info,
        bindings,
        None,
        None,
        &host_info,
    )
}

pub fn compile_shader_glsl_at_offset_with_bindings_and_host_info(
    code: &[u64],
    stage: ShaderStage,
    base_offset: u32,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    bindings: &mut backend::bindings::Bindings,
    host_info: &crate::host_translate_info::HostTranslateInfo,
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
        host_info,
    )
}

/// Compile a Maxwell shader to GLSL through the upstream-shaped Environment
/// translation bridge.
pub fn compile_shader_glsl_from_env_with_bindings_and_host_info(
    code: &[u64],
    base_offset: u32,
    env: &mut dyn Environment,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    bindings: &mut backend::bindings::Bindings,
    host_info: &crate::host_translate_info::HostTranslateInfo,
) -> CompiledGlslShader {
    let stage = env.shader_stage();
    let dump = ShaderIrDumpConfig::from_env(stage, base_offset, code);
    let mut program = translate_program_from_env_with_host_info(code, base_offset, env, host_info);
    if let Some(dump) = dump.as_ref() {
        dump.write(&program, "00_env_translated");
    }
    convert_legacy_to_generic(&mut program, runtime_info);
    if let Some(dump) = dump.as_ref() {
        dump.write(&program, "01_env_legacy_to_generic");
    }
    let source = backend::glsl::emit_glsl(profile, runtime_info, &mut program, bindings);
    CompiledGlslShader {
        source,
        info: program.info,
        stage,
    }
}

/// Compile a Maxwell shader to SPIR-V through the upstream-shaped Environment
/// translation bridge.
pub fn compile_shader_from_env_with_host_info(
    code: &[u64],
    base_offset: u32,
    env: &mut dyn Environment,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    host_info: &crate::host_translate_info::HostTranslateInfo,
) -> CompiledShader {
    let stage = env.shader_stage();
    let dump = ShaderIrDumpConfig::from_env(stage, base_offset, code);
    let mut program = translate_program_from_env_with_host_info(code, base_offset, env, host_info);
    if let Some(dump) = dump.as_ref() {
        dump.write(&program, "00_env_translated");
    }
    convert_legacy_to_generic(&mut program, runtime_info);
    if let Some(dump) = dump.as_ref() {
        dump.write(&program, "01_env_legacy_to_generic");
    }
    let spirv_words = backend::emit_spirv(&program, profile, runtime_info);
    CompiledShader {
        spirv_words,
        info: program.info,
        stage,
    }
}

pub fn compile_shader_from_env_with_bindings_and_host_info(
    code: &[u64],
    base_offset: u32,
    env: &mut dyn Environment,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    bindings: &mut backend::bindings::Bindings,
    host_info: &crate::host_translate_info::HostTranslateInfo,
) -> CompiledShader {
    let stage = env.shader_stage();
    let dump = ShaderIrDumpConfig::from_env(stage, base_offset, code);
    let mut program = translate_program_from_env_with_host_info(code, base_offset, env, host_info);
    if let Some(dump) = dump.as_ref() {
        dump.write(&program, "00_env_translated");
    }
    convert_legacy_to_generic(&mut program, runtime_info);
    if let Some(dump) = dump.as_ref() {
        dump.write(&program, "01_env_legacy_to_generic");
    }
    let spirv_words = backend::emit_spirv_with_bindings(&program, profile, runtime_info, bindings);
    CompiledShader {
        spirv_words,
        info: program.info,
        stage,
    }
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
    let host_info = host_info_from_profile(profile);
    emit_glsl_program_at_offset(
        code,
        stage,
        base_offset,
        profile,
        runtime_info,
        bindings,
        Some(texture_bound_buffer),
        None,
        &host_info,
    )
}

pub fn compile_shader_glsl_at_offset_with_bindings_and_texture_bound_and_host_info(
    code: &[u64],
    stage: ShaderStage,
    base_offset: u32,
    texture_bound_buffer: u32,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    bindings: &mut backend::bindings::Bindings,
    host_info: &crate::host_translate_info::HostTranslateInfo,
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
        host_info,
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
    let host_info = host_info_from_profile(profile);
    emit_glsl_program_at_offset(
        code,
        stage,
        base_offset,
        profile,
        runtime_info,
        bindings,
        Some(texture_bound_buffer),
        Some(sph),
        &host_info,
    )
}

pub fn compile_shader_glsl_at_offset_with_bindings_and_texture_bound_and_sph_and_host_info(
    code: &[u64],
    stage: ShaderStage,
    base_offset: u32,
    texture_bound_buffer: u32,
    sph: &ProgramHeader,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    bindings: &mut backend::bindings::Bindings,
    host_info: &crate::host_translate_info::HostTranslateInfo,
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
        host_info,
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
    host_info: &crate::host_translate_info::HostTranslateInfo,
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

    optimize_glsl_with_optional_ir_dump(
        &mut program,
        stage,
        base_offset,
        code,
        texture_bound_buffer,
        sph,
        host_info,
    );

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

fn host_info_from_profile(profile: &Profile) -> crate::host_translate_info::HostTranslateInfo {
    crate::host_translate_info::HostTranslateInfo {
        support_int64: profile.support_int64,
        min_ssbo_alignment: profile.min_ssbo_alignment as u32,
        ..Default::default()
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
    let host_info = host_info_from_profile(profile);
    compile_dual_vertex_shader_glsl_at_offset_with_bindings_and_host_info(
        vertex_a_code,
        vertex_a_base_offset,
        vertex_b_code,
        vertex_b_base_offset,
        profile,
        runtime_info,
        bindings,
        &host_info,
    )
}

pub fn compile_dual_vertex_shader_glsl_at_offset_with_bindings_and_host_info(
    vertex_a_code: &[u64],
    vertex_a_base_offset: u32,
    vertex_b_code: &[u64],
    vertex_b_base_offset: u32,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    bindings: &mut backend::bindings::Bindings,
    host_info: &crate::host_translate_info::HostTranslateInfo,
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

    let mut vertex_a = translate_and_optimize_with_host_info(
        vertex_a_code,
        ShaderStage::VertexA,
        vertex_a_base_offset,
        host_info,
    );
    let mut vertex_b = translate_and_optimize_with_host_info(
        vertex_b_code,
        ShaderStage::VertexB,
        vertex_b_base_offset,
        host_info,
    );
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

fn hash_runtime_info(info: &RuntimeInfo) -> u64 {
    let mut hasher = DefaultHasher::new();
    info.generic_input_types.hash(&mut hasher);
    info.previous_stage_stores.mask.hash(&mut hasher);
    info.previous_stage_legacy_stores_mapping.hash(&mut hasher);
    info.convert_depth_mode.hash(&mut hasher);
    info.force_early_z.hash(&mut hasher);
    info.tess_primitive.hash(&mut hasher);
    info.tess_spacing.hash(&mut hasher);
    info.tess_clockwise.hash(&mut hasher);
    info.input_topology.hash(&mut hasher);
    match info.fixed_state_point_size {
        Some(value) => {
            true.hash(&mut hasher);
            value.to_bits().hash(&mut hasher);
        }
        None => false.hash(&mut hasher),
    }
    info.alpha_test_func.hash(&mut hasher);
    info.alpha_test_reference.to_bits().hash(&mut hasher);
    info.y_negate.hash(&mut hasher);
    info.glasm_use_storage_buffers.hash(&mut hasher);
    info.frag_color_types.hash(&mut hasher);
    info.xfb_count.hash(&mut hasher);
    for varying in info.xfb_varyings.iter().take(info.xfb_count as usize) {
        varying.buffer.hash(&mut hasher);
        varying.stride.hash(&mut hasher);
        varying.offset.hash(&mut hasher);
        varying.components.hash(&mut hasher);
    }
    hasher.finish()
}

fn translate_and_optimize_with_host_info(
    code: &[u64],
    stage: ShaderStage,
    base_offset: u32,
    host_info: &crate::host_translate_info::HostTranslateInfo,
) -> Program {
    let cfg_blocks = control_flow::build_cfg(code);
    let mut program = translate_cfg_to_program(code, stage, base_offset, &cfg_blocks, None);
    ir_opt::optimize_with_host_info(&mut program, host_info);
    program
}

fn apply_environment_program_metadata(
    program: &mut Program,
    env: &dyn Environment,
    host_info: &crate::host_translate_info::HostTranslateInfo,
) {
    program.stage = env.shader_stage();
    program.local_memory_size = env.local_memory_size();
    match program.stage {
        ShaderStage::TessellationControl => {
            program.invocations = env.sph().threads_per_input_primitive();
        }
        ShaderStage::Geometry => {
            let sph = env.sph();
            program.output_topology = output_topology_from_sph(sph.output_topology());
            program.output_vertices = sph.max_output_vertices();
            program.invocations = sph.threads_per_input_primitive();
            program.is_geometry_passthrough = sph.geometry_passthrough();
            if program.is_geometry_passthrough {
                let mask = env.gp_passthrough_mask();
                for bit in 0..mask.len() * 32 {
                    program
                        .info
                        .passthrough
                        .set(bit, ((mask[bit / 32] >> (bit % 32)) & 1) == 0);
                }
                if !host_info.support_geometry_shader_passthrough {
                    program.output_vertices = output_vertices_for_topology(program.output_topology);
                    // Upstream lowers passthrough here. Ruzu keeps the current
                    // backend passthrough path until `EmitGeometryPassthrough`
                    // is ported.
                }
            }
        }
        ShaderStage::Compute => {
            program.workgroup_size = env.workgroup_size();
            program.shared_memory_size = env.shared_memory_size();
        }
        _ => {}
    }
}

fn output_topology_from_sph(topology: super::program_header::OutputTopology) -> OutputTopology {
    match topology {
        super::program_header::OutputTopology::PointList => OutputTopology::PointList,
        super::program_header::OutputTopology::LineStrip => OutputTopology::LineStrip,
        super::program_header::OutputTopology::TriangleStrip => OutputTopology::TriangleStrip,
    }
}

fn output_vertices_for_topology(topology: OutputTopology) -> u32 {
    match topology {
        OutputTopology::PointList => 1,
        OutputTopology::LineStrip => 2,
        OutputTopology::TriangleStrip => 3,
    }
}

fn optimize_program_with_env(
    program: &mut Program,
    env: &mut dyn Environment,
    host_info: &crate::host_translate_info::HostTranslateInfo,
    sph: Option<&ProgramHeader>,
) {
    ir_opt::ssa_rewrite_pass::ssa_rewrite_pass(program);
    ir_opt::identity_removal::identity_removal_pass(program);
    ir_opt::constant_propagation::constant_propagation_pass(program);
    ir_opt::identity_removal::identity_removal_pass(program);
    ir_opt::position_pass::position_pass(env, program);
    ir_opt::global_memory_to_storage_buffer_pass::global_memory_to_storage_buffer_pass(
        program, host_info,
    );
    ir_opt::texture_pass::texture_pass(env, program, host_info);
    ir_opt::dead_code_elimination::dead_code_elimination_pass(program);
    if let Some(sph) = sph {
        ir_opt::collect_info::collect_shader_info_pass_with_sph(program, sph);
    } else {
        ir_opt::collect_info::collect_shader_info_pass(program);
    }
    ir_opt::layer_pass::layer_pass(program, host_info);
    ir_opt::vendor_workaround_pass::vendor_workaround_pass(program);
}

fn optimize_glsl_with_optional_ir_dump(
    program: &mut Program,
    stage: ShaderStage,
    base_offset: u32,
    code: &[u64],
    texture_bound_buffer: Option<u32>,
    sph: Option<&ProgramHeader>,
    host_info: &crate::host_translate_info::HostTranslateInfo,
) {
    let dump = ShaderIrDumpConfig::from_env(stage, base_offset, code);
    if let Some(dump) = dump.as_ref() {
        dump.write(program, "00_translated");
    }

    ir_opt::ssa_rewrite_pass::ssa_rewrite_pass(program);
    if let Some(dump) = dump.as_ref() {
        dump.write(program, "01_ssa_rewrite");
    }
    ir_opt::identity_removal::identity_removal_pass(program);
    if let Some(dump) = dump.as_ref() {
        dump.write(program, "02_identity_removal_1");
    }
    ir_opt::constant_propagation::constant_propagation_pass(program);
    if let Some(dump) = dump.as_ref() {
        dump.write(program, "03_constant_propagation");
    }
    ir_opt::identity_removal::identity_removal_pass(program);
    if let Some(dump) = dump.as_ref() {
        dump.write(program, "04_identity_removal_2");
    }
    ir_opt::global_memory_to_storage_buffer_pass::global_memory_to_storage_buffer_pass(
        program, host_info,
    );
    if let Some(dump) = dump.as_ref() {
        dump.write(program, "05_global_memory_to_storage_buffer");
    }
    if let Some(texture_bound_buffer) = texture_bound_buffer {
        ir_opt::texture_pass::texture_pass_bound_textures(program, texture_bound_buffer);
        if let Some(dump) = dump.as_ref() {
            dump.write(program, "06_texture_pass");
        }
    }
    let code_hash = hash_code(code);
    if shader_hash_env_matches("RUZU_SHADER_SKIP_DCE_HASH", code_hash) {
        log::warn!(
            "[SHADER_SKIP_DCE] stage={:?} base=0x{:X} hash=0x{:016X}",
            stage,
            base_offset,
            code_hash
        );
        if let Some(dump) = dump.as_ref() {
            dump.write(program, "07_dead_code_elimination_skipped");
        }
    } else {
        ir_opt::dead_code_elimination::dead_code_elimination_pass(program);
        if let Some(dump) = dump.as_ref() {
            dump.write(program, "07_dead_code_elimination");
        }
    }
    if let Some(sph) = sph {
        ir_opt::collect_info::collect_shader_info_pass_with_sph(program, sph);
    } else {
        ir_opt::collect_info::collect_shader_info_pass(program);
    }
    if let Some(dump) = dump.as_ref() {
        dump.write(program, "08_collect_shader_info");
    }
    ir_opt::layer_pass::layer_pass(program, host_info);
    if let Some(dump) = dump.as_ref() {
        dump.write(program, "09_layer_pass");
    }
    ir_opt::vendor_workaround_pass::vendor_workaround_pass(program);
    if let Some(dump) = dump.as_ref() {
        dump.write(program, "10_vendor_workaround_pass");
    }
}

struct ShaderIrDumpConfig {
    dir: std::path::PathBuf,
    stage: ShaderStage,
    base_offset: u32,
    code_hash: u64,
    words: usize,
    decoded_words: Option<String>,
}

impl ShaderIrDumpConfig {
    fn from_env(stage: ShaderStage, base_offset: u32, code: &[u64]) -> Option<Self> {
        let dir = std::env::var_os("RUZU_DUMP_SHADER_IR_DIR").map(std::path::PathBuf::from)?;
        let code_hash = hash_code(code);
        if let Ok(spec) = std::env::var("RUZU_DUMP_SHADER_IR_STAGE") {
            if !spec.split(',').any(|raw| {
                let value = raw.trim();
                value == "*" || value.eq_ignore_ascii_case(&format!("{stage:?}"))
            }) {
                return None;
            }
        }
        if let Ok(spec) = std::env::var("RUZU_DUMP_SHADER_IR_CODE_HASH") {
            if !spec.split(',').any(|raw| {
                let value = raw.trim();
                if value == "*" {
                    return true;
                }
                let hex = value
                    .strip_prefix("0x")
                    .or_else(|| value.strip_prefix("0X"))
                    .unwrap_or(value);
                u64::from_str_radix(hex, 16).is_ok_and(|target| target == code_hash)
            }) {
                return None;
            }
        }
        if let Ok(spec) = std::env::var("RUZU_DUMP_SHADER_IR_BASE") {
            if !spec.split(',').any(|raw| {
                let value = raw.trim();
                if value == "*" {
                    return true;
                }
                let hex = value
                    .strip_prefix("0x")
                    .or_else(|| value.strip_prefix("0X"))
                    .unwrap_or(value);
                u32::from_str_radix(hex, 16).is_ok_and(|target| target == base_offset)
            }) {
                return None;
            }
        }
        if let Err(err) = std::fs::create_dir_all(&dir) {
            log::warn!(
                "Failed to create RUZU_DUMP_SHADER_IR_DIR {}: {}",
                dir.display(),
                err
            );
            return None;
        }
        Some(Self {
            dir,
            stage,
            base_offset,
            code_hash,
            words: code.len(),
            decoded_words: std::env::var_os("RUZU_DUMP_SHADER_IR_WORDS")
                .map(|_| decoded_shader_words(stage, base_offset, code)),
        })
    }

    fn write(&self, program: &Program, phase: &str) {
        let path = self.dir.join(format!(
            "{phase}_{:?}_base_{:06X}_hash_{:016X}.txt",
            self.stage, self.base_offset, self.code_hash
        ));
        let mut out = String::new();
        out.push_str(&format!(
            "phase={phase}\nstage={:?}\nbase_offset=0x{:X}\ncode_hash=0x{:016X}\nwords={}\nblocks={}\nsyntax_nodes={}\npost_order={:?}\n\n",
            self.stage,
            self.base_offset,
            self.code_hash,
            self.words,
            program.blocks.len(),
            program.syntax_list.len(),
            program.post_order_blocks,
        ));
        out.push_str("syntax:\n");
        for (index, node) in program.syntax_list.iter().enumerate() {
            out.push_str(&format!("  {index:04}: {node:?}\n"));
        }
        if let Some(decoded_words) = &self.decoded_words {
            out.push_str("\nshader_words:\n");
            out.push_str(decoded_words);
        }
        out.push_str("\nopcode_counts:\n");
        for (name, count) in opcode_counts(program) {
            out.push_str(&format!("  {name}: {count}\n"));
        }
        out.push_str("\nblocks:\n");
        for (block_index, block) in program.blocks.iter().enumerate() {
            out.push_str(&format!(
                "block {block_index} order={} preds={:?} succs={:?} live_insts={}\n",
                block.order,
                block.imm_predecessors,
                block.imm_successors,
                block.indexed_iter().count(),
            ));
            for (inst_index, inst) in block.indexed_iter() {
                out.push_str(&format!(
                    "  {inst_index:04}: {} args={:?} phi={:?}\n",
                    inst.opcode.name(),
                    inst.args,
                    inst.phi_args,
                ));
            }
        }
        if let Err(err) = std::fs::write(&path, out) {
            log::warn!("Failed to write shader IR dump {}: {}", path.display(), err);
        } else {
            log::info!(
                "[SHADER_IR_DUMP] phase={} stage={:?} hash=0x{:016X} path={}",
                phase,
                self.stage,
                self.code_hash,
                path.display()
            );
        }
    }
}

fn decoded_shader_words(stage: ShaderStage, base_offset: u32, code: &[u64]) -> String {
    let mut out = String::new();
    for (index, &word) in code.iter().enumerate() {
        let abs = base_offset + (index as u32 * 8);
        let sched = is_sched_control_word(base_offset, index);
        let predicate = super::frontend::instruction::Instruction::new(word).pred();
        let opcode = super::frontend::maxwell_opcodes::decode_opcode(word)
            .map(|op| format!("{op:?}"))
            .unwrap_or_else(|| "UNKNOWN".to_string());
        out.push_str(&format!(
            "  index={index:04} abs=0x{abs:06X} sched={sched} opcode={opcode} pred={}{} word=0x{word:016X} stage={stage:?}\n",
            if predicate.negated { "!" } else { "" },
            predicate.index,
        ));
    }
    out
}

fn opcode_counts(program: &Program) -> BTreeMap<&'static str, usize> {
    let mut counts = BTreeMap::new();
    for block in &program.blocks {
        for (_, inst) in block.indexed_iter() {
            *counts.entry(inst.opcode.name()).or_insert(0) += 1;
        }
    }
    counts
}

fn shader_hash_env_matches(env_name: &str, code_hash: u64) -> bool {
    let Ok(spec) = std::env::var(env_name) else {
        return false;
    };
    spec.split(',').any(|raw| {
        let value = raw.trim();
        if value.is_empty() {
            return false;
        }
        if value == "*" {
            return true;
        }
        let hex = value
            .strip_prefix("0x")
            .or_else(|| value.strip_prefix("0X"))
            .unwrap_or(value);
        u64::from_str_radix(hex, 16).is_ok_and(|target| target == code_hash)
    })
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
    use crate::environment::Environment;
    use crate::frontend::control_flow::{CfgBlock, Condition, EndClass};
    use crate::ir::program::SyntaxNode;
    use crate::program_header::OutputTopology;
    use crate::shader_info::{ReplaceConstant, TexturePixelFormat, TextureType};

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
    fn test_compile_shader_glsl_uses_fragment_color_types() {
        let profile = Profile {
            need_declared_frag_colors: true,
            ..Profile::default()
        };
        let mut runtime_info = RuntimeInfo::default();
        runtime_info.frag_color_types[1] = crate::runtime_info::AttributeType::UnsignedInt;
        runtime_info.frag_color_types[2] = crate::runtime_info::AttributeType::SignedInt;

        let compiled = compile_shader_glsl(&[], ShaderStage::Fragment, &profile, &runtime_info);

        assert!(compiled
            .source
            .contains("layout(location=0)out vec4 frag_color0;"));
        assert!(compiled
            .source
            .contains("layout(location=1)out uvec4 frag_color1;"));
        assert!(compiled
            .source
            .contains("layout(location=2)out ivec4 frag_color2;"));
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
    fn cfg_translation_materializes_return_epilogue() {
        let cfg_blocks = vec![CfgBlock {
            begin: 0,
            end: 1,
            end_class: EndClass::Exit,
            branch_true: None,
            branch_false: None,
            cond: Condition::always(),
            stack_depth: 0,
        }];

        let program = translate_cfg_to_program(&[0], ShaderStage::VertexB, 0, &cfg_blocks, None);
        let return_block = program
            .syntax_list
            .windows(2)
            .find_map(|nodes| match (&nodes[0], &nodes[1]) {
                (SyntaxNode::Block(block), SyntaxNode::Return) => Some(*block),
                _ => None,
            })
            .expect("return syntax must have a preceding block");

        assert_eq!(program.block(return_block).front().opcode, Opcode::Epilogue);
    }

    #[test]
    fn cfg_translation_materializes_conditional_branch_predicate() {
        use crate::ir::opcodes::Opcode;

        let cfg_blocks = vec![
            CfgBlock {
                begin: 0,
                end: 1,
                end_class: EndClass::Branch,
                branch_true: Some(2),
                branch_false: Some(1),
                cond: Condition {
                    pred: 2,
                    negated: true,
                },
                stack_depth: 0,
            },
            CfgBlock {
                begin: 1,
                end: 2,
                end_class: EndClass::Branch,
                branch_true: Some(2),
                branch_false: None,
                cond: Condition::always(),
                stack_depth: 0,
            },
            CfgBlock {
                begin: 2,
                end: 3,
                end_class: EndClass::Return,
                branch_true: None,
                branch_false: None,
                cond: Condition::always(),
                stack_depth: 0,
            },
        ];

        let program = translate_cfg_to_program(
            &[0, 0, 0],
            ShaderStage::VertexB,
            0,
            cfg_blocks.as_slice(),
            None,
        );

        let cond = program
            .syntax_list
            .iter()
            .find_map(|node| match node {
                SyntaxNode::If { cond, .. } => Some(*cond),
                _ => None,
            })
            .expect("conditional branch should produce an If syntax node");
        let Value::Inst(cond_ref) = cond else {
            panic!("If condition must be an IR value, got {cond:?}");
        };
        let cond_inst = program.block(cond_ref.block).inst(cond_ref.inst);
        assert_eq!(cond_inst.opcode, Opcode::ConditionRef);
        let Value::Inst(not_ref) = cond_inst.args[0] else {
            panic!("negated predicate should feed ConditionRef through LogicalNot");
        };
        assert_eq!(
            program.block(not_ref.block).inst(not_ref.inst).opcode,
            Opcode::LogicalNot
        );
    }

    #[test]
    fn clear_unreachable_blocks_preserves_indices_but_drops_stale_instructions() {
        use crate::ir::basic_block::Block;
        use crate::ir::instruction::Inst;
        use crate::ir::opcodes::Opcode;
        use crate::ir::value::{InstRef, Reg, Value};

        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.blocks.push(Block::new());
        let cond = program
            .block_mut(1)
            .append_inst(Inst::new(Opcode::GetRegister, vec![Value::Reg(Reg(3))]));
        program.syntax_list.push(SyntaxNode::If {
            cond: Value::Inst(InstRef {
                block: 1,
                inst: cond,
            }),
            body: 1,
            merge: 0,
        });
        program.post_order_blocks = vec![0];

        clear_unreachable_blocks(&mut program);

        assert_eq!(program.blocks.len(), 2);
        assert!(program.block(1).is_empty());
        assert_eq!(program.post_order_blocks, vec![0]);
        let SyntaxNode::If { cond, .. } = program.syntax_list[0] else {
            panic!("expected If syntax node");
        };
        assert_eq!(cond, Value::ImmU1(false));
    }

    #[test]
    fn sched_control_skip_uses_absolute_shader_offset() {
        assert!(is_sched_control_word(0, 0));
        assert!(!is_sched_control_word(0, 1));
        assert!(is_sched_control_word(8, 3));
        assert!(!is_sched_control_word(8, 0));
        assert!(is_sched_control_word(0x50, 2));
    }

    struct DummyEnvironment {
        sph: ProgramHeader,
    }

    impl DummyEnvironment {
        fn compute() -> Self {
            let mut sph = ProgramHeader::default();
            sph.raw[3] = (OutputTopology::TriangleStrip as u32) << 24;
            Self { sph }
        }
    }

    impl Environment for DummyEnvironment {
        fn read_instruction(&mut self, _address: u32) -> u64 {
            0
        }

        fn read_cbuf_value(&mut self, _cbuf_index: u32, _cbuf_offset: u32) -> u32 {
            0
        }

        fn read_texture_type(&mut self, _raw_handle: u32) -> TextureType {
            TextureType::Color2D
        }

        fn read_texture_pixel_format(&mut self, _raw_handle: u32) -> TexturePixelFormat {
            TexturePixelFormat::A8B8G8R8Unorm
        }

        fn is_texture_pixel_format_integer(&mut self, _raw_handle: u32) -> bool {
            false
        }

        fn read_viewport_transform_state(&mut self) -> u32 {
            1
        }

        fn texture_bound_buffer(&self) -> u32 {
            7
        }

        fn local_memory_size(&self) -> u32 {
            0x240
        }

        fn shared_memory_size(&self) -> u32 {
            0x180
        }

        fn workgroup_size(&self) -> [u32; 3] {
            [8, 4, 2]
        }

        fn has_hle_macro_state(&self) -> bool {
            false
        }

        fn get_replace_const_buffer(
            &mut self,
            _bank: u32,
            _offset: u32,
        ) -> Option<ReplaceConstant> {
            None
        }

        fn dump(&mut self, _pipeline_hash: u64, _shader_hash: u64) {}

        fn sph(&self) -> &ProgramHeader {
            &self.sph
        }

        fn gp_passthrough_mask(&self) -> &[u32; 8] {
            static MASK: [u32; 8] = [0; 8];
            &MASK
        }

        fn shader_stage(&self) -> ShaderStage {
            ShaderStage::Compute
        }

        fn start_address(&self) -> u32 {
            0
        }

        fn is_proprietary_driver(&self) -> bool {
            false
        }
    }

    #[test]
    fn translate_program_from_env_uses_environment_metadata() {
        let mut env = DummyEnvironment::compute();

        let program = translate_program_from_env(&[0, 0], 0, &mut env);

        assert_eq!(program.stage, ShaderStage::Compute);
        assert_eq!(program.local_memory_size, 0x240);
        assert_eq!(program.shared_memory_size, 0x180);
        assert_eq!(program.workgroup_size, [8, 4, 2]);
        assert!(!program.blocks.is_empty());
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

    #[test]
    fn pipeline_cache_keys_runtime_info_that_affects_emission() {
        let mut cache = PipelineCache::with_default_profile();
        let code: Vec<u64> = vec![0x0000_0000_0000_0000];

        let default_runtime = RuntimeInfo::default();
        let mut converted_depth_runtime = RuntimeInfo::default();
        converted_depth_runtime.convert_depth_mode = true;

        let _ = cache.get_or_compile(&code, ShaderStage::VertexB, &default_runtime);
        assert_eq!(cache.len(), 1);

        let _ = cache.get_or_compile(&code, ShaderStage::VertexB, &converted_depth_runtime);
        assert_eq!(cache.len(), 2);
    }
}
