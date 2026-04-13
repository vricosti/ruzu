// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! IR Program — the top-level container for a translated shader.
//!
//! Matches zuyu's `Program` from `program.h`. Contains basic blocks,
//! a structured control flow AST, shader metadata, and stage info.

use super::basic_block::Block;
use super::types::{OutputTopology, ShaderStage};
use super::value::Value;

use crate::shader_info;

/// Abstract syntax tree node for structured control flow.
///
/// Matches zuyu's `AbstractSyntaxNode` from `abstract_syntax_list.h`.
/// The structured CF pass converts flat CFG to this nested representation,
/// which SPIR-V emission can directly consume.
#[derive(Debug, Clone)]
pub enum SyntaxNode {
    /// A basic block to execute.
    Block(u32),
    /// If-then: condition, body block, merge block.
    If { cond: Value, body: u32, merge: u32 },
    /// End of an if-then region.
    EndIf { merge: u32 },
    /// Loop: body block, continue block, merge (break target) block.
    Loop {
        body: u32,
        continue_block: u32,
        merge: u32,
    },
    /// Repeat (back-edge of a loop): condition, loop header, merge block.
    Repeat {
        cond: Value,
        loop_header: u32,
        merge: u32,
    },
    /// Break out of a loop: condition, merge (loop merge) block, skip block.
    Break { cond: Value, merge: u32, skip: u32 },
    /// Return from the shader.
    Return,
    /// Unreachable code marker.
    Unreachable,
}

/// Shader metadata collected during translation.
///
/// Now a type alias for the upstream-faithful `shader_info::Info`
/// struct. The previous simplified `ShaderInfo` with separate
/// descriptor types and boolean fields was deleted as part of the
/// cross-crate type-unification pass. The upstream `Info` carries all
/// the same data via `VaryingState` bitmasks and the full descriptor
/// types (`ConstantBufferDescriptor`, `TextureDescriptor`, etc.).
///
/// Convenience methods (`register_cbuf`, `register_texture`) that the
/// IR emitter depends on are provided as a trait impl below.
pub type ShaderInfo = shader_info::Info;

/// Re-export the upstream descriptor types under the names the rest of
/// the recompiler already expects. Code that used the old simplified
/// `CbufDescriptor` / `TexDescriptor` / `StorageDescriptor` should
/// migrate to these names.
pub type CbufDescriptor = shader_info::ConstantBufferDescriptor;
pub type TexDescriptor = shader_info::TextureDescriptor;
pub type StorageDescriptor = shader_info::StorageBufferDescriptor;

/// Convenience helpers on `ShaderInfo` (= `shader_info::Info`) used
/// by the IR emitter to register resource accesses incrementally.
pub trait ShaderInfoExt {
    fn register_cbuf(&mut self, index: u32);
    fn register_texture(
        &mut self,
        index: u32,
        texture_type: shader_info::TextureType,
        is_depth: bool,
    );
}

impl ShaderInfoExt for ShaderInfo {
    fn register_cbuf(&mut self, index: u32) {
        if !self
            .constant_buffer_descriptors
            .iter()
            .any(|d| d.index == index)
        {
            self.constant_buffer_descriptors
                .push(shader_info::ConstantBufferDescriptor { index, count: 1 });
        }
    }

    fn register_texture(
        &mut self,
        index: u32,
        texture_type: shader_info::TextureType,
        is_depth: bool,
    ) {
        if !self
            .texture_descriptors
            .iter()
            .any(|d| d.cbuf_index == index)
        {
            self.texture_descriptors
                .push(shader_info::TextureDescriptor {
                    texture_type,
                    is_depth,
                    is_multisample: false,
                    has_secondary: false,
                    cbuf_index: index,
                    cbuf_offset: 0,
                    shift_left: 0,
                    secondary_cbuf_index: 0,
                    secondary_cbuf_offset: 0,
                    secondary_shift_left: 0,
                    count: 1,
                    size_shift: 0,
                });
        }
    }
}

/// A complete translated shader program.
#[derive(Debug, Clone)]
pub struct Program {
    /// Structured control flow AST.
    pub syntax_list: Vec<SyntaxNode>,
    /// All basic blocks.
    pub blocks: Vec<Block>,
    /// Post-order traversal of blocks (for optimization passes).
    pub post_order_blocks: Vec<u32>,
    /// Shader metadata collected during translation.
    pub info: ShaderInfo,
    /// Shader stage.
    pub stage: ShaderStage,
    /// Local memory size in bytes.
    /// Upstream: `IR::Program::local_memory_size`.
    pub local_memory_size: u32,
    /// Shared memory size in bytes (compute shaders).
    /// Upstream: `IR::Program::shared_memory_size`.
    pub shared_memory_size: u32,
    /// Workgroup size (compute shaders).
    pub workgroup_size: [u32; 3],
    /// Output topology (geometry shaders).
    pub output_topology: OutputTopology,
    /// Number of output vertices (geometry shaders).
    pub output_vertices: u32,
    /// Number of invocations (geometry shaders).
    pub invocations: u32,
    /// Whether this is a passthrough geometry shader.
    pub is_geometry_passthrough: bool,
}

impl Program {
    /// Create a new empty program for the given stage.
    pub fn new(stage: ShaderStage) -> Self {
        Self {
            syntax_list: Vec::new(),
            blocks: Vec::new(),
            post_order_blocks: Vec::new(),
            info: ShaderInfo::default(),
            stage,
            local_memory_size: 0,
            shared_memory_size: 0,
            workgroup_size: [1, 1, 1],
            output_topology: OutputTopology::TriangleStrip,
            output_vertices: 0,
            invocations: 1,
            is_geometry_passthrough: false,
        }
    }

    /// Add a new block and return its index.
    pub fn add_block(&mut self) -> u32 {
        let idx = self.blocks.len() as u32;
        self.blocks.push(Block::new());
        idx
    }

    /// Get a block by index.
    pub fn block(&self, idx: u32) -> &Block {
        &self.blocks[idx as usize]
    }

    /// Get a mutable block by index.
    pub fn block_mut(&mut self, idx: u32) -> &mut Block {
        &mut self.blocks[idx as usize]
    }

    /// Number of blocks.
    pub fn num_blocks(&self) -> usize {
        self.blocks.len()
    }
}
