// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! IR Program — the top-level container for a translated shader.
//!
//! Matches zuyu's `Program` from `program.h`. Contains basic blocks,
//! a structured control flow AST, shader metadata, and stage info.

use super::basic_block::Block;
use super::types::{OutputTopology, ShaderStage};
use super::value::Value;

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
    If {
        cond: Value,
        body: u32,
        merge: u32,
    },
    /// End of an if-then region.
    EndIf {
        merge: u32,
    },
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
    Break {
        cond: Value,
        merge: u32,
        skip: u32,
    },
    /// Return from the shader.
    Return,
    /// Unreachable code marker.
    Unreachable,
}

/// Constant buffer descriptor used by the shader.
#[derive(Debug, Clone)]
pub struct CbufDescriptor {
    /// Constant buffer index (0..17).
    pub index: u32,
    /// Byte size accessed.
    pub size: u32,
}

/// Texture descriptor used by the shader.
#[derive(Debug, Clone)]
pub struct TexDescriptor {
    /// Descriptor index in the texture pool.
    pub index: u32,
    /// Texture type (1D, 2D, 3D, etc.).
    pub texture_type: u8,
    /// Whether this is a depth texture.
    pub is_depth: bool,
}

/// Storage buffer descriptor.
#[derive(Debug, Clone)]
pub struct StorageDescriptor {
    /// Constant buffer index where the SSBO address is stored.
    pub cbuf_index: u32,
    /// Byte offset within the constant buffer.
    pub cbuf_offset: u32,
    /// Whether writes to this buffer are observed.
    pub is_written: bool,
}

/// Shader metadata collected during translation.
///
/// Matches zuyu's `Info` / `ShaderInfo` from `shader_info.h`.
#[derive(Debug, Clone, Default)]
pub struct ShaderInfo {
    /// Constant buffer descriptors referenced by the shader.
    pub constant_buffer_descriptors: Vec<CbufDescriptor>,
    /// Texture descriptors referenced by the shader.
    pub texture_descriptors: Vec<TexDescriptor>,
    /// Storage buffer descriptors referenced by the shader.
    pub storage_buffer_descriptors: Vec<StorageDescriptor>,
    /// Bitmask of which generic input attributes are used (bits 0..31).
    pub loads_generics: u32,
    /// Bitmask of which generic output attributes are written (bits 0..31).
    pub stores_generics: u32,
    /// Whether the shader reads vertex ID.
    pub loads_vertex_id: bool,
    /// Whether the shader reads instance ID.
    pub loads_instance_id: bool,
    /// Whether the shader reads front-facing.
    pub loads_front_face: bool,
    /// Whether the shader reads point coord.
    pub loads_point_coord: bool,
    /// Whether the shader reads position (fragment).
    pub loads_position: bool,
    /// Whether the shader writes position (vertex).
    pub stores_position: bool,
    /// Whether the shader writes point size.
    pub stores_point_size: bool,
    /// Whether the shader writes clip distances.
    pub stores_clip_distance: bool,
    /// Whether the shader writes layer.
    pub stores_layer: bool,
    /// Whether the shader writes viewport index.
    pub stores_viewport_index: bool,
    /// Whether the shader writes frag depth.
    pub stores_frag_depth: bool,
    /// Whether the shader writes sample mask.
    pub stores_sample_mask: bool,
    /// Whether the shader uses discard/kill.
    pub uses_demote: bool,
    /// Local memory size in bytes.
    pub local_memory_size: u32,
    /// Shared memory size in bytes.
    pub shared_memory_size: u32,
}

impl ShaderInfo {
    /// Register use of a constant buffer.
    pub fn register_cbuf(&mut self, index: u32) {
        if !self.constant_buffer_descriptors.iter().any(|d| d.index == index) {
            self.constant_buffer_descriptors.push(CbufDescriptor { index, size: 0x10000 });
        }
    }

    /// Register use of a texture.
    pub fn register_texture(&mut self, index: u32, texture_type: u8, is_depth: bool) {
        if !self.texture_descriptors.iter().any(|d| d.index == index) {
            self.texture_descriptors.push(TexDescriptor {
                index,
                texture_type,
                is_depth,
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
