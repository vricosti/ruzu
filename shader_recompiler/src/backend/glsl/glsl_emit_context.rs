// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL emit context.
//!
//! Maps to upstream `backend/glsl/glsl_emit_context.h` and
//! `glsl_emit_context.cpp`.

use crate::backend::bindings::Bindings;
use crate::ir;
use crate::profile::Profile;
use crate::runtime_info::RuntimeInfo;
use crate::stage::Stage;

use super::var_alloc::VarAlloc;

/// Per-generic-output element info.
#[derive(Debug, Clone, Default)]
pub struct GenericElementInfo {
    pub name: String,
    pub first_element: u32,
    pub num_components: u32,
}

/// Texture/image definition with binding index and count.
#[derive(Debug, Clone, Copy)]
pub struct TextureImageDefinition {
    pub binding: u32,
    pub count: u32,
}

/// GLSL emission context.
pub struct EmitContext<'a> {
    pub header: String,
    pub code: String,
    pub var_alloc: VarAlloc,
    pub profile: &'a Profile,
    pub runtime_info: &'a RuntimeInfo,
    pub stage: Stage,
    pub stage_name: &'static str,
    pub position_name: &'static str,
    pub texture_buffers: Vec<TextureImageDefinition>,
    pub image_buffers: Vec<TextureImageDefinition>,
    pub textures: Vec<TextureImageDefinition>,
    pub images: Vec<TextureImageDefinition>,
    pub output_generics: [[GenericElementInfo; 4]; 32],
    pub num_safety_loop_vars: u32,
    pub uses_y_direction: bool,
    pub uses_cc_carry: bool,
    pub uses_geometry_passthrough: bool,
}

impl<'a> EmitContext<'a> {
    pub fn new(
        program: &ir::Program,
        bindings: &mut Bindings,
        profile: &'a Profile,
        runtime_info: &'a RuntimeInfo,
    ) -> Self {
        let mut ctx = Self {
            header: String::new(),
            code: String::new(),
            var_alloc: VarAlloc::new(),
            profile,
            runtime_info,
            stage: program.stage.into(),
            stage_name: "invalid",
            position_name: "gl_Position",
            texture_buffers: Vec::new(),
            image_buffers: Vec::new(),
            textures: Vec::new(),
            images: Vec::new(),
            output_generics: Default::default(),
            num_safety_loop_vars: 0,
            uses_y_direction: false,
            uses_cc_carry: false,
            uses_geometry_passthrough: false,
        };

        // Set GLSL version header
        ctx.header.push_str("#version 460 core\n");

        match program.stage {
            ir::types::ShaderStage::Vertex => {
                ctx.stage_name = "vertex";
            }
            ir::types::ShaderStage::TessellationControl => {
                ctx.stage_name = "tess_control";
            }
            ir::types::ShaderStage::TessellationEval => {
                ctx.stage_name = "tess_eval";
            }
            ir::types::ShaderStage::Geometry => {
                ctx.stage_name = "geometry";
            }
            ir::types::ShaderStage::Fragment => {
                ctx.stage_name = "fragment";
            }
            ir::types::ShaderStage::Compute => {
                ctx.stage_name = "compute";
            }
        }

        // Set up texture bindings
        // Note: upstream has texture_buffer_descriptors, image_buffer_descriptors, and
        // image_descriptors on ShaderInfo, but those are not yet ported.
        // For now we bind texture_descriptors with one binding each.
        for _desc in &program.info.texture_descriptors {
            ctx.textures.push(TextureImageDefinition {
                binding: bindings.texture,
                count: 1,
            });
            bindings.texture += 1;
        }

        ctx
    }

    /// Append a line of GLSL code.
    pub fn add_line(&mut self, line: &str) {
        self.code.push_str(line);
        self.code.push('\n');
    }

    /// Append formatted text to the code followed by a newline.
    pub fn add_fmt(&mut self, text: String) {
        self.code.push_str(&text);
        self.code.push('\n');
    }

    /// Append a line to the header.
    pub fn add_header(&mut self, line: &str) {
        self.header.push_str(line);
        self.header.push('\n');
    }
}
