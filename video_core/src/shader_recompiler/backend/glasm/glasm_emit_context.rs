// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM emit context.
//!
//! Maps to upstream `backend/glasm/glasm_emit_context.h` and
//! `glasm_emit_context.cpp`.

use crate::shader_recompiler::backend::bindings::Bindings;
use crate::shader_recompiler::ir;
use crate::shader_recompiler::profile::Profile;
use crate::shader_recompiler::runtime_info::RuntimeInfo;
use crate::shader_recompiler::stage::Stage;

use super::reg_alloc::RegAlloc;

/// GLASM emission context.
///
/// Accumulates NV_gpu_program assembly text and manages register allocation.
pub struct EmitContext<'a> {
    pub code: String,
    pub reg_alloc: RegAlloc,
    pub profile: &'a Profile,
    pub runtime_info: &'a RuntimeInfo,
    pub stage: Stage,
    pub stage_name: &'static str,
    pub attrib_name: &'static str,
    pub texture_buffer_bindings: Vec<u32>,
    pub image_buffer_bindings: Vec<u32>,
    pub texture_bindings: Vec<u32>,
    pub image_bindings: Vec<u32>,
    pub num_safety_loop_vars: u32,
    pub uses_y_direction: bool,
}

impl<'a> EmitContext<'a> {
    /// Create a new GLASM emit context from a program and bindings.
    ///
    /// Matches upstream `EmitContext::EmitContext(...)`.
    pub fn new(
        program: &ir::Program,
        bindings: &mut Bindings,
        profile: &'a Profile,
        runtime_info: &'a RuntimeInfo,
    ) -> Self {
        let mut ctx = Self {
            code: String::new(),
            reg_alloc: RegAlloc::new(),
            profile,
            runtime_info,
            stage: program.stage.into(),
            stage_name: "invalid",
            attrib_name: "invalid",
            texture_buffer_bindings: Vec::new(),
            image_buffer_bindings: Vec::new(),
            texture_bindings: Vec::new(),
            image_bindings: Vec::new(),
            num_safety_loop_vars: 0,
            uses_y_direction: false,
        };

        // Set stage names matching upstream
        match program.stage {
            ir::types::ShaderStage::Vertex => {
                ctx.stage_name = "vertex";
                ctx.attrib_name = "vertex";
            }
            ir::types::ShaderStage::TessellationControl => {
                ctx.stage_name = "primitive";
                ctx.attrib_name = "primitive";
            }
            ir::types::ShaderStage::TessellationEval => {
                ctx.stage_name = "primitive";
                ctx.attrib_name = "primitive";
            }
            ir::types::ShaderStage::Geometry => {
                ctx.stage_name = "primitive";
                ctx.attrib_name = "vertex";
            }
            ir::types::ShaderStage::Fragment => {
                ctx.stage_name = "fragment";
                ctx.attrib_name = "fragment";
            }
            ir::types::ShaderStage::Compute => {
                ctx.stage_name = "invocation";
                ctx.attrib_name = "invocation";
            }
        }

        // Emit constant buffer declarations
        for desc in &program.info.constant_buffer_descriptors {
            ctx.add_line(&format!(
                "CBUFFER c{}[]={{program.buffer[{}]}};",
                desc.index, desc.index
            ));
        }

        // Emit fragment output declarations
        if matches!(program.stage, ir::types::ShaderStage::Fragment) {
            ctx.add_line("OUTPUT frag_color0=result.color;");
        }

        // Set up texture bindings
        // Note: upstream has texture_buffer_descriptors, image_buffer_descriptors, and
        // image_descriptors on ShaderInfo, but those are not yet ported.
        // For now we bind texture_descriptors with one binding each.
        for _desc in &program.info.texture_descriptors {
            ctx.texture_bindings.push(bindings.texture);
            bindings.texture += 1;
        }

        ctx
    }

    /// Append a line of GLASM assembly to the output.
    pub fn add_line(&mut self, line: &str) {
        self.code.push_str(line);
        self.code.push('\n');
    }

    /// Append formatted text to the output followed by a newline.
    pub fn add_fmt(&mut self, text: String) {
        self.code.push_str(&text);
        self.code.push('\n');
    }
}
