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

use super::var_alloc::{glsl_type_str, GlslVarType, VarAlloc};

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
            ir::types::ShaderStage::VertexB => {
                ctx.stage_name = "vs";
            }
            ir::types::ShaderStage::TessellationControl => {
                ctx.stage_name = "tcs";
            }
            ir::types::ShaderStage::TessellationEval => {
                ctx.stage_name = "tes";
            }
            ir::types::ShaderStage::Geometry => {
                ctx.stage_name = "gs";
            }
            ir::types::ShaderStage::Fragment => {
                ctx.stage_name = "fs";
                ctx.position_name = "gl_FragCoord";
            }
            ir::types::ShaderStage::Compute => {
                ctx.stage_name = "cs";
            }
            ir::types::ShaderStage::VertexA => {
                unreachable!("VertexA must be merged into VertexB before GLSL emission");
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
            ctx.header.push_str(&format!(
                "layout(binding={})uniform sampler2D {}_tex{};\n",
                bindings.texture, ctx.stage_name, _desc.cbuf_index
            ));
            bindings.texture += 1;
        }

        ctx.define_generic_inputs(program);
        ctx.define_generic_outputs(program);
        ctx.define_fragment_outputs(program);
        ctx.define_constant_buffers(bindings, program);
        ctx.define_helper_functions();

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

    pub fn is_input_array(&self) -> bool {
        matches!(
            self.stage,
            Stage::Geometry | Stage::TessellationControl | Stage::TessellationEval
        )
    }

    fn input_array_decorator(&self) -> &'static str {
        if self.is_input_array() {
            "[]"
        } else {
            ""
        }
    }

    fn output_decorator(&self, invocations: u32) -> String {
        if self.stage == Stage::TessellationControl {
            format!("[{}]", invocations)
        } else {
            String::new()
        }
    }

    fn define_generic_inputs(&mut self, program: &ir::Program) {
        for index in 0..32usize {
            if !program.info.loads.generic_any(index)
                || !self.runtime_info.previous_stage_stores.generic_any(index)
            {
                continue;
            }
            self.header.push_str(&format!(
                "layout(location={})in vec4 in_attr{}{};\n",
                index,
                index,
                self.input_array_decorator()
            ));
        }
    }

    fn define_generic_outputs(&mut self, program: &ir::Program) {
        for index in 0..32usize {
            if program.info.stores.generic_any(index) {
                self.define_generic_output(index, program.invocations);
            }
        }
    }

    fn define_generic_output(&mut self, index: usize, invocations: u32) {
        let num_components = 4u32;
        let name = format!("out_attr{}", index);
        self.header.push_str(&format!(
            "layout(location={})out vec4 {}{};\n",
            index,
            name,
            self.output_decorator(invocations)
        ));
        let info = GenericElementInfo {
            name,
            first_element: 0,
            num_components,
        };
        for component in 0..4usize {
            self.output_generics[index][component] = info.clone();
        }
    }

    fn define_fragment_outputs(&mut self, program: &ir::Program) {
        if self.stage != Stage::Fragment {
            return;
        }
        for (render_target, &enabled) in program.info.stores_frag_color.iter().enumerate() {
            if enabled || self.profile.need_declared_frag_colors {
                self.header.push_str(&format!(
                    "layout(location={})out vec4 frag_color{};\n",
                    render_target, render_target
                ));
            }
        }
    }

    fn define_constant_buffers(&mut self, bindings: &mut Bindings, program: &ir::Program) {
        for desc in &program.info.constant_buffer_descriptors {
            let cbuf_type = if self.profile.has_gl_cbuf_ftou_bug {
                "uvec4"
            } else {
                "vec4"
            };
            let used_size = program.info.constant_buffer_used_sizes[desc.index as usize];
            let cbuf_used_size = used_size.max(16).div_ceil(16);
            let cbuf_binding_size = if program.info.uses_global_memory {
                0x1000
            } else {
                cbuf_used_size
            };
            self.header.push_str(&format!(
                "layout(std140,binding={}) uniform {}_cbuf_{}{{{} {}_cbuf{}[{}];}};\n",
                bindings.uniform_buffer,
                self.stage_name,
                desc.index,
                cbuf_type,
                self.stage_name,
                desc.index,
                cbuf_binding_size
            ));
            bindings.uniform_buffer += desc.count;
        }
    }

    fn define_helper_functions(&mut self) {
        self.header.push_str(
            "\n#define ftoi floatBitsToInt\n#define ftou floatBitsToUint\n#define itof intBitsToFloat\n#define utof uintBitsToFloat\n",
        );
    }

    pub fn define_variables(&self, header: &mut String) {
        for var_type in [
            GlslVarType::U1,
            GlslVarType::F16x2,
            GlslVarType::U32,
            GlslVarType::F32,
            GlslVarType::U64,
            GlslVarType::F64,
            GlslVarType::U32x2,
            GlslVarType::F32x2,
            GlslVarType::U32x3,
            GlslVarType::F32x3,
            GlslVarType::U32x4,
            GlslVarType::F32x4,
            GlslVarType::PrecF32,
            GlslVarType::PrecF64,
        ] {
            let tracker = self.var_alloc.get_use_tracker(var_type);
            let type_name = glsl_type_str(var_type);
            if tracker.uses_temp {
                header.push_str(&format!(
                    "{} t{}={}(0);\n",
                    type_name,
                    self.var_alloc.representation_indexed(0, var_type),
                    type_name
                ));
            }
            for index in 0..tracker.num_used {
                header.push_str(&format!(
                    "{} {}={}(0);\n",
                    type_name,
                    self.var_alloc
                        .representation_indexed(index as u32, var_type),
                    type_name
                ));
            }
        }
        for index in 0..self.num_safety_loop_vars {
            header.push_str(&format!("int loop{}=0x2000;\n", index));
        }
    }
}
