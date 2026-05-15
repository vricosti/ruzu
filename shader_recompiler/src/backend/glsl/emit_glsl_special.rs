// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL special operation emission (phi, void, prologue/epilogue).
//!
//! Maps to upstream `backend/glsl/emit_glsl_special.cpp`.

use super::glsl_emit_context::EmitContext;

fn output_vertex_index(ctx: &EmitContext) -> &'static str {
    if ctx.stage == crate::stage::Stage::TessellationControl {
        "[gl_InvocationID]"
    } else {
        ""
    }
}

fn initialize_output_varyings(ctx: &mut EmitContext) {
    if ctx.uses_geometry_passthrough {
        return;
    }
    if matches!(
        ctx.stage,
        crate::stage::Stage::VertexB | crate::stage::Stage::Geometry
    ) {
        ctx.add_line("gl_Position=vec4(0,0,0,1);");
    }
    let output_decorator = output_vertex_index(ctx);
    for index in 0..ctx.output_generics.len() {
        let mut element = 0usize;
        while element < 4 {
            let info = ctx.output_generics[index][element].clone();
            if info.name.is_empty() || info.num_components == 0 {
                element += 1;
                continue;
            }
            let varying_name = format!("{}{}", info.name, output_decorator);
            match info.num_components {
                1 => {
                    let value = if element == 3 { '1' } else { '0' };
                    ctx.add_fmt(format!("{}={}.f;", varying_name, value));
                }
                2 | 3 => {
                    if element + (info.num_components as usize) < 4 {
                        ctx.add_fmt(format!("{}=vec{}(0);", varying_name, info.num_components));
                    } else {
                        let zeros = if info.num_components == 3 {
                            "0,0,"
                        } else {
                            "0,"
                        };
                        ctx.add_fmt(format!(
                            "{}=vec{}({}1);",
                            varying_name, info.num_components, zeros
                        ));
                    }
                }
                4 => ctx.add_fmt(format!("{}=vec4(0,0,0,1);", varying_name)),
                _ => {}
            }
            element += info.num_components as usize;
        }
    }
}

pub fn emit_phi(_ctx: &mut EmitContext) {}
pub fn emit_void(_ctx: &mut EmitContext) {}
pub fn emit_reference(_ctx: &mut EmitContext) {}
pub fn emit_phi_move(ctx: &mut EmitContext) {
    ctx.add_line("// phi move");
}
pub fn emit_prologue(ctx: &mut EmitContext) {
    initialize_output_varyings(ctx);
}
pub fn emit_epilogue(_ctx: &mut EmitContext) {}
pub fn emit_emit_vertex(ctx: &mut EmitContext) {
    ctx.add_line("EmitVertex();");
    initialize_output_varyings(ctx);
}
pub fn emit_end_primitive(ctx: &mut EmitContext) {
    ctx.add_line("EndPrimitive();");
}
