// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL special operation emission (phi, void, prologue/epilogue).
//!
//! Maps to upstream `backend/glsl/emit_glsl_special.cpp`.

use super::glsl_emit_context::EmitContext;
use crate::ir;
use crate::ir::instruction::Inst;
use crate::runtime_info::CompareFunction;

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
pub fn emit_fragment_alpha_test(ctx: &mut EmitContext) {
    let Some(func) = ctx.runtime_info.alpha_test_func else {
        return;
    };
    if func == CompareFunction::Always {
        return;
    }
    // RUZU_DISABLE_ALPHA_TEST=1 — diagnostic kill-switch: skip the emulated
    // alpha-test discard entirely (MK8D black-3D investigation).
    if std::env::var_os("RUZU_DISABLE_ALPHA_TEST").is_some() {
        return;
    }
    let op = match func {
        CompareFunction::Never => None,
        CompareFunction::Less => Some("<"),
        CompareFunction::Equal => Some("=="),
        CompareFunction::LessThanEqual => Some("<="),
        CompareFunction::Greater => Some(">"),
        CompareFunction::NotEqual => Some("!="),
        CompareFunction::GreaterThanEqual => Some(">="),
        CompareFunction::Always => unreachable!(),
    };
    if let Some(op) = op {
        ctx.add_fmt(format!(
            "if(!(frag_color0.a{}{}f)){{discard;}}",
            op, ctx.runtime_info.alpha_test_reference
        ));
    } else {
        ctx.add_line("discard;");
    }
}
pub fn emit_emit_vertex(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    let stream = ctx.var_alloc.consume(program, &inst.args[0]);
    ctx.add_fmt(format!("EmitStreamVertex(int({}));", stream));
    initialize_output_varyings(ctx);
}
pub fn emit_end_primitive(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    let stream = ctx.var_alloc.consume(program, &inst.args[0]);
    ctx.add_fmt(format!("EndStreamPrimitive(int({}));", stream));
}
