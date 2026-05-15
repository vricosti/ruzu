// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL backend: emit GLSL source from IR.
//!
//! Maps to upstream `backend/glsl/`.
//!
//! Entry point is [`emit_glsl()`] which takes an IR program and returns
//! a GLSL source string.

pub mod emit_glsl;
pub mod emit_glsl_atomic;
pub mod emit_glsl_barriers;
pub mod emit_glsl_bitwise_conversion;
pub mod emit_glsl_composite;
pub mod emit_glsl_context_get_set;
pub mod emit_glsl_control_flow;
pub mod emit_glsl_convert;
pub mod emit_glsl_floating_point;
pub mod emit_glsl_image;
pub mod emit_glsl_integer;
pub mod emit_glsl_logical;
pub mod emit_glsl_memory;
pub mod emit_glsl_not_implemented;
pub mod emit_glsl_select;
pub mod emit_glsl_shared_memory;
pub mod emit_glsl_special;
pub mod emit_glsl_undefined;
pub mod emit_glsl_warp;
pub mod glsl_emit_context;
pub mod var_alloc;

use crate::backend::bindings::Bindings;
use crate::ir;
use crate::profile::Profile;
use crate::runtime_info::RuntimeInfo;

/// Emit GLSL source from an IR program.
///
/// Returns the GLSL source string.
pub fn emit_glsl(
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    program: &mut ir::Program,
    bindings: &mut Bindings,
) -> String {
    let mut ctx = glsl_emit_context::EmitContext::new(program, bindings, profile, runtime_info);
    emit_glsl::emit_program(&mut ctx, program);
    let mut header = std::mem::take(&mut ctx.header);
    header.push_str("void main(){\n");
    if program.local_memory_size > 0 {
        header.push_str(&format!(
            "uint lmem[{}];\n",
            program.local_memory_size.div_ceil(4)
        ));
    }
    ctx.define_variables(&mut header);
    let code = std::mem::take(&mut ctx.code);
    if std::env::var_os("RUZU_FORCE_FRAGMENT_RED").is_some()
        && program.stage == ir::types::ShaderStage::Fragment
    {
        return format!("{}frag_color0=vec4(1.0,0.0,0.0,1.0);return;}}\n", header);
    }
    if let Ok(mode) = std::env::var("RUZU_FRAGMENT_DEBUG") {
        if program.stage == ir::types::ShaderStage::Fragment {
            match mode.as_str() {
                "attr1" if program.info.loads.generic_any(1) => {
                    return format!("{}frag_color0=abs(in_attr1);return;}}\n", header);
                }
                "attr2" if program.info.loads.generic_any(2) => {
                    return format!("{}frag_color0=abs(in_attr2);return;}}\n", header);
                }
                "tex0_attr1" if !ctx.textures.is_empty() && program.info.loads.generic_any(1) => {
                    return format!(
                        "{}frag_color0=texture(tex{},in_attr1.xy);return;}}\n",
                        header, ctx.textures[0].binding
                    );
                }
                "tex0_attr2" if !ctx.textures.is_empty() && program.info.loads.generic_any(2) => {
                    return format!(
                        "{}frag_color0=texture(tex{},in_attr2.xy);return;}}\n",
                        header, ctx.textures[0].binding
                    );
                }
                "tex1_attr2" if ctx.textures.len() > 1 && program.info.loads.generic_any(2) => {
                    return format!(
                        "{}frag_color0=texture(tex{},in_attr2.xy);return;}}\n",
                        header, ctx.textures[1].binding
                    );
                }
                "tex0_zero" => {
                    if let Some(texture) = ctx.textures.first() {
                        return format!(
                            "{}frag_color0=texture(tex{},vec2(0.0,0.0));return;}}\n",
                            header, texture.binding
                        );
                    }
                    return format!("{}frag_color0=vec4(0.0,1.0,0.0,1.0);return;}}\n", header);
                }
                "tex0_fetch0" => {
                    if let Some(texture) = ctx.textures.first() {
                        return format!(
                            "{}frag_color0=texelFetch(tex{},ivec2(0,0),0);return;}}\n",
                            header, texture.binding
                        );
                    }
                    return format!("{}frag_color0=vec4(0.0,1.0,0.0,1.0);return;}}\n", header);
                }
                _ => {}
            }
        }
    }
    if std::env::var_os("RUZU_FORCE_VERTEX_TRIANGLE").is_some()
        && matches!(
            program.stage,
            ir::types::ShaderStage::VertexA | ir::types::ShaderStage::VertexB
        )
    {
        return format!(
            "{}vec2 p[3]=vec2[3](vec2(-1.0,-1.0),vec2(3.0,-1.0),vec2(-1.0,3.0));gl_Position=vec4(p[gl_VertexID%3],0.0,1.0);return;}}\n",
            header
        );
    }
    format!("{}{}{}", header, code, "}\n")
}

/// Convenience overload without explicit bindings.
pub fn emit_glsl_default(profile: &Profile, program: &ir::Program) -> String {
    let mut bindings = Bindings::default();
    let runtime_info = RuntimeInfo::default();
    let mut program = program.clone();
    emit_glsl(profile, &runtime_info, &mut program, &mut bindings)
}
