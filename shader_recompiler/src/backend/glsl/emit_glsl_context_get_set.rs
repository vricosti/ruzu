// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL context get/set emission (constant buffers, attributes).
//!
//! Maps to upstream `backend/glsl/emit_glsl_context_get_set.cpp`.

use crate::ir;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::value::{Attribute, InstRef, Value};

use super::glsl_emit_context::EmitContext;
use super::var_alloc::GlslVarType;

const SWIZZLE: [&str; 4] = ["x", "y", "z", "w"];

fn cbuf_index(offset: u32) -> u32 {
    (offset / 4) % 4
}

fn offset_swizzle(offset: u32) -> &'static str {
    SWIZZLE[cbuf_index(offset) as usize]
}

fn inst_mut<'a>(program: &'a mut ir::Program, inst_ref: InstRef) -> &'a mut Inst {
    program.block_mut(inst_ref.block).inst_mut(inst_ref.inst)
}

fn choose_cbuf(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    binding: &Value,
    index: &str,
) -> String {
    if binding.is_immediate() {
        format!("{}_cbuf{}[{}]", ctx.stage_name, binding.imm_u32(), index)
    } else {
        let binding_var = ctx.var_alloc.consume(program, binding);
        format!("GetCbufIndirect({}, {})", binding_var, index)
    }
}

fn get_cbuf(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    ret: &str,
    binding: &Value,
    offset: &Value,
    num_bits: u32,
    cast: &str,
    bit_offset: Option<String>,
) {
    let is_immediate = offset.is_immediate();
    let component_indexing_bug = !is_immediate && ctx.profile.has_gl_component_indexing_bug;
    if is_immediate {
        let u32_offset = offset.imm_u32();
        let signed_offset = u32_offset as i32;
        const CBUF_SIZE: u32 = 0x10000;
        if signed_offset < 0 || u32_offset > CBUF_SIZE {
            ctx.add_fmt(format!("{}=0u;", ret));
            return;
        }
    }

    let offset_var = if is_immediate {
        String::new()
    } else {
        ctx.var_alloc.consume(program, offset)
    };
    let index = if is_immediate {
        format!("{}", offset.imm_u32() / 16)
    } else {
        format!("{}>>4", offset_var)
    };
    let swizzle = if is_immediate {
        format!(".{}", offset_swizzle(offset.imm_u32()))
    } else {
        format!("[({}>>2)%4]", offset_var)
    };

    let cbuf = choose_cbuf(ctx, program, binding, &index);
    let cbuf_cast = format!("{}({}{{}})", cast, cbuf);
    let extraction = if num_bits == 32 {
        cbuf_cast
    } else {
        format!(
            "bitfieldExtract({},int({}),{})",
            cbuf_cast,
            bit_offset.expect("bit offset required for sub-32 cbuf loads"),
            num_bits
        )
    };

    if !component_indexing_bug {
        ctx.add_fmt(format!("{}={};", ret, extraction.replace("{}", &swizzle)));
        return;
    }

    let cbuf_offset = format!("{}>>2", offset_var);
    for (i, swizzle_string) in [".x", ".y", ".z", ".w"].iter().enumerate() {
        ctx.add_fmt(format!(
            "if(({}&3)=={}){}={};",
            cbuf_offset,
            i,
            ret,
            extraction.replace("{}", swizzle_string)
        ));
    }
}

fn get_cbuf8(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    binding: &Value,
    offset: &Value,
    cast: &str,
) {
    let ret = ctx
        .var_alloc
        .define(inst_mut(program, inst_ref), GlslVarType::U32);
    let bit_offset = if offset.is_immediate() {
        format!("{}", (offset.imm_u32() % 4) * 8)
    } else {
        let offset_var = ctx.var_alloc.consume(program, offset);
        format!("({}%4)*8", offset_var)
    };
    get_cbuf(
        ctx,
        program,
        &ret,
        binding,
        offset,
        8,
        cast,
        Some(bit_offset),
    );
}

fn get_cbuf16(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    binding: &Value,
    offset: &Value,
    cast: &str,
) {
    let ret = ctx
        .var_alloc
        .define(inst_mut(program, inst_ref), GlslVarType::U32);
    let bit_offset = if offset.is_immediate() {
        format!("{}", ((offset.imm_u32() / 2) % 2) * 16)
    } else {
        let offset_var = ctx.var_alloc.consume(program, offset);
        format!("(({}>>1)%2)*16", offset_var)
    };
    get_cbuf(
        ctx,
        program,
        &ret,
        binding,
        offset,
        16,
        cast,
        Some(bit_offset),
    );
}

pub fn emit_get_cbuf(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    opcode: Opcode,
    binding: &Value,
    offset: &Value,
) {
    match opcode {
        Opcode::GetCbufU8 => {
            let cast = if ctx.profile.has_gl_cbuf_ftou_bug {
                ""
            } else {
                "ftou"
            };
            get_cbuf8(ctx, program, inst_ref, binding, offset, cast);
        }
        Opcode::GetCbufS8 => {
            let cast = if ctx.profile.has_gl_cbuf_ftou_bug {
                "int"
            } else {
                "ftoi"
            };
            get_cbuf8(ctx, program, inst_ref, binding, offset, cast);
        }
        Opcode::GetCbufU16 => {
            let cast = if ctx.profile.has_gl_cbuf_ftou_bug {
                ""
            } else {
                "ftou"
            };
            get_cbuf16(ctx, program, inst_ref, binding, offset, cast);
        }
        Opcode::GetCbufS16 => {
            let cast = if ctx.profile.has_gl_cbuf_ftou_bug {
                "int"
            } else {
                "ftoi"
            };
            get_cbuf16(ctx, program, inst_ref, binding, offset, cast);
        }
        Opcode::GetCbufU32 => {
            let ret = ctx
                .var_alloc
                .define(inst_mut(program, inst_ref), GlslVarType::U32);
            let cast = if ctx.profile.has_gl_cbuf_ftou_bug {
                ""
            } else {
                "ftou"
            };
            get_cbuf(ctx, program, &ret, binding, offset, 32, cast, None);
        }
        Opcode::GetCbufF32 => {
            let ret = ctx
                .var_alloc
                .define(inst_mut(program, inst_ref), GlslVarType::F32);
            let cast = if ctx.profile.has_gl_cbuf_ftou_bug {
                "utof"
            } else {
                ""
            };
            get_cbuf(ctx, program, &ret, binding, offset, 32, cast, None);
        }
        Opcode::GetCbufU32x2 => {
            let cast = if ctx.profile.has_gl_cbuf_ftou_bug {
                ""
            } else {
                "ftou"
            };
            let ret = ctx
                .var_alloc
                .define(inst_mut(program, inst_ref), GlslVarType::U32x2);
            if offset.is_immediate() {
                let u32_offset = offset.imm_u32();
                let signed_offset = u32_offset as i32;
                const CBUF_SIZE: u32 = 0x10000;
                if signed_offset < 0 || u32_offset > CBUF_SIZE {
                    ctx.add_fmt(format!("{}=uvec2(0u);", ret));
                    return;
                }
                let cbuf = format!("{}_cbuf{}", ctx.stage_name, binding.imm_u32());
                if u32_offset % 2 == 0 {
                    ctx.add_fmt(format!(
                        "{}={}({}[{}].{}{});",
                        ret,
                        cast,
                        cbuf,
                        u32_offset / 16,
                        offset_swizzle(u32_offset),
                        offset_swizzle(u32_offset + 4)
                    ));
                } else {
                    ctx.add_fmt(format!(
                        "{}=uvec2({}({}[{}].{}),{}({}[{}].{}));",
                        ret,
                        cast,
                        cbuf,
                        u32_offset / 16,
                        offset_swizzle(u32_offset),
                        cast,
                        cbuf,
                        (u32_offset + 4) / 16,
                        offset_swizzle(u32_offset + 4)
                    ));
                }
                return;
            }

            let offset_var = ctx.var_alloc.consume(program, offset);
            let cbuf = choose_cbuf(ctx, program, binding, &format!("{}>>4", offset_var));
            if !ctx.profile.has_gl_component_indexing_bug {
                ctx.add_fmt(format!(
                    "{}=uvec2({}({}[({}>>2)%4]),{}({}[(({}+4)>>2)%4]));",
                    ret, cast, cbuf, offset_var, cast, cbuf, offset_var
                ));
                return;
            }

            let cbuf_offset = format!("{}>>2", offset_var);
            for swizzle in 0..4 {
                ctx.add_fmt(format!(
                    "if(({}&3)=={}){}=uvec2({}({}.{}),{}({}.{}));",
                    cbuf_offset,
                    swizzle,
                    ret,
                    cast,
                    cbuf,
                    SWIZZLE[swizzle],
                    cast,
                    cbuf,
                    SWIZZLE[(swizzle + 1) % 4]
                ));
            }
        }
        _ => unreachable!("not a cbuf opcode: {:?}", opcode),
    }
}

pub fn emit_get_attribute(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    attr: Attribute,
    vertex: &str,
) {
    let ret = ctx
        .var_alloc
        .define(inst_mut(program, inst_ref), GlslVarType::F32);
    let element = attr.0 % 4;
    let swizzle = SWIZZLE[element as usize];
    if attr.is_generic() {
        let index = attr.generic_index();
        if !ctx
            .runtime_info
            .previous_stage_stores
            .generic(index as usize, element as usize)
        {
            let default_value = if element == 3 { "1.f" } else { "0.f" };
            ctx.add_fmt(format!("{}={};", ret, default_value));
            return;
        }
        let input_index = if ctx.is_input_array() {
            format!("[{}]", vertex)
        } else {
            String::new()
        };
        ctx.add_fmt(format!(
            "{}=in_attr{}{}.{};",
            ret, index, input_index, swizzle
        ));
        return;
    }
    if attr.is_position() {
        let input_decorator = if ctx.is_input_array() {
            format!("gl_in[{}].", vertex)
        } else {
            String::new()
        };
        ctx.add_fmt(format!(
            "{}={}{}.{};",
            ret, input_decorator, ctx.position_name, swizzle
        ));
        return;
    }
    match attr {
        Attribute::PRIMITIVE_ID => ctx.add_fmt(format!("{}=itof(gl_PrimitiveID);", ret)),
        Attribute::LAYER => ctx.add_fmt(format!("{}=itof(gl_Layer);", ret)),
        Attribute::POINT_SIZE => ctx.add_fmt(format!("{}=gl_PointSize;", ret)),
        Attribute::BASE_INSTANCE => ctx.add_fmt(format!("{}=itof(gl_BaseInstance);", ret)),
        Attribute::BASE_VERTEX => ctx.add_fmt(format!("{}=itof(gl_BaseVertex);", ret)),
        Attribute::DRAW_ID => ctx.add_fmt(format!("{}=itof(gl_DrawID);", ret)),
        _ => ctx.add_fmt(format!("// GetAttribute({}) not fully implemented", attr.0)),
    }
}

pub fn emit_get_attribute_u32(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    attr: Attribute,
) {
    let ret = ctx
        .var_alloc
        .define(inst_mut(program, inst_ref), GlslVarType::U32);
    match attr {
        Attribute::PRIMITIVE_ID => ctx.add_fmt(format!("{}=uint(gl_PrimitiveID);", ret)),
        Attribute::BASE_INSTANCE => ctx.add_fmt(format!("{}=uint(gl_BaseInstance);", ret)),
        Attribute::BASE_VERTEX => ctx.add_fmt(format!("{}=uint(gl_BaseVertex);", ret)),
        Attribute::DRAW_ID => ctx.add_fmt(format!("{}=uint(gl_DrawID);", ret)),
        _ => ctx.add_fmt(format!(
            "// GetAttributeU32({}) not fully implemented",
            attr.0
        )),
    }
}

pub fn emit_set_attribute(ctx: &mut EmitContext, attr_raw: u32, value: &str) {
    let element = attr_raw % 4;
    let swizzle = SWIZZLE[element as usize];
    if attr_raw >= 32 && attr_raw < 160 {
        let index = (attr_raw - 32) / 4;
        let info = &ctx.output_generics[index as usize][element as usize];
        if info.name.is_empty() {
            ctx.add_fmt(format!("out_attr{}.{}={};", index, swizzle, value));
        } else if info.num_components == 1 {
            ctx.add_fmt(format!("{}={};", info.name, value));
        } else {
            let index_element = element - info.first_element;
            ctx.add_fmt(format!(
                "{}.{}={};",
                info.name, SWIZZLE[index_element as usize], value
            ));
        }
        return;
    }
    if attr_raw >= 28 && attr_raw < 32 {
        ctx.add_fmt(format!("gl_Position.{}={};", swizzle, value));
        return;
    }
    ctx.add_fmt(format!(
        "// SetAttribute({}) not fully implemented",
        attr_raw
    ));
}

pub fn emit_set_frag_color(ctx: &mut EmitContext, render_target: u32, component: u32, value: &str) {
    if component < 4 {
        ctx.add_fmt(format!(
            "frag_color{}.{}={};",
            render_target, SWIZZLE[component as usize], value
        ));
    }
}

#[cfg(test)]
mod tests {
    use crate::backend::bindings::Bindings;
    use crate::backend::glsl::emit_glsl;
    use crate::ir::basic_block::Block;
    use crate::ir::emitter::Emitter;
    use crate::ir::program::ShaderInfoExt;
    use crate::ir::types::ShaderStage;
    use crate::ir::value::{Attribute, Value};
    use crate::ir_opt::optimize;
    use crate::profile::Profile;
    use crate::runtime_info::RuntimeInfo;

    #[test]
    fn glsl_cbuf_load_defines_ssa_value_and_declares_uniform() {
        let mut program = crate::ir::Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.info.register_cbuf(0);
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let value = emitter.get_cbuf_f32(Value::ImmU32(0), Value::ImmU32(16));
            emitter.set_attribute(Attribute::generic(0, 0), value, Value::ImmU32(0));
        }
        optimize(&mut program);

        let mut bindings = Bindings::default();
        let source = emit_glsl(
            &Profile::default(),
            &RuntimeInfo::default(),
            &mut program,
            &mut bindings,
        );

        assert!(source.contains("uniform vs_cbuf_0{vec4 vs_cbuf0[2];};"));
        assert!(source.contains("f_0=(vs_cbuf0[1].x);"));
        assert!(source.contains("layout(location=0)out vec4 out_attr0;"));
        assert!(source.contains("out_attr0.x=f_0;"));
        assert!(!source.contains("b_0"));
    }

    #[test]
    fn glsl_cbuf_u32x2_matches_upstream_vector_load_shape() {
        let mut program = crate::ir::Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.info.register_cbuf(0);
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let pair = emitter.get_cbuf_u32x2(Value::ImmU32(0), Value::ImmU32(16));
            let x = emitter.composite_extract_u32x2(pair, Value::ImmU32(0));
            emitter.set_attribute(Attribute::generic(0, 0), x, Value::ImmU32(0));
        }
        optimize(&mut program);

        let mut bindings = Bindings::default();
        let source = emit_glsl(
            &Profile::default(),
            &RuntimeInfo::default(),
            &mut program,
            &mut bindings,
        );

        assert!(source.contains("u2_0=ftou(vs_cbuf0[1].xy);"));
        assert!(source.contains("u_0=u2_0.x;"));
    }

    #[test]
    fn glsl_generic_input_defaults_when_previous_stage_did_not_store() {
        let mut program = crate::ir::Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let value = emitter.get_attribute(Attribute::generic(1, 3), Value::ImmU32(0));
            emitter.set_attribute(Attribute::generic(0, 0), value, Value::ImmU32(0));
        }
        optimize(&mut program);

        let mut bindings = Bindings::default();
        let source = emit_glsl(
            &Profile::default(),
            &RuntimeInfo::default(),
            &mut program,
            &mut bindings,
        );

        assert!(source.contains("f_0=1.f;"));
        assert!(!source.contains("in_attr1"));
        assert!(source.contains("out_attr0.x=f_0;"));
    }
}
