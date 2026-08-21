// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V shared-memory emission — port of upstream
//! `backend/spirv/emit_spirv_shared_memory.cpp`.

use super::spirv_emit_context::SpirvEmitContext;
use crate::ir::{self, Opcode};
use rspirv::spirv::Word;

fn pointer(
    ctx: &mut SpirvEmitContext,
    pointer_type: Word,
    array: Word,
    offset: Word,
    shift: u32,
) -> Word {
    let shift_id = ctx.constant_u32(shift);
    let index = ctx
        .builder
        .shift_right_arithmetic(ctx.u32_type, None, offset, shift_id)
        .unwrap();
    ctx.builder
        .access_chain(pointer_type, None, array, vec![ctx.const_zero_u32, index])
        .unwrap()
}

fn word(ctx: &mut SpirvEmitContext, offset: Word) -> Word {
    let shift_id = ctx.constant_u32(2);
    let index = ctx
        .builder
        .shift_right_arithmetic(ctx.u32_type, None, offset, shift_id)
        .unwrap();
    let pointer = ctx
        .builder
        .access_chain(ctx.shared_u32, None, ctx.shared_memory_u32, vec![index])
        .unwrap();
    ctx.builder
        .load(ctx.u32_type, None, pointer, None, vec![])
        .unwrap()
}

fn extract_args(ctx: &mut SpirvEmitContext, offset: Word, mask: u32, count: u32) -> (Word, Word) {
    let three = ctx.constant_u32(3);
    let shift = ctx
        .builder
        .shift_left_logical(ctx.u32_type, None, offset, three)
        .unwrap();
    let mask_id = ctx.constant_u32(mask);
    let bit = ctx
        .builder
        .bitwise_and(ctx.u32_type, None, shift, mask_id)
        .unwrap();
    let count_id = ctx.constant_u32(count);
    (bit, count_id)
}

pub fn emit_load_shared_u8(ctx: &mut SpirvEmitContext, offset: Word) -> Word {
    if ctx.uses_explicit_workgroup_layout {
        let pointer = ctx
            .builder
            .access_chain(
                ctx.shared_u8,
                None,
                ctx.shared_memory_u8,
                vec![ctx.const_zero_u32, offset],
            )
            .unwrap();
        let value = ctx
            .builder
            .load(ctx.u8_type, None, pointer, None, vec![])
            .unwrap();
        ctx.builder.u_convert(ctx.u32_type, None, value).unwrap()
    } else {
        let (bit, count) = extract_args(ctx, offset, 24, 8);
        let value = word(ctx, offset);
        ctx.builder
            .bit_field_u_extract(ctx.u32_type, None, value, bit, count)
            .unwrap()
    }
}

pub fn emit_load_shared_s8(ctx: &mut SpirvEmitContext, offset: Word) -> Word {
    if ctx.uses_explicit_workgroup_layout {
        let pointer = ctx
            .builder
            .access_chain(
                ctx.shared_u8,
                None,
                ctx.shared_memory_u8,
                vec![ctx.const_zero_u32, offset],
            )
            .unwrap();
        let value = ctx
            .builder
            .load(ctx.u8_type, None, pointer, None, vec![])
            .unwrap();
        ctx.builder.s_convert(ctx.u32_type, None, value).unwrap()
    } else {
        let (bit, count) = extract_args(ctx, offset, 24, 8);
        let value = word(ctx, offset);
        ctx.builder
            .bit_field_s_extract(ctx.u32_type, None, value, bit, count)
            .unwrap()
    }
}

pub fn emit_load_shared_u16(ctx: &mut SpirvEmitContext, offset: Word) -> Word {
    if ctx.uses_explicit_workgroup_layout {
        let pointer = pointer(ctx, ctx.shared_u16, ctx.shared_memory_u16, offset, 1);
        let value = ctx
            .builder
            .load(ctx.u16_type, None, pointer, None, vec![])
            .unwrap();
        ctx.builder.u_convert(ctx.u32_type, None, value).unwrap()
    } else {
        let (bit, count) = extract_args(ctx, offset, 16, 16);
        let value = word(ctx, offset);
        ctx.builder
            .bit_field_u_extract(ctx.u32_type, None, value, bit, count)
            .unwrap()
    }
}

pub fn emit_load_shared_s16(ctx: &mut SpirvEmitContext, offset: Word) -> Word {
    if ctx.uses_explicit_workgroup_layout {
        let pointer = pointer(ctx, ctx.shared_u16, ctx.shared_memory_u16, offset, 1);
        let value = ctx
            .builder
            .load(ctx.u16_type, None, pointer, None, vec![])
            .unwrap();
        ctx.builder.s_convert(ctx.u32_type, None, value).unwrap()
    } else {
        let (bit, count) = extract_args(ctx, offset, 16, 16);
        let value = word(ctx, offset);
        ctx.builder
            .bit_field_s_extract(ctx.u32_type, None, value, bit, count)
            .unwrap()
    }
}

pub fn emit_load_shared_u32(ctx: &mut SpirvEmitContext, offset: Word) -> Word {
    if ctx.uses_explicit_workgroup_layout {
        let pointer = pointer(ctx, ctx.shared_u32, ctx.shared_memory_u32, offset, 2);
        ctx.builder
            .load(ctx.u32_type, None, pointer, None, vec![])
            .unwrap()
    } else {
        word(ctx, offset)
    }
}

pub fn emit_load_shared_u64(ctx: &mut SpirvEmitContext, offset: Word) -> Word {
    if ctx.uses_explicit_workgroup_layout {
        let pointer = pointer(ctx, ctx.shared_u32x2, ctx.shared_memory_u32x2, offset, 3);
        return ctx
            .builder
            .load(ctx.u32_vec2_type, None, pointer, None, vec![])
            .unwrap();
    }
    let shift = ctx.constant_u32(2);
    let base_index = ctx
        .builder
        .shift_right_arithmetic(ctx.u32_type, None, offset, shift)
        .unwrap();
    let next_index = ctx
        .builder
        .i_add(ctx.u32_type, None, base_index, ctx.const_one_u32)
        .unwrap();
    let lhs_pointer = ctx
        .builder
        .access_chain(
            ctx.shared_u32,
            None,
            ctx.shared_memory_u32,
            vec![base_index],
        )
        .unwrap();
    let rhs_pointer = ctx
        .builder
        .access_chain(
            ctx.shared_u32,
            None,
            ctx.shared_memory_u32,
            vec![next_index],
        )
        .unwrap();
    let lhs = ctx
        .builder
        .load(ctx.u32_type, None, lhs_pointer, None, vec![])
        .unwrap();
    let rhs = ctx
        .builder
        .load(ctx.u32_type, None, rhs_pointer, None, vec![])
        .unwrap();
    ctx.builder
        .composite_construct(ctx.u32_vec2_type, None, vec![lhs, rhs])
        .unwrap()
}

pub fn emit_load_shared_u128(ctx: &mut SpirvEmitContext, offset: Word) -> Word {
    if ctx.uses_explicit_workgroup_layout {
        let pointer = pointer(ctx, ctx.shared_u32x4, ctx.shared_memory_u32x4, offset, 4);
        return ctx
            .builder
            .load(ctx.u32_vec4_type, None, pointer, None, vec![])
            .unwrap();
    }
    let shift = ctx.constant_u32(2);
    let base_index = ctx
        .builder
        .shift_right_arithmetic(ctx.u32_type, None, offset, shift)
        .unwrap();
    let mut values = Vec::with_capacity(4);
    for index_offset in 0..4 {
        let index = if index_offset == 0 {
            base_index
        } else {
            let offset_id = ctx.constant_u32(index_offset);
            ctx.builder
                .i_add(ctx.u32_type, None, base_index, offset_id)
                .unwrap()
        };
        let pointer = ctx
            .builder
            .access_chain(ctx.shared_u32, None, ctx.shared_memory_u32, vec![index])
            .unwrap();
        values.push(
            ctx.builder
                .load(ctx.u32_type, None, pointer, None, vec![])
                .unwrap(),
        );
    }
    ctx.builder
        .composite_construct(ctx.u32_vec4_type, None, values)
        .unwrap()
}

pub fn emit_write_shared_u8(ctx: &mut SpirvEmitContext, offset: Word, value: Word) {
    if ctx.uses_explicit_workgroup_layout {
        let pointer = ctx
            .builder
            .access_chain(
                ctx.shared_u8,
                None,
                ctx.shared_memory_u8,
                vec![ctx.const_zero_u32, offset],
            )
            .unwrap();
        let converted = ctx.builder.u_convert(ctx.u8_type, None, value).unwrap();
        ctx.builder.store(pointer, converted, None, vec![]).unwrap();
    } else {
        ctx.builder
            .function_call(
                ctx.void_type,
                None,
                ctx.shared_store_u8_func,
                vec![offset, value],
            )
            .unwrap();
    }
}

pub fn emit_write_shared_u16(ctx: &mut SpirvEmitContext, offset: Word, value: Word) {
    if ctx.uses_explicit_workgroup_layout {
        let pointer = pointer(ctx, ctx.shared_u16, ctx.shared_memory_u16, offset, 1);
        let converted = ctx.builder.u_convert(ctx.u16_type, None, value).unwrap();
        ctx.builder.store(pointer, converted, None, vec![]).unwrap();
    } else {
        ctx.builder
            .function_call(
                ctx.void_type,
                None,
                ctx.shared_store_u16_func,
                vec![offset, value],
            )
            .unwrap();
    }
}

pub fn emit_write_shared_u32(ctx: &mut SpirvEmitContext, offset: Word, value: Word) {
    let pointer = if ctx.uses_explicit_workgroup_layout {
        pointer(ctx, ctx.shared_u32, ctx.shared_memory_u32, offset, 2)
    } else {
        let shift = ctx.constant_u32(2);
        let word_offset = ctx
            .builder
            .shift_right_arithmetic(ctx.u32_type, None, offset, shift)
            .unwrap();
        ctx.builder
            .access_chain(
                ctx.shared_u32,
                None,
                ctx.shared_memory_u32,
                vec![word_offset],
            )
            .unwrap()
    };
    ctx.builder.store(pointer, value, None, vec![]).unwrap();
}

pub fn emit_write_shared_u64(ctx: &mut SpirvEmitContext, offset: Word, value: Word) {
    if ctx.uses_explicit_workgroup_layout {
        let pointer = pointer(ctx, ctx.shared_u32x2, ctx.shared_memory_u32x2, offset, 3);
        ctx.builder.store(pointer, value, None, vec![]).unwrap();
        return;
    }
    let shift = ctx.constant_u32(2);
    let word_offset = ctx
        .builder
        .shift_right_arithmetic(ctx.u32_type, None, offset, shift)
        .unwrap();
    let next_offset = ctx
        .builder
        .i_add(ctx.u32_type, None, word_offset, ctx.const_one_u32)
        .unwrap();
    for (index, component) in [(word_offset, 0), (next_offset, 1)] {
        let pointer = ctx
            .builder
            .access_chain(ctx.shared_u32, None, ctx.shared_memory_u32, vec![index])
            .unwrap();
        let element = ctx
            .builder
            .composite_extract(ctx.u32_type, None, value, vec![component])
            .unwrap();
        ctx.builder.store(pointer, element, None, vec![]).unwrap();
    }
}

pub fn emit_write_shared_u128(ctx: &mut SpirvEmitContext, offset: Word, value: Word) {
    if ctx.uses_explicit_workgroup_layout {
        let pointer = pointer(ctx, ctx.shared_u32x4, ctx.shared_memory_u32x4, offset, 4);
        ctx.builder.store(pointer, value, None, vec![]).unwrap();
        return;
    }
    let shift = ctx.constant_u32(2);
    let base_index = ctx
        .builder
        .shift_right_arithmetic(ctx.u32_type, None, offset, shift)
        .unwrap();
    for index_offset in 0..4 {
        let index = if index_offset == 0 {
            base_index
        } else {
            let offset_id = ctx.constant_u32(index_offset);
            ctx.builder
                .i_add(ctx.u32_type, None, base_index, offset_id)
                .unwrap()
        };
        let pointer = ctx
            .builder
            .access_chain(ctx.shared_u32, None, ctx.shared_memory_u32, vec![index])
            .unwrap();
        let element = ctx
            .builder
            .composite_extract(ctx.u32_type, None, value, vec![index_offset])
            .unwrap();
        ctx.builder.store(pointer, element, None, vec![]).unwrap();
    }
}

pub fn emit_load(ctx: &mut SpirvEmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let offset = ctx.resolve_value(inst.arg(0));
    let value = match inst.opcode {
        Opcode::LoadSharedU8 => emit_load_shared_u8(ctx, offset),
        Opcode::LoadSharedS8 => emit_load_shared_s8(ctx, offset),
        Opcode::LoadSharedU16 => emit_load_shared_u16(ctx, offset),
        Opcode::LoadSharedS16 => emit_load_shared_s16(ctx, offset),
        Opcode::LoadSharedU32 => emit_load_shared_u32(ctx, offset),
        Opcode::LoadSharedU64 => emit_load_shared_u64(ctx, offset),
        Opcode::LoadSharedU128 => emit_load_shared_u128(ctx, offset),
        _ => unreachable!("not a shared-memory load: {:?}", inst.opcode),
    };
    ctx.set_value(block_idx, inst_idx, value);
}

pub fn emit_store(ctx: &mut SpirvEmitContext, inst: &ir::Inst) {
    let offset = ctx.resolve_value(inst.arg(0));
    let value = ctx.resolve_value(inst.arg(1));
    match inst.opcode {
        Opcode::WriteSharedU8 => emit_write_shared_u8(ctx, offset, value),
        Opcode::WriteSharedU16 => emit_write_shared_u16(ctx, offset, value),
        Opcode::WriteSharedU32 => emit_write_shared_u32(ctx, offset, value),
        Opcode::WriteSharedU64 => emit_write_shared_u64(ctx, offset, value),
        Opcode::WriteSharedU128 => emit_write_shared_u128(ctx, offset, value),
        _ => unreachable!("not a shared-memory store: {:?}", inst.opcode),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::bindings::Bindings;
    use crate::ir::types::ShaderStage;
    use crate::profile::Profile;
    use crate::runtime_info::RuntimeInfo;
    use rspirv::dr::Operand;
    use rspirv::spirv;

    fn contains_opcode(ctx: &SpirvEmitContext, opcode: spirv::Op) -> bool {
        ctx.builder.module_ref().functions.iter().any(|function| {
            function.blocks.iter().any(|block| {
                block
                    .instructions
                    .iter()
                    .any(|instruction| instruction.class.opcode == opcode)
            })
        })
    }

    #[test]
    fn fallback_shared_memory_emits_real_accesses_and_cas_helpers() {
        let mut program = ir::Program::new(ShaderStage::Compute);
        program.shared_memory_size = 64;
        program.info.uses_int8 = true;
        program.info.uses_int16 = true;
        program.info.uses_shared_increment = true;
        program.info.uses_shared_decrement = true;
        let mut ctx = SpirvEmitContext::new(&program, &Profile::default(), &RuntimeInfo::default());
        ctx.define_global_variables(&program, &mut Bindings::default());

        assert_ne!(ctx.shared_memory_u32, 0);
        assert_ne!(ctx.shared_store_u8_func, 0);
        assert_ne!(ctx.shared_store_u16_func, 0);
        assert_ne!(ctx.increment_cas_shared, 0);
        assert_ne!(ctx.decrement_cas_shared, 0);

        ctx.builder
            .begin_function(
                ctx.void_type,
                None,
                spirv::FunctionControl::NONE,
                ctx.void_fn_type,
            )
            .unwrap();
        ctx.builder.begin_block(None).unwrap();
        let offset = ctx.const_zero_u32;
        let value = emit_load_shared_u8(&mut ctx, offset);
        emit_write_shared_u8(&mut ctx, offset, value);
        emit_write_shared_u32(&mut ctx, offset, value);
        super::super::emit_spirv_atomic::emit_shared_atomic_iadd_32(&mut ctx, offset, value);
        super::super::emit_spirv_atomic::emit_shared_atomic_inc_32(&mut ctx, offset, value);
        super::super::emit_spirv_atomic::emit_shared_atomic_dec_32(&mut ctx, offset, value);
        ctx.builder.ret().unwrap();
        ctx.builder.end_function().unwrap();

        for opcode in [
            spirv::Op::AccessChain,
            spirv::Op::Load,
            spirv::Op::Store,
            spirv::Op::AtomicCompareExchange,
            spirv::Op::AtomicIAdd,
            spirv::Op::FunctionCall,
            spirv::Op::BitFieldUExtract,
        ] {
            assert!(contains_opcode(&ctx, opcode), "missing {opcode:?}");
        }
    }

    #[test]
    fn explicit_shared_memory_declares_upstream_layouts() {
        let mut program = ir::Program::new(ShaderStage::Compute);
        program.shared_memory_size = 64;
        program.info.uses_int8 = true;
        program.info.uses_int16 = true;
        program.info.uses_int64 = true;
        let profile = Profile {
            supported_spirv: 0x0001_0400,
            support_int8: true,
            support_int16: true,
            support_int64: true,
            support_explicit_workgroup_layout: true,
            support_workgroup_layout_8bit_access: true,
            support_workgroup_layout_16bit_access: true,
            ..Profile::default()
        };
        let mut ctx = SpirvEmitContext::new(&program, &profile, &RuntimeInfo::default());
        ctx.define_global_variables(&program, &mut Bindings::default());

        for variable in [
            ctx.shared_memory_u8,
            ctx.shared_memory_u16,
            ctx.shared_memory_u32,
            ctx.shared_memory_u64,
            ctx.shared_memory_u32x2,
            ctx.shared_memory_u32x4,
        ] {
            assert_ne!(variable, 0);
            assert!(ctx.interfaces.contains(&variable));
        }
        for capability in [
            spirv::Capability::WorkgroupMemoryExplicitLayoutKHR,
            spirv::Capability::WorkgroupMemoryExplicitLayout8BitAccessKHR,
            spirv::Capability::WorkgroupMemoryExplicitLayout16BitAccessKHR,
        ] {
            assert!(ctx
                .builder
                .module_ref()
                .capabilities
                .iter()
                .any(|instruction| {
                    matches!(
                        instruction.operands.as_slice(),
                        [Operand::Capability(found)] if *found == capability
                    )
                }));
        }
        assert!(ctx
            .builder
            .module_ref()
            .extensions
            .iter()
            .any(|instruction| {
                matches!(
                    instruction.operands.as_slice(),
                    [Operand::LiteralString(extension)]
                        if extension == "SPV_KHR_workgroup_memory_explicit_layout"
                )
            }));
    }

    #[test]
    fn local_memory_uses_private_array_accesses() {
        let mut program = ir::Program::new(ShaderStage::Compute);
        program.local_memory_size = 16;
        let profile = Profile {
            supported_spirv: 0x0001_0400,
            ..Profile::default()
        };
        let mut ctx = SpirvEmitContext::new(&program, &profile, &RuntimeInfo::default());
        ctx.define_global_variables(&program, &mut Bindings::default());
        assert_ne!(ctx.local_memory, 0);
        assert!(ctx.interfaces.contains(&ctx.local_memory));

        ctx.builder
            .begin_function(
                ctx.void_type,
                None,
                spirv::FunctionControl::NONE,
                ctx.void_fn_type,
            )
            .unwrap();
        ctx.builder.begin_block(None).unwrap();
        let offset = ctx.const_zero_u32;
        let value = super::super::emit_spirv_context_get_set::emit_load_local(&mut ctx, offset);
        super::super::emit_spirv_context_get_set::emit_write_local(&mut ctx, offset, value);
        ctx.builder.ret().unwrap();
        ctx.builder.end_function().unwrap();

        assert!(contains_opcode(&ctx, spirv::Op::AccessChain));
        assert!(contains_opcode(&ctx, spirv::Op::Load));
        assert!(contains_opcode(&ctx, spirv::Op::Store));
    }
}
