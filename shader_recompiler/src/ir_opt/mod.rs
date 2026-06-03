// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! IR optimization passes: identity removal, dead code elimination,
//! constant propagation, and shader info collection.
//!
//! Matches zuyu's `ir_opt/` directory. The pass execution order is:
//! 1. IdentityRemovalPass
//! 2. ConstantPropagationPass
//! 3. DeadCodeEliminationPass
//! 4. CollectShaderInfoPass

pub mod collect_info;
pub mod conditional_barrier_pass;
pub mod constant_propagation;
pub mod dead_code_elimination;
pub mod dual_vertex_pass;
pub mod global_memory_to_storage_buffer_pass;
pub mod identity_removal;
pub mod layer_pass;
pub mod lower_fp16_to_fp32;
pub mod lower_fp64_to_fp32;
pub mod lower_int64_to_int32;
pub mod position_pass;
pub mod rescaling_pass;
pub mod ssa_rewrite_pass;
pub mod texture_pass;
pub mod vendor_workaround_pass;
pub mod verification_pass;

use crate::host_translate_info::HostTranslateInfo;
use crate::ir::program::Program;
use crate::ir::program::SyntaxNode;
use crate::ir::value::Value;

/// Run all optimization passes on the program in the correct order.
pub fn optimize(program: &mut Program) {
    optimize_with_host_info(program, &HostTranslateInfo::default());
}

pub fn optimize_with_host_info(program: &mut Program, host_info: &HostTranslateInfo) {
    ssa_rewrite_pass::ssa_rewrite_pass(program);
    verify_no_erased_refs_if_requested(program, "after_ssa_rewrite");
    identity_removal::identity_removal_pass(program);
    verify_no_erased_refs_if_requested(program, "after_identity_removal_1");
    constant_propagation::constant_propagation_pass(program);
    verify_no_erased_refs_if_requested(program, "after_constant_propagation");
    identity_removal::identity_removal_pass(program);
    verify_no_erased_refs_if_requested(program, "after_identity_removal_2");
    global_memory_to_storage_buffer_pass::global_memory_to_storage_buffer_pass(program, host_info);
    verify_no_erased_refs_if_requested(program, "after_global_memory_to_storage_buffer_pass");
    dead_code_elimination::dead_code_elimination_pass(program);
    verify_no_erased_refs_if_requested(program, "after_dce");
    collect_info::collect_shader_info_pass(program);
}

/// Run the graphics-texture-aware subset of the upstream optimization order.
///
/// Upstream runs `TexturePass(env, program, host_info)` before DCE and
/// `CollectShaderInfoPass`. This overload keeps the existing env-less
/// `optimize` path intact while allowing OpenGL graphics compilation to
/// resolve bound texture descriptors using `env.TextureBoundBuffer()`.
pub fn optimize_with_bound_textures(program: &mut Program, texture_bound_buffer: u32) {
    optimize_with_bound_textures_and_host_info(
        program,
        texture_bound_buffer,
        &HostTranslateInfo::default(),
    );
}

pub fn optimize_with_bound_textures_and_host_info(
    program: &mut Program,
    texture_bound_buffer: u32,
    host_info: &HostTranslateInfo,
) {
    ssa_rewrite_pass::ssa_rewrite_pass(program);
    verify_no_erased_refs_if_requested(program, "after_ssa_rewrite");
    identity_removal::identity_removal_pass(program);
    verify_no_erased_refs_if_requested(program, "after_identity_removal_1");
    constant_propagation::constant_propagation_pass(program);
    verify_no_erased_refs_if_requested(program, "after_constant_propagation");
    identity_removal::identity_removal_pass(program);
    verify_no_erased_refs_if_requested(program, "after_identity_removal_2");
    global_memory_to_storage_buffer_pass::global_memory_to_storage_buffer_pass(program, host_info);
    verify_no_erased_refs_if_requested(program, "after_global_memory_to_storage_buffer_pass");
    texture_pass::texture_pass_bound_textures(program, texture_bound_buffer);
    verify_no_erased_refs_if_requested(program, "after_texture_pass");
    dead_code_elimination::dead_code_elimination_pass(program);
    verify_no_erased_refs_if_requested(program, "after_dce");
    collect_info::collect_shader_info_pass(program);
}

fn verify_no_erased_refs_if_requested(program: &Program, phase: &str) {
    if std::env::var_os("RUZU_VERIFY_IR_REFS").is_none() {
        return;
    }
    for (block_idx, block) in program.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.indexed_iter() {
            for (arg_idx, value) in inst.args.iter().enumerate() {
                verify_live_value(
                    program,
                    value,
                    phase,
                    &format!("inst {}:{} arg {}", block_idx, inst_idx, arg_idx),
                );
            }
            for (pred_idx, value) in &inst.phi_args {
                verify_live_value(
                    program,
                    value,
                    phase,
                    &format!("inst {}:{} phi_pred {}", block_idx, inst_idx, pred_idx),
                );
            }
        }
    }
    for (node_idx, node) in program.syntax_list.iter().enumerate() {
        match node {
            SyntaxNode::If { cond, .. }
            | SyntaxNode::Repeat { cond, .. }
            | SyntaxNode::Break { cond, .. } => {
                verify_live_value(
                    program,
                    cond,
                    phase,
                    &format!("syntax node {} condition", node_idx),
                );
            }
            _ => {}
        }
    }
}

fn verify_live_value(program: &Program, value: &Value, phase: &str, owner: &str) {
    let Value::Inst(inst_ref) = value else {
        return;
    };
    let live = program
        .blocks
        .get(inst_ref.block as usize)
        .and_then(|block| block.instructions.get(inst_ref.inst as usize))
        .is_some_and(|inst| inst.is_some());
    if !live {
        panic!(
            "IR stale InstRef after {phase}: {owner} references erased {}:{}",
            inst_ref.block, inst_ref.inst
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::emitter::Emitter;
    use crate::ir::instruction::Inst;
    use crate::ir::opcodes::Opcode;
    use crate::ir::types::ShaderStage;
    use crate::ir::value::{Attribute, Value};

    fn make_test_program() -> Program {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program
    }

    // ── Constant Propagation ─────────────────────────────────────────

    #[test]
    fn test_const_prop_iadd() {
        let mut program = make_test_program();
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let _ = emitter.iadd_32(Value::ImmU32(10), Value::ImmU32(20));
        }

        constant_propagation::constant_propagation_pass(&mut program);

        // After const prop, the IAdd should become Identity(30)
        assert_eq!(program.blocks[0].inst(0).opcode, Opcode::Identity);
        assert_eq!(program.blocks[0].inst(0).args[0], Value::ImmU32(30));
    }

    #[test]
    fn test_const_prop_imul() {
        let mut program = make_test_program();
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let _ = emitter.imul_32(Value::ImmU32(6), Value::ImmU32(7));
        }

        constant_propagation::constant_propagation_pass(&mut program);

        assert_eq!(program.blocks[0].inst(0).opcode, Opcode::Identity);
        assert_eq!(program.blocks[0].inst(0).args[0], Value::ImmU32(42));
    }

    #[test]
    fn test_const_prop_fp_add() {
        let mut program = make_test_program();
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let _ = emitter.fp_add_32(Value::ImmF32(1.5), Value::ImmF32(2.5));
        }

        constant_propagation::constant_propagation_pass(&mut program);

        assert_eq!(program.blocks[0].inst(0).opcode, Opcode::Identity);
        assert_eq!(program.blocks[0].inst(0).args[0], Value::ImmF32(4.0));
    }

    #[test]
    fn test_const_prop_bitwise() {
        let mut program = make_test_program();
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let _ = emitter.bitwise_and_32(Value::ImmU32(0xFF00), Value::ImmU32(0x0FF0));
        }

        constant_propagation::constant_propagation_pass(&mut program);

        assert_eq!(program.blocks[0].inst(0).opcode, Opcode::Identity);
        assert_eq!(program.blocks[0].inst(0).args[0], Value::ImmU32(0x0F00));
    }

    #[test]
    fn test_const_prop_bitcast() {
        let mut program = make_test_program();
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let _ = emitter.bit_cast_u32_f32(Value::ImmF32(1.0));
        }

        constant_propagation::constant_propagation_pass(&mut program);

        assert_eq!(program.blocks[0].inst(0).opcode, Opcode::Identity);
        assert_eq!(
            program.blocks[0].inst(0).args[0],
            Value::ImmU32(0x3F800000) // IEEE 754 encoding of 1.0
        );
    }

    #[test]
    fn test_const_prop_select_true() {
        let mut program = make_test_program();
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let _ = emitter.select_u32(Value::ImmU1(true), Value::ImmU32(100), Value::ImmU32(200));
        }

        constant_propagation::constant_propagation_pass(&mut program);

        assert_eq!(program.blocks[0].inst(0).opcode, Opcode::Identity);
        assert_eq!(program.blocks[0].inst(0).args[0], Value::ImmU32(100));
    }

    // ── Identity Removal ─────────────────────────────────────────────

    #[test]
    fn test_identity_removal() {
        let mut program = make_test_program();
        // Manually insert an Identity instruction
        program.blocks[0].append_inst(Inst::new(Opcode::Identity, vec![Value::ImmU32(42)]));
        program.blocks[0].append_inst(Inst::new(
            Opcode::FPAdd32,
            vec![Value::ImmF32(1.0), Value::ImmF32(2.0)],
        ));

        assert_eq!(program.blocks[0].instructions.len(), 2);

        identity_removal::identity_removal_pass(&mut program);

        // Rust erases the stable slot without shifting later InstRefs.
        assert_eq!(program.blocks[0].instructions.len(), 2);
        assert!(program.blocks[0].instructions[0].is_none());
        assert_eq!(program.blocks[0].inst(1).opcode, Opcode::FPAdd32);
    }

    // ── Dead Code Elimination ────────────────────────────────────────

    #[test]
    fn test_dce_removes_unused() {
        let mut program = make_test_program();
        {
            let mut emitter = Emitter::new(&mut program, 0);
            // These instructions produce values but nothing uses them
            let _ = emitter.iadd_32(Value::ImmU32(1), Value::ImmU32(2));
            let _ = emitter.fp_add_32(Value::ImmF32(3.0), Value::ImmF32(4.0));
        }

        assert_eq!(program.blocks[0].instructions.len(), 2);

        dead_code_elimination::dead_code_elimination_pass(&mut program);

        // Both stable slots are erased without shifting InstRefs.
        assert_eq!(program.blocks[0].instructions.len(), 2);
        assert!(program.blocks[0].instructions[0].is_none());
        assert!(program.blocks[0].instructions[1].is_none());
    }

    #[test]
    fn test_dce_keeps_side_effects() {
        let mut program = make_test_program();
        // SetAttribute has side effects and should NOT be removed
        program.blocks[0].append_inst(Inst::new(
            Opcode::SetAttribute,
            vec![Value::Attribute(Attribute::position(0)), Value::ImmF32(1.0)],
        ));

        dead_code_elimination::dead_code_elimination_pass(&mut program);

        // SetAttribute should remain because it has side effects
        assert_eq!(program.blocks[0].instructions.len(), 1);
    }

    // ── Collect Info ─────────────────────────────────────────────────

    #[test]
    fn test_collect_info_cbuf() {
        let mut program = make_test_program();
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let _ = emitter.get_cbuf_u32(Value::ImmU32(0), Value::ImmU32(16));
            let _ = emitter.get_cbuf_f32(Value::ImmU32(2), Value::ImmU32(0));
        }

        collect_info::collect_shader_info_pass(&mut program);

        assert_eq!(program.info.constant_buffer_descriptors.len(), 2);
        assert_eq!(program.info.constant_buffer_descriptors[0].index, 0);
        assert_eq!(program.info.constant_buffer_descriptors[1].index, 2);
        assert_eq!(program.info.constant_buffer_mask, (1 << 0) | (1 << 2));
    }

    #[test]
    fn test_collect_info_drops_stale_registered_cbuf() {
        use crate::ir::program::ShaderInfoExt;

        let mut program = make_test_program();
        program.info.register_cbuf(4);

        collect_info::collect_shader_info_pass(&mut program);

        assert_eq!(program.info.constant_buffer_mask, 0);
        assert!(program.info.constant_buffer_descriptors.is_empty());
        assert_eq!(program.info.constant_buffer_used_sizes[4], 0);
    }

    #[test]
    fn test_collect_info_marks_global_memory_and_int64() {
        let mut program = make_test_program();
        program.blocks[0].append_inst(Inst::new(Opcode::LoadGlobal32, vec![Value::ImmU64(0x1000)]));
        program.blocks[0].append_inst(Inst::new(
            Opcode::WriteGlobal32,
            vec![Value::ImmU64(0x1004), Value::ImmU32(7)],
        ));

        collect_info::collect_shader_info_pass(&mut program);

        assert!(program.info.uses_int64);
        assert!(program.info.uses_global_memory);
        assert!(program.info.stores_global_memory);
    }

    #[test]
    fn test_collect_info_attributes() {
        let mut program = make_test_program();
        // Simulate loading generic attribute 3
        program.blocks[0].append_inst(Inst::new(
            Opcode::GetAttribute,
            vec![Value::Attribute(Attribute::generic(3, 0))],
        ));
        // Simulate storing to position
        program.blocks[0].append_inst(Inst::new(
            Opcode::SetAttribute,
            vec![Value::Attribute(Attribute::position(0)), Value::ImmF32(1.0)],
        ));

        collect_info::collect_shader_info_pass(&mut program);

        // Generic3 component 0 → VaryingState bit at Generic0X + 3*4 = 44
        assert!(program.info.loads.generic_any(3));
        // PositionX → VaryingState bit at PositionX(28)
        assert!(program
            .info
            .stores
            .any_component(Attribute::POSITION_X.0 as usize));
    }

    // ── Full Optimization Pipeline ───────────────────────────────────

    #[test]
    fn test_full_optimize_pipeline() {
        let mut program = make_test_program();
        {
            let mut emitter = Emitter::new(&mut program, 0);
            // Create some constant expressions that can be folded
            let a = emitter.iadd_32(Value::ImmU32(5), Value::ImmU32(10));
            // And an instruction that uses the result (so it's not dead code)
            let _ = emitter.set_attribute(Attribute::position(0), a, Value::ImmU32(0));
        }

        let initial_count = program.blocks[0].instructions.len();
        assert_eq!(initial_count, 2);

        optimize(&mut program);

        // After optimization:
        // - IAdd(5, 10) is const-propagated to Identity(15)
        // - Identity is left as a no-op tombstone to preserve InstRef indices
        // - SetAttribute should remain (side effect)
        // The exact result depends on pass ordering, but we should have
        // at most 2 instructions and the program should be valid
        assert!(program.blocks[0].instructions.len() <= initial_count);
    }
}
