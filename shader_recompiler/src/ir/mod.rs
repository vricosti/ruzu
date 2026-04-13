// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Intermediate Representation for the Maxwell shader recompiler.
//!
//! This module provides the core IR data structures used to represent shader
//! programs between the Maxwell frontend (decoder/translator) and the SPIR-V
//! backend. The design matches zuyu's `frontend/ir/` directory.
//!
//! # Architecture
//!
//! ```text
//! Maxwell binary → Frontend → IR::Program → Optimization → SPIR-V Backend
//! ```
//!
//! An IR::Program consists of basic blocks containing SSA instructions.
//! Instructions reference values (immediates, registers, other instruction
//! results). A structured control flow AST (`SyntaxNode`) describes nested
//! if/loop/break regions for direct SPIR-V emission.

pub mod abstract_syntax_list;
pub mod attribute;
pub mod basic_block;
pub mod breadth_first_search;
pub mod condition;
pub mod emitter;
pub mod flow_test;
pub mod instruction;
pub mod modifiers;
pub mod opcodes;
pub mod patch;
pub mod post_order;
pub mod pred;
pub mod program;
pub mod reg;
pub mod types;
pub mod value;

// Re-export key types for convenience.
pub use abstract_syntax_list::{AbstractSyntaxData, AbstractSyntaxList, AbstractSyntaxNode};
pub use basic_block::Block;
pub use condition::{Condition, IrPred};
pub use emitter::Emitter;
pub use flow_test::FlowTest;
pub use instruction::Inst;
pub use opcodes::Opcode;
pub use patch::IrPatch;
pub use post_order::post_order;
pub use program::{Program, ShaderInfo, SyntaxNode};
pub use types::{FmzMode, FpControl, FpRounding, ShaderStage, TextureInstInfo, Type};
pub use value::{Attribute, InstRef, Patch, Pred, Reg, Value};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ir_program_construction() {
        let mut program = Program::new(ShaderStage::VertexB);
        assert_eq!(program.stage, ShaderStage::VertexB);
        assert_eq!(program.num_blocks(), 0);

        program.blocks.push(Block::new());
        assert_eq!(program.num_blocks(), 1);
    }

    #[test]
    fn test_ir_emitter_basic() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());

        let result = {
            let mut emitter = Emitter::new(&mut program, 0);
            emitter.fp_add_32(Value::ImmF32(1.0), Value::ImmF32(2.0))
        };

        assert!(result.is_inst());
        assert_eq!(program.blocks[0].instructions.len(), 1);
        assert_eq!(program.blocks[0].instructions[0].opcode, Opcode::FPAdd32);
    }

    #[test]
    fn test_ir_emitter_integer_ops() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());

        {
            let mut emitter = Emitter::new(&mut program, 0);
            let a = emitter.iadd_32(Value::ImmU32(10), Value::ImmU32(20));
            let b = emitter.imul_32(a, Value::ImmU32(3));
            let _ = emitter.isub_32(b, Value::ImmU32(1));
        }

        assert_eq!(program.blocks[0].instructions.len(), 3);
        assert_eq!(program.blocks[0].instructions[0].opcode, Opcode::IAdd32);
        assert_eq!(program.blocks[0].instructions[1].opcode, Opcode::IMul32);
        assert_eq!(program.blocks[0].instructions[2].opcode, Opcode::ISub32);
    }

    #[test]
    fn test_ir_emitter_comparison() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());

        {
            let mut emitter = Emitter::new(&mut program, 0);
            let cmp = emitter.fp_ord_less_than_32(Value::ImmF32(1.0), Value::ImmF32(2.0));
            let _ = emitter.select_f32(cmp, Value::ImmF32(10.0), Value::ImmF32(20.0));
        }

        assert_eq!(program.blocks[0].instructions.len(), 2);
        assert_eq!(
            program.blocks[0].instructions[0].opcode,
            Opcode::FPOrdLessThan32
        );
        assert_eq!(program.blocks[0].instructions[1].opcode, Opcode::SelectF32);
    }

    #[test]
    fn test_ir_value_types() {
        let u32_val = Value::ImmU32(42);
        assert!(u32_val.is_immediate());
        assert!(!u32_val.is_inst());
        assert_eq!(u32_val.imm_u32(), 42);

        let f32_val = Value::ImmF32(3.14);
        assert!(f32_val.is_immediate());
        assert_eq!(f32_val.imm_f32(), 3.14);

        let bool_val = Value::ImmU1(true);
        assert!(bool_val.is_immediate());
        assert!(bool_val.imm_u1());

        let inst_val = Value::Inst(InstRef { block: 0, inst: 5 });
        assert!(inst_val.is_inst());
        assert!(!inst_val.is_immediate());
        let r = inst_val.inst_ref();
        assert_eq!(r.block, 0);
        assert_eq!(r.inst, 5);
    }

    #[test]
    fn test_ir_register_special_values() {
        let rz = Reg::RZ;
        assert!(rz.is_zero());
        assert_eq!(rz.index(), 255);

        let r5 = Reg(5);
        assert!(!r5.is_zero());
        assert_eq!(r5.index(), 5);

        let pt = Pred::PT;
        assert!(pt.is_true());

        let p3 = Pred(3);
        assert!(!p3.is_true());
    }

    #[test]
    fn test_ir_attribute_layout() {
        // Position attributes: 28-31
        let pos_x = Attribute::position(0);
        assert!(pos_x.is_position());
        assert_eq!(pos_x.position_element(), 0);

        // Generic attributes: 32 + index*4 + component
        let gen5_y = Attribute::generic(5, 1);
        assert!(gen5_y.is_generic());
        assert_eq!(gen5_y.generic_index(), 5);
        assert_eq!(gen5_y.generic_element(), 1);
    }

    #[test]
    fn test_ir_opcode_metadata() {
        let meta = Opcode::FPAdd32.meta();
        assert_eq!(meta.name, "FPAdd32");
        assert_eq!(meta.return_type, Type::F32);

        let meta = Opcode::IAdd32.meta();
        assert_eq!(meta.name, "IAdd32");
        assert_eq!(meta.return_type, Type::U32);

        let meta = Opcode::FPOrdLessThan32.meta();
        assert_eq!(meta.return_type, Type::U1);

        assert!(!Opcode::FPAdd32.may_have_side_effects());
        assert!(Opcode::SetAttribute.may_have_side_effects());
    }

    #[test]
    fn test_ir_instruction_creation() {
        let inst = Inst::new(
            Opcode::FPAdd32,
            vec![Value::ImmF32(1.0), Value::ImmF32(2.0)],
        );
        assert_eq!(inst.opcode, Opcode::FPAdd32);
        assert_eq!(inst.args.len(), 2);
        assert_eq!(inst.use_count, 0);
        assert!(!inst.has_uses());
        assert!(!inst.may_have_side_effects());
    }
}
