// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_conversion_floating_point.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;

const ROUNDING_OP_NONE: u32 = 0;
const ROUNDING_OP_PASS: u32 = 3;
const ROUNDING_OP_ROUND: u32 = 8;
const ROUNDING_OP_FLOOR: u32 = 9;
const ROUNDING_OP_CEIL: u32 = 10;
const ROUNDING_OP_TRUNC: u32 = 11;

pub fn f2f(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b_f32(insn, opcode);

    let abs_src = bit(insn, 49);
    let neg_src = bit(insn, 45);
    let sat = bit(insn, 50);
    let _dst_size = field(insn, 8, 2); // 0=F16, 1=F32, 2=F64
    let _src_size = field(insn, 10, 2);
    let rounding_op = field(insn, 39, 4) & 0x0B;

    let a = tv.ir.fp_abs_neg_32(src, abs_src, neg_src);

    let mut result = match rounding_op {
        ROUNDING_OP_NONE | ROUNDING_OP_PASS => a,
        ROUNDING_OP_ROUND => tv.ir.fp_round_even_32(a),
        ROUNDING_OP_FLOOR => tv.ir.fp_floor_32(a),
        ROUNDING_OP_CEIL => tv.ir.fp_ceil_32(a),
        ROUNDING_OP_TRUNC => tv.ir.fp_trunc_32(a),
        _ => {
            log::warn!("F2F rounding op {} not implemented; passing through", rounding_op);
            a
        }
    };

    if bit(insn, 47) {
        log::warn!("F2F CC is not implemented");
    }

    if _src_size != 2 || _dst_size != 2 {
        log::warn!(
            "F2F src/dst formats {}/{} not fully ported; treating as F32",
            _src_size,
            _dst_size
        );
    }

    if sat {
        result = tv.ir.fp_saturate_32(result);
    }

    tv.set_f(dst, result);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    #[test]
    fn f2f_pass_rounding_does_not_truncate() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut tv = TranslatorVisitor::new(&mut program, 0);

        let insn = 1u64
            | (2u64 << 8)
            | (2u64 << 10)
            | (2u64 << 20)
            | ((ROUNDING_OP_PASS as u64) << 39);

        f2f(&mut tv, insn, MaxwellOpcode::F2F_reg);
        let opcodes: Vec<_> = tv
            .ir
            .program
            .blocks[0]
            .iter()
            .map(|inst| inst.opcode)
            .collect();

        assert!(!opcodes.contains(&Opcode::FPTrunc32));
        assert!(!opcodes.contains(&Opcode::FPFloor32));
        assert!(!opcodes.contains(&Opcode::FPCeil32));
        assert!(!opcodes.contains(&Opcode::FPRoundEven32));
    }
}
