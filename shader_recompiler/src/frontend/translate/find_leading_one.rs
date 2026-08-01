// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/find_leading_one.cpp

use super::{bit, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

/// FLO — Find leading one (most significant set bit).
///
/// Encodes: FLO_reg, FLO_cbuf, FLO_imm.
pub fn flo(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let mut src = tv.decode_src_b(insn, opcode);

    // BitField<40, 1> tilde — invert source
    let tilde = bit(insn, 40);
    // BitField<41, 1> shift — XOR result with 31 when set (unless result == -1)
    let shift = bit(insn, 41);
    if bit(insn, 47) {
        panic!("FLO CC not implemented upstream");
    }
    // BitField<48, 1> is_signed
    let is_signed = bit(insn, 48);

    if tilde {
        src = tv.ir.bitwise_not_32(src);
    }

    let mut result = if is_signed {
        tv.ir.find_s_msb_32(src)
    } else {
        tv.ir.find_u_msb_32(src)
    };

    if shift {
        // Upstream: when result != -1, result = result XOR 31
        let not_found = tv.ir.i_equal(result, Value::ImmU32(u32::MAX));
        let xored = tv.ir.bitwise_xor_32(result, Value::ImmU32(31));
        result = tv.ir.select_u32(not_found, result, xored);
    }

    tv.set_x(dst, result);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    #[test]
    #[should_panic(expected = "FLO CC not implemented upstream")]
    fn flo_cc_matches_upstream_rejection() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut visitor = TranslatorVisitor::new(&mut program, 0);
        flo(&mut visitor, 1u64 << 47, MaxwellOpcode::FLO_reg);
    }
}
