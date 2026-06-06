// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_query.cpp
//!
//! Implements TXQ and TXQ_b (texture query — dimensions, texture type, etc.).

use super::{field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::types::TextureInstInfo;
use crate::ir::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
enum Mode {
    Dimension = 1,
    TextureType = 2,
    SamplePos = 5,
}

impl Mode {
    fn from_bits(raw: u64) -> Self {
        match raw & 0x7 {
            1 => Self::Dimension,
            2 => Self::TextureType,
            5 => Self::SamplePos,
            value => panic!("Mode {}", value),
        }
    }
}

fn query(v: &mut TranslatorVisitor, handle: Value, mode: Mode, src_reg: u32, mask: u32) -> Value {
    match mode {
        Mode::Dimension => {
            let needs_num_mips = ((mask >> 3) & 1) != 0;
            let skip_mips = Value::ImmU1(!needs_num_mips);
            let lod = v.x(src_reg);
            v.ir.image_query_dimensions_full(
                handle,
                lod,
                skip_mips,
                TextureInstInfo::default().to_u32(),
            )
        }
        Mode::TextureType | Mode::SamplePos => panic!("Mode {:?}", mode),
    }
}

fn impl_txq(v: &mut TranslatorVisitor, insn: u64, cbuf_offset: Option<u32>) {
    let dest_reg = field(insn, 0, 8);
    let mut src_reg = field(insn, 8, 8);
    let mode = Mode::from_bits(field(insn, 22, 3) as u64);
    let mask = field(insn, 31, 4);
    let handle = if let Some(cbuf_offset) = cbuf_offset {
        Value::ImmU32(cbuf_offset)
    } else {
        let handle = v.x(src_reg);
        src_reg += 1;
        handle
    };
    let query = query(v, handle, mode, src_reg, mask);
    let mut reg = dest_reg;
    for element in 0..4 {
        if ((mask >> element) & 1) == 0 {
            continue;
        }
        let value = v.ir.composite_extract_u32x4(query, Value::ImmU32(element));
        v.set_x(reg, value);
        reg += 1;
    }
}

/// TXQ — Texture Query (bound form).
///
/// Queries texture metadata (dimensions, mip count, etc.) using a
/// cbuf-bound texture handle.
///
/// Upstream: `TranslatorVisitor::TXQ(u64 insn)`
pub fn txq(tv: &mut TranslatorVisitor, insn: u64, _opcode: MaxwellOpcode) {
    let cbuf_offset = field(insn, 36, 13) * 4;
    impl_txq(tv, insn, Some(cbuf_offset));
}

/// TXQ_b — Texture Query (bindless form).
///
/// Upstream: `TranslatorVisitor::TXQ_b(u64 insn)`
pub fn txq_b(tv: &mut TranslatorVisitor, insn: u64, _opcode: MaxwellOpcode) {
    impl_txq(tv, insn, None);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    fn fresh_program() -> Program {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        program
    }

    #[test]
    fn txq_bound_dimension_preserves_mask_and_skip_mips() {
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = 4u64 | (8u64 << 8) | (1u64 << 22) | (0x3u64 << 31) | (7u64 << 36);

        txq(&mut tv, insn, MaxwellOpcode::TXQ);

        let query = tv.ir.program.blocks[0]
            .iter()
            .find(|inst| inst.opcode == Opcode::BoundImageQueryDimensions)
            .expect("TXQ should emit BoundImageQueryDimensions before texture pass");
        assert_eq!(query.args.len(), 3);
        assert_eq!(query.args[0], Value::ImmU32(28));
        assert_eq!(query.args[2], Value::ImmU1(true));

        let stores = tv.ir.program.blocks[0]
            .iter()
            .filter(|inst| inst.opcode == Opcode::SetRegister)
            .count();
        assert_eq!(stores, 2);
    }

    #[test]
    fn txq_bindless_uses_src_handle_and_next_reg_lod() {
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = 4u64 | (8u64 << 8) | (1u64 << 22) | (0x8u64 << 31);

        txq_b(&mut tv, insn, MaxwellOpcode::TXQ_b);

        let query = tv.ir.program.blocks[0]
            .iter()
            .find(|inst| inst.opcode == Opcode::BindlessImageQueryDimensions)
            .expect("TXQ_b should emit BindlessImageQueryDimensions before texture pass");
        assert_eq!(query.args.len(), 3);
        assert!(!query.args[0].is_immediate());
        assert_eq!(query.args[2], Value::ImmU1(false));
    }
}
