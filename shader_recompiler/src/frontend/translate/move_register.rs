// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/move_register.cpp

use super::{field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

pub fn mov(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let mask = field(insn, 39, 4);
    if mask != 0xf && mask != 0x1 {
        log::warn!("(STUBBED) Masked Mov");
        return;
    }
    let src = match opcode {
        MaxwellOpcode::MOV_reg => {
            let src_reg = field(insn, 20, 8);
            tv.x(src_reg)
        }
        MaxwellOpcode::MOV_cbuf => tv.get_cbuf(insn),
        MaxwellOpcode::MOV_imm => tv.get_imm20(insn),
        _ => unreachable!("invalid MOV opcode {opcode:?}"),
    };
    tv.set_x(dst, src);
}

pub fn mov32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let mask = field(insn, 12, 4);
    if mask != 0xf && mask != 0x1 {
        log::warn!("(STUBBED) Masked Mov");
        return;
    }
    let imm = tv.decode_imm32(insn);
    tv.set_x(dst, Value::ImmU32(imm));
}
