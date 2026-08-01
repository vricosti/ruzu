// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/output_geometry.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Value;

fn out(tv: &mut TranslatorVisitor<'_>, insn: u64, stream_index: Value) {
    let dest_reg = field(insn, 0, 8);
    let emit = bit(insn, 39);
    let cut = bit(insn, 40);
    let stream_index = tv.ir.bitwise_and_32(stream_index, Value::ImmU32(0b11));

    if emit {
        tv.ir.emit_vertex(stream_index.clone());
    }
    if cut {
        tv.ir.end_primitive(stream_index);
    }
    tv.set_x(dest_reg, Value::ImmU32(0));
}

pub fn out_reg(tv: &mut TranslatorVisitor<'_>, insn: u64) {
    let stream_index = tv.get_reg20(insn);
    out(tv, insn, stream_index);
}

pub fn out_cbuf(tv: &mut TranslatorVisitor<'_>, insn: u64) {
    let stream_index = tv.get_cbuf(insn);
    out(tv, insn, stream_index);
}

pub fn out_imm(tv: &mut TranslatorVisitor<'_>, insn: u64) {
    let stream_index = tv.get_imm20(insn);
    out(tv, insn, stream_index);
}
