// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Memory operation translation: LDG, STG, LDC, LDL, STL, LDS, STS.

use super::{field, TranslatorVisitor};
use crate::ir::value::Value;

pub fn ldg(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_reg = tv.src_a_reg(insn);
    let offset = field(insn, 20, 24);
    // Sign-extend 24-bit offset
    let offset = if offset & 0x800000 != 0 {
        (offset | 0xFF000000) as i32
    } else {
        offset as i32
    };

    // Build 64-bit address from register pair + offset
    let base_lo = tv.x(src_reg);
    let base_hi = tv.x(src_reg + 1);
    let addr = tv.ir.composite_construct_u32x2(base_lo, base_hi);
    // Add offset (simplified: just use lo for now)
    let addr_with_offset = tv.ir.iadd_32(base_lo, Value::ImmU32(offset as u32));

    // Load size from bits [48:46]
    let size = field(insn, 46, 3);
    match size {
        0 => {
            // U8
            let val = tv.ir.load_global_32(Value::Inst(addr_with_offset.inst_ref()));
            tv.set_x(dst, val);
        }
        2 => {
            // U32 (32 bits)
            let val = tv.ir.load_global_32(Value::Inst(addr_with_offset.inst_ref()));
            tv.set_x(dst, val);
        }
        _ => {
            // Default to 32-bit load
            let val = tv.ir.load_global_32(Value::Inst(addr_with_offset.inst_ref()));
            tv.set_x(dst, val);
        }
    }
}

pub fn stg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_data_reg = tv.dst_reg(insn);
    let src_addr_reg = tv.src_a_reg(insn);
    let offset = field(insn, 20, 24);
    let offset = if offset & 0x800000 != 0 {
        (offset | 0xFF000000) as i32
    } else {
        offset as i32
    };

    let base_lo = tv.x(src_addr_reg);
    let addr = tv.ir.iadd_32(base_lo, Value::ImmU32(offset as u32));
    let data = tv.x(src_data_reg);

    tv.ir.write_global_32(Value::Inst(addr.inst_ref()), data);
}

pub fn ldc(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let cb_index = field(insn, 36, 5);
    let offset_reg = tv.src_a_reg(insn);
    let imm_offset = field(insn, 20, 16);

    let offset = tv.x(offset_reg);
    let final_offset = tv.ir.iadd_32(offset, Value::ImmU32(imm_offset));

    tv.ir.program.info.register_cbuf(cb_index);
    let result = tv.ir.get_cbuf_u32(Value::ImmU32(cb_index), final_offset);

    tv.set_x(dst, result);
}

pub fn ldl(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_reg = tv.src_a_reg(insn);
    let offset = field(insn, 20, 24);

    let base = tv.x(src_reg);
    let addr = tv.ir.iadd_32(base, Value::ImmU32(offset));
    let result = tv.ir.load_local(addr);

    tv.set_x(dst, result);
}

pub fn stl(tv: &mut TranslatorVisitor, insn: u64) {
    let src_data_reg = tv.dst_reg(insn);
    let src_addr_reg = tv.src_a_reg(insn);
    let offset = field(insn, 20, 24);

    let base = tv.x(src_addr_reg);
    let addr = tv.ir.iadd_32(base, Value::ImmU32(offset));
    let data = tv.x(src_data_reg);

    tv.ir.write_local(addr, data);
}
