// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/atomic_operations_shared_memory.cpp
//!
//! ATOMS — Atomic Operation on Shared Memory. Mirrors upstream's
//! anonymous-namespace `ApplyAtomsOp`/`AtomsOffset`/`StoreResult`
//! helpers + the public `TranslatorVisitor::ATOMS` entry.

use super::{field, sfield, TranslatorVisitor};
use crate::ir::value::{Reg, Value};

/// Maxwell AtomOp encoding (4-bit field at [55:52]). Mirrors upstream
/// `AtomOp` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AtomOp {
    Add = 0,
    Min = 1,
    Max = 2,
    Inc = 3,
    Dec = 4,
    And = 5,
    Or = 6,
    Xor = 7,
    Exch = 8,
}

impl AtomOp {
    fn from_bits(v: u32) -> Self {
        match v & 0xF {
            0 => AtomOp::Add,
            1 => AtomOp::Min,
            2 => AtomOp::Max,
            3 => AtomOp::Inc,
            4 => AtomOp::Dec,
            5 => AtomOp::And,
            6 => AtomOp::Or,
            7 => AtomOp::Xor,
            8 => AtomOp::Exch,
            other => panic!("Integer Atoms Operation {}", other),
        }
    }
}

/// Maxwell AtomsSize encoding (2-bit field at [29:28]). Mirrors upstream
/// `AtomsSize`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AtomsSize {
    U32 = 0,
    S32 = 1,
    U64 = 2,
}

impl AtomsSize {
    fn from_bits(v: u32) -> Self {
        match v & 0x3 {
            0 => AtomsSize::U32,
            1 => AtomsSize::S32,
            2 => AtomsSize::U64,
            // Reserved encoding 3 — upstream throws.
            other => panic!("Invalid AtomsSize {}", other),
        }
    }
}

/// Port of upstream `AtomsOffset(TranslatorVisitor& v, u64 insn)`.
fn atoms_offset(tv: &mut TranslatorVisitor, insn: u64) -> Value {
    let offset_reg = field(insn, 8, 8);
    if offset_reg == Reg::RZ.0 as u32 {
        let absolute = (field(insn, 30, 22) << 2) as u32;
        Value::ImmU32(absolute)
    } else {
        let relative = (sfield(insn, 30, 22) << 2) as i32;
        let reg_val = tv.x(offset_reg);
        tv.ir.iadd_32(reg_val, Value::ImmU32(relative as u32))
    }
}

/// Port of upstream `ApplyAtomsOp` — dispatches to the right IR helper
/// based on the 4-bit AtomOp encoding.
fn apply_atoms_op(
    tv: &mut TranslatorVisitor,
    offset: Value,
    op_b: Value,
    op: AtomOp,
    is_signed: bool,
) -> Value {
    match op {
        AtomOp::Add => tv.ir.shared_atomic_iadd_32(offset, op_b),
        AtomOp::Min => tv.ir.shared_atomic_imin_32(offset, op_b, is_signed),
        AtomOp::Max => tv.ir.shared_atomic_imax_32(offset, op_b, is_signed),
        AtomOp::And => tv.ir.shared_atomic_and_32(offset, op_b),
        AtomOp::Or => tv.ir.shared_atomic_or_32(offset, op_b),
        AtomOp::Xor => tv.ir.shared_atomic_xor_32(offset, op_b),
        AtomOp::Exch => tv.ir.shared_atomic_exchange_32(offset, op_b),
        // Inc/Dec atomics need a CAS loop helper not present in ruzu IR;
        // upstream uses `ir.SharedAtomicInc/Dec`. Panic to match upstream
        // when those IR helpers throw.
        AtomOp::Inc | AtomOp::Dec => panic!("Integer Atoms Inc/Dec not implemented"),
    }
}

/// ATOMS — Atomic Operation on Shared Memory.
///
/// Port of upstream `TranslatorVisitor::ATOMS(u64 insn)`.
pub fn atoms(tv: &mut TranslatorVisitor<'_>, insn: u64) {
    let dest_reg = tv.dst_reg(insn);
    let src_reg_b = field(insn, 20, 8);
    let size = AtomsSize::from_bits(field(insn, 28, 2));
    let op = AtomOp::from_bits(field(insn, 52, 4));

    let size_64 = size == AtomsSize::U64;
    if size_64 && op != AtomOp::Exch {
        panic!("64-bit Atoms Operation {:?}", op);
    }
    let is_signed = size == AtomsSize::S32;
    let offset = atoms_offset(tv, insn);

    if size_64 {
        // U64 path requires `L()` register-pair load and an `IR::U64`
        // atomic — not wired in ruzu's IR. Panic to match upstream
        // until it's ported.
        panic!("ATOMS 64-bit not implemented");
    }
    let op_b = tv.x(src_reg_b);
    let result = apply_atoms_op(tv, offset, op_b, op, is_signed);
    tv.set_x(dest_reg, result);
}

/// ATOMS_CAS — Atomic Compare-and-Swap on Shared Memory.
///
/// Upstream does not implement ATOMS_CAS as a real instruction —
/// it's covered by `not_implemented.cpp::ATOMS_cas` which throws.
pub fn atoms_cas(_tv: &mut TranslatorVisitor<'_>, _insn: u64) {
    panic!("ATOMS_cas not implemented (upstream NotImplementedException)");
}
