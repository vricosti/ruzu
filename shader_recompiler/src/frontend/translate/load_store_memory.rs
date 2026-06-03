// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/load_store_memory.cpp
//!
//! Implements LDG and STG (load/store global memory).

use super::{field, TranslatorVisitor};
use crate::ir::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LoadSize {
    U8,
    S8,
    U16,
    S16,
    B32,
    B64,
    B128,
    U128,
}

impl LoadSize {
    fn from_bits(bits: u32) -> Self {
        match bits {
            0 => Self::U8,
            1 => Self::S8,
            2 => Self::U16,
            3 => Self::S16,
            4 => Self::B32,
            5 => Self::B64,
            6 => Self::B128,
            7 => Self::U128,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StoreSize {
    U8,
    S8,
    U16,
    S16,
    B32,
    B64,
    B128,
}

impl StoreSize {
    fn from_bits(bits: u32) -> Self {
        match bits {
            0 => Self::U8,
            1 => Self::S8,
            2 => Self::U16,
            3 => Self::S16,
            4 => Self::B32,
            5 => Self::B64,
            6 => Self::B128,
            _ => panic!("Invalid STG size {}", bits),
        }
    }
}

fn address(tv: &mut TranslatorVisitor, insn: u64) -> Value {
    let addr_reg = field(insn, 8, 8);
    let extended = field(insn, 45, 1) != 0;
    let addr_offset = if addr_reg == 255 {
        field(insn, 20, 24) as u64
    } else {
        let raw = field(insn, 20, 24);
        let sign_bit = 1u32 << 23;
        if raw & sign_bit != 0 {
            (raw | !((1u32 << 24) - 1)) as i32 as i64 as u64
        } else {
            raw as u64
        }
    };

    let packed = if extended {
        if addr_reg & 1 != 0 {
            panic!("Unaligned LDG/STG address register {}", addr_reg);
        }
        let lo = tv.x(addr_reg);
        let hi = tv.x(addr_reg + 1);
        let vector = tv.ir.composite_construct_u32x2(lo, hi);
        tv.ir.pack_uint_2x32(vector)
    } else {
        let lo = tv.x(addr_reg);
        let vector = tv.ir.composite_construct_u32x2(lo, Value::ImmU32(0));
        tv.ir.pack_uint_2x32(vector)
    };

    tv.ir.iadd_64(packed, Value::ImmU64(addr_offset))
}

/// LDG — Load Global Memory.
///
/// Loads from a global (device) address built from a register plus a
/// sign-extended 24-bit immediate offset.  Supports U8, S8, U16, S16, B32,
/// B64, and B128 widths.
///
/// Upstream: `TranslatorVisitor::LDG(u64 insn)`
pub fn ldg(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let address = address(tv, insn);
    let size = LoadSize::from_bits(field(insn, 48, 3));

    match size {
        LoadSize::U8 | LoadSize::S8 | LoadSize::U16 | LoadSize::S16 | LoadSize::B32 => {
            let val = tv.ir.load_global_32(address);
            tv.set_x(dst, val);
        }
        LoadSize::B64 => {
            if dst & 1 != 0 {
                panic!("Unaligned LDG data register {}", dst);
            }
            let vector = tv.ir.load_global_64(address);
            for index in 0..2 {
                let val = tv.ir.composite_extract_u32x2(vector, Value::ImmU32(index));
                tv.set_x(dst + index, val);
            }
        }
        LoadSize::B128 | LoadSize::U128 => {
            if dst & 3 != 0 {
                panic!("Unaligned LDG data register {}", dst);
            }
            let vector = tv.ir.load_global_128(address);
            for index in 0..4 {
                let val = tv.ir.composite_extract_u32x4(vector, Value::ImmU32(index));
                tv.set_x(dst + index, val);
            }
        }
    }
}

/// STG — Store Global Memory.
///
/// Stores a register value into a global (device) address built from a
/// register plus a sign-extended 24-bit immediate offset.
///
/// Upstream: `TranslatorVisitor::STG(u64 insn)`
pub fn stg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_data_reg = tv.dst_reg(insn);
    let address = address(tv, insn);
    let size = StoreSize::from_bits(field(insn, 48, 3));

    match size {
        StoreSize::U8 | StoreSize::S8 | StoreSize::U16 | StoreSize::S16 | StoreSize::B32 => {
            let data = tv.x(src_data_reg);
            tv.ir.write_global_32(address, data);
        }
        StoreSize::B64 => {
            if src_data_reg & 1 != 0 {
                panic!("Unaligned STG data register {}", src_data_reg);
            }
            let x = tv.x(src_data_reg);
            let y = tv.x(src_data_reg + 1);
            let vector = tv.ir.composite_construct_u32x2(x, y);
            tv.ir.write_global_64(address, vector);
        }
        StoreSize::B128 => {
            if src_data_reg & 3 != 0 {
                panic!("Unaligned STG data register {}", src_data_reg);
            }
            let x = tv.x(src_data_reg);
            let y = tv.x(src_data_reg + 1);
            let z = tv.x(src_data_reg + 2);
            let w = tv.x(src_data_reg + 3);
            let vector = tv.ir.composite_construct_u32x4(x, y, z, w);
            tv.ir.write_global_128(address, vector);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    fn fresh_program() -> Program {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program
    }

    fn opcodes_emitted(program: &Program) -> Vec<Opcode> {
        program.blocks[0].iter().map(|inst| inst.opcode).collect()
    }

    fn encode_mem(dst: u32, addr: u32, offset: u32, extended: bool, size: u32) -> u64 {
        u64::from(dst)
            | (u64::from(addr) << 8)
            | (u64::from(offset & 0x00ff_ffff) << 20)
            | (u64::from(extended as u32) << 45)
            | (u64::from(size) << 48)
    }

    #[test]
    fn ldg_b64_uses_64_bit_address_and_extracts_two_registers() {
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);

        // size=5 is upstream LoadSize::B64. `.E` must pack an aligned
        // register pair into a 64-bit address, then load/extract two words.
        ldg(&mut tv, encode_mem(2, 4, 0x20, true, 5));

        let opcodes = opcodes_emitted(&program);
        assert!(opcodes.contains(&Opcode::PackUint2x32));
        assert!(opcodes.contains(&Opcode::IAdd64));
        assert!(opcodes.contains(&Opcode::LoadGlobal64));
        assert_eq!(
            opcodes
                .iter()
                .filter(|opcode| **opcode == Opcode::CompositeExtractU32x2)
                .count(),
            2
        );
    }

    #[test]
    fn ldg_b128_uses_load_global_128_and_extracts_four_registers() {
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);

        // size=6 is upstream LoadSize::B128.
        ldg(&mut tv, encode_mem(4, 8, 0, true, 6));

        let opcodes = opcodes_emitted(&program);
        assert!(opcodes.contains(&Opcode::LoadGlobal128));
        assert_eq!(
            opcodes
                .iter()
                .filter(|opcode| **opcode == Opcode::CompositeExtractU32x4)
                .count(),
            4
        );
    }
}
