// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_floating_point_conversion.cpp

use super::common_encoding::{cast_fp_rounding, MaxwellFpRounding};
use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::types::FpControl;
use crate::ir::value::Value;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FloatFormat {
    F16 = 1,
    F32 = 2,
    F64 = 3,
}

impl FloatFormat {
    fn decode(value: u32) -> Self {
        match value {
            1 => Self::F16,
            2 => Self::F32,
            3 => Self::F64,
            _ => panic!("Invalid float format {}", value),
        }
    }

    fn bit_size(self) -> u32 {
        match self {
            Self::F16 => 16,
            Self::F32 => 32,
            Self::F64 => 64,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IntFormat {
    U8 = 0,
    U16 = 1,
    U32 = 2,
    U64 = 3,
}

impl IntFormat {
    fn decode(value: u32) -> Self {
        match value {
            0 => Self::U8,
            1 => Self::U16,
            2 => Self::U32,
            3 => Self::U64,
            _ => unreachable!("2-bit integer format"),
        }
    }
}

fn small_abs(tv: &mut TranslatorVisitor<'_>, value: Value, bit_size: u32) -> Value {
    let least_value = Value::ImmU32((-(1i32 << (bit_size - 1))) as u32);
    let mask = tv
        .ir
        .shift_right_arithmetic_32(value.clone(), Value::ImmU32(bit_size - 1));
    let sum = tv.ir.iadd_32(value.clone(), mask.clone());
    let absolute = tv.ir.bitwise_xor_32(sum, mask);
    let is_least = tv.ir.i_equal(value.clone(), least_value);
    tv.ir.select_u32(is_least, value, absolute)
}

fn decode_source(
    tv: &mut TranslatorVisitor<'_>,
    insn: u64,
    opcode: MaxwellOpcode,
    format: IntFormat,
) -> Value {
    let is_64 = format == IntFormat::U64;
    match opcode {
        MaxwellOpcode::I2F_reg if is_64 => {
            let reg = field(insn, 20, 8);
            let lo = tv.x(reg);
            let hi = tv.x(reg + 1);
            let pair = tv.ir.composite_construct_u32x2(lo, hi);
            tv.ir.pack_uint_2x32(pair)
        }
        MaxwellOpcode::I2F_reg => tv.get_reg20(insn),
        MaxwellOpcode::I2F_cbuf if is_64 => tv.get_packed_cbuf(insn),
        MaxwellOpcode::I2F_cbuf => tv.get_cbuf(insn),
        MaxwellOpcode::I2F_imm if is_64 => tv.get_packed_imm20(insn),
        MaxwellOpcode::I2F_imm => tv.get_imm20(insn),
        _ => panic!("Invalid opcode {:?} for I2F", opcode),
    }
}

pub fn i2f(tv: &mut TranslatorVisitor<'_>, insn: u64, opcode: MaxwellOpcode) {
    let dest_reg = field(insn, 0, 8);
    let float_format = FloatFormat::decode(field(insn, 8, 2));
    let int_format = IntFormat::decode(field(insn, 10, 2));
    let is_signed = bit(insn, 13);
    let fp_rounding = MaxwellFpRounding::from_field(field(insn, 39, 2));
    let selector = field(insn, 41, 2);
    let cc = bit(insn, 47);
    let neg = bit(insn, 45);
    let abs = bit(insn, 49);

    if cc {
        panic!("I2F CC not implemented");
    }

    let mut src = decode_source(tv, insn, opcode, int_format);
    let src_bit_size = match int_format {
        IntFormat::U8 => {
            src = if is_signed {
                tv.ir
                    .bit_field_s_extract(src, Value::ImmU32(selector * 8), Value::ImmU32(8))
            } else {
                tv.ir
                    .bit_field_u_extract(src, Value::ImmU32(selector * 8), Value::ImmU32(8))
            };
            if abs {
                src = small_abs(tv, src, 8);
            }
            8
        }
        IntFormat::U16 => {
            if selector == 1 || selector == 3 {
                panic!("Invalid U16 selector {}", selector);
            }
            src = if is_signed {
                tv.ir
                    .bit_field_s_extract(src, Value::ImmU32(selector * 8), Value::ImmU32(16))
            } else {
                tv.ir
                    .bit_field_u_extract(src, Value::ImmU32(selector * 8), Value::ImmU32(16))
            };
            if abs {
                src = small_abs(tv, src, 16);
            }
            16
        }
        IntFormat::U32 | IntFormat::U64 => {
            if selector != 0 {
                panic!("Unexpected selector {}", selector);
            }
            if abs && is_signed {
                src = if int_format == IntFormat::U64 {
                    tv.ir.iabs_64(src)
                } else {
                    tv.ir.iabs_32(src)
                };
            }
            if int_format == IntFormat::U64 {
                64
            } else {
                32
            }
        }
    };

    let conversion_src_bit_size = if int_format == IntFormat::U64 { 64 } else { 32 };
    let fp_control = FpControl {
        no_contraction: false,
        rounding: cast_fp_rounding(fp_rounding),
        fmz_mode: crate::ir::types::FmzMode::DontCare,
    };
    let mut value = tv.ir.convert_i_to_f(
        float_format.bit_size(),
        conversion_src_bit_size,
        is_signed,
        src.clone(),
        fp_control,
    );

    if neg {
        let negated = match float_format {
            FloatFormat::F16 => tv.ir.fp_neg_16(value.clone()),
            FloatFormat::F32 => tv.ir.fp_neg_32(value.clone()),
            FloatFormat::F64 => tv.ir.fp_neg_64(value.clone()),
        };
        if abs || !is_signed {
            value = negated;
        } else {
            let is_least = if src_bit_size == 64 {
                tv.ir.i_equal(src, Value::ImmU64(i64::MIN as u64))
            } else {
                let least = if src_bit_size == 32 {
                    i32::MIN
                } else {
                    -(1i32 << (src_bit_size - 1))
                };
                tv.ir.i_equal(src, Value::ImmU32(least as u32))
            };
            value = match float_format {
                FloatFormat::F16 => tv.ir.select_f16(is_least, value, negated),
                FloatFormat::F32 => tv.ir.select_f32(is_least, value, negated),
                FloatFormat::F64 => tv.ir.select_f64(is_least, value, negated),
            };
        }
    }

    match float_format {
        FloatFormat::F16 => {
            let zero = tv
                .ir
                .fp_convert(16, Value::ImmF32(0.0), 32, FpControl::default());
            let pair = tv.ir.composite_construct_f16x2(value, zero);
            let packed = tv.ir.pack_float_2x16(pair);
            tv.set_x(dest_reg, packed);
        }
        FloatFormat::F32 => tv.set_f(dest_reg, value),
        FloatFormat::F64 => {
            if dest_reg & 1 != 0 {
                panic!("Unaligned destination {}", dest_reg);
            }
            tv.set_d(dest_reg, value);
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

    fn translate(insn: u64, opcode: MaxwellOpcode) -> Program {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        {
            let mut tv = TranslatorVisitor::new(&mut program, 0);
            i2f(&mut tv, insn, opcode);
        }
        program
    }

    #[test]
    fn i2f_u8_extracts_selected_byte_before_conversion() {
        let insn = (2u64 << 8) | (2u64 << 41) | (3u64 << 20);
        let program = translate(insn, MaxwellOpcode::I2F_reg);
        let block = &program.blocks[0];
        let extract = block
            .iter()
            .find(|inst| inst.opcode == Opcode::BitFieldUExtract)
            .expect("I2F.U8 must extract the selected byte");
        assert_eq!(extract.args[1], Value::ImmU32(16));
        assert_eq!(extract.args[2], Value::ImmU32(8));
        assert!(block
            .iter()
            .any(|inst| inst.opcode == Opcode::ConvertF32U32));
    }

    #[test]
    fn i2f_s8_uses_signed_bitfield_extract() {
        let insn = (2u64 << 8) | (1u64 << 13) | (3u64 << 41) | (3u64 << 20);
        let program = translate(insn, MaxwellOpcode::I2F_reg);
        let extract = program.blocks[0]
            .iter()
            .find(|inst| inst.opcode == Opcode::BitFieldSExtract)
            .expect("I2F.S8 must sign-extend the selected byte");
        assert_eq!(extract.args[1], Value::ImmU32(24));
        assert_eq!(extract.args[2], Value::ImmU32(8));
    }

    #[test]
    #[should_panic(expected = "Invalid U16 selector 1")]
    fn i2f_rejects_odd_u16_selector_like_upstream() {
        let insn = (2u64 << 8) | (1u64 << 10) | (1u64 << 41) | (3u64 << 20);
        let _ = translate(insn, MaxwellOpcode::I2F_reg);
    }

    #[test]
    fn i2f_u64_register_source_is_packed_from_two_registers() {
        let insn = (3u64 << 8) | (3u64 << 10) | (1u64 << 13) | (4u64 << 20);
        let program = translate(insn, MaxwellOpcode::I2F_reg);
        let opcodes: Vec<_> = program.blocks[0].iter().map(|inst| inst.opcode).collect();
        assert!(opcodes.contains(&Opcode::PackUint2x32));
        assert!(opcodes.contains(&Opcode::ConvertF64S64));
        assert!(opcodes.contains(&Opcode::UnpackDouble2x32));
    }
}
