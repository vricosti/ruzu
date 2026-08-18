use super::helpers::{emit_imm_shift, get_address};
use crate::frontend::a32::decoder::DecodedArm;
use crate::frontend::a32::types::Reg;
use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::acc_type::AccType;
use crate::ir::terminal::Terminal;
use crate::ir::value::Value;

// --- LDR ---

pub fn arm_ldr_imm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let imm12 = inst.imm12();

    let offset = Value::ImmU32(imm12);
    let address = if rn == Reg::R15 {
        // PC-relative (literal)
        let base = Value::ImmU32(ir.pc() & !3); // Align PC to 4 bytes
        if u {
            ir.ir().add_32(base, offset, Value::ImmU1(false))
        } else {
            ir.ir().sub_32(base, offset, Value::ImmU1(true))
        }
    } else {
        get_address(ir, p, u, w, rn, offset)
    };

    let value = ir.read_memory_32(address, AccType::Normal);

    if rt == Reg::R15 {
        ir.load_write_pc(value);
        if !p && w && rn == Reg::R13 {
            ir.set_term(Terminal::PopRSBHint);
        } else {
            ir.set_term(Terminal::FastDispatchHint);
        }
        return false;
    }

    ir.set_register(rt, value);
    true
}

pub fn arm_ldr_reg(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let rm = inst.rm();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let shift_type = inst.shift_type();
    let imm5 = inst.imm5();

    let carry_in = ir.get_c_flag();
    let rm_val = ir.get_register(rm);
    let (offset, _) = emit_imm_shift(ir, rm_val, shift_type, imm5, carry_in);
    let address = get_address(ir, p, u, w, rn, offset);

    let value = ir.read_memory_32(address, AccType::Normal);

    if rt == Reg::R15 {
        ir.load_write_pc(value);
        ir.set_term(Terminal::FastDispatchHint);
        return false;
    }

    ir.set_register(rt, value);
    true
}

// --- STR ---

pub fn arm_str_imm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let imm12 = inst.imm12();

    let offset = Value::ImmU32(imm12);
    let address = get_address(ir, p, u, w, rn, offset);
    let value = ir.get_register(rt);
    ir.write_memory_32(address, value, AccType::Normal);
    true
}

pub fn arm_str_reg(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let rm = inst.rm();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let shift_type = inst.shift_type();
    let imm5 = inst.imm5();

    let carry_in = ir.get_c_flag();
    let rm_val = ir.get_register(rm);
    let (offset, _) = emit_imm_shift(ir, rm_val, shift_type, imm5, carry_in);
    let address = get_address(ir, p, u, w, rn, offset);
    let value = ir.get_register(rt);
    ir.write_memory_32(address, value, AccType::Normal);
    true
}

// --- LDRB ---

pub fn arm_ldrb_imm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let imm12 = inst.imm12();

    let offset = Value::ImmU32(imm12);
    let address = if rn == Reg::R15 {
        let base = Value::ImmU32(ir.pc() & !3);
        if u {
            ir.ir().add_32(base, offset, Value::ImmU1(false))
        } else {
            ir.ir().sub_32(base, offset, Value::ImmU1(true))
        }
    } else {
        get_address(ir, p, u, w, rn, offset)
    };

    let value = ir.read_memory_8(address, AccType::Normal);
    let extended = ir.ir().zero_extend_byte_to_word(value);
    ir.set_register(rt, extended);
    true
}

pub fn arm_ldrb_reg(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let rm = inst.rm();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let shift_type = inst.shift_type();
    let imm5 = inst.imm5();

    let carry_in = ir.get_c_flag();
    let rm_val = ir.get_register(rm);
    let (offset, _) = emit_imm_shift(ir, rm_val, shift_type, imm5, carry_in);
    let address = get_address(ir, p, u, w, rn, offset);

    let value = ir.read_memory_8(address, AccType::Normal);
    let extended = ir.ir().zero_extend_byte_to_word(value);
    ir.set_register(rt, extended);
    true
}

// --- STRB ---

pub fn arm_strb_imm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let imm12 = inst.imm12();

    let offset = Value::ImmU32(imm12);
    let address = get_address(ir, p, u, w, rn, offset);
    let value = ir.get_register(rt);
    let byte = ir.ir().least_significant_byte(value);
    ir.write_memory_8(address, byte, AccType::Normal);
    true
}

pub fn arm_strb_reg(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let rm = inst.rm();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let shift_type = inst.shift_type();
    let imm5 = inst.imm5();

    let carry_in = ir.get_c_flag();
    let rm_val = ir.get_register(rm);
    let (offset, _) = emit_imm_shift(ir, rm_val, shift_type, imm5, carry_in);
    let address = get_address(ir, p, u, w, rn, offset);
    let value = ir.get_register(rt);
    let byte = ir.ir().least_significant_byte(value);
    ir.write_memory_8(address, byte, AccType::Normal);
    true
}

// --- LDRH ---

pub fn arm_ldrh_imm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    // For extra load/store, immediate is imm4H:imm4L
    let imm4h = (inst.raw >> 8) & 0xF;
    let imm4l = inst.raw & 0xF;
    let imm8 = (imm4h << 4) | imm4l;

    let offset = Value::ImmU32(imm8);
    let address = if rn == Reg::R15 {
        let base = Value::ImmU32(ir.pc() & !3);
        if u {
            ir.ir().add_32(base, offset, Value::ImmU1(false))
        } else {
            ir.ir().sub_32(base, offset, Value::ImmU1(true))
        }
    } else {
        get_address(ir, p, u, w, rn, offset)
    };

    let value = ir.read_memory_16(address, AccType::Normal);
    let extended = ir.ir().zero_extend_half_to_word(value);
    ir.set_register(rt, extended);
    true
}

pub fn arm_ldrh_reg(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let rm = inst.rm();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();

    let offset = ir.get_register(rm);
    let address = get_address(ir, p, u, w, rn, offset);

    let value = ir.read_memory_16(address, AccType::Normal);
    let extended = ir.ir().zero_extend_half_to_word(value);
    ir.set_register(rt, extended);
    true
}

// --- STRH ---

pub fn arm_strh_imm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let imm4h = (inst.raw >> 8) & 0xF;
    let imm4l = inst.raw & 0xF;
    let imm8 = (imm4h << 4) | imm4l;

    let offset = Value::ImmU32(imm8);
    let address = get_address(ir, p, u, w, rn, offset);
    let value = ir.get_register(rt);
    let half = ir.ir().least_significant_half(value);
    ir.write_memory_16(address, half, AccType::Normal);
    true
}

pub fn arm_strh_reg(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let rm = inst.rm();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();

    let offset = ir.get_register(rm);
    let address = get_address(ir, p, u, w, rn, offset);
    let value = ir.get_register(rt);
    let half = ir.ir().least_significant_half(value);
    ir.write_memory_16(address, half, AccType::Normal);
    true
}

// --- LDRSB ---

pub fn arm_ldrsb_imm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let imm4h = (inst.raw >> 8) & 0xF;
    let imm4l = inst.raw & 0xF;
    let imm8 = (imm4h << 4) | imm4l;

    let offset = Value::ImmU32(imm8);
    let address = if rn == Reg::R15 {
        let base = Value::ImmU32(ir.pc() & !3);
        if u {
            ir.ir().add_32(base, offset, Value::ImmU1(false))
        } else {
            ir.ir().sub_32(base, offset, Value::ImmU1(true))
        }
    } else {
        get_address(ir, p, u, w, rn, offset)
    };

    let value = ir.read_memory_8(address, AccType::Normal);
    let extended = ir.ir().sign_extend_byte_to_word(value);
    ir.set_register(rt, extended);
    true
}

pub fn arm_ldrsb_reg(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let rm = inst.rm();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();

    let offset = ir.get_register(rm);
    let address = get_address(ir, p, u, w, rn, offset);

    let value = ir.read_memory_8(address, AccType::Normal);
    let extended = ir.ir().sign_extend_byte_to_word(value);
    ir.set_register(rt, extended);
    true
}

// --- LDRSH ---

pub fn arm_ldrsh_imm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let imm4h = (inst.raw >> 8) & 0xF;
    let imm4l = inst.raw & 0xF;
    let imm8 = (imm4h << 4) | imm4l;

    let offset = Value::ImmU32(imm8);
    let address = if rn == Reg::R15 {
        let base = Value::ImmU32(ir.pc() & !3);
        if u {
            ir.ir().add_32(base, offset, Value::ImmU1(false))
        } else {
            ir.ir().sub_32(base, offset, Value::ImmU1(true))
        }
    } else {
        get_address(ir, p, u, w, rn, offset)
    };

    let value = ir.read_memory_16(address, AccType::Normal);
    let extended = ir.ir().sign_extend_half_to_word(value);
    ir.set_register(rt, extended);
    true
}

pub fn arm_ldrsh_reg(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rn = inst.rn();
    let rm = inst.rm();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();

    let offset = ir.get_register(rm);
    let address = get_address(ir, p, u, w, rn, offset);

    let value = ir.read_memory_16(address, AccType::Normal);
    let extended = ir.ir().sign_extend_half_to_word(value);
    ir.set_register(rt, extended);
    true
}

// --- LDRD ---

pub fn arm_ldrd_imm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rt2 = Reg::from_u32((rt as u32) + 1);
    let rn = inst.rn();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let imm4h = (inst.raw >> 8) & 0xF;
    let imm4l = inst.raw & 0xF;
    let imm8 = (imm4h << 4) | imm4l;

    let offset = Value::ImmU32(imm8);
    let address = if rn == Reg::R15 {
        let base = Value::ImmU32(ir.pc() & !3);
        if u {
            ir.ir().add_32(base, offset, Value::ImmU1(false))
        } else {
            ir.ir().sub_32(base, offset, Value::ImmU1(true))
        }
    } else {
        get_address(ir, p, u, w, rn, offset)
    };

    // Upstream `arm_LDRD_imm` issues a single 64-bit ATOMIC read and splits
    // the result across the two destination registers, with most/least
    // significant word ordering driven by CPSR.E. Two separate 32-bit Normal
    // reads break atomicity and the backend pattern matchers that fold
    // adjacent halves back into 64-bit ops, so the game's 64-bit atomic
    // accesses behave incorrectly. Match the upstream emit shape exactly.
    let data = ir.read_memory_64(address, AccType::Atomic);
    let e_flag = ir
        .current_location
        .expect("current_location not set")
        .e_flag();
    let lo_word = ir.ir().least_significant_word(data);
    let hi_word = ir.ir().most_significant_word(data);
    if e_flag {
        ir.set_register(rt, hi_word);
        ir.set_register(rt2, lo_word);
    } else {
        ir.set_register(rt, lo_word);
        ir.set_register(rt2, hi_word);
    }
    true
}

pub fn arm_ldrd_reg(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rt2 = Reg::from_u32((rt as u32) + 1);
    let rn = inst.rn();
    let rm = inst.rm();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();

    let offset = ir.get_register(rm);
    let address = get_address(ir, p, u, w, rn, offset);

    // Match upstream `arm_LDRD_reg`: single 64-bit ATOMIC read, split
    // most/least significant word per CPSR.E.
    let data = ir.read_memory_64(address, AccType::Atomic);
    let e_flag = ir
        .current_location
        .expect("current_location not set")
        .e_flag();
    let lo_word = ir.ir().least_significant_word(data);
    let hi_word = ir.ir().most_significant_word(data);
    if e_flag {
        ir.set_register(rt, hi_word);
        ir.set_register(rt2, lo_word);
    } else {
        ir.set_register(rt, lo_word);
        ir.set_register(rt2, hi_word);
    }
    true
}

// --- STRD ---

pub fn arm_strd_imm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rt2 = Reg::from_u32((rt as u32) + 1);
    let rn = inst.rn();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();
    let imm4h = (inst.raw >> 8) & 0xF;
    let imm4l = inst.raw & 0xF;
    let imm8 = (imm4h << 4) | imm4l;

    let offset = Value::ImmU32(imm8);
    let address = get_address(ir, p, u, w, rn, offset);

    // Match upstream `arm_STRD_imm`: pack the two source registers into a
    // single 64-bit value (low/high order driven by CPSR.E) and emit one
    // 64-bit ATOMIC write. Two separate 32-bit Normal writes break atomicity
    // and prevent the backend from recognising 64-bit atomic accesses.
    let value_a = ir.get_register(rt);
    let value_b = ir.get_register(rt2);
    let e_flag = ir
        .current_location
        .expect("current_location not set")
        .e_flag();
    let data = if e_flag {
        ir.ir().pack_2x32_to_1x64(value_b, value_a)
    } else {
        ir.ir().pack_2x32_to_1x64(value_a, value_b)
    };
    ir.write_memory_64(address, data, AccType::Atomic);
    true
}

pub fn arm_strd_reg(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let rt2 = Reg::from_u32((rt as u32) + 1);
    let rn = inst.rn();
    let rm = inst.rm();
    let p = inst.p_flag();
    let u = inst.u_flag();
    let w = inst.w_flag();

    let offset = ir.get_register(rm);
    let address = get_address(ir, p, u, w, rn, offset);

    // Match upstream `arm_STRD_reg`: single 64-bit ATOMIC write of the packed
    // register pair, with low/high order driven by CPSR.E.
    let value_a = ir.get_register(rt);
    let value_b = ir.get_register(rt2);
    let e_flag = ir
        .current_location
        .expect("current_location not set")
        .e_flag();
    let data = if e_flag {
        ir.ir().pack_2x32_to_1x64(value_b, value_a)
    } else {
        ir.ir().pack_2x32_to_1x64(value_a, value_b)
    };
    ir.write_memory_64(address, data, AccType::Atomic);
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::a32::decoder::ArmInstId;
    use crate::frontend::a32::fpscr::FPSCR;
    use crate::frontend::a32::psr::PSR;
    use crate::ir::block::Block;
    use crate::ir::location::A32LocationDescriptor;
    use crate::ir::opcode::Opcode;

    #[test]
    fn arm_strd_imm_emits_single_atomic_write_memory_64() {
        let loc = A32LocationDescriptor::new(0x1000, PSR::default(), FPSCR::default(), false);
        let mut block = Block::new(loc.to_location());
        let mut ir = A32IREmitter::with_location(&mut block, loc);
        let inst = DecodedArm {
            raw: 0xE1A1_20F4,
            id: ArmInstId::STRD_imm,
        };

        assert!(arm_strd_imm(&mut ir, &inst));
        assert_eq!(
            block
                .instructions
                .iter()
                .filter(|inst| inst.opcode == Opcode::A32WriteMemory64)
                .count(),
            1
        );
        assert_eq!(
            block
                .instructions
                .iter()
                .filter(|inst| inst.opcode == Opcode::A32WriteMemory32)
                .count(),
            0
        );
    }

    #[test]
    fn arm_ldrd_imm_emits_single_atomic_read_memory_64() {
        let loc = A32LocationDescriptor::new(0x1000, PSR::default(), FPSCR::default(), false);
        let mut block = Block::new(loc.to_location());
        let mut ir = A32IREmitter::with_location(&mut block, loc);
        let inst = DecodedArm {
            raw: 0xE1B1_20D4,
            id: ArmInstId::LDRD_imm,
        };

        assert!(arm_ldrd_imm(&mut ir, &inst));
        assert_eq!(
            block
                .instructions
                .iter()
                .filter(|inst| inst.opcode == Opcode::A32ReadMemory64)
                .count(),
            1
        );
        assert_eq!(
            block
                .instructions
                .iter()
                .filter(|inst| inst.opcode == Opcode::A32ReadMemory32)
                .count(),
            0
        );
    }
}
