use rxbyak::dword_ptr;
use rxbyak::JmpType;
use rxbyak::RegExp;
use rxbyak::R15;

use crate::backend::x64::emit_context::EmitContext;
use crate::backend::x64::jit_state::A64JitState;
use crate::backend::x64::reg_alloc::RegAlloc;
use crate::ir::inst::Inst;
use crate::ir::value::InstRef;

// ---------------------------------------------------------------------------
// Helper: OR the QC flag in jit_state
// ---------------------------------------------------------------------------

/// `or [r15 + fpsr_qc], 1` — sets the QC (saturation) sticky flag.
fn set_qc_flag(ra: &mut RegAlloc) {
    let offset = A64JitState::offset_of_fpsr_qc();
    ra.asm
        .or_(dword_ptr(RegExp::from(R15) + offset as i32), 1)
        .unwrap();
}

// ---------------------------------------------------------------------------
// Signed saturated add: result = clamp(a + b, MIN, MAX), set QC on overflow
// ---------------------------------------------------------------------------

fn emit_signed_saturated_add(ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst, bitsize: usize) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_gpr(&mut args[0]);
    let op2 = ra.use_gpr(&mut args[1]);

    // Compute saturation value: sign bit of result → sat_val = MAX or MIN
    let sat_val = ra.scratch_gpr();
    match bitsize {
        8 => {
            ra.asm
                .mov(sat_val.cvt32().unwrap(), result.cvt32().unwrap())
                .unwrap();
            ra.asm.sar(sat_val.cvt32().unwrap(), 7u8).unwrap(); // all 1s or 0s based on sign
            ra.asm.xor_(sat_val.cvt8().unwrap(), 0x7Fi32).unwrap(); // MAX if positive, MIN if negative
            ra.asm
                .add(result.cvt8().unwrap(), op2.cvt8().unwrap())
                .unwrap();
        }
        16 => {
            ra.asm
                .mov(sat_val.cvt32().unwrap(), result.cvt32().unwrap())
                .unwrap();
            ra.asm.sar(sat_val.cvt32().unwrap(), 15u8).unwrap();
            ra.asm.xor_(sat_val.cvt16().unwrap(), 0x7FFFi32).unwrap();
            ra.asm
                .add(result.cvt16().unwrap(), op2.cvt16().unwrap())
                .unwrap();
        }
        32 => {
            // BT result, 31 → CF = sign, ADC sat_val, 0x7FFF_FFFE → 0x7FFF_FFFF or 0x8000_0000
            ra.asm.bt_imm(result, 31).unwrap();
            ra.asm
                .mov(sat_val.cvt32().unwrap(), 0x7FFF_FFFEi32)
                .unwrap();
            ra.asm.adc(sat_val.cvt32().unwrap(), 0i32).unwrap();
            ra.asm
                .add(result.cvt32().unwrap(), op2.cvt32().unwrap())
                .unwrap();
        }
        64 => {
            ra.asm.bt_imm(result, 63).unwrap();
            ra.asm.mov(sat_val, 0x7FFF_FFFF_FFFF_FFFEi64).unwrap();
            ra.asm.adc(sat_val, 0i32).unwrap();
            ra.asm.add(result, op2).unwrap();
        }
        _ => unreachable!(),
    }

    // On overflow (OF=1), use the saturation value instead
    ra.asm.cmovo(result, sat_val).unwrap();

    // Set QC flag if overflow occurred
    let label_no_overflow = ra.asm.create_label();
    ra.asm.jno(&label_no_overflow, JmpType::Near).unwrap();
    set_qc_flag(ra);
    ra.asm.bind(&label_no_overflow).unwrap();

    ra.release(sat_val);
    ra.define_value(inst_ref, result);
}

pub fn emit_signed_saturated_add8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_signed_saturated_add(ra, inst_ref, inst, 8);
}
pub fn emit_signed_saturated_add16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_signed_saturated_add(ra, inst_ref, inst, 16);
}
pub fn emit_signed_saturated_add32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_signed_saturated_add(ra, inst_ref, inst, 32);
}
pub fn emit_signed_saturated_add64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_signed_saturated_add(ra, inst_ref, inst, 64);
}

// ---------------------------------------------------------------------------
// Signed saturated sub: result = clamp(a - b, MIN, MAX), set QC on overflow
// ---------------------------------------------------------------------------

fn emit_signed_saturated_sub(ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst, bitsize: usize) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_gpr(&mut args[0]);
    let op2 = ra.use_gpr(&mut args[1]);

    let sat_val = ra.scratch_gpr();
    match bitsize {
        8 => {
            ra.asm
                .mov(sat_val.cvt32().unwrap(), result.cvt32().unwrap())
                .unwrap();
            ra.asm.sar(sat_val.cvt32().unwrap(), 7u8).unwrap();
            ra.asm.xor_(sat_val.cvt8().unwrap(), 0x7Fi32).unwrap();
            ra.asm
                .sub(result.cvt8().unwrap(), op2.cvt8().unwrap())
                .unwrap();
        }
        16 => {
            ra.asm
                .mov(sat_val.cvt32().unwrap(), result.cvt32().unwrap())
                .unwrap();
            ra.asm.sar(sat_val.cvt32().unwrap(), 15u8).unwrap();
            ra.asm.xor_(sat_val.cvt16().unwrap(), 0x7FFFi32).unwrap();
            ra.asm
                .sub(result.cvt16().unwrap(), op2.cvt16().unwrap())
                .unwrap();
        }
        32 => {
            ra.asm.bt_imm(result, 31).unwrap();
            ra.asm
                .mov(sat_val.cvt32().unwrap(), 0x7FFF_FFFEi32)
                .unwrap();
            ra.asm.adc(sat_val.cvt32().unwrap(), 0i32).unwrap();
            ra.asm
                .sub(result.cvt32().unwrap(), op2.cvt32().unwrap())
                .unwrap();
        }
        64 => {
            ra.asm.bt_imm(result, 63).unwrap();
            ra.asm.mov(sat_val, 0x7FFF_FFFF_FFFF_FFFEi64).unwrap();
            ra.asm.adc(sat_val, 0i32).unwrap();
            ra.asm.sub(result, op2).unwrap();
        }
        _ => unreachable!(),
    }

    ra.asm.cmovo(result, sat_val).unwrap();

    let label_no_overflow = ra.asm.create_label();
    ra.asm.jno(&label_no_overflow, JmpType::Near).unwrap();
    set_qc_flag(ra);
    ra.asm.bind(&label_no_overflow).unwrap();

    ra.release(sat_val);
    ra.define_value(inst_ref, result);
}

pub fn emit_signed_saturated_sub8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_signed_saturated_sub(ra, inst_ref, inst, 8);
}
pub fn emit_signed_saturated_sub16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_signed_saturated_sub(ra, inst_ref, inst, 16);
}
pub fn emit_signed_saturated_sub32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_signed_saturated_sub(ra, inst_ref, inst, 32);
}
pub fn emit_signed_saturated_sub64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_signed_saturated_sub(ra, inst_ref, inst, 64);
}

// ---------------------------------------------------------------------------
// Unsigned saturated add: result = min(a + b, MAX), set QC on carry
// ---------------------------------------------------------------------------

fn emit_unsigned_saturated_add(ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst, bitsize: usize) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_gpr(&mut args[0]);
    let op2 = ra.use_gpr(&mut args[1]);

    match bitsize {
        8 => {
            ra.asm
                .add(result.cvt8().unwrap(), op2.cvt8().unwrap())
                .unwrap();
        }
        16 => {
            ra.asm
                .add(result.cvt16().unwrap(), op2.cvt16().unwrap())
                .unwrap();
        }
        32 => {
            ra.asm
                .add(result.cvt32().unwrap(), op2.cvt32().unwrap())
                .unwrap();
        }
        64 => {
            ra.asm.add(result, op2).unwrap();
        }
        _ => unreachable!(),
    }

    // On carry (CF=1), set result to all-ones (MAX)
    let sat_val = ra.scratch_gpr();
    ra.asm.mov(sat_val.cvt32().unwrap(), -1i32).unwrap();
    if bitsize == 64 {
        ra.asm.mov(sat_val, -1i64).unwrap();
    }
    ra.asm.cmovb(result, sat_val).unwrap();

    // Set QC if carry
    let label_no_carry = ra.asm.create_label();
    ra.asm.jae(&label_no_carry, JmpType::Near).unwrap();
    set_qc_flag(ra);
    ra.asm.bind(&label_no_carry).unwrap();

    ra.release(sat_val);
    ra.define_value(inst_ref, result);
}

pub fn emit_unsigned_saturated_add8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_unsigned_saturated_add(ra, inst_ref, inst, 8);
}
pub fn emit_unsigned_saturated_add16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_unsigned_saturated_add(ra, inst_ref, inst, 16);
}
pub fn emit_unsigned_saturated_add32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_unsigned_saturated_add(ra, inst_ref, inst, 32);
}
pub fn emit_unsigned_saturated_add64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_unsigned_saturated_add(ra, inst_ref, inst, 64);
}

// ---------------------------------------------------------------------------
// Unsigned saturated sub: result = max(a - b, 0), set QC on borrow
// ---------------------------------------------------------------------------

fn emit_unsigned_saturated_sub(ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst, bitsize: usize) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_gpr(&mut args[0]);
    let op2 = ra.use_gpr(&mut args[1]);

    match bitsize {
        8 => {
            ra.asm
                .sub(result.cvt8().unwrap(), op2.cvt8().unwrap())
                .unwrap();
        }
        16 => {
            ra.asm
                .sub(result.cvt16().unwrap(), op2.cvt16().unwrap())
                .unwrap();
        }
        32 => {
            ra.asm
                .sub(result.cvt32().unwrap(), op2.cvt32().unwrap())
                .unwrap();
        }
        64 => {
            ra.asm.sub(result, op2).unwrap();
        }
        _ => unreachable!(),
    }

    // On borrow (CF=1), set result to 0
    let zero = ra.scratch_gpr();
    ra.asm
        .xor_(zero.cvt32().unwrap(), zero.cvt32().unwrap())
        .unwrap();
    ra.asm.cmovb(result, zero).unwrap();

    // Set QC if borrow
    let label_no_borrow = ra.asm.create_label();
    ra.asm.jae(&label_no_borrow, JmpType::Near).unwrap();
    set_qc_flag(ra);
    ra.asm.bind(&label_no_borrow).unwrap();

    ra.release(zero);
    ra.define_value(inst_ref, result);
}

pub fn emit_unsigned_saturated_sub8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_unsigned_saturated_sub(ra, inst_ref, inst, 8);
}
pub fn emit_unsigned_saturated_sub16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_unsigned_saturated_sub(ra, inst_ref, inst, 16);
}
pub fn emit_unsigned_saturated_sub32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_unsigned_saturated_sub(ra, inst_ref, inst, 32);
}
pub fn emit_unsigned_saturated_sub64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_unsigned_saturated_sub(ra, inst_ref, inst, 64);
}

// ---------------------------------------------------------------------------
// SignedSaturation: clamp value to signed N-bit range, set QC
// Args: (value: U32, bit_width: U8)
// ---------------------------------------------------------------------------

pub fn emit_signed_saturation(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_gpr(&mut args[0]);
    let n = args[1].get_immediate_u8();

    // Signed N-bit range: [-(1 << (n-1)), (1 << (n-1)) - 1]
    let max_val = (1i32 << (n - 1)) - 1;
    let min_val = -(1i32 << (n - 1));

    // Compare and clamp
    let temp = ra.scratch_gpr();
    ra.asm.mov(temp.cvt32().unwrap(), max_val).unwrap();
    ra.asm.cmp(result.cvt32().unwrap(), max_val).unwrap();

    let label_no_clamp_high = ra.asm.create_label();
    ra.asm.jle(&label_no_clamp_high, JmpType::Near).unwrap();
    ra.asm
        .mov(result.cvt32().unwrap(), temp.cvt32().unwrap())
        .unwrap();
    set_qc_flag(ra);
    let label_done = ra.asm.create_label();
    ra.asm.jmp(&label_done, JmpType::Near).unwrap();

    ra.asm.bind(&label_no_clamp_high).unwrap();
    ra.asm.cmp(result.cvt32().unwrap(), min_val).unwrap();
    let label_no_clamp_low = ra.asm.create_label();
    ra.asm.jge(&label_no_clamp_low, JmpType::Near).unwrap();
    ra.asm.mov(result.cvt32().unwrap(), min_val).unwrap();
    set_qc_flag(ra);
    ra.asm.jmp(&label_done, JmpType::Near).unwrap();

    ra.asm.bind(&label_no_clamp_low).unwrap();
    ra.asm.bind(&label_done).unwrap();

    ra.release(temp);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// UnsignedSaturation: clamp value to unsigned N-bit range, set QC
// Args: (value: U32, bit_width: U8)
// ---------------------------------------------------------------------------

pub fn emit_unsigned_saturation(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_gpr(&mut args[0]);
    let n = args[1].get_immediate_u8();

    // Unsigned N-bit range: [0, (1 << n) - 1]
    let max_val = if n >= 32 { u32::MAX } else { (1u32 << n) - 1 };

    // If negative (sign bit set), clamp to 0
    let temp = ra.scratch_gpr();
    ra.asm
        .xor_(temp.cvt32().unwrap(), temp.cvt32().unwrap())
        .unwrap();
    ra.asm
        .test(result.cvt32().unwrap(), result.cvt32().unwrap())
        .unwrap();

    // If signed negative, result = 0 and set QC
    let label_not_neg = ra.asm.create_label();
    ra.asm.jns(&label_not_neg, JmpType::Near).unwrap();
    ra.asm
        .mov(result.cvt32().unwrap(), temp.cvt32().unwrap())
        .unwrap();
    set_qc_flag(ra);
    let label_done = ra.asm.create_label();
    ra.asm.jmp(&label_done, JmpType::Near).unwrap();

    ra.asm.bind(&label_not_neg).unwrap();
    // If > max_val, clamp to max_val
    ra.asm.cmp(result.cvt32().unwrap(), max_val as i32).unwrap();
    let label_no_clamp = ra.asm.create_label();
    ra.asm.jbe(&label_no_clamp, JmpType::Near).unwrap();
    ra.asm.mov(result.cvt32().unwrap(), max_val as i32).unwrap();
    set_qc_flag(ra);
    ra.asm.jmp(&label_done, JmpType::Near).unwrap();

    ra.asm.bind(&label_no_clamp).unwrap();
    ra.asm.bind(&label_done).unwrap();

    ra.release(temp);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// SignedSaturatedDoublingMultiplyReturnHigh: (a * b * 2) >> N, with saturation
// Args: (a: U16/U32, b: U16/U32)
// ---------------------------------------------------------------------------

pub fn emit_signed_saturated_doubling_multiply_return_high16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_signed_saturated_doubling_multiply_return_high(ra, inst_ref, inst, 16);
}

pub fn emit_signed_saturated_doubling_multiply_return_high32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_signed_saturated_doubling_multiply_return_high(ra, inst_ref, inst, 32);
}

fn emit_signed_saturated_doubling_multiply_return_high(
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
    bitsize: usize,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());

    match bitsize {
        16 => {
            // Sign-extend both to 32-bit, multiply, shift >> 15 (double >> 16 = >> 15)
            let a = ra.use_scratch_gpr(&mut args[0]);
            let b = ra.use_gpr(&mut args[1]);

            // Sign-extend 16→32
            ra.asm
                .movsx(a.cvt32().unwrap(), a.cvt16().unwrap())
                .unwrap();
            let b_ext = ra.scratch_gpr();
            ra.asm
                .movsx(b_ext.cvt32().unwrap(), b.cvt16().unwrap())
                .unwrap();

            // imul
            ra.asm
                .imul(a.cvt32().unwrap(), b_ext.cvt32().unwrap())
                .unwrap();
            // Double and take high half: (a*b*2) >> 16 = (a*b) >> 15
            ra.asm.sar(a.cvt32().unwrap(), 15u8).unwrap();

            // Check for INT16_MIN * INT16_MIN overflow (result should be INT16_MAX)
            ra.asm.cmp(a.cvt32().unwrap(), 0x8000i32).unwrap();
            let label_no_overflow = ra.asm.create_label();
            ra.asm.jne(&label_no_overflow, JmpType::Near).unwrap();
            ra.asm.mov(a.cvt32().unwrap(), 0x7FFFi32).unwrap();
            set_qc_flag(ra);
            ra.asm.bind(&label_no_overflow).unwrap();

            ra.release(b_ext);
            ra.define_value(inst_ref, a);
        }
        32 => {
            // Sign-extend both to 64-bit, multiply, shift >> 31
            let a = ra.use_scratch_gpr(&mut args[0]);
            let b = ra.use_gpr(&mut args[1]);

            ra.asm.movsxd(a, a.cvt32().unwrap()).unwrap();
            let b_ext = ra.scratch_gpr();
            ra.asm.movsxd(b_ext, b.cvt32().unwrap()).unwrap();

            ra.asm.imul(a, b_ext).unwrap();
            ra.asm.sar(a, 31u8).unwrap();

            // Check for INT32_MIN * INT32_MIN overflow
            ra.asm.mov(b_ext, 0x8000_0000i64).unwrap();
            ra.asm.cmp(a, b_ext).unwrap();
            let label_no_overflow = ra.asm.create_label();
            ra.asm.jne(&label_no_overflow, JmpType::Near).unwrap();
            ra.asm.mov(a.cvt32().unwrap(), 0x7FFF_FFFFi32).unwrap();
            set_qc_flag(ra);
            ra.asm.bind(&label_no_overflow).unwrap();

            ra.release(b_ext);
            ra.define_value(inst_ref, a);
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_saturation_fn_signatures() {
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_signed_saturated_add8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_signed_saturated_add64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_unsigned_saturated_sub32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_signed_saturation;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_unsigned_saturation;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_signed_saturated_doubling_multiply_return_high16;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_signed_saturated_doubling_multiply_return_high32;
    }
}
