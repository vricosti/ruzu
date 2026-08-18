use crate::frontend::a32::decoder::DecodedArm;
use crate::frontend::a32::types::Reg;
use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::terminal::Terminal;

/// Check if an MRC/MCR to cp10/cp11 is a VFP system register transfer (VMRS/VMSR).
///
/// Upstream decoder pattern (vfp.inc):
///   VMSR: 1110 1110 111L:CRn Rt 1010 0001 0000  (opc1=7, CRm=0, opc2=0)
///   VMRS: same with L=1
///
/// We check opc1=7, CRm=0, opc2=0 to avoid matching unrelated cp10/cp11 ops.
fn is_vfp_system_reg(inst: &DecodedArm) -> bool {
    let opc1 = (inst.raw >> 21) & 7;
    let opc2 = (inst.raw >> 5) & 7;
    let crm = inst.raw & 0xF;
    opc1 == 7 && opc2 == 0 && crm == 0
}

/// Handle VMSR — write a CPU register to a VFP system register.
/// Matches upstream `TranslatorVisitor::vfp_VMSR`.
///
/// VMSR FPSCR, Rt: MCR p10, #7, Rt, c1, c0, #0
fn vfp_vmsr(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let crn = (inst.raw >> 16) & 0xF;

    if rt == Reg::R15 {
        // UNPREDICTABLE
        ir.exception_raised(crate::frontend::a32::types::Exception::UndefinedInstruction);
        return true;
    }

    match crn {
        1 => {
            // VMSR FPSCR, Rt
            // Writing FPSCR changes mode bits that affect translation, so we must
            // end the block and re-enter dispatch (matching upstream behavior).
            let loc = ir.current_location.expect("current_location not set");
            let next_loc = loc.advance_pc(4).advance_it();
            ir.base.push_rsb(next_loc.into());
            ir.update_upper_location_descriptor();
            let value = ir.get_register(rt);
            ir.set_fpscr(value);
            let next_pc = ir.ir().imm32(next_loc.pc());
            ir.branch_write_pc(next_pc);
            ir.set_term(Terminal::PopRSBHint);
            false // terminal — block ends here
        }
        8 => {
            // VMSR FPEXC, Rt — no-op.
            // INTENTIONAL DIVERGENCE: upstream does not decode FPEXC through this
            // path (vfp.inc only matches the FPSCR forms). We handle it here as a
            // bring-up convenience to avoid crashing on SDK init code that touches
            // FPEXC. The write is silently ignored since we always report EN=1.
            true
        }
        _ => {
            // Unknown VFP system register — raise exception
            ir.exception_raised(crate::frontend::a32::types::Exception::UndefinedInstruction);
            true
        }
    }
}

/// Handle VMRS — read a VFP system register into a CPU register.
/// Matches upstream `TranslatorVisitor::vfp_VMRS`.
///
/// VMRS Rt, FPSCR: MRC p10, #7, Rt, c1, c0, #0
/// VMRS APSR_nzcv, FPSCR: MRC p10, #7, R15, c1, c0, #0
fn vfp_vmrs(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rt = inst.rt();
    let crn = (inst.raw >> 16) & 0xF;

    match crn {
        1 => {
            // VMRS Rt, FPSCR
            if rt == Reg::R15 {
                // VMRS APSR_nzcv, FPSCR — transfer NZCV flags only
                let nzcv = ir.get_fpscr_nzcv();
                ir.set_cpsr_nzcv_raw(nzcv);
            } else {
                let fpscr = ir.get_fpscr();
                ir.set_register(rt, fpscr);
            }
            true
        }
        8 => {
            // VMRS Rt, FPEXC — return EN bit set (1 << 30).
            // INTENTIONAL DIVERGENCE: upstream does not decode FPEXC through this
            // path (vfp.inc only matches the FPSCR forms). We handle it here as a
            // bring-up convenience to avoid crashing on SDK init code that reads
            // FPEXC. Returns a fixed value with EN=1.
            if rt != Reg::R15 {
                let val = ir.ir().imm32(1 << 30);
                ir.set_register(rt, val);
            }
            true
        }
        _ => {
            // Unknown VFP system register
            ir.exception_raised(crate::frontend::a32::types::Exception::UndefinedInstruction);
            true
        }
    }
}

/// ARM MCR — Move CPU Register to Coprocessor Register.
///
/// Encoding: cond 1110 opc1(3) 0 CRn(4) Rt(4) coproc(4) opc2(3) 1 CRm(4)
///
/// Packs the coprocessor fields into a u64 `coproc_info` matching dynarmic's
/// `CoprocessorInfo` layout and calls `ir.coproc_send_one_word()`.
pub fn arm_mcr(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let coproc_no = inst.coproc_no();

    // VFP system register transfers (VMSR): MCR p10, #7, Rt, CRn, c0, #0
    if (coproc_no == 10 || coproc_no == 11) && is_vfp_system_reg(inst) {
        return vfp_vmsr(ir, inst);
    }

    // Other VFP/NEON cp10/cp11 operations not yet handled
    if coproc_no == 10 || coproc_no == 11 {
        ir.exception_raised(crate::frontend::a32::types::Exception::UndefinedInstruction);
        return true;
    }

    let coproc_info = inst.pack_coproc_info(false);
    let rt = inst.rt();

    let word = if rt == Reg::R15 {
        // MCR with Rt=PC: value is UNKNOWN (dynarmic doesn't define)
        ir.ir().imm32(0)
    } else {
        ir.get_register(rt)
    };

    ir.coproc_send_one_word(coproc_info, word);
    true
}

/// ARM MRC — Move Coprocessor Register to CPU Register.
///
/// Encoding: cond 1110 opc1(3) 1 CRn(4) Rt(4) coproc(4) opc2(3) 1 CRm(4)
///
/// Packs the coprocessor fields into a u64 `coproc_info` matching dynarmic's
/// `CoprocessorInfo` layout and calls `ir.coproc_get_one_word()`.
pub fn arm_mrc(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let coproc_no = inst.coproc_no();

    // VFP system register transfers (VMRS): MRC p10, #7, Rt, CRn, c0, #0
    if (coproc_no == 10 || coproc_no == 11) && is_vfp_system_reg(inst) {
        return vfp_vmrs(ir, inst);
    }

    // Other VFP/NEON cp10/cp11 operations not yet handled
    if coproc_no == 10 || coproc_no == 11 {
        ir.exception_raised(crate::frontend::a32::types::Exception::UndefinedInstruction);
        return true;
    }

    let coproc_info = inst.pack_coproc_info(false);
    let rt = inst.rt();

    let word = ir.coproc_get_one_word(coproc_info);

    if rt == Reg::R15 {
        // MRC with Rt=PC: result goes to NZCV flags, not a register.
        // Extract bits [31:28] (ARM NZCV) and write to CPSR NZCV.
        let mask = ir.ir().imm32(0xF000_0000);
        let nzcv = ir.ir().and_32(word, mask);
        ir.set_cpsr_nzcv(nzcv);
    } else {
        ir.set_register(rt, word);
    }

    true
}

/// ARM CDP — Coprocessor Data Processing.
///
/// Currently a no-op (stub). Most CDP operations are for VFP/NEON which are
/// handled separately, or for debug coprocessors which can be ignored.
pub fn arm_cdp(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let coproc_no = inst.coproc_no();
    if coproc_no == 10 || coproc_no == 11 {
        ir.exception_raised(crate::frontend::a32::types::Exception::UndefinedInstruction);
        return true;
    }

    let coproc_info = inst.pack_coproc_info(false);
    ir.coproc_internal_operation(coproc_info);
    true
}

/// ARM MRRC — Move to two ARM Registers from Coprocessor.
///
/// Encoding: cond 1100 0101 Rt2(4) Rt(4) coproc(4) opc(4) CRm(4)
///
/// Reads a 64-bit value from the coprocessor and writes the low 32 bits to Rt
/// and the high 32 bits to Rt2. Used for CNTPCT (CP15 C14 opc=0).
pub fn arm_mrrc(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let coproc_no = inst.coproc_no();
    if coproc_no == 10 || coproc_no == 11 {
        // VFP VMOV (f64 to 2xcore) or VMOV (2xf32 to 2xcore).
        // Port of upstream `vfp_VMOV_f64_2u32` / `vfp_VMOV_2f32_2u32`.
        return vfp_vmov_f64_to_2u32(ir, inst);
    }

    let coproc_info = inst.pack_coproc_info_two();
    let rt = inst.rt();
    let rt2 = inst.rt2();

    // Get 64-bit value from coprocessor
    let val64 = ir.coproc_get_two_words(coproc_info);

    // Split into low and high 32-bit halves
    let lo = ir.ir().least_significant_word(val64);
    let hi = ir.ir().most_significant_word(val64);

    if rt != Reg::R15 {
        ir.set_register(rt, lo);
    }
    if rt2 != Reg::R15 {
        ir.set_register(rt2, hi);
    }

    true
}

/// ARM MCRR — Move to Coprocessor from two ARM Registers.
///
/// Encoding: cond 1100 0100 Rt2(4) Rt(4) coproc(4) opc(4) CRm(4)
///
/// Writes two 32-bit register values to the coprocessor as a 64-bit value.
pub fn arm_mcrr(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let coproc_no = inst.coproc_no();
    if coproc_no == 10 || coproc_no == 11 {
        // VFP VMOV (2xcore to f64) or VMOV (2xcore to 2xf32).
        return vfp_vmov_2u32_to_f64(ir, inst);
    }

    let coproc_info = inst.pack_coproc_info_two();
    let rt = inst.rt();
    let rt2 = inst.rt2();

    let word1 = if rt != Reg::R15 {
        ir.get_register(rt)
    } else {
        ir.ir().imm32(0)
    };
    let word2 = if rt2 != Reg::R15 {
        ir.get_register(rt2)
    } else {
        ir.ir().imm32(0)
    };

    ir.coproc_send_two_words(coproc_info, word1, word2);
    true
}

/// VFP VMOV (f64 to 2xcore): VMOV Rt, Rt2, Dm
/// Port of upstream `vfp_VMOV_f64_2u32`.
/// Also handles VMOV (2xf32 to 2xcore) when coproc=10.
fn vfp_vmov_f64_to_2u32(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    use crate::frontend::a32::types::ExtReg;

    let rt = inst.rt();
    let rt2 = inst.rt2();
    let vm = inst.raw & 0xF;
    let m_bit = ((inst.raw >> 5) & 1) != 0;
    let coproc = inst.coproc_no();

    if coproc == 11 {
        // f64 to 2xcore: Dm = {M:Vm}
        let dm_index = ((if m_bit { 16 } else { 0 }) + vm) as u8;
        let dm = ExtReg::from_double(dm_index);
        let value = ir.get_extended_register_64(dm);
        let lo = ir.ir().least_significant_word(value);
        let hi = ir.ir().most_significant_word(value);
        if rt != Reg::R15 {
            ir.set_register(rt, lo);
        }
        if rt2 != Reg::R15 {
            ir.set_register(rt2, hi);
        }
    } else {
        // 2xf32 to 2xcore: Sm = {Vm:M}, Sm1 = Sm+1
        let sm_index = ((vm << 1) | (if m_bit { 1 } else { 0 })) as u8;
        let sm = ExtReg::from_single(sm_index);
        let sm1 = ExtReg::from_single(sm_index + 1);
        let lo = ir.get_extended_register_32(sm);
        let hi = ir.get_extended_register_32(sm1);
        if rt != Reg::R15 {
            ir.set_register(rt, lo);
        }
        if rt2 != Reg::R15 {
            ir.set_register(rt2, hi);
        }
    }
    true
}

/// VFP VMOV (2xcore to f64): VMOV Dm, Rt, Rt2
/// Port of upstream `vfp_VMOV_2u32_f64`.
/// Also handles VMOV (2xcore to 2xf32) when coproc=10.
fn vfp_vmov_2u32_to_f64(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    use crate::frontend::a32::types::ExtReg;
    let rt = inst.rt();
    let rt2 = inst.rt2();
    let vm = inst.raw & 0xF;
    let m_bit = ((inst.raw >> 5) & 1) != 0;
    let coproc = inst.coproc_no();

    let lo = if rt != Reg::R15 {
        ir.get_register(rt)
    } else {
        ir.ir().imm32(0)
    };
    let hi = if rt2 != Reg::R15 {
        ir.get_register(rt2)
    } else {
        ir.ir().imm32(0)
    };

    if coproc == 11 {
        // 2xcore to f64: Dm = {M:Vm}
        let dm_index = ((if m_bit { 16 } else { 0 }) + vm) as u8;
        let dm = ExtReg::from_double(dm_index);
        let value = ir.ir().pack_2x32_to_1x64(lo, hi);
        ir.set_extended_register_64(dm, value);
    } else {
        // 2xcore to 2xf32: Sm = {Vm:M}, Sm1 = Sm+1
        let sm_index = ((vm << 1) | (if m_bit { 1 } else { 0 })) as u8;
        let sm = ExtReg::from_single(sm_index);
        let sm1 = ExtReg::from_single(sm_index + 1);
        ir.set_extended_register_32(sm, lo);
        ir.set_extended_register_32(sm1, hi);
    }
    true
}
