use crate::frontend::a32::decoder::DecodedArm;
use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::terminal::Terminal;
use crate::ir::value::Value;

/// ARM SVC (Supervisor Call).
///
/// Matching dynarmic: advance PC past SVC before halting so the host
/// can read the SVC instruction at PC−4.
pub fn arm_svc(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let imm24 = inst.imm24();
    let loc = ir.current_location.expect("current_location not set");

    ir.base.push_rsb(loc.advance_pc(4).to_location());

    // Advance PC past SVC (matching dynarmic's BranchWritePC(PC + 4))
    let next_pc = loc.pc().wrapping_add(4);
    ir.branch_write_pc(Value::ImmU32(next_pc));

    ir.call_supervisor(imm24);
    ir.set_term(Terminal::CheckHalt {
        else_: Box::new(Terminal::PopRSBHint),
    });
    false
}

/// ARM UDF (Undefined instruction).
///
/// Upstream routes the `arm_UDF` decode entry to `UndefinedInstruction()`
/// (i.e. `RaiseException(UndefinedInstruction)`), which performs the full
/// lifecycle: `UpdateUpperLocationDescriptor` + `BranchWritePC(PC+4)` +
/// `ExceptionRaised` + `SetTerm`. The previous manual body raised the wrong
/// exception kind (Unpredictable) and skipped the PC bookkeeping.
pub fn arm_udf(ir: &mut A32IREmitter, _inst: &DecodedArm) -> bool {
    super::undefined_instruction(ir)
}

/// ARM BKPT (Breakpoint).
pub fn arm_bkpt(ir: &mut A32IREmitter, _inst: &DecodedArm) -> bool {
    ir.exception_raised(crate::frontend::a32::types::Exception::Breakpoint);
    ir.set_term(Terminal::CheckHalt {
        else_: Box::new(Terminal::ReturnToDispatch),
    });
    false
}
