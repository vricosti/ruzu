//! IR value classification helpers shared across the A64 backend emitters.
//!
//! Several A64 ops (`A64GetS`/`A64GetD`/`A64GetQ` and the long-group element
//! reversals) are typed as `U128` and physically hold their scalar payload in
//! an XMM register, even when a later integer consumer (a 32/64-bit memory
//! write, a conditional select) wants the value in a GPR. These helpers walk
//! the `Identity`/`VectorZeroUpper` forwarding chain to decide whether a value
//! should be pulled out of XMM rather than routed through the normal GPR path.
//!
//! Kept in one module (rather than duplicated per emitter file) so the opcode
//! set stays consistent across every consumer.

use crate::backend::x64::emit_context::EmitContext;
use crate::backend::x64::reg_alloc::RegAlloc;
use crate::ir::opcode::Opcode;
use crate::ir::value::Value;

/// Returns true if `value` currently resides (through any number of `Identity`
/// chain links) in an XMM register according to the register allocator.
pub fn ir_value_resolves_to_xmm(ctx: &EmitContext, ra: &RegAlloc, value: &Value) -> bool {
    let Some(block) = ctx.block else {
        return false;
    };
    let mut cur = match value {
        Value::Inst(r) => *r,
        _ => return false,
    };
    loop {
        if let Some(loc) = ra.value_location(cur) {
            if loc.is_xmm() {
                return true;
            }
        }
        let inst = block.get(cur);
        if inst.opcode != Opcode::Identity {
            return false;
        }
        match inst.args[0] {
            Value::Inst(next) => cur = next,
            _ => return false,
        }
    }
}

/// Returns true if `value` is produced (through any number of `Identity` or
/// `VectorZeroUpper` forwarding links) by a vector-register read or a long-group
/// element reversal — i.e. an op whose result physically lives in XMM.
pub fn ir_value_is_vector_backed(ctx: &EmitContext, value: &Value) -> bool {
    let Some(block) = ctx.block else {
        return false;
    };
    let mut cur = match value {
        Value::Inst(r) => *r,
        _ => return false,
    };
    loop {
        let inst = block.get(cur);
        match inst.opcode {
            Opcode::Identity | Opcode::VectorZeroUpper => match inst.args[0] {
                Value::Inst(next) => cur = next,
                _ => return false,
            },
            Opcode::A64GetS
            | Opcode::A64GetD
            | Opcode::A64GetQ
            | Opcode::VectorReverseElementsInLongGroups8
            | Opcode::VectorReverseElementsInLongGroups16
            | Opcode::VectorReverseElementsInLongGroups32 => return true,
            _ => return false,
        }
    }
}
