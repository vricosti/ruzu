use std::fmt;

use crate::ir::opcode::Opcode;
use crate::ir::types::Type;
use crate::ir::value::{InstRef, Value};

/// Maximum number of arguments per IR instruction.
pub const MAX_ARGS: usize = 5;

/// A single IR instruction in SSA form.
#[derive(Debug, Clone)]
pub struct Inst {
    /// The operation this instruction performs.
    pub opcode: Opcode,
    /// Arguments to the instruction (up to MAX_ARGS, rest are Value::Void).
    pub args: [Value; MAX_ARGS],
    /// Number of other instructions that use this instruction's result.
    pub use_count: u32,
    /// If non-zero, this instruction is a pseudo-op that forwards to another instruction.
    /// Used by Identity and optimization passes.
    pub pseudo_of: Option<InstRef>,
    /// Linked list of pseudo-operations associated with this instruction.
    /// Upstream: `Inst* next_pseudoop` — used by GetCarryFromOp, GetOverflowFromOp,
    /// GetNZCVFromOp to find the flag-extracting pseudo-op from the flag-producing
    /// instruction. The backend emits flag extraction (SETC, SETO, LAHF) immediately
    /// after the producing instruction, preventing register allocator interference.
    pub next_pseudoop: Option<InstRef>,
}

impl Inst {
    /// Create a new instruction with the given opcode and arguments.
    pub fn new(opcode: Opcode, args: &[Value]) -> Self {
        assert!(
            args.len() <= MAX_ARGS,
            "Too many args ({}) for opcode {:?}",
            args.len(),
            opcode
        );
        let mut inst_args = [Value::Void; MAX_ARGS];
        for (i, arg) in args.iter().enumerate() {
            inst_args[i] = *arg;
        }
        Self {
            opcode,
            args: inst_args,
            use_count: 0,
            pseudo_of: None,
            next_pseudoop: None,
        }
    }

    /// Get the return type of this instruction.
    ///
    /// NOTE: For `Identity`, this returns `Opaque` (the declared opcode
    /// return type), NOT the type of the aliased argument. Callers that
    /// care about the *real* type behind an `Identity` chain should use
    /// `Block::inst_real_return_type` (which mirrors upstream
    /// `Inst::GetType()` in microinstruction.cpp:624-628 by recursing
    /// through Identity arg chains until a non-Identity inst is found).
    pub fn return_type(&self) -> Type {
        self.opcode.return_type()
    }

    /// Get the number of arguments.
    pub fn num_args(&self) -> usize {
        self.opcode.num_args()
    }

    /// Get argument at index.
    pub fn arg(&self, idx: usize) -> Value {
        self.args[idx]
    }

    /// Set argument at index.
    pub fn set_arg(&mut self, idx: usize, value: Value) {
        self.args[idx] = value;
    }

    /// Returns true if this instruction has been tombstoned (removed).
    pub fn is_tombstone(&self) -> bool {
        self.opcode == Opcode::Void && self.use_count == 0
    }

    /// Returns true if this opcode has side effects.
    pub fn has_side_effects(&self) -> bool {
        self.opcode.has_side_effects()
    }

    /// Tombstone this instruction (mark as removed).
    pub fn tombstone(&mut self) {
        self.opcode = Opcode::Void;
        self.args = [Value::Void; MAX_ARGS];
        self.pseudo_of = None;
        self.next_pseudoop = None;
    }

    /// Replace this instruction with an Identity forwarding to another value.
    pub fn replace_with_identity(&mut self, value: Value) {
        self.opcode = Opcode::Identity;
        self.args = [Value::Void; MAX_ARGS];
        self.args[0] = value;
    }

    /// Get all non-Void argument values as an iterator.
    pub fn arg_values(&self) -> impl Iterator<Item = &Value> {
        self.args[..self.num_args()].iter()
    }
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.opcode)?;
        let n = self.num_args();
        if n > 0 {
            write!(f, " ")?;
            for i in 0..n {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", self.args[i])?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_inst_creation() {
        let inst = Inst::new(
            Opcode::Add32,
            &[
                Value::Inst(InstRef(0)),
                Value::Inst(InstRef(1)),
                Value::ImmU1(false),
            ],
        );
        assert_eq!(inst.opcode, Opcode::Add32);
        assert_eq!(inst.num_args(), 3);
        assert_eq!(inst.use_count, 0);
    }

    #[test]
    fn test_inst_tombstone() {
        let mut inst = Inst::new(
            Opcode::Add32,
            &[Value::ImmU32(1), Value::ImmU32(2), Value::ImmU1(false)],
        );
        assert!(!inst.is_tombstone());
        inst.tombstone();
        assert!(inst.is_tombstone());
    }
}
