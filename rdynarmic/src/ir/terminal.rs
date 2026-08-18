use crate::ir::cond::Cond;
use crate::ir::location::LocationDescriptor;
use std::fmt;

/// Terminal instruction for an IR block.
/// Determines control flow after the block's instructions execute.
#[derive(Debug, Clone)]
pub enum Terminal {
    /// Invalid/unset terminal.
    Invalid,

    /// Fall back to interpreter starting at `next` for `num_instructions` instructions.
    Interpret {
        next: LocationDescriptor,
        num_instructions: usize,
    },

    /// Return control to the dispatcher (which reads current CPU state).
    ReturnToDispatch,

    /// Jump to block at `next` if cycles remain, else return to dispatch.
    LinkBlock { next: LocationDescriptor },

    /// Unconditionally jump to block at `next` (optimization, only for safe forward jumps).
    LinkBlockFast { next: LocationDescriptor },

    /// Check RSB (Return Stack Buffer) for predicted return address.
    /// Falls back to dispatch on miss.
    PopRSBHint,

    /// Look up current location in fast dispatch table.
    /// Falls back to dispatch on miss.
    FastDispatchHint,

    /// Conditional terminal: if `cond` then `then_` else `else_`.
    If {
        cond: Cond,
        then_: Box<Terminal>,
        else_: Box<Terminal>,
    },

    /// Branch on check bit: if check_bit != 0 then `then_` else `else_`.
    CheckBit {
        then_: Box<Terminal>,
        else_: Box<Terminal>,
    },

    /// Check if halt was requested. If not halted, execute `else_`.
    CheckHalt { else_: Box<Terminal> },
}

impl Terminal {
    pub fn is_invalid(&self) -> bool {
        matches!(self, Terminal::Invalid)
    }

    pub fn interpret(next: LocationDescriptor) -> Self {
        Terminal::Interpret {
            next,
            num_instructions: 1,
        }
    }

    pub fn link_block(next: LocationDescriptor) -> Self {
        Terminal::LinkBlock { next }
    }

    pub fn link_block_fast(next: LocationDescriptor) -> Self {
        Terminal::LinkBlockFast { next }
    }

    pub fn if_then_else(cond: Cond, then_: Terminal, else_: Terminal) -> Self {
        Terminal::If {
            cond,
            then_: Box::new(then_),
            else_: Box::new(else_),
        }
    }

    pub fn check_bit(then_: Terminal, else_: Terminal) -> Self {
        Terminal::CheckBit {
            then_: Box::new(then_),
            else_: Box::new(else_),
        }
    }

    pub fn check_halt(else_: Terminal) -> Self {
        Terminal::CheckHalt {
            else_: Box::new(else_),
        }
    }
}

impl fmt::Display for Terminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminal::Invalid => write!(f, "Invalid"),
            Terminal::Interpret {
                next,
                num_instructions,
            } => write!(f, "Interpret({}, n={})", next, num_instructions),
            Terminal::ReturnToDispatch => write!(f, "ReturnToDispatch"),
            Terminal::LinkBlock { next } => write!(f, "LinkBlock({})", next),
            Terminal::LinkBlockFast { next } => write!(f, "LinkBlockFast({})", next),
            Terminal::PopRSBHint => write!(f, "PopRSBHint"),
            Terminal::FastDispatchHint => write!(f, "FastDispatchHint"),
            Terminal::If { cond, then_, else_ } => {
                write!(f, "If({}, {}, {})", cond, then_, else_)
            }
            Terminal::CheckBit { then_, else_ } => {
                write!(f, "CheckBit({}, {})", then_, else_)
            }
            Terminal::CheckHalt { else_ } => {
                write!(f, "CheckHalt({})", else_)
            }
        }
    }
}
