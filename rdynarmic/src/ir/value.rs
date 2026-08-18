use std::fmt;

use crate::frontend::a32::types::{ExtReg as A32ExtReg, Reg as A32Reg};
use crate::frontend::a64::types::{Reg as A64Reg, Vec as A64Vec};
use crate::ir::acc_type::AccType;
use crate::ir::cond::Cond;
use crate::ir::types::Type;

/// Index into a Block's instruction arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstRef(pub u32);

impl InstRef {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for InstRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

/// An IR value — either an immediate or a reference to an instruction's result.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Void,
    Inst(InstRef),
    EmptyNZCVImmediateMarker,
    ImmU1(bool),
    ImmU8(u8),
    ImmU16(u16),
    ImmU32(u32),
    ImmU64(u64),
    ImmA64Reg(A64Reg),
    ImmA64Vec(A64Vec),
    ImmA32Reg(A32Reg),
    ImmA32ExtReg(A32ExtReg),
    ImmCoprocInfo(u64),
    ImmCond(Cond),
    ImmAccType(AccType),
}

impl Value {
    /// Get the IR type of this value.
    pub fn get_type(&self) -> Type {
        match self {
            Value::Void => Type::Void,
            Value::Inst(_) => Type::Opaque,
            Value::EmptyNZCVImmediateMarker => Type::NZCVFlags,
            Value::ImmU1(_) => Type::U1,
            Value::ImmU8(_) => Type::U8,
            Value::ImmU16(_) => Type::U16,
            Value::ImmU32(_) => Type::U32,
            Value::ImmU64(_) => Type::U64,
            Value::ImmA64Reg(_) => Type::A64Reg,
            Value::ImmA64Vec(_) => Type::A64Vec,
            Value::ImmA32Reg(_) => Type::A32Reg,
            Value::ImmA32ExtReg(_) => Type::A32ExtReg,
            Value::ImmCoprocInfo(_) => Type::CoprocInfo,
            Value::ImmCond(_) => Type::Cond,
            Value::ImmAccType(_) => Type::AccType,
        }
    }

    /// Returns true if this is an immediate value (not an instruction reference).
    pub fn is_immediate(&self) -> bool {
        !matches!(self, Value::Inst(_) | Value::Void)
    }

    pub fn empty_nzcv_immediate_marker() -> Self {
        Value::EmptyNZCVImmediateMarker
    }

    pub fn get_a32_reg(&self) -> A32Reg {
        match self {
            Value::ImmA32Reg(r) => *r,
            _ => panic!("Value::get_a32_reg called on {:?}", self),
        }
    }

    pub fn get_a32_ext_reg(&self) -> A32ExtReg {
        match self {
            Value::ImmA32ExtReg(r) => *r,
            _ => panic!("Value::get_a32_ext_reg called on {:?}", self),
        }
    }

    pub fn get_coproc_info(&self) -> u64 {
        match self {
            Value::ImmCoprocInfo(v) => *v,
            _ => panic!("Value::get_coproc_info called on {:?}", self),
        }
    }

    /// Returns true if this is a reference to an instruction.
    pub fn is_inst(&self) -> bool {
        matches!(self, Value::Inst(_))
    }

    /// Get the instruction reference, panics if not an Inst value.
    pub fn inst_ref(&self) -> InstRef {
        match self {
            Value::Inst(r) => *r,
            _ => panic!("Value::inst_ref called on non-Inst value: {:?}", self),
        }
    }

    /// Get as bool, panics if not ImmU1.
    pub fn get_u1(&self) -> bool {
        match self {
            Value::ImmU1(v) => *v,
            _ => panic!("Value::get_u1 called on {:?}", self),
        }
    }

    /// Get as u8, panics if not ImmU8.
    pub fn get_u8(&self) -> u8 {
        match self {
            Value::ImmU8(v) => *v,
            _ => panic!("Value::get_u8 called on {:?}", self),
        }
    }

    /// Get as u16, panics if not ImmU16.
    pub fn get_u16(&self) -> u16 {
        match self {
            Value::ImmU16(v) => *v,
            _ => panic!("Value::get_u16 called on {:?}", self),
        }
    }

    /// Get as u32, panics if not ImmU32.
    pub fn get_u32(&self) -> u32 {
        match self {
            Value::ImmU32(v) => *v,
            _ => panic!("Value::get_u32 called on {:?}", self),
        }
    }

    /// Get as u64, panics if not ImmU64.
    pub fn get_u64(&self) -> u64 {
        match self {
            Value::ImmU64(v) => *v,
            _ => panic!("Value::get_u64 called on {:?}", self),
        }
    }

    /// Get as A64Reg, panics if not ImmA64Reg.
    pub fn get_a64_reg(&self) -> A64Reg {
        match self {
            Value::ImmA64Reg(r) => *r,
            _ => panic!("Value::get_a64_reg called on {:?}", self),
        }
    }

    /// Get as A64Vec, panics if not ImmA64Vec.
    pub fn get_a64_vec(&self) -> A64Vec {
        match self {
            Value::ImmA64Vec(v) => *v,
            _ => panic!("Value::get_a64_vec called on {:?}", self),
        }
    }

    /// Get as Cond, panics if not ImmCond.
    pub fn get_cond(&self) -> Cond {
        match self {
            Value::ImmCond(c) => *c,
            _ => panic!("Value::get_cond called on {:?}", self),
        }
    }

    /// Get as AccType, panics if not ImmAccType.
    pub fn get_acc_type(&self) -> AccType {
        match self {
            Value::ImmAccType(a) => *a,
            _ => panic!("Value::get_acc_type called on {:?}", self),
        }
    }

    /// Get any immediate value as u64 (zero-extends smaller types).
    pub fn get_imm_as_u64(&self) -> u64 {
        match self {
            Value::ImmU1(v) => *v as u64,
            Value::ImmU8(v) => *v as u64,
            Value::ImmU16(v) => *v as u64,
            Value::ImmU32(v) => *v as u64,
            Value::ImmU64(v) => *v,
            _ => panic!("Value::get_imm_as_u64 called on {:?}", self),
        }
    }

    /// Get any immediate value as i64 (sign-extends smaller types).
    pub fn get_imm_as_s64(&self) -> i64 {
        match self {
            Value::ImmU1(v) => *v as i64,
            Value::ImmU8(v) => *v as i8 as i64,
            Value::ImmU16(v) => *v as i16 as i64,
            Value::ImmU32(v) => *v as i32 as i64,
            Value::ImmU64(v) => *v as i64,
            _ => panic!("Value::get_imm_as_s64 called on {:?}", self),
        }
    }

    /// Returns true if this is an immediate zero value.
    pub fn is_zero(&self) -> bool {
        match self {
            Value::ImmU1(v) => !v,
            Value::ImmU8(v) => *v == 0,
            Value::ImmU16(v) => *v == 0,
            Value::ImmU32(v) => *v == 0,
            Value::ImmU64(v) => *v == 0,
            _ => false,
        }
    }

    /// Returns true if all bits are set (for integer immediates).
    pub fn has_all_bits_set(&self) -> bool {
        match self {
            Value::ImmU1(v) => *v,
            Value::ImmU8(v) => *v == 0xFF,
            Value::ImmU16(v) => *v == 0xFFFF,
            Value::ImmU32(v) => *v == 0xFFFF_FFFF,
            Value::ImmU64(v) => *v == 0xFFFF_FFFF_FFFF_FFFF,
            _ => false,
        }
    }

    /// Returns true if this is an unsigned immediate with the given value.
    pub fn is_unsigned_imm(&self, val: u64) -> bool {
        self.is_immediate() && self.get_imm_as_u64() == val
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Void => write!(f, "void"),
            Value::Inst(r) => write!(f, "{}", r),
            Value::EmptyNZCVImmediateMarker => write!(f, "<empty_nzcv>"),
            Value::ImmU1(v) => write!(f, "#{}", *v as u8),
            Value::ImmU8(v) => write!(f, "#{:#x}", v),
            Value::ImmU16(v) => write!(f, "#{:#x}", v),
            Value::ImmU32(v) => write!(f, "#{:#x}", v),
            Value::ImmU64(v) => write!(f, "#{:#x}", v),
            Value::ImmA64Reg(r) => write!(f, "{}", r),
            Value::ImmA64Vec(v) => write!(f, "{}", v),
            Value::ImmA32Reg(r) => write!(f, "{}", r),
            Value::ImmA32ExtReg(r) => write!(f, "{}", r),
            Value::ImmCoprocInfo(v) => write!(f, "coproc:{:#x}", v),
            Value::ImmCond(c) => write!(f, "{}", c),
            Value::ImmAccType(a) => write!(f, "{}", a),
        }
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Value::ImmU1(v)
    }
}
impl From<u8> for Value {
    fn from(v: u8) -> Self {
        Value::ImmU8(v)
    }
}
impl From<u16> for Value {
    fn from(v: u16) -> Self {
        Value::ImmU16(v)
    }
}
impl From<u32> for Value {
    fn from(v: u32) -> Self {
        Value::ImmU32(v)
    }
}
impl From<u64> for Value {
    fn from(v: u64) -> Self {
        Value::ImmU64(v)
    }
}
impl From<A64Reg> for Value {
    fn from(r: A64Reg) -> Self {
        Value::ImmA64Reg(r)
    }
}
impl From<A64Vec> for Value {
    fn from(v: A64Vec) -> Self {
        Value::ImmA64Vec(v)
    }
}
impl From<A32Reg> for Value {
    fn from(r: A32Reg) -> Self {
        Value::ImmA32Reg(r)
    }
}
impl From<A32ExtReg> for Value {
    fn from(r: A32ExtReg) -> Self {
        Value::ImmA32ExtReg(r)
    }
}
impl From<Cond> for Value {
    fn from(c: Cond) -> Self {
        Value::ImmCond(c)
    }
}
impl From<AccType> for Value {
    fn from(a: AccType) -> Self {
        Value::ImmAccType(a)
    }
}
impl From<InstRef> for Value {
    fn from(r: InstRef) -> Self {
        Value::Inst(r)
    }
}
