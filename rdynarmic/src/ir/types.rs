use std::fmt;

/// IR type system. Types are bit flags to allow compatibility checks via bitwise OR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum Type {
    Void = 0,
    A32Reg = 1 << 0,
    A32ExtReg = 1 << 1,
    A64Reg = 1 << 2,
    A64Vec = 1 << 3,
    Opaque = 1 << 4,
    U1 = 1 << 5,
    U8 = 1 << 6,
    U16 = 1 << 7,
    U32 = 1 << 8,
    U64 = 1 << 9,
    U128 = 1 << 10,
    CoprocInfo = 1 << 11,
    NZCVFlags = 1 << 12,
    Cond = 1 << 13,
    Table = 1 << 14,
    AccType = 1 << 15,
}

impl Type {
    /// Returns the raw bit value of this type.
    pub fn bits(self) -> u16 {
        self as u16
    }

    /// Check if two types are compatible.
    /// Opaque is compatible with any non-Void type.
    pub fn is_compatible_with(self, other: Type) -> bool {
        if self == other {
            return true;
        }
        if self == Type::Opaque && other != Type::Void {
            return true;
        }
        if other == Type::Opaque && self != Type::Void {
            return true;
        }
        false
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Void => write!(f, "Void"),
            Type::A32Reg => write!(f, "A32Reg"),
            Type::A32ExtReg => write!(f, "A32ExtReg"),
            Type::A64Reg => write!(f, "A64Reg"),
            Type::A64Vec => write!(f, "A64Vec"),
            Type::Opaque => write!(f, "Opaque"),
            Type::U1 => write!(f, "U1"),
            Type::U8 => write!(f, "U8"),
            Type::U16 => write!(f, "U16"),
            Type::U32 => write!(f, "U32"),
            Type::U64 => write!(f, "U64"),
            Type::U128 => write!(f, "U128"),
            Type::CoprocInfo => write!(f, "CoprocInfo"),
            Type::NZCVFlags => write!(f, "NZCVFlags"),
            Type::Cond => write!(f, "Cond"),
            Type::Table => write!(f, "Table"),
            Type::AccType => write!(f, "AccType"),
        }
    }
}
