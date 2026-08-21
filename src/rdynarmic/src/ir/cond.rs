use std::fmt;

/// ARM condition codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Cond {
    EQ = 0,  // Equal (Z=1)
    NE = 1,  // Not equal (Z=0)
    CS = 2,  // Carry set / unsigned higher or same (C=1)
    CC = 3,  // Carry clear / unsigned lower (C=0)
    MI = 4,  // Minus / negative (N=1)
    PL = 5,  // Plus / positive or zero (N=0)
    VS = 6,  // Overflow (V=1)
    VC = 7,  // No overflow (V=0)
    HI = 8,  // Unsigned higher (C=1 && Z=0)
    LS = 9,  // Unsigned lower or same (C=0 || Z=1)
    GE = 10, // Signed greater than or equal (N=V)
    LT = 11, // Signed less than (N!=V)
    GT = 12, // Signed greater than (Z=0 && N=V)
    LE = 13, // Signed less than or equal (Z=1 || N!=V)
    AL = 14, // Always
    NV = 15, // Never (architecturally behaves like AL)
}

impl Cond {
    /// Aliases
    pub const HS: Cond = Cond::CS;
    pub const LO: Cond = Cond::CC;

    /// Invert the condition code.
    pub fn invert(self) -> Cond {
        let val = self as u8;
        Cond::from_u8(val ^ 1)
    }

    /// Create from raw 4-bit value.
    pub fn from_u8(val: u8) -> Cond {
        match val & 0xF {
            0 => Cond::EQ,
            1 => Cond::NE,
            2 => Cond::CS,
            3 => Cond::CC,
            4 => Cond::MI,
            5 => Cond::PL,
            6 => Cond::VS,
            7 => Cond::VC,
            8 => Cond::HI,
            9 => Cond::LS,
            10 => Cond::GE,
            11 => Cond::LT,
            12 => Cond::GT,
            13 => Cond::LE,
            14 => Cond::AL,
            15 => Cond::NV,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Cond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Cond::EQ => "eq",
            Cond::NE => "ne",
            Cond::CS => "cs",
            Cond::CC => "cc",
            Cond::MI => "mi",
            Cond::PL => "pl",
            Cond::VS => "vs",
            Cond::VC => "vc",
            Cond::HI => "hi",
            Cond::LS => "ls",
            Cond::GE => "ge",
            Cond::LT => "lt",
            Cond::GT => "gt",
            Cond::LE => "le",
            Cond::AL => "al",
            Cond::NV => "nv",
        };
        write!(f, "{}", s)
    }
}
