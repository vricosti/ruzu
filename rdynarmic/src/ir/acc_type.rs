use std::fmt;

/// Memory access type for load/store operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum AccType {
    Normal = 0,
    Vec = 1,
    VecStreaming = 2,
    Atomic = 3,
    Ordered = 4,
    OrderedAtomic = 5,
    LimitedOrdered = 6,
    Unpriv = 7,
    IfetchOrdered = 8,
    VecIfetchStreaming = 9,
    Stream = 10,
    Aligned = 11,
    NonTemporal = 12,
    AtNonTemporal = 13,
    AtAligned = 14,
}

impl AccType {
    pub fn from_u8(val: u8) -> Self {
        match val {
            0 => AccType::Normal,
            1 => AccType::Vec,
            2 => AccType::VecStreaming,
            3 => AccType::Atomic,
            4 => AccType::Ordered,
            5 => AccType::OrderedAtomic,
            6 => AccType::LimitedOrdered,
            7 => AccType::Unpriv,
            8 => AccType::IfetchOrdered,
            9 => AccType::VecIfetchStreaming,
            10 => AccType::Stream,
            11 => AccType::Aligned,
            12 => AccType::NonTemporal,
            13 => AccType::AtNonTemporal,
            14 => AccType::AtAligned,
            _ => AccType::Normal,
        }
    }
}

impl fmt::Display for AccType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
