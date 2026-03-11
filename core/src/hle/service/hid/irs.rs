//! Port of zuyu/src/core/hle/service/hid/irs.h and irs.cpp
//!
//! IRS and IRS_SYS services ("irs", "irs:sys").

/// IRS service - IR sensor interface.
pub struct Irs;

impl Irs {
    pub fn new() -> Self {
        Self
    }
}

/// IRS_SYS service - system-level IR sensor interface.
pub struct IrsSys;

impl IrsSys {
    pub fn new() -> Self {
        Self
    }
}
