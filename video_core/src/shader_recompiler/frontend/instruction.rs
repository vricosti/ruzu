// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/maxwell/instruction.h`
//!
//! Maxwell instruction word and predicate extraction.

use super::super::ir::flow_test::FlowTest;

/// Maxwell predicate condition from an instruction word.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Predicate {
    pub index: u32,
    pub negated: bool,
}

impl Predicate {
    pub fn new(index: u32, negated: bool) -> Self {
        Self { index, negated }
    }

    pub fn from_raw(raw: u64) -> Self {
        Self {
            index: (raw & 7) as u32,
            negated: (raw & 8) != 0,
        }
    }

    pub fn from_bool(value: bool) -> Self {
        Self {
            index: 7, // PT
            negated: !value,
        }
    }
}

/// Maxwell instruction word (64-bit).
#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    pub raw: u64,
}

impl Instruction {
    pub fn new(raw: u64) -> Self {
        Self { raw }
    }

    /// Extract the execution predicate (bits [19:16]).
    pub fn pred(&self) -> Predicate {
        Predicate::from_raw((self.raw >> 16) & 0xF)
    }

    /// Extract the branch control bits.
    pub fn branch_is_cbuf(&self) -> bool {
        (self.raw >> 5) & 1 != 0
    }

    /// Extract the flow test (bits [4:0] of the branch field).
    pub fn branch_flow_test(&self) -> Option<FlowTest> {
        FlowTest::from_u64(self.raw & 0x1F)
    }

    /// Extract the branch offset (signed, bits [43:20]).
    pub fn branch_offset(&self) -> i32 {
        let raw = ((self.raw >> 20) & 0x00FF_FFFF) as u32;
        // Sign-extend from 24 bits
        if raw & (1 << 23) != 0 {
            (raw | 0xFF00_0000) as i32
        } else {
            raw as i32
        }
    }

    /// Extract the absolute branch target (bits [51:20]).
    pub fn branch_absolute(&self) -> u32 {
        ((self.raw >> 20) & 0xFFFF_FFFF) as u32
    }
}
