// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM register allocator.
//!
//! Maps to upstream `backend/glasm/reg_alloc.h` and `reg_alloc.cpp`.

use std::fmt;

/// GLASM value type tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Void,
    Register,
    U32,
    U64,
}

/// Register identifier, packed into a u32.
///
/// Matches upstream `Id` bitfield layout:
/// - bit 0: is_valid
/// - bit 1: is_long
/// - bit 2: is_spill
/// - bit 3: is_condition_code
/// - bit 4: is_null
/// - bits 5..31: index
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Id {
    pub raw: u32,
}

impl Id {
    pub fn new() -> Self {
        Self { raw: 0 }
    }

    pub fn is_valid(&self) -> bool {
        self.raw & 1 != 0
    }

    pub fn set_valid(&mut self, v: bool) {
        if v {
            self.raw |= 1;
        } else {
            self.raw &= !1;
        }
    }

    pub fn is_long(&self) -> bool {
        self.raw & (1 << 1) != 0
    }

    pub fn set_long(&mut self, v: bool) {
        if v {
            self.raw |= 1 << 1;
        } else {
            self.raw &= !(1 << 1);
        }
    }

    pub fn is_spill(&self) -> bool {
        self.raw & (1 << 2) != 0
    }

    pub fn set_spill(&mut self, v: bool) {
        if v {
            self.raw |= 1 << 2;
        } else {
            self.raw &= !(1 << 2);
        }
    }

    pub fn is_condition_code(&self) -> bool {
        self.raw & (1 << 3) != 0
    }

    pub fn set_condition_code(&mut self, v: bool) {
        if v {
            self.raw |= 1 << 3;
        } else {
            self.raw &= !(1 << 3);
        }
    }

    pub fn is_null(&self) -> bool {
        self.raw & (1 << 4) != 0
    }

    pub fn set_null(&mut self, v: bool) {
        if v {
            self.raw |= 1 << 4;
        } else {
            self.raw &= !(1 << 4);
        }
    }

    pub fn index(&self) -> u32 {
        self.raw >> 5
    }

    pub fn set_index(&mut self, idx: u32) {
        self.raw = (self.raw & 0x1F) | (idx << 5);
    }
}

impl Default for Id {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Id(raw=0x{:08X})", self.raw)
    }
}

/// A typed GLASM value: either a register reference or an immediate.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value {
    pub value_type: Type,
    pub id: Id,
    pub imm_u32: u32,
    pub imm_u64: u64,
}

impl Value {
    pub fn void_val() -> Self {
        Self {
            value_type: Type::Void,
            id: Id::new(),
            imm_u32: 0,
            imm_u64: 0,
        }
    }

    pub fn from_register(id: Id) -> Self {
        Self {
            value_type: Type::Register,
            id,
            imm_u32: 0,
            imm_u64: 0,
        }
    }

    pub fn from_u32(v: u32) -> Self {
        Self {
            value_type: Type::U32,
            id: Id::new(),
            imm_u32: v,
            imm_u64: 0,
        }
    }

    pub fn from_u64(v: u64) -> Self {
        Self {
            value_type: Type::U64,
            id: Id::new(),
            imm_u32: 0,
            imm_u64: v,
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::void_val()
    }
}

/// Newtype wrappers matching upstream Register, ScalarRegister, etc.
pub type Register = Value;
pub type ScalarRegister = Value;
pub type ScalarU32 = Value;
pub type ScalarS32 = Value;
pub type ScalarF32 = Value;
pub type ScalarF64 = Value;

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_condition_code() {
            return write!(f, "CC");
        }
        if self.is_spill() {
            return write!(f, "SPILL");
        }
        if self.is_null() {
            if self.is_long() {
                write!(f, "DC.x")
            } else {
                write!(f, "RC.x")
            }
        } else if self.is_long() {
            write!(f, "D{}.x", self.index())
        } else {
            write!(f, "R{}.x", self.index())
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.value_type {
            Type::Void => write!(f, "VOID"),
            Type::Register => {
                // Non-scalar (vector) form
                if self.id.is_null() {
                    if self.id.is_long() {
                        write!(f, "DC")
                    } else {
                        write!(f, "RC")
                    }
                } else if self.id.is_long() {
                    write!(f, "D{}", self.id.index())
                } else {
                    write!(f, "R{}", self.id.index())
                }
            }
            Type::U32 => write!(f, "{}", self.imm_u32),
            Type::U64 => write!(f, "{}", self.imm_u64),
        }
    }
}

/// GLASM register allocator.
///
/// Matches upstream `RegAlloc` class.
pub const NUM_REGS: usize = 4096;

pub struct RegAlloc {
    num_used_registers: usize,
    num_used_long_registers: usize,
    register_use: Vec<bool>,
    long_register_use: Vec<bool>,
}

impl RegAlloc {
    pub fn new() -> Self {
        Self {
            num_used_registers: 0,
            num_used_long_registers: 0,
            register_use: vec![false; NUM_REGS],
            long_register_use: vec![false; NUM_REGS],
        }
    }

    /// Allocate a short register for the result of an instruction.
    pub fn define(&mut self) -> Register {
        let id = self.alloc(false);
        Value::from_register(id)
    }

    /// Allocate a long (64-bit) register for the result of an instruction.
    pub fn long_define(&mut self) -> Register {
        let id = self.alloc(true);
        Value::from_register(id)
    }

    /// Allocate a short register.
    pub fn alloc_reg(&mut self) -> Register {
        let id = self.alloc(false);
        Value::from_register(id)
    }

    /// Allocate a long register.
    pub fn alloc_long_reg(&mut self) -> Register {
        let id = self.alloc(true);
        Value::from_register(id)
    }

    /// Free a register.
    pub fn free_reg(&mut self, reg: Register) {
        self.free(reg.id);
    }

    /// Invalidate condition codes (no-op for now, matches upstream).
    pub fn invalidate_condition_codes(&mut self) {
        // This does nothing for now, matching upstream
    }

    /// Number of short registers used.
    pub fn num_used_registers(&self) -> usize {
        self.num_used_registers
    }

    /// Number of long registers used.
    pub fn num_used_long_registers(&self) -> usize {
        self.num_used_long_registers
    }

    /// Whether no registers are in use.
    pub fn is_empty(&self) -> bool {
        !self.register_use.iter().any(|&x| x) && !self.long_register_use.iter().any(|&x| x)
    }

    fn alloc(&mut self, is_long: bool) -> Id {
        let (num_regs, use_set) = if is_long {
            (
                &mut self.num_used_long_registers,
                &mut self.long_register_use,
            )
        } else {
            (&mut self.num_used_registers, &mut self.register_use)
        };

        for reg in 0..NUM_REGS {
            if use_set[reg] {
                continue;
            }
            *num_regs = (*num_regs).max(reg + 1);
            use_set[reg] = true;
            let mut id = Id::new();
            id.set_valid(true);
            id.set_long(is_long);
            id.set_spill(false);
            id.set_condition_code(false);
            id.set_null(false);
            id.set_index(reg as u32);
            return id;
        }
        panic!("Register spilling not implemented");
    }

    fn free(&mut self, id: Id) {
        if !id.is_valid() {
            panic!("Freeing invalid register");
        }
        if id.is_spill() {
            panic!("Free spill not implemented");
        }
        let idx = id.index() as usize;
        if id.is_long() {
            self.long_register_use[idx] = false;
        } else {
            self.register_use[idx] = false;
        }
    }
}

impl Default for RegAlloc {
    fn default() -> Self {
        Self::new()
    }
}
