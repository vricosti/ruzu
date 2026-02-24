// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Fermi 2D engine stub (NV class 902D).
//!
//! Handles 2D blitting operations (surface copies, fills). Currently a stub.

use super::{ClassId, Engine, ENGINE_REG_COUNT};

pub struct Fermi2D {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
}

impl Fermi2D {
    pub fn new() -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
        }
    }
}

impl Default for Fermi2D {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine for Fermi2D {
    fn class_id(&self) -> ClassId {
        ClassId::Twod
    }

    fn write_reg(&mut self, method: u32, value: u32) {
        let idx = method as usize;
        if idx < ENGINE_REG_COUNT {
            self.regs[idx] = value;
        }
        log::trace!("Fermi2D: reg[0x{:X}] = 0x{:X}", method, value);
    }
}
