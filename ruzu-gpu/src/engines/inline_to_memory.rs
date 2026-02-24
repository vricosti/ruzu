// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Inline-to-Memory engine stub (NV class A140).
//!
//! Copies inline data from pushbuffer commands directly into GPU memory.
//! Currently a stub.

use super::{ClassId, Engine, ENGINE_REG_COUNT};

pub struct InlineToMemory {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
}

impl InlineToMemory {
    pub fn new() -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
        }
    }
}

impl Default for InlineToMemory {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine for InlineToMemory {
    fn class_id(&self) -> ClassId {
        ClassId::InlineToMemory
    }

    fn write_reg(&mut self, method: u32, value: u32) {
        let idx = method as usize;
        if idx < ENGINE_REG_COUNT {
            self.regs[idx] = value;
        }
        log::trace!("InlineToMemory: reg[0x{:X}] = 0x{:X}", method, value);
    }
}
