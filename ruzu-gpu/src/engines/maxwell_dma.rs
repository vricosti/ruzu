// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell DMA engine stub (NV class B0B5).
//!
//! Handles GPU memory copy operations. Currently a stub.

use super::{ClassId, Engine, ENGINE_REG_COUNT};

pub struct MaxwellDMA {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
}

impl MaxwellDMA {
    pub fn new() -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
        }
    }
}

impl Default for MaxwellDMA {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine for MaxwellDMA {
    fn class_id(&self) -> ClassId {
        ClassId::Dma
    }

    fn write_reg(&mut self, method: u32, value: u32) {
        let idx = method as usize;
        if idx < ENGINE_REG_COUNT {
            self.regs[idx] = value;
        }
        log::trace!("MaxwellDMA: reg[0x{:X}] = 0x{:X}", method, value);
    }
}
