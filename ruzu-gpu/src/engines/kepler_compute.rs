// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Kepler Compute engine stub (NV class B1C0).
//!
//! Handles compute shader dispatch. Currently a stub.

use super::{ClassId, Engine, ENGINE_REG_COUNT};

pub struct KeplerCompute {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
}

impl KeplerCompute {
    pub fn new() -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
        }
    }
}

impl Default for KeplerCompute {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine for KeplerCompute {
    fn class_id(&self) -> ClassId {
        ClassId::Compute
    }

    fn write_reg(&mut self, method: u32, value: u32) {
        let idx = method as usize;
        if idx < ENGINE_REG_COUNT {
            self.regs[idx] = value;
        }
        log::trace!("KeplerCompute: reg[0x{:X}] = 0x{:X}", method, value);
    }
}
