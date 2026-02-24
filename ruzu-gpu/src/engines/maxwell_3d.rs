// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell 3D engine stub.
//!
//! This is the main 3D rendering engine (NV class B197). It handles vertex
//! setup, rasterization state, shaders, and draw calls. Currently a stub
//! that records register writes.

use super::{ClassId, Engine, ENGINE_REG_COUNT};

pub struct Maxwell3D {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
}

impl Maxwell3D {
    pub fn new() -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
        }
    }
}

impl Default for Maxwell3D {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine for Maxwell3D {
    fn class_id(&self) -> ClassId {
        ClassId::Threed
    }

    fn write_reg(&mut self, method: u32, value: u32) {
        let idx = method as usize;
        if idx < ENGINE_REG_COUNT {
            self.regs[idx] = value;
        }
        log::trace!("Maxwell3D: reg[0x{:X}] = 0x{:X}", method, value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_reg() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(0x100, 0xDEAD);
        assert_eq!(engine.regs[0x100], 0xDEAD);
    }

    #[test]
    fn test_class_id() {
        let engine = Maxwell3D::new();
        assert_eq!(engine.class_id(), ClassId::Threed);
    }
}
