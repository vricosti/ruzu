// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Fermi 2D engine stub (NV class 902D).
//!
//! Handles 2D blitting operations (surface copies, fills). Detects blit
//! trigger writes and logs parameters; actual pixel copy is not yet implemented.

use super::{ClassId, Engine, ENGINE_REG_COUNT};

// ── Register constants (method = byte_offset / 4) ──────────────────────────

// Destination surface descriptor (0x80..0x85)
const DST_FORMAT: u32 = 0x80;
const DST_PITCH: u32 = 0x82;
const DST_WIDTH: u32 = 0x84;
const DST_HEIGHT: u32 = 0x85;
const DST_ADDR_HIGH: u32 = 0x86;
const DST_ADDR_LOW: u32 = 0x87;

// Source surface descriptor (0x8C..0x91)
const SRC_FORMAT: u32 = 0x8C;
const SRC_PITCH: u32 = 0x8E;
const SRC_WIDTH: u32 = 0x90;
const SRC_HEIGHT: u32 = 0x91;
const SRC_ADDR_HIGH: u32 = 0x92;
const SRC_ADDR_LOW: u32 = 0x93;

// Blit trigger
const BLIT_TRIGGER: u32 = 0x237;

pub struct Fermi2D {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
    /// Set when a blit trigger is detected; consumed by tests / future logic.
    pub pending_blit: bool,
}

impl Fermi2D {
    pub fn new() -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
            pending_blit: false,
        }
    }

    // ── Typed accessors ────────────────────────────────────────────────

    pub fn dst_addr(&self) -> u64 {
        ((self.regs[DST_ADDR_HIGH as usize] as u64) << 32)
            | (self.regs[DST_ADDR_LOW as usize] as u64)
    }

    pub fn src_addr(&self) -> u64 {
        ((self.regs[SRC_ADDR_HIGH as usize] as u64) << 32)
            | (self.regs[SRC_ADDR_LOW as usize] as u64)
    }

    pub fn dst_width(&self) -> u32 {
        self.regs[DST_WIDTH as usize]
    }

    pub fn dst_height(&self) -> u32 {
        self.regs[DST_HEIGHT as usize]
    }

    pub fn dst_pitch(&self) -> u32 {
        self.regs[DST_PITCH as usize]
    }

    pub fn dst_format(&self) -> u32 {
        self.regs[DST_FORMAT as usize]
    }

    pub fn src_width(&self) -> u32 {
        self.regs[SRC_WIDTH as usize]
    }

    pub fn src_height(&self) -> u32 {
        self.regs[SRC_HEIGHT as usize]
    }

    pub fn src_pitch(&self) -> u32 {
        self.regs[SRC_PITCH as usize]
    }

    pub fn src_format(&self) -> u32 {
        self.regs[SRC_FORMAT as usize]
    }

    // ── Blit handling ──────────────────────────────────────────────────

    fn handle_blit(&mut self) {
        log::debug!(
            "Fermi2D: BLIT dst=0x{:X} ({}x{} pitch={} fmt={}) src=0x{:X} ({}x{} pitch={} fmt={})",
            self.dst_addr(),
            self.dst_width(),
            self.dst_height(),
            self.dst_pitch(),
            self.dst_format(),
            self.src_addr(),
            self.src_width(),
            self.src_height(),
            self.src_pitch(),
            self.src_format(),
        );
        self.pending_blit = true;
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

        if method == BLIT_TRIGGER {
            self.handle_blit();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_address_accessors() {
        let mut eng = Fermi2D::new();
        eng.write_reg(DST_ADDR_HIGH, 0x0000_00AB);
        eng.write_reg(DST_ADDR_LOW, 0xCDEF_0000);
        assert_eq!(eng.dst_addr(), 0xAB_CDEF_0000);

        eng.write_reg(SRC_ADDR_HIGH, 0x12);
        eng.write_reg(SRC_ADDR_LOW, 0x3456_7890);
        assert_eq!(eng.src_addr(), 0x12_3456_7890);
    }

    #[test]
    fn test_surface_accessors() {
        let mut eng = Fermi2D::new();
        eng.write_reg(DST_WIDTH, 1280);
        eng.write_reg(DST_HEIGHT, 720);
        eng.write_reg(DST_PITCH, 5120);
        eng.write_reg(DST_FORMAT, 0xA5);
        assert_eq!(eng.dst_width(), 1280);
        assert_eq!(eng.dst_height(), 720);
        assert_eq!(eng.dst_pitch(), 5120);
        assert_eq!(eng.dst_format(), 0xA5);

        eng.write_reg(SRC_WIDTH, 640);
        eng.write_reg(SRC_HEIGHT, 480);
        eng.write_reg(SRC_PITCH, 2560);
        eng.write_reg(SRC_FORMAT, 0xB6);
        assert_eq!(eng.src_width(), 640);
        assert_eq!(eng.src_height(), 480);
        assert_eq!(eng.src_pitch(), 2560);
        assert_eq!(eng.src_format(), 0xB6);
    }

    #[test]
    fn test_blit_trigger_sets_pending() {
        let mut eng = Fermi2D::new();
        assert!(!eng.pending_blit);

        eng.write_reg(DST_ADDR_HIGH, 0);
        eng.write_reg(DST_ADDR_LOW, 0x1000);
        eng.write_reg(DST_WIDTH, 1280);
        eng.write_reg(DST_HEIGHT, 720);
        eng.write_reg(DST_PITCH, 5120);
        eng.write_reg(DST_FORMAT, 0xA5);

        eng.write_reg(SRC_ADDR_HIGH, 0);
        eng.write_reg(SRC_ADDR_LOW, 0x2000);
        eng.write_reg(SRC_WIDTH, 1280);
        eng.write_reg(SRC_HEIGHT, 720);
        eng.write_reg(SRC_PITCH, 5120);
        eng.write_reg(SRC_FORMAT, 0xA5);

        // Trigger blit
        eng.write_reg(BLIT_TRIGGER, 1);
        assert!(eng.pending_blit);
    }

    #[test]
    fn test_no_trigger_without_blit_method() {
        let mut eng = Fermi2D::new();
        eng.write_reg(0x100, 42); // Random register
        assert!(!eng.pending_blit);
    }
}
