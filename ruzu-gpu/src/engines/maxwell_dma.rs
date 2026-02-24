// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell DMA engine stub (NV class B0B5).
//!
//! Handles GPU memory copy operations. Detects launch trigger writes and logs
//! parameters; actual DMA copy is not yet implemented.

use super::{ClassId, Engine, ENGINE_REG_COUNT};

// ── Register constants (method = byte_offset / 4) ──────────────────────────

const LAUNCH_DMA: u32 = 0xC0;

const SRC_ADDR_HIGH: u32 = 0x100;
const SRC_ADDR_LOW: u32 = 0x101;
const DST_ADDR_HIGH: u32 = 0x102;
const DST_ADDR_LOW: u32 = 0x103;

const PITCH_IN: u32 = 0x104;
const PITCH_OUT: u32 = 0x105;
const LINE_LENGTH: u32 = 0x106;
const LINE_COUNT: u32 = 0x107;

pub struct MaxwellDMA {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
    /// Set when a DMA launch trigger is detected; consumed by tests / future logic.
    pub pending_launch: bool,
}

impl MaxwellDMA {
    pub fn new() -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
            pending_launch: false,
        }
    }

    // ── Typed accessors ────────────────────────────────────────────────

    pub fn src_addr(&self) -> u64 {
        ((self.regs[SRC_ADDR_HIGH as usize] as u64) << 32)
            | (self.regs[SRC_ADDR_LOW as usize] as u64)
    }

    pub fn dst_addr(&self) -> u64 {
        ((self.regs[DST_ADDR_HIGH as usize] as u64) << 32)
            | (self.regs[DST_ADDR_LOW as usize] as u64)
    }

    pub fn pitch_in(&self) -> u32 {
        self.regs[PITCH_IN as usize]
    }

    pub fn pitch_out(&self) -> u32 {
        self.regs[PITCH_OUT as usize]
    }

    pub fn line_length(&self) -> u32 {
        self.regs[LINE_LENGTH as usize]
    }

    pub fn line_count(&self) -> u32 {
        self.regs[LINE_COUNT as usize]
    }

    // ── Launch handling ────────────────────────────────────────────────

    fn handle_launch(&mut self) {
        log::debug!(
            "MaxwellDMA: LAUNCH src=0x{:X} dst=0x{:X} pitch_in={} pitch_out={} {}x{}",
            self.src_addr(),
            self.dst_addr(),
            self.pitch_in(),
            self.pitch_out(),
            self.line_length(),
            self.line_count(),
        );
        self.pending_launch = true;
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

        if method == LAUNCH_DMA {
            self.handle_launch();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_address_accessors() {
        let mut eng = MaxwellDMA::new();
        eng.write_reg(SRC_ADDR_HIGH, 0xAB);
        eng.write_reg(SRC_ADDR_LOW, 0xCDEF_0000);
        assert_eq!(eng.src_addr(), 0xAB_CDEF_0000);

        eng.write_reg(DST_ADDR_HIGH, 0x12);
        eng.write_reg(DST_ADDR_LOW, 0x3456_7890);
        assert_eq!(eng.dst_addr(), 0x12_3456_7890);
    }

    #[test]
    fn test_pitch_and_size_accessors() {
        let mut eng = MaxwellDMA::new();
        eng.write_reg(PITCH_IN, 5120);
        eng.write_reg(PITCH_OUT, 5120);
        eng.write_reg(LINE_LENGTH, 5120);
        eng.write_reg(LINE_COUNT, 720);
        assert_eq!(eng.pitch_in(), 5120);
        assert_eq!(eng.pitch_out(), 5120);
        assert_eq!(eng.line_length(), 5120);
        assert_eq!(eng.line_count(), 720);
    }

    #[test]
    fn test_launch_trigger_sets_pending() {
        let mut eng = MaxwellDMA::new();
        assert!(!eng.pending_launch);

        eng.write_reg(SRC_ADDR_HIGH, 0);
        eng.write_reg(SRC_ADDR_LOW, 0x1000);
        eng.write_reg(DST_ADDR_HIGH, 0);
        eng.write_reg(DST_ADDR_LOW, 0x2000);
        eng.write_reg(PITCH_IN, 5120);
        eng.write_reg(PITCH_OUT, 5120);
        eng.write_reg(LINE_LENGTH, 5120);
        eng.write_reg(LINE_COUNT, 720);

        // Trigger DMA launch
        eng.write_reg(LAUNCH_DMA, 1);
        assert!(eng.pending_launch);
    }

    #[test]
    fn test_no_trigger_without_launch_method() {
        let mut eng = MaxwellDMA::new();
        eng.write_reg(0x200, 42); // Random register
        assert!(!eng.pending_launch);
    }
}
