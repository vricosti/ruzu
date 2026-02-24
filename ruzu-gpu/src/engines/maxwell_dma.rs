// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell DMA engine stub (NV class B0B5).
//!
//! Handles GPU memory copy operations. Detects launch trigger writes and logs
//! parameters; actual DMA copy is not yet implemented.

use super::{ClassId, Engine, PendingWrite, ENGINE_REG_COUNT};

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

    fn execute_pending(
        &mut self,
        read_gpu: &dyn Fn(u64, &mut [u8]),
    ) -> Vec<PendingWrite> {
        if !self.pending_launch {
            return vec![];
        }
        self.pending_launch = false;

        let lines = self.line_count();
        let ll = self.line_length();
        if lines == 0 || ll == 0 {
            return vec![];
        }

        let pi = self.pitch_in().max(ll);
        let po = self.pitch_out().max(ll);

        // Build destination buffer line-by-line.
        let dst_size = (po as u64 * lines as u64) as usize;
        let mut dst_buf = vec![0u8; dst_size];
        let mut line_buf = vec![0u8; ll as usize];

        for line in 0..lines {
            let src_off = self.src_addr() + (line as u64 * pi as u64);
            read_gpu(src_off, &mut line_buf);
            let dst_off = (line * po) as usize;
            let w = ll as usize;
            if dst_off + w <= dst_buf.len() {
                dst_buf[dst_off..dst_off + w].copy_from_slice(&line_buf);
            }
        }

        log::debug!(
            "MaxwellDMA: copy executed {}x{} (pi={} po={}) src=0x{:X} -> dst=0x{:X}",
            ll, lines, pi, po, self.src_addr(), self.dst_addr()
        );

        vec![PendingWrite {
            gpu_va: self.dst_addr(),
            data: dst_buf,
        }]
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

    #[test]
    fn test_dma_copies_lines() {
        let mut eng = MaxwellDMA::new();

        // 2 lines of 8 bytes each, same pitch.
        eng.write_reg(SRC_ADDR_HIGH, 0);
        eng.write_reg(SRC_ADDR_LOW, 0x1000);
        eng.write_reg(DST_ADDR_HIGH, 0);
        eng.write_reg(DST_ADDR_LOW, 0x2000);
        eng.write_reg(PITCH_IN, 8);
        eng.write_reg(PITCH_OUT, 8);
        eng.write_reg(LINE_LENGTH, 8);
        eng.write_reg(LINE_COUNT, 2);

        eng.write_reg(LAUNCH_DMA, 1);
        assert!(eng.pending_launch);

        // Source data.
        let src: Vec<u8> = (0..16).collect();

        let writes = eng.execute_pending(&|addr, buf| {
            let offset = (addr - 0x1000) as usize;
            let len = buf.len();
            buf.copy_from_slice(&src[offset..offset + len]);
        });

        assert!(!eng.pending_launch);
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].gpu_va, 0x2000);
        assert_eq!(writes[0].data, src);
    }

    #[test]
    fn test_dma_different_pitches() {
        let mut eng = MaxwellDMA::new();

        // Copy 4 bytes per line, 2 lines. pitch_in=8, pitch_out=16.
        eng.write_reg(SRC_ADDR_HIGH, 0);
        eng.write_reg(SRC_ADDR_LOW, 0x1000);
        eng.write_reg(DST_ADDR_HIGH, 0);
        eng.write_reg(DST_ADDR_LOW, 0x2000);
        eng.write_reg(PITCH_IN, 8);
        eng.write_reg(PITCH_OUT, 16);
        eng.write_reg(LINE_LENGTH, 4);
        eng.write_reg(LINE_COUNT, 2);

        eng.write_reg(LAUNCH_DMA, 1);

        // Source memory: pitch_in=8 per line.
        let src = vec![
            1, 2, 3, 4, 0xAA, 0xBB, 0xCC, 0xDD, // line 0 (4 useful + 4 padding)
            5, 6, 7, 8, 0xEE, 0xFF, 0x11, 0x22, // line 1 (4 useful + 4 padding)
        ];

        let writes = eng.execute_pending(&|addr, buf| {
            let offset = (addr - 0x1000) as usize;
            let len = buf.len();
            buf.copy_from_slice(&src[offset..offset + len]);
        });

        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].gpu_va, 0x2000);
        let dst = &writes[0].data;
        assert_eq!(dst.len(), 32); // 2 lines * pitch_out=16
        // Line 0: 4 bytes copied + 12 zeros.
        assert_eq!(&dst[0..4], &[1, 2, 3, 4]);
        assert_eq!(&dst[4..16], &[0; 12]);
        // Line 1: 4 bytes copied + 12 zeros.
        assert_eq!(&dst[16..20], &[5, 6, 7, 8]);
        assert_eq!(&dst[20..32], &[0; 12]);
    }
}
