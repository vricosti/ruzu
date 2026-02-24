// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Fermi 2D engine stub (NV class 902D).
//!
//! Handles 2D blitting operations (surface copies, fills). Detects blit
//! trigger writes and logs parameters; actual pixel copy is not yet implemented.

use super::{ClassId, Engine, PendingWrite, ENGINE_REG_COUNT};

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

    fn execute_pending(
        &mut self,
        read_gpu: &dyn Fn(u64, &mut [u8]),
    ) -> Vec<PendingWrite> {
        if !self.pending_blit {
            return vec![];
        }
        self.pending_blit = false;

        let src_h = self.src_height();
        let dst_h = self.dst_height();
        let height = src_h.min(dst_h);
        if height == 0 {
            return vec![];
        }

        let sp = self.src_pitch();
        let dp = self.dst_pitch();
        let copy_width = sp.min(dp);
        if copy_width == 0 {
            return vec![];
        }

        // Read source surface.
        let src_size = (sp as u64 * src_h as u64) as usize;
        let mut src_buf = vec![0u8; src_size];
        read_gpu(self.src_addr(), &mut src_buf);

        // Build destination buffer line-by-line.
        let dst_size = (dp as u64 * dst_h as u64) as usize;
        let mut dst_buf = vec![0u8; dst_size];
        for line in 0..height {
            let src_off = (line * sp) as usize;
            let dst_off = (line * dp) as usize;
            let w = copy_width as usize;
            if src_off + w <= src_buf.len() && dst_off + w <= dst_buf.len() {
                dst_buf[dst_off..dst_off + w].copy_from_slice(&src_buf[src_off..src_off + w]);
            }
        }

        log::debug!(
            "Fermi2D: blit executed {}x{} (sp={} dp={} cw={}) src=0x{:X} -> dst=0x{:X}",
            copy_width, height, sp, dp, copy_width,
            self.src_addr(), self.dst_addr()
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

    #[test]
    fn test_blit_copies_pixels() {
        let mut eng = Fermi2D::new();

        // Set up matching src/dst: 4x2, pitch=16 (4 pixels * 4 bytes).
        eng.write_reg(SRC_ADDR_HIGH, 0);
        eng.write_reg(SRC_ADDR_LOW, 0x1000);
        eng.write_reg(SRC_WIDTH, 4);
        eng.write_reg(SRC_HEIGHT, 2);
        eng.write_reg(SRC_PITCH, 16);

        eng.write_reg(DST_ADDR_HIGH, 0);
        eng.write_reg(DST_ADDR_LOW, 0x2000);
        eng.write_reg(DST_WIDTH, 4);
        eng.write_reg(DST_HEIGHT, 2);
        eng.write_reg(DST_PITCH, 16);

        // Trigger blit.
        eng.write_reg(BLIT_TRIGGER, 1);
        assert!(eng.pending_blit);

        // Source data: 2 rows of 16 bytes each.
        let src_data: Vec<u8> = (0..32).collect();

        let writes = eng.execute_pending(&|addr, buf| {
            assert_eq!(addr, 0x1000);
            let len = buf.len().min(src_data.len());
            buf[..len].copy_from_slice(&src_data[..len]);
        });

        assert!(!eng.pending_blit);
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].gpu_va, 0x2000);
        assert_eq!(writes[0].data, src_data);
    }

    #[test]
    fn test_blit_different_pitches() {
        let mut eng = Fermi2D::new();

        // Source: pitch=8, 2 rows.
        eng.write_reg(SRC_ADDR_HIGH, 0);
        eng.write_reg(SRC_ADDR_LOW, 0x1000);
        eng.write_reg(SRC_WIDTH, 2);
        eng.write_reg(SRC_HEIGHT, 2);
        eng.write_reg(SRC_PITCH, 8);

        // Destination: pitch=16 (wider stride), 2 rows.
        eng.write_reg(DST_ADDR_HIGH, 0);
        eng.write_reg(DST_ADDR_LOW, 0x2000);
        eng.write_reg(DST_WIDTH, 4);
        eng.write_reg(DST_HEIGHT, 2);
        eng.write_reg(DST_PITCH, 16);

        eng.write_reg(BLIT_TRIGGER, 1);

        // Source: 2 rows * 8 bytes = 16 bytes.
        let src_data: Vec<u8> = vec![
            1, 2, 3, 4, 5, 6, 7, 8, // row 0
            9, 10, 11, 12, 13, 14, 15, 16, // row 1
        ];

        let writes = eng.execute_pending(&|addr, buf| {
            assert_eq!(addr, 0x1000);
            let len = buf.len().min(src_data.len());
            buf[..len].copy_from_slice(&src_data[..len]);
        });

        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].gpu_va, 0x2000);
        // copy_width = min(8, 16) = 8; each row copies 8 bytes into a 16-byte stride.
        let dst = &writes[0].data;
        assert_eq!(dst.len(), 32); // 2 rows * 16 bytes
        assert_eq!(&dst[0..8], &[1, 2, 3, 4, 5, 6, 7, 8]);
        assert_eq!(&dst[8..16], &[0, 0, 0, 0, 0, 0, 0, 0]); // padding
        assert_eq!(&dst[16..24], &[9, 10, 11, 12, 13, 14, 15, 16]);
        assert_eq!(&dst[24..32], &[0, 0, 0, 0, 0, 0, 0, 0]); // padding
    }
}
