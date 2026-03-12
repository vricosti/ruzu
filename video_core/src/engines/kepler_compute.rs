// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Kepler Compute engine (NV class B1C0).
//!
//! Handles compute shader dispatch. When the game writes the LAUNCH register,
//! a Queue Meta Data (QMD) descriptor is read from GPU memory, parsed, and
//! recorded as a `DispatchCall`. The backend later consumes these via
//! `take_dispatch_calls()`.

use super::{ClassId, Engine, PendingWrite, ENGINE_REG_COUNT};

// ── Register offset constants (method addresses) ────────────────────────────

/// Dispatch trigger — any write to this method queues a compute launch.
const LAUNCH: u32 = 0xAF;

/// QMD address register — bits [31:0] of `regs[0xAD]`, shifted left by 8.
const QMD_ADDRESS: u32 = 0xAD;

/// Texture sampler pool base: +0 addr_high, +1 addr_low, +2 limit.
const TSC_BASE: u32 = 0x557;

/// Texture image pool base: +0 addr_high, +1 addr_low, +2 limit.
const TIC_BASE: u32 = 0x55D;

/// Shader code base address: +0 addr_high, +1 addr_low.
const CODE_LOC_BASE: u32 = 0x582;

/// Texture constant buffer index.
const TEX_CB_INDEX: u32 = 0x982;

// ── QMD constants and types ─────────────────────────────────────────────────

/// Word count of a Queue Meta Data descriptor (256 bytes / 4).
const QMD_WORD_COUNT: usize = 64;

/// Constant buffer configuration from a QMD descriptor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct QmdConstBuffer {
    pub address: u64,
    pub size: u32,
}

/// Parsed Queue Meta Data (QMD) descriptor — 256-byte structure at a GPU VA.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueueMetaData {
    pub grid_dim_x: u32,
    pub grid_dim_y: u32,
    pub grid_dim_z: u32,
    pub block_dim_x: u32,
    pub block_dim_y: u32,
    pub block_dim_z: u32,
    pub shared_alloc: u32,
    pub program_start: u32,
    pub const_buffer_enable_mask: u32,
    pub const_buffers: [QmdConstBuffer; 8],
    pub local_pos_alloc: u32,
    pub gpr_alloc: u32,
}

impl QueueMetaData {
    /// Parse a QMD from 64 little-endian u32 words.
    pub fn from_words(w: &[u32; QMD_WORD_COUNT]) -> Self {
        // Word 0x08: program_start (full word).
        let program_start = w[0x08];

        // Word 0x0C: grid_dim_x in bits [30:0].
        let grid_dim_x = w[0x0C] & 0x7FFF_FFFF;

        // Word 0x0D: grid_dim_y in bits [15:0], grid_dim_z in bits [31:16].
        let grid_dim_y = w[0x0D] & 0xFFFF;
        let grid_dim_z = (w[0x0D] >> 16) & 0xFFFF;

        // Word 0x11: shared_alloc in bits [17:0].
        let shared_alloc = w[0x11] & 0x3FFFF;

        // Word 0x12: block_dim_x in bits [31:16].
        let block_dim_x = (w[0x12] >> 16) & 0xFFFF;

        // Word 0x13: block_dim_y in bits [15:0], block_dim_z in bits [31:16].
        let block_dim_y = w[0x13] & 0xFFFF;
        let block_dim_z = (w[0x13] >> 16) & 0xFFFF;

        // Word 0x14: const_buffer_enable_mask in bits [7:0].
        let const_buffer_enable_mask = w[0x14] & 0xFF;

        // Words 0x1D..0x2C: 8 constant buffer configs, 2 words each.
        // Per CB: word0 = addr_low, word1 = addr_high[7:0] | size[31:15].
        let mut const_buffers = [QmdConstBuffer::default(); 8];
        for i in 0..8 {
            let base = 0x1D + i * 2;
            let addr_low = w[base] as u64;
            let word1 = w[base + 1];
            let addr_high = (word1 & 0xFF) as u64;
            let size = (word1 >> 15) & 0x1FFFF;
            const_buffers[i] = QmdConstBuffer {
                address: (addr_high << 32) | addr_low,
                size,
            };
        }

        // Word 0x2D: local_pos_alloc in bits [19:0].
        let local_pos_alloc = w[0x2D] & 0xFFFFF;

        // Word 0x2E: gpr_alloc in bits [28:24].
        let gpr_alloc = (w[0x2E] >> 24) & 0x1F;

        Self {
            grid_dim_x,
            grid_dim_y,
            grid_dim_z,
            block_dim_x,
            block_dim_y,
            block_dim_z,
            shared_alloc,
            program_start,
            const_buffer_enable_mask,
            const_buffers,
            local_pos_alloc,
            gpr_alloc,
        }
    }
}

/// A recorded compute dispatch with all relevant state.
#[derive(Debug, Clone)]
pub struct DispatchCall {
    pub qmd: QueueMetaData,
    pub qmd_address: u64,
    pub code_address: u64,
    pub tsc_address: u64,
    pub tsc_limit: u32,
    pub tic_address: u64,
    pub tic_limit: u32,
    pub tex_cb_index: u32,
}

// ── Engine struct ───────────────────────────────────────────────────────────

pub struct KeplerCompute {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
    dispatch_calls: Vec<DispatchCall>,
    /// QMD GPU VA set on LAUNCH write, consumed by execute_pending.
    pending_launch: Option<u64>,
}

impl KeplerCompute {
    pub fn new() -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
            dispatch_calls: Vec::new(),
            pending_launch: None,
        }
    }

    // ── Register accessors ──────────────────────────────────────────────

    /// GPU VA of the QMD descriptor (regs[0xAD] << 8).
    pub fn launch_desc_address(&self) -> u64 {
        (self.regs[QMD_ADDRESS as usize] as u64) << 8
    }

    /// Shader code base address from CODE_LOC_BASE registers.
    pub fn code_address(&self) -> u64 {
        let high = self.regs[CODE_LOC_BASE as usize] as u64;
        let low = self.regs[(CODE_LOC_BASE + 1) as usize] as u64;
        (high << 32) | low
    }

    /// Texture sampler pool address.
    pub fn tsc_address(&self) -> u64 {
        let high = self.regs[TSC_BASE as usize] as u64;
        let low = self.regs[(TSC_BASE + 1) as usize] as u64;
        (high << 32) | low
    }

    /// Texture sampler pool limit.
    pub fn tsc_limit(&self) -> u32 {
        self.regs[(TSC_BASE + 2) as usize]
    }

    /// Texture image pool address.
    pub fn tic_address(&self) -> u64 {
        let high = self.regs[TIC_BASE as usize] as u64;
        let low = self.regs[(TIC_BASE + 1) as usize] as u64;
        (high << 32) | low
    }

    /// Texture image pool limit.
    pub fn tic_limit(&self) -> u32 {
        self.regs[(TIC_BASE + 2) as usize]
    }

    /// Texture constant buffer index.
    pub fn tex_cb_index(&self) -> u32 {
        self.regs[TEX_CB_INDEX as usize]
    }

    /// Drain and return all recorded dispatch calls.
    pub fn take_dispatch_calls(&mut self) -> Vec<DispatchCall> {
        std::mem::take(&mut self.dispatch_calls)
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

        if method == LAUNCH {
            let qmd_addr = self.launch_desc_address();
            self.pending_launch = Some(qmd_addr);
            log::debug!("KeplerCompute: LAUNCH queued, QMD @ 0x{:X}", qmd_addr);
        } else {
            log::trace!("KeplerCompute: reg[0x{:X}] = 0x{:X}", method, value);
        }
    }

    fn execute_pending(
        &mut self,
        read_gpu: &dyn Fn(u64, &mut [u8]),
    ) -> Vec<PendingWrite> {
        if let Some(qmd_addr) = self.pending_launch.take() {
            // Read 256 bytes (64 words) of QMD from GPU memory.
            let mut raw = [0u8; QMD_WORD_COUNT * 4];
            read_gpu(qmd_addr, &mut raw);

            // Convert bytes to u32 words (little-endian).
            let mut words = [0u32; QMD_WORD_COUNT];
            for (i, chunk) in raw.chunks_exact(4).enumerate() {
                words[i] = u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
            }

            let qmd = QueueMetaData::from_words(&words);

            let dispatch = DispatchCall {
                qmd,
                qmd_address: qmd_addr,
                code_address: self.code_address(),
                tsc_address: self.tsc_address(),
                tsc_limit: self.tsc_limit(),
                tic_address: self.tic_address(),
                tic_limit: self.tic_limit(),
                tex_cb_index: self.tex_cb_index(),
            };

            log::debug!(
                "KeplerCompute: dispatch grid=({},{},{}) block=({},{},{}) code=0x{:X}",
                dispatch.qmd.grid_dim_x,
                dispatch.qmd.grid_dim_y,
                dispatch.qmd.grid_dim_z,
                dispatch.qmd.block_dim_x,
                dispatch.qmd.block_dim_y,
                dispatch.qmd.block_dim_z,
                dispatch.code_address,
            );

            self.dispatch_calls.push(dispatch);
        }

        vec![]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_class_id() {
        let engine = KeplerCompute::new();
        assert_eq!(engine.class_id(), ClassId::Compute);
    }

    #[test]
    fn test_write_reg() {
        let mut engine = KeplerCompute::new();
        engine.write_reg(0x100, 0xDEAD);
        assert_eq!(engine.regs[0x100], 0xDEAD);
    }

    #[test]
    fn test_write_reg_high_method() {
        let mut engine = KeplerCompute::new();
        engine.write_reg(TEX_CB_INDEX, 5);
        assert_eq!(engine.regs[TEX_CB_INDEX as usize], 5);
        assert_eq!(engine.tex_cb_index(), 5);
    }

    #[test]
    fn test_launch_desc_address() {
        let mut engine = KeplerCompute::new();
        engine.regs[QMD_ADDRESS as usize] = 0x1234;
        assert_eq!(engine.launch_desc_address(), 0x1234 << 8);
    }

    #[test]
    fn test_code_address() {
        let mut engine = KeplerCompute::new();
        engine.regs[CODE_LOC_BASE as usize] = 0x0001; // high
        engine.regs[(CODE_LOC_BASE + 1) as usize] = 0x2000; // low
        assert_eq!(engine.code_address(), 0x0001_0000_2000);
    }

    #[test]
    fn test_tsc_address() {
        let mut engine = KeplerCompute::new();
        engine.regs[TSC_BASE as usize] = 0x0002; // high
        engine.regs[(TSC_BASE + 1) as usize] = 0x4000; // low
        engine.regs[(TSC_BASE + 2) as usize] = 64; // limit
        assert_eq!(engine.tsc_address(), 0x0002_0000_4000);
        assert_eq!(engine.tsc_limit(), 64);
    }

    #[test]
    fn test_tic_address() {
        let mut engine = KeplerCompute::new();
        engine.regs[TIC_BASE as usize] = 0x0003; // high
        engine.regs[(TIC_BASE + 1) as usize] = 0x8000; // low
        engine.regs[(TIC_BASE + 2) as usize] = 128; // limit
        assert_eq!(engine.tic_address(), 0x0003_0000_8000);
        assert_eq!(engine.tic_limit(), 128);
    }

    #[test]
    fn test_qmd_from_words_basic() {
        let mut w = [0u32; QMD_WORD_COUNT];
        w[0x08] = 0x500; // program_start
        w[0x0C] = 64; // grid_dim_x
        w[0x0D] = 32 | (16 << 16); // grid_dim_y=32, grid_dim_z=16
        w[0x11] = 0x1000; // shared_alloc
        w[0x12] = 256 << 16; // block_dim_x=256
        w[0x13] = 4 | (2 << 16); // block_dim_y=4, block_dim_z=2
        w[0x2D] = 0x400; // local_pos_alloc
        w[0x2E] = 32 << 24; // gpr_alloc (bits [28:24] → mask 0x1F → 0)

        let qmd = QueueMetaData::from_words(&w);
        assert_eq!(qmd.program_start, 0x500);
        assert_eq!(qmd.grid_dim_x, 64);
        assert_eq!(qmd.grid_dim_y, 32);
        assert_eq!(qmd.grid_dim_z, 16);
        assert_eq!(qmd.shared_alloc, 0x1000);
        assert_eq!(qmd.block_dim_x, 256);
        assert_eq!(qmd.block_dim_y, 4);
        assert_eq!(qmd.block_dim_z, 2);
        assert_eq!(qmd.local_pos_alloc, 0x400);
        // gpr_alloc: bits [28:24] of 32<<24 = 0b10_0000 >> 24 = 32, masked &0x1F = 0
        assert_eq!(qmd.gpr_alloc, 0);
    }

    #[test]
    fn test_qmd_from_words_const_buffers() {
        let mut w = [0u32; QMD_WORD_COUNT];
        w[0x14] = 0b0000_0101; // enable CB 0 and CB 2

        // CB 0 at words 0x1D, 0x1E: addr_low=0x1000, word1: addr_high=1, size=512
        w[0x1D] = 0x1000;
        w[0x1E] = 1 | (512 << 15); // addr_high[7:0]=1, size in bits[31:15]

        // CB 2 at words 0x21, 0x22: addr_low=0x2000, word1: addr_high=0, size=256
        w[0x21] = 0x2000;
        w[0x22] = 0 | (256 << 15);

        let qmd = QueueMetaData::from_words(&w);
        assert_eq!(qmd.const_buffer_enable_mask, 0b0000_0101);
        assert_eq!(qmd.const_buffers[0].address, 0x0001_0000_1000);
        assert_eq!(qmd.const_buffers[0].size, 512);
        assert_eq!(qmd.const_buffers[2].address, 0x2000);
        assert_eq!(qmd.const_buffers[2].size, 256);
        // Unused CB should be zero.
        assert_eq!(qmd.const_buffers[1].address, 0);
        assert_eq!(qmd.const_buffers[1].size, 0);
    }

    #[test]
    fn test_qmd_from_words_zeros() {
        let w = [0u32; QMD_WORD_COUNT];
        let qmd = QueueMetaData::from_words(&w);
        assert_eq!(qmd.grid_dim_x, 0);
        assert_eq!(qmd.grid_dim_y, 0);
        assert_eq!(qmd.grid_dim_z, 0);
        assert_eq!(qmd.block_dim_x, 0);
        assert_eq!(qmd.block_dim_y, 0);
        assert_eq!(qmd.block_dim_z, 0);
        assert_eq!(qmd.shared_alloc, 0);
        assert_eq!(qmd.program_start, 0);
        assert_eq!(qmd.const_buffer_enable_mask, 0);
        assert_eq!(qmd.local_pos_alloc, 0);
        assert_eq!(qmd.gpr_alloc, 0);
        for cb in &qmd.const_buffers {
            assert_eq!(cb.address, 0);
            assert_eq!(cb.size, 0);
        }
    }

    /// Helper: build a QMD byte blob from words for use with execute_pending.
    fn make_qmd_bytes(w: &[u32; QMD_WORD_COUNT]) -> Vec<u8> {
        let mut bytes = vec![0u8; QMD_WORD_COUNT * 4];
        for (i, &word) in w.iter().enumerate() {
            bytes[i * 4..i * 4 + 4].copy_from_slice(&word.to_le_bytes());
        }
        bytes
    }

    #[test]
    fn test_launch_creates_dispatch() {
        let mut engine = KeplerCompute::new();

        // Set up code address.
        engine.regs[CODE_LOC_BASE as usize] = 0x0001;
        engine.regs[(CODE_LOC_BASE + 1) as usize] = 0xA000;

        // Set up TSC/TIC pools.
        engine.regs[TSC_BASE as usize] = 0;
        engine.regs[(TSC_BASE + 1) as usize] = 0x5000;
        engine.regs[(TSC_BASE + 2) as usize] = 32;
        engine.regs[TIC_BASE as usize] = 0;
        engine.regs[(TIC_BASE + 1) as usize] = 0x6000;
        engine.regs[(TIC_BASE + 2) as usize] = 64;
        engine.regs[TEX_CB_INDEX as usize] = 2;

        // Prepare QMD at address 0x100 << 8 = 0x10000.
        engine.regs[QMD_ADDRESS as usize] = 0x100;

        let mut qmd_words = [0u32; QMD_WORD_COUNT];
        qmd_words[0x08] = 0x200; // program_start
        qmd_words[0x0C] = 8; // grid_dim_x
        qmd_words[0x0D] = 4 | (2 << 16); // grid_dim_y=4, z=2
        qmd_words[0x12] = 128 << 16; // block_dim_x=128
        qmd_words[0x13] = 1 | (1 << 16); // block_dim_y=1, z=1
        let qmd_bytes = make_qmd_bytes(&qmd_words);

        // Write LAUNCH.
        engine.write_reg(LAUNCH, 1);

        // Execute pending with a mock reader.
        let writes = engine.execute_pending(&|addr, buf| {
            assert_eq!(addr, 0x10000);
            buf[..qmd_bytes.len()].copy_from_slice(&qmd_bytes);
        });
        assert!(writes.is_empty());

        let dispatches = engine.take_dispatch_calls();
        assert_eq!(dispatches.len(), 1);
        let d = &dispatches[0];
        assert_eq!(d.qmd_address, 0x10000);
        assert_eq!(d.code_address, 0x0001_0000_A000);
        assert_eq!(d.tsc_address, 0x5000);
        assert_eq!(d.tsc_limit, 32);
        assert_eq!(d.tic_address, 0x6000);
        assert_eq!(d.tic_limit, 64);
        assert_eq!(d.tex_cb_index, 2);
        assert_eq!(d.qmd.grid_dim_x, 8);
        assert_eq!(d.qmd.grid_dim_y, 4);
        assert_eq!(d.qmd.grid_dim_z, 2);
        assert_eq!(d.qmd.block_dim_x, 128);
        assert_eq!(d.qmd.program_start, 0x200);
    }

    #[test]
    fn test_multiple_dispatches() {
        let mut engine = KeplerCompute::new();
        engine.regs[QMD_ADDRESS as usize] = 0x100;

        let qmd_bytes = make_qmd_bytes(&[0u32; QMD_WORD_COUNT]);

        // First launch.
        engine.write_reg(LAUNCH, 1);
        engine.execute_pending(&|_addr, buf| {
            buf[..qmd_bytes.len()].copy_from_slice(&qmd_bytes);
        });

        // Second launch (update QMD address).
        engine.regs[QMD_ADDRESS as usize] = 0x200;
        engine.write_reg(LAUNCH, 1);
        engine.execute_pending(&|_addr, buf| {
            buf[..qmd_bytes.len()].copy_from_slice(&qmd_bytes);
        });

        let dispatches = engine.take_dispatch_calls();
        assert_eq!(dispatches.len(), 2);
        assert_eq!(dispatches[0].qmd_address, 0x100 << 8);
        assert_eq!(dispatches[1].qmd_address, 0x200 << 8);
    }

    #[test]
    fn test_take_dispatch_calls_drains() {
        let mut engine = KeplerCompute::new();
        engine.regs[QMD_ADDRESS as usize] = 0x100;
        let qmd_bytes = make_qmd_bytes(&[0u32; QMD_WORD_COUNT]);

        engine.write_reg(LAUNCH, 1);
        engine.execute_pending(&|_addr, buf| {
            buf[..qmd_bytes.len()].copy_from_slice(&qmd_bytes);
        });

        let first = engine.take_dispatch_calls();
        assert_eq!(first.len(), 1);

        // Second take should be empty.
        let second = engine.take_dispatch_calls();
        assert!(second.is_empty());
    }

    #[test]
    fn test_no_pending_no_dispatch() {
        let mut engine = KeplerCompute::new();

        // Call execute_pending without writing LAUNCH.
        let writes = engine.execute_pending(&|_addr, _buf| {
            panic!("should not read GPU memory");
        });
        assert!(writes.is_empty());

        let dispatches = engine.take_dispatch_calls();
        assert!(dispatches.is_empty());
    }
}
