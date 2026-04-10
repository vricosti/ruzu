// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell DMA engine stub (NV class B0B5).
//!
//! Handles GPU memory copy operations. Detects launch trigger writes and logs
//! parameters; actual DMA copy is not yet implemented.

use std::sync::Arc;

use parking_lot::Mutex;

use super::engine_interface::{EngineInterface, EngineInterfaceState};
use super::{ClassId, Engine, PendingWrite, ENGINE_REG_COUNT};
use crate::memory_manager::MemoryManager;
use crate::rasterizer_interface::RasterizerInterface;

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
    interface_state: EngineInterfaceState,
    memory_manager: Arc<Mutex<MemoryManager>>,
    /// Set when a DMA launch trigger is detected; consumed by tests / future logic.
    pub pending_launch: bool,
    rasterizer: Option<[usize; 2]>,
}

impl MaxwellDMA {
    /// Corresponds to upstream `MaxwellDMA(Core::System&, MemoryManager&)`.
    /// Rust stores the upstream `MemoryManager&` owner directly; the broader
    /// `System&` constructor dependency remains outside this bounded slice.
    pub fn new(memory_manager: Arc<Mutex<MemoryManager>>) -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
            interface_state: {
                let mut state = EngineInterfaceState::new();
                state.execution_mask[LAUNCH_DMA as usize] = true;
                state
            },
            memory_manager,
            pending_launch: false,
            rasterizer: None,
        }
    }

    /// Corresponds to upstream `MaxwellDMA::CallMethod`.
    pub fn call_method(&mut self, method: u32, argument: u32, _is_last_call: bool) {
        let idx = method as usize;
        if idx < ENGINE_REG_COUNT {
            self.regs[idx] = argument;
        }
        if method == LAUNCH_DMA {
            self.handle_launch();
        }
    }

    /// Corresponds to upstream `MaxwellDMA::CallMultiMethod`.
    pub fn call_multi_method(
        &mut self,
        method: u32,
        args: &[u32],
        _amount: u32,
        _methods_pending: u32,
    ) {
        for &arg in args {
            self.call_method(method, arg, false);
        }
    }

    /// Corresponds to `MaxwellDMA::BindRasterizer`.
    pub fn bind_rasterizer(&mut self, rasterizer: &dyn RasterizerInterface) {
        self.rasterizer = Some(unsafe {
            std::mem::transmute::<*const dyn RasterizerInterface, [usize; 2]>(rasterizer)
        });
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

impl EngineInterface for MaxwellDMA {
    fn call_method(&mut self, method: u32, method_argument: u32, is_last_call: bool) {
        MaxwellDMA::call_method(self, method, method_argument, is_last_call);
    }

    fn call_multi_method(
        &mut self,
        method: u32,
        base_start: &[u32],
        amount: u32,
        methods_pending: u32,
    ) {
        MaxwellDMA::call_multi_method(self, method, base_start, amount, methods_pending);
    }

    fn consume_sink_impl(&mut self) {
        let sink = std::mem::take(&mut self.interface_state.method_sink);
        for (method, value) in sink {
            let idx = method as usize;
            if idx < ENGINE_REG_COUNT {
                self.regs[idx] = value;
            }
        }
    }

    fn execution_mask(&self) -> &[bool] {
        &self.interface_state.execution_mask
    }

    fn push_method_sink(&mut self, method: u32, value: u32) {
        self.interface_state.method_sink.push((method, value));
    }

    fn set_current_dma_segment(&mut self, segment: u64) {
        self.interface_state.current_dma_segment = segment;
    }

    fn current_dirty(&self) -> bool {
        self.interface_state.current_dirty
    }

    fn set_current_dirty(&mut self, dirty: bool) {
        self.interface_state.current_dirty = dirty;
    }
}

impl Default for MaxwellDMA {
    fn default() -> Self {
        Self::new(Arc::new(Mutex::new(MemoryManager::new(0))))
    }
}

impl Engine for MaxwellDMA {
    fn class_id(&self) -> ClassId {
        ClassId::Dma
    }

    fn write_reg(&mut self, method: u32, value: u32) {
        log::trace!("MaxwellDMA: reg[0x{:X}] = 0x{:X}", method, value);
        self.call_method(method, value, true);
    }

    fn execute_pending(&mut self, read_gpu: &dyn Fn(u64, &mut [u8])) -> Vec<PendingWrite> {
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
            ll,
            lines,
            pi,
            po,
            self.src_addr(),
            self.dst_addr()
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

    fn new_test_engine() -> MaxwellDMA {
        MaxwellDMA::new(Arc::new(Mutex::new(MemoryManager::new(0))))
    }

    #[test]
    fn test_address_accessors() {
        let mut eng = new_test_engine();
        eng.write_reg(SRC_ADDR_HIGH, 0xAB);
        eng.write_reg(SRC_ADDR_LOW, 0xCDEF_0000);
        assert_eq!(eng.src_addr(), 0xAB_CDEF_0000);

        eng.write_reg(DST_ADDR_HIGH, 0x12);
        eng.write_reg(DST_ADDR_LOW, 0x3456_7890);
        assert_eq!(eng.dst_addr(), 0x12_3456_7890);
    }

    #[test]
    fn test_pitch_and_size_accessors() {
        let mut eng = new_test_engine();
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
        let mut eng = new_test_engine();
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
        let mut eng = new_test_engine();
        eng.write_reg(0x200, 42); // Random register
        assert!(!eng.pending_launch);
    }

    #[test]
    fn test_bind_rasterizer_stores_reference() {
        let syncpoints =
            std::sync::Arc::new(crate::host1x::syncpoint_manager::SyncpointManager::new());
        let rasterizer = crate::renderer_null::null_rasterizer::RasterizerNull::new(syncpoints);
        let mut eng = new_test_engine();
        assert!(eng.rasterizer.is_none());
        eng.bind_rasterizer(&rasterizer);
        assert!(eng.rasterizer.is_some());
    }

    #[test]
    fn test_dma_copies_lines() {
        let mut eng = new_test_engine();

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
        let mut eng = new_test_engine();

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

    #[test]
    fn test_call_method_launch_trigger_sets_pending() {
        let mut eng = new_test_engine();
        assert!(!eng.pending_launch);
        eng.call_method(LAUNCH_DMA, 1, true);
        assert!(eng.pending_launch);
    }

    #[test]
    fn test_call_multi_method_launch_trigger_sets_pending() {
        let mut eng = new_test_engine();
        assert!(!eng.pending_launch);
        eng.call_multi_method(LAUNCH_DMA, &[1], 1, 1);
        assert!(eng.pending_launch);
    }

    #[test]
    fn test_constructor_keeps_memory_manager_owner() {
        let memory_manager = Arc::new(Mutex::new(MemoryManager::new(0x44)));
        let eng = MaxwellDMA::new(Arc::clone(&memory_manager));
        assert!(Arc::ptr_eq(&eng.memory_manager, &memory_manager));
    }
}
