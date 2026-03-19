// Debug/tracing helpers for physical_core.rs
// Extract these into a separate file so the main module stays clean and committable.
// To disable all debug tracing, comment out the `use` in physical_core.rs.

use crate::arm::arm_interface::ThreadContext;
use std::sync::{Arc, Mutex};
use crate::hle::kernel::k_process::KProcess;

/// State for the instruction ring buffer used to capture the last N instructions
/// before an abort handler is entered.
pub struct InstructionRingBuffer {
    buf: Vec<(u64, u32, [u64; 16])>,
    idx: usize,
    dumped: bool,
}

impl InstructionRingBuffer {
    pub const SIZE: usize = 2000;

    pub fn new() -> Self {
        Self {
            buf: vec![(0, 0, [0; 16]); Self::SIZE],
            idx: 0,
            dumped: false,
        }
    }

    pub fn record(&mut self, ctx: &ThreadContext, insn: u32) {
        self.buf[self.idx] = (
            ctx.pc,
            insn,
            [
                ctx.r[0], ctx.r[1], ctx.r[2], ctx.r[3],
                ctx.r[4], ctx.r[5], ctx.r[6], ctx.r[7],
                ctx.r[8], ctx.r[9], ctx.r[10], ctx.r[11],
                ctx.r[12], ctx.sp, ctx.lr, 0,
            ],
        );
        self.idx = (self.idx + 1) % Self::SIZE;
    }

    /// Log entry to __nnDetailInitLibc0 (real function at 0xc5f740).
    pub fn check_initlibc0_entry(&self, ctx: &ThreadContext) {
        if ctx.pc == 0xc5f740 {
            log::error!(
                "=== __nnDetailInitLibc0 ENTRY: r0={:#x} r1={:#x} r2={:#x} r3={:#x} SP={:#x} LR={:#x} ===",
                ctx.r[0], ctx.r[1], ctx.r[2], ctx.r[3], ctx.sp, ctx.lr
            );
        }
    }

    /// Log return check in rtld (CMP R0, #0 at 0x200908).
    pub fn check_rtld_init_return(&self, ctx: &ThreadContext) {
        if ctx.pc == 0x200908 {
            log::error!(
                "=== rtld init check: R0={:#x} (0=OK, non-0=FAIL) module_r5={:#x} r6={:#x} ===",
                ctx.r[0], ctx.r[5], ctx.r[6]
            );
        }
    }

    /// Detect abort: if PC reaches the abort handler area (0x1d31d00-0x1d32000),
    /// dump the ring buffer and return true.
    pub fn check_and_dump_abort(&mut self, ctx: &ThreadContext) -> bool {
        if self.dumped || ctx.pc < 0x1d31d00 || ctx.pc >= 0x1d32000 {
            return false;
        }
        log::error!(
            "=== INSTRUCTION TRACE (last {} before abort at PC={:#x}) ===",
            Self::SIZE,
            ctx.pc
        );
        for k in 0..Self::SIZE {
            let idx = (self.idx + k) % Self::SIZE;
            let (pc, insn, regs) = self.buf[idx];
            if pc != 0 {
                log::error!(
                    "  [{:>4}] PC={:#010x} [{:#010x}] r0={:#x} r1={:#x} r2={:#x} r3={:#x} r4={:#x} r5={:#x} r6={:#x} r7={:#x} r8={:#x} r9={:#x} r10={:#x} r11={:#x} SP={:#x} LR={:#x}",
                    k, pc, insn,
                    regs[0], regs[1], regs[2], regs[3],
                    regs[4], regs[5], regs[6], regs[7],
                    regs[8], regs[9], regs[10], regs[11],
                    regs[13], regs[14],
                );
            }
        }
        self.dumped = true;
        true
    }
}

/// Dump module memory regions for comparison with zuyu.
/// Called after SetHeapSize (SVC ~#89).
pub fn dump_module_memory(process: &Arc<Mutex<KProcess>>) {
    let proc = process.lock().unwrap();
    for (label, addr) in [
        ("sdk_module_desc(0x22c8000)", 0x22c8000u64),
        ("abort_desc(0x207c850)", 0x207c850u64),
        ("sdk_base(0x1c9c000)", 0x1c9c000u64),
    ] {
        if let Some(memory) = proc.page_table.get_base().m_memory.as_ref() {
            let mut bytes = [0u8; 64];
            memory.lock().unwrap().read_block(addr, &mut bytes);
            log::error!("MEMDUMP {} @ {:#x}: {:02x?}", label, addr, &bytes[..64]);
        } else {
            let mem = proc.process_memory.read().unwrap();
            if mem.is_valid_range(addr, 64) {
                let bytes = mem.read_block(addr, 64);
                log::error!("MEMDUMP {} @ {:#x}: {:02x?}", label, addr, &bytes[..64]);
            } else {
                log::error!("MEMDUMP {} @ {:#x}: NOT MAPPED", label, addr);
            }
        }
    }
}
