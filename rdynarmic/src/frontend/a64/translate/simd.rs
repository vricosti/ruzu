use crate::frontend::a64::decoder::DecodedInst;
use crate::frontend::a64::translate::visitor::TranslatorVisitor;

/// SIMD/NEON instruction handlers.
/// Most SIMD instructions are complex and fall back to interpreter initially.
/// The most commonly used ones will be JIT-compiled incrementally.
impl<'a> TranslatorVisitor<'a> {
    // --- Crypto (AES/SHA) ---
    pub fn aese(&mut self, _inst: &DecodedInst) -> bool {
        self.interpret_this_instruction()
    }
    pub fn aesd(&mut self, _inst: &DecodedInst) -> bool {
        self.interpret_this_instruction()
    }
    pub fn aesmc(&mut self, _inst: &DecodedInst) -> bool {
        self.interpret_this_instruction()
    }
    pub fn aesimc(&mut self, _inst: &DecodedInst) -> bool {
        self.interpret_this_instruction()
    }

    // --- DC/IC cache operations (mostly NOP in userspace emulation) ---
    pub fn dc_ivac(&mut self, _inst: &DecodedInst) -> bool {
        true
    }
    pub fn dc_isw(&mut self, _inst: &DecodedInst) -> bool {
        true
    }
    pub fn dc_csw(&mut self, _inst: &DecodedInst) -> bool {
        true
    }
    pub fn dc_cisw(&mut self, _inst: &DecodedInst) -> bool {
        true
    }
    /// `DC ZVA, Xt` — Data Cache Zero by VA. Zeros a `2^DCZID_EL0[3:0]`-word
    /// block aligned down from `Xt`. On Switch hardware the block size is
    /// 64 bytes (DCZID_EL0 = 4 → 16 words = 64 B), and userspace zero loops
    /// rely on this for fast bulk zeroing (e.g., libnx's `random_bytes` uses
    /// it as part of a mid-init clear).
    ///
    /// Previously fell back to `interpret_this_instruction` which (in ruzu)
    /// is a no-op that exits the JIT and re-enters at PC+4 — the surrounding
    /// counter loop then iterates millions of times at JIT-exit speed,
    /// effectively wedging boot. Now we emit the 64-byte zero inline so the
    /// loop runs at native JIT speed.
    pub fn dc_zva(&mut self, inst: &DecodedInst) -> bool {
        use crate::frontend::a64::types::Reg;
        use crate::ir::acc_type::AccType;
        let rt = Reg::from_u32(inst.rd());
        let base = self.x(64, rt);
        // Align down to a 64-byte boundary.
        let mask = self.ir.ir().imm64(!0x3Fu64);
        let aligned = self.ir.ir().and_64(base, mask);
        // Emit 8 × 8-byte zero stores covering the full 64-byte block.
        let zero = self.ir.ir().imm64(0);
        for i in 0..8 {
            let offset = self.ir.ir().imm64((i * 8) as u64);
            let addr = self.addr_add(aligned, offset);
            self.mem_write(addr, zero, 8, AccType::Normal);
        }
        true
    }
    pub fn dc_cvac(&mut self, _inst: &DecodedInst) -> bool {
        true
    }
    pub fn dc_cvau(&mut self, _inst: &DecodedInst) -> bool {
        true
    }
    pub fn dc_cvap(&mut self, _inst: &DecodedInst) -> bool {
        true
    }
    pub fn dc_civac(&mut self, _inst: &DecodedInst) -> bool {
        true
    }
    pub fn ic_iallu(&mut self, _inst: &DecodedInst) -> bool {
        true
    }
    pub fn ic_ialluis(&mut self, _inst: &DecodedInst) -> bool {
        true
    }
    pub fn ic_ivau(&mut self, _inst: &DecodedInst) -> bool {
        true
    }
}
