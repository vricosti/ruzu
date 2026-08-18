/// Instruction set identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InstructionSet {
    ARM = 0,
    Thumb = 1,
    Jazelle = 2,
    ThumbEE = 3,
}

/// Program Status Register (CPSR) wrapper.
/// Provides bit-field accessors for the ARM32 Current Program Status Register.
///
/// Layout:
///   [31] N  - Negative
///   [30] Z  - Zero
///   [29] C  - Carry
///   [28] V  - Overflow
///   [27] Q  - Sticky overflow (DSP)
///   [26:25] IT[1:0]
///   [24] J  - Jazelle
///   [19:16] GE[3:0]
///   [15:10] IT[7:2]
///   [9]  E  - Endian
///   [8]  A  - Imprecise abort disable
///   [7]  I  - IRQ disable
///   [6]  F  - FIQ disable
///   [5]  T  - Thumb
///   [4:0] M - Mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PSR(pub u32);

impl PSR {
    pub fn new(value: u32) -> Self {
        Self(value)
    }

    pub fn value(self) -> u32 {
        self.0
    }

    // --- Condition flags ---

    pub fn n(self) -> bool {
        self.0 & (1 << 31) != 0
    }
    pub fn z(self) -> bool {
        self.0 & (1 << 30) != 0
    }
    pub fn c(self) -> bool {
        self.0 & (1 << 29) != 0
    }
    pub fn v(self) -> bool {
        self.0 & (1 << 28) != 0
    }
    pub fn q(self) -> bool {
        self.0 & (1 << 27) != 0
    }

    pub fn set_n(&mut self, val: bool) {
        self.set_bit(31, val);
    }
    pub fn set_z(&mut self, val: bool) {
        self.set_bit(30, val);
    }
    pub fn set_c(&mut self, val: bool) {
        self.set_bit(29, val);
    }
    pub fn set_v(&mut self, val: bool) {
        self.set_bit(28, val);
    }
    pub fn set_q(&mut self, val: bool) {
        self.set_bit(27, val);
    }

    pub fn nzcv(self) -> u32 {
        self.0 & 0xF000_0000
    }

    pub fn set_nzcv(&mut self, nzcv: u32) {
        self.0 = (self.0 & !0xF000_0000) | (nzcv & 0xF000_0000);
    }

    // --- GE flags ---

    pub fn ge(self) -> u32 {
        (self.0 >> 16) & 0xF
    }

    pub fn set_ge(&mut self, ge: u32) {
        self.0 = (self.0 & !0x000F_0000) | ((ge & 0xF) << 16);
    }

    // --- IT state ---

    /// Get the full 8-bit IT state. IT[7:2] are at bits [15:10], IT[1:0] at bits [26:25].
    pub fn it(self) -> u8 {
        let upper = ((self.0 >> 10) & 0x3F) as u8; // bits [15:10] -> IT[7:2]
        let lower = ((self.0 >> 25) & 0x3) as u8; // bits [26:25] -> IT[1:0]
        (upper << 2) | lower
    }

    pub fn set_it(&mut self, it: u8) {
        let upper = ((it >> 2) & 0x3F) as u32;
        let lower = (it & 0x3) as u32;
        self.0 = (self.0 & !(0x3F << 10)) | (upper << 10);
        self.0 = (self.0 & !(0x3 << 25)) | (lower << 25);
    }

    // --- T (Thumb) flag ---

    pub fn t(self) -> bool {
        self.0 & (1 << 5) != 0
    }

    pub fn set_t(&mut self, val: bool) {
        self.set_bit(5, val);
    }

    pub fn instruction_set(self) -> InstructionSet {
        let j = self.0 & (1 << 24) != 0;
        let t = self.t();
        match (j, t) {
            (false, false) => InstructionSet::ARM,
            (false, true) => InstructionSet::Thumb,
            (true, false) => InstructionSet::Jazelle,
            (true, true) => InstructionSet::ThumbEE,
        }
    }

    // --- Endian ---

    pub fn e(self) -> bool {
        self.0 & (1 << 9) != 0
    }
    pub fn set_e(&mut self, val: bool) {
        self.set_bit(9, val);
    }

    // --- Interrupt masks ---

    pub fn a(self) -> bool {
        self.0 & (1 << 8) != 0
    }
    pub fn i(self) -> bool {
        self.0 & (1 << 7) != 0
    }
    pub fn f(self) -> bool {
        self.0 & (1 << 6) != 0
    }

    pub fn set_a(&mut self, val: bool) {
        self.set_bit(8, val);
    }
    pub fn set_i(&mut self, val: bool) {
        self.set_bit(7, val);
    }
    pub fn set_f(&mut self, val: bool) {
        self.set_bit(6, val);
    }

    // --- Mode ---

    pub fn mode(self) -> u32 {
        self.0 & 0x1F
    }

    pub fn set_mode(&mut self, mode: u32) {
        self.0 = (self.0 & !0x1F) | (mode & 0x1F);
    }

    // --- Jazelle ---

    pub fn j(self) -> bool {
        self.0 & (1 << 24) != 0
    }
    pub fn set_j(&mut self, val: bool) {
        self.set_bit(24, val);
    }

    // --- Helper ---

    fn set_bit(&mut self, bit: u32, val: bool) {
        if val {
            self.0 |= 1 << bit;
        } else {
            self.0 &= !(1 << bit);
        }
    }
}

impl Default for PSR {
    fn default() -> Self {
        Self(0)
    }
}

/// Mask for CPSR bits that affect code translation.
/// T flag, E flag, IT state bits.
pub const CPSR_MODE_MASK: u32 = 0x0600_FE20;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_psr_flags() {
        let mut psr = PSR::new(0);
        assert!(!psr.n());
        psr.set_n(true);
        assert!(psr.n());
        psr.set_z(true);
        psr.set_c(true);
        psr.set_v(true);
        assert_eq!(psr.nzcv(), 0xF000_0000);
    }

    #[test]
    fn test_psr_ge() {
        let mut psr = PSR::new(0);
        psr.set_ge(0xF);
        assert_eq!(psr.ge(), 0xF);
        psr.set_ge(0x5);
        assert_eq!(psr.ge(), 0x5);
    }

    #[test]
    fn test_psr_it_state() {
        let mut psr = PSR::new(0);
        psr.set_it(0xFF);
        assert_eq!(psr.it(), 0xFF);
        psr.set_it(0xA5);
        assert_eq!(psr.it(), 0xA5);
        psr.set_it(0);
        assert_eq!(psr.it(), 0);
    }

    #[test]
    fn test_psr_thumb() {
        let mut psr = PSR::new(0);
        assert_eq!(psr.instruction_set(), InstructionSet::ARM);
        psr.set_t(true);
        assert_eq!(psr.instruction_set(), InstructionSet::Thumb);
    }

    #[test]
    fn test_psr_round_trip() {
        let val = 0xF00F_FE3F; // Many bits set
        let psr = PSR::new(val);
        assert_eq!(psr.value(), val);
    }

    #[test]
    fn test_cpsr_mode_mask() {
        let psr = PSR::new(CPSR_MODE_MASK);
        assert!(psr.t()); // bit 5
        assert!(psr.e()); // bit 9
                          // IT bits should be set
        assert_ne!(psr.it(), 0);
    }
}
