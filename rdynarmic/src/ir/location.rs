use crate::frontend::a32::fpscr::{FPSCR, FPSCR_MODE_MASK};
use crate::frontend::a32::it_state::ITState;
use crate::frontend::a32::psr::{CPSR_MODE_MASK, PSR};
use std::fmt;

/// Generic location descriptor — a unique u64 hash identifying a block's location.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LocationDescriptor(pub u64);

impl LocationDescriptor {
    pub fn new(value: u64) -> Self {
        Self(value)
    }

    pub fn value(self) -> u64 {
        self.0
    }
}

impl fmt::Display for LocationDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "loc:{:#018x}", self.0)
    }
}

/// A64-specific location descriptor.
/// Encodes: PC (56 bits), FPCR (bits 37..=56), single_stepping (bit 57).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct A64LocationDescriptor {
    pc: u64,
    fpcr: u32,
    single_stepping: bool,
}

impl A64LocationDescriptor {
    pub const PC_BIT_COUNT: u32 = 56;
    pub const PC_MASK: u64 = (1u64 << Self::PC_BIT_COUNT) - 1;
    pub const FPCR_MASK: u32 = 0x07C8_0000;
    pub const FPCR_SHIFT: u32 = 37;
    pub const SINGLE_STEPPING_BIT: u32 = 57;

    pub fn new(pc: u64, fpcr: u32, single_stepping: bool) -> Self {
        Self {
            pc: pc & Self::PC_MASK,
            fpcr: fpcr & Self::FPCR_MASK,
            single_stepping,
        }
    }

    pub fn from_location(loc: LocationDescriptor) -> Self {
        let val = loc.value();
        Self {
            pc: val & Self::PC_MASK,
            fpcr: ((val >> Self::FPCR_SHIFT) as u32) & Self::FPCR_MASK,
            single_stepping: (val >> Self::SINGLE_STEPPING_BIT) & 1 != 0,
        }
    }

    /// Get PC, sign-extended from 56 bits.
    pub fn pc(self) -> u64 {
        let shift = 64 - Self::PC_BIT_COUNT;
        ((self.pc as i64) << shift >> shift) as u64
    }

    pub fn fpcr(self) -> u32 {
        self.fpcr
    }

    pub fn single_stepping(self) -> bool {
        self.single_stepping
    }

    pub fn set_pc(self, new_pc: u64) -> Self {
        Self::new(new_pc, self.fpcr, self.single_stepping)
    }

    pub fn advance_pc(self, amount: i64) -> Self {
        Self::new(
            self.pc.wrapping_add(amount as u64),
            self.fpcr,
            self.single_stepping,
        )
    }

    pub fn set_single_stepping(self, ss: bool) -> Self {
        Self::new(self.pc, self.fpcr, ss)
    }

    /// Compute the unique hash for this location, matching EmitTerminalPopRSBHint layout.
    pub fn unique_hash(self) -> u64 {
        let fpcr_u64 = (self.fpcr as u64) << Self::FPCR_SHIFT;
        let ss_u64 = (self.single_stepping as u64) << Self::SINGLE_STEPPING_BIT;
        self.pc | fpcr_u64 | ss_u64
    }

    pub fn to_location(self) -> LocationDescriptor {
        LocationDescriptor(self.unique_hash())
    }
}

impl From<A64LocationDescriptor> for LocationDescriptor {
    fn from(a64: A64LocationDescriptor) -> Self {
        a64.to_location()
    }
}

impl From<LocationDescriptor> for A64LocationDescriptor {
    fn from(loc: LocationDescriptor) -> Self {
        A64LocationDescriptor::from_location(loc)
    }
}

impl fmt::Display for A64LocationDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "(pc:{:#x} fpcr:{:#x} ss:{})",
            self.pc(),
            self.fpcr,
            self.single_stepping
        )
    }
}

/// A32-specific location descriptor.
/// Encodes: PC (32 bits), CPSR (masked), FPSCR (masked), single_stepping.
///
/// UniqueHash layout (64 bits):
///   Lower 32 bits: PC
///   Upper 32 bits: FPSCR mode | T flag | E flag | single_step | IT state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct A32LocationDescriptor {
    arm_pc: u32,
    cpsr: PSR,
    fpscr: FPSCR,
    single_stepping: bool,
}

impl A32LocationDescriptor {
    pub fn new(arm_pc: u32, cpsr: PSR, fpscr: FPSCR, single_stepping: bool) -> Self {
        Self {
            arm_pc,
            cpsr: PSR::new(cpsr.value() & CPSR_MODE_MASK),
            fpscr: FPSCR::new(fpscr.value() & FPSCR_MODE_MASK),
            single_stepping,
        }
    }

    pub fn at(pc: u32) -> Self {
        Self::new(pc, PSR::default(), FPSCR::default(), false)
    }

    pub fn pc(self) -> u32 {
        self.arm_pc
    }

    pub fn cpsr(self) -> PSR {
        self.cpsr
    }

    pub fn fpscr(self) -> FPSCR {
        self.fpscr
    }

    pub fn single_stepping(self) -> bool {
        self.single_stepping
    }

    pub fn t_flag(self) -> bool {
        self.cpsr.t()
    }

    pub fn e_flag(self) -> bool {
        self.cpsr.e()
    }

    pub fn it(self) -> ITState {
        ITState::new(self.cpsr.it())
    }

    pub fn set_pc(self, pc: u32) -> Self {
        Self { arm_pc: pc, ..self }
    }

    pub fn advance_pc(self, amount: i32) -> Self {
        Self {
            arm_pc: self.arm_pc.wrapping_add(amount as u32),
            ..self
        }
    }

    pub fn set_t_flag(self, t: bool) -> Self {
        let mut cpsr = self.cpsr;
        cpsr.set_t(t);
        Self {
            cpsr: PSR::new(cpsr.value() & CPSR_MODE_MASK),
            ..self
        }
    }

    pub fn set_it(self, it: ITState) -> Self {
        let mut cpsr = self.cpsr;
        cpsr.set_it(it.value());
        Self {
            cpsr: PSR::new(cpsr.value() & CPSR_MODE_MASK),
            ..self
        }
    }

    pub fn advance_it(self) -> Self {
        let mut it = self.it();
        it.advance();
        self.set_it(it)
    }

    pub fn set_single_stepping(self, ss: bool) -> Self {
        Self {
            single_stepping: ss,
            ..self
        }
    }

    /// Compute the upper 32 bits of the unique hash.
    /// This encodes the state bits that affect translation.
    pub fn upper_location_descriptor(self) -> u32 {
        let mut upper = self.fpscr.value() & FPSCR_MODE_MASK;
        if self.t_flag() {
            upper |= 1 << 0;
        }
        if self.e_flag() {
            upper |= 1 << 1;
        }
        if self.single_stepping {
            upper |= 1 << 2;
        }
        upper |= (self.cpsr.it() as u32) << 8;
        upper
    }

    /// Compute the unique hash for block cache keying.
    pub fn unique_hash(self) -> u64 {
        let lower = self.arm_pc as u64;
        let upper = (self.upper_location_descriptor() as u64) << 32;
        lower | upper
    }

    pub fn to_location(self) -> LocationDescriptor {
        LocationDescriptor(self.unique_hash())
    }

    pub fn from_location(loc: LocationDescriptor) -> Self {
        let value = loc.value();
        let pc = value as u32;
        let upper = (value >> 32) as u32;
        let mut cpsr = PSR::default();
        cpsr.set_t((upper & (1 << 0)) != 0);
        cpsr.set_e((upper & (1 << 1)) != 0);
        cpsr.set_it(((upper >> 8) & 0xff) as u8);
        let fpscr = FPSCR::new(upper & FPSCR_MODE_MASK);
        Self::new(pc, cpsr, fpscr, (upper & (1 << 2)) != 0)
    }
}

impl From<A32LocationDescriptor> for LocationDescriptor {
    fn from(a32: A32LocationDescriptor) -> Self {
        a32.to_location()
    }
}

impl From<LocationDescriptor> for A32LocationDescriptor {
    fn from(loc: LocationDescriptor) -> Self {
        A32LocationDescriptor::from_location(loc)
    }
}

impl fmt::Display for A32LocationDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "(pc:{:#x} cpsr:{:#x} fpscr:{:#x} T:{} ss:{})",
            self.arm_pc,
            self.cpsr.value(),
            self.fpscr.value(),
            self.t_flag() as u8,
            self.single_stepping
        )
    }
}

#[cfg(test)]
mod a32_tests {
    use super::*;

    #[test]
    fn test_a32_location_basic() {
        let loc = A32LocationDescriptor::at(0x0800_1000);
        assert_eq!(loc.pc(), 0x0800_1000);
        assert!(!loc.t_flag());
        assert!(!loc.single_stepping());
    }

    #[test]
    fn test_a32_location_thumb() {
        let mut cpsr = PSR::default();
        cpsr.set_t(true);
        let loc = A32LocationDescriptor::new(0x1000, cpsr, FPSCR::default(), false);
        assert!(loc.t_flag());
        // T flag should be encoded in the hash
        let hash = loc.unique_hash();
        assert_ne!(hash, 0x1000); // upper bits should be set
    }

    #[test]
    fn test_a32_location_hash_differs_by_state() {
        let loc1 = A32LocationDescriptor::at(0x1000);
        let loc2 = loc1.set_t_flag(true);
        assert_ne!(loc1.unique_hash(), loc2.unique_hash());
        assert_eq!(loc1.pc(), loc2.pc()); // same PC
    }

    #[test]
    fn test_a32_advance_pc() {
        let loc = A32LocationDescriptor::at(0x1000);
        let next = loc.advance_pc(4);
        assert_eq!(next.pc(), 0x1004);
        let thumb = loc.set_t_flag(true).advance_pc(2);
        assert_eq!(thumb.pc(), 0x1002);
    }

    #[test]
    fn test_a32_it_state_in_location() {
        let mut cpsr = PSR::default();
        cpsr.set_t(true);
        cpsr.set_it(0xEF); // ITTT AL
        let loc = A32LocationDescriptor::new(0x2000, cpsr, FPSCR::default(), false);
        assert!(loc.it().is_in_it_block());
    }
}
