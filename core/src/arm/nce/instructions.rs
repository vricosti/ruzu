// SPDX-FileCopyrightText: Copyright Skyline Team and Contributors
// SPDX-License-Identifier: MPL-2.0

//! Port of zuyu/src/core/arm/nce/instructions.h
//! ARM64 instruction encoding helpers for NCE patching.

/// System register identifiers used in MRS/MSR instructions.
///
/// Corresponds to upstream `Core::NCE::SystemRegister`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SystemRegister {
    TpidrEl0 = 0x5E82,
    TpidrroEl0 = 0x5E83,
    CntfrqEl0 = 0x5F00,
    CntpctEl0 = 0x5F01,
}

// ---------------------------------------------------------------------------
// Bitfield extraction helpers
// ---------------------------------------------------------------------------

/// Extract bits [lo..lo+width) from a u32 value.
const fn extract_bits(value: u32, lo: u32, width: u32) -> u32 {
    (value >> lo) & ((1u32 << width) - 1)
}

/// Format (set) bits [lo..lo+width) in a u32 value.
const fn format_bits(value: u32, lo: u32, _width: u32) -> u32 {
    value << lo
}

// ---------------------------------------------------------------------------
// SVC instruction
// ---------------------------------------------------------------------------

/// SVC (Supervisor Call) instruction decoder.
///
/// Corresponds to upstream `Core::NCE::SVC`.
/// Layout: sig0[0:5] | value[5:21] | sig1[21:32]
#[derive(Debug, Clone, Copy)]
pub struct Svc {
    pub raw: u32,
}

impl Svc {
    pub const fn new(raw: u32) -> Self {
        Self { raw }
    }

    pub const fn get_sig0(&self) -> u32 {
        extract_bits(self.raw, 0, 5)
    }

    pub const fn get_value(&self) -> u32 {
        extract_bits(self.raw, 5, 16)
    }

    pub const fn get_sig1(&self) -> u32 {
        extract_bits(self.raw, 21, 11)
    }

    pub const fn verify(&self) -> bool {
        self.get_sig0() == 0x1 && self.get_sig1() == 0x6A0
    }
}

#[test]
fn test_svc() {
    let svc = Svc::new(0xD40000C1);
    assert!(svc.verify());
    assert_eq!(svc.get_value(), 0x6);
}

// ---------------------------------------------------------------------------
// MRS instruction
// ---------------------------------------------------------------------------

/// MRS (Move System Register to general-purpose register) instruction decoder.
///
/// Corresponds to upstream `Core::NCE::MRS`.
/// Layout: rt[0:5] | system_reg[5:20] | sig[20:32]
#[derive(Debug, Clone, Copy)]
pub struct Mrs {
    pub raw: u32,
}

impl Mrs {
    pub const fn new(raw: u32) -> Self {
        Self { raw }
    }

    pub const fn get_rt(&self) -> u32 {
        extract_bits(self.raw, 0, 5)
    }

    pub const fn get_system_reg(&self) -> u32 {
        extract_bits(self.raw, 5, 15)
    }

    pub const fn get_sig(&self) -> u32 {
        extract_bits(self.raw, 20, 12)
    }

    pub const fn verify(&self) -> bool {
        self.get_sig() == 0xD53
    }
}

#[test]
fn test_mrs() {
    let mrs = Mrs::new(0xD53BE020);
    assert!(mrs.verify());
    assert_eq!(mrs.get_system_reg(), SystemRegister::CntpctEl0 as u32);
    assert_eq!(mrs.get_rt(), 0x0);
}

// ---------------------------------------------------------------------------
// MSR instruction
// ---------------------------------------------------------------------------

/// MSR (Move general-purpose register to System Register) instruction decoder.
///
/// Corresponds to upstream `Core::NCE::MSR`.
/// Layout: rt[0:5] | system_reg[5:20] | sig[20:32]
#[derive(Debug, Clone, Copy)]
pub struct Msr {
    pub raw: u32,
}

impl Msr {
    pub const fn new(raw: u32) -> Self {
        Self { raw }
    }

    pub const fn get_rt(&self) -> u32 {
        extract_bits(self.raw, 0, 5)
    }

    pub const fn get_system_reg(&self) -> u32 {
        extract_bits(self.raw, 5, 15)
    }

    pub const fn get_sig(&self) -> u32 {
        extract_bits(self.raw, 20, 12)
    }

    pub const fn verify(&self) -> bool {
        self.get_sig() == 0xD51
    }
}

#[test]
fn test_msr() {
    let msr = Msr::new(0xD51BD040);
    assert!(msr.verify());
    assert_eq!(msr.get_system_reg(), SystemRegister::TpidrEl0 as u32);
    assert_eq!(msr.get_rt(), 0x0);
}

// ---------------------------------------------------------------------------
// Exclusive load/store instruction
// ---------------------------------------------------------------------------

/// Exclusive load/store instruction decoder.
///
/// Corresponds to upstream `Core::NCE::Exclusive`.
/// Layout: rt[0:5] | rn[5:10] | rt2[10:15] | o0[15:16] | rs[16:21] | l[21:23] | sig[23:30] | size[30:32]
#[derive(Debug, Clone, Copy)]
pub struct Exclusive {
    pub raw: u32,
}

impl Exclusive {
    pub const fn new(raw: u32) -> Self {
        Self { raw }
    }

    pub const fn get_sig(&self) -> u32 {
        extract_bits(self.raw, 23, 7)
    }

    pub const fn verify(&self) -> bool {
        self.get_sig() == 0x10
    }

    /// Convert to ordered variant by setting the o0 bit.
    pub const fn as_ordered(&self) -> u32 {
        self.raw | format_bits(1, 15, 1)
    }
}

#[test]
fn test_exclusive() {
    assert!(Exclusive::new(0xC85FFC00).verify());
    assert_eq!(Exclusive::new(0xC85FFC00).as_ordered(), 0xC85FFC00);
    assert_eq!(Exclusive::new(0xC85F7C00).as_ordered(), 0xC85FFC00);
    assert_eq!(Exclusive::new(0xC8200440).as_ordered(), 0xC8208440);
}
