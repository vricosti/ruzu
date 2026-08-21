//! NZCV flag conversion between ARM64 and x86-64 RFLAGS.
//!
//! ARM64 NZCV: bit 31=N, 30=Z, 29=C, 28=V
//! x86-64 RFLAGS: bit 15=SF(N), 14=ZF(Z), 8=CF(C), 0=OF(V)
//!
//! Conversion uses magic multiplier technique for branchless operation.

/// ARM64 NZCV mask (bits 31:28)
pub const ARM_MASK: u32 = 0xF000_0000;

/// x86-64 RFLAGS mask for N/Z/C/V bits
pub const X64_MASK: u32 = 0xC101;

/// x86-64 flag bit positions
pub const X64_N_FLAG_BIT: u32 = 15; // SF
pub const X64_Z_FLAG_BIT: u32 = 14; // ZF
pub const X64_C_FLAG_BIT: u32 = 8; // CF
pub const X64_V_FLAG_BIT: u32 = 0; // OF

/// x86-64 individual flag masks
pub const X64_N_FLAG_MASK: u32 = 1 << X64_N_FLAG_BIT;
pub const X64_Z_FLAG_MASK: u32 = 1 << X64_Z_FLAG_BIT;
pub const X64_C_FLAG_MASK: u32 = 1 << X64_C_FLAG_BIT;
pub const X64_V_FLAG_MASK: u32 = 1 << X64_V_FLAG_BIT;

/// Magic multiplier: ARM NZCV (bits 3:0 after shift) → x64 flag positions
pub const TO_X64_MULTIPLIER: u32 = 0x1081;

/// Magic multiplier: x64 flags → ARM NZCV (bits 31:28)
pub const FROM_X64_MULTIPLIER: u32 = 0x1021_0000;

/// Convert ARM64 NZCV (bits 31:28) to x86-64 RFLAGS format.
///
/// Uses: `((nzcv >> 28) * 0x1081) & 0xC101`
#[inline]
pub fn to_x64(nzcv: u32) -> u32 {
    ((nzcv >> 28).wrapping_mul(TO_X64_MULTIPLIER)) & X64_MASK
}

/// Convert x86-64 RFLAGS to ARM64 NZCV (bits 31:28).
///
/// Uses: `((x64_flags & 0xC101) * 0x10210000) & 0xF0000000`
#[inline]
pub fn from_x64(x64_flags: u32) -> u32 {
    ((x64_flags & X64_MASK).wrapping_mul(FROM_X64_MULTIPLIER)) & ARM_MASK
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nzcv_round_trip() {
        // Test all 16 possible NZCV combinations
        for nzcv_bits in 0u32..16 {
            let arm_nzcv = nzcv_bits << 28;
            let x64 = to_x64(arm_nzcv);
            let back = from_x64(x64);
            assert_eq!(back, arm_nzcv, "Round-trip failed for NZCV={:#x}", arm_nzcv);
        }
    }

    #[test]
    fn test_nzcv_individual_flags() {
        // N flag only
        let n_only = 0x8000_0000u32; // bit 31
        let x64_n = to_x64(n_only);
        assert_ne!(x64_n & (1 << X64_N_FLAG_BIT), 0, "N flag should set SF");
        assert_eq!(x64_n & (1 << X64_Z_FLAG_BIT), 0);
        assert_eq!(x64_n & (1 << X64_C_FLAG_BIT), 0);
        assert_eq!(x64_n & (1 << X64_V_FLAG_BIT), 0);

        // Z flag only
        let z_only = 0x4000_0000u32; // bit 30
        let x64_z = to_x64(z_only);
        assert_ne!(x64_z & (1 << X64_Z_FLAG_BIT), 0, "Z flag should set ZF");

        // C flag only
        let c_only = 0x2000_0000u32; // bit 29
        let x64_c = to_x64(c_only);
        assert_ne!(x64_c & (1 << X64_C_FLAG_BIT), 0, "C flag should set CF");

        // V flag only
        let v_only = 0x1000_0000u32; // bit 28
        let x64_v = to_x64(v_only);
        assert_ne!(x64_v & (1 << X64_V_FLAG_BIT), 0, "V flag should set OF");
    }

    #[test]
    fn test_nzcv_all_set() {
        let all = 0xF000_0000u32;
        let x64 = to_x64(all);
        assert_eq!(x64, X64_MASK); // all mapped flags should be set
        assert_eq!(from_x64(x64), all);
    }

    #[test]
    fn test_nzcv_none_set() {
        assert_eq!(to_x64(0), 0);
        assert_eq!(from_x64(0), 0);
    }
}
