/// Floating-Point Status and Control Register (FPSCR) wrapper.
///
/// Layout:
///   [31] N   - Negative condition flag
///   [30] Z   - Zero condition flag
///   [29] C   - Carry condition flag
///   [28] V   - Overflow condition flag
///   [27] QC  - Cumulative saturation
///   [26] AHP - Alternate half-precision
///   [25] DN  - Default NaN
///   [24] FTZ - Flush to zero
///   [23:22] RMode - Rounding mode
///   [21:20] Stride
///   [18:16] Len
///   [15] IDE - Input denormal exception enable
///   [12] IXE - Inexact exception enable
///   [11] UFE - Underflow exception enable
///   [10] OFE - Overflow exception enable
///   [9]  DZE - Division by zero exception enable
///   [8]  IOE - Invalid operation exception enable
///   [7]  IDC - Input denormal cumulative
///   [4]  IXC - Inexact cumulative
///   [3]  UFC - Underflow cumulative
///   [2]  OFC - Overflow cumulative
///   [1]  DZC - Division by zero cumulative
///   [0]  IOC - Invalid operation cumulative
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FPSCR(pub u32);

impl FPSCR {
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

    pub fn nzcv(self) -> u32 {
        self.0 & 0xF000_0000
    }

    pub fn set_nzcv(&mut self, nzcv: u32) {
        self.0 = (self.0 & !0xF000_0000) | (nzcv & 0xF000_0000);
    }

    // --- Control bits ---

    pub fn qc(self) -> bool {
        self.0 & (1 << 27) != 0
    }
    pub fn ahp(self) -> bool {
        self.0 & (1 << 26) != 0
    }
    pub fn dn(self) -> bool {
        self.0 & (1 << 25) != 0
    }
    pub fn ftz(self) -> bool {
        self.0 & (1 << 24) != 0
    }

    /// Rounding mode: 0=RN, 1=RP, 2=RM, 3=RZ
    pub fn rmode(self) -> u32 {
        (self.0 >> 22) & 0x3
    }

    /// Indicates the stride of a vector.
    /// Upstream owner: frontend/A32/FPSCR.h `FPSCR::Stride()`.
    pub fn stride(self) -> Option<usize> {
        match (self.0 >> 20) & 0x3 {
            0b00 => Some(1),
            0b11 => Some(2),
            _ => None,
        }
    }

    /// Indicates the length of a vector.
    /// Upstream owner: frontend/A32/FPSCR.h `FPSCR::Len()`.
    pub fn len(self) -> usize {
        (((self.0 >> 16) & 0x7) + 1) as usize
    }

    // --- Exception enables ---

    pub fn ide(self) -> bool {
        self.0 & (1 << 15) != 0
    }
    pub fn ixe(self) -> bool {
        self.0 & (1 << 12) != 0
    }
    pub fn ufe(self) -> bool {
        self.0 & (1 << 11) != 0
    }
    pub fn ofe(self) -> bool {
        self.0 & (1 << 10) != 0
    }
    pub fn dze(self) -> bool {
        self.0 & (1 << 9) != 0
    }
    pub fn ioe(self) -> bool {
        self.0 & (1 << 8) != 0
    }

    // --- Cumulative exception flags ---

    pub fn idc(self) -> bool {
        self.0 & (1 << 7) != 0
    }
    pub fn ixc(self) -> bool {
        self.0 & (1 << 4) != 0
    }
    pub fn ufc(self) -> bool {
        self.0 & (1 << 3) != 0
    }
    pub fn ofc(self) -> bool {
        self.0 & (1 << 2) != 0
    }
    pub fn dzc(self) -> bool {
        self.0 & (1 << 1) != 0
    }
    pub fn ioc(self) -> bool {
        self.0 & 1 != 0
    }
}

impl Default for FPSCR {
    fn default() -> Self {
        Self(0)
    }
}

/// Mask for FPSCR bits that affect code translation (rounding mode, DN, FTZ, AHP, Len, Stride).
pub const FPSCR_MODE_MASK: u32 = 0x07F7_0000;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fpscr_nzcv() {
        let mut fpscr = FPSCR::new(0);
        fpscr.set_nzcv(0xF000_0000);
        assert!(fpscr.n());
        assert!(fpscr.z());
        assert!(fpscr.c());
        assert!(fpscr.v());
        assert_eq!(fpscr.nzcv(), 0xF000_0000);
    }

    #[test]
    fn test_fpscr_rmode() {
        let fpscr = FPSCR::new(0x00C0_0000); // RMode = 3
        assert_eq!(fpscr.rmode(), 3);
    }

    #[test]
    fn test_fpscr_scalar_defaults_match_upstream() {
        let fpscr = FPSCR::new(0);
        assert_eq!(fpscr.stride(), Some(1));
        assert_eq!(fpscr.len(), 1);
    }

    #[test]
    fn test_fpscr_stride_encoding_matches_upstream() {
        assert_eq!(FPSCR::new(0x0000_0000).stride(), Some(1));
        assert_eq!(FPSCR::new(0x0030_0000).stride(), Some(2));
        assert_eq!(FPSCR::new(0x0010_0000).stride(), None);
        assert_eq!(FPSCR::new(0x0020_0000).stride(), None);
    }

    #[test]
    fn test_fpscr_mode_mask() {
        let fpscr = FPSCR::new(FPSCR_MODE_MASK);
        assert!(fpscr.ahp());
        assert!(fpscr.dn());
        assert!(fpscr.ftz());
        assert_eq!(fpscr.rmode(), 3);
    }
}
