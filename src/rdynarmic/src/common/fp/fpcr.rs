use super::rounding_mode::RoundingMode;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Fpcr {
    value: u32,
}

impl Fpcr {
    const MASK: u32 = 0x07ff_9f00;

    pub const fn new(data: u32) -> Self {
        Self {
            value: data & Self::MASK,
        }
    }

    pub const fn ahp(self) -> bool {
        self.value & (1 << 26) != 0
    }

    pub fn set_ahp(&mut self, value: bool) {
        self.set_bit(26, value);
    }

    pub const fn dn(self) -> bool {
        self.value & (1 << 25) != 0
    }

    pub fn set_dn(&mut self, value: bool) {
        self.set_bit(25, value);
    }

    pub const fn fz(self) -> bool {
        self.value & (1 << 24) != 0
    }

    pub fn set_fz(&mut self, value: bool) {
        self.set_bit(24, value);
    }

    pub const fn rmode(self) -> RoundingMode {
        match (self.value >> 22) & 3 {
            0 => RoundingMode::ToNearestTieEven,
            1 => RoundingMode::TowardsPlusInfinity,
            2 => RoundingMode::TowardsMinusInfinity,
            3 => RoundingMode::TowardsZero,
            _ => unreachable!(),
        }
    }

    pub fn set_rmode(&mut self, value: RoundingMode) {
        let value = value as u32;
        assert!(value <= 3, "FPCR: invalid rounding mode");
        self.value = (self.value & !(3 << 22)) | (value << 22);
    }

    pub const fn stride(self) -> Option<usize> {
        match (self.value >> 20) & 3 {
            0 => Some(1),
            3 => Some(2),
            _ => None,
        }
    }

    pub fn set_stride(&mut self, value: usize) {
        assert!((1..=2).contains(&value), "FPCR: invalid stride");
        self.value = (self.value & !(3 << 20)) | (if value == 1 { 0 } else { 3 } << 20);
    }

    pub const fn fz16(self) -> bool {
        self.value & (1 << 19) != 0
    }

    pub fn set_fz16(&mut self, value: bool) {
        self.set_bit(19, value);
    }

    pub const fn len(self) -> usize {
        (((self.value >> 16) & 7) + 1) as usize
    }

    pub fn set_len(&mut self, value: usize) {
        assert!((1..=8).contains(&value), "FPCR: invalid len");
        self.value = (self.value & !(7 << 16)) | (((value - 1) as u32) << 16);
    }

    pub const fn ide(self) -> bool {
        self.value & (1 << 15) != 0
    }

    pub const fn ixe(self) -> bool {
        self.value & (1 << 12) != 0
    }

    pub const fn ufe(self) -> bool {
        self.value & (1 << 11) != 0
    }

    pub const fn ofe(self) -> bool {
        self.value & (1 << 10) != 0
    }

    pub const fn dze(self) -> bool {
        self.value & (1 << 9) != 0
    }

    pub const fn ioe(self) -> bool {
        self.value & (1 << 8) != 0
    }

    pub const fn value(self) -> u32 {
        self.value
    }

    pub fn asimd_standard_value(self) -> Self {
        let mut value = Self::default();
        value.set_ahp(self.ahp());
        value.set_fz16(self.fz16());
        value.set_fz(true);
        value.set_dn(true);
        value
    }

    fn set_bit(&mut self, bit: u32, value: bool) {
        self.value = (self.value & !(1 << bit)) | ((value as u32) << bit);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn standard_asimd_value_matches_upstream() {
        let fpcr = Fpcr::new((1 << 26) | (1 << 19) | (2 << 22));
        assert_eq!(
            fpcr.asimd_standard_value().value(),
            (1 << 26) | (1 << 25) | (1 << 24) | (1 << 19)
        );
    }
}
