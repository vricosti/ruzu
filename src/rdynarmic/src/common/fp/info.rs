pub trait FloatFormat: Copy {
    const TOTAL_WIDTH: usize;
    const EXPONENT_WIDTH: usize;
    const EXPLICIT_MANTISSA_WIDTH: usize;
    const IMPLICIT_LEADING_BIT: u64;
    const SIGN_MASK: u64;
    const EXPONENT_MASK: u64;
    const MANTISSA_MASK: u64;
    const MANTISSA_MSB: u64;
    const EXPONENT_MIN: i32;
    const EXPONENT_MAX: i32;
    const EXPONENT_BIAS: i32;

    fn to_bits(self) -> u64;
    fn from_bits(value: u64) -> Self;

    fn zero(sign: bool) -> Self {
        Self::from_bits(if sign { Self::SIGN_MASK } else { 0 })
    }

    fn infinity(sign: bool) -> Self {
        Self::from_bits(Self::EXPONENT_MASK | Self::zero(sign).to_bits())
    }

    fn max_normal(sign: bool) -> Self {
        Self::from_bits((Self::EXPONENT_MASK - 1) | Self::zero(sign).to_bits())
    }

    fn default_nan() -> Self {
        Self::from_bits(Self::EXPONENT_MASK | (1u64 << (Self::EXPLICIT_MANTISSA_WIDTH - 1)))
    }
}

macro_rules! impl_float_format {
    ($type:ty, $total:expr, $exponent:expr, $mantissa:expr, $sign:expr,
     $exp_mask:expr, $mantissa_mask:expr, $mantissa_msb:expr,
     $min:expr, $max:expr, $bias:expr) => {
        impl FloatFormat for $type {
            const TOTAL_WIDTH: usize = $total;
            const EXPONENT_WIDTH: usize = $exponent;
            const EXPLICIT_MANTISSA_WIDTH: usize = $mantissa;
            const IMPLICIT_LEADING_BIT: u64 = 1u64 << $mantissa;
            const SIGN_MASK: u64 = $sign;
            const EXPONENT_MASK: u64 = $exp_mask;
            const MANTISSA_MASK: u64 = $mantissa_mask;
            const MANTISSA_MSB: u64 = $mantissa_msb;
            const EXPONENT_MIN: i32 = $min;
            const EXPONENT_MAX: i32 = $max;
            const EXPONENT_BIAS: i32 = $bias;

            fn to_bits(self) -> u64 {
                self as u64
            }

            fn from_bits(value: u64) -> Self {
                value as Self
            }
        }
    };
}

impl_float_format!(u16, 16, 5, 10, 0x8000, 0x7c00, 0x03ff, 0x0200, -14, 15, 15);
impl_float_format!(
    u32,
    32,
    8,
    23,
    0x8000_0000,
    0x7f80_0000,
    0x007f_ffff,
    0x0040_0000,
    -126,
    127,
    127
);
impl_float_format!(
    u64,
    64,
    11,
    52,
    0x8000_0000_0000_0000,
    0x7ff0_0000_0000_0000,
    0x000f_ffff_ffff_ffff,
    0x0008_0000_0000_0000,
    -1022,
    1023,
    1023
);
