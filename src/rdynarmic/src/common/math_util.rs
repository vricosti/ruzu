const fn make_recip_lut() -> [u8; 256] {
    let mut result = [0; 256];
    let mut i = 0;
    while i < result.len() {
        let a = (i as u64 + 256) * 2 + 1;
        let b = (1u64 << 19) / a;
        result[i] = ((b + 1) / 2) as u8;
        i += 1;
    }
    result
}

const fn make_recip_sqrt_lut() -> [u8; 512] {
    let mut result = [0; 512];
    let mut i = 128;
    while i < result.len() {
        let a = if i < 256 {
            i as u64 * 2 + 1
        } else {
            ((i as u64) | 1) * 2
        };

        let mut b = 512u64;
        while a * (b + 1) * (b + 1) < (1u64 << 28) {
            b += 1;
        }
        result[i] = ((b + 1) / 2) as u8;
        i += 1;
    }
    result
}

const RECIP_LUT: [u8; 256] = make_recip_lut();
const RECIP_SQRT_LUT: [u8; 512] = make_recip_sqrt_lut();

/// Input is a u1.8 fixed-point value in `[1.0, 2.0)`.
pub fn recip_estimate(a: u64) -> u8 {
    RECIP_LUT[(a - 256) as usize]
}

/// Input is a u0.9 fixed-point value in `[0.25, 1.0)`.
pub fn recip_sqrt_estimate(a: u64) -> u8 {
    RECIP_SQRT_LUT[(a & 0x1ff) as usize]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reciprocal_lut_matches_upstream_boundaries() {
        assert_eq!(recip_estimate(256), 0xff);
        assert_eq!(recip_estimate(384), 0x55);
        assert_eq!(recip_estimate(511), 0x00);
    }

    #[test]
    fn reciprocal_sqrt_lut_matches_upstream_boundaries() {
        assert_eq!(recip_sqrt_estimate(128), 0xff);
        assert_eq!(recip_sqrt_estimate(256), 0x69);
        assert_eq!(recip_sqrt_estimate(384), 0x27);
        assert_eq!(recip_sqrt_estimate(511), 0x00);
    }
}
