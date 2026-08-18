use super::unpacked::{FpUnpacked, NORMALIZED_POINT_POSITION};

const PRODUCT_POINT_POSITION: usize = NORMALIZED_POINT_POSITION * 2;

fn sticky_logical_shift_right(value: u128, amount: i32) -> u128 {
    if amount < 0 {
        let shift = (-amount) as u32;
        return if shift >= 128 { 0 } else { value << shift };
    }
    if amount == 0 {
        return value;
    }
    if amount >= 128 {
        return u128::from(value != 0);
    }

    let shift = amount as u32;
    let discarded_mask = (1u128 << shift) - 1;
    (value >> shift) | u128::from(value & discarded_mask != 0)
}

fn reduce_mantissa(sign: bool, exponent: i32, mantissa: u128) -> FpUnpacked {
    const POINT_POSITION_CORRECTION: i32 =
        NORMALIZED_POINT_POSITION as i32 - (PRODUCT_POINT_POSITION as i32 - 64);
    FpUnpacked {
        sign,
        exponent: exponent + POINT_POSITION_CORRECTION,
        mantissa: (mantissa >> 64) as u64 | u64::from(mantissa as u64 != 0),
    }
}

/// Fused multiply-add on normalized unpacked values.
///
/// This is the direct counterpart of upstream `common/fp/fused.cpp`.
pub fn fused_mul_add(addend: FpUnpacked, op1: FpUnpacked, op2: FpUnpacked) -> FpUnpacked {
    let product_sign = op1.sign != op2.sign;
    let mut product_exponent = op1.exponent + op2.exponent;
    let mut product_value = u128::from(op1.mantissa) * u128::from(op2.mantissa);
    if product_value & (1u128 << (PRODUCT_POINT_POSITION + 1)) != 0 {
        product_value >>= 1;
        product_exponent += 1;
    }

    if product_value == 0 {
        return addend;
    }
    if addend.mantissa == 0 {
        return reduce_mantissa(product_sign, product_exponent, product_value);
    }

    let exp_diff = product_exponent - addend.exponent;
    if product_sign == addend.sign {
        if exp_diff <= 0 {
            let result = addend.mantissa.wrapping_add(sticky_logical_shift_right(
                product_value,
                NORMALIZED_POINT_POSITION as i32 - exp_diff,
            ) as u64);
            return FpUnpacked {
                sign: addend.sign,
                exponent: addend.exponent,
                mantissa: result,
            };
        }

        let result = product_value
            + sticky_logical_shift_right(
                u128::from(addend.mantissa),
                exp_diff - NORMALIZED_POINT_POSITION as i32,
            );
        return reduce_mantissa(product_sign, product_exponent, result);
    }

    let addend_long = u128::from(addend.mantissa) << NORMALIZED_POINT_POSITION;
    let (result_sign, mut result_exponent, mut result) =
        if exp_diff == 0 && product_value > addend_long {
            (product_sign, product_exponent, product_value - addend_long)
        } else if exp_diff <= 0 {
            (
                !product_sign,
                addend.exponent,
                addend_long - sticky_logical_shift_right(product_value, -exp_diff),
            )
        } else {
            (
                product_sign,
                product_exponent,
                product_value - sticky_logical_shift_right(addend_long, exp_diff),
            )
        };

    if result >> 64 == 0 {
        return FpUnpacked {
            sign: result_sign,
            exponent: result_exponent,
            mantissa: result as u64,
        };
    }

    let highest_upper_bit = 127 - result.leading_zeros() as i32 - 64;
    let required_shift = NORMALIZED_POINT_POSITION as i32 - highest_upper_bit;
    result <<= required_shift as u32;
    result_exponent -= required_shift;
    reduce_mantissa(result_sign, result_exponent, result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::fp::unpacked::to_normalized;

    #[test]
    fn cancellation_preserves_zero_mantissa() {
        let one = to_normalized(false, 0, 1);
        let minus_one = to_normalized(true, 0, 1);
        let result = fused_mul_add(one, minus_one, one);
        assert_eq!(result.mantissa, 0);
    }
}
