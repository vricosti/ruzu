/// Shift left with the signed shift semantics used by upstream `Safe`.
pub fn arithmetic_shift_left_u64(value: u64, shift_amount: i32) -> u64 {
    if shift_amount >= u64::BITS as i32 {
        return 0;
    }
    if shift_amount < 0 {
        return arithmetic_shift_right_u64(value, -shift_amount);
    }
    value << shift_amount
}

fn arithmetic_shift_right_u64(value: u64, shift_amount: i32) -> u64 {
    if shift_amount >= u64::BITS as i32 {
        return if value >> 63 != 0 { u64::MAX } else { 0 };
    }
    if shift_amount < 0 {
        return arithmetic_shift_left_u64(value, -shift_amount);
    }
    ((value as i64) >> shift_amount) as u64
}

pub fn logical_shift_right_u64(value: u64, shift_amount: i32) -> u64 {
    if shift_amount >= u64::BITS as i32 {
        return 0;
    }
    if shift_amount < 0 {
        let left = -shift_amount;
        return if left >= u64::BITS as i32 {
            0
        } else {
            value << left
        };
    }
    value >> shift_amount
}

pub fn negate_u64(value: u64) -> u64 {
    value.wrapping_neg()
}
