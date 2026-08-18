#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ResidualError {
    Zero,
    LessThanHalf,
    Half,
    GreaterThanHalf,
}

pub fn residual_error_on_right_shift(mantissa: u64, shift_amount: i32) -> ResidualError {
    if shift_amount <= 0 || mantissa == 0 {
        return ResidualError::Zero;
    }
    if shift_amount > u64::BITS as i32 {
        return if mantissa >> 63 != 0 {
            ResidualError::GreaterThanHalf
        } else {
            ResidualError::LessThanHalf
        };
    }

    let half_bit_position = shift_amount as u32 - 1;
    let half = 1u64 << half_bit_position;
    let error_mask = if shift_amount == u64::BITS as i32 {
        u64::MAX
    } else {
        (1u64 << shift_amount) - 1
    };
    match mantissa & error_mask {
        0 => ResidualError::Zero,
        error if error < half => ResidualError::LessThanHalf,
        error if error == half => ResidualError::Half,
        _ => ResidualError::GreaterThanHalf,
    }
}
