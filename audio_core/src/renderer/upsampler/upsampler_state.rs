use common::fixed_point::FixedPoint;

#[derive(Debug, Clone, Copy, Default)]
pub struct UpsamplerState {
    pub ratio: FixedPoint<16, 16>,
    pub history: [FixedPoint<24, 8>; Self::HISTORY_SIZE],
    pub window_size: u16,
    pub history_output_index: u16,
    pub history_input_index: u16,
    pub history_start_index: u16,
    pub history_end_index: u16,
    pub initialized: bool,
    pub sample_index: u8,
}

impl UpsamplerState {
    pub const HISTORY_SIZE: usize = 20;
}
