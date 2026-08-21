use crate::common::common::{CpuAddr, MAX_CHANNELS};

use super::UpsamplerState;

#[derive(Debug, Clone, Default)]
pub struct UpsamplerInfo {
    pub states: [UpsamplerState; MAX_CHANNELS],
    pub manager_ptr: CpuAddr,
    pub samples_pos: CpuAddr,
    pub sample_count: u32,
    pub input_count: u32,
    pub enabled: bool,
    pub inputs: [i16; MAX_CHANNELS],
}
