use crate::common::common::{
    get_splitter_send_data_magic, MAX_BIQUAD_FILTERS, MAX_MIX_BUFFERS, UNUSED_MIX_ID,
};

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct BiquadFilterParameter2 {
    pub enabled: bool,
    pub reserved1: u8,
    pub reserved2: u8,
    pub reserved3: u8,
    pub numerator: [f32; 3],
    pub denominator: [f32; 2],
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct BiquadFilterParameterLegacy {
    pub enabled: bool,
    pub _padding: u8,
    pub b: [i16; 3],
    pub a: [i16; 2],
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct InParameter {
    pub magic: u32,
    pub id: i32,
    pub mix_volumes: [f32; MAX_MIX_BUFFERS as usize],
    pub mix_id: u32,
    pub in_use: bool,
    pub reset_prev_volume: bool,
    pub _padding: [u8; 2],
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct InParameterVersion2a {
    pub magic: u32,
    pub id: i32,
    pub mix_volumes: [f32; MAX_MIX_BUFFERS as usize],
    pub mix_id: u32,
    pub biquad_filters: [BiquadFilterParameterLegacy; MAX_BIQUAD_FILTERS as usize],
    pub in_use: bool,
    pub reset_prev_volume: bool,
    pub reserved: [u8; 10],
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct InParameterVersion2b {
    pub magic: u32,
    pub id: i32,
    pub mix_volumes: [f32; MAX_MIX_BUFFERS as usize],
    pub mix_id: u32,
    pub biquad_filters: [BiquadFilterParameter2; MAX_BIQUAD_FILTERS as usize],
    pub in_use: bool,
    pub reset_prev_volume: bool,
    pub reserved: [u8; 10],
}

impl Default for InParameterVersion2b {
    fn default() -> Self {
        Self {
            magic: 0,
            id: 0,
            mix_volumes: [0.0; MAX_MIX_BUFFERS as usize],
            mix_id: 0,
            biquad_filters: [BiquadFilterParameter2::default(); MAX_BIQUAD_FILTERS as usize],
            in_use: false,
            reset_prev_volume: false,
            reserved: [0; 10],
        }
    }
}

#[derive(Debug, Clone)]
pub struct SplitterDestinationData {
    id: i32,
    destination_id: i32,
    mix_volumes: [f32; MAX_MIX_BUFFERS as usize],
    prev_mix_volumes: [f32; MAX_MIX_BUFFERS as usize],
    biquad_filters: [BiquadFilterParameter2; MAX_BIQUAD_FILTERS as usize],
    next: Option<usize>,
    in_use: bool,
    need_update: bool,
}

impl SplitterDestinationData {
    pub fn new(id: i32) -> Self {
        Self {
            id,
            destination_id: UNUSED_MIX_ID,
            mix_volumes: [0.0; MAX_MIX_BUFFERS as usize],
            prev_mix_volumes: [0.0; MAX_MIX_BUFFERS as usize],
            biquad_filters: [BiquadFilterParameter2::default(); MAX_BIQUAD_FILTERS as usize],
            next: None,
            in_use: false,
            need_update: false,
        }
    }

    pub fn clear_mix_volume(&mut self) {
        self.mix_volumes.fill(0.0);
        self.prev_mix_volumes.fill(0.0);
    }

    pub fn get_id(&self) -> i32 {
        self.id
    }

    pub fn is_configured(&self) -> bool {
        self.in_use && self.destination_id != UNUSED_MIX_ID
    }

    pub fn get_mix_id(&self) -> i32 {
        self.destination_id
    }

    pub fn get_mix_volume(&self, index: u32) -> f32 {
        self.mix_volumes.get(index as usize).copied().unwrap_or(0.0)
    }

    pub fn get_mix_volume_slice(&self) -> &[f32] {
        &self.mix_volumes
    }

    pub fn get_mix_volume_prev(&self, index: u32) -> f32 {
        self.prev_mix_volumes
            .get(index as usize)
            .copied()
            .unwrap_or(0.0)
    }

    pub fn get_mix_volume_prev_slice(&self) -> &[f32] {
        &self.prev_mix_volumes
    }

    pub fn update(&mut self, params: &InParameter) {
        if params.id != self.id || params.magic != get_splitter_send_data_magic() {
            return;
        }

        self.destination_id = params.mix_id as i32;
        self.mix_volumes = params.mix_volumes;

        if !self.in_use && params.in_use {
            self.prev_mix_volumes = self.mix_volumes;
            self.need_update = false;
        }

        self.in_use = params.in_use;
    }

    pub fn mark_as_need_to_update_internal_state(&mut self) {
        self.need_update = true;
    }

    pub fn update_internal_state(&mut self) {
        if self.in_use && self.need_update {
            self.prev_mix_volumes = self.mix_volumes;
        }
        self.need_update = false;
    }

    pub fn get_biquad_filters(&self) -> &[BiquadFilterParameter2] {
        &self.biquad_filters
    }

    pub fn get_biquad_filters_mut(&mut self) -> &mut [BiquadFilterParameter2] {
        &mut self.biquad_filters
    }

    pub fn next(&self) -> Option<usize> {
        self.next
    }

    pub fn set_next(&mut self, next: Option<usize>) {
        self.next = next;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::{offset_of, size_of};

    #[test]
    fn splitter_destination_parameter_layouts_match_upstream() {
        assert_eq!(size_of::<BiquadFilterParameter2>(), 0x18);
        assert_eq!(size_of::<BiquadFilterParameterLegacy>(), 0xC);
        assert_eq!(size_of::<InParameter>(), 0x70);
        assert_eq!(size_of::<InParameterVersion2a>(), 0x90);
        assert_eq!(size_of::<InParameterVersion2b>(), 0xA8);
        assert_eq!(offset_of!(InParameterVersion2a, biquad_filters), 0x6C);
        assert_eq!(offset_of!(InParameterVersion2a, in_use), 0x84);
        assert_eq!(offset_of!(InParameterVersion2b, biquad_filters), 0x6C);
        assert_eq!(offset_of!(InParameterVersion2b, in_use), 0x9C);
    }
}
