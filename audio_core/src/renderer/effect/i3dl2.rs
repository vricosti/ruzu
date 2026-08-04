use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::renderer::behavior::ErrorInfo;
use crate::renderer::memory::PoolMapper;
use common::fixed_point::FixedPoint;
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::sync::OnceLock;

use super::effect_info_base::{
    EffectInfoBase, InParameterVersion1, InParameterVersion2, ParameterState, UsageState,
};

pub(crate) type I3dl2Fixed = FixedPoint<50, 14>;

fn initialized_i3dl2_reverb_states() -> &'static parking_lot::Mutex<std::collections::HashSet<usize>>
{
    static INITIALIZED: OnceLock<parking_lot::Mutex<std::collections::HashSet<usize>>> =
        OnceLock::new();
    INITIALIZED.get_or_init(|| parking_lot::Mutex::new(std::collections::HashSet::new()))
}

#[cfg(test)]
fn i3dl2_reverb_state_is_initialized(addr: CpuAddr) -> bool {
    initialized_i3dl2_reverb_states()
        .lock()
        .contains(&(addr as usize))
}

pub(crate) fn drop_i3dl2_reverb_state_if_initialized(addr: CpuAddr) {
    if addr == 0 {
        return;
    }
    if initialized_i3dl2_reverb_states()
        .lock()
        .remove(&(addr as usize))
    {
        unsafe { std::ptr::drop_in_place(addr as *mut I3dl2ReverbState) };
    }
}

#[derive(Debug, Default)]
pub struct I3dl2DelayLine {
    pub buffer: Vec<I3dl2Fixed>,
    pub max_delay: i32,
    pub input: usize,
    pub output: usize,
    pub delay: i32,
    pub wet_gain: f32,
}

impl I3dl2DelayLine {
    pub(crate) fn initialize(&mut self, delay_time: i32) {
        self.max_delay = delay_time;
        self.buffer = vec![I3dl2Fixed::default(); delay_time.wrapping_add(1) as usize];
        self.output = 0;
        self.set_delay(delay_time);
        self.wet_gain = 0.0;
    }

    pub(crate) fn set_delay(&mut self, delay_time: i32) {
        if self.max_delay < delay_time {
            return;
        }
        self.delay = delay_time;
        self.input = (self.output + delay_time as usize) % (self.max_delay as usize + 1);
    }

    pub(crate) fn tick(&mut self, sample: I3dl2Fixed) -> I3dl2Fixed {
        self.write(sample);
        let output = self.read();
        self.output += 1;
        if self.output >= self.max_delay as usize {
            self.output = 0;
        }
        output
    }

    pub(crate) fn read(&self) -> I3dl2Fixed {
        self.buffer[self.output]
    }

    fn write(&mut self, sample: I3dl2Fixed) {
        self.buffer[self.input] = sample;
        self.input += 1;
        if self.input >= self.max_delay as usize {
            self.input = 0;
        }
    }

    pub(crate) fn tap_out(&self, index: i32) -> I3dl2Fixed {
        let mut output = self.input as i64 - (index as i64 + 1);
        if output < 0 {
            output += self.max_delay as i64 + 1;
        }
        self.buffer[output as usize]
    }
}

#[derive(Debug, Default)]
pub struct I3dl2ReverbStateInner {
    pub lowpass_0: f32,
    pub lowpass_1: f32,
    pub lowpass_2: f32,
    pub early_delay_line: I3dl2DelayLine,
    pub early_tap_steps: [i32; 20],
    pub early_gain: f32,
    pub late_gain: f32,
    pub early_to_late_taps: i32,
    pub fdn_delay_lines: [I3dl2DelayLine; 4],
    pub decay_delay_lines0: [I3dl2DelayLine; 4],
    pub decay_delay_lines1: [I3dl2DelayLine; 4],
    pub last_reverb_echo: f32,
    pub center_delay_line: I3dl2DelayLine,
    pub lowpass_coeff: [[f32; 3]; 4],
    pub shelf_filter: [f32; 4],
    pub dry_gain: f32,
}

const I3DL2_STATE_MAGIC: u64 = 0x4933_444C_3253_5441;

#[repr(C)]
pub struct I3dl2ReverbState {
    magic: u64,
    inner: MaybeUninit<I3dl2ReverbStateInner>,
}

impl Default for I3dl2ReverbState {
    fn default() -> Self {
        Self {
            magic: I3DL2_STATE_MAGIC,
            inner: MaybeUninit::new(I3dl2ReverbStateInner::default()),
        }
    }
}

impl I3dl2ReverbState {
    pub(crate) fn is_initialized(&self) -> bool {
        self.magic == I3DL2_STATE_MAGIC
    }

    pub(crate) fn reset_and_mark_initialized(&mut self) {
        if self.is_initialized() {
            unsafe { self.inner.assume_init_drop() };
        }
        self.inner.write(I3dl2ReverbStateInner::default());
        self.magic = I3DL2_STATE_MAGIC;
        initialized_i3dl2_reverb_states()
            .lock()
            .insert(self as *mut Self as usize);
    }
}

impl Deref for I3dl2ReverbState {
    type Target = I3dl2ReverbStateInner;

    fn deref(&self) -> &Self::Target {
        assert!(self.is_initialized());
        unsafe { self.inner.assume_init_ref() }
    }
}

impl DerefMut for I3dl2ReverbState {
    fn deref_mut(&mut self) -> &mut Self::Target {
        assert!(self.is_initialized());
        unsafe { self.inner.assume_init_mut() }
    }
}

impl Drop for I3dl2ReverbState {
    fn drop(&mut self) {
        initialized_i3dl2_reverb_states()
            .lock()
            .remove(&(self as *mut Self as usize));
        if self.is_initialized() {
            unsafe { self.inner.assume_init_drop() };
            self.magic = 0;
        }
    }
}

const _: () = assert!(std::mem::size_of::<I3dl2ReverbState>() <= 0x500);

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ParameterVersion1 {
    pub inputs: [i8; MAX_CHANNELS],
    pub outputs: [i8; MAX_CHANNELS],
    pub channel_count_max: u16,
    pub channel_count: u16,
    pub unk10: [u8; 0x4],
    pub sample_rate: u32,
    pub room_hf_gain: f32,
    pub reference_hf: f32,
    pub late_reverb_decay_time: f32,
    pub late_reverb_hf_decay_ratio: f32,
    pub room_gain: f32,
    pub reflection_gain: f32,
    pub reverb_gain: f32,
    pub late_reverb_diffusion: f32,
    pub reflection_delay: f32,
    pub late_reverb_delay_time: f32,
    pub late_reverb_density: f32,
    pub dry_gain: f32,
    pub state: ParameterState,
    pub unk49: [u8; 0x3],
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct ParameterVersion2 {
    pub inputs: [i8; MAX_CHANNELS],
    pub outputs: [i8; MAX_CHANNELS],
    pub channel_count_max: u16,
    pub channel_count: u16,
    pub unk10: [u8; 0x4],
    pub sample_rate: u32,
    pub room_hf_gain: f32,
    pub reference_hf: f32,
    pub late_reverb_decay_time: f32,
    pub late_reverb_hf_decay_ratio: f32,
    pub room_gain: f32,
    pub reflection_gain: f32,
    pub reverb_gain: f32,
    pub late_reverb_diffusion: f32,
    pub reflection_delay: f32,
    pub late_reverb_delay_time: f32,
    pub late_reverb_density: f32,
    pub dry_gain: f32,
    pub state: ParameterState,
    pub unk49: [u8; 0x3],
}

pub fn update_v1(
    effect: &mut EffectInfoBase,
    error_info: &mut ErrorInfo,
    in_params: &InParameterVersion1,
    pool_mapper: &PoolMapper<'_>,
) {
    let specific = EffectInfoBase::read_specific::<ParameterVersion1>(&in_params.specific);
    if EffectInfoBase::is_channel_count_valid(specific.channel_count_max as i32) {
        let old_state = effect.read_parameter::<ParameterVersion1>().state;
        let mut params = specific;
        if !EffectInfoBase::is_channel_count_valid(specific.channel_count as i32) {
            params.channel_count = params.channel_count_max;
        }
        if !EffectInfoBase::is_channel_count_valid(specific.channel_count as i32)
            || old_state != ParameterState::Updated
        {
            params.state = old_state;
        }
        effect.write_parameter(&params);
        effect.apply_common_settings(
            in_params.is_new,
            in_params.enabled,
            in_params.mix_id as i32,
            in_params.process_order as i32,
        );
        if effect.buffer_unmapped || in_params.is_new {
            effect.set_usage(UsageState::New);
            effect.write_parameter_at(0x48, &ParameterState::Initialized);
            effect.buffer_unmapped = !pool_mapper.try_attach_buffer(
                error_info,
                &mut effect.workbuffers[0],
                in_params.workbuffer,
                in_params.workbuffer_size as u64,
            );
            return;
        }
    }
    EffectInfoBase::set_success(error_info);
}

pub fn update_v2(
    effect: &mut EffectInfoBase,
    error_info: &mut ErrorInfo,
    in_params: &InParameterVersion2,
    pool_mapper: &PoolMapper<'_>,
) {
    let specific = EffectInfoBase::read_specific::<ParameterVersion2>(&in_params.specific);
    if EffectInfoBase::is_channel_count_valid(specific.channel_count_max as i32) {
        let old_state = effect.read_parameter::<ParameterVersion2>().state;
        let mut params = specific;
        if !EffectInfoBase::is_channel_count_valid(specific.channel_count as i32) {
            params.channel_count = params.channel_count_max;
        }
        if !EffectInfoBase::is_channel_count_valid(specific.channel_count as i32)
            || old_state != ParameterState::Updated
        {
            params.state = old_state;
        }
        effect.write_parameter(&params);
        effect.apply_common_settings(
            in_params.is_new,
            in_params.enabled,
            in_params.mix_id as i32,
            in_params.process_order as i32,
        );
        if effect.buffer_unmapped || in_params.is_new {
            effect.set_usage(UsageState::New);
            effect.write_parameter_at(0x48, &ParameterState::Initialized);
            effect.buffer_unmapped = !pool_mapper.try_attach_buffer(
                error_info,
                &mut effect.workbuffers[0],
                in_params.workbuffer,
                in_params.workbuffer_size as u64,
            );
            return;
        }
    }
    EffectInfoBase::set_success(error_info);
}

pub fn update_for_command_generation(effect: &mut EffectInfoBase) {
    effect.set_usage(if effect.is_enabled() {
        UsageState::Enabled
    } else {
        UsageState::Disabled
    });
    effect.write_parameter_at(0x48, &ParameterState::Updated);
}

pub fn get_workbuffer(effect: &mut EffectInfoBase, index: i32) -> CpuAddr {
    effect.get_single_buffer(index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::renderer::effect::effect_info_base::EffectType;

    #[test]
    fn effect_info_cleanup_drops_registered_i3dl2_state() {
        let mut effect = EffectInfoBase::default();
        effect.set_type(EffectType::I3dl2Reverb);
        let state =
            unsafe { &mut *(effect.get_state_buffer().as_mut_ptr() as *mut I3dl2ReverbState) };
        let address = state as *mut I3dl2ReverbState as CpuAddr;

        state.reset_and_mark_initialized();
        state.early_delay_line.initialize(32);
        assert!(i3dl2_reverb_state_is_initialized(address));

        effect.cleanup();
        assert!(!i3dl2_reverb_state_is_initialized(address));
    }
}
