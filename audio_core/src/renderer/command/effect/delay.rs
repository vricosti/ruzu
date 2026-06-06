use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::renderer::command::mix::copy_mix_buffer;
use crate::renderer::command::util::write_copy;
use crate::renderer::effect::delay;
use crate::renderer::effect::effect_info_base::ParameterState;
use std::fmt::Write;
use std::sync::OnceLock;

fn initialized_delay_states() -> &'static parking_lot::Mutex<std::collections::HashSet<usize>> {
    static INITIALIZED: OnceLock<parking_lot::Mutex<std::collections::HashSet<usize>>> =
        OnceLock::new();
    INITIALIZED.get_or_init(|| parking_lot::Mutex::new(std::collections::HashSet::new()))
}

pub(crate) fn drop_delay_state_if_initialized(addr: CpuAddr) {
    if addr == 0 {
        return;
    }
    if initialized_delay_states().lock().remove(&(addr as usize)) {
        unsafe { std::ptr::drop_in_place(addr as *mut DelayState) };
    }
}

fn mark_delay_state_initialized(addr: CpuAddr) {
    if addr != 0 {
        initialized_delay_states().lock().insert(addr as usize);
    }
}

#[cfg(test)]
fn delay_state_is_initialized(addr: CpuAddr) -> bool {
    initialized_delay_states().lock().contains(&(addr as usize))
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct DelayPayload {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: delay::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub effect_enabled: bool,
    pub _padding0: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
pub struct DelayCommand {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: delay::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub effect_enabled: bool,
}

impl DelayPayload {
    pub fn process(&self, mix_buffers: &mut [i32], sample_count: usize) {
        process_delay_command(self, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_delay_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_delay_command(self, dump);
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct DelayState {
    pub unk_000: [i32; 8],
    pub delay_lines: [DelayLine; MAX_CHANNELS as usize],
    pub feedback_gain: Fixed18_14,
    pub delay_feedback_gain: Fixed18_14,
    pub delay_feedback_cross_gain: Fixed18_14,
    pub lowpass_gain: Fixed18_14,
    pub lowpass_feedback_gain: Fixed18_14,
    pub lowpass_z: [Fixed50_14; MAX_CHANNELS as usize],
}

const _: () = assert!(std::mem::size_of::<DelayState>() <= 0x500);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[repr(transparent)]
pub struct Fixed18_14(i32);

impl Fixed18_14 {
    const ZERO: Self = Self(0);
    const ONE: Self = Self(1 << 14);
    const FEEDBACK_SCALE: Self = Self((0.97998046875f32 * (1 << 14) as f32) as i32);
    const LOWPASS_SCALE: Self = Self((0.949951171875f32 * (1 << 14) as f32) as i32);

    fn from_raw(raw: i32) -> Self {
        Self(raw)
    }

    fn to_int_floor(self) -> i32 {
        (self.0 & !((1 << 14) - 1)) >> 14
    }

    fn half(self) -> Self {
        Self(self.0 >> 1)
    }
}

impl std::ops::Add for Fixed18_14 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0.wrapping_add(rhs.0))
    }
}

impl std::ops::Sub for Fixed18_14 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0.wrapping_sub(rhs.0))
    }
}

impl std::ops::Mul for Fixed18_14 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self((((self.0 as i64) * (rhs.0 as i64)) >> 14) as i32)
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[repr(transparent)]
pub struct Fixed50_14(i64);

impl Fixed50_14 {
    const ZERO: Self = Self(0);

    fn from_i32_sample(sample: i32) -> Self {
        Self((sample as i64) << 14)
    }

    fn to_int_floor(self) -> i32 {
        ((self.0 & !((1_i64 << 14) - 1)) >> 14) as i32
    }
}

impl std::ops::Add for Fixed50_14 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0.wrapping_add(rhs.0))
    }
}

impl std::ops::Mul<Fixed18_14> for Fixed50_14 {
    type Output = Self;

    fn mul(self, rhs: Fixed18_14) -> Self::Output {
        Self((((self.0 as i128) * (rhs.0 as i128)) >> 14) as i64)
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct DelayLine {
    pub sample_count_max: i32,
    pub sample_count: i32,
    pub buffer: Vec<Fixed50_14>,
    pub buffer_pos: u32,
    pub decay_rate: Fixed18_14,
}

impl DelayLine {
    fn read(&self) -> Fixed50_14 {
        if self.buffer.is_empty() {
            return Fixed50_14::ZERO;
        }
        self.buffer[self.buffer_pos as usize]
    }

    fn write(&mut self, value: Fixed50_14) {
        if self.buffer.is_empty() {
            self.buffer.push(Fixed50_14::ZERO);
            self.sample_count = 1;
            self.buffer_pos = 0;
        }
        self.buffer[self.buffer_pos as usize] = value;
        self.buffer_pos = (self.buffer_pos + 1) % self.buffer.len() as u32;
    }
}

impl Default for DelayLine {
    fn default() -> Self {
        Self {
            sample_count_max: 0,
            sample_count: 0,
            buffer: Vec::new(),
            buffer_pos: 0,
            decay_rate: Fixed18_14::ZERO,
        }
    }
}

impl Default for DelayState {
    fn default() -> Self {
        Self {
            unk_000: [0; 8],
            delay_lines: std::array::from_fn(|_| DelayLine::default()),
            feedback_gain: Fixed18_14::ZERO,
            delay_feedback_gain: Fixed18_14::ZERO,
            delay_feedback_cross_gain: Fixed18_14::ZERO,
            lowpass_gain: Fixed18_14::ONE,
            lowpass_feedback_gain: Fixed18_14::ZERO,
            lowpass_z: [Fixed50_14::ZERO; MAX_CHANNELS as usize],
        }
    }
}

pub fn write_delay_payload(cmd: &DelayCommand, output: &mut [u8]) -> usize {
    let mut payload: DelayPayload = unsafe { std::mem::zeroed() };
    payload.inputs = cmd.inputs;
    payload.outputs = cmd.outputs;
    payload.parameter = cmd.parameter;
    payload.state = cmd.state;
    payload.workbuffer = cmd.workbuffer;
    payload.effect_enabled = cmd.effect_enabled;
    payload._padding0 = [0; 7];
    write_copy(&payload, output)
}

pub fn process_delay_command(payload: &DelayPayload, mix_buffers: &mut [i32], sample_count: usize) {
    let channel_count = payload.parameter.channel_count.max(0) as usize;
    if channel_count == 0 {
        return;
    }

    let Some(state) = read_delay_state_mut(payload.state) else {
        return;
    };
    if payload.effect_enabled {
        match payload.parameter.state {
            ParameterState::Updating => set_delay_effect_parameter(&payload.parameter, state),
            ParameterState::Initialized => {
                initialize_delay_effect(&payload.parameter, state, payload.workbuffer)
            }
            ParameterState::Updated => {}
        }
    }

    apply_delay_effect(
        &payload.parameter,
        state,
        payload.effect_enabled,
        &payload.inputs,
        &payload.outputs,
        mix_buffers,
        sample_count,
        payload.workbuffer,
    );
}

pub fn verify_delay_command(_payload: &DelayPayload) -> bool {
    true
}

pub fn dump_delay_command(payload: &DelayPayload, dump: &mut String) {
    let _ = write!(
        dump,
        "DelayCommand\n\tenabled {} \n\tinputs: ",
        payload.effect_enabled
    );
    for input in &payload.inputs {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = write!(dump, "\n\toutputs: ");
    for output in &payload.outputs {
        let _ = write!(dump, "{:02X}, ", output);
    }
    let _ = writeln!(dump);
}

pub fn set_delay_effect_parameter(parameter: &delay::ParameterVersion2, state: &mut DelayState) {
    let mut channel_spread = Fixed18_14::from_raw(parameter.channel_spread);
    state.feedback_gain =
        Fixed18_14::from_raw(parameter.feedback_gain) * Fixed18_14::FEEDBACK_SCALE;
    state.delay_feedback_gain = state.feedback_gain * (Fixed18_14::ONE - channel_spread);
    if parameter.channel_count == 4 || parameter.channel_count == 6 {
        channel_spread = channel_spread.half();
    }
    state.delay_feedback_cross_gain = channel_spread * state.feedback_gain;
    state.lowpass_feedback_gain =
        Fixed18_14::from_raw(parameter.lowpass_amount) * Fixed18_14::LOWPASS_SCALE;
    state.lowpass_gain = Fixed18_14::ONE - state.lowpass_feedback_gain;
}

pub fn initialize_delay_effect(
    parameter: &delay::ParameterVersion2,
    state: &mut DelayState,
    _workbuffer_addr: CpuAddr,
) {
    let state_addr = state as *mut DelayState as CpuAddr;
    drop_delay_state_if_initialized(state_addr);

    // The state pointer references EffectInfoBase's raw state storage. Build
    // Vec-backed delay lines in place; upstream's workbuffer parameter is unused.
    unsafe { std::ptr::write(state, DelayState::default()) };
    mark_delay_state_initialized(state_addr);

    let channel_count = parameter.channel_count.max(0) as usize;
    let sample_rate = Fixed18_14::from_raw(parameter.sample_rate);
    let sample_count_max = sample_count_max(parameter);
    let delay_samples = delay_sample_count(parameter, sample_rate);

    for channel in 0..channel_count.min(MAX_CHANNELS as usize) {
        let channel_samples = delay_samples.min(sample_count_max);
        let buffer_len = channel_samples.max(1) as usize;
        state.delay_lines[channel].sample_count_max = sample_count_max;
        state.delay_lines[channel].sample_count = channel_samples;
        state.delay_lines[channel].buffer = vec![Fixed50_14::ZERO; buffer_len];
        state.delay_lines[channel].buffer_pos = 0;
        state.delay_lines[channel].decay_rate = Fixed18_14::ONE;
    }

    set_delay_effect_parameter(parameter, state);
}

pub fn apply_delay_effect(
    parameter: &delay::ParameterVersion2,
    state: &mut DelayState,
    enabled: bool,
    inputs: &[i16; MAX_CHANNELS as usize],
    outputs: &[i16; MAX_CHANNELS as usize],
    mix_buffers: &mut [i32],
    sample_count: usize,
    _workbuffer_addr: CpuAddr,
) {
    let channel_count = parameter.channel_count.max(0) as usize;
    let active_channels = channel_count.min(inputs.len()).min(outputs.len());
    if active_channels == 0 {
        return;
    }

    if !enabled || !matches!(active_channels, 1 | 2 | 4 | 6) {
        for channel in 0..active_channels {
            copy_mix_buffer(mix_buffers, sample_count, outputs[channel], inputs[channel]);
        }
        return;
    }

    let in_gain = Fixed18_14::from_raw(parameter.in_gain);
    let wet_gain = Fixed18_14::from_raw(parameter.wet_gain);
    let dry_gain = Fixed18_14::from_raw(parameter.dry_gain);

    for sample_index in 0..sample_count {
        let mut input_samples = [Fixed50_14::ZERO; MAX_CHANNELS as usize];
        let mut delay_samples = [Fixed50_14::ZERO; MAX_CHANNELS as usize];

        for channel in 0..active_channels {
            input_samples[channel] = Fixed50_14::from_i32_sample(
                mix_buffer_sample(mix_buffers, inputs[channel], sample_count, sample_index)
                    .wrapping_mul(64),
            );

            delay_samples[channel] = state.delay_lines[channel].read();
        }

        let mut gained_samples = [Fixed50_14::ZERO; MAX_CHANNELS as usize];
        for dst in 0..active_channels {
            let mut delay = Fixed50_14::ZERO;
            for (src, &delay_sample) in delay_samples.iter().enumerate().take(active_channels) {
                delay = delay + delay_sample * delay_matrix_value(state, active_channels, src, dst);
            }
            gained_samples[dst] = input_samples[dst] * in_gain + delay;
        }

        for channel in 0..active_channels {
            state.lowpass_z[channel] = gained_samples[channel] * state.lowpass_gain
                + state.lowpass_z[channel] * state.lowpass_feedback_gain;
            state.delay_lines[channel].write(state.lowpass_z[channel]);
        }

        for channel in 0..active_channels {
            let output_sample =
                input_samples[channel] * dry_gain + delay_samples[channel] * wet_gain;
            set_mix_buffer_sample(
                mix_buffers,
                outputs[channel],
                sample_count,
                sample_index,
                output_sample.to_int_floor() / 64,
            );
        }
    }
}

fn read_delay_state_mut(addr: CpuAddr) -> Option<&'static mut DelayState> {
    if addr == 0 {
        return None;
    }
    crate::raw_write_trace::maybe_trace_write_at(
        "delay:state_mut",
        addr,
        std::mem::size_of::<DelayState>(),
    );
    Some(unsafe { &mut *(addr as *mut DelayState) })
}

fn sample_count_max(parameter: &delay::ParameterVersion2) -> i32 {
    let raw_32_32 = (0.064f32 * (1_u64 << 32) as f32) as i128;
    let sample_rate = Fixed18_14::from_raw(parameter.sample_rate).to_int_floor() as i128;
    let delay_time_max = parameter.delay_time_max as i128;
    ((raw_32_32 * sample_rate * delay_time_max) >> 32) as i32
}

fn delay_sample_count(parameter: &delay::ParameterVersion2, sample_rate: Fixed18_14) -> i32 {
    let delay_time = parameter.delay_time as i128;
    let sample_rate_per_ms = (sample_rate.0 as i128) / 1000;
    let raw = (delay_time * sample_rate_per_ms) >> 14;
    ((raw & !((1_i128 << 14) - 1)) >> 14) as i32
}

fn delay_matrix_value(
    state: &DelayState,
    channel_count: usize,
    src: usize,
    dst: usize,
) -> Fixed18_14 {
    match channel_count {
        1 => state.feedback_gain,
        2 => {
            if src == dst {
                state.delay_feedback_gain
            } else {
                state.delay_feedback_cross_gain
            }
        }
        4 => match (src, dst) {
            (0, 0) | (1, 1) | (2, 2) | (3, 3) => state.delay_feedback_gain,
            (0, 1) | (0, 2) | (1, 0) | (1, 3) | (2, 0) | (2, 3) | (3, 1) | (3, 2) => {
                state.delay_feedback_cross_gain
            }
            _ => Fixed18_14::ZERO,
        },
        6 => match (src, dst) {
            (0, 0) | (1, 1) | (2, 2) | (4, 4) | (5, 5) => state.delay_feedback_gain,
            (3, 3) => state.feedback_gain,
            (0, 2)
            | (0, 4)
            | (1, 2)
            | (1, 5)
            | (2, 0)
            | (2, 1)
            | (4, 0)
            | (4, 5)
            | (5, 1)
            | (5, 4) => state.delay_feedback_cross_gain,
            _ => Fixed18_14::ZERO,
        },
        _ => Fixed18_14::ZERO,
    }
}

fn mix_buffer_sample(
    mix_buffers: &[i32],
    buffer_index: i16,
    sample_count: usize,
    sample_index: usize,
) -> i32 {
    if buffer_index < 0 {
        return 0;
    }
    let buffer_index = buffer_index as usize;
    mix_buffers
        .get(buffer_index * sample_count + sample_index)
        .copied()
        .unwrap_or(0)
}

fn set_mix_buffer_sample(
    mix_buffers: &mut [i32],
    buffer_index: i16,
    sample_count: usize,
    sample_index: usize,
    value: i32,
) {
    if buffer_index < 0 {
        return;
    }
    let buffer_index = buffer_index as usize;
    if let Some(sample) = mix_buffers.get_mut(buffer_index * sample_count + sample_index) {
        *sample = value;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::renderer::effect::effect_info_base::{EffectInfoBase, EffectType};

    fn parameter(delay_time: i32) -> delay::ParameterVersion2 {
        delay::ParameterVersion2 {
            channel_count_max: 1,
            channel_count: 1,
            delay_time_max: 100,
            delay_time,
            sample_rate: 48_000 << 14,
            state: ParameterState::Initialized,
            in_gain: 1 << 14,
            dry_gain: 1 << 14,
            ..delay::ParameterVersion2::default()
        }
    }

    #[test]
    fn initialize_delay_effect_replaces_registered_vec_state() {
        let mut state = DelayState::default();
        let addr = &mut state as *mut DelayState as CpuAddr;

        initialize_delay_effect(&parameter(10), &mut state, 0);
        assert!(delay_state_is_initialized(addr));
        let first_len = state.delay_lines[0].buffer.len();

        initialize_delay_effect(&parameter(20), &mut state, 0);
        assert!(delay_state_is_initialized(addr));
        assert_ne!(state.delay_lines[0].buffer.len(), 0);
        assert_ne!(state.delay_lines[0].buffer.len(), first_len);

        drop_delay_state_if_initialized(addr);
        assert!(!delay_state_is_initialized(addr));
    }

    #[test]
    fn effect_info_cleanup_drops_registered_delay_state() {
        let mut effect = EffectInfoBase::default();
        effect.set_type(EffectType::Delay);
        let state = unsafe { &mut *(effect.get_state_buffer().as_mut_ptr() as *mut DelayState) };
        let addr = state as *mut DelayState as CpuAddr;

        initialize_delay_effect(&parameter(10), state, 0);
        assert!(delay_state_is_initialized(addr));

        effect.cleanup();
        assert!(!delay_state_is_initialized(addr));
    }

    #[test]
    fn initialize_delay_effect_keeps_zero_delay_line_addressable() {
        let mut state = DelayState::default();

        initialize_delay_effect(&parameter(0), &mut state, 0);

        assert_eq!(state.delay_lines[0].sample_count, 0);
        assert_eq!(state.delay_lines[0].buffer.len(), 1);
        assert_eq!(state.delay_lines[0].read(), Fixed50_14::ZERO);

        drop_delay_state_if_initialized(&mut state as *mut DelayState as CpuAddr);
    }

    #[test]
    fn apply_delay_effect_uses_fixed_point_floor_output() {
        let mut state = DelayState::default();
        let mut params = parameter(1);
        params.state = ParameterState::Initialized;
        initialize_delay_effect(&params, &mut state, 0);

        let inputs = [0, 0, 0, 0, 0, 0];
        let outputs = [1, 0, 0, 0, 0, 0];
        let mut mix_buffers = vec![0; 2 * 2];
        mix_buffers[0] = 1000;
        mix_buffers[1] = -1000;

        apply_delay_effect(
            &params,
            &mut state,
            true,
            &inputs,
            &outputs,
            &mut mix_buffers,
            2,
            0,
        );

        assert_eq!(mix_buffers[2], 1000);
        assert_eq!(mix_buffers[3], -1000);

        drop_delay_state_if_initialized(&mut state as *mut DelayState as CpuAddr);
    }
}
