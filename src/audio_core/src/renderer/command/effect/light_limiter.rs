use crate::adsp::apps::audio_renderer::command_list_processor::MemoryHandle;
use crate::common::common::{CpuAddr, MAX_CHANNELS};
use crate::renderer::command::mix::copy_mix_buffer;
use crate::renderer::command::util::write_copy;
use crate::renderer::effect::effect_info_base::ParameterState;
use crate::renderer::effect::light_limiter;
use crate::renderer::effect::light_limiter::{ProcessingMode, StatisticsInternal};
use common::fixed_point::FixedPoint;
use std::fmt::Write;
use std::sync::OnceLock;

type Fixed49_15 = FixedPoint<49, 15>;

fn initialized_light_limiter_states(
) -> &'static parking_lot::Mutex<std::collections::HashSet<usize>> {
    static INITIALIZED: OnceLock<parking_lot::Mutex<std::collections::HashSet<usize>>> =
        OnceLock::new();
    INITIALIZED.get_or_init(|| parking_lot::Mutex::new(std::collections::HashSet::new()))
}

pub(crate) fn drop_light_limiter_state_if_initialized(addr: CpuAddr) {
    if addr == 0 {
        return;
    }
    if initialized_light_limiter_states()
        .lock()
        .remove(&(addr as usize))
    {
        unsafe { std::ptr::drop_in_place(addr as *mut LightLimiterState) };
    }
}

fn mark_light_limiter_state_initialized(addr: CpuAddr) {
    if addr != 0 {
        initialized_light_limiter_states()
            .lock()
            .insert(addr as usize);
    }
}

#[cfg(test)]
fn light_limiter_state_is_initialized(addr: CpuAddr) -> bool {
    initialized_light_limiter_states()
        .lock()
        .contains(&(addr as usize))
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct LightLimiterVersion1Payload {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: light_limiter::ParameterVersion1,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub effect_enabled: bool,
    pub _padding0: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
pub struct LightLimiterVersion1Command {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: light_limiter::ParameterVersion1,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub effect_enabled: bool,
}

impl LightLimiterVersion1Payload {
    pub fn process(&self, memory: &MemoryHandle, mix_buffers: &mut [i32], sample_count: usize) {
        process_light_limiter_v1_command(self, memory, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_light_limiter_v1_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_light_limiter_v1_command(self, dump);
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct LightLimiterVersion2Payload {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: light_limiter::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub result_state: CpuAddr,
    pub effect_enabled: bool,
    pub _padding0: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
pub struct LightLimiterVersion2Command {
    pub inputs: [i16; MAX_CHANNELS],
    pub outputs: [i16; MAX_CHANNELS],
    pub parameter: light_limiter::ParameterVersion2,
    pub state: CpuAddr,
    pub workbuffer: CpuAddr,
    pub result_state: CpuAddr,
    pub effect_enabled: bool,
}

impl LightLimiterVersion2Payload {
    pub fn process(&self, memory: &MemoryHandle, mix_buffers: &mut [i32], sample_count: usize) {
        process_light_limiter_v2_command(self, memory, mix_buffers, sample_count);
    }

    pub fn verify(&self) -> bool {
        verify_light_limiter_v2_command(self)
    }

    pub fn dump(&self, dump: &mut String) {
        dump_light_limiter_v2_command(self, dump);
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct LightLimiterState {
    pub samples_average: [Fixed49_15; MAX_CHANNELS as usize],
    pub compression_gain: [Fixed49_15; MAX_CHANNELS as usize],
    pub look_ahead_sample_offsets: [i32; MAX_CHANNELS as usize],
    pub look_ahead_sample_buffers: [Vec<Fixed49_15>; MAX_CHANNELS as usize],
}

impl Default for LightLimiterState {
    fn default() -> Self {
        Self {
            samples_average: [Fixed49_15::from_base(0); MAX_CHANNELS as usize],
            compression_gain: [Fixed49_15::from_int(1); MAX_CHANNELS as usize],
            look_ahead_sample_offsets: [0; MAX_CHANNELS as usize],
            look_ahead_sample_buffers: std::array::from_fn(|_| Vec::new()),
        }
    }
}

const _: () = assert!(std::mem::size_of::<LightLimiterState>() <= 0x500);

pub fn write_light_limiter_v1_payload(
    cmd: &LightLimiterVersion1Command,
    output: &mut [u8],
) -> usize {
    let mut payload: LightLimiterVersion1Payload = unsafe { std::mem::zeroed() };
    payload.inputs = cmd.inputs;
    payload.outputs = cmd.outputs;
    payload.parameter = cmd.parameter;
    payload.state = cmd.state;
    payload.workbuffer = cmd.workbuffer;
    payload.effect_enabled = cmd.effect_enabled;
    payload._padding0 = [0; 7];
    write_copy(&payload, output)
}

pub fn write_light_limiter_v2_payload(
    cmd: &LightLimiterVersion2Command,
    output: &mut [u8],
) -> usize {
    let mut payload: LightLimiterVersion2Payload = unsafe { std::mem::zeroed() };
    payload.inputs = cmd.inputs;
    payload.outputs = cmd.outputs;
    payload.parameter = cmd.parameter;
    payload.state = cmd.state;
    payload.workbuffer = cmd.workbuffer;
    payload.result_state = cmd.result_state;
    payload.effect_enabled = cmd.effect_enabled;
    payload._padding0 = [0; 7];
    write_copy(&payload, output)
}

pub fn process_light_limiter_v1_command(
    payload: &LightLimiterVersion1Payload,
    memory: &MemoryHandle,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let parameter = light_limiter_v1_to_v2(payload.parameter);
    process_light_limiter_command(
        &parameter,
        memory,
        &payload.inputs,
        &payload.outputs,
        payload.state,
        payload.workbuffer,
        0,
        payload.effect_enabled,
        mix_buffers,
        sample_count,
    );
}

pub fn verify_light_limiter_v1_command(_payload: &LightLimiterVersion1Payload) -> bool {
    true
}

pub fn dump_light_limiter_v1_command(payload: &LightLimiterVersion1Payload, dump: &mut String) {
    let _ = write!(dump, "LightLimiterVersion1Command\n\tinputs: ");
    for input in &payload.inputs {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = write!(dump, "\n\toutputs: ");
    for output in &payload.outputs {
        let _ = write!(dump, "{:02X}, ", output);
    }
    let _ = writeln!(dump);
}

pub fn process_light_limiter_v2_command(
    payload: &LightLimiterVersion2Payload,
    memory: &MemoryHandle,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    process_light_limiter_command(
        &payload.parameter,
        memory,
        &payload.inputs,
        &payload.outputs,
        payload.state,
        payload.workbuffer,
        payload.result_state,
        payload.effect_enabled,
        mix_buffers,
        sample_count,
    );
}

pub fn verify_light_limiter_v2_command(_payload: &LightLimiterVersion2Payload) -> bool {
    true
}

pub fn dump_light_limiter_v2_command(payload: &LightLimiterVersion2Payload, dump: &mut String) {
    let _ = write!(dump, "LightLimiterVersion2Command\n\tinputs: \n");
    for input in &payload.inputs {
        let _ = write!(dump, "{:02X}, ", input);
    }
    let _ = write!(dump, "\n\toutputs: ");
    for output in &payload.outputs {
        let _ = write!(dump, "{:02X}, ", output);
    }
    let _ = writeln!(dump);
}

pub fn process_light_limiter_command(
    parameter: &light_limiter::ParameterVersion2,
    memory: &MemoryHandle,
    inputs: &[i16; MAX_CHANNELS as usize],
    outputs: &[i16; MAX_CHANNELS as usize],
    state_addr: CpuAddr,
    workbuffer_addr: CpuAddr,
    result_state_addr: CpuAddr,
    effect_enabled: bool,
    mix_buffers: &mut [i32],
    sample_count: usize,
) {
    let channel_count = parameter.channel_count.max(0) as usize;
    if channel_count == 0 {
        return;
    }

    let Some(state) = read_light_limiter_state_mut(state_addr) else {
        return;
    };

    if effect_enabled {
        match parameter.state {
            ParameterState::Updating => {
                update_light_limiter_effect_parameter(parameter, state);
            }
            ParameterState::Initialized => {
                initialize_light_limiter_effect(parameter, state, memory, workbuffer_addr);
            }
            ParameterState::Updated => {}
        }
    }

    apply_light_limiter_effect(
        parameter,
        state,
        memory,
        effect_enabled,
        inputs,
        outputs,
        mix_buffers,
        sample_count,
        workbuffer_addr,
        result_state_addr,
    );
}

pub fn light_limiter_v1_to_v2(
    parameter: light_limiter::ParameterVersion1,
) -> light_limiter::ParameterVersion2 {
    light_limiter::ParameterVersion2 {
        inputs: parameter.inputs,
        outputs: parameter.outputs,
        channel_count_max: parameter.channel_count_max,
        channel_count: parameter.channel_count,
        sample_rate: parameter.sample_rate,
        look_ahead_time_max: parameter.look_ahead_time_max,
        attack_time: parameter.attack_time,
        release_time: parameter.release_time,
        look_ahead_time: parameter.look_ahead_time,
        attack_coeff: parameter.attack_coeff,
        release_coeff: parameter.release_coeff,
        threshold: parameter.threshold,
        input_gain: parameter.input_gain,
        output_gain: parameter.output_gain,
        look_ahead_samples_min: parameter.look_ahead_samples_min,
        look_ahead_samples_max: parameter.look_ahead_samples_max,
        state: parameter.state,
        statistics_enabled: parameter.statistics_enabled,
        statistics_reset_required: parameter.statistics_reset_required,
        processing_mode: parameter.processing_mode,
    }
}

pub fn update_light_limiter_effect_parameter(
    _parameter: &light_limiter::ParameterVersion2,
    _state: &mut LightLimiterState,
) {
}

pub fn initialize_light_limiter_effect(
    parameter: &light_limiter::ParameterVersion2,
    state: &mut LightLimiterState,
    _memory: &MemoryHandle,
    _workbuffer_addr: CpuAddr,
) {
    let state_addr = state as *mut LightLimiterState as CpuAddr;
    drop_light_limiter_state_if_initialized(state_addr);
    unsafe { std::ptr::write(state, LightLimiterState::default()) };
    mark_light_limiter_state_initialized(state_addr);

    let channel_count = parameter.channel_count.max(0) as usize;
    let look_ahead_samples_max = parameter.look_ahead_samples_max.max(0) as usize;
    for channel in 0..channel_count.min(MAX_CHANNELS as usize) {
        state.look_ahead_sample_buffers[channel]
            .resize(look_ahead_samples_max, Fixed49_15::from_base(0));
    }
}

pub fn apply_light_limiter_effect(
    parameter: &light_limiter::ParameterVersion2,
    state: &mut LightLimiterState,
    _memory: &MemoryHandle,
    enabled: bool,
    inputs: &[i16; MAX_CHANNELS as usize],
    outputs: &[i16; MAX_CHANNELS as usize],
    mix_buffers: &mut [i32],
    sample_count: usize,
    _workbuffer_addr: CpuAddr,
    result_state_addr: CpuAddr,
) {
    let channel_count = parameter.channel_count.max(0) as usize;
    let active_channels = channel_count.min(inputs.len()).min(outputs.len());
    if active_channels == 0 {
        return;
    }

    if !enabled {
        for channel in 0..active_channels {
            if parameter.inputs[channel] != parameter.outputs[channel] {
                copy_mix_buffer(mix_buffers, sample_count, outputs[channel], inputs[channel]);
            }
        }
        return;
    }

    let mut statistics = if parameter.statistics_enabled {
        read_statistics_internal_mut(result_state_addr)
    } else {
        None
    };

    if let Some(stats) = statistics.as_deref_mut() {
        if parameter.statistics_reset_required {
            for channel in 0..active_channels {
                stats.channel_compression_gain_min[channel] = 1.0;
                stats.channel_max_sample[channel] = 0.0;
            }
        }
    }

    for sample_index in 0..sample_count {
        for channel in 0..active_channels {
            let input = mix_buffer_sample(mix_buffers, inputs[channel], sample_count, sample_index);
            let sample =
                Fixed49_15::from_base(input as i64) * Fixed49_15::from_f32(parameter.input_gain);
            let abs_sample = if sample < Fixed49_15::from_base(0) {
                -sample
            } else {
                sample
            };
            let coeff = if abs_sample > state.samples_average[channel] {
                Fixed49_15::from_f32(parameter.attack_coeff)
            } else {
                Fixed49_15::from_f32(parameter.release_coeff)
            };
            state.samples_average[channel] += Fixed49_15::from_f32(
                ((abs_sample - state.samples_average[channel]) * coeff).to_f32(),
            );

            let average = state.samples_average[channel];
            let mut new_average_sample = Fixed49_15::from_f64(recip_estimate(average.to_f64()));
            if !matches!(parameter.processing_mode, ProcessingMode::Mode1) {
                let temp = Fixed49_15::from_int(2) - (average * new_average_sample);
                new_average_sample = Fixed49_15::from_int(2) - (average * temp);
            }

            let threshold = Fixed49_15::from_f32(parameter.threshold);
            let attenuation = if average > threshold {
                threshold * new_average_sample
            } else {
                Fixed49_15::from_int(1)
            };
            let coeff = if attenuation < state.compression_gain[channel] {
                Fixed49_15::from_f32(parameter.attack_coeff)
            } else {
                Fixed49_15::from_f32(parameter.release_coeff)
            };
            state.compression_gain[channel] +=
                (attenuation - state.compression_gain[channel]) * coeff;

            let lookahead_buffer = &mut state.look_ahead_sample_buffers[channel];
            let offset = state.look_ahead_sample_offsets[channel] as usize;
            let lookahead_sample = lookahead_buffer[offset];
            lookahead_buffer[offset] = sample;
            state.look_ahead_sample_offsets[channel] =
                (state.look_ahead_sample_offsets[channel] + 1) % parameter.look_ahead_samples_min;

            let output_sample = lookahead_sample
                * state.compression_gain[channel]
                * Fixed49_15::from_f32(parameter.output_gain)
                * Fixed49_15::from_int(Fixed49_15::ONE);
            set_mix_buffer_sample(
                mix_buffers,
                outputs[channel],
                sample_count,
                sample_index,
                output_sample
                    .to_long()
                    .clamp(i32::MIN as i64, i32::MAX as i64) as i32,
            );

            if let Some(stats) = statistics.as_deref_mut() {
                stats.channel_max_sample[channel] =
                    stats.channel_max_sample[channel].max(abs_sample.to_f32());
                stats.channel_compression_gain_min[channel] = stats.channel_compression_gain_min
                    [channel]
                    .min(state.compression_gain[channel].to_f32());
            }
        }
    }
}

fn read_light_limiter_state_mut(addr: CpuAddr) -> Option<&'static mut LightLimiterState> {
    if addr == 0 {
        return None;
    }
    crate::raw_write_trace::maybe_trace_write_at(
        "light_limiter:state_mut",
        addr,
        std::mem::size_of::<LightLimiterState>(),
    );
    Some(unsafe { &mut *(addr as *mut LightLimiterState) })
}

fn read_statistics_internal_mut(addr: CpuAddr) -> Option<&'static mut StatisticsInternal> {
    if addr == 0 {
        return None;
    }
    crate::raw_write_trace::maybe_trace_write_at(
        "light_limiter:statistics_internal_mut",
        addr,
        std::mem::size_of::<StatisticsInternal>(),
    );
    Some(unsafe { &mut *(addr as *mut StatisticsInternal) })
}

fn recip_estimate(value: f64) -> f64 {
    let q = (value * 512.0) as i32;
    let r = 1.0 / (((q as f64) + 0.5) / 512.0);
    let s = (256.0 * r + 0.5) as i32;
    s as f64 / 256.0
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

    fn parameter() -> light_limiter::ParameterVersion2 {
        light_limiter::ParameterVersion2 {
            channel_count_max: 1,
            channel_count: 1,
            look_ahead_samples_min: 1,
            look_ahead_samples_max: 4,
            state: ParameterState::Initialized,
            ..light_limiter::ParameterVersion2::default()
        }
    }

    #[test]
    fn effect_info_cleanup_drops_registered_light_limiter_state() {
        let mut effect = EffectInfoBase::default();
        effect.set_type(EffectType::LightLimiter);
        let state =
            unsafe { &mut *(effect.get_state_buffer().as_mut_ptr() as *mut LightLimiterState) };
        let address = state as *mut LightLimiterState as CpuAddr;

        initialize_light_limiter_effect(&parameter(), state, &MemoryHandle::default(), 0);
        assert!(light_limiter_state_is_initialized(address));
        assert_eq!(state.look_ahead_sample_buffers[0].len(), 4);

        effect.cleanup();
        assert!(!light_limiter_state_is_initialized(address));
    }
}
