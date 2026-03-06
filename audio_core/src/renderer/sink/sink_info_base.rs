use crate::common::common::{CpuAddr, SampleFormat, MAX_CHANNELS};
use crate::renderer::behavior::behavior_info::ErrorInfo;
use crate::renderer::memory::{AddressInfo, PoolMapper};
use crate::renderer::upsampler::UpsamplerManager;
use common::fixed_point::FixedPoint;
use common::ResultCode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SinkType {
    Invalid,
    DeviceSink,
    CircularBufferSink,
}

impl Default for SinkType {
    fn default() -> Self {
        Self::Invalid
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DeviceInParameter {
    pub name: [u8; 0x100],
    pub input_count: u32,
    pub inputs: [i8; MAX_CHANNELS],
    pub unk10a: u8,
    pub downmix_enabled: bool,
    pub downmix_coeff: [f32; 4],
}

impl Default for DeviceInParameter {
    fn default() -> Self {
        Self {
            name: [0; 0x100],
            input_count: 0,
            inputs: [0; MAX_CHANNELS],
            unk10a: 0,
            downmix_enabled: false,
            downmix_coeff: [0.0; 4],
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DeviceState {
    pub upsampler_info: Option<usize>,
    pub downmix_coeff: [FixedPoint<16, 16>; 4],
    pub unk18: [u8; 0x18],
}

impl Default for DeviceState {
    fn default() -> Self {
        Self {
            upsampler_info: None,
            downmix_coeff: [FixedPoint::from_base(0); 4],
            unk18: [0; 0x18],
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CircularBufferInParameter {
    pub cpu_address: u64,
    pub size: u32,
    pub input_count: u32,
    pub sample_count: u32,
    pub previous_pos: u32,
    pub format: SampleFormat,
    pub inputs: [i8; MAX_CHANNELS],
    pub in_use: bool,
    pub unk23: [u8; 5],
}

impl Default for CircularBufferInParameter {
    fn default() -> Self {
        Self {
            cpu_address: 0,
            size: 0,
            input_count: 0,
            sample_count: 0,
            previous_pos: 0,
            format: SampleFormat::PcmInt16,
            inputs: [0; MAX_CHANNELS],
            in_use: false,
            unk23: [0; 5],
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CircularBufferState {
    pub last_pos2: u32,
    pub current_pos: i32,
    pub last_pos: u32,
    pub unk0c: [u8; 4],
    pub address_info: AddressInfo,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct SinkInParameter {
    pub sink_type: SinkType,
    pub in_use: bool,
    pub node_id: u32,
    pub unk08: [u8; 0x18],
    pub device: DeviceInParameter,
    pub circular_buffer: CircularBufferInParameter,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct SinkOutStatus {
    pub write_offset: u32,
    pub unk04: [u8; 0x1C],
}

#[derive(Debug, Clone, Default)]
pub struct SinkInfoBase {
    pub sink_type: SinkType,
    pub in_use: bool,
    pub buffer_unmapped: bool,
    pub node_id: u32,
    pub device_state: DeviceState,
    pub circular_state: CircularBufferState,
    pub device_parameter: DeviceInParameter,
    pub circular_parameter: CircularBufferInParameter,
}

impl SinkInfoBase {
    pub fn reset_type(&mut self, sink_type: SinkType) {
        self.clean_up();
        self.sink_type = sink_type;
        if sink_type == SinkType::CircularBufferSink {
            self.circular_state.address_info.setup(0, 0);
        }
    }

    pub fn clean_up_with_upsampler(&mut self, upsampler_manager: &mut UpsamplerManager) {
        if let Some(index) = self.device_state.upsampler_info.take() {
            upsampler_manager.free(index);
        }
        self.clean_up();
    }

    pub fn clean_up(&mut self) {
        self.in_use = false;
        self.buffer_unmapped = false;
        self.node_id = 0;
        self.device_state = DeviceState::default();
        self.circular_state = CircularBufferState::default();
        self.device_parameter = DeviceInParameter::default();
        self.circular_parameter = CircularBufferInParameter::default();
        self.sink_type = SinkType::Invalid;
    }

    pub fn update(
        &mut self,
        error_info: &mut ErrorInfo,
        out_status: &mut SinkOutStatus,
        in_params: &SinkInParameter,
        pool_mapper: &PoolMapper<'_>,
    ) {
        match in_params.sink_type {
            SinkType::Invalid => {
                self.clean_up();
                *out_status = SinkOutStatus::default();
                error_info.error_code = ResultCode::SUCCESS;
                error_info.address = CpuAddr::default();
            }
            SinkType::DeviceSink => {
                let device_params = in_params.device;
                if self.in_use == in_params.in_use {
                    self.device_parameter.downmix_enabled = device_params.downmix_enabled;
                    self.device_parameter.downmix_coeff = device_params.downmix_coeff;
                } else {
                    self.sink_type = in_params.sink_type;
                    self.in_use = in_params.in_use;
                    self.node_id = in_params.node_id;
                    self.device_parameter = device_params;
                }
                for (dst, src) in self
                    .device_state
                    .downmix_coeff
                    .iter_mut()
                    .zip(self.device_parameter.downmix_coeff)
                {
                    *dst = FixedPoint::from_f32(src);
                }
                self.buffer_unmapped = false;
                *out_status = SinkOutStatus::default();
                error_info.error_code = ResultCode::SUCCESS;
                error_info.address = CpuAddr::default();
            }
            SinkType::CircularBufferSink => {
                let buffer_params = in_params.circular_buffer;
                if self.in_use == buffer_params.in_use && !self.buffer_unmapped {
                    error_info.error_code = ResultCode::SUCCESS;
                    error_info.address = CpuAddr::default();
                    out_status.write_offset = self.circular_state.last_pos2;
                    return;
                }

                self.sink_type = in_params.sink_type;
                self.node_id = in_params.node_id;
                self.in_use = in_params.in_use;

                if self.in_use {
                    self.buffer_unmapped = !pool_mapper.try_attach_buffer(
                        error_info,
                        &mut self.circular_state.address_info,
                        buffer_params.cpu_address as usize,
                        buffer_params.size as u64,
                    );
                    self.circular_parameter = buffer_params;
                } else {
                    self.circular_parameter = buffer_params;
                    self.buffer_unmapped = false;
                    error_info.error_code = ResultCode::SUCCESS;
                    error_info.address = CpuAddr::default();
                }

                out_status.write_offset = self.circular_state.last_pos2;
            }
        }
    }

    pub fn update_for_command_generation(&mut self) {
        if self.sink_type != SinkType::CircularBufferSink || !self.in_use {
            return;
        }

        let params = self.circular_parameter;
        let state = &mut self.circular_state;
        let pos = state.current_pos;
        state.last_pos2 = state.last_pos;
        state.last_pos = pos as u32;
        state.current_pos += (params.input_count
            * params.sample_count
            * crate::common::common::get_sample_format_byte_size(SampleFormat::PcmInt16) as u32)
            as i32;
        if params.size > 0 {
            state.current_pos %= params.size as i32;
        }
    }

    pub fn get_device_state(&mut self) -> &mut DeviceState {
        &mut self.device_state
    }

    pub fn get_type(&self) -> SinkType {
        self.sink_type
    }

    pub fn is_used(&self) -> bool {
        self.in_use
    }

    pub fn should_skip(&self) -> bool {
        self.buffer_unmapped
    }

    pub fn get_node_id(&self) -> u32 {
        self.node_id
    }
}
