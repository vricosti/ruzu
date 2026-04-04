use crate::common::common::{get_sample_format_byte_size, CpuAddr, SampleFormat};
use crate::renderer::behavior::behavior_info::ErrorInfo;
use crate::renderer::memory::PoolMapper;
use crate::renderer::sink::{SinkInParameter, SinkInfoBase, SinkOutStatus, SinkType};
use common::ResultCode;

#[derive(Debug, Clone, Default)]
pub struct CircularBufferSinkInfo {
    pub base: SinkInfoBase,
}

impl CircularBufferSinkInfo {
    pub fn new() -> Self {
        let mut info = Self::default();
        info.base.sink_type = SinkType::CircularBufferSink;
        info.base.circular_state.address_info.setup(0, 0);
        info
    }

    pub fn clean_up(&mut self) {
        self.base.device_state.upsampler_info = None;
        self.base.circular_parameter = Default::default();
        self.base.sink_type = SinkType::Invalid;
    }

    pub fn update(
        &mut self,
        error_info: &mut ErrorInfo,
        out_status: &mut SinkOutStatus,
        in_params: &SinkInParameter,
        pool_mapper: &PoolMapper<'_>,
    ) {
        let buffer_params = in_params.circular_buffer();

        if self.base.in_use == buffer_params.in_use && !self.base.buffer_unmapped {
            error_info.error_code = ResultCode::SUCCESS;
            error_info.address = CpuAddr::default();
            out_status.write_offset = self.base.circular_state.last_pos2;
            return;
        }

        self.base.node_id = in_params.node_id;
        self.base.in_use = in_params.in_use;

        if self.base.in_use {
            self.base.buffer_unmapped = !pool_mapper.try_attach_buffer(
                error_info,
                &mut self.base.circular_state.address_info,
                buffer_params.cpu_address as usize,
                buffer_params.size as u64,
            );
            self.base.circular_parameter = buffer_params;
        } else {
            self.base.circular_parameter = buffer_params;
        }

        out_status.write_offset = self.base.circular_state.last_pos2;
    }

    pub fn update_for_command_generation(&mut self) {
        if self.base.in_use {
            let params = self.base.circular_parameter;
            let state = &mut self.base.circular_state;
            let pos = state.current_pos;
            state.last_pos2 = state.last_pos;
            state.last_pos = pos as u32;
            state.current_pos += (params.input_count
                * params.sample_count
                * get_sample_format_byte_size(SampleFormat::PcmInt16) as u32)
                as i32;
            if params.size > 0 {
                state.current_pos %= params.size as i32;
            }
        }
    }
}
