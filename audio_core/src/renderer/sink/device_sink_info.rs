use crate::common::common::CpuAddr;
use crate::renderer::behavior::behavior_info::ErrorInfo;
use crate::renderer::memory::PoolMapper;
use crate::renderer::sink::{SinkInParameter, SinkInfoBase, SinkOutStatus, SinkType};
use common::ResultCode;

#[derive(Debug, Clone, Default)]
pub struct DeviceSinkInfo {
    pub base: SinkInfoBase,
}

impl DeviceSinkInfo {
    pub fn new() -> Self {
        Self {
            base: SinkInfoBase {
                sink_type: SinkType::DeviceSink,
                ..Default::default()
            },
        }
    }

    pub fn clean_up(&mut self) {
        self.base.device_state.clear_upsampler_index();
        self.base.device_parameter = Default::default();
        self.base.sink_type = SinkType::Invalid;
    }

    pub fn update(
        &mut self,
        error_info: &mut ErrorInfo,
        out_status: &mut SinkOutStatus,
        in_params: &SinkInParameter,
        _pool_mapper: &PoolMapper<'_>,
    ) {
        let device_params = unsafe { in_params.specific.device };
        if self.base.in_use == in_params.in_use {
            self.base.device_parameter.downmix_enabled = device_params.downmix_enabled;
            self.base.device_parameter.downmix_coeff = device_params.downmix_coeff;
        } else {
            self.base.sink_type = in_params.sink_type;
            self.base.in_use = in_params.in_use;
            self.base.node_id = in_params.node_id;
            self.base.device_parameter = device_params;
        }

        for (dst, src) in self
            .base
            .device_state
            .downmix_coeff
            .iter_mut()
            .zip(self.base.device_parameter.downmix_coeff)
        {
            *dst = (src * 65536.0) as i32;
        }

        *out_status = SinkOutStatus::default();
        error_info.error_code = ResultCode::SUCCESS;
        error_info.address = CpuAddr::default();
    }

    pub fn update_for_command_generation(&mut self) {}
}
