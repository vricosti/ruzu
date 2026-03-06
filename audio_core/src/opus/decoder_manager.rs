use crate::adsp::apps::opus::OpusDecoder as AdspOpusDecoder;
use crate::errors::{RESULT_INVALID_OPUS_CHANNEL_COUNT, RESULT_INVALID_OPUS_SAMPLE_RATE};
use crate::opus::hardware_opus::HardwareOpus;
use crate::opus::parameters::{
    OpusMultiStreamParameters, OpusMultiStreamParametersEx, OpusParameters, OpusParametersEx,
    MAX_CHANNELS, OPUS_STREAM_COUNT_MAX,
};
use crate::Result;
use common::alignment::align_up;
use common::ResultCode;
use parking_lot::Mutex;
use std::sync::Arc;

pub struct OpusDecoderManager {
    hardware_opus: HardwareOpus,
    required_workbuffer_sizes: [u32; MAX_CHANNELS],
}

impl OpusDecoderManager {
    pub fn new() -> Self {
        Self::new_with_hardware_opus(HardwareOpus::new())
    }

    pub fn new_with_hardware_opus(hardware_opus: HardwareOpus) -> Self {
        let mut required_workbuffer_sizes = [0; MAX_CHANNELS];
        for channel in 0..MAX_CHANNELS {
            required_workbuffer_sizes[channel] =
                hardware_opus.get_work_buffer_size((channel + 1) as u32);
        }
        Self {
            hardware_opus,
            required_workbuffer_sizes,
        }
    }

    pub fn new_from_adsp(decoder: Arc<Mutex<AdspOpusDecoder>>) -> Self {
        Self::new_with_hardware_opus(HardwareOpus::new_from_adsp(decoder))
    }

    pub fn get_hardware_opus(&self) -> &HardwareOpus {
        &self.hardware_opus
    }

    pub fn get_work_buffer_size(&self, params: &OpusParameters, out_size: &mut u32) -> Result {
        let ex = OpusParametersEx {
            sample_rate: params.sample_rate,
            channel_count: params.channel_count,
            use_large_frame_size: false,
            ..Default::default()
        };
        self.get_work_buffer_size_ex_ex(&ex, out_size)
    }

    pub fn get_work_buffer_size_ex(&self, params: &OpusParametersEx, out_size: &mut u32) -> Result {
        self.get_work_buffer_size_ex_ex(params, out_size)
    }

    pub fn get_work_buffer_size_ex_ex(
        &self,
        params: &OpusParametersEx,
        out_size: &mut u32,
    ) -> Result {
        if !is_valid_channel_count(params.channel_count) {
            return RESULT_INVALID_OPUS_CHANNEL_COUNT;
        }
        if !is_valid_sample_rate(params.sample_rate) {
            return RESULT_INVALID_OPUS_SAMPLE_RATE;
        }

        let mut work_buffer_size =
            self.required_workbuffer_sizes[params.channel_count as usize - 1];
        let frame_size = if params.use_large_frame_size {
            5760
        } else {
            1920
        };
        let frame_samples = (frame_size as u64 * params.channel_count as u64)
            / (48_000 / params.sample_rate) as u64;
        work_buffer_size = work_buffer_size
            .saturating_add(align_up(frame_samples, 64) as u32)
            .saturating_add(0x600);
        *out_size = work_buffer_size;
        ResultCode::SUCCESS
    }

    pub fn get_work_buffer_size_for_multi_stream(
        &self,
        params: &OpusMultiStreamParameters,
        out_size: &mut u32,
    ) -> Result {
        let ex = OpusMultiStreamParametersEx {
            sample_rate: params.sample_rate,
            channel_count: params.channel_count,
            total_stream_count: params.total_stream_count,
            stereo_stream_count: params.stereo_stream_count,
            use_large_frame_size: false,
            mappings: params.mappings,
            ..Default::default()
        };
        self.get_work_buffer_size_for_multi_stream_ex_ex(&ex, out_size)
    }

    pub fn get_work_buffer_size_for_multi_stream_ex(
        &self,
        params: &OpusMultiStreamParametersEx,
        out_size: &mut u32,
    ) -> Result {
        self.get_work_buffer_size_for_multi_stream_ex_ex(params, out_size)
    }

    pub fn get_work_buffer_size_for_multi_stream_ex_ex(
        &self,
        params: &OpusMultiStreamParametersEx,
        out_size: &mut u32,
    ) -> Result {
        if !is_valid_multi_stream_channel_count(params.channel_count) {
            return RESULT_INVALID_OPUS_CHANNEL_COUNT;
        }
        if !is_valid_sample_rate(params.sample_rate) {
            return RESULT_INVALID_OPUS_SAMPLE_RATE;
        }
        if !is_valid_stream_count(
            params.channel_count,
            params.total_stream_count,
            params.stereo_stream_count,
        ) {
            return RESULT_INVALID_OPUS_SAMPLE_RATE;
        }

        let mut work_buffer_size = self.hardware_opus.get_work_buffer_size_for_multi_stream(
            params.total_stream_count,
            params.stereo_stream_count,
        );
        let frame_size = if params.use_large_frame_size {
            5760
        } else {
            1920
        };
        work_buffer_size = work_buffer_size
            .saturating_add(align_up(1500 * params.total_stream_count as u64, 64) as u32)
            .saturating_add(align_up(
                (frame_size as u64 * params.channel_count as u64)
                    / (48_000 / params.sample_rate) as u64,
                64,
            ) as u32);
        *out_size = work_buffer_size;
        ResultCode::SUCCESS
    }

    pub fn required_workbuffer_sizes(&self) -> [u32; MAX_CHANNELS] {
        self.required_workbuffer_sizes
    }
}

fn is_valid_channel_count(channel_count: u32) -> bool {
    channel_count == 1 || channel_count == 2
}

fn is_valid_multi_stream_channel_count(channel_count: u32) -> bool {
    channel_count > 0 && channel_count <= OPUS_STREAM_COUNT_MAX as u32
}

fn is_valid_sample_rate(sample_rate: u32) -> bool {
    matches!(sample_rate, 8_000 | 12_000 | 16_000 | 24_000 | 48_000)
}

fn is_valid_stream_count(
    channel_count: u32,
    total_stream_count: u32,
    stereo_stream_count: u32,
) -> bool {
    total_stream_count > 0
        && stereo_stream_count <= total_stream_count
        && total_stream_count.saturating_add(stereo_stream_count) <= channel_count
}

impl Default for OpusDecoderManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adsp::apps::opus::{Direction, Message};

    #[test]
    fn get_work_buffer_size_rejects_invalid_channel_count() {
        let manager = OpusDecoderManager::new();
        let params = OpusParametersEx {
            sample_rate: 48_000,
            channel_count: 3,
            use_large_frame_size: false,
            ..Default::default()
        };
        let mut out_size = 0;

        assert_eq!(
            manager.get_work_buffer_size_ex_ex(&params, &mut out_size),
            RESULT_INVALID_OPUS_CHANNEL_COUNT
        );
    }

    #[test]
    fn get_work_buffer_size_rejects_invalid_sample_rate() {
        let manager = OpusDecoderManager::new();
        let params = OpusParametersEx {
            sample_rate: 44_100,
            channel_count: 2,
            use_large_frame_size: false,
            ..Default::default()
        };
        let mut out_size = 0;

        assert_eq!(
            manager.get_work_buffer_size_ex_ex(&params, &mut out_size),
            RESULT_INVALID_OPUS_SAMPLE_RATE
        );
    }

    #[test]
    fn get_work_buffer_size_matches_zuyu_formula() {
        let manager = OpusDecoderManager::new();
        let params = OpusParametersEx {
            sample_rate: 24_000,
            channel_count: 2,
            use_large_frame_size: false,
            ..Default::default()
        };
        let mut out_size = 0;

        assert_eq!(
            manager.get_work_buffer_size_ex_ex(&params, &mut out_size),
            ResultCode::SUCCESS
        );

        let expected = manager.required_workbuffer_sizes()[1]
            + align_up((1920 * 2 / (48_000 / 24_000)) as u64, 64) as u32
            + 0x600;
        assert_eq!(out_size, expected);
    }

    #[test]
    fn get_work_buffer_size_for_multi_stream_rejects_invalid_channel_count() {
        let manager = OpusDecoderManager::new();
        let params = OpusMultiStreamParametersEx {
            sample_rate: 48_000,
            channel_count: 0,
            total_stream_count: 1,
            stereo_stream_count: 0,
            ..Default::default()
        };
        let mut out_size = 0;

        assert_eq!(
            manager.get_work_buffer_size_for_multi_stream_ex_ex(&params, &mut out_size),
            RESULT_INVALID_OPUS_CHANNEL_COUNT
        );
    }

    #[test]
    fn get_work_buffer_size_for_multi_stream_rejects_invalid_stream_count() {
        let manager = OpusDecoderManager::new();
        let params = OpusMultiStreamParametersEx {
            sample_rate: 48_000,
            channel_count: 2,
            total_stream_count: 2,
            stereo_stream_count: 1,
            ..Default::default()
        };
        let mut out_size = 0;

        assert_eq!(
            manager.get_work_buffer_size_for_multi_stream_ex_ex(&params, &mut out_size),
            RESULT_INVALID_OPUS_SAMPLE_RATE
        );
    }

    #[test]
    fn get_work_buffer_size_for_multi_stream_matches_zuyu_formula() {
        let manager = OpusDecoderManager::new();
        let params = OpusMultiStreamParametersEx {
            sample_rate: 48_000,
            channel_count: 6,
            total_stream_count: 4,
            stereo_stream_count: 2,
            use_large_frame_size: true,
            ..Default::default()
        };
        let mut out_size = 0;

        assert_eq!(
            manager.get_work_buffer_size_for_multi_stream_ex_ex(&params, &mut out_size),
            ResultCode::SUCCESS
        );

        let expected = manager.hardware_opus.get_work_buffer_size_for_multi_stream(
            params.total_stream_count,
            params.stereo_stream_count,
        ) + align_up(1500 * params.total_stream_count as u64, 64) as u32
            + align_up((5760 * params.channel_count) as u64, 64) as u32;
        assert_eq!(out_size, expected);
    }

    #[test]
    fn adsp_backed_manager_populates_required_workbuffer_sizes() {
        let decoder = Arc::new(Mutex::new(AdspOpusDecoder::new(Arc::new(Mutex::new(
            ruzu_core::core::System::new(),
        )))));
        {
            let decoder = decoder.lock();
            decoder.send(Direction::Dsp, Message::Start);
            assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        }

        let manager = OpusDecoderManager::new_from_adsp(decoder);
        let sizes = manager.required_workbuffer_sizes();

        assert_eq!(sizes[0], 0x2000);
        assert_eq!(sizes[1], 0x4000);
    }
}
