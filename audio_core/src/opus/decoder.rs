use crate::adsp::apps::opus::OpusDecoder as AdspOpusDecoder;
use crate::common::common::align_audio;
use crate::errors::{RESULT_BUFFER_TOO_SMALL, RESULT_INPUT_DATA_TOO_SMALL};
use crate::opus::hardware_opus::HardwareOpus;
use crate::opus::parameters::{OpusMultiStreamParametersEx, OpusPacketHeader, OpusParametersEx};
use crate::Result;
use common::alignment::align_up;
use common::ResultCode;
use parking_lot::Mutex;
use std::mem::size_of;
use std::sync::Arc;

pub struct OpusDecoder {
    hardware_opus: HardwareOpus,
    shared_buffer: Vec<u8>,
    shared_buffer_size: u64,
    in_data_offset: usize,
    in_data_size: usize,
    out_data_offset: usize,
    buffer_size: u64,
    sample_rate: i32,
    channel_count: i32,
    use_large_frame_size: bool,
    total_stream_count: i32,
    stereo_stream_count: i32,
    shared_memory_mapped: bool,
    decode_object_initialized: bool,
}

impl OpusDecoder {
    pub fn new(hardware_opus: HardwareOpus) -> Self {
        Self {
            hardware_opus,
            shared_buffer: Vec::new(),
            shared_buffer_size: 0,
            in_data_offset: 0,
            in_data_size: 0,
            out_data_offset: 0,
            buffer_size: 0,
            sample_rate: 0,
            channel_count: 0,
            use_large_frame_size: false,
            total_stream_count: 0,
            stereo_stream_count: 0,
            shared_memory_mapped: false,
            decode_object_initialized: false,
        }
    }

    pub fn new_from_adsp(decoder: Arc<Mutex<AdspOpusDecoder>>) -> Self {
        Self::new(HardwareOpus::new_from_adsp(decoder))
    }

    pub fn initialize(&mut self, params: &OpusParametersEx, transfer_memory_size: u64) -> Result {
        let frame_size = if params.use_large_frame_size {
            5760
        } else {
            1920
        };
        let input_size = 0x600usize;

        self.shared_buffer_size = transfer_memory_size;
        self.shared_buffer = vec![0; transfer_memory_size as usize];
        self.shared_memory_mapped = true;
        self.buffer_size = align_audio(
            (frame_size * params.channel_count as u64)
                / (48_000 / params.sample_rate.max(1) as u64),
            16,
        );
        if transfer_memory_size < self.buffer_size + input_size as u64 {
            return RESULT_BUFFER_TOO_SMALL;
        }
        self.out_data_offset = transfer_memory_size as usize - self.buffer_size as usize;
        self.in_data_size = input_size;
        self.in_data_offset = self.out_data_offset - self.in_data_size;

        let rc = self.hardware_opus.initialize_decode_object(
            params.sample_rate,
            params.channel_count,
            transfer_memory_size,
        );
        if rc.is_error() {
            if self.shared_memory_mapped {
                self.shared_memory_mapped = false;
                let _ = self.hardware_opus.unmap_memory(self.shared_buffer_size);
            }
            return rc;
        }
        self.sample_rate = params.sample_rate as i32;
        self.channel_count = params.channel_count as i32;
        self.use_large_frame_size = params.use_large_frame_size;
        self.total_stream_count = 0;
        self.stereo_stream_count = 0;
        self.decode_object_initialized = true;
        ResultCode::SUCCESS
    }

    pub fn initialize_multi_stream(
        &mut self,
        params: &OpusMultiStreamParametersEx,
        transfer_memory_size: u64,
    ) -> Result {
        let frame_size = if params.use_large_frame_size {
            5760
        } else {
            1920
        };
        let input_size = align_up(1500 * params.total_stream_count as u64, 64) as usize;

        self.shared_buffer_size = transfer_memory_size;
        self.shared_buffer = vec![0; transfer_memory_size as usize];
        self.shared_memory_mapped = true;
        self.buffer_size = align_audio(
            (frame_size * params.channel_count as u64)
                / (48_000 / params.sample_rate.max(1) as u64),
            16,
        );
        if transfer_memory_size < self.buffer_size + input_size as u64 {
            return RESULT_BUFFER_TOO_SMALL;
        }
        self.out_data_offset = transfer_memory_size as usize - self.buffer_size as usize;
        self.in_data_size = input_size;
        self.in_data_offset = self.out_data_offset - self.in_data_size;

        let rc = self.hardware_opus.initialize_multi_stream_decode_object(
            params.sample_rate,
            params.channel_count,
            params.total_stream_count,
            params.stereo_stream_count,
            &params.mappings[..params.channel_count as usize],
            transfer_memory_size,
        );
        if rc.is_error() {
            if self.shared_memory_mapped {
                self.shared_memory_mapped = false;
                let _ = self.hardware_opus.unmap_memory(self.shared_buffer_size);
            }
            return rc;
        }
        self.sample_rate = params.sample_rate as i32;
        self.channel_count = params.channel_count as i32;
        self.total_stream_count = params.total_stream_count as i32;
        self.stereo_stream_count = params.stereo_stream_count as i32;
        self.use_large_frame_size = params.use_large_frame_size;
        self.decode_object_initialized = true;
        ResultCode::SUCCESS
    }

    pub fn decode_interleaved(
        &mut self,
        out_data_size: &mut u32,
        out_time_taken: Option<&mut u64>,
        out_sample_count: &mut u32,
        input_data: &[u8],
        output_data: &mut [u8],
        reset: bool,
    ) -> Result {
        if input_data.len() <= size_of::<OpusPacketHeader>() {
            return RESULT_INPUT_DATA_TOO_SMALL;
        }
        let header = reverse_header(read_header(input_data));
        if self.in_data_size < header.size as usize
            || input_data.len() < header.size as usize + size_of::<OpusPacketHeader>()
        {
            return RESULT_BUFFER_TOO_SMALL;
        }
        if !self.shared_memory_mapped {
            let rc = self.hardware_opus.map_memory(self.shared_buffer_size);
            if rc.is_error() {
                return rc;
            }
            self.shared_memory_mapped = true;
        }
        let payload = &input_data
            [size_of::<OpusPacketHeader>()..size_of::<OpusPacketHeader>() + header.size as usize];
        let channel_count = self.channel_count.max(1) as u32;
        let out_sample_bytes = self.channel_count.max(0) as u32 * size_of::<i16>() as u32;
        let hardware_opus = &self.hardware_opus;
        let (in_data, out_data) = io_buffers_mut(
            &mut self.shared_buffer,
            self.in_data_offset,
            self.in_data_size,
            self.out_data_offset,
            self.buffer_size as usize,
        );
        in_data[..header.size as usize].copy_from_slice(payload);
        let mut time_taken = 0;
        let mut decoded_samples = 0;
        let rc = hardware_opus.decode_interleaved(
            &mut decoded_samples,
            out_data,
            channel_count,
            &in_data[..header.size as usize],
            &mut time_taken,
            reset,
        );
        if rc.is_error() {
            return rc;
        }
        let output_size = decoded_samples.saturating_mul(out_sample_bytes) as usize;
        if output_data.len() < output_size || out_data.len() < output_size {
            return RESULT_BUFFER_TOO_SMALL;
        }
        output_data[..output_size].copy_from_slice(&out_data[..output_size]);
        *out_data_size = header.size + size_of::<OpusPacketHeader>() as u32;
        *out_sample_count = decoded_samples;
        if let Some(out) = out_time_taken {
            *out = time_taken / 1000;
        }
        ResultCode::SUCCESS
    }

    pub fn set_context(&mut self, _context: &[u8]) -> Result {
        if !self.shared_memory_mapped {
            let rc = self.hardware_opus.map_memory(self.shared_buffer_size);
            if rc.is_error() {
                return rc;
            }
            self.shared_memory_mapped = true;
        }
        ResultCode::SUCCESS
    }

    pub fn decode_interleaved_for_multi_stream(
        &mut self,
        out_data_size: &mut u32,
        out_time_taken: Option<&mut u64>,
        out_sample_count: &mut u32,
        input_data: &[u8],
        output_data: &mut [u8],
        reset: bool,
    ) -> Result {
        if input_data.len() <= size_of::<OpusPacketHeader>() {
            return RESULT_INPUT_DATA_TOO_SMALL;
        }
        let header = reverse_header(read_header(input_data));
        if self.in_data_size < header.size as usize
            || input_data.len() < header.size as usize + size_of::<OpusPacketHeader>()
        {
            return RESULT_BUFFER_TOO_SMALL;
        }
        if !self.shared_memory_mapped {
            let rc = self.hardware_opus.map_memory(self.shared_buffer_size);
            if rc.is_error() {
                return rc;
            }
            self.shared_memory_mapped = true;
        }

        let payload = &input_data
            [size_of::<OpusPacketHeader>()..size_of::<OpusPacketHeader>() + header.size as usize];
        let channel_count = self.channel_count.max(1) as u32;
        let out_sample_bytes = self.channel_count.max(0) as u32 * size_of::<i16>() as u32;
        let hardware_opus = &self.hardware_opus;
        let (in_data, out_data) = io_buffers_mut(
            &mut self.shared_buffer,
            self.in_data_offset,
            self.in_data_size,
            self.out_data_offset,
            self.buffer_size as usize,
        );
        in_data[..header.size as usize].copy_from_slice(payload);

        let mut time_taken = 0;
        let mut decoded_samples = 0;
        let rc = hardware_opus.decode_interleaved_for_multi_stream(
            &mut decoded_samples,
            out_data,
            channel_count,
            &in_data[..header.size as usize],
            &mut time_taken,
            reset,
        );
        if rc.is_error() {
            return rc;
        }

        let output_size = decoded_samples.saturating_mul(out_sample_bytes) as usize;
        if output_data.len() < output_size || out_data.len() < output_size {
            return RESULT_BUFFER_TOO_SMALL;
        }
        output_data[..output_size].copy_from_slice(&out_data[..output_size]);
        *out_data_size = header.size + size_of::<OpusPacketHeader>() as u32;
        *out_sample_count = decoded_samples;
        if let Some(out) = out_time_taken {
            *out = time_taken / 1000;
        }
        ResultCode::SUCCESS
    }
}

impl Drop for OpusDecoder {
    fn drop(&mut self) {
        if self.decode_object_initialized {
            let _ = if self.total_stream_count > 0 {
                self.hardware_opus
                    .shutdown_multi_stream_decode_object(self.shared_buffer_size)
            } else {
                self.hardware_opus
                    .shutdown_decode_object(self.shared_buffer_size)
            };
        }
    }
}

fn read_header(input_data: &[u8]) -> OpusPacketHeader {
    let mut header = OpusPacketHeader::default();
    if input_data.len() >= size_of::<OpusPacketHeader>() {
        header.size = u32::from_ne_bytes(input_data[0..4].try_into().unwrap_or([0; 4]));
        header.final_range = u32::from_ne_bytes(input_data[4..8].try_into().unwrap_or([0; 4]));
    }
    header
}

fn reverse_header(header: OpusPacketHeader) -> OpusPacketHeader {
    OpusPacketHeader {
        size: header.size.swap_bytes(),
        final_range: header.final_range.swap_bytes(),
    }
}

fn io_buffers_mut(
    shared_buffer: &mut [u8],
    in_data_offset: usize,
    in_data_size: usize,
    out_data_offset: usize,
    out_data_size: usize,
) -> (&mut [u8], &mut [u8]) {
    let (prefix, suffix) = shared_buffer.split_at_mut(out_data_offset);
    let in_data = &mut prefix[in_data_offset..in_data_offset + in_data_size];
    let out_data = &mut suffix[..out_data_size];
    (in_data, out_data)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adsp::apps::opus::{Direction, Message};

    fn packet_with_payload(size: usize) -> Vec<u8> {
        let mut packet = Vec::with_capacity(size_of::<OpusPacketHeader>() + size);
        packet.extend_from_slice(&(size as u32).to_be_bytes());
        packet.extend_from_slice(&0u32.to_be_bytes());
        packet.extend((0..size).map(|i| i as u8));
        packet
    }

    #[test]
    fn initialize_sets_single_stream_input_and_output_regions() {
        let mut decoder = OpusDecoder::new(HardwareOpus::new());
        let params = OpusParametersEx {
            sample_rate: 48_000,
            channel_count: 2,
            use_large_frame_size: false,
            ..Default::default()
        };

        assert_eq!(decoder.initialize(&params, 0x6000), ResultCode::SUCCESS);
        assert_eq!(decoder.in_data_size, 0x600);
        assert_eq!(
            decoder.out_data_offset,
            0x6000 - decoder.buffer_size as usize
        );
        assert_eq!(
            decoder.in_data_offset + decoder.in_data_size,
            decoder.out_data_offset
        );
    }

    #[test]
    fn initialize_multi_stream_uses_aligned_input_region() {
        let mut decoder = OpusDecoder::new(HardwareOpus::new());
        let params = OpusMultiStreamParametersEx {
            sample_rate: 48_000,
            channel_count: 6,
            total_stream_count: 3,
            stereo_stream_count: 1,
            ..Default::default()
        };

        assert_eq!(
            decoder.initialize_multi_stream(&params, 0x12000),
            ResultCode::SUCCESS
        );
        assert_eq!(decoder.in_data_size, align_up(1500 * 3, 64) as usize);
        assert_eq!(
            decoder.in_data_offset + decoder.in_data_size,
            decoder.out_data_offset
        );
    }

    #[test]
    fn decode_interleaved_rejects_payload_larger_than_internal_input_region() {
        let mut decoder = OpusDecoder::new(HardwareOpus::new());
        let params = OpusParametersEx {
            sample_rate: 48_000,
            channel_count: 2,
            use_large_frame_size: false,
            ..Default::default()
        };
        assert_eq!(decoder.initialize(&params, 0x6000), ResultCode::SUCCESS);

        let packet = packet_with_payload(decoder.in_data_size + 1);
        let mut out_data_size = 0;
        let mut out_samples = 0;
        let mut output = vec![0; 0x1000];

        assert_eq!(
            decoder.decode_interleaved(
                &mut out_data_size,
                None,
                &mut out_samples,
                &packet,
                &mut output,
                false
            ),
            RESULT_BUFFER_TOO_SMALL
        );
    }

    #[test]
    fn decode_interleaved_for_multi_stream_uses_staged_buffers() {
        let mut decoder = OpusDecoder::new(HardwareOpus::new());
        let params = OpusMultiStreamParametersEx {
            sample_rate: 48_000,
            channel_count: 4,
            total_stream_count: 2,
            stereo_stream_count: 1,
            ..Default::default()
        };
        assert_eq!(
            decoder.initialize_multi_stream(&params, 0x12000),
            ResultCode::SUCCESS
        );

        let packet = packet_with_payload(128);
        let mut out_data_size = 0;
        let mut out_time_taken = 0;
        let mut out_samples = 0;
        let mut output = vec![0; decoder.buffer_size as usize];

        assert_eq!(
            decoder.decode_interleaved_for_multi_stream(
                &mut out_data_size,
                Some(&mut out_time_taken),
                &mut out_samples,
                &packet,
                &mut output,
                false
            ),
            ResultCode::SUCCESS
        );
        assert_eq!(out_data_size, (size_of::<OpusPacketHeader>() + 128) as u32);
        assert_eq!(
            out_samples,
            decoder.buffer_size as u32 / (params.channel_count * 2)
        );
        assert_eq!(out_time_taken, 128);
    }

    #[test]
    fn initialize_single_stream_clears_prior_multistream_counts() {
        let mut decoder = OpusDecoder::new(HardwareOpus::new());
        let multi_params = OpusMultiStreamParametersEx {
            sample_rate: 48_000,
            channel_count: 4,
            total_stream_count: 2,
            stereo_stream_count: 1,
            ..Default::default()
        };
        assert_eq!(
            decoder.initialize_multi_stream(&multi_params, 0x12000),
            ResultCode::SUCCESS
        );
        assert_eq!(decoder.total_stream_count, 2);
        assert_eq!(decoder.stereo_stream_count, 1);

        let single_params = OpusParametersEx {
            sample_rate: 48_000,
            channel_count: 2,
            use_large_frame_size: false,
            ..Default::default()
        };
        assert_eq!(
            decoder.initialize(&single_params, 0x6000),
            ResultCode::SUCCESS
        );
        assert_eq!(decoder.total_stream_count, 0);
        assert_eq!(decoder.stereo_stream_count, 0);
    }

    #[test]
    fn adsp_backed_decoder_decodes_single_stream_packet() {
        let adsp_decoder = Arc::new(Mutex::new(AdspOpusDecoder::new(Arc::new(Mutex::new(
            ruzu_core::core::System::new(),
        )))));
        {
            let decoder = adsp_decoder.lock();
            decoder.send(Direction::Dsp, Message::Start);
            assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        }

        let mut decoder = OpusDecoder::new_from_adsp(adsp_decoder);
        let params = OpusParametersEx {
            sample_rate: 48_000,
            channel_count: 2,
            use_large_frame_size: false,
            ..Default::default()
        };
        assert_eq!(decoder.initialize(&params, 0x6000), ResultCode::SUCCESS);

        let packet = packet_with_payload(96);
        let mut out_data_size = 0;
        let mut out_time_taken = 0;
        let mut out_samples = 0;
        let mut output = vec![0; decoder.buffer_size as usize];

        assert_eq!(
            decoder.decode_interleaved(
                &mut out_data_size,
                Some(&mut out_time_taken),
                &mut out_samples,
                &packet,
                &mut output,
                false,
            ),
            ResultCode::SUCCESS
        );
        assert_eq!(out_data_size, (size_of::<OpusPacketHeader>() + 96) as u32);
        assert!(out_samples > 0);
        assert!(out_time_taken > 0);
    }

    #[test]
    fn adsp_backed_decoder_decodes_multistream_packet() {
        let adsp_decoder = Arc::new(Mutex::new(AdspOpusDecoder::new(Arc::new(Mutex::new(
            ruzu_core::core::System::new(),
        )))));
        {
            let decoder = adsp_decoder.lock();
            decoder.send(Direction::Dsp, Message::Start);
            assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        }

        let mut decoder = OpusDecoder::new_from_adsp(adsp_decoder);
        let params = OpusMultiStreamParametersEx {
            sample_rate: 48_000,
            channel_count: 2,
            total_stream_count: 1,
            stereo_stream_count: 1,
            mappings: {
                let mut mappings = [0; crate::opus::parameters::OPUS_STREAM_COUNT_MAX + 1];
                mappings[0] = 0;
                mappings[1] = 1;
                mappings
            },
            ..Default::default()
        };
        assert_eq!(
            decoder.initialize_multi_stream(&params, 0x12000),
            ResultCode::SUCCESS
        );

        let packet = packet_with_payload(96);
        let mut out_data_size = 0;
        let mut out_time_taken = 0;
        let mut out_samples = 0;
        let mut output = vec![0; decoder.buffer_size as usize];

        assert_eq!(
            decoder.decode_interleaved_for_multi_stream(
                &mut out_data_size,
                Some(&mut out_time_taken),
                &mut out_samples,
                &packet,
                &mut output,
                false,
            ),
            ResultCode::SUCCESS
        );
        assert_eq!(out_data_size, (size_of::<OpusPacketHeader>() + 96) as u32);
        assert!(out_samples > 0);
        assert!(out_time_taken > 0);
    }
}
