use crate::adsp::apps::opus::opus_decode_object::{
    DecodeObjectHeader, FLAG_INITIALIZED, FLAG_MAPPED, FLAG_MULTI_STREAM,
};
use crate::adsp::apps::opus::shared_memory::SharedMemory;
use crate::errors::RESULT_LIB_OPUS_INVALID_STATE;
use crate::opus::hardware_opus::HardwareOpus;
use crate::Result;
use common::ResultCode;

pub const DECODE_MULTI_STREAM_OBJECT_MAGIC: u32 = 0xDEAD_BEEF;

pub struct OpusMultiStreamDecodeObject {
    magic: u32,
    initialized: bool,
    state_valid: bool,
    self_buffer: u64,
    final_range: u32,
    hardware_opus: HardwareOpus,
    sample_rate: u32,
    channel_count: u32,
    total_stream_count: u32,
    stereo_stream_count: u32,
    buffer_size: u64,
    mappings: Vec<u8>,
}

impl OpusMultiStreamDecodeObject {
    fn is_valid_channel_count(channel_count: u32) -> bool {
        matches!(channel_count, 1 | 2)
    }

    fn is_valid_stream_counts(total_stream_count: u32, stereo_stream_count: u32) -> bool {
        total_stream_count > 0
            && stereo_stream_count <= total_stream_count
            && Self::is_valid_channel_count(total_stream_count)
    }

    pub fn get_work_buffer_size(total_stream_count: u32, stereo_stream_count: u32) -> u32 {
        if !Self::is_valid_stream_counts(total_stream_count, stereo_stream_count) {
            return 0;
        }
        HardwareOpus::new()
            .get_work_buffer_size_for_multi_stream(total_stream_count, stereo_stream_count)
    }

    pub fn initialize(buffer: u64, comparison_buffer: u64, existing: Option<Self>) -> Self {
        match existing {
            Some(mut decode_object) => {
                if decode_object.magic == DECODE_MULTI_STREAM_OBJECT_MAGIC {
                    if !decode_object.initialized || decode_object.self_buffer == comparison_buffer
                    {
                        decode_object.state_valid = true;
                    }
                } else {
                    decode_object.magic = 0;
                    decode_object.initialized = false;
                    decode_object.state_valid = true;
                    decode_object.self_buffer = buffer;
                    decode_object.final_range = 0;
                }
                if decode_object.self_buffer == 0 {
                    decode_object.self_buffer = buffer;
                }
                decode_object
            }
            None => Self {
                magic: 0,
                initialized: false,
                state_valid: true,
                self_buffer: buffer,
                final_range: 0,
                hardware_opus: HardwareOpus::new(),
                sample_rate: 0,
                channel_count: 0,
                total_stream_count: 0,
                stereo_stream_count: 0,
                buffer_size: 0,
                mappings: Vec::new(),
            },
        }
    }

    pub fn initialize_decoder(
        &mut self,
        sample_rate: u32,
        total_stream_count: u32,
        channel_count: u32,
        stereo_stream_count: u32,
        mappings: &[u8],
        buffer_size: u64,
    ) -> Result {
        if !self.state_valid {
            return RESULT_LIB_OPUS_INVALID_STATE;
        }
        if self.initialized {
            return ResultCode::SUCCESS;
        }
        let result = self.hardware_opus.initialize_multi_stream_decode_object(
            sample_rate,
            channel_count,
            total_stream_count,
            stereo_stream_count,
            mappings,
            buffer_size,
        );
        if result.is_success() {
            self.magic = DECODE_MULTI_STREAM_OBJECT_MAGIC;
            self.initialized = true;
            self.state_valid = true;
            self.final_range = 0;
            self.sample_rate = sample_rate;
            self.channel_count = channel_count;
            self.total_stream_count = total_stream_count;
            self.stereo_stream_count = stereo_stream_count;
            self.buffer_size = buffer_size;
            self.mappings = mappings.to_vec();
        }
        result
    }

    pub fn shutdown(&mut self) -> Result {
        if !self.state_valid {
            return RESULT_LIB_OPUS_INVALID_STATE;
        }
        if self.initialized {
            let result = self
                .hardware_opus
                .shutdown_multi_stream_decode_object(self.buffer_size);
            if result.is_error() {
                return result;
            }
            self.magic = 0;
            self.initialized = false;
            self.state_valid = false;
            self.self_buffer = 0;
            self.final_range = 0;
            self.sample_rate = 0;
            self.channel_count = 0;
            self.total_stream_count = 0;
            self.stereo_stream_count = 0;
            self.buffer_size = 0;
            self.mappings.clear();
        }
        ResultCode::SUCCESS
    }

    pub fn reset_decoder(&mut self) -> Result {
        if !self.state_valid || !self.initialized {
            return RESULT_LIB_OPUS_INVALID_STATE;
        }
        self.hardware_opus.reset_multi_stream_decode_object()
    }

    pub fn decode(
        &mut self,
        out_sample_count: &mut u32,
        output_data: &mut [u8],
        channel_count: u32,
        input_data: &[u8],
        out_time_taken: &mut u64,
    ) -> Result {
        if !self.state_valid || !self.initialized {
            return RESULT_LIB_OPUS_INVALID_STATE;
        }
        self.hardware_opus.decode_interleaved_for_multi_stream(
            out_sample_count,
            output_data,
            channel_count,
            input_data,
            out_time_taken,
            false,
        )
    }

    pub fn get_final_range(&self) -> u32 {
        self.final_range
    }

    pub fn set_final_range(&mut self, final_range: u32) {
        self.final_range = final_range;
    }

    pub fn matches_config(
        shared: &SharedMemory,
        buffer: u64,
        sample_rate: u32,
        channel_count: u32,
        total_stream_count: u32,
        stereo_stream_count: u32,
        buffer_size: u64,
    ) -> bool {
        let Some(header) = DecodeObjectHeader::read(shared, buffer) else {
            return false;
        };
        header.matches_multi_stream_config(
            sample_rate,
            channel_count,
            total_stream_count,
            stereo_stream_count,
            buffer_size,
        )
    }

    pub fn write_successful_header(
        shared: &mut SharedMemory,
        buffer: u64,
        sample_rate: u32,
        channel_count: u32,
        total_stream_count: u32,
        stereo_stream_count: u32,
        buffer_size: u64,
    ) -> bool {
        DecodeObjectHeader {
            magic: DECODE_MULTI_STREAM_OBJECT_MAGIC,
            sample_rate,
            channel_count,
            total_stream_count,
            stereo_stream_count,
            _reserved0: 0,
            buffer_size,
            final_range: 0,
            flags: FLAG_INITIALIZED | FLAG_MULTI_STREAM | FLAG_MAPPED,
        }
        .write(shared, buffer)
    }

    pub fn shutdown_with_header(
        &mut self,
        shared: &mut SharedMemory,
        buffer: u64,
        buffer_size: u64,
    ) -> Result {
        let Some(header) = DecodeObjectHeader::read(shared, buffer) else {
            return RESULT_LIB_OPUS_INVALID_STATE;
        };
        if !header.is_initialized()
            || !header.is_multi_stream()
            || !header.matches_buffer_size(buffer_size)
        {
            return RESULT_LIB_OPUS_INVALID_STATE;
        }
        let result = self.shutdown();
        if result.is_success() {
            let _ = DecodeObjectHeader::clear(shared, buffer);
        }
        result
    }

    pub fn map_memory(
        shared: &mut SharedMemory,
        buffer: u64,
        buffer_size: u64,
        mapped: bool,
    ) -> Result {
        let Some(mut header) = DecodeObjectHeader::read(shared, buffer) else {
            return RESULT_LIB_OPUS_INVALID_STATE;
        };
        if !header.is_initialized()
            || !header.is_multi_stream()
            || !header.matches_buffer_size(buffer_size)
        {
            return RESULT_LIB_OPUS_INVALID_STATE;
        }
        header.set_mapped(mapped);
        if header.write(shared, buffer) {
            ResultCode::SUCCESS
        } else {
            RESULT_LIB_OPUS_INVALID_STATE
        }
    }

    pub fn decode_packet(
        &mut self,
        object_header: &DecodeObjectHeader,
        payload: &[u8],
        output: &mut [u8],
        decoded_samples: &mut u32,
        time_taken: &mut u64,
        packet_final_range: u32,
        expected_final_range: u32,
        decoded_final_range: &mut Option<u32>,
    ) -> Result {
        if !object_header.can_decode_multi_stream() {
            return RESULT_LIB_OPUS_INVALID_STATE;
        }
        let result = self.decode(
            decoded_samples,
            output,
            object_header.channel_count,
            payload,
            time_taken,
        );
        if result.is_error() {
            return result;
        }
        if expected_final_range != 0 && packet_final_range != expected_final_range {
            return RESULT_LIB_OPUS_INVALID_STATE;
        }
        self.set_final_range(packet_final_range);
        *decoded_final_range = Some(self.get_final_range());
        ResultCode::SUCCESS
    }

    pub fn decode_interleaved_message(
        &mut self,
        shared: &SharedMemory,
        buffer: u64,
        payload: &[u8],
        output: &mut [u8],
        decoded_samples: &mut u32,
        time_taken: &mut u64,
        packet_final_range: u32,
        expected_final_range: u32,
        reset_requested: bool,
        decoded_final_range: &mut Option<u32>,
    ) -> Result {
        let Some(object_header) = DecodeObjectHeader::read(shared, buffer) else {
            return RESULT_LIB_OPUS_INVALID_STATE;
        };
        if reset_requested {
            let result = self.reset_decoder();
            if result.is_error() {
                return result;
            }
        }
        self.decode_packet(
            &object_header,
            payload,
            output,
            decoded_samples,
            time_taken,
            packet_final_range,
            expected_final_range,
            decoded_final_range,
        )
    }
}
