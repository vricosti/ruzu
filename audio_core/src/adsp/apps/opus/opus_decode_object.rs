use crate::adsp::apps::opus::shared_memory::SharedMemory;
use crate::errors::RESULT_LIB_OPUS_INVALID_STATE;
use crate::opus::hardware_opus::HardwareOpus;
use crate::Result;
use common::ResultCode;
use std::mem::size_of;

pub const DECODE_OBJECT_MAGIC: u32 = 0xDEAD_BEEF;
pub const FLAG_INITIALIZED: u32 = 1 << 0;
pub const FLAG_MULTI_STREAM: u32 = 1 << 1;
pub const FLAG_MAPPED: u32 = 1 << 2;

#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct DecodeObjectHeader {
    pub magic: u32,
    pub sample_rate: u32,
    pub channel_count: u32,
    pub total_stream_count: u32,
    pub stereo_stream_count: u32,
    pub _reserved0: u32,
    pub buffer_size: u64,
    pub final_range: u32,
    pub flags: u32,
}

impl DecodeObjectHeader {
    pub fn read(shared: &SharedMemory, buffer: u64) -> Option<Self> {
        let raw = shared.read_transfer(buffer as usize, size_of::<DecodeObjectHeader>())?;
        Some(Self {
            magic: u32::from_ne_bytes(raw[0..4].try_into().unwrap_or([0; 4])),
            sample_rate: u32::from_ne_bytes(raw[4..8].try_into().unwrap_or([0; 4])),
            channel_count: u32::from_ne_bytes(raw[8..12].try_into().unwrap_or([0; 4])),
            total_stream_count: u32::from_ne_bytes(raw[12..16].try_into().unwrap_or([0; 4])),
            stereo_stream_count: u32::from_ne_bytes(raw[16..20].try_into().unwrap_or([0; 4])),
            _reserved0: u32::from_ne_bytes(raw[20..24].try_into().unwrap_or([0; 4])),
            buffer_size: u64::from_ne_bytes(raw[24..32].try_into().unwrap_or([0; 8])),
            final_range: u32::from_ne_bytes(raw[32..36].try_into().unwrap_or([0; 4])),
            flags: u32::from_ne_bytes(raw[36..40].try_into().unwrap_or([0; 4])),
        })
    }

    pub fn write(self, shared: &mut SharedMemory, buffer: u64) -> bool {
        let mut raw = [0u8; size_of::<DecodeObjectHeader>()];
        raw[0..4].copy_from_slice(&self.magic.to_ne_bytes());
        raw[4..8].copy_from_slice(&self.sample_rate.to_ne_bytes());
        raw[8..12].copy_from_slice(&self.channel_count.to_ne_bytes());
        raw[12..16].copy_from_slice(&self.total_stream_count.to_ne_bytes());
        raw[16..20].copy_from_slice(&self.stereo_stream_count.to_ne_bytes());
        raw[20..24].copy_from_slice(&self._reserved0.to_ne_bytes());
        raw[24..32].copy_from_slice(&self.buffer_size.to_ne_bytes());
        raw[32..36].copy_from_slice(&self.final_range.to_ne_bytes());
        raw[36..40].copy_from_slice(&self.flags.to_ne_bytes());
        shared.write_transfer(buffer as usize, &raw)
    }

    pub fn clear(shared: &mut SharedMemory, buffer: u64) -> bool {
        Self::default().write(shared, buffer)
    }

    pub fn write_final_range(shared: &mut SharedMemory, buffer: u64, final_range: u32) -> bool {
        let Some(mut header) = Self::read(shared, buffer) else {
            return false;
        };
        header.final_range = final_range;
        header.write(shared, buffer)
    }

    pub fn is_initialized(&self) -> bool {
        self.magic == DECODE_OBJECT_MAGIC && (self.flags & FLAG_INITIALIZED) != 0
    }

    pub fn is_multi_stream(&self) -> bool {
        (self.flags & FLAG_MULTI_STREAM) != 0
    }

    pub fn is_mapped(&self) -> bool {
        (self.flags & FLAG_MAPPED) != 0
    }

    pub fn matches_buffer_size(&self, buffer_size: u64) -> bool {
        self.buffer_size == buffer_size
    }

    pub fn matches_single_config(
        &self,
        sample_rate: u32,
        channel_count: u32,
        buffer_size: u64,
    ) -> bool {
        self.is_initialized()
            && !self.is_multi_stream()
            && self.sample_rate == sample_rate
            && self.channel_count == channel_count
            && self.matches_buffer_size(buffer_size)
    }

    pub fn matches_multi_stream_config(
        &self,
        sample_rate: u32,
        channel_count: u32,
        total_stream_count: u32,
        stereo_stream_count: u32,
        buffer_size: u64,
    ) -> bool {
        self.is_initialized()
            && self.is_multi_stream()
            && self.sample_rate == sample_rate
            && self.channel_count == channel_count
            && self.total_stream_count == total_stream_count
            && self.stereo_stream_count == stereo_stream_count
            && self.matches_buffer_size(buffer_size)
    }

    pub fn can_decode_single(&self) -> bool {
        self.is_initialized() && self.is_mapped() && !self.is_multi_stream()
    }

    pub fn can_decode_multi_stream(&self) -> bool {
        self.is_initialized() && self.is_mapped() && self.is_multi_stream()
    }

    pub fn set_mapped(&mut self, mapped: bool) {
        if mapped {
            self.flags |= FLAG_MAPPED;
        } else {
            self.flags &= !FLAG_MAPPED;
        }
    }
}

pub struct OpusDecodeObject {
    magic: u32,
    initialized: bool,
    state_valid: bool,
    self_buffer: u64,
    final_range: u32,
    hardware_opus: HardwareOpus,
    sample_rate: u32,
    channel_count: u32,
    buffer_size: u64,
}

impl OpusDecodeObject {
    fn is_valid_channel_count(channel_count: u32) -> bool {
        matches!(channel_count, 1 | 2)
    }

    pub fn get_work_buffer_size(channel_count: u32) -> u32 {
        if !Self::is_valid_channel_count(channel_count) {
            return 0;
        }
        HardwareOpus::new().get_work_buffer_size(channel_count)
    }

    pub fn initialize(buffer: u64, comparison_buffer: u64, existing: Option<Self>) -> Self {
        match existing {
            Some(mut decode_object) => {
                if decode_object.magic == DECODE_OBJECT_MAGIC {
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
                buffer_size: 0,
            },
        }
    }

    pub fn initialize_decoder(
        &mut self,
        sample_rate: u32,
        channel_count: u32,
        buffer_size: u64,
    ) -> Result {
        if !self.state_valid {
            return RESULT_LIB_OPUS_INVALID_STATE;
        }
        if self.initialized {
            return ResultCode::SUCCESS;
        }
        let result =
            self.hardware_opus
                .initialize_decode_object(sample_rate, channel_count, buffer_size);
        if result.is_success() {
            self.magic = DECODE_OBJECT_MAGIC;
            self.initialized = true;
            self.state_valid = true;
            self.final_range = 0;
            self.sample_rate = sample_rate;
            self.channel_count = channel_count;
            self.buffer_size = buffer_size;
        }
        result
    }

    pub fn shutdown(&mut self) -> Result {
        if !self.state_valid {
            return RESULT_LIB_OPUS_INVALID_STATE;
        }
        if self.initialized {
            let result = self.hardware_opus.shutdown_decode_object(self.buffer_size);
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
            self.buffer_size = 0;
        }
        ResultCode::SUCCESS
    }

    pub fn reset_decoder(&mut self) -> Result {
        if !self.state_valid || !self.initialized {
            return RESULT_LIB_OPUS_INVALID_STATE;
        }
        self.hardware_opus.reset_decode_object()
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
        self.hardware_opus.decode_interleaved(
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

    pub fn header(&self) -> DecodeObjectHeader {
        DecodeObjectHeader {
            magic: DECODE_OBJECT_MAGIC,
            sample_rate: self.sample_rate,
            channel_count: self.channel_count,
            total_stream_count: 0,
            stereo_stream_count: 0,
            _reserved0: 0,
            buffer_size: self.buffer_size,
            final_range: self.final_range,
            flags: FLAG_INITIALIZED | FLAG_MAPPED,
        }
    }

    pub fn matches_config(
        shared: &SharedMemory,
        buffer: u64,
        sample_rate: u32,
        channel_count: u32,
        buffer_size: u64,
    ) -> bool {
        let Some(header) = DecodeObjectHeader::read(shared, buffer) else {
            return false;
        };
        header.matches_single_config(sample_rate, channel_count, buffer_size)
    }

    pub fn write_successful_header(
        shared: &mut SharedMemory,
        buffer: u64,
        sample_rate: u32,
        channel_count: u32,
        buffer_size: u64,
    ) -> bool {
        DecodeObjectHeader {
            magic: DECODE_OBJECT_MAGIC,
            sample_rate,
            channel_count,
            total_stream_count: 0,
            stereo_stream_count: 0,
            _reserved0: 0,
            buffer_size,
            final_range: 0,
            flags: FLAG_INITIALIZED | FLAG_MAPPED,
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
        if !header.matches_single_config(header.sample_rate, header.channel_count, buffer_size) {
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
        if !header.is_initialized() || !header.matches_buffer_size(buffer_size) {
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
        if !object_header.can_decode_single() {
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
