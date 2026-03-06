pub const OPUS_STREAM_COUNT_MAX: usize = 255;
pub const MAX_CHANNELS: usize = 2;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct OpusParameters {
    pub sample_rate: u32,
    pub channel_count: u32,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct OpusParametersEx {
    pub sample_rate: u32,
    pub channel_count: u32,
    pub use_large_frame_size: bool,
    pub padding: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct OpusMultiStreamParameters {
    pub sample_rate: u32,
    pub channel_count: u32,
    pub total_stream_count: u32,
    pub stereo_stream_count: u32,
    pub mappings: [u8; OPUS_STREAM_COUNT_MAX + 1],
}

impl Default for OpusMultiStreamParameters {
    fn default() -> Self {
        Self {
            sample_rate: 0,
            channel_count: 0,
            total_stream_count: 0,
            stereo_stream_count: 0,
            mappings: [0; OPUS_STREAM_COUNT_MAX + 1],
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct OpusMultiStreamParametersEx {
    pub sample_rate: u32,
    pub channel_count: u32,
    pub total_stream_count: u32,
    pub stereo_stream_count: u32,
    pub use_large_frame_size: bool,
    pub padding: [u8; 7],
    pub mappings: [u8; OPUS_STREAM_COUNT_MAX + 1],
}

impl Default for OpusMultiStreamParametersEx {
    fn default() -> Self {
        Self {
            sample_rate: 0,
            channel_count: 0,
            total_stream_count: 0,
            stereo_stream_count: 0,
            use_large_frame_size: false,
            padding: [0; 7],
            mappings: [0; OPUS_STREAM_COUNT_MAX + 1],
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct OpusPacketHeader {
    pub size: u32,
    pub final_range: u32,
}
