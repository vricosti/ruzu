use common::alignment::align_up;

pub type CpuAddr = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PlayState {
    Started = 0,
    Stopped = 1,
    Paused = 2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum SrcQuality {
    Medium = 0,
    High = 1,
    Low = 2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum SampleFormat {
    Invalid = 0,
    PcmInt8 = 1,
    PcmInt16 = 2,
    PcmInt24 = 3,
    PcmInt32 = 4,
    PcmFloat = 5,
    Adpcm = 6,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SessionTypes {
    AudioIn,
    AudioOut,
    FinalOutputRecorder,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Channels {
    FrontLeft = 0,
    FrontRight = 1,
    Center = 2,
    Lfe = 3,
    BackLeft = 4,
    BackRight = 5,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum OldChannels {
    FrontLeft = 0,
    FrontRight = 1,
    BackLeft = 2,
    BackRight = 3,
    Center = 4,
    Lfe = 5,
}

pub const BUFFER_COUNT: u32 = 32;
pub const MAX_RENDERER_SESSIONS: usize = 2;
pub const TARGET_SAMPLE_COUNT: u32 = 240;
pub const TARGET_SAMPLE_RATE: u32 = 48_000;
pub const MAX_CHANNELS: usize = 6;
pub const MAX_MIX_BUFFERS: u32 = 24;
pub const MAX_WAVE_BUFFERS: u32 = 4;
pub const LOWEST_VOICE_PRIORITY: i32 = 0xFF;
pub const HIGHEST_VOICE_PRIORITY: i32 = 0;
pub const BUFFER_ALIGNMENT: u32 = 0x40;
pub const WORKBUFFER_ALIGNMENT: u32 = 0x1000;
pub const FINAL_MIX_ID: i32 = 0;
pub const INVALID_DISTANCE_FROM_FINAL_MIX: i32 = i32::MIN;
pub const UNUSED_SPLITTER_ID: i32 = -1;
pub const UNUSED_MIX_ID: i32 = i32::MAX;
pub const INVALID_NODE_ID: u32 = 0xF000_0000;
pub const INVALID_PROCESS_ORDER: i32 = -1;
pub const MAX_BIQUAD_FILTERS: u32 = 2;
pub const MAX_EFFECTS: u32 = 256;

pub const fn is_channel_count_valid(channel_count: u16) -> bool {
    channel_count <= 6
        && (channel_count == 1 || channel_count == 2 || channel_count == 4 || channel_count == 6)
}

pub fn use_old_channel_mapping(inputs: &mut [i16], outputs: &mut [i16]) {
    let old_center = OldChannels::Center as usize;
    let new_center = Channels::Center as usize;
    let old_lfe = OldChannels::Lfe as usize;
    let new_lfe = Channels::Lfe as usize;

    inputs.swap(old_center, new_center);
    inputs.swap(old_lfe, new_lfe);
    outputs.swap(old_center, new_center);
    outputs.swap(old_lfe, new_lfe);
}

pub const fn make_magic(a: char, b: char, c: char, d: char) -> u32 {
    (a as u32) | ((b as u32) << 8) | ((c as u32) << 16) | ((d as u32) << 24)
}

pub const fn get_splitter_in_param_header_magic() -> u32 {
    make_magic('S', 'N', 'D', 'H')
}

pub const fn get_splitter_info_magic() -> u32 {
    make_magic('S', 'N', 'D', 'I')
}

pub const fn get_splitter_send_data_magic() -> u32 {
    make_magic('S', 'N', 'D', 'D')
}

pub const fn get_sample_format_byte_size(format: SampleFormat) -> usize {
    match format {
        SampleFormat::PcmInt8 => 1,
        SampleFormat::PcmInt16 => 2,
        SampleFormat::PcmInt24 => 3,
        SampleFormat::PcmInt32 | SampleFormat::PcmFloat => 4,
        SampleFormat::Invalid | SampleFormat::Adpcm => 2,
    }
}

pub const fn align_audio(value: u64, alignment: u64) -> u64 {
    align_up(value, alignment)
}
