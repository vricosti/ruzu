#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum CommandId {
    Invalid = 0x00,
    DataSourcePcmInt16Version1 = 0x01,
    DataSourcePcmInt16Version2 = 0x02,
    DataSourcePcmFloatVersion1 = 0x03,
    DataSourcePcmFloatVersion2 = 0x04,
    DataSourceAdpcmVersion1 = 0x05,
    DataSourceAdpcmVersion2 = 0x06,
    Volume = 0x07,
    VolumeRamp = 0x08,
    BiquadFilter = 0x09,
    Mix = 0x0A,
    MixRamp = 0x0B,
    MixRampGrouped = 0x0C,
    DepopPrepare = 0x0D,
    DepopForMixBuffers = 0x0E,
    Delay = 0x0F,
    Upsample = 0x10,
    DownMix6chTo2ch = 0x11,
    Aux = 0x12,
    DeviceSink = 0x13,
    CircularBufferSink = 0x14,
    Reverb = 0x15,
    I3dl2Reverb = 0x16,
    Performance = 0x17,
    ClearMixBuffer = 0x18,
    CopyMixBuffer = 0x19,
    LightLimiterVersion1 = 0x1A,
    LightLimiterVersion2 = 0x1B,
    MultiTapBiquadFilter = 0x1C,
    Capture = 0x1D,
    Compressor = 0x1E,
}

pub const COMMAND_MAGIC: u32 = 0xCAFE_BABE;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CommandHeader {
    pub magic: u32,
    pub enabled: u8,
    pub type_: CommandId,
    pub size: i16,
    pub estimated_process_time: u32,
    pub node_id: u32,
}

impl CommandHeader {
    pub fn new(type_: CommandId, size: usize, estimated_process_time: u32, node_id: u32) -> Self {
        Self {
            magic: COMMAND_MAGIC,
            enabled: 1,
            type_,
            size: size as i16,
            estimated_process_time,
            node_id,
        }
    }
}
