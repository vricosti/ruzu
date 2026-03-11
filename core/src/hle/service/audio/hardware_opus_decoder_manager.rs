//! Port of zuyu/src/core/hle/service/audio/hardware_opus_decoder_manager.h and .cpp
//!
//! IHardwareOpusDecoderManager service ("hwopus").

/// IPC command table for IHardwareOpusDecoderManager ("hwopus"):
///
/// | Cmd | Name                                         |
/// |-----|----------------------------------------------|
/// | 0   | OpenHardwareOpusDecoder                      |
/// | 1   | GetWorkBufferSize                            |
/// | 2   | OpenOpusDecoderForMultiStream                |
/// | 3   | GetWorkBufferSizeForMultiStream              |
/// | 4   | OpenHardwareOpusDecoderEx                    |
/// | 5   | GetWorkBufferSizeEx                          |
/// | 6   | OpenHardwareOpusDecoderForMultiStreamEx       |
/// | 7   | GetWorkBufferSizeForMultiStreamEx            |
/// | 8   | GetWorkBufferSizeExEx                        |
/// | 9   | GetWorkBufferSizeForMultiStreamExEx          |
pub struct IHardwareOpusDecoderManager {
    // impl: AudioCore::OpusDecoder::OpusDecoderManager
}

impl IHardwareOpusDecoderManager {
    pub fn new() -> Self {
        Self {}
    }
}
