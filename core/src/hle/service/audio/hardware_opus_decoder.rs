//! Port of zuyu/src/core/hle/service/audio/hardware_opus_decoder.h and .cpp
//!
//! IHardwareOpusDecoder service.

/// IPC command table for IHardwareOpusDecoder:
///
/// | Cmd | Name                                                      |
/// |-----|-----------------------------------------------------------|
/// | 0   | DecodeInterleavedOld                                      |
/// | 1   | SetContext                                                |
/// | 2   | DecodeInterleavedForMultiStreamOld                        |
/// | 3   | SetContextForMultiStream                                  |
/// | 4   | DecodeInterleavedWithPerfOld                              |
/// | 5   | DecodeInterleavedForMultiStreamWithPerfOld                |
/// | 6   | DecodeInterleavedWithPerfAndResetOld                      |
/// | 7   | DecodeInterleavedForMultiStreamWithPerfAndResetOld        |
/// | 8   | DecodeInterleaved                                        |
/// | 9   | DecodeInterleavedForMultiStream                           |
pub struct IHardwareOpusDecoder {
    // impl: AudioCore::OpusDecoder::OpusDecoder
}

impl IHardwareOpusDecoder {
    pub fn new() -> Self {
        Self {}
    }
}
