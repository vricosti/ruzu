//! Port of zuyu/src/core/hle/service/audio/audio_out.h and audio_out.cpp
//!
//! IAudioOut service.

/// IPC command table for IAudioOut:
///
/// | Cmd | Name                            |
/// |-----|---------------------------------|
/// | 0   | GetAudioOutState                |
/// | 1   | Start                           |
/// | 2   | Stop                            |
/// | 3   | AppendAudioOutBuffer            |
/// | 4   | RegisterBufferEvent             |
/// | 5   | GetReleasedAudioOutBuffers      |
/// | 6   | ContainsAudioOutBuffer          |
/// | 7   | AppendAudioOutBufferAuto        |
/// | 8   | GetReleasedAudioOutBuffersAuto  |
/// | 9   | GetAudioOutBufferCount          |
/// | 10  | GetAudioOutPlayedSampleCount    |
/// | 11  | FlushAudioOutBuffers            |
/// | 12  | SetAudioOutVolume               |
/// | 13  | GetAudioOutVolume               |
pub struct IAudioOut {
    // TODO: Wire up when audio_core and kernel are available.
}

impl IAudioOut {
    pub fn new() -> Self {
        Self {}
    }
}
